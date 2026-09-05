;;; mcp-server-lib-http.el --- HTTP transport for MCP server -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Laurynas Biveinis

;; Author: Laurynas Biveinis <laurynas.biveinis@gmail.com>
;; Keywords: comm, tools
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (mcp-server-lib "0.2.0") (dash "2.20.0") (lgr "0") (simple-httpd "1.6") (compat "31"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; HTTP transport for MCP (Model Context Protocol) server.
;;
;; This provides an HTTP interface to the MCP server, enabling:
;; - Concurrent request handling
;; - Web-based client integration
;; - Better async operation support
;; - Standard REST API interface
;;
;; Usage:
;;   (require 'mcp-server-lib-http)
;;   (mcp-server-lib-http-start)  ; Start on localhost:8080
;;   (mcp-server-lib-http-start :port 9000)  ; Custom port
;;   (mcp-server-lib-http-stop)   ; Stop server
;;
;; The server exposes three endpoints:
;;   POST /mcp/v1/messages - Process JSON-RPC requests
;;   POST /mcp/v1/sessions/{id}/messages - Session-scoped JSON-RPC requests
;;   POST /mcp/v1/cwd/{path}/messages - JSON-RPC requests with working directory
;;
;; Example with curl:
;;   curl -X POST http://localhost:8080/mcp/v1/messages \
;;     -H "Content-Type: application/json" \
;;     -d '{"jsonrpc":"2.0","method":"tools/list","id":1}'

;;; Code:

(require 'mcp-server-lib)
(require 'simple-httpd)
(require 'json)
(require 'lgr)
(require 'dash)

;;; Customization

(defgroup mcp-server-lib-http nil
  "HTTP transport for MCP server."
  :group 'mcp-server-lib
  :prefix "mcp-server-lib-http-")

(defcustom mcp-server-lib-http-host "localhost"
  "Default host for HTTP server."
  :type 'string
  :group 'mcp-server-lib-http)

(defcustom mcp-server-lib-http-port 8080
  "Default port for HTTP server."
  :type 'integer
  :group 'mcp-server-lib-http)

(defcustom mcp-server-lib-http-cors-enabled t
  "Enable CORS headers for web clients."
  :type 'boolean
  :group 'mcp-server-lib-http)

(defcustom mcp-server-lib-http-log-requests nil
  "Log HTTP requests and responses."
  :type 'boolean
  :group 'mcp-server-lib-http)

(defcustom mcp-server-lib-http-log-base-name "mcp-http"
  "Base name for HTTP request log files.
Files are named BASE-HOSTNAME-YYYY-MM-DD-HH.log.
Uses `mcp-server-lib-log-directory' for the directory."
  :type 'string
  :group 'mcp-server-lib-http)

(defcustom mcp-server-lib-http-async-timeout 176400
  "Seconds to wait for an async tool's callback over the HTTP transport.

Deliberately a separate variable from `mcp-server-lib-async-timeout',
which governs the stdio transport's blocking poll, so the two transports
can be tuned independently.  The default is 49 hours: long enough to
outlast a tool such as `ask_user_question' that waits on a human to
respond, rather than one that answers promptly.  On expiry the request
is answered with a JSON-RPC error, which releases `simple-httpd''s
per-connection slot and lets queued requests through.

This is also the longest a keep-alive connection can be blocked: until
that release, `simple-httpd' won't dispatch anything else queued behind
this request on the same connection (see
`mcp-server-lib-http--dispatch-main-thread').  A client that shares one
connection across requests will therefore stall behind a parked prompt
for as long as this variable allows -- lower it if your client does
that.  A client that gets a fresh connection per request is unaffected.
This value can also defeat itself: a client with its own, shorter read
timeout gives up and closes the socket before this deadline ever fires,
so the caller sees a raw connection failure instead of the JSON-RPC
timeout error this variable exists to produce."
  :type 'number
  :group 'mcp-server-lib-http)

;;; HTTP file logger

(defvar mcp-server-lib-http-logger (lgr-get-logger "mcp-server-lib-http")
  "Logger for MCP HTTP requests, separate from tool call logger.")

(when mcp-server-lib-log-directory
  (lgr-reset-appenders mcp-server-lib-http-logger)
  (-> mcp-server-lib-http-logger
      (lgr-add-appender
       (-> (mcp-server-lib--rotating-file-appender
            :directory (file-truename mcp-server-lib-log-directory)
            :base-name mcp-server-lib-http-log-base-name)
           (lgr-set-layout
            (lgr-layout-format
             :format "[%t] %L %m"
             :timestamp-format "%Y-%m-%d %H:%M:%S"))))
      (lgr-set-threshold lgr-level-debug)))

;;; Internal variables



;;; Helper functions

(defun mcp-server-lib-http--log (message &rest args)
  "Log MESSAGE with ARGS if logging is enabled.
Logs to `*Messages*' and to a rotating log file.
Every failure in here -- a bad format string, or `lgr-debug' hitting
the rotating file appender when its directory is gone, the disk is
full, or permissions changed -- is caught and reported rather than
let to propagate.  This function is called from the same path that
answers an HTTP request and releases `simple-httpd''s per-connection
`:request-active' slot; a throw from a *diagnostic* log call must
never be able to skip that release or a timer cancel downstream of
it, no matter where a future call site places it."
  (when mcp-server-lib-http-log-requests
    (condition-case err
        (let ((formatted (apply #'format message args)))
          (message "[MCP HTTP] %s" formatted)
          (lgr-debug mcp-server-lib-http-logger "%s" formatted))
      (error
       (message "[MCP HTTP] Logging failed: %s" (error-message-string err))))))

(defconst mcp-server-lib-http-jsonrpc-error-timeout -32000
  "JSON-RPC 2.0 error code for a request the async deadline abandoned.
In the implementation-defined server-error range the spec reserves
\(-32000 to -32099\), and distinct from
`mcp-server-lib-jsonrpc-error-internal' so a client can tell a timed-out
async call apart from a handler crash by code alone, not just by the
human-readable message text.")

(defun mcp-server-lib-http--send-error (proc http-status jsonrpc-code message &optional id)
  "Send a JSON-RPC error response to PROC.
HTTP-STATUS is the HTTP status code for the response.  JSONRPC-CODE is
the JSON-RPC 2.0 error code for the body -- callers choose it explicitly
so a deadline, a crash, and a malformed request no longer collapse onto
the same hardcoded code and become indistinguishable to a client.
MESSAGE is the human-readable error text.  ID is the originating
request's JSON-RPC id, when it is known.  Per the JSON-RPC 2.0 spec it
must be :null when the request never parsed far enough to have one
\(e.g. a parse error\); callers with no id simply omit this argument
and get that default."
  (with-temp-buffer
    (insert
     (json-encode
      `((jsonrpc . "2.0")
        (id . ,(or id :null))
        (error . ((code . ,jsonrpc-code)
                  (message . ,message))))))
    (httpd-send-header proc "application/json" http-status
                       :Access-Control-Allow-Origin "*")))

(defun mcp-server-lib-http--send-response (proc response-text)
  "Send successful RESPONSE-TEXT to PROC."
  (with-temp-buffer
    (insert response-text)
    (httpd-send-header proc "application/json" 200
                       :Access-Control-Allow-Origin "*")))

;;; MCP sessions and server-initiated requests
;;
;; The transport can send the client a JSON-RPC *request* -- currently only
;; `roots/list' -- but MCP gives a server exactly one place to put one: an SSE
;; stream.  Either the stream a client opens with GET, or the stream that
;; answers a client's POST.  We use the second: "the server MAY send JSON-RPC
;; requests and notifications before sending the JSON-RPC response" to a POSTed
;; request.  That needs no GET route (we answer 405, which the spec allows) and
;; no registry of open streams, because the stream is the connection already in
;; hand.  It stays open for one round trip, not indefinitely.
;;
;; The client's answer arrives as a separate POST, so it has to be correlated
;; back: `--pending' maps our outbound id to the waiter, and `--sessions' keeps
;; the resulting roots so later requests -- which arrive on other connections --
;; can still see them, keyed by the `Mcp-Session-Id' we mint at initialize and
;; the client echoes back.

(defcustom mcp-server-lib-http-roots-timeout 10
  "Seconds to wait for a client's `roots/list' answer during initialize.
Unlike `mcp-server-lib-http-async-timeout', which waits on a human, this
waits on a program, so it is short.  On expiry the initialize response is
sent anyway with no roots recorded: failing to learn the roots must never
cost the client its handshake."
  :type 'number
  :group 'mcp-server-lib-http)

(defcustom mcp-server-lib-http-max-sessions 512
  "Hard cap on remembered MCP sessions.
MCP has no session-close hook to evict on, and clients that never send an
HTTP DELETE -- which is all of them observed so far -- leave their entry
behind forever, so a long-lived Emacs would otherwise accumulate one per
`initialize'.  Entries are tiny; this is a safety valve, not a working-set
limit."
  :type 'integer
  :group 'mcp-server-lib-http)

(defvar mcp-server-lib-http--sessions (make-hash-table :test 'equal)
  "Hash of MCP session id -> plist, currently (:roots ROOTS :created TIME).")

(defun mcp-server-lib-http--record-session (session-id roots)
  "Remember ROOTS for SESSION-ID, evicting the oldest sessions past the cap.
Eviction is oldest-first rather than wholesale, because roots are asked
for once, at initialize, and never again: dropping a live session's entry
would not make it re-resolve, it would silently demote that client to
whatever fallback `mcp-server-lib-default-directory-function' ends on --
the wrong-directory failure this feature exists to remove.  Evicting the
oldest confines that to the sessions least likely to still be running."
  (let ((over (- (1+ (hash-table-count mcp-server-lib-http--sessions))
                 mcp-server-lib-http-max-sessions)))
    (when (> over 0)
      (let (by-age)
        (maphash (lambda (sid plist)
                   (push (cons sid (plist-get plist :created)) by-age))
                 mcp-server-lib-http--sessions)
        (setq by-age (sort by-age (lambda (a b) (time-less-p (cdr a) (cdr b)))))
        (dolist (entry (seq-take by-age over))
          (remhash (car entry) mcp-server-lib-http--sessions)))))
  (puthash session-id (list :roots roots :created (current-time))
           mcp-server-lib-http--sessions))

(defvar mcp-server-lib-http--pending (make-hash-table :test 'equal)
  "Hash of our outbound JSON-RPC request id -> callback of one argument.
The callback receives the parsed `result' of the client's response, or
nil if the request timed out, errored, or the connection died.")

(defvar mcp-server-lib-http--outbound-id-counter 0
  "Counter behind `mcp-server-lib-http--next-outbound-id'.")

(defun mcp-server-lib-http--uuid ()
  "Return a fresh MCP session id.
The spec asks for a globally unique, cryptographically secure id made of
visible ASCII, so this is `secure-hash' over random state rather than
`random' alone."
  (secure-hash 'sha256 (format "%s%s%s" (random) (current-time) (emacs-pid))
               nil nil nil))

(defun mcp-server-lib-http--next-outbound-id ()
  "Return a fresh id for a server-initiated JSON-RPC request.
Rendered as a string with an `srv-' prefix.  JSON-RPC gives each side of
a connection its own id space, so a server id is never required to avoid
the client's -- but making them visibly disjoint means a mix-up shows up
as a miss in `mcp-server-lib-http--pending' rather than as a plausible
collision that resolves the wrong waiter."
  (format "srv-%d" (cl-incf mcp-server-lib-http--outbound-id-counter)))

(defun mcp-server-lib-http--header (request name)
  "Return header NAME from simple-httpd REQUEST, case-insensitively, or nil.
HTTP header names are case-insensitive and `simple-httpd' preserves
whatever casing the client sent, so an `assoc' on one spelling silently
misses clients that choose another."
  (cadr (seq-find (lambda (h) (and (stringp (car h))
                                   (string-equal-ignore-case (car h) name)))
                  request)))

(defun mcp-server-lib-http--parse (body)
  "Parse BODY as JSON, returning an alist, or nil if it will not parse."
  (condition-case nil
      (json-read-from-string (decode-coding-string body 'utf-8 t))
    (error nil)))

(defun mcp-server-lib-http--response-p (json)
  "Return non-nil if JSON is a JSON-RPC response rather than a request.
A response carries an id and a `result' or `error' and no `method'."
  (and (consp json)
       (null (alist-get 'method json))
       (alist-get 'id json)
       (or (assq 'result json) (assq 'error json))))

(defun mcp-server-lib-http--wants-roots-p (json)
  "Return non-nil if JSON is an initialize request declaring the roots capability.
Soliciting roots from a client that never declared them is not merely
useless, it is harmful: the client will not answer, so the handshake
stalls until `mcp-server-lib-http-roots-timeout'.  A client with a
shorter initialization timeout of its own -- the MCP Java SDK defaults to
10 seconds -- gives up first and the connection fails outright."
  (and (equal (alist-get 'method json) "initialize")
       (assq 'roots (alist-get 'capabilities (alist-get 'params json)))
       t))

(defun mcp-server-lib-http--resolve-pending (json)
  "Deliver JSON, a client JSON-RPC response, to its waiter.
Returns non-nil if a waiter was found.  An unmatched id is dropped: it
means the waiter already gave up, so there is nothing left to answer."
  (let* ((id (format "%s" (alist-get 'id json)))
         (callback (gethash id mcp-server-lib-http--pending)))
    (when callback
      (remhash id mcp-server-lib-http--pending)
      (funcall callback (alist-get 'result json))
      t)))

;;; SSE framing
;;
;; `httpd-send-header' cannot serve these: it always computes a
;; `Content-Length' from the response buffer, which ends the body at the first
;; frame.  So the header is written by hand -- and with it the responsibility
;; `httpd-send-header' would otherwise discharge, namely clearing
;; `:request-active' and draining `:request-queue' when the response is
;; complete.  Skipping that release wedges the connection for every later
;; request on it, so `--sse-close' does both explicitly.

(defun mcp-server-lib-http--sse-open (proc session-id)
  "Begin a chunked `text/event-stream' response on PROC for SESSION-ID."
  (process-send-string
   proc
   (concat "HTTP/1.1 200 OK\r\n"
           "Content-Type: text/event-stream\r\n"
           "Cache-Control: no-cache\r\n"
           "Transfer-Encoding: chunked\r\n"
           (format "Mcp-Session-Id: %s\r\n" session-id)
           (if mcp-server-lib-http-cors-enabled
               "Access-Control-Allow-Origin: *\r\n"
             "")
           "\r\n")))

(defun mcp-server-lib-http--sse-send (proc data)
  "Send DATA, a JSON string, as one SSE event in one HTTP chunk on PROC."
  (let* ((frame (encode-coding-string (format "data: %s\n\n" data) 'utf-8 t))
         (chunk (concat (format "%x\r\n" (length frame)) frame "\r\n")))
    (process-send-string proc chunk)))

(defun mcp-server-lib-http--sse-close (proc request)
  "End the chunked response on PROC and release its `simple-httpd' slot.
REQUEST decides whether the connection survives: honour a client's
`Connection: close' by deleting PROC, otherwise release the slot and let
any queued request through, exactly as `httpd-send-header' would."
  (process-send-string proc "0\r\n\r\n")
  (if (httpd--connection-close-p request)
      (delete-process proc)
    (process-put proc :request-active nil)
    ;; simple-httpd only grew a request queue in 1.6; on 1.5.1 there is
    ;; nothing to drain and the slot never existed to begin with.
    (when (fboundp 'httpd--pop-request)
      (httpd--pop-request proc))))

;;; Request handlers

(defun mcp-server-lib-http--extract-tool-name (body)
  "Extract tool name from JSON-RPC BODY if it is a tools/call request.
Returns the tool name string, or nil if not a tools/call or on parse error."
  (condition-case nil
      (let* ((json-object (json-read-from-string
                           (decode-coding-string body 'utf-8 t)))
             (method (alist-get 'method json-object))
             (params (alist-get 'params json-object)))
        (when (equal method "tools/call")
          (alist-get 'name params)))
    (error nil)))

(defun mcp-server-lib-http--extract-request-id (body)
  "Extract the JSON-RPC id from BODY, or nil if BODY has none or won't parse.
Lets an error response raised after JSON parsing already succeeded
elsewhere -- a deadline, or a crash handling the request -- correlate
back to the request that triggered it, the same way the stdio transport
does.  Nil here means \"unknown\", which callers must render as :null
rather than guess at a real id."
  (condition-case nil
      (alist-get 'id (json-read-from-string
                      (decode-coding-string body 'utf-8 t)))
    (error nil)))

(defun mcp-server-lib-http--restore-sentinel (proc own prev)
  "Restore PREV as PROC's sentinel, but only if OWN is still installed.
Every async request chains a sentinel onto PROC so that connection death
cancels its deadline.  Restoring it when the request ends keeps that
chain one link deep; without the restore a keep-alive connection gains a
link per async request for its whole life, since nothing ever removed
the previous one.  If PROC's sentinel is no longer OWN then something
else has chained onto it since, and that chain owns the restore -- leave
it alone rather than clobbering it."
  (when (and own (eq (process-sentinel proc) own))
    (set-process-sentinel proc prev)))

(defun mcp-server-lib-http--dispatch-initialize-roots (proc body request)
  "Answer an initialize BODY on PROC with SSE, soliciting `roots/list' first.
REQUEST is the simple-httpd request.  Writes the `roots/list' request
into the stream, waits for the client to POST its answer on another
connection, records the roots against a freshly minted MCP session, then
writes the InitializeResult and closes.

The initialize response is sent exactly once, by whichever of three
things happens first: the client answers, the wait times out, or the
connection dies.  Losing that race in any direction would leave a client
hung on a handshake that never completes."
  (let* ((session-id (mcp-server-lib-http--uuid))
         (outbound-id (mcp-server-lib-http--next-outbound-id))
         (finished nil)
         (timer nil)
         (prev-sentinel nil)
         (own-sentinel nil)
         (finish
          (lambda (roots)
            (unless finished
              (setq finished t)
              (when timer (cancel-timer timer) (setq timer nil))
              (remhash outbound-id mcp-server-lib-http--pending)
              (mcp-server-lib-http--restore-sentinel
               proc own-sentinel prev-sentinel)
              (mcp-server-lib-http--record-session session-id roots)
              (mcp-server-lib-http--log
               "Session %s roots: %s" session-id (or roots "none"))
              (when (process-live-p proc)
                (condition-case err
                    (let ((mcp-server-lib--request-roots roots))
                      (mcp-server-lib-http--sse-send
                       proc (mcp-server-lib-process-jsonrpc body))
                      (mcp-server-lib-http--sse-close proc request))
                  (error
                   (message "[MCP HTTP] Error completing initialize: %s"
                            (error-message-string err)))))))))
    (mcp-server-lib-http--sse-open proc session-id)
    (puthash outbound-id
             (lambda (result) (funcall finish (alist-get 'roots result)))
             mcp-server-lib-http--pending)
    ;; Arm the give-up paths before asking, not after: a client that answers
    ;; synchronously would otherwise find no timer to cancel and no sentinel
    ;; to restore, and a throw between asking and arming would strand the
    ;; handshake with nothing left to complete it.
    (setq timer (run-at-time mcp-server-lib-http-roots-timeout nil
                             (lambda ()
                               (mcp-server-lib-http--log
                                "roots/list timed out after %ss"
                                mcp-server-lib-http-roots-timeout)
                               (funcall finish nil))))
    (setq prev-sentinel (process-sentinel proc)
          own-sentinel
          (lambda (p event)
            (unless (string-prefix-p "open " event)
              (funcall finish nil))
            (when prev-sentinel (funcall prev-sentinel p event))))
    (set-process-sentinel proc own-sentinel)
    (condition-case err
        (mcp-server-lib-http--sse-send
         proc (json-encode `((jsonrpc . "2.0")
                             (id . ,outbound-id)
                             (method . "roots/list"))))
      (error
       (message "[MCP HTTP] Error sending roots/list: %s"
                (error-message-string err))
       (funcall finish nil)))))

(defun mcp-server-lib-http--dispatch-main-thread (proc body sid dir request)
  "Process JSON-RPC BODY on the main thread.
PROC is the HTTP connection process.  SID and DIR are the session-id
and working directory.  REQUEST is the simple-httpd request, needed to
read the `Mcp-Session-Id' header and to honour `Connection: close' when
the answer is an SSE stream.

An async tool's response is delayed, not streamed: nothing is written
until the callback fires, and then it goes out through
`mcp-server-lib-http--send-response' like any other response.  That
matters because `httpd-send-header' is the only thing that clears
`simple-httpd''s per-connection `:request-active' slot and drains
`:request-queue'; writing the response by hand leaks the slot and wedges
the connection for every later request.

Holding `:request-active' for the whole async wait also means this
keep-alive connection serves nothing else until the async op completes,
up to `mcp-server-lib-http-async-timeout' seconds.  That is inherent to
HTTP/1.1, which requires responses in request order per connection --
releasing the slot early would let responses interleave instead of
serialising them.  `Connection: close', or keeping async ops short, are
the ways around the stall; an early release here is not."
  (let* ((mcp-server-lib--request-session-id sid)
         (mcp-server-lib--request-cwd dir)
         (mcp-server-lib--request-roots
          (plist-get (gethash (mcp-server-lib-http--header
                               request "Mcp-Session-Id")
                              mcp-server-lib-http--sessions)
                     :roots))
         (response-sent nil)
         (deadline-timer nil)
         (prev-sentinel nil)
         (own-sentinel nil)
         (request-id (mcp-server-lib-http--extract-request-id body))
         (mcp-server-lib--async-response-fn
          (lambda (response)
            (if response-sent
                ;; This request is already over -- the deadline answered
                ;; it, or the connection died.  Either way its slot is
                ;; released, so `httpd-send-header' would raise "No active
                ;; request", or worse, write onto a *later* request's
                ;; slot.  Drop the answer, and name the actual cause: a
                ;; dropped answer is a prompt the user typed into that
                ;; went nowhere, so the reason must not be guessed at.
                (if (process-live-p proc)
                    (message
                     "[MCP HTTP] Dropping late async response; already answered")
                  (message
                   "[MCP HTTP] Dropping late async response; connection died (status: %s)"
                   (process-status proc)))
              (setq response-sent t)
              (mcp-server-lib-http--log "Async response: %s" response)
              (when deadline-timer
                (cancel-timer deadline-timer)
                (setq deadline-timer nil))
              ;; Restore before send, not after: `httpd-send-header'
              ;; releases `:request-active' and drains `:request-queue',
              ;; which can hand this connection to a queued request.  If
              ;; that request's own `:async-pending' setup ran before we
              ;; restored, it would read our sentinel as still installed,
              ;; capture it as its own `prev', and chain onto it --
              ;; `--restore-sentinel''s ownership check would then see
              ;; someone else's sentinel in place and correctly refuse to
              ;; touch it, permanently leaking this link instead of losing
              ;; a cancellation.  Restoring first closes that window
              ;; unconditionally, regardless of how or when the send that
              ;; follows hands the connection off.
              (mcp-server-lib-http--restore-sentinel
               proc own-sentinel prev-sentinel)
              (if (not (process-live-p proc))
                  (message
                   "[MCP HTTP] Cannot send async response: connection dead (status: %s)"
                   (process-status proc))
                (condition-case err
                    (if response
                        (mcp-server-lib-http--send-response proc response)
                      (with-temp-buffer
                        (httpd-send-header proc "text/plain" 202)))
                  (error
                   (message
                    "[MCP HTTP] Error sending async response: %s"
                    (error-message-string err)))))))))
    ;; `send-once' is the one place a response write and the bookkeeping
    ;; that it happened are inseparable: every path below that can write
    ;; a response calls it, so a path that writes without also recording
    ;; `response-sent' -- the gap that let a late callback overwrite a
    ;; queued request's answer -- can no longer exist without also
    ;; failing to send.  The three paths above that interleave the mark
    ;; with timer/sentinel teardown keep doing that inline instead: they
    ;; must run that teardown *before* deciding whether to write at all,
    ;; which does not fit this shape.
    (cl-flet ((send-once (thunk)
                (unless response-sent
                  (setq response-sent t)
                  (condition-case err
                      (funcall thunk)
                    (error
                     (message "[MCP HTTP] Error sending response: %s"
                              (error-message-string err)))))))
      (condition-case err
          (let ((json (mcp-server-lib-http--parse body)))
            (cond
             ;; A JSON-RPC response from the client, answering something we
             ;; asked.  The core dispatcher keys on `method' alone, so a
             ;; response reaching it would come back "Method not found: nil".
             ;; Per spec a response or notification is answered 202, no body.
             ((mcp-server-lib-http--response-p json)
              (mcp-server-lib-http--resolve-pending json)
              (send-once
               (lambda ()
                 (with-temp-buffer
                   (httpd-send-header proc "text/plain" 202)))))
             ;; Initialize from a roots-capable client: answer over SSE so we
             ;; can ask for its roots before completing the handshake.  Every
             ;; other request, and every client that did not declare roots,
             ;; takes the unchanged path below.
             ((mcp-server-lib-http--wants-roots-p json)
              (setq response-sent t)
              (mcp-server-lib-http--dispatch-initialize-roots proc body request))
             (t
              (let ((response (mcp-server-lib-process-jsonrpc body)))
                (cond
             ;; Async tool - the callback sends the response later.  Write
             ;; nothing now: no headers, no chunked framing, no keepalive.
             ((eq response :async-pending)
              ;; `mcp-server-lib-process-jsonrpc' can invoke the async
              ;; callback synchronously, before this branch ever runs (a
              ;; handler that answers immediately still returns
              ;; `:async-pending' to its caller).  When that happens
              ;; `response-sent' is already t here, and arming a 49-hour
              ;; timer plus chaining a sentinel that nothing will ever
              ;; restore would reintroduce the leak the guard above exists
              ;; to close, just on a narrower trigger.  Skip both.
              (unless response-sent
                ;; Arming is two steps -- start the deadline timer, then
                ;; chain the sentinel -- and either can throw (the second
                ;; step reads and replaces PROC's sentinel, which a test
                ;; or an unusual process state can poison).  A throw after
                ;; the timer is already running would otherwise leak it:
                ;; the crash this propagates to answers with a 500 and
                ;; nothing else ever cancels a timer nobody else knows
                ;; about, so it would sit in `timer-list' for the full
                ;; timeout before firing into a no-op.  Catch here, cancel
                ;; whatever got armed, and re-signal so the outer handler
                ;; still produces the crash response.
                (condition-case arm-err
                    (progn
                      ;; Capture the timeout now: the var is re-read at
                      ;; fire time otherwise, so a value changed after
                      ;; arming (e.g. a test's `let' unwinding) would make
                      ;; the deadline report a duration it never actually
                      ;; waited.
                      (let ((timeout-seconds mcp-server-lib-http-async-timeout))
                        (setq deadline-timer
                              (run-at-time
                               timeout-seconds nil
                               (lambda ()
                                 (setq deadline-timer nil)
                                 (unless response-sent
                                   (setq response-sent t)
                                   (message
                                    "[MCP HTTP] Async deadline reached after %ss"
                                    timeout-seconds)
                                   ;; Same ordering requirement as the
                                   ;; callback path above: restore before
                                   ;; the send-error below, so a request
                                   ;; the send's queue-drain hands this
                                   ;; connection to can never capture our
                                   ;; sentinel as its own `prev'.
                                   (mcp-server-lib-http--restore-sentinel
                                    proc own-sentinel prev-sentinel)
                                   (when (process-live-p proc)
                                     (condition-case err
                                         (mcp-server-lib-http--send-error
                                          proc 200
                                          mcp-server-lib-http-jsonrpc-error-timeout
                                          (format "Async operation timeout (%ss)"
                                                  timeout-seconds)
                                          request-id)
                                       ;; This send is the request's last
                                       ;; chance at an answer --
                                       ;; `response-sent' is already t and
                                       ;; the sentinel already restored, so
                                       ;; nothing downstream will retry or
                                       ;; notice.  A silent failure here is
                                       ;; exactly the unanswered-request
                                       ;; shape this deadline exists to
                                       ;; eliminate.
                                       (error
                                        (message
                                         "[MCP HTTP] Error sending deadline response: %s"
                                         (error-message-string err))))))))))
                      (setq prev-sentinel (process-sentinel proc)
                            own-sentinel
                            (lambda (p event)
                              (unless (string-prefix-p "open " event)
                                (when deadline-timer
                                  (cancel-timer deadline-timer)
                                  (setq deadline-timer nil))
                                (setq response-sent t)
                                (mcp-server-lib-http--restore-sentinel
                                 proc own-sentinel prev-sentinel))
                              (when prev-sentinel (funcall prev-sentinel p event))))
                      (set-process-sentinel proc own-sentinel))
                  (error
                   (when deadline-timer
                     (cancel-timer deadline-timer)
                     (setq deadline-timer nil))
                   (signal (car arm-err) (cdr arm-err))))))
                 ;; Normal response
                 (response
                  (send-once
                   (lambda ()
                     (mcp-server-lib-http--log "Response: %s" response)
                     (mcp-server-lib-http--send-response proc response))))
                 ;; Notification - no response needed
                 (t
                  (send-once
                   (lambda ()
                     (with-temp-buffer
                       (httpd-send-header proc "text/plain" 202))))))))))
        (error
         (send-once
          (lambda ()
            (mcp-server-lib-http--send-error
             proc 200 mcp-server-lib-jsonrpc-error-internal
             (format "Internal error: %s" (error-message-string err))
             request-id))))))))

(defun mcp-server-lib-http--handle-jsonrpc-request (proc request &optional session-id cwd)
  "Handle a JSON-RPC HTTP request from PROC with REQUEST headers.
If SESSION-ID is non-nil, bind `mcp-server-lib--request-session-id'
during processing so that tool handlers can resolve per-session state.
If CWD is non-nil, bind `mcp-server-lib--request-cwd' to it so that
`mcp-server-lib-default-directory-function' can use it, or if no
custom function is set, it is used directly as `default-directory'.

All requests are dispatched on the main thread via `run-at-time'.
Long-running tools should use the async callback pattern (`:async t')
to avoid blocking Emacs."
  (let* ((method (caar request))
         (content (cadr (assoc "Content" request)))
         (body (or content "")))

    (mcp-server-lib-http--log "Method: %s" method)
    (mcp-server-lib-http--log "Body: %s" body)

    (cond
     ;; Handle OPTIONS for CORS preflight
     ((string= method "OPTIONS")
      (with-temp-buffer
        (httpd-send-header proc "text/plain" 204
                           :Access-Control-Allow-Origin "*")))

     ;; Handle POST requests
     ((string= method "POST")
      (if (string-empty-p body)
          (mcp-server-lib-http--send-error
           proc 400 mcp-server-lib-jsonrpc-error-invalid-request
           "Empty request body")
        ;; Defer from httpd process filter to main thread event loop.
        (let ((sid session-id)
              (dir cwd)
              (req request))
          (run-at-time
           0 nil
           (lambda ()
             (mcp-server-lib-http--dispatch-main-thread proc body sid dir req))))))

     ;; Reject other methods
     (t
      (mcp-server-lib-http--send-error
       proc 405 mcp-server-lib-jsonrpc-error-invalid-request
       "Method not allowed")))))

(defun httpd/mcp/v1/messages (proc _uri-path _uri-query request)
  "Handle POST requests to /mcp/v1/messages endpoint.
PROC is the process, REQUEST is headers."
  (mcp-server-lib-http--log "Received request to /mcp/v1/messages")
  (mcp-server-lib-http--handle-jsonrpc-request proc request))

(defun httpd/mcp/v1/sessions (proc uri-path _uri-query request)
  "Handle MCP requests with session routing.
Matches /mcp/v1/sessions/{session-id}/messages.
Extracts the session-id from URI-PATH and binds it as
`mcp-server-lib--request-session-id' during processing."
  (let* ((parts (split-string uri-path "/"))
         (session-id (nth 4 parts)))
    (mcp-server-lib-http--log "Session MCP request for session: %s" session-id)
    (mcp-server-lib-http--handle-jsonrpc-request proc request session-id)))

(defun httpd/mcp/v1/cwd (proc uri-path _uri-query request)
  "Handle MCP requests with working directory routing.
Matches /mcp/v1/cwd/{path}/messages.
Extracts the directory path from URI-PATH and binds `default-directory'
to it during tool execution.

The path between /cwd/ and the trailing /messages is taken as the
working directory (with a leading / prepended by the URL structure).
For example, /mcp/v1/cwd/Users/foo/project/messages sets
`default-directory' to /Users/foo/project."
  (let* ((prefix "/mcp/v1/cwd")
         (suffix "/messages")
         (rest (substring uri-path (length prefix)))
         (cwd (if (string-suffix-p suffix rest)
                  (substring rest 0 (- (length rest) (length suffix)))
                rest))
         (cwd (if (string-empty-p cwd) nil
                (file-name-as-directory cwd))))
    (mcp-server-lib-http--log "CWD MCP request for directory: %s" cwd)
    (mcp-server-lib-http--handle-jsonrpc-request proc request nil cwd)))

;;; Public API
;;;###autoload
(cl-defun mcp-server-lib-http-start (&key (host mcp-server-lib-http-host)
                                          (port mcp-server-lib-http-port))
  "Start MCP HTTP server on HOST:PORT.

Arguments:
  :host  Host to bind to (default: mcp-server-lib-http-host)
  :port  Port to listen on (default: mcp-server-lib-http-port)

Example:

  (mcp-server-lib-http-start)
  (mcp-server-lib-http-start :port 9000)
  (mcp-server-lib-http-start :host \"0.0.0.0\" :port 9000)"
  (interactive)

  ;; Stop existing server if running
  (when (process-status "httpd")
    (mcp-server-lib-http-stop))

  ;; Start MCP server lib if not already running
  (unless mcp-server-lib--running
    (error "MCP server not running. Call `mcp-server-lib-start' first"))

  ;; Configure httpd
  (setq httpd-host host
        httpd-port port)

  ;; Start server
  (httpd-start)

  (message "MCP HTTP server started on http://%s:%d" host port)
  (message "Endpoints: POST /mcp/v1/messages, POST /mcp/v1/sessions/{id}/messages, POST /mcp/v1/cwd/{path}/messages"))

;;;###autoload
(defun mcp-server-lib-http-stop ()
  "Stop the MCP HTTP server."
  (interactive)
  (when (process-status "httpd")
    (httpd-stop)
    (message "MCP HTTP server stopped")))

;;;###autoload
(defun mcp-server-lib-http-status ()
  "Display status of MCP HTTP server."
  (interactive)
  (if (process-status "httpd")
      (message "MCP HTTP server running on http://%s:%d"
               httpd-host httpd-port)
    (message "MCP HTTP server not running")))

(provide 'mcp-server-lib-http)
;;; mcp-server-lib-http.el ends here
