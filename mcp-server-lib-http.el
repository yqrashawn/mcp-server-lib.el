;;; mcp-server-lib-http.el --- HTTP transport for MCP server -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Laurynas Biveinis

;; Author: Laurynas Biveinis <laurynas.biveinis@gmail.com>
;; Keywords: comm, tools
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (mcp-server-lib "0.2.0") (simple-httpd "1.5.1"))

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

Deliberately separate from `mcp-server-lib-async-timeout', which governs
the stdio transport's blocking poll; the two want very different numbers.
On expiry the request is answered with a JSON-RPC error, which releases
`simple-httpd''s per-connection slot and lets queued requests through.

This is also the longest a keep-alive connection can be blocked: until
that release, `simple-httpd' won't dispatch anything else queued behind
this request on the same connection (see
`mcp-server-lib-http--dispatch-main-thread').  A larger value trades a
longer worst-case stall on that connection for more room for a slow
async op to finish on its own.

The default is 49 hours: long enough that a human question outlives
cchp's own 48-hour ceiling, so cchp reports the timeout rather than
Emacs.  One idle file descriptor is the entire cost of a long value."
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
Logs to `*Messages*' and to a rotating log file."
  (when mcp-server-lib-http-log-requests
    (let ((formatted (apply #'format message args)))
      (message "[MCP HTTP] %s" formatted)
      (lgr-debug mcp-server-lib-http-logger "%s" formatted))))

(defun mcp-server-lib-http--send-error (proc code message &optional id)
  "Send error response with CODE and MESSAGE to PROC.
ID is the originating request's JSON-RPC id, when it is known.  Per the
JSON-RPC 2.0 spec it must be :null when the request never parsed far
enough to have one (e.g. a parse error); callers with no id simply omit
this argument and get that default."
  (with-temp-buffer
    (insert
     (json-encode
      `((jsonrpc . "2.0")
        (id . ,(or id :null))
        (error . ((code . -32603)
                  (message . ,message))))))
    (httpd-send-header proc "application/json" code
                       :Access-Control-Allow-Origin "*")))

(defun mcp-server-lib-http--send-response (proc response-text)
  "Send successful RESPONSE-TEXT to PROC."
  (with-temp-buffer
    (insert response-text)
    (httpd-send-header proc "application/json" 200
                       :Access-Control-Allow-Origin "*")))

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

(defun mcp-server-lib-http--dispatch-main-thread (proc body sid dir)
  "Process JSON-RPC BODY on the main thread.
PROC is the HTTP connection process.  SID and DIR are the session-id
and working directory.

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
              (mcp-server-lib-http--log "Async response: %s" response)
              (setq response-sent t)
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
                  (funcall thunk))))
      (condition-case err
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
                ;; Capture the timeout now: the var is re-read at fire
                ;; time otherwise, so a value changed after arming (e.g.
                ;; a test's `let' unwinding) would make the deadline
                ;; report a duration it never actually waited.
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
                             ;; Same ordering requirement as the callback
                             ;; path above: restore before the send-error
                             ;; below, so a request the send's queue-drain
                             ;; hands this connection to can never capture
                             ;; our sentinel as its own `prev'.
                             (mcp-server-lib-http--restore-sentinel
                              proc own-sentinel prev-sentinel)
                             (when (process-live-p proc)
                               (condition-case err
                                   (mcp-server-lib-http--send-error
                                    proc 504
                                    (format "Async operation timeout (%ss)"
                                            timeout-seconds)
                                    request-id)
                                 ;; This send is the request's last chance
                                 ;; at an answer -- `response-sent' is
                                 ;; already t and the sentinel already
                                 ;; restored, so nothing downstream will
                                 ;; retry or notice.  A silent failure here
                                 ;; is exactly the unanswered-request shape
                                 ;; this deadline exists to eliminate.
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
                (set-process-sentinel proc own-sentinel)))
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
                   (httpd-send-header proc "text/plain" 202)))))))
        (json-error
         (send-once
          (lambda ()
            (mcp-server-lib-http--send-error
             proc 400 (format "Invalid JSON: %s" (error-message-string err))))))
        (error
         (send-once
          (lambda ()
            (mcp-server-lib-http--send-error
             proc 500 (format "Internal error: %s" (error-message-string err))
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
           proc 400 "Empty request body")
        ;; Defer from httpd process filter to main thread event loop.
        (let ((sid session-id)
              (dir cwd))
          (run-at-time
           0 nil
           (lambda ()
             (mcp-server-lib-http--dispatch-main-thread proc body sid dir))))))

     ;; Reject other methods
     (t
      (mcp-server-lib-http--send-error
       proc 405 "Method not allowed")))))

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
