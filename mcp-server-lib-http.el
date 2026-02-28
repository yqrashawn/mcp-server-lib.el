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
;; The server exposes two endpoints:
;;   POST /mcp/v1/messages - Process JSON-RPC requests
;;   POST /mcp/v1/sessions/{id}/messages - Session-scoped JSON-RPC requests
;;
;; Example with curl:
;;   curl -X POST http://localhost:8080/mcp/v1/messages \
;;     -H "Content-Type: application/json" \
;;     -d '{"jsonrpc":"2.0","method":"tools/list","id":1}'

;;; Code:

(require 'mcp-server-lib)
(require 'simple-httpd)
(require 'json)

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

;;; Internal variables



;;; Helper functions

(defun mcp-server-lib-http--log (message &rest args)
  "Log MESSAGE with ARGS if logging is enabled."
  (when mcp-server-lib-http-log-requests
    (apply #'message (concat "[MCP HTTP] " message) args)))

(defun mcp-server-lib-http--send-error (proc code message)
  "Send error response with CODE and MESSAGE to PROC."
  (with-temp-buffer
    (insert
     (json-encode
      `((jsonrpc . "2.0")
        (id . :null)
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

(defun mcp-server-lib-http--handle-jsonrpc-request (proc request &optional session-id)
  "Handle a JSON-RPC HTTP request from PROC with REQUEST headers.
If SESSION-ID is non-nil, bind `mcp-server-lib--request-session-id'
during processing so that tool handlers can resolve per-session state."
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
        ;; Schedule processing on the main thread via timer.
        ;; IMPORTANT: We must NOT use make-thread here because async
        ;; tools (e.g. ask_user_question) use `recursive-edit' to read
        ;; keyboard input, and keyboard reading only works on the main
        ;; thread.  `run-at-time 0' defers to the next event-loop
        ;; iteration, keeping the httpd process filter responsive while
        ;; allowing `recursive-edit' to work correctly.
        (let ((sid session-id))
          (run-at-time
           0 nil
           (lambda ()
             (let ((mcp-server-lib--request-session-id sid))
               (condition-case err
                   (let ((response (mcp-server-lib-process-jsonrpc body)))
                     (mcp-server-lib-http--log "Response: %s" response)
                     (if response
                         (mcp-server-lib-http--send-response proc response)
                       ;; Notification - no response needed
                       (with-temp-buffer
                         (httpd-send-header proc "text/plain" 204))))
                 (json-error
                  (mcp-server-lib-http--send-error
                   proc 400 (format "Invalid JSON: %s" (error-message-string err))))
                 (error
                  (mcp-server-lib-http--send-error
                   proc 500 (format "Internal error: %s" (error-message-string err)))))))))))

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
  (message "Endpoints: POST /mcp/v1/messages, POST /mcp/v1/sessions/{id}/messages" host port))

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
