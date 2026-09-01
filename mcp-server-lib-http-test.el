;;; mcp-server-lib-http-test.el --- Tests for the HTTP transport -*- lexical-binding: t; -*-

;;; Commentary:
;; Regression coverage for the `:request-active' slot leak.  `simple-httpd'
;; serialises one request at a time per keep-alive connection: `:417' claims
;; the slot, `:473' refuses to dispatch a queued request while it is held, and
;; `:887' (inside `httpd-send-header') is the only release, after which it
;; drains `:request-queue'.  An async response written with a raw
;; `process-send-string' never reaches that release.

;;; Code:

(require 'ert)
(require 'seq)
(require 'json)
(require 'mcp-server-lib)
(require 'mcp-server-lib-commands)
(require 'mcp-server-lib-http)

(defconst mcp-server-lib-http-test--port 18699
  "Port for the test server.  Deliberately not 18684, the live server's port.")

(defvar mcp-server-lib-http-test--callback nil
  "Captured callback of the pending async test tool.")

(defun mcp-server-lib-http-test--slow-tool (callback)
  "Async test tool.  Stash CALLBACK; answer only when the test releases it."
  (setq mcp-server-lib-http-test--callback callback))

(defun mcp-server-lib-http-test--connect ()
  "Open one raw keep-alive TCP connection to the test server."
  (make-network-process
   :name "mcp-http-test-client"
   :host "127.0.0.1"
   :service mcp-server-lib-http-test--port
   :coding 'binary
   :noquery t
   :buffer (generate-new-buffer " *mcp-http-test-client*")))

(defun mcp-server-lib-http-test--send (proc body)
  "Send BODY to PROC as a keep-alive JSON-RPC POST."
  (process-send-string
   proc
   (format (concat "POST /mcp/v1/messages HTTP/1.1\r\n"
                   "Host: 127.0.0.1\r\n"
                   "Content-Type: application/json\r\n"
                   "Content-Length: %d\r\n"
                   "Connection: keep-alive\r\n\r\n%s")
           (string-bytes body) body)))

(defun mcp-server-lib-http-test--received (proc)
  "Return everything received on PROC so far."
  (with-current-buffer (process-buffer proc) (buffer-string)))

(defun mcp-server-lib-http-test--wait (pred &optional seconds)
  "Pump the event loop until PRED is non-nil or SECONDS elapse."
  (let ((deadline (+ (float-time) (or seconds 5))))
    (while (and (not (funcall pred)) (< (float-time) deadline))
      (accept-process-output nil 0.05))
    (funcall pred)))

(defun mcp-server-lib-http-test--response-count (proc)
  "Number of complete HTTP responses received on PROC so far."
  (let ((s (mcp-server-lib-http-test--received proc)) (n 0) (i 0))
    (while (string-match "HTTP/1\\.1 " s i)
      (setq n (1+ n) i (match-end 0)))
    n))

(defun mcp-server-lib-http-test--server-conns ()
  "Return the httpd client connection processes."
  (seq-filter (lambda (p) (string-prefix-p "httpd <" (process-name p)))
              (process-list)))

(ert-deftest mcp-server-lib-http-test-queued-request-behind-async-is-served ()
  "A request issued while an async tool is pending must still be answered.

This is the D1 tripwire.  Asserting only that `:request-active' is nil
after a lone async call passes vacuously: with nothing enqueued,
`:request-queue' is nil either way.  The leak's signature is a *second*
request, issued during the wait, that is never dispatched."
  (setq mcp-server-lib-http-test--callback nil)
  (let ((mcp-server-lib-http-port mcp-server-lib-http-test--port)
        (proc nil))
    (unwind-protect
        (progn
          (mcp-server-lib-register-tool
           #'mcp-server-lib-http-test--slow-tool
           :id "slow_test_tool"
           :async t
           :description "Test tool that answers only when the test releases it.")
          (mcp-server-lib-start)
          (mcp-server-lib-http-start :port mcp-server-lib-http-test--port)
          (setq proc (mcp-server-lib-http-test--connect))

          ;; A: async tool call -- parks, holding the connection's slot
          (mcp-server-lib-http-test--send
           proc
           (concat "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"tools/call\","
                   "\"params\":{\"name\":\"slow_test_tool\",\"arguments\":{}}}"))
          (should (mcp-server-lib-http-test--wait
                   (lambda () mcp-server-lib-http-test--callback)))

          ;; B: second request on the SAME connection, while A is pending
          (mcp-server-lib-http-test--send
           proc "{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"tools/list\"}")
          (accept-process-output nil 0.3)

          ;; release A
          (funcall mcp-server-lib-http-test--callback
                   (json-encode
                    '((jsonrpc . "2.0") (id . 1)
                      (result . ((content . [((type . "text") (text . "done"))]))))))

          ;; both requests must be answered
          (should (mcp-server-lib-http-test--wait
                   (lambda ()
                     (let ((s (mcp-server-lib-http-test--received proc)))
                       (and (string-match-p "\"id\":1" s)
                            (string-match-p "\"id\":2" s)))))))
      (when (and proc (process-live-p proc)) (delete-process proc))
      (when (and proc (buffer-live-p (process-buffer proc)))
        (kill-buffer (process-buffer proc)))
      (ignore-errors (mcp-server-lib-http-stop))
      (ignore-errors (mcp-server-lib-unregister-tool "slow_test_tool"))
      (ignore-errors (mcp-server-lib-stop)))))

(ert-deftest mcp-server-lib-http-test-slot-released-after-async ()
  "After an async response, no connection may still hold `:request-active'."
  (setq mcp-server-lib-http-test--callback nil)
  (let ((mcp-server-lib-http-port mcp-server-lib-http-test--port)
        (proc nil))
    (unwind-protect
        (progn
          (mcp-server-lib-register-tool
           #'mcp-server-lib-http-test--slow-tool
           :id "slow_test_tool"
           :async t
           :description "Test tool that answers only when the test releases it.")
          (mcp-server-lib-start)
          (mcp-server-lib-http-start :port mcp-server-lib-http-test--port)
          (setq proc (mcp-server-lib-http-test--connect))
          (mcp-server-lib-http-test--send
           proc
           (concat "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"tools/call\","
                   "\"params\":{\"name\":\"slow_test_tool\",\"arguments\":{}}}"))
          (should (mcp-server-lib-http-test--wait
                   (lambda () mcp-server-lib-http-test--callback)))
          (funcall mcp-server-lib-http-test--callback
                   (json-encode
                    '((jsonrpc . "2.0") (id . 1)
                      (result . ((content . [((type . "text") (text . "done"))]))))))
          (should (mcp-server-lib-http-test--wait
                   (lambda ()
                     (seq-every-p (lambda (p) (null (process-get p :request-active)))
                                  (mcp-server-lib-http-test--server-conns))))))
      (when (and proc (process-live-p proc)) (delete-process proc))
      (when (and proc (buffer-live-p (process-buffer proc)))
        (kill-buffer (process-buffer proc)))
      (ignore-errors (mcp-server-lib-http-stop))
      (ignore-errors (mcp-server-lib-unregister-tool "slow_test_tool"))
      (ignore-errors (mcp-server-lib-stop)))))

(defun mcp-server-lib-http-test--abandon-tool (_callback)
  "Async test tool that never calls its callback."
  nil)

(ert-deftest mcp-server-lib-http-test-deadline-answers-abandoned-call ()
  "An async callback that never fires must still produce a response.

Without a deadline the request holds `:request-active' forever and the
client waits until its own timeout with nothing to show for it."
  (let ((mcp-server-lib-http-port mcp-server-lib-http-test--port)
        (mcp-server-lib-http-async-timeout 1)
        (proc nil))
    (unwind-protect
        (progn
          (mcp-server-lib-register-tool
           #'mcp-server-lib-http-test--abandon-tool
           :id "abandon_test_tool"
           :async t
           :description "Test tool that never answers.")
          (mcp-server-lib-start)
          (mcp-server-lib-http-start :port mcp-server-lib-http-test--port)
          (setq proc (mcp-server-lib-http-test--connect))
          (mcp-server-lib-http-test--send
           proc
           (concat "{\"jsonrpc\":\"2.0\",\"id\":7,\"method\":\"tools/call\","
                   "\"params\":{\"name\":\"abandon_test_tool\",\"arguments\":{}}}"))
          ;; the deadline, not the tool, must answer
          (should (mcp-server-lib-http-test--wait
                   (lambda ()
                     (string-match-p
                      "\"error\""
                      (mcp-server-lib-http-test--received proc)))
                   5))
          ;; and the slot must be free again
          (should (seq-every-p (lambda (p) (null (process-get p :request-active)))
                               (mcp-server-lib-http-test--server-conns))))
      (when (and proc (process-live-p proc)) (delete-process proc))
      (when (and proc (buffer-live-p (process-buffer proc)))
        (kill-buffer (process-buffer proc)))
      (ignore-errors (mcp-server-lib-http-stop))
      (ignore-errors (mcp-server-lib-unregister-tool "abandon_test_tool"))
      (ignore-errors (mcp-server-lib-stop)))))

(provide 'mcp-server-lib-http-test)
;;; mcp-server-lib-http-test.el ends here
