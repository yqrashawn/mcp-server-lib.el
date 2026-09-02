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

(defun mcp-server-lib-http-test--nth-response (proc n)
  "Return the raw text of the Nth (1-indexed) HTTP response received on PROC."
  (let* ((s (mcp-server-lib-http-test--received proc))
         (starts nil)
         (i 0))
    (while (string-match "HTTP/1\\.1 " s i)
      (push (match-beginning 0) starts)
      (setq i (match-end 0)))
    (setq starts (nreverse starts))
    (substring s (nth (1- n) starts) (or (nth n starts) (length s)))))

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
          ;; Guard against `seq-every-p' passing vacuously on an empty
          ;; list -- `(seq-every-p pred nil)' is t, so if
          ;; `--server-conns' ever returned nil (name-prefix drift,
          ;; teardown timing) this wait would report success on its
          ;; very first poll, having proven nothing.
          (should (mcp-server-lib-http-test--wait
                   (lambda ()
                     (and (mcp-server-lib-http-test--server-conns)
                          (seq-every-p (lambda (p) (null (process-get p :request-active)))
                                       (mcp-server-lib-http-test--server-conns)))))))
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
          ;; and the slot must be free again.  Guard against
          ;; `seq-every-p' passing vacuously on an empty list --
          ;; `(seq-every-p pred nil)' is t, so if `--server-conns'
          ;; ever returned nil (name-prefix drift, teardown timing)
          ;; the assertion below would prove nothing.
          (should (mcp-server-lib-http-test--server-conns))
          (should (seq-every-p (lambda (p) (null (process-get p :request-active)))
                               (mcp-server-lib-http-test--server-conns))))
      (when (and proc (process-live-p proc)) (delete-process proc))
      (when (and proc (buffer-live-p (process-buffer proc)))
        (kill-buffer (process-buffer proc)))
      (ignore-errors (mcp-server-lib-http-stop))
      (ignore-errors (mcp-server-lib-unregister-tool "abandon_test_tool"))
      (ignore-errors (mcp-server-lib-stop)))))

(ert-deftest mcp-server-lib-http-test-late-answer-is-dropped ()
  "A callback firing after the deadline must not answer someone else's request.

`httpd-send-header' already refuses to write once `:request-active' is
nil, so a late answer landing on an *idle* connection is harmless and
proves nothing -- it passes with or without the guard.  The hazard is a
late answer landing while the connection is serving a *different*
request: `:request-active' is non-nil again, for the new request, so
`httpd-send-header' writes -- delivering request 9's payload as the
reply to request 33 and clearing request 33's slot, which then strands
request 33 with no answer at all.

The slot-holder is a second *async* call, and that is load-bearing.  A
synchronous method claims and releases the slot inside two back-to-back
`run-at-time 0' ticks (`httpd--pop-request' defers the dispatch, and
`mcp-server-lib-http--handle-jsonrpc-request' defers POST again), so the
window is too narrow to act in: a `tools/list' slot-holder lost that
race once in twelve runs even with the guard in place.  A parked async
call holds the slot until the test hands it back."
  (setq mcp-server-lib-http-test--callback nil)
  (let ((mcp-server-lib-http-port mcp-server-lib-http-test--port)
        (mcp-server-lib-http-async-timeout 1)
        (proc nil)
        (late-callback nil))
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
          ;; request 9: async, and deliberately abandoned past its deadline
          (mcp-server-lib-http-test--send
           proc
           (concat "{\"jsonrpc\":\"2.0\",\"id\":9,\"method\":\"tools/call\","
                   "\"params\":{\"name\":\"slow_test_tool\",\"arguments\":{}}}"))
          (should (mcp-server-lib-http-test--wait
                   (lambda () mcp-server-lib-http-test--callback)))
          (setq late-callback mcp-server-lib-http-test--callback)
          ;; the deadline answers request 9 and releases the slot
          (should (mcp-server-lib-http-test--wait
                   (lambda ()
                     (string-match-p "\"error\""
                                     (mcp-server-lib-http-test--received proc)))
                   5))
          (should (= 1 (mcp-server-lib-http-test--response-count proc)))
          ;; request 33 takes the slot and parks, holding it while we act
          (let ((mcp-server-lib-http-async-timeout 30))
            (setq mcp-server-lib-http-test--callback nil)
            (mcp-server-lib-http-test--send
             proc
             (concat "{\"jsonrpc\":\"2.0\",\"id\":33,\"method\":\"tools/call\","
                     "\"params\":{\"name\":\"slow_test_tool\",\"arguments\":{}}}"))
            (should (mcp-server-lib-http-test--wait
                     (lambda () mcp-server-lib-http-test--callback)))
            (should (seq-some (lambda (p) (process-get p :request-active))
                              (mcp-server-lib-http-test--server-conns)))
            ;; request 9 answers now, far too late, onto request 33's slot
            (funcall late-callback
                     (json-encode
                      '((jsonrpc . "2.0") (id . 9)
                        (result . ((content . [((type . "text")
                                                (text . "late"))]))))))
            ;; wait for the second response the guard has to prevent
            (mcp-server-lib-http-test--wait
             (lambda () (> (mcp-server-lib-http-test--response-count proc) 1))
             1)
            (should (= 1 (mcp-server-lib-http-test--response-count proc)))
            ;; and request 33 must still get its own answer, uncorrupted
            (funcall mcp-server-lib-http-test--callback
                     (json-encode
                      '((jsonrpc . "2.0") (id . 33)
                        (result . ((content . [((type . "text")
                                                (text . "own-answer"))]))))))
            (should (mcp-server-lib-http-test--wait
                     (lambda ()
                       (= 2 (mcp-server-lib-http-test--response-count proc)))))
            (let ((second-response
                   (mcp-server-lib-http-test--nth-response proc 2)))
              (should (string-match-p "own-answer" second-response))
              (should-not (string-match-p "late" second-response)))))
      (when (and proc (process-live-p proc)) (delete-process proc))
      (when (and proc (buffer-live-p (process-buffer proc)))
        (kill-buffer (process-buffer proc)))
      (ignore-errors (mcp-server-lib-http-stop))
      (ignore-errors (mcp-server-lib-unregister-tool "slow_test_tool"))
      (ignore-errors (mcp-server-lib-stop)))))

(ert-deftest mcp-server-lib-http-test-connection-death-cancels-deadline ()
  "Killing the connection must cancel the pending deadline timer.

Identifies the specific deadline timer object rather than comparing
`(length timer-list)': an unrelated ambient timer firing inside the wait
window would satisfy a bare length comparison with no cancellation
having happened at all, so a real regression could still pass.  The
deadline is also deliberately far outside the wait bound below: a
one-shot `run-at-time' self-removes from `timer-list' the moment it
fires, so if the wait bound were close to the deadline, the timer would
disappear on its own and the test would pass whether or not connection
death actually cancels anything."
  (setq mcp-server-lib-http-test--callback nil)
  (let ((mcp-server-lib-http-port mcp-server-lib-http-test--port)
        (mcp-server-lib-http-async-timeout 30)
        (proc nil)
        (timers-before nil)
        (deadline nil))
    (unwind-protect
        (progn
          (mcp-server-lib-register-tool
           #'mcp-server-lib-http-test--slow-tool
           :id "slow_test_tool"
           :async t
           :description "Test tool that answers only when the test releases it.")
          (mcp-server-lib-start)
          (mcp-server-lib-http-start :port mcp-server-lib-http-test--port)
          (setq timers-before (copy-sequence timer-list))
          (setq proc (mcp-server-lib-http-test--connect))
          (mcp-server-lib-http-test--send
           proc
           (concat "{\"jsonrpc\":\"2.0\",\"id\":11,\"method\":\"tools/call\","
                   "\"params\":{\"name\":\"slow_test_tool\",\"arguments\":{}}}"))
          (should (mcp-server-lib-http-test--wait
                   (lambda () mcp-server-lib-http-test--callback)))
          ;; Identify the one new timer object by reference (`eq'), not
          ;; by counting: this is the deadline our request armed, and
          ;; nothing else.
          (let ((new-timers (seq-difference timer-list timers-before #'eq)))
            (should (= 1 (length new-timers)))
            (setq deadline (car new-timers)))
          (delete-process proc)
          (should (mcp-server-lib-http-test--wait
                   (lambda () (not (memq deadline timer-list)))
                   5)))
      (when (and proc (process-live-p proc)) (delete-process proc))
      (when (and proc (buffer-live-p (process-buffer proc)))
        (kill-buffer (process-buffer proc)))
      (ignore-errors (mcp-server-lib-http-stop))
      (ignore-errors (mcp-server-lib-unregister-tool "slow_test_tool"))
      (ignore-errors (mcp-server-lib-stop)))))

(ert-deftest mcp-server-lib-http-test-sentinel-restored-after-async ()
  "A finished async request must leave the connection's sentinel as it found it.

Every async request chains a sentinel onto the connection so that
connection death cancels its deadline.  Nothing used to remove the
previous one, so a keep-alive connection gained a link per async request
for its whole life -- measured at six installs over five sequential
calls on one connection, each lambda wrapping the last and all of them
firing nested on disconnect.  Asserting that `httpd--sentinel' is back
in place once a request completes pins the chain at depth one.

Two requests, not one, and a check *while* each is parked: without the
mid-flight check a version that installed no sentinel at all would also
pass the final assertion, which is the vacuous shape this suite has
already been bitten by twice."
  (setq mcp-server-lib-http-test--callback nil)
  (let ((mcp-server-lib-http-port mcp-server-lib-http-test--port)
        (mcp-server-lib-http-async-timeout 30)
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
          ;; two sequential async calls down one keep-alive connection
          (dotimes (i 2)
            (setq mcp-server-lib-http-test--callback nil)
            (mcp-server-lib-http-test--send
             proc
             (format
              (concat "{\"jsonrpc\":\"2.0\",\"id\":%d,\"method\":\"tools/call\","
                      "\"params\":{\"name\":\"slow_test_tool\",\"arguments\":{}}}")
              (+ 40 i)))
            (should (mcp-server-lib-http-test--wait
                     (lambda () mcp-server-lib-http-test--callback)))
            ;; parked: our own sentinel is the installed one.  Guard
            ;; against `seq-every-p' passing vacuously on an empty
            ;; list -- `(seq-every-p pred nil)' is t, so if
            ;; `--server-conns' ever returned nil (name-prefix drift,
            ;; teardown timing) the assertion below would prove nothing.
            (should (mcp-server-lib-http-test--server-conns))
            (should (seq-every-p
                     (lambda (p)
                       (not (eq (process-sentinel p) #'httpd--sentinel)))
                     (mcp-server-lib-http-test--server-conns)))
            (funcall mcp-server-lib-http-test--callback
                     (json-encode
                      `((jsonrpc . "2.0") (id . ,(+ 40 i))
                        (result . ((content . [((type . "text")
                                                (text . "done"))]))))))
            (should (mcp-server-lib-http-test--wait
                     (lambda ()
                       (= (1+ i)
                          (mcp-server-lib-http-test--response-count proc)))))
            ;; answered: httpd's own sentinel is back, chain depth one.
            ;; Same vacuous-pass guard as above.
            (should (mcp-server-lib-http-test--server-conns))
            (should (seq-every-p
                     (lambda (p)
                       (eq (process-sentinel p) #'httpd--sentinel))
                     (mcp-server-lib-http-test--server-conns)))))
      (when (and proc (process-live-p proc)) (delete-process proc))
      (when (and proc (buffer-live-p (process-buffer proc)))
        (kill-buffer (process-buffer proc)))
      (ignore-errors (mcp-server-lib-http-stop))
      (ignore-errors (mcp-server-lib-unregister-tool "slow_test_tool"))
      (ignore-errors (mcp-server-lib-stop)))))

(defun mcp-server-lib-http-test--sync-answering-async-tool (callback)
  "Async test tool that answers CALLBACK immediately, synchronously."
  (funcall callback "immediate"))

(ert-deftest mcp-server-lib-http-test-sync-answer-leaves-no-residue ()
  "A tool that answers its async callback synchronously must not arm a
deadline timer or leave a sentinel chained onto the connection.

`mcp-server-lib--handle-tools-call-apply' returns `:async-pending'
whenever a tool is registered `:async t', even when the handler calls
its callback before `apply' returns -- the real answer is already on
the wire by the time `dispatch-main-thread' reaches its `:async-pending'
branch.  Without a guard there, that branch would still arm a deadline
timer and chain a sentinel for a request that is already fully
answered, and nothing would ever restore either: the real answer's own
restore call already ran and no-opped, having found no sentinel
installed yet.

Looks for a timer whose remaining delay matches the timeout below,
rather than asserting `timer-list' did not grow at all: batch Emacs can
start unrelated timers of its own, and a bare growth check would be
exactly the counting mistake fixed elsewhere in this file for
`connection-death-cancels-deadline'.  A distinctive, non-round timeout
value makes an accidental match with something ambient practically
impossible."
  (let ((mcp-server-lib-http-port mcp-server-lib-http-test--port)
        (mcp-server-lib-http-async-timeout 87654.321)
        (proc nil))
    (unwind-protect
        (progn
          (mcp-server-lib-register-tool
           #'mcp-server-lib-http-test--sync-answering-async-tool
           :id "sync_test_tool"
           :async t
           :description "Test tool that answers its callback immediately.")
          (mcp-server-lib-start)
          (mcp-server-lib-http-start :port mcp-server-lib-http-test--port)
          (setq proc (mcp-server-lib-http-test--connect))
          (mcp-server-lib-http-test--send
           proc
           (concat "{\"jsonrpc\":\"2.0\",\"id\":50,\"method\":\"tools/call\","
                   "\"params\":{\"name\":\"sync_test_tool\",\"arguments\":{}}}"))
          (should (mcp-server-lib-http-test--wait
                   (lambda () (= 1 (mcp-server-lib-http-test--response-count proc)))))
          (should-not
           (seq-find
            (lambda (tm)
              (< (abs (- (float-time
                          (time-subtract (timer--time tm) (current-time)))
                         mcp-server-lib-http-async-timeout))
                 5))
            timer-list))
          (should (mcp-server-lib-http-test--server-conns))
          (should (seq-every-p
                   (lambda (p) (eq (process-sentinel p) #'httpd--sentinel))
                   (mcp-server-lib-http-test--server-conns))))
      (when (and proc (process-live-p proc)) (delete-process proc))
      (when (and proc (buffer-live-p (process-buffer proc)))
        (kill-buffer (process-buffer proc)))
      (ignore-errors (mcp-server-lib-http-stop))
      (ignore-errors (mcp-server-lib-unregister-tool "sync_test_tool"))
      (ignore-errors (mcp-server-lib-stop)))))

(ert-deftest mcp-server-lib-http-test-send-once-logs-failed-send ()
  "A response thunk that throws inside `send-once' must be logged, not swallowed.

Marking `response-sent' before the funcall is required -- a throwing
send must not leave the flag clear and the request re-sendable -- but
that alone makes the failure invisible: every other write path in this
file logs its own send failure, so a thunk that throws here must too."
  (let ((mcp-server-lib-http-port mcp-server-lib-http-test--port)
        (proc nil)
        (messages-before nil))
    (unwind-protect
        (cl-letf (((symbol-function 'mcp-server-lib-http--send-response)
                   (lambda (&rest _) (error "send-response boom"))))
          (mcp-server-lib-start)
          (mcp-server-lib-http-start :port mcp-server-lib-http-test--port)
          (setq messages-before
                (with-current-buffer (messages-buffer) (buffer-string)))
          (setq proc (mcp-server-lib-http-test--connect))
          (mcp-server-lib-http-test--send
           proc "{\"jsonrpc\":\"2.0\",\"id\":99,\"method\":\"tools/list\"}")
          (should
           (mcp-server-lib-http-test--wait
            (lambda ()
              (string-match-p
               "\\[MCP HTTP\\] Error sending response"
               (substring (with-current-buffer (messages-buffer) (buffer-string))
                          (length messages-before))))
            5)))
      (when (and proc (process-live-p proc)) (delete-process proc))
      (when (and proc (buffer-live-p (process-buffer proc)))
        (kill-buffer (process-buffer proc)))
      (ignore-errors (mcp-server-lib-http-stop))
      (ignore-errors (mcp-server-lib-stop)))))

(ert-deftest mcp-server-lib-http-test-log-failure-during-callback-does-not-resurrect-request ()
  "If logging an async response throws, the request must stay marked sent.

Moving `(setq response-sent t)' above the `--log' call closes this: if
`--log' signals -- a broken file appender, say -- the throw escapes
into `mcp-server-lib.el''s own retry handler, which calls this
transport's response function a SECOND time with a synthetic error
payload.  Before the fix `response-sent' was still nil at that point (it
was set only after `--log' returned), so the guard treated the retry as
a fresh, legitimate send and wrote it to the wire, resurrecting a
request this connection may already have moved past -- exactly the
wrong-request-gets-the-answer hazard `late-answer-is-dropped' exists to
prevent.  After the fix the retry is correctly dropped as already
answered, and nothing is written at all."
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
           (concat "{\"jsonrpc\":\"2.0\",\"id\":66,\"method\":\"tools/call\","
                   "\"params\":{\"name\":\"slow_test_tool\",\"arguments\":{}}}"))
          (should (mcp-server-lib-http-test--wait
                   (lambda () mcp-server-lib-http-test--callback)))
          (let ((poisoned t))
            (cl-letf (((symbol-function 'mcp-server-lib-http--log)
                       (lambda (&rest _)
                         (when poisoned
                           (setq poisoned nil)
                           (error "poisoned --log")))))
              (funcall mcp-server-lib-http-test--callback
                       (json-encode
                        '((jsonrpc . "2.0") (id . 66)
                          (result . ((content . [((type . "text") (text . "done"))]))))))))
          ;; give the retry every chance to write, then confirm it did not
          (accept-process-output nil 0.3)
          (should (= 0 (mcp-server-lib-http-test--response-count proc))))
      (when (and proc (process-live-p proc)) (delete-process proc))
      (when (and proc (buffer-live-p (process-buffer proc)))
        (kill-buffer (process-buffer proc)))
      (ignore-errors (mcp-server-lib-http-stop))
      (ignore-errors (mcp-server-lib-unregister-tool "slow_test_tool"))
      (ignore-errors (mcp-server-lib-stop)))))

(ert-deftest mcp-server-lib-http-test-crash-while-arming-marks-answered ()
  "A crash while arming the async wait must still mark the request answered,
so a callback that fires later cannot write a second response onto a slot
the crash's own error response has since handed to a different request.

Reproduces the exact sequence this closes: the handler stashes its
callback and returns `:async-pending', something throws inside that
branch (here, a poisoned `run-at-time' standing in for the deadline
timer), the outer `error' handler answers with a 500 through
`httpd-send-header' -- releasing `:request-active' and dispatching
request 33, parked behind it -- and only then does request 21's stashed
callback fire.  Before the fix, four of the seven paths that can write a
response never set `response-sent', including this one: the late
callback's own guard never tripped, and it corrupted request 33's answer.

A single request without a follow-up cannot show this: `httpd-send-header'
already refuses to write once `:request-active' is nil again, so a late
answer landing with nothing else queued is caught by that check anyway
and proves nothing about `response-sent' specifically -- exactly the
vacuous-pass shape `late-answer-is-dropped' already had to design around
for the deadline path."
  (setq mcp-server-lib-http-test--callback nil)
  (let ((mcp-server-lib-http-port mcp-server-lib-http-test--port)
        (proc nil)
        (real-run-at-time (symbol-function 'run-at-time))
        (crashed-callback nil))
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
          ;; request 21: async, and its own dispatch crashes while arming
          (cl-letf (((symbol-function 'run-at-time)
                     (lambda (time &rest args)
                       ;; Let the framework's own "defer to main thread"
                       ;; scheduling through unharmed -- it always uses a
                       ;; delay of 0 -- and poison only the deadline's
                       ;; arm, the one call that uses the real timeout.
                       (if (eq time 0)
                           (apply real-run-at-time time args)
                         (error "poisoned run-at-time")))))
            (mcp-server-lib-http-test--send
             proc
             (concat "{\"jsonrpc\":\"2.0\",\"id\":21,\"method\":\"tools/call\","
                     "\"params\":{\"name\":\"slow_test_tool\",\"arguments\":{}}}"))
            ;; the handler stashed its callback before the crash
            (should (mcp-server-lib-http-test--wait
                     (lambda () mcp-server-lib-http-test--callback)))
            (setq crashed-callback mcp-server-lib-http-test--callback)
            ;; the crash answers with an internal-error response and
            ;; releases the slot
            (should (mcp-server-lib-http-test--wait
                     (lambda () (= 1 (mcp-server-lib-http-test--response-count proc)))))
            (should (string-match-p
                     "\"code\":-32603"
                     (mcp-server-lib-http-test--nth-response proc 1))))
          ;; request 33 takes the slot and parks, holding it while we act
          (setq mcp-server-lib-http-test--callback nil)
          (mcp-server-lib-http-test--send
           proc
           (concat "{\"jsonrpc\":\"2.0\",\"id\":33,\"method\":\"tools/call\","
                   "\"params\":{\"name\":\"slow_test_tool\",\"arguments\":{}}}"))
          (should (mcp-server-lib-http-test--wait
                   (lambda () mcp-server-lib-http-test--callback)))
          ;; request 21 answers now, far too late, onto request 33's slot
          (funcall crashed-callback
                   (json-encode
                    '((jsonrpc . "2.0") (id . 21)
                      (result . ((content . [((type . "text") (text . "late"))]))))))
          ;; wait for the second response the guard has to prevent
          (mcp-server-lib-http-test--wait
           (lambda () (> (mcp-server-lib-http-test--response-count proc) 1))
           1)
          (should (= 1 (mcp-server-lib-http-test--response-count proc)))
          ;; and request 33 must still get its own answer, uncorrupted
          (funcall mcp-server-lib-http-test--callback
                   (json-encode
                    '((jsonrpc . "2.0") (id . 33)
                      (result . ((content . [((type . "text") (text . "own-answer"))]))))))
          (should (mcp-server-lib-http-test--wait
                   (lambda ()
                     (= 2 (mcp-server-lib-http-test--response-count proc)))))
          (let ((second-response (mcp-server-lib-http-test--nth-response proc 2)))
            (should (string-match-p "own-answer" second-response))
            (should-not (string-match-p "late" second-response))))
      (when (and proc (process-live-p proc)) (delete-process proc))
      (when (and proc (buffer-live-p (process-buffer proc)))
        (kill-buffer (process-buffer proc)))
      (ignore-errors (mcp-server-lib-http-stop))
      (ignore-errors (mcp-server-lib-unregister-tool "slow_test_tool"))
      (ignore-errors (mcp-server-lib-stop)))))

(ert-deftest mcp-server-lib-http-test-crash-after-arm-cancels-deadline-timer ()
  "A crash after the deadline timer is armed must cancel it, not leak it.

`crash-while-arming-marks-answered' poisons the arm itself
\(`run-at-time'\), so the timer is never created and there is nothing to
leak in that test.  Here the arm succeeds first -- `process-sentinel'
throws only once that has already happened -- so the async-pending
branch has to notice the timer it already created and cancel it before
signalling on to the crash's own 500 response.  Without that guard the
timer would sit in `timer-list' for the full
`mcp-server-lib-http-async-timeout' delay before firing into a no-op."
  (setq mcp-server-lib-http-test--callback nil)
  (let ((mcp-server-lib-http-port mcp-server-lib-http-test--port)
        (mcp-server-lib-http-async-timeout 30)
        (proc nil)
        (timers-before nil))
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
          (setq timers-before (copy-sequence timer-list))
          (let ((real-process-sentinel (symbol-function 'process-sentinel))
                (poisoned t))
            (cl-letf (((symbol-function 'process-sentinel)
                       (lambda (p)
                         (if poisoned
                             (progn (setq poisoned nil)
                                    (error "poisoned process-sentinel"))
                           (funcall real-process-sentinel p)))))
              (mcp-server-lib-http-test--send
               proc
               (concat "{\"jsonrpc\":\"2.0\",\"id\":23,\"method\":\"tools/call\","
                       "\"params\":{\"name\":\"slow_test_tool\",\"arguments\":{}}}"))
              ;; the handler stashed its callback before the crash
              (should (mcp-server-lib-http-test--wait
                       (lambda () mcp-server-lib-http-test--callback)))
              ;; the crash answers with an internal-error response
              (should (mcp-server-lib-http-test--wait
                       (lambda () (= 1 (mcp-server-lib-http-test--response-count proc)))))
              (should (string-match-p
                       "\"code\":-32603"
                       (mcp-server-lib-http-test--nth-response proc 1)))))
          ;; the arm succeeded before the crash -- no new timer must
          ;; survive it.  `eq'-based, like `connection-death-cancels-
          ;; deadline', so an ambient batch-Emacs timer cannot hide a
          ;; real leak.
          (should (null (seq-difference timer-list timers-before #'eq))))
      (when (and proc (process-live-p proc)) (delete-process proc))
      (when (and proc (buffer-live-p (process-buffer proc)))
        (kill-buffer (process-buffer proc)))
      (ignore-errors (mcp-server-lib-http-stop))
      (ignore-errors (mcp-server-lib-unregister-tool "slow_test_tool"))
      (ignore-errors (mcp-server-lib-stop)))))

(ert-deftest mcp-server-lib-http-test-deadline-preserves-request-id ()
  "The deadline's error response must carry the original request's id.

Before the fix `mcp-server-lib-http--send-error' hardcoded `:null', so a
client correlating responses by id could never match a timeout back to
the request that produced it -- unlike the stdio transport, which
already threads the real id through its own timeout path."
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
           (concat "{\"jsonrpc\":\"2.0\",\"id\":777,\"method\":\"tools/call\","
                   "\"params\":{\"name\":\"abandon_test_tool\",\"arguments\":{}}}"))
          (should (mcp-server-lib-http-test--wait
                   (lambda ()
                     (string-match-p
                      "\"error\"" (mcp-server-lib-http-test--received proc)))
                   5))
          (should (string-match-p
                   "\"id\":777" (mcp-server-lib-http-test--received proc)))
          (should-not (string-match-p
                       "\"id\":null" (mcp-server-lib-http-test--received proc))))
      (when (and proc (process-live-p proc)) (delete-process proc))
      (when (and proc (buffer-live-p (process-buffer proc)))
        (kill-buffer (process-buffer proc)))
      (ignore-errors (mcp-server-lib-http-stop))
      (ignore-errors (mcp-server-lib-unregister-tool "abandon_test_tool"))
      (ignore-errors (mcp-server-lib-stop)))))

(ert-deftest mcp-server-lib-http-test-deadline-answers-with-200-and-distinct-code ()
  "The deadline's response must be a 200 carrying its own JSON-RPC code.

Before the fix the deadline answered 504, but
`mcp-server-lib-process-jsonrpc' already returns its own JSON-RPC
errors as ordinary 200 responses -- only transport-level failures use
4xx/5xx -- so a Streamable-HTTP client that checks `response.ok' before
parsing raises a transport error on the 504 and never reads the body,
silently discarding request-id preservation for exactly the case it
exists for.  And the JSON-RPC code was hardcoded to -32603 for every
caller, so a client could not tell this apart from a handler crash by
code alone."
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
           (concat "{\"jsonrpc\":\"2.0\",\"id\":88,\"method\":\"tools/call\","
                   "\"params\":{\"name\":\"abandon_test_tool\",\"arguments\":{}}}"))
          (should (mcp-server-lib-http-test--wait
                   (lambda ()
                     (string-match-p
                      "\"error\"" (mcp-server-lib-http-test--received proc)))
                   5))
          (let ((response (mcp-server-lib-http-test--nth-response proc 1)))
            (should (string-match-p "\\`HTTP/1\\.1 200" response))
            (should (string-match-p
                     (format "\"code\":%d"
                             mcp-server-lib-http-jsonrpc-error-timeout)
                     response))
            (should-not (string-match-p "\"code\":-32603" response))))
      (when (and proc (process-live-p proc)) (delete-process proc))
      (when (and proc (buffer-live-p (process-buffer proc)))
        (kill-buffer (process-buffer proc)))
      (ignore-errors (mcp-server-lib-http-stop))
      (ignore-errors (mcp-server-lib-unregister-tool "abandon_test_tool"))
      (ignore-errors (mcp-server-lib-stop)))))

(ert-deftest mcp-server-lib-http-test-deadline-send-failure-is-logged ()
  "A deadline whose own error response fails to send must log why.

Before the fix, `ignore-errors' swallowed that failure silently:
`response-sent' was already t and the sentinel already restored, so
nothing downstream would ever retry or notice -- the request was simply
never answered, with no diagnostic anywhere."
  (let ((mcp-server-lib-http-port mcp-server-lib-http-test--port)
        (mcp-server-lib-http-async-timeout 1)
        (proc nil)
        (messages-before nil))
    (unwind-protect
        (cl-letf (((symbol-function 'mcp-server-lib-http--send-error)
                   (lambda (&rest _) (error "send-error boom"))))
          (mcp-server-lib-register-tool
           #'mcp-server-lib-http-test--abandon-tool
           :id "abandon_test_tool"
           :async t
           :description "Test tool that never answers.")
          (mcp-server-lib-start)
          (mcp-server-lib-http-start :port mcp-server-lib-http-test--port)
          (setq messages-before
                (with-current-buffer (messages-buffer) (buffer-string)))
          (setq proc (mcp-server-lib-http-test--connect))
          (mcp-server-lib-http-test--send
           proc
           (concat "{\"jsonrpc\":\"2.0\",\"id\":15,\"method\":\"tools/call\","
                   "\"params\":{\"name\":\"abandon_test_tool\",\"arguments\":{}}}"))
          (should
           (mcp-server-lib-http-test--wait
            (lambda ()
              (string-match-p
               "\\[MCP HTTP\\] Error sending deadline response"
               (substring (with-current-buffer (messages-buffer) (buffer-string))
                          (length messages-before))))
            5)))
      (when (and proc (process-live-p proc)) (delete-process proc))
      (when (and proc (buffer-live-p (process-buffer proc)))
        (kill-buffer (process-buffer proc)))
      (ignore-errors (mcp-server-lib-http-stop))
      (ignore-errors (mcp-server-lib-unregister-tool "abandon_test_tool"))
      (ignore-errors (mcp-server-lib-stop)))))

(ert-deftest mcp-server-lib-http-test-deadline-reports-armed-timeout ()
  "The deadline's own diagnostic must report the delay it actually waited,
not whatever `mcp-server-lib-http-async-timeout' holds when it fires.

Before the fix the lambda closed over the *symbol*, re-reading the
customizable variable at fire time; mutating it after arming -- as this
test does, and as a live `customize-set-variable' or a dynamically scoped
`let' elsewhere could -- made the reported number diverge from the delay
that actually elapsed.

Uses the stash-callback tool, not the abandon tool, so there is a
positive signal (the stash) that arming has already happened -- and
therefore already captured the timeout -- before the mutation below.
Mutating blind, with no such signal, would race the deferred dispatch:
land before arming and the mutated value would be the one captured,
proving nothing wrong even on the unfixed code."
  (setq mcp-server-lib-http-test--callback nil)
  (let ((mcp-server-lib-http-port mcp-server-lib-http-test--port)
        (mcp-server-lib-http-async-timeout 1)
        (proc nil)
        (messages-before nil))
    (unwind-protect
        (progn
          (mcp-server-lib-register-tool
           #'mcp-server-lib-http-test--slow-tool
           :id "slow_test_tool"
           :async t
           :description "Test tool that answers only when the test releases it.")
          (mcp-server-lib-start)
          (mcp-server-lib-http-start :port mcp-server-lib-http-test--port)
          (setq messages-before
                (with-current-buffer (messages-buffer) (buffer-string)))
          (setq proc (mcp-server-lib-http-test--connect))
          (mcp-server-lib-http-test--send
           proc
           (concat "{\"jsonrpc\":\"2.0\",\"id\":16,\"method\":\"tools/call\","
                   "\"params\":{\"name\":\"slow_test_tool\",\"arguments\":{}}}"))
          ;; the handler has stashed its callback, so the deadline is
          ;; already armed and has already captured the timeout in effect
          (should (mcp-server-lib-http-test--wait
                   (lambda () mcp-server-lib-http-test--callback)))
          ;; mutate the timeout now, after arming, before it fires
          (setq mcp-server-lib-http-async-timeout 999)
          (should (mcp-server-lib-http-test--wait
                   (lambda ()
                     (string-match-p
                      "\"error\"" (mcp-server-lib-http-test--received proc)))
                   5))
          (let ((logged (substring
                         (with-current-buffer (messages-buffer) (buffer-string))
                         (length messages-before))))
            (should (string-match-p "Async deadline reached after 1s" logged))
            (should-not
             (string-match-p "Async deadline reached after 999s" logged))))
      (when (and proc (process-live-p proc)) (delete-process proc))
      (when (and proc (buffer-live-p (process-buffer proc)))
        (kill-buffer (process-buffer proc)))
      (ignore-errors (mcp-server-lib-http-stop))
      (ignore-errors (mcp-server-lib-unregister-tool "slow_test_tool"))
      (ignore-errors (mcp-server-lib-stop)))))

(provide 'mcp-server-lib-http-test)
;;; mcp-server-lib-http-test.el ends here
