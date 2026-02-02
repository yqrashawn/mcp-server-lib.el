;;; mcp-server-lib-async-example.el --- Example of true non-blocking async using async.el -*- lexical-binding: t; -*-

;; This shows how to integrate async.el to avoid ANY blocking in the main Emacs thread.

;;; Code:

(require 'async)
(require 'mcp-server-lib)

;;; Option A: Run the entire tool handler in a subprocess

(defun mcp-server-lib--handle-tools-call-apply-async (id tool args-vals method-metrics)
  "Execute tool in a subprocess using async.el for true non-blocking behavior."
  (let* ((handler (plist-get tool :handler))
         (tool-name (plist-get tool :id))
         ;; Capture the current context that the subprocess needs
         (working-dir (if (fboundp '++workspace-current-project-root)
                          (++workspace-current-project-root)
                        default-directory)))
    
    ;; Start async subprocess
    (async-start
     ;; Code to run in subprocess
     `(lambda ()
        ;; Set up environment in subprocess
        (setq default-directory ,working-dir)
        ;; Load required libraries
        (require 'mcp-server-lib)
        ;; Call the tool handler synchronously in subprocess
        (apply #',handler ',args-vals))
     
     ;; Callback when subprocess finishes (runs in main Emacs, NON-BLOCKING)
     (lambda (result)
       ;; This callback is called when the subprocess completes
       ;; It runs in the main Emacs thread but does NOT block
       (mcp-server-lib--handle-tools-call-handle-result
        id args-vals tool result method-metrics)))))

;;; Option B: Hybrid approach - keep current polling but reduce timeout

;; For quick operations (< 30 seconds), use the current polling approach
;; For long operations, use async.el subprocess

(defun mcp-server-lib--is-long-running-tool-p (tool)
  "Check if TOOL is expected to be long-running."
  (let ((tool-name (plist-get tool :id)))
    ;; Add more patterns as needed
    (or (string-match-p "^run_shell_command$" tool-name)
        (string-match-p "^build" tool-name)
        (string-match-p "^test" tool-name))))

(defun mcp-server-lib--handle-tools-call-apply-hybrid (id tool args-vals method-metrics)
  "Use async.el for long-running tools, polling for quick tools."
  (if (mcp-server-lib--is-long-running-tool-p tool)
      ;; Long-running: use async.el subprocess (non-blocking)
      (mcp-server-lib--handle-tools-call-apply-async id tool args-vals method-metrics)
    ;; Quick tools: use current polling approach (may block briefly)
    (mcp-server-lib--handle-tools-call-apply id tool args-vals method-metrics)))

;;; Usage Example

;; To enable true non-blocking async for ALL tools:
;; (advice-add 'mcp-server-lib--handle-tools-call-apply
;;             :override #'mcp-server-lib--handle-tools-call-apply-async)

;; To enable hybrid approach (recommended):
;; (advice-add 'mcp-server-lib--handle-tools-call-apply
;;             :override #'mcp-server-lib--handle-tools-call-apply-hybrid)

(provide 'mcp-server-lib-async-example)
;;; mcp-server-lib-async-example.el ends here
