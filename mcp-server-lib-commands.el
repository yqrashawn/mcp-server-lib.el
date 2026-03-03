;;; mcp-server-lib-commands.el --- User commands for MCP server -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Laurynas Biveinis

;; This file is part of mcp-server-lib.el.

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file provides interactive user commands for the MCP server library.

;;; Code:

(require 'mcp-server-lib)

;;;###autoload
(defun mcp-server-lib-start ()
  "Start the MCP server and begin handling client requests.

This function starts the MCP server that can process JSON-RPC
requests via `mcp-server-lib-process-jsonrpc'.  Once started, the server
will dispatch incoming requests to the appropriate tool
handlers that have been registered with `mcp-server-lib-register-tool'.

See also: `mcp-server-lib-stop'"
  (interactive)
  (when mcp-server-lib--running
    (error "MCP server is already running"))

  (when (called-interactively-p 'any)
    (message "Emacs starting handling MCP requests"))
  (setq mcp-server-lib--running t))

;;;###autoload
(defun mcp-server-lib-stop ()
  "Stop the MCP server from processing client requests.

Sets the server state to stopped, which prevents further processing of
client requests.  Cancels all pending async operations and their timers
to ensure clean shutdown.

See also: `mcp-server-lib-start'"
  (interactive)
  (unless mcp-server-lib--running
    (error "MCP server is not running"))

  (when (called-interactively-p 'any)
    (message "Emacs stopping handling MCP requests"))
  
  ;; Cancel all pending async operation timers
  (dolist (op mcp-server-lib--pending-async-operations)
    (let ((timer (cdr op)))
      (when (timerp timer)
        (cancel-timer timer))))
  (setq mcp-server-lib--pending-async-operations nil)
  
  ;; Mark server as not running
  (setq mcp-server-lib--running nil)
  t)

;;; Script Installation

(defun mcp-server-lib--package-script-path ()
  "Return the path to emacs-mcp-stdio.sh in the package directory.
Returns nil if not found."
  (let* ((library-path (locate-library "mcp-server-lib"))
         (package-dir
          (and library-path (file-name-directory library-path)))
         (script-path
          (and package-dir
               (expand-file-name "emacs-mcp-stdio.sh" package-dir))))
    (when (and script-path (file-exists-p script-path))
      script-path)))

(defun mcp-server-lib--installed-script-path ()
  "Return the path where the script should be installed."
  (expand-file-name "emacs-mcp-stdio.sh"
                    mcp-server-lib-install-directory))

;;;###autoload
(defun mcp-server-lib-install ()
  "Install emacs-mcp-stdio.sh to `mcp-server-lib-install-directory'."
  (interactive)
  (let ((source (mcp-server-lib--package-script-path))
        (target (mcp-server-lib--installed-script-path)))
    (unless source
      (error "Cannot find emacs-mcp-stdio.sh in package directory"))
    (when (file-exists-p target)
      (unless (yes-or-no-p
               (format "File already exists at %s. Overwrite? "
                       target))
        (user-error "Installation cancelled")))
    (make-directory (file-name-directory target) t)
    (copy-file source target t)
    (set-file-modes target #o755)
    (message "Script installed to: %s" target)))

;;;###autoload
(defun mcp-server-lib-uninstall ()
  "Remove installed emacs-mcp-stdio.sh from `mcp-server-lib-install-directory'."
  (interactive)
  (let ((target (mcp-server-lib--installed-script-path)))
    (unless (file-exists-p target)
      (user-error "No script found at: %s" target))
    (when (yes-or-no-p (format "Remove script at %s? " target))
      (delete-file target)
      (message "Script removed from: %s" target))))


(provide 'mcp-server-lib-commands)

;; Local Variables:
;; byte-compile-warnings: (not unresolved)
;; package-lint-main-file: "mcp-server-lib.el"
;; End:

;;; mcp-server-lib-commands.el ends here
