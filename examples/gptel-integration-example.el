;;; gptel-integration-example.el --- Example of gptel integration with mcp-server-lib -*- lexical-binding: t; -*-

;; This file demonstrates how to use gptel tools with mcp-server-lib

;;; Commentary:

;; This example shows three ways to register tools:
;; 1. Using mcp-server-lib-register-tool with gptel schema
;; 2. Defining a gptel tool and registering it with MCP
;; 3. Using both gptel and MCP with the same tool definition

;;; Code:

(require 'mcp-server-lib)
(require 'gptel)

;;;; Example 1: Direct registration with gptel schema

(defun get-buffer-content (buffer-name)
  "Return the content of BUFFER-NAME."
  (mcp-server-lib-with-error-handling
    (if-let ((buf (get-buffer buffer-name)))
        (with-current-buffer buf
          (buffer-string))
      (mcp-server-lib-tool-throw
       (format "Buffer '%s' not found" buffer-name)))))

(mcp-server-lib-register-tool #'get-buffer-content
  :id "get_buffer_content"
  :description "Get the full content of an Emacs buffer by name"
  :args (list '(:name "buffer_name"
               :type string
               :description "Name of the buffer to read"))
  :read-only t)

;;;; Example 2: gptel tool registered with MCP

;; First, define a gptel tool
(gptel-make-tool
 :function (lambda (filename)
            (if (file-exists-p filename)
                (with-temp-buffer
                  (insert-file-contents filename)
                  (buffer-string))
              (format "File '%s' not found" filename)))
 :name "read_file"
 :description "Read the contents of a file from the filesystem"
 :args (list '(:name "filename"
              :type string
              :description "Absolute path to the file to read"))
 :category "filesystem")

;; Now register it with MCP
(mcp-server-lib-register-gptel-tool
 (gptel-get-tool "read_file"))

;;;; Example 3: Tool with multiple arguments and optional parameters

(gptel-make-tool
 :function (lambda (directory pattern recursive)
            (let ((default-directory directory)
                  (files '()))
              (if recursive
                  (setq files (directory-files-recursively directory pattern))
                (setq files (directory-files directory t pattern)))
              (mapconcat #'identity files "\n")))
 :name "list_files"
 :description "List files in a directory, optionally recursively"
 :args (list '(:name "directory"
              :type string
              :description "Directory path to list files from")
         '(:name "pattern"
              :type string
              :description "Regular expression pattern to filter files"
              :optional t)
         '(:name "recursive"
              :type string
              :description "Whether to search recursively"
              :optional t
              :enum ["true" "false"]))
 :category "filesystem")

(mcp-server-lib-register-gptel-tool
 (gptel-get-tool "list_files"))

;;;; Example 4: Tool with enum for limited choices

(defun get-emacs-info (info-type)
  "Get various types of Emacs information."
  (pcase info-type
    ("version" emacs-version)
    ("user" user-login-name)
    ("directory" user-emacs-directory)
    ("features" (format "%S" features))
    (_ (format "Unknown info type: %s" info-type))))

(mcp-server-lib-register-tool #'get-emacs-info
  :id "get_emacs_info"
  :description "Get various information about the running Emacs instance"
  :args (list '(:name "info_type"
               :type string
               :description "Type of information to retrieve"
               :enum ["version" "user" "directory" "features"]))
  :read-only t)

;;;; Example 5: Complex tool with nested structure

(defun search-org-headings (file-path keyword max-results)
  "Search for org headings containing KEYWORD in FILE-PATH."
  (mcp-server-lib-with-error-handling
    (unless (file-exists-p file-path)
      (mcp-server-lib-tool-throw
       (format "File not found: %s" file-path)))

    (let ((results '())
          (max (string-to-number (or max-results "10"))))
      (with-temp-buffer
        (insert-file-contents file-path)
        (org-mode)
        (goto-char (point-min))
        (while (and (< (length results) max)
                   (re-search-forward
                    (format "^\\*+ .*%s.*$" (regexp-quote keyword))
                    nil t))
          (push (match-string 0) results)))
      (if results
          (mapconcat #'identity (nreverse results) "\n")
        (format "No headings found matching '%s'" keyword)))))

(mcp-server-lib-register-tool #'search-org-headings
  :id "search_org_headings"
  :description "Search for org-mode headings containing a keyword"
  :args (list '(:name "file_path"
               :type string
               :description "Path to the org file to search")
          '(:name "keyword"
               :type string
               :description "Keyword to search for in headings")
          '(:name "max_results"
               :type string
               :description "Maximum number of results to return"
               :optional t))
  :read-only t)

;;;; Starting the MCP server

;; To start the server with these tools:
;; (mcp-server-lib-start)

;; Then test with:
;; (mcp-server-lib-process-jsonrpc-parsed
;;  (mcp-server-lib-create-tools-list-request))

(provide 'gptel-integration-example)
;;; gptel-integration-example.el ends here
