;; -*- mode: eask; lexical-binding: t -*-

(package "mcp-server-lib"
         "0.2.0"
         "Model Context Protocol server library")

(author "Laurynas Biveinis" "laurynas.biveinis@gmail.com")
(license "GPLv3")
(website-url "https://github.com/laurynas-biveinis/mcp-server-lib.el")
(keywords "comm" "tools")

(package-file "mcp-server-lib.el")
(files "mcp-server-lib-commands.el"
       "mcp-server-lib-ert.el"
       "mcp-server-lib-http.el"
       "emacs-mcp-stdio.sh")

;; Run both suites unconditionally -- `mcp-server-lib-test.el' fails to
;; parse on main (pre-existing, unrelated to the http transport), and
;; with `&&' a flake in the http suite skipped it outright, so its own
;; JUnit report was never even attempted.  `;' always attempts both;
;; the trailing `test' reports failure if either did.
(script "test" "eask test ert mcp-server-lib-http-test.el; s1=$?; eask test ert mcp-server-lib-test.el; s2=$?; test \"$s1\" -eq 0 -a \"$s2\" -eq 0")

(script "org-lint" 
        (concat "emacs -Q --batch "
                "--eval \"(require 'org)\" "
                "--eval \"(require 'org-lint)\" "
                "--eval \"(let ((all-checks-passed t) "
                        "(files '(\\\"README.org\\\" \\\"TODO.org\\\"))) "
                    "(dolist (file files all-checks-passed) "
                      "(with-temp-buffer "
                        "(insert-file-contents file) "
                        "(org-mode) "
                        "(let ((results (org-lint))) "
                          "(when results "
                            "(message \\\"Found issues in %s: %S\\\" file results) "
                            "(setq all-checks-passed nil))))) "
                    "(unless all-checks-passed (kill-emacs 1)))\""))

(source "gnu")
(source "melpa")

(depends-on "emacs" "27.1")
(depends-on "gptel" "0.9")
(depends-on "dash" "2.20.0")
(depends-on "lgr" "0")
(depends-on "simple-httpd" "1.6")
