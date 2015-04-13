(require 'cl-lib)

(eval-and-compile
 (defvar ess-julia-extensions-directory
   (directory-file-name
    (file-name-directory
     (if (and (boundp 'load-file-name) load-file-name) ;; A nice default
         (file-truename load-file-name)
       (error "could not establish directory for ESS-julia-extensions"))))
   "Directory of this file. Necessary for loading the ESSx module."))

(defun julia-active-module-path (position)
  "Return a list of strings that designates the path of the active module at POSITION. For example, '(\"Foo\" \"Bar\") would be returned when Foo.Bar.

Useful for evaluating Julia code in a region within a given module."
  ;; FIXME this is a quick fix, and does not handle sub/nested modules.
  ;; A proper solution would be parsing the module structure, wait until
  ;; julia-mode is merged into ESS.
  (let* ((module-regexp "module\s-*\\(\\sw*\\)"))
    (save-excursion
      (goto-char position)
      (when (search-backward-regexp module-regexp nil t)
        (list (match-string-no-properties 1))))))

(defun julia-module-path-string (module-path)
  "Convert a module path to a string that can be parsed by the Julia process.

For example, '(\"Foo\" \"Bar\") => \"[:Foo :Bar]\""
  (format "[%s]"
          (cl-reduce (lambda (x y)
                       (format "%s, %s" x y))
                     module-path
                     :key (lambda (m)
                            (format ":%s" m)))))

(defun julia-ensure-module (process module file)
  "Ensures that MODULE is defined in PROCESS, loading FILE if necessary.

NOTE: This is necessary until workspace() in Julia is fixed to
reload the REPL interaction interface."
  (ess-send-string process (format "isdefined(:%s) || include(%S)" module file)))

(defun julia-escape-string (string)
  "Escape characters in a string so that it can be passed to `Base.include_string` in Julia. Also wraps string in double quotes.

The following are escaped: double quotes, $ (interpolation)."
  (with-output-to-string
    (princ "\"")
    (mapc (lambda (c)
            (princ
             (case c
               (?$ "\\$")
               (?\" "\\\"")
               (otherwise (string c)))))
          string)
    (princ "\"")))

(defun julia-send-region (process start end)
  "Send the region between START and END to a Julia process. Evaluated in the current module when applicable, uses the correct line numbers."
  (let* ((line (line-number-at-pos start))
         (modpath (julia-active-module-path start)) ; FIXME see note for function
         (file buffer-file-truename)
         (modpath-string (if modpath
                             (concat ", "
                                     (julia-module-path-string modpath))
                           ""))
         (code (buffer-substring-no-properties start end))
         (string (format "ESSx.eval_string(%s, %d, %S%s)"
                         (julia-escape-string code) line file modpath-string)))
    (julia-ensure-module process "ESS"
                         (format "%sess-julia.jl" ess-etc-directory))
    (julia-ensure-module process "ESSx"
                         (format "%s/ess-julia-extensions.jl"
                                 ess-julia-extensions-directory))
    (ess-send-string process string)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; code below this line is for experimentating with these extensions,
;;; and not meant to be integrated into ESS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun julia--eval-region (start end)
  "Evaluate a region in the current process. Ensures that there is a process and takes care of aesthetics (blinking, etc). "
  ;; FIXME is this the right way to ensure process is running?
  (ess-force-buffer-current)
  (ess-blink-region start end)
  (julia-send-region (ess-get-process ess-current-process-name) start end))

(defun julia-eval-region (start end)
  "Evaluate the active region."
  ;; FIXME should this test for MARK-ACTIVE?
  (interactive "r")
  (julia--eval-region start end)
  (if (and (fboundp 'deactivate-mark) ess-eval-deactivate-mark)
      (deactivate-mark)))

(defun julia-eval-buffer ()
  "Send the current buffer to the inferior Julia process."
  (interactive)
  (julia--eval-region (point-min) (point-max)))

(defun julia-eval-line ()
  "Evaluate the current line of code."
  (interactive)
  (julia--eval-region (line-beginning-position) (line-end-position)))

(defun julia-eval-paragraph ()
  "Evaluate the paragraph that contains the point."
  (interactive)
  (save-excursion
    (forward-paragraph)
    (let ((end (point)))
      (backward-paragraph)
      (julia--eval-region (point) end))))

(defun julia-eval-dwim (&optional line-p)
  "Evaluate the region when active, otherwise the paragraph that contains the point."
  (interactive)
  (if (and transient-mark-mode mark-active
           (> (region-end) (region-beginning)))
      (let ((end (region-end)))
        (julia--eval-region (region-beginning) end)
        (goto-char end))
    (if line-p
        (progn
          (julia-eval-line)
          (ess-next-code-line 1))
      (julia-eval-paragraph))))

(defun customize-julia-extensions ()
  "Establish bindings for experimenting with REPL interaction from a Julia code buffer."
  (interactive)
  (local-set-key (kbd "<C-return>") (lambda () (interactive)
                                      (julia-eval-dwim t)))
  (local-set-key (kbd "C-c <C-return>") 'julia-eval-line)
  (local-set-key (kbd "C-c C-r") 'julia-eval-region)
  (local-set-key (kbd "C-c C-c") 'julia-eval-dwim)
  (local-set-key (kbd "C-c C-b") 'julia-eval-buffer))

(add-hook 'julia-mode-hook 'customize-julia-extensions)
