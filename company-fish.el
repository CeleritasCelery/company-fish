;;; company-fish.el --- Fish backend for company-mode  -*- lexical-binding: t; -*-

(require 'company)
(require 'dash)
(require 's)
(require 'cl-lib)

(defvar company-fish-executable "fish"
  "The `fish' executable.")

(defvar company-fish-enabled-modes '(shell-mode eshell-mode) "enabled modes.")

(defun company-fish (command &optional arg &rest ignored)
  "Complete shell commands and options using Fish shell. See `company's COMMAND ARG and IGNORED for details."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-fish))
    (prefix (company-fish--prefix))
    (candidates (cons :async (lambda (callback) (company-fish--candidates callback))))
    (annotation (company-fish--annotation arg))
    (meta (get-text-property 0 'annotation arg))
    (sorted t)))

(defun company-fish--annotation (candidate)
  (-let [annotation (get-text-property 0 'annotation candidate)]
    (when annotation
      (format " (%s)" annotation))))

(defun company-fish--prefix ()
  (when (and (-contains? company-fish-enabled-modes major-mode)) ;; not inside string
    (let ((prefix (company-grab-symbol))
          (cmd (buffer-substring
                (line-beginning-position)
                (point))))
      (cond ((s-prefix? "-" prefix) (cons prefix t)) ;; command line option
            ((s-equals? prefix cmd) prefix)))));; command

(defun company-fish--bol ()
  (cl-case major-mode
    (shell-mode (comint-bol))
    (eshell-mode (eshell-bol))
    (otherwise (beginning-of-line))))

(defun company-fish--line-beginning-position ()
  (save-excursion (company-fish--bol) (point)))

(defun company-fish--candidates (callback)
  (let* (;; Keep spaces at the end with OMIT-NULLS=nil in `split-string'.
         (raw-prompt (buffer-substring (company-fish--line-beginning-position)
				       (point)))
         (tokens (split-string raw-prompt split-string-default-separators nil))
         ;; Fish does not support subcommand completion.  We make
         ;; a special case of 'sudo' and 'env' since they are
         ;; the most common cases involving subcommands.  See
         ;; https://github.com/fish-shell/fish-shell/issues/4093.
         (prompt (if (or (eq 1 (length tokens))
                         (not (member (car tokens) '("sudo" "env"))))
                     raw-prompt
                   (setq tokens (cdr tokens))
                   (while (and tokens
                               (or (string-match "^-.*" (car tokens))
                                   (string-match "=" (car tokens))))
                     ;; Skip env/sudo parameters, like LC_ALL=C.
                     (setq tokens (cdr tokens)))
                   (mapconcat 'identity tokens " ")))
         (buffer (generate-new-buffer "fish-candidates")))
    (set-process-sentinel (start-process  "fish-candidate"
                                          buffer
                                          company-fish-executable
                                          (format "-c complete -C'%s'" prompt))
                          (lambda (_ event)
                            (when (s-equals? event "finished\n")
                              (funcall callback (company-fish--parse buffer)))))))

(defun company-fish--parse (buffer)
  (let ((str (s-trim (with-current-buffer buffer
                       (buffer-string))))
        (-compare-fn (-lambda ((lhs) (rhs))
                       (s-equals? lhs rhs))))
    (kill-buffer buffer)
    (when (s-present? str)
      (->> (s-lines str)
           (--map (-let [(c a) (split-string it "\t")]
                    (cons c a)))
           (nreverse)
           (-distinct)
           (-sort (-lambda ((lhs) (rhs))
                    (if (eq (s-prefix? "--" lhs)
                            (s-prefix? "--" rhs))
                        (s-less? lhs rhs)
                      (s-prefix? "--" rhs))))
           (--map (-let [(cand . annot) it]
                    (when annot
                      (put-text-property 0 1 'annotation annot cand))
                    cand))))))

(provide 'company-fish)
