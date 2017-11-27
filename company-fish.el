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
    (candidates (company-fish--candidates))
    (annotation (company-fish--annotation arg))
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
                (save-excursion
                  (beginning-of-line)
                  (skip-syntax-forward "w_")
                  (point)))))
      (when (or (s-prefix? "-" prefix) ;; command line option
                (s-equals? prefix cmd) ;; command or built in
                (s-prefix? "git" cmd)) ;; git command
        prefix))))

(defun company-fish--candidates ()
  (--map (-let [(cand . annot) it]
           (when annot
             (put-text-property 0 1 'annotation annot cand))
           cand)
         (company-fish--complete (buffer-substring (line-beginning-position) (point)))))

(defun company-fish--complete (raw-prompt)
  "Complete RAW-PROMPT (any string) using the fish shell.
Returns a list of candidates as either a string or a cons
cell (candidate . annotation)."
  (let* (;; Keep spaces at the end with OMIT-NULLS=nil in `split-string'.
         (toks (split-string raw-prompt split-string-default-separators nil))
         ;; The first non-empty `car' is the command.  Discard
         ;; leading empty strings.
         (tokens (progn (while (string= (car toks) "")
                          (setq toks (cdr toks)))
                        toks))
         ;; Fish does not support subcommand completion.  We make
         ;; a special case of 'sudo' and 'env' since they are
         ;; the most common cases involving subcommands.  See
         ;; https://github.com/fish-shell/fish-shell/issues/4093.
         (prompt (if (not (member (car tokens) '("sudo" "env")))
                     raw-prompt
                   (setq tokens (cdr tokens))
                   (while (and tokens
                               (or (string-match "^-.*" (car tokens))
                                   (string-match "=" (car tokens))))
                     ;; Skip env/sudo parameters, like LC_ALL=C.
                     (setq tokens (cdr tokens)))
                   (mapconcat 'identity tokens " ")))
         (candidates (--map (-let [(c a) (split-string it "\t")]
                              (cons c a))
                            (split-string
                             (with-output-to-string
                               (with-current-buffer standard-output
                                 (call-process company-fish-executable nil t nil "-c" (format "complete -C'%s'" prompt))))
                             "\n" t)))
         ;; Fish will return duplicate candidates with different annotations.
         ;; so we remove them. Generally the first candidates will have
         ;; the "least descriptive" annotation so reverse the list.
         (filtered (let ((-compare-fn (-lambda ((lhs) (rhs))
                                        (s-equals? lhs rhs))))
                     (-distinct (nreverse candidates)))))

    ;; Sort the candidates so that short options appear first
    (-sort (-lambda ((lhs) (rhs))
             (if (eq (s-prefix? "--" lhs)
                     (s-prefix? "--" rhs))
                 (s-less? lhs rhs)
               (s-prefix? "--" rhs)))
           filtered)))

(provide 'company-fish)
