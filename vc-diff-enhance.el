;;; vc-diff-enhance.el --- Enhancements to vc-diff. -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Darren Embry

;;; Commentary:

;;; Code:

;; (defun vc-diff (&optional historic not-urgent) ...)

(defface vc-diff-added-face
  '((default :inherit diff-added))
  "Face used to highlight added words."
  :group 'vc)

(defface vc-diff-removed-face
  '((default :inherit diff-removed))
  "Face used to highlight removed words."
  :group 'vc)

(defvar vc-diff--ignore-space nil)
(defvar vc-diff--unignore-space nil)
(defvar vc-diff--word-diff nil)

;; doesn't handle bundled options
(defun vc-diff-enhance/includes-option-p (curval &rest options)
  (if options
      (let* ((option (car options))
             (option-regex (concat "\\(\\`\\|\\ \\)" option "\\(\\'\\|\\ \\)")))
        (cond ((null curval) nil)
              ((and (listp curval) (member option curval)) t)
              ((and (stringp curval) (string-match option-regex curval)) t)
              (t (apply #'vc-diff-enhance/includes-option-p curval (cdr options)))))
    nil))

;; doesn't handle values other than nil, a string, or a list
(defun vc-diff-enhance/append-option-unconditionally (curval option)
  (cond ((listp curval)
         (append curval (list option)))
        ((equal curval "") option)      ;keep it a string if it's the empty string
        ((and (stringp curval)
              curval                    ;not nil?
              (> (length curval) 0))
         (concat curval " " option))
        ((null curval)
         (list option))
        (t curval)))

(defun vc-diff-enhance/remove-option (curval &rest options)
  (if options
      (let* ((option (car options))
             (option-regex (concat "\\(\\`\\|\\ \\)" option "\\(\\'\\|\\ \\)"))
             (result (cond ((listp curval)
                            (delete option curval))
                           ((and (stringp curval) curval)
                            (replace-regexp-in-string option-regex "\\1\\2" curval))
                           (t curval))))
        (apply #'vc-diff-enhance/remove-option result (cdr options)))
    curval))

(defun vc-diff-enhance/append-option (curval &rest options)
  (if (apply #'vc-diff-enhance/includes-option-p curval options)
      curval
    (vc-diff-enhance/append-option-unconditionally curval (car options))))

(defun vc-diff-enhance/append-word-diff-option (curval)
  (setq curval (vc-diff-enhance/append-option curval "--word-diff"))
  curval)
  
(defun vc-diff-enhance/append-ignore-white-space-options (curval)
  (setq curval (vc-diff-enhance/append-option curval "--ignore-cr-at-eol"))
  (setq curval (vc-diff-enhance/append-option curval "--ignore-space-at-eol"))
  (setq curval (vc-diff-enhance/append-option curval "-b" "--ignore-space-change"))
  (setq curval (vc-diff-enhance/append-option curval "-w" "--ignore-all-space"))
  (setq curval (vc-diff-enhance/append-option curval "--ignore-blank-lines"))
  curval)

(defun vc-diff-enhance/remove-ignore-white-space-options (curval)
  (setq curval (vc-diff-enhance/remove-option curval "--ignore-cr-at-eol"))
  (setq curval (vc-diff-enhance/remove-option curval "--ignore-space-at-eol"))
  (setq curval (vc-diff-enhance/remove-option curval "-b" "--ignore-space-change"))
  (setq curval (vc-diff-enhance/remove-option curval "-w" "--ignore-all-space"))
  (setq curval (vc-diff-enhance/remove-option curval "--ignore-blank-lines"))
  curval)

(defun vc-diff-enhance/fix-word-diff-buffer ()
  (when (and (boundp 'diff-vc-backend)
             (equal diff-vc-backend 'Git))
    (save-window-excursion
      (with-current-buffer "*vc-diff*"
        (let ((inhibit-read-only t))
          (while (re-search-forward "\\({\\(\\+\\)\\|\\[\\(-\\)\\)" nil t)
            (let* ((front-start (match-beginning 0))
                   (front-end   (match-end 0))
                   (addition-p  (match-beginning 2))
                   back-start
                   back-end)
              (cond (addition-p
                     (re-search-forward "\\+}" nil t)
                     (setq back-start (match-beginning 0))
                     (setq back-end (match-end 0)))
                    (t
                     (re-search-forward "\\-\\]" nil t)
                     (setq back-start (match-beginning 0))
                     (setq back-end (match-end 0))))
              (if addition-p
                  (overlay-put (make-overlay front-end back-start)
                               'face 'vc-diff-added-face)
                (overlay-put (make-overlay front-end back-start)
                             'face 'vc-diff-removed-face))
              ;; Make sure to delete back-to-front, because the text will shift.
              (delete-region back-start back-end)
              (delete-region front-start front-end)
              (goto-char front-end)))
          (goto-char (point-min)))))))

(defun vc-diff-enhance-advice (orig-fun &rest args)
  "Advice around `vc-diff' for `vc-diff-enhance'.

ORIG-FUN is the original `vc-diff' function.
HISTORIC and NOT-URGENT arguments are as in `vc-diff'."
  (if (boundp 'vc-git-diff-switches)
      (let ((result)
            (vc-git-diff-switches vc-git-diff-switches))
        (if vc-diff--ignore-space
            (setq vc-git-diff-switches (vc-diff-enhance/append-ignore-white-space-options vc-git-diff-switches)))
        (if vc-diff--unignore-space
            (setq vc-git-diff-switches (vc-diff-enhance/remove-ignore-white-space-options vc-git-diff-switches)))
        (if vc-diff--word-diff
            (setq vc-git-diff-switches (vc-diff-enhance/append-word-diff-option vc-git-diff-switches)))
        (setq result (apply orig-fun args))
        (if vc-diff--word-diff
            (vc-diff-enhance/fix-word-diff-buffer))
        result)
    (apply orig-fun args)))

(defun vc-diff-enhance/prefix-arg-to-flag (arg existing-value)
  (let ((numeric-value (prefix-numeric-value arg))
        (t-or-nil-value (if arg t nil)))
    (cond ((eq arg '-) nil)
          ((null arg)
           (cond ((not (or executing-kbd-macro noninteractive))
                  (not existing-value))
                 (t t-or-nil-value)))
          ((or (numberp arg)
               (and (listp arg) (numberp (car arg))))
           (cond ((< numeric-value 0) nil)
                 ((> numeric-value 0) t)
                 ((not (or executing-kbd-macro noninteractive))
                  (not existing-value))
                 (t t-or-nil-value)))
          (t t-or-nil-value))))

(defun vc-diff-toggle-ignore-space (&optional arg)
  "Turn `vc-diff-ignore-space' on or off.

When called with a negative numeric argument ARG or the symbol
`-', turns the flag off.

When called with a positive numeric argument ARG, turns the flag
on.

Otherwise, when called interactively, toggles the flag.

Otherwise, when called non-interactively, sets the flag to the
value of ARG."
  (interactive "P")
  (setq vc-diff--ignore-space (vc-diff-enhance/prefix-arg-to-flag
                               arg vc-diff--ignore-space))
  (if vc-diff--ignore-space
      (setq vc-diff--unignore-space nil))
  (message "Ignore Whitespace for vc-diff %s."
           (if vc-diff--ignore-space "enabled" "disabled")))

(defun vc-diff-toggle-unignore-space (&optional arg)
  "Turn `vc-diff-unignore-space' on or off.

When called with a negative numeric argument ARG or the symbol
`-', turns the flag off.

When called with a positive numeric argument ARG, turns the flag
on.

Otherwise, when called interactively, toggles the flag.

Otherwise, when called non-interactively, sets the flag to the
value of ARG."
  (interactive "P")
  (setq vc-diff--unignore-space (vc-diff-enhance/prefix-arg-to-flag
                                 arg vc-diff--unignore-space))
  (if vc-diff--unignore-space
      (setq vc-diff--ignore-space nil))
  (message "Unignore Whitespace for vc-diff %s."
           (if vc-diff--unignore-space "enabled" "disabled")))

(defun vc-diff-toggle-word-diff (&optional arg)
  "Turn `vc-diff-word-diff' on or off.

When called with a negative numeric argument ARG or the symbol
`-', turns the flag off.

When called with a positive numeric argument ARG, turns the flag
on.

Otherwise, when called interactively, toggles the flag.

Otherwise, when called non-interactively, sets the flag to the
value of ARG."
  (interactive "P")
  (setq vc-diff--word-diff (vc-diff-enhance/prefix-arg-to-flag
                            arg vc-diff--word-diff))
  (message "Word Diff for vc-diff %s."
           (if vc-diff--word-diff "enabled" "disabled")))

(global-set-key (kbd "C-x v S") #'vc-diff-toggle-ignore-space)
(global-set-key (kbd "C-x v Z") #'vc-diff-toggle-unignore-space)
(global-set-key (kbd "C-x v W") #'vc-diff-toggle-word-diff)

(advice-add 'vc-diff :around 'vc-diff-enhance-advice)

(provide 'vc-diff-enhance)
;;; vc-diff-enhance.el ends here
