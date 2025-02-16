;;; sloff.el --- A great helper for slacking off at work in Emacs. -*- lexical-binding: t -*

;;; Commentary:

;; This package allows the contents of one buffer to increase as you type
;; in other buffers, as if you were editing that buffer. Additionally, it
;; can dim the focus of other buffers, making it appear as though you are
;; not editing them. This enables you to pretend you are editing one
;; buffer while actually doing other things in Emacs. Make this package
;; your ultimate tool for slacking off at work!

;;; Code:

(require 'cl-lib)
(require 'hl-line)
(require 'corfu)

(defvar sloff-reference-buffer nil)

(defvar sloff-buffer nil)

(defvar sloff-cursor-type 'box)

(defvar sloff-action-predicate 'sloff-insert-default-predicate)

(defcustom sloff-show-cursor-in-selected-windows-p nil
  "Non-nil means Sloff mode will not hide the cursor in selected window."
  :group 'sloff)

(defun sloff-insert-default-predicate ()
  (and (not (eq (current-buffer) sloff-buffer)) (not (minibufferp))))

(defun sloff-select-reference-buffer ()
  (interactive)
  (setf sloff-reference-buffer (get-buffer (read-buffer "Select a reference buffer: "))))

(defun sloff-buffer-insert-content ()
  (when (funcall sloff-action-predicate)
    (when-let ((window (get-buffer-window sloff-buffer)))
      (if-let ((content (with-current-buffer sloff-reference-buffer
                          (when-let ((beg (point))
                                     (end (ignore-errors
                                            (forward-char 1)
                                            (point))))
                            (buffer-substring beg end)))))
          (with-selected-window window
            (insert content)
            (hl-line-highlight))
        (progn
          (setf (window-buffer window) sloff-reference-buffer)
          (sloff-mode -1))))))

(defun sloff-buffer-update ()
  (when-let ((window (get-buffer-window sloff-buffer)))
    (force-window-update window)))

(defun sloff-blink-cursor-timer-function@after ()
  (when (funcall sloff-action-predicate)
    (with-current-buffer sloff-buffer
      (setf cursor-in-non-selected-windows (if cursor-in-non-selected-windows nil sloff-cursor-type))
      (sloff-buffer-update))
    (unless sloff-show-cursor-in-selected-windows-p
      (internal-show-cursor nil nil))))

(defun sloff-blink-cursor-start@after ()
  (when (funcall sloff-action-predicate)
    (with-current-buffer sloff-buffer
      (setf cursor-in-non-selected-windows nil)
      (sloff-buffer-update))))

(defun sloff-blink-cursor-end@after ()
  (when (funcall sloff-action-predicate)
    (with-current-buffer sloff-buffer
      (setf cursor-in-non-selected-windows sloff-cursor-type)
      (sloff-buffer-update))
    (unless sloff-show-cursor-in-selected-windows-p
      (internal-show-cursor nil nil))))

(defun sloff-hl-line-highlight@around (function &rest args)
  (if (funcall sloff-action-predicate)
      (hl-line-unhighlight)
    (apply function args)))

(defun sloff-corfu--popup-show@around (function pos &rest args)
  (if-let ((window (get-buffer-window sloff-buffer)))
      (with-selected-window window
        (apply function (posn-at-point (window-point window) window) args))
    (apply function pos args)))

(defun sloff-mode-enable ()
  (sloff-select-reference-buffer)
  (setf sloff-buffer (get-buffer-create (concat "​" (buffer-name sloff-reference-buffer))))
  (if-let* ((window (get-buffer-window sloff-reference-buffer)))
      (setf (window-buffer window) sloff-buffer)
    (pop-to-buffer sloff-buffer))
  (with-current-buffer sloff-buffer
    (erase-buffer)
    (cl-multiple-value-bind (mode content)
        (with-current-buffer sloff-reference-buffer
          (cl-values major-mode (buffer-substring (point-min) (point))))
      (insert content)
      (funcall mode))
    (when-let ((window (get-buffer-window sloff-buffer)))
      (setf (window-point window) (point-max))))
  (advice-add #'blink-cursor-timer-function :after #'sloff-blink-cursor-timer-function@after)
  (advice-add #'blink-cursor-start :after #'sloff-blink-cursor-start@after)
  (advice-add #'blink-cursor-end :after #'sloff-blink-cursor-end@after)
  (advice-add #'hl-line-highlight :around #'sloff-hl-line-highlight@around)
  (advice-add #'corfu--popup-show :around #'sloff-corfu--popup-show@around)
  (add-hook 'post-self-insert-hook #'sloff-buffer-insert-content))

(defvar sloff-kill-sloff-buffer-p t)

(defun sloff-mode-disable ()
  (when sloff-kill-sloff-buffer-p
    (when-let ((buffer sloff-buffer))
      (when-let ((window (get-buffer-window buffer)))
        (setf (window-buffer window) sloff-reference-buffer))
      (kill-buffer buffer)))
  (setf sloff-reference-buffer nil
        sloff-buffer nil)
  (advice-remove #'blink-cursor-timer-function #'sloff-blink-cursor-timer-function@after)
  (advice-remove #'blink-cursor-start #'sloff-blink-cursor-start@after)
  (advice-remove #'blink-cursor-end #'sloff-blink-cursor-end@after)
  (advice-remove #'hl-line-highlight #'sloff-hl-line-highlight@around)
  (advice-remove #'corfu--popup-show #'sloff-corfu--popup-show@around)
  (remove-hook 'post-self-insert-hook #'sloff-buffer-insert-content))

(define-minor-mode sloff-mode
  "Minor mode to slack off at work time."
  :global t
  :group 'sloff
  (if sloff-mode (condition-case nil (sloff-mode-enable) (quit (sloff-mode -1))) (sloff-mode-disable)))

(defun sloff-emergency-quit ()
  (interactive)
  (with-temp-message (or (current-message) "")
    (let ((sloff-kill-sloff-buffer-p nil)
          (buffer sloff-buffer))
      (sloff-mode -1)
      (when-let ((window (get-buffer-window buffer)))
        (select-window window)))))

(defun sloff-emergency-switch ()
  (interactive)
  (when-let ((buffer sloff-buffer))
    (if-let ((window (get-buffer-window buffer)))
        (select-window window)
      (pop-to-buffer buffer))
    (delete-other-windows)))

(defun sloff-explore-insert-content ()
  (when sloff-mode (sloff-buffer-insert-content)))

(defun sloff-explore-mode-enable ()
  (add-hook 'post-command-hook #'sloff-explore-insert-content nil t)
  (set (make-local-variable 'cursor-type) 'hbar)
  (set (make-local-variable 'sloff-show-cursor-in-selected-windows-p) t))

(defun sloff-explore-mode-disable ()
  (remove-hook 'post-command-hook #'sloff-explore-insert-content t)
  (kill-local-variable 'cursor-type)
  (kill-local-variable 'sloff-show-cursor-in-selected-windows-p))

(define-minor-mode sloff-explore-mode
  "Minor mode to slack off for exploring your own content at work time."
  :group 'sloff
  (if sloff-explore-mode (sloff-explore-mode-enable) (sloff-explore-mode-disable)))

(cl-defun sloff-move-to-window-line (&optional (percent 0.5))
  (let ((recenter-positions (list percent)))
    (move-to-window-line-top-bottom)))

(defun sloff-fontify-no-font-lock-buffers ()
  (cl-flet ((circular-list (&rest elems)
              (when elems
                (let ((list (cl-copy-list elems)))
                  (setf (cdr (last list)) list)))))
    (cl-loop with window-list = (window-list-1 (frame-first-window) 'ignore)
             with window-ring = (apply #'circular-list window-list)
             initially (unless (> (length window-list) 1) (cl-return))
             for windows on window-ring
             for (previous . next-windows) = windows
             for (current) = next-windows
             do (with-selected-window current
                  (unless font-lock-mode
                    (save-excursion
                      (cl-loop with line-start = (progn (sloff-move-to-window-line 0.0) (line-number-at-pos))
                               and line-end = (progn (sloff-move-to-window-line 1.0) (line-number-at-pos))
                               for line from line-start to line-end
                               for line-count from 0
                               do
                               (goto-char (point-min))
                               (forward-line (1- line))
                               (cl-loop with line = (line-number-at-pos)
                                        and words = (apply
                                                     #'circular-list
                                                     (with-selected-window previous
                                                       (save-excursion
                                                         (sloff-move-to-window-line 1.0)
                                                         (previous-line (min line-count (1- (line-number-at-pos))))
                                                         (or
                                                          (save-excursion
                                                            (cl-loop with line = (line-number-at-pos)
                                                                     initially (beginning-of-line) (back-to-indentation)
                                                                     for word-start = (if (and (forward-word +1) (forward-word -1)) (point) (cl-return words))
                                                                     for word-end = (if (forward-word +1) (point) (cl-return words))
                                                                     while (= (line-number-at-pos) line)
                                                                     collect (buffer-substring word-start word-end) into words
                                                                     finally (cl-return words)))
                                                          (save-excursion
                                                            (back-to-indentation)
                                                            (list (buffer-substring (point) (line-end-position))))))))
                                        initially (beginning-of-line) (back-to-indentation)
                                        for word in words
                                        for word-start = (point)
                                        for word-end = (if (forward-word +1) (point) (cl-return))
                                        do (let ((buffer-read-only nil))
                                             (ignore-error text-read-only (put-text-property word-start word-end 'face (get-text-property 0 'face word))))
                                        while (= (line-number-at-pos) line))))
                    (with-selected-window previous)))
             until (eq next-windows window-ring))))

(defvar sloff-fontify-timer nil)

(defvar sloff-fontify-delay 0.5)

(define-minor-mode sloff-fontify-mode
  "Minor mode to fontify windows without font-lock to make their appearance similar to other windows."
  :global t
  :group 'sloff
  (if sloff-fontify-mode
      (cl-assert (null (cl-shiftf sloff-fontify-timer (run-with-idle-timer sloff-fontify-delay t #'sloff-fontify-no-font-lock-buffers))))
    (cancel-timer (cl-shiftf sloff-fontify-timer nil))))

(provide 'sloff)
;;; sloff.el ends here
