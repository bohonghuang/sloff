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

(defvar sloff-reference-buffer nil)

(defvar sloff-buffer nil)

(defvar sloff-cursor-type 'box)

(defvar sloff-action-predicate 'sloff-insert-default-predicate)

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
    (internal-show-cursor nil nil)))

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
    (internal-show-cursor nil nil)))

(defun sloff-hl-line-highlight@around (function &rest args)
  (if (funcall sloff-action-predicate)
      (hl-line-unhighlight)
    (apply function args)))

(defun sloff-mode-enable ()
  (sloff-select-reference-buffer)
  (pop-to-buffer (setf sloff-buffer (get-buffer-create (concat "​" (buffer-name sloff-reference-buffer)))))
  (with-current-buffer sloff-buffer
    (erase-buffer)
    (cl-multiple-value-bind (mode content)
        (with-current-buffer sloff-reference-buffer
          (cl-values major-mode (buffer-substring (point-min) (point))))
      (insert content)
      (funcall mode)))
  (advice-add #'blink-cursor-timer-function :after #'sloff-blink-cursor-timer-function@after)
  (advice-add #'blink-cursor-start :after #'sloff-blink-cursor-start@after)
  (advice-add #'blink-cursor-end :after #'sloff-blink-cursor-end@after)
  (advice-add #'hl-line-highlight :around #'sloff-hl-line-highlight@around)
  (add-hook 'post-self-insert-hook #'sloff-buffer-insert-content))

(defvar sloff-kill-sloff-buffer-p t)

(defun sloff-mode-disable ()
  (when sloff-kill-sloff-buffer-p
    (when-let ((buffer sloff-buffer))
      (kill-buffer buffer)))
  (setf sloff-reference-buffer nil
        sloff-buffer nil)
  (advice-remove #'blink-cursor-timer-function #'sloff-blink-cursor-timer-function@after)
  (advice-remove #'blink-cursor-start #'sloff-blink-cursor-start@after)
  (advice-remove #'blink-cursor-end #'sloff-blink-cursor-end@after)
  (advice-remove #'hl-line-highlight #'sloff-hl-line-highlight@around)
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

(provide 'sloff)
;;; sloff.el ends here
