;;; uniformat.el --- Unified formatting interface. -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Taiki Sugawara

;; Author: Taiki Sugawara <buzz.taiki@gmail.com>
;; Keywords: convenience

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

;;; Code:

(defgroup uniformat ()
  "Unified formatting interface."
  :group 'convenience
  :prefix "uniformat-")

(defcustom uniformat-format-buffer-functions '(uniformat-eglot-format-buffer-maybe
                                               uniformat-format-buffer-by-mode)
  "A list of formating buffer functions.
Functions are called by no argument in the order given until one of them returns non-nil."
  :type '(list function))

(defcustom uniformat-format-region-functions '(uniformat-eglot-format-region-maybe
                                               uniformat-format-region-by-mode)
  "A list of formating region functions.
Functions are called by two arguments (START END) in the order given until one of them returns non-nil."
  :type '(list function))


;;;###autoload
(defcustom uniformat-mode-formatters nil
  "An alist mapping major modes to their formatter functions.

Each entry can be either:
- A single function that formats the entire buffer.
- A cons cell that form (BUFFER-FN . REGION-FN),
  where BUFFER-FN is a function that formats the entire buffer,
  and REGION-FN is a function that formats a region given two arguments (START END)."
  :type '(alist :key-type symbol :value-type (choice
                                              (function :tag "Formatting buffer function")
                                              (list :tag "Formatting buffer and region functions"
                                                    (choice :tag "Buffer" (const nil) function)
                                                    (choice :tag "Region" (const nil) function)))))

;;;###autoload
(defun uniformat-buffer ()
  "Format current buffer."
  (interactive)
  (let ((formatted (run-hook-with-args-until-success 'uniformat-format-buffer-functions)))
    (when (and (not formatted) (called-interactively-p))
      (message "No formatter found."))))

;;;###autoload
(defun uniformat-region (start end)
  "Format region between START and END."
  (interactive "r")
  (unless (run-hook-with-args-until-success 'uniformat-format-region-functions start end)
    (message "No formatter found.")))

;;;###autoload
(defun uniformat ()
  "Format current buffer or region DWIM."
  (interactive)
  (call-interactively (if (use-region-p) #'uniformat-region #'uniformat-buffer)))

;;;###autoload
(define-minor-mode uniformat-buffer-on-save-mode
  "A minor mode to format buffer on save."
  :global nil
  (if uniformat-buffer-on-save-mode
      (add-hook 'before-save-hook #'uniformat-buffer nil t)
    (remove-hook 'before-save-hook #'uniformat-buffer t)))

(defun uniformat-buffer-on-save-mode-turn-on ()
  "Turn on `uniformat-buffer-on-save-mode'."
  (uniformat-buffer-on-save-mode 1))

;;;###autoload
(define-globalized-minor-mode global-uniformat-buffer-on-save-mode uniformat-buffer-on-save-mode uniformat-buffer-on-save-mode-turn-on)

;; mode based formatter
(defun uniformat-format-buffer-by-mode ()
  "Format current buffer by major mode according to `uniformat-mode-formatters'."
  (let ((formatter (pcase (alist-get major-mode uniformat-mode-formatters)
                     (`(,fn . ,_) fn)
                     (fn fn))))
    (when formatter
      (funcall formatter)
      t)))

(defun uniformat-format-region-by-mode (start end)
  "Format current region between START and END by major mode according to `uniformat-mode-formatters'."
  (let ((formatter (pcase (alist-get major-mode uniformat-mode-formatters)
                     (`(,_ ,fn . ,_) fn)
                     (_ nil))))
    (when formatter
      (funcall formatter start end)
      t)))


;; eglot support
(require 'eglot)

(defun uniformat-eglot-format-buffer-maybe ()
  "Format current buffer using eglot if the buffer is managed by eglot and a
formatter is provided by language server.

Returns non-nil if formatter was called.  Otherwise, returns nil."
  (when (and (eglot-managed-p)
             (eglot-server-capable '(:textDocument/formatting :documentFormattingProvider nil)))
    (ignore-errors (eglot-code-action-organize-imports (point-min) (point-max)))
    (eglot-format-buffer)
    t))

(defun uniformat-eglot-format-region-maybe (start end)
  "Format current region between START and END using eglot if the buffer is managed by eglot and a
formatter is provided by language server.

Returns non-nil if formatter was called.  Otherwise, returns nil."
  (when (and (eglot-managed-p)
             (let ((range '(:start ,(eglot--pos-to-lsp-position start) :end ,(eglot--pos-to-lsp-position end))))
               (eglot-server-capable `(:textDocument/rangeFormatting :documentRangeFormattingProvider (:range ,range)))))
    (ignore-errors (eglot-code-action-organize-imports start end))
    (eglot-format start end)
    t))


(provide 'uniformat)
;;; uniformat.el ends here
