;;; djr --- A collection of handy functions for me.
;;; Commentary:
;;; It's messy in here, trust me.

;;; Code:
(require 'yas-mage)

(defun djr/buffer-file-directory ()
  "Get the directory path from the current buffer."
  (file-name-directory (or (buffer-file-name) "")))

(defun djr/parent-directory ()
  "Get the directory name of the parent directory."
  (file-name-nondirectory (expand-file-name ".")))

(defun djr/grandparent-directory ()
  "Get the directory name of the grandparent directory."
  (file-name-nondirectory (expand-file-name "..")))

(defvar djr/last-prompt-built-list '()
  "The last list to be built with djr/prompt-build-list.")

(defun djr/prompt-build-list--format-prompt (prompt list)
  "Formats prompt PROMPT and list LIST for prompt-build-list."
  (format
   "%s (empty string to exit) %s: "
   prompt
   (format "[%s]" (mapconcat
                   #'identity
                   list
                   ", "))))

(defun djr/prompt-build-list--prompt (prompt list)
  "Prompts with PROMPT for list input LIST."
  (read-from-minibuffer (djr/prompt-build-list--format-prompt prompt list)))

(defun djr/prompt-build-list--add (list item)
  "Add string ITEM to the prompt list LIST, special logic for splitting on strings."
  (add-to-list 'list item t))

(defun djr/prompt-build-list (prompt &optional process-func format-func)
  "Prompt with PROMPT until empty string, return a list of responses, processed with PROCESS-FUNC using FORMAT-FUNC to format the list in the prompt."
  (or process-func (setq process-func #'identity))
  (or format-func (setq format-func #'identity))
  (let* ((inputs '())
        (in-string (djr/prompt-build-list--prompt prompt (seq-map format-func djr/last-prompt-built-list))))
    (while (not (equal in-string ""))
      (setq default nil)
      (setq inputs (djr/prompt-build-list--add inputs (funcall process-func in-string)))
      (setq in-string (djr/prompt-build-list--prompt prompt (seq-map format-func inputs))))
    (if inputs (setq djr/last-prompt-built-list inputs) djr/last-prompt-built-list)))

(provide 'djr '(yas-mage))
;;; djr.el ends here
