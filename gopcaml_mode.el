(defun fake-module-reload (module)
  (let ((tmpfile (make-temp-file
                  (file-name-nondirectory module) nil module-file-suffix)))
    (copy-file module tmpfile t)
    (module-load tmpfile)))

(add-to-list 'load-path (expand-file-name "./_build/default/"))
(require 'gopcaml)
(fake-module-reload "./_build/default/gopcaml.so")
(gopcaml-print-region)
(gopcaml-print-region)
(gopcaml-version)

(defgroup gopcaml-faces nil
  "Faces for gopcaml mode."
  :group 'gopcaml-mode
  :group 'faces)

(defface gopcaml-highlight
  '((((min-colors 88) (background dark))
     (:background "yellow1" :foreground "black"))
    (((background dark)) (:background "yellow" :foreground "black"))
    (((min-colors 88)) (:background "yellow1"))
    (t (:background "yellow")))
  "Highlight used for gopcaml-mode"
  :group 'gopcaml-faces)

(defun gopcaml-highlight-overlay ()
  "Example function - highlights a current expression"
  (interactive)
  (let ((area (gopcaml-get-enclosing-structure-bounds (point)))
	start end)
    (setq start (caar area))
    (setq end (cadar area))
    (goto-char start)
    (set-mark (point))
    (goto-char end)
    (activate-mark)
    ))

(defun gopcaml-mode-print-region (start end)
  (interactive "r")
  (gopcaml-print-region start end))

(provide 'gopcaml-mode)
