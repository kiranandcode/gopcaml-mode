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



(defun gopcaml-mode-print-region (start end)
  (interactive "r")
  (gopcaml-print-region start end))

(provide 'gopcaml-mode)
