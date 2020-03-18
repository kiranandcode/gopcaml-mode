;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((nil
  ;; ensure that the init-file has been setup correctly
  (eval if (or (not (boundp 'gopcaml-dev-mode)) (not gopcaml-dev-mode))
	(error "Please ensure that your init file is setup as in the README."))
  ;; add project to load directory
  (eval
   when (and (current-buffer) (buffer-file-name (current-buffer)))
   (add-to-list 'load-path
		(expand-file-name
		 (file-name-directory
		  (buffer-file-name
		   (current-buffer)))))
   ;; setup gopcaml-mode to autoload
   (autoload 'gopcaml-mode
     (expand-file-name
      "gopcaml-mode"
      (file-name-directory (buffer-file-name (current-buffer))))
     nil t nil)
   )
  ;; global variable to track whether dev mode has been loaded for specific files
  (eval setq gopcaml-dev-mode-filemap
	(if (not (boundp 'gopcaml-dev-mode-filemap))
	    (make-hash-table) gopcaml-dev-mode-filemap))
  ;; if the current file has not be loaded into gopcaml-mode
  (eval if
  	(not (gethash (buffer-file-name (current-buffer))
		      gopcaml-dev-mode-filemap nil))
	  ;; and it is a ml file
	  (when (and (current-buffer)
		     (buffer-file-name (current-buffer))
		     (file-name-extension (buffer-file-name (current-buffer)))
		     (string-match "\\(ml\\|mli\\)"
		      (file-name-extension (buffer-file-name (current-buffer)))))
	    ;; then start gopcaml-mode
	    ;; (setting gopcaml-dev-mode-file to the name of this file)
	    (setq
	     gopcaml-dev-mode-file
	     (buffer-file-name (current-buffer)))
	    ;; gopcaml-mode will then initialize for this buffer and then record
	    ;; that the file specified by gopcaml-dev-mode-file is not being used
	    (gopcaml-mode)))
	;; otherwise for safety just clear to gopcaml-dev-mode-file variable
  	(setq gopcaml-dev-mode-file nil))
  ))



