;; EMACS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages
(add-to-list 'load-path "~/.emacs.d/packages")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Backup NONE

(setq make-backup-files         nil) ; Don't want any backup files
(setq auto-save-list-file-name  nil) ; Don't want any .saves files
(setq auto-save-default         nil) ; Don't want any auto saving

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TABS 

;; Dont use mouse wheel
(setq mouse-wheel-mode nil)

;; Include tabbar
(require 'tabbar)
 (tabbar-mode)
 (setq tabbar-buffer-groups-function
           (lambda ()
             (list "All Buffers")))

(setq tabbar-buffer-list-function
     	(lambda ()
     	  (remove-if
     	   (lambda(buffer)
     	     (find (aref (buffer-name buffer) 0) " *"))
     	   (buffer-list))))

(defun tabbar-buffer-groups (buffer)
   "Return the list of group names BUFFER belongs to.
 Return only one group for each buffer."
   (with-current-buffer (get-buffer buffer)
     (cond
      ((string-equal "*" (substring (buffer-name) 0 1))
       '("Emacs Buffer"))
      ((eq major-mode 'dired-mode)
       '("Dired"))
      (t
       '("User Buffer"))
      )))

;; C-S-<tab> ;; C-S-<win>-<tab>
(global-set-key (kbd "<C-S-iso-lefttab>") 'tabbar-forward-tab)
(global-set-key (kbd "<C-S-s-iso-lefttab>") 'tabbar-backward-tab)
;; C-x C-<left> ;; C-x C-<right>
(global-set-key (kbd "C-x C-<right>") 'tabbar-forward-group)
(global-set-key (kbd "C-x C-<left>") 'tabbar-backward-group)

(dolist (func '(tabbar-mode tabbar-forward-tab tabbar-forward-group tabbar-backward-tab tabbar-backward-group))
      (autoload func "tabbar" "Tabs at the top of buffers and easy control-tab navigation"))
     
    (defmacro defun-prefix-alt (name on-no-prefix on-prefix &optional do-always)
      `(defun ,name (arg)
         (interactive "P")
         ,do-always
         (if (equal nil arg)
             ,on-no-prefix
           ,on-prefix)))
     
    (defun-prefix-alt shk-tabbar-next (tabbar-forward-tab) (tabbar-forward-group) (tabbar-mode 1))
    (defun-prefix-alt shk-tabbar-prev (tabbar-backward-tab) (tabbar-backward-group) (tabbar-mode 1))
     
    (global-set-key [(control tab)] 'shk-tabbar-next)
    (global-set-key [(control shift tab)] 'shk-tabbar-prev)

 ;; add a buffer modification state indicator in the tab label,
 ;; and place a space around the label to make it looks less crowd
 (defadvice tabbar-buffer-tab-label (after fixup_tab_label_space_and_flag activate)
   (setq ad-return-value
		(if (and (buffer-modified-p (tabbar-tab-value tab))
				 (buffer-file-name (tabbar-tab-value tab)))
			(concat " + " (concat ad-return-value " "))
			(concat " " (concat ad-return-value " ")))))

 ;; called each time the modification state of the buffer changed
 (defun ztl-modification-state-change ()
   (tabbar-set-template tabbar-current-tabset nil)
   (tabbar-display-update))
 ;; first-change-hook is called BEFORE the change is made
 (defun ztl-on-buffer-modification ()
   (set-buffer-modified-p t)
   (ztl-modification-state-change))
 (add-hook 'after-save-hook 'ztl-modification-state-change)
 ;; this doesn't work for revert, I don't know
 ;;(add-hook 'after-revert-hook 'ztl-modification-state-change)
 (add-hook 'first-change-hook 'ztl-on-buffer-modification)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generalidades

(setq mac-option-key-is-meta t)
(setq mac-right-option-modifier nil)

;; Los archivos .bb son templates de SQL
(setq auto-mode-alist (cons '("bb$" . sql-mode) auto-mode-alist))
;; La tecla 'Supr' borra la parte seleccionada
(pending-delete-mode 1)
;; Remarca la zona seleccionada
(transient-mark-mode +1)
;; Abre archivos comprimidos con gzip y bzip2
(auto-compression-mode 1)
;; Algunas combinaciones de teclas
(global-set-key "\C-g" 'goto-line)


;; Entra en modo de coloreado por sintaxis
(global-font-lock-mode t)
;; make it easy on eyes first ...
(set-foreground-color "gray")
(set-background-color "black")

(setq font-lock-maximum-decoration t)
(setq font-lock-maximum-size 262144)

;; Muestra pares de parentesis/corchetes/llaves       
(require 'paren)
(show-paren-mode)
(setq show-paren-mismatch t)
;; Mostrar los bloques marcados mientras los estamos marcando
(setq transient-mark-mode t)
; Hacer scroll por 10 lineas              
(setq scroll-step 10)
;; No seguir agregando lineas en blanco al final
(setq next-line-add-newlines nil)
;; Agregar automaticamente fin de linea a los archivos
(setq require-final-newline t)
;; El modo default para archivos nuevos es text-mode         
(setq default-major-mode 'tex-mode)
; Recordamos nuestra posicion en el buffer               
(setq save-place t)
;; Acepta 'y' o 'n' cuando pide 'yes' o 'no'
(fset 'yes-or-no-p 'y-or-n-p)
;; Resalta la linea que esta el cursor
;(global-hl-line-mode 1)
;; Indenta por default en 4 tabs/espacios 
(setq standard-indent 4)
;; Deshabilita los tab para indent
(setq-default indent-tabs-mode nil)
;; Autocompletado de filas      
(setq auto-fill-mode 1)
;; Muestra el numero de linea               
(line-number-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Multi web mode
(require 'multi-web-mode)
(setq mweb-default-major-mode 'html-mode)
(setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
		  (js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
		  (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
(setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5" "tt" "tmpl"))
(multi-web-global-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs Development Tools
(load-file "~/.emacs.d/cedet/common/cedet.el")
(global-ede-mode 1)                      ; Enable the Project management system
(semantic-load-enable-code-helpers)      ; Enable prototype help and smart completion 
(global-srecode-minor-mode 1)            ; Enable template insertion menu

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BEST PERL MODE
(add-to-list 'load-path "~/.emacs.d/pde")
(load "pde-load")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Smart tabs (Autocomplete with TAB)
(require 'smart-tab)
(global-smart-tab-mode 1)
(define-key read-expression-map [(tab)] 'hippie-expand)
(defun hippie-unexpand ()
  (interactive)
  (hippie-expand 0))
(define-key read-expression-map [(shift tab)] 'hippie-unexpand)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Snippet (Autocomplete)
(add-to-list 'load-path
             "~/.emacs.d/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Autoanadir parentesis o llaves de cierre
(setq skeleton-pair t)
(global-set-key "[" 'skeleton-pair-insert-maybe)
(global-set-key "(" 'skeleton-pair-insert-maybe)
(global-set-key "{" 'skeleton-pair-insert-maybe)
(global-set-key "'" 'skeleton-pair-insert-maybe)
(global-set-key "\"" 'skeleton-pair-insert-maybe)
(global-set-key "<" 'skeleton-pair-insert-maybe)

;; Match HTML tags
(add-hook 'html-mode-hook
	  (lambda ()
	    (define-key html-mode-map (kbd "<M-left>") 'sgml-skip-tag-backward)
	    (define-key html-mode-map (kbd "<M-right>") 'sgml-skip-tag-forward)
	    )
	  )


