;;; Directory Local Variables            -*- no-byte-compile: t -*-
;;; For more information see (info "(emacs) Directory Variables")

((nil . ((eval . (display-fill-column-indicator-mode 1))
         (fill-column . 72)))
 (auto-mode-alist . (("README\\'" . gfm-mode)
                     ("README.md\\'" . gfm-mode)))
 (emacs-lisp-mode . ((elisp-flymake-byte-compile-load-path . load-path)))
 (lisp-data-mode . ((indent-tabs-mode . nil)
                    (sentence-end-double-space . t))))
