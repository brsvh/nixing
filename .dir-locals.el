;;; Directory Local Variables            -*- no-byte-compile: t -*-
;;; For more information see (info "(emacs) Directory Variables")

((auto-mode-alist . (("\\.md\\'" . gfm-mode)))
 (nix-mode . ((eval . (display-fill-column-indicator-mode 1))
              (eval . (display-line-numbers-mode 1))))
 (prog-mode . ((eval . (display-fill-column-indicator-mode 1))
	       (fill-column . 72)
               (indent-tabs-mode . nil)
               (sentence-end-double-space . t)))
 (org-mode . ((eval . (org-toggle-link-display))))
 (text-mode . ((fill-column . 72)
               (indent-tabs-mode . nil)
               (sentence-end-double-space . t))))
