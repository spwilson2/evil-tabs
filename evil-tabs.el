;;; evil-tabs.el --- Integrating Vim-style tabs for Evil mode users.
;; Copyright 2013 Kris Jenkins
;; Copyright 2017 Sean Wilson
;; 
;; Author: Sean Wilson <spwilson2@wisc.edu>
;; Maintainer: Sean Wilson <spwilson2@wisc.edu>
;; Keywords: evil tab tabs vim eyebrowse
;; URL: https://github.com/spwilson2/evil-tabs
;; Created: 30th September 2013
;; Version: 0.2.0-dev
;; Package-Requires: ((evil "0.0.0") (eyebrowse "0.0.0"))

;;; Commentary:
;;
;; Integrating Vim-style tabs for Evil mode users.
;;; Code:

(require 'evil)
(require 'eyebrowse)

(defvar evil-tabs-mode-map (make-sparse-keymap)
  "Evil-tabs-mode's keymap.")

;; TODO: need to add support for tabe specifying a file
;; use eyebrowse-new-workspace:
;(evil-define-command evil-tabs-tabedit (file)
;  (interactive "<f>")
;  (elscreen-find-file file))

(evil-define-command evil-tabs-tabedit ()
  (eyebrowse-create-window-config))

;; Quit only if this is the last tab open, otherwise close this tab.
;; Todo: Also need sure that this doesn't break window functionality
(evil-define-command evil-tab-sensitive-quit (&optional bang)
  :repeat nil
  (interactive "<!>")
  (let ((window-configs (eyebrowse--get 'window-configs)))
  (if (> (length window-configs) 1)
    (eyebrowse-close-window-config)
    (evil-quit bang))))

(evil-define-command evil-tab-close ()
  (eyebrowse-close-window-config))

(evil-define-motion evil-tabs-goto-tab (&optional count)
  (if count
     (eyebrowse-next-window-config (- count 1))
     (eyebrowse-next-window-config 1)))


(evil-ex-define-cmd "tabe[dit]" 'evil-tabs-tabedit)
(evil-ex-define-cmd "tabclone" 'evil-tabs-tabedit)
(evil-ex-define-cmd "tabc[lose]" 'evil-tab-close)
(evil-ex-define-cmd "q[uit]" 'evil-tab-sensitive-quit)

; TODO allow numbers
(evil-define-key 'normal evil-tabs-mode-map
  "gt" 'eyebrowse-next-window-config
  "gT" 'eyebrowse-prev-window-config
;  "gt" 'evil-tabs-goto-tab
;  "T" 'evil-tabs-current-buffer-to-tab
  )

;TODO: Close all tabs but 'current-slot
;(evil-ex-define-cmd "tabo[nly]" 'TODO)

; TODO Moves the current buffer to its own tab
;(evil-define-command evil-tabs-current-buffer-to-tab ()
;  (let ((nwindows (length (window-list)))
;        (cb (current-buffer)))
;    (when (> nwindows 1)
;      (delete-window)
;      (elscreen-create)
;      (switch-to-buffer cb))))
;
;(evil-ex-define-cmd "tabd[isplay]" 'elscreen-toggle-display-tab)
;(evil-ex-define-cmd "tabg[oto]" 'elscreen-goto)
;(evil-ex-define-cmd "tabnew" 'elscreen-create)
;(evil-ex-define-cmd "tabn[ext]" 'elscreen-next)
;(evil-ex-define-cmd "tabp[rev]" 'elscreen-previous)
;(evil-ex-define-cmd "tabr[ename]" 'elscreen-screen-nickname)
;(evil-ex-define-cmd "tabs[elect]" 'elscreen-select-and-goto)
;(evil-ex-define-cmd "tabw[ith]" 'elscreen-find-and-goto-by-buffer)

;;;###autoload
(define-minor-mode evil-tabs-mode
  "Integrating Vim-style tabs for Evil mode users."
  :global t
  :keymap evil-tabs-mode-map
  (let ((prev-state evil-state))
    (evil-normal-state)
    (evil-change-state prev-state)

    ; Wrap around like vim does for tabs.
    (make-local-variable 'eyebrowse-wrap-around)
    (setq eyebrowse-wrap-around t)
    ))

;;;###autoload
(defun turn-on-evil-tabs-mode ()
  "Enable `evil-tabs-mode' in the current buffer."
    (evil-tabs-mode 1))

;;;###autoload
(defun turn-off-evil-tabs-mode ()
  "Disable `evil-tabs-mode' in the current buffer."
  (evil-tabs-mode -1))

;;;###autoload
(define-globalized-minor-mode global-evil-tabs-mode evil-tabs-mode turn-on-evil-tabs-mode)

(provide 'evil-tabs)

;;; evil-tabs.el ends here
