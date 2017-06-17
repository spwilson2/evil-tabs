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

(evil-define-command evil-tabs-tabedit (&optional file)
  (interactive "<f>")
  "Open the given file in a new tab.
If a file isn't provided just open a scratch buffer instead."
  (let
      ;; Save the original setting so we don't confuse users
      ((evil-tabs--temp-eyebrowse-new-workspace eyebrowse-new-workspace))
    (progn
      (if file (setq eyebrowse-new-workspace file) (setq eyebrowse-new-workspace t))
      (eyebrowse-create-window-config))
    ;; Restore the original setting.
    (setq eyebrowse-new-workspace evil-tabs--temp-eyebrowse-new-workspace)))

(evil-define-command evil-tabs-tabclone ()
  (interactive)
  "Clone the current tab's window tab configuration into a new tab."
  (let
      ;; Save the original setting so we don't confuse users
      ((evil-tabs--temp-eyebrowse-new-workspace eyebrowse-new-workspace))
    (progn
      (setq eyebrowse-new-workspace nil)
      (eyebrowse-create-window-config))
    ;; Restore the original setting.
    (setq eyebrowse-new-workspace evil-tabs--temp-eyebrowse-new-workspace)))

;; Quit only if this is the last tab open, otherwise close this tab.
(evil-define-command evil-tab-sensitive-quit (&optional bang)
  (let ((window-configs (eyebrowse--get 'window-configs)))
  (if (and (one-window-p) (> (length window-configs) 1))
    (eyebrowse-close-window-config)
    (evil-quit bang))))

(evil-define-command evil-tab-close ()
  (eyebrowse-close-window-config))

(defun evil-tabs-goto-tab (&optional count)
  (interactive "P")
  "Go back or forward count tabs. If count is nil, go forward a single tab."
  (if count
      (progn
	(message (int-to-string count))
      (let ((motion
	     (if (> count 0) ;; Incase evil ever gets negative prefix working.
		 'eyebrowse-next-window-config
	         'eyebrowse-prev-window-config))
	    (count (abs count)))
	(while (>= (decf count) 0)
	  (eval 'motion))))

    (eyebrowse-next-window-config nil)))

;;(evil-define-motion 'evil-tabs-goto-tab)
(evil-define-motion evil--tabs-goto-tab-motion (&optional count)
  (evil-tabs-goto-tab count))

;;(evil-define-motion evil-tabs-goto-tab (&optional count)
;;  (interactive "P")
;;  (progn
;;    (message (when count (number-to-string count)))
;;  (if count
;;     (eyebrowse-next-window-config count)
;;     (eyebrowse-next-window-config 1))))

(evil-ex-define-cmd "tabe[dit]" 'evil-tabs-tabedit)
(evil-ex-define-cmd "tabclone" 'evil-tabs-tabedit)
(evil-ex-define-cmd "tabc[lose]" 'evil-tab-close)
(evil-ex-define-cmd "q[uit]" 'evil-tab-sensitive-quit)
(evil-ex-define-cmd "tabnew" 'evil-tabs-tabed)
(evil-ex-define-cmd "tabn[ext]" 'eyebrowse-next-window-config)
(evil-ex-define-cmd "tabp[rev]" 'eyebrowse-prev-window-config)

; TODO allow numbers
(evil-define-key 'normal evil-tabs-mode-map
  "gt" 'evil--tabs-goto-tab-motion
  ;"gT" 'evil--tabs-goto-tab-motion
  "gT" 'evil-tabs-goto-tab
  "gc" 'eyebrowse-close-window-config
  "zx" 'eyebrowse-last-window-config
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
