;;; +evil.el -*- lexical-binding: t; -*-

(use-package! evil
  :config
  (setq evil-want-fine-undo t)
  (map! (:when IS-MAC
          :g "s-t"   #'+workspace/new
          :g "s-T"   #'+workspace/display
          :g "s-1"   #'+workspace/switch-to-0
          :g "s-2"   #'+workspace/switch-to-1
          :g "s-3"   #'+workspace/switch-to-2
          :g "s-4"   #'+workspace/switch-to-3
          :g "s-5"   #'+workspace/switch-to-4
          :g "s-6"   #'+workspace/switch-to-5
          :g "s-7"   #'+workspace/switch-to-6
          :g "s-8"   #'+workspace/switch-to-7
          :g "s-9"   #'+workspace/switch-to-8
          :g "s-0"   #'+workspace/switch-to-final
          :g "s-}"   #'+workspace/switch-right
          :g "s-{"   #'+workspace/switch-left)
        :g "C-c s" #'window-toggle-side-windows
        :g "C-c c" #'calendar
        :g "C-c C-d" #'dirvish))

(use-package! evil-owl
  :after evil
  :config
  (setq evil-owl-max-string-length 500)
  (add-to-list 'display-buffer-alist
               '("*evil-owl*"
                 (display-buffer-in-side-window)
                 (side . bottom)
                 (window-height . 0.5)))
  (set-popup-rule! "^\\*evil-owl\\*" :size 0.5)
  (evil-owl-mode))

(use-package evil-goggles
  :defer t
  :after evil
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))
