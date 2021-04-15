(setenv "DISPLAY" ":0.0")



(leaf exwm
  :config
  (setq
   exwm-input--update-focus-interval 0.001
   exwm-workspace-number 10
   exwm-manage-configurations '((t char-mode t))))

(add-hook 'exwm-update-class-hook
          (lambda ()
            (unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                        (string= "gimp" exwm-instance-name))
              (exwm-workspace-rename-buffer exwm-class-name))))
(add-hook 'exwm-update-title-hook
          (lambda ()
            (when (or (not exwm-instance-name)
                      (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                      (string= "gimp" exwm-instance-name))
              (exwm-workspace-rename-buffer exwm-title))))

(defun odd/spawn-process (process)
  (interactive)
  (let ((upper-cased (s-upper-camel-case process)))
    (start-process upper-cased upper-cased process)))

(defun odd/swap-workspace ()
  (interactive)
  (let ((current-index exwm-workspace--current)
        (other-index (exwm-workspace--prompt-for-workspace)))
    (exwm-workspace-swap current-index other-index)))

(setq exwm-input-global-keys
      `(
        ([?\s-,] . split-window-below)
        ([?\s-.] . split-window-right)
        ,@(mapcar (lambda (i)
                    `(,(kbd (format "s-%d" i)) .
                      (lambda ()
                        (interactive)
                        (exwm-workspace-switch-create ,i))))
                  (number-sequence 0 9))
        ([?\s-H] . evil-window-move-far-left)
        ([?\s-J] . evil-window-move-very-bottom)
        ([?\s-K] . evil-window-move-very-top)
        ([?\s-L] . evil-window-move-far-right)
        ([?\s-b] . ivy-switch-buffer)
        ([?\s-d] . dmenu)
        ([?\s-e] . eshell)
        ([?\s-y] . exwm-floating-toggle-floating)
        ([?\s-g] . dired-jump)
        ([?\s-h] . evil-window-left)
        ([?\s-j] . evil-window-down)
        ([?\s-k] . evil-window-up)
        ([?\s-l] . evil-window-right)
        ([?\s-s] . battery)
        ([?\s-o] . delete-other-windows)
        ([?\s-m] . odd/swap-workspace)
        ([?\s-n] . exwm-layout-toggle-fullscreen)
        ([?\s-q] . kill-this-buffer)
        ([?\s-v] . (lambda () (interactive) (odd/spawn-process "alacritty")))
        ([?\s-c] . (lambda () (interactive) (odd/spawn-process "brave")))
        ([?\s-x] . odd/m-x)
        ([XF86AudioLowerVolume] . pulseaudio-control-decrease-volume)
        ([XF86AudioMute] . pulseaudio-control-toggle-current-sink-mute)
        ([XF86AudioRaiseVolume] . pulseaudio-control-increase-volume)))

(require 'exwm-randr)
(exwm-randr-enable)
(setq exwm-randr-workspace-monitor-plist '(1 "HDMI-1" 2 "HDMI-1" 3 "HDMI-1" 4 "HDMI-1" 5 "HDMI-1"
                                             0 "HDMI-0" 6 "HDMI-0" 7 "HDMI-0" 8 "HDMI-0" 9 "HDMI-0" ))
(add-hook 'exwm-randr-screen-change-hook
          (lambda ()
            (start-process-shell-command
             "xrandr" nil "xrandr --output HDMI-1 --left-of HDMI-0")))
(exwm-enable)
