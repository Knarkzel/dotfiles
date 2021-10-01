(in-package :stumpwm)

(setf *mouse-focus-policy* :click)
(setf *window-border-style* :thin)
(setf *input-window-gravity* :top-right)
(setf *message-window-gravity* :top-right)
(setf *message-window-window-gravity* :top-right)
(setf *suppress-frame-indicator* t)
(setf *suppress-window-placement-indicator* t)
(setf *grab-pointer-character* 0)
(setf *grab-pointer-character-mask* 0)

(set-prefix-key (kbd "s-b"))
(set-win-bg-color "#2E3440")
(which-key-mode)

;;      "DEFAULT"           ;; 1
(gnewbg "ALPHA")            ;; 2
(gnewbg "BETA")             ;; 3
(gnewbg "GAMMA")            ;; 4

(define-key *root-map* (kbd "r") "loadrc") 
(define-key *root-map* (kbd "o") "only") 

(define-key *top-map* (kbd "XF86AudioMute") "run-shell-command amixer set 'Master' 0%")
(define-key *top-map* (kbd "XF86AudioLowerVolume") "run-shell-command amixer set 'Master' 5%-")
(define-key *top-map* (kbd "XF86AudioRaiseVolume") "run-shell-command amixer set 'Master' 5%+")
(define-key *top-map* (kbd "Print") "run-shell-command scrot -s '/tmp/%F_%T_$wx$h.png' -e 'xclip -selection clipboard -target image/png -i $f'")

(define-key *top-map* (kbd "s-q") "kill")
(define-key *top-map* (kbd "s-f") "pull-hidden-next") 

(define-key *top-map* (kbd "s-d") "run-shell-command rofi -show run")
(define-key *top-map* (kbd "s-e") "run-shell-command rofi -show calc -modi calc -no-show-match -no-sort")
(define-key *top-map* (kbd "s-;") "eval")
(define-key *top-map* (kbd "s-o") "run-shell-command ~/.scripts/pdf.sh")

(define-key *top-map* (kbd "s-y") "float-this")
(define-key *top-map* (kbd "s-Y") "unfloat-this")
(define-key *top-map* (kbd "s-n") "fullscreen")

(define-key *top-map* (kbd "s-h") "move-focus left")
(define-key *top-map* (kbd "s-l") "move-focus right")
(define-key *top-map* (kbd "s-Left") "move-window left")
(define-key *top-map* (kbd "s-Right") "move-window right")

(define-key *top-map* (kbd "s-0") "vgroups")
(define-key *top-map* (kbd "s-1") "gselect 1")
(define-key *top-map* (kbd "s-2") "gselect 2")
(define-key *top-map* (kbd "s-3") "gselect 3")
(define-key *top-map* (kbd "s-4") "gselect 4")
(define-key *top-map* (kbd "s-!") "gmove 1")
(define-key *top-map* (kbd "s-@") "gmove 2")
(define-key *top-map* (kbd "s-#") "gmove 3")
(define-key *top-map* (kbd "s-$") "gmove 4")

(defcommand firefox () ()
  "Start firefox or switch to it"
  (run-or-raise "firefox" '(:class "Firefox")))
(define-key *top-map* (kbd "s-c") "firefox")
(define-key *top-map* (kbd "s-C") "run-shell-command firefox")

(defcommand alacritty () ()
  "Start Alacritty or switch to it"
  (run-or-raise "alacritty" '(:class "Alacritty")))
(define-key *top-map* (kbd "s-v") "alacritty")
(define-key *top-map* (kbd "s-V") "run-shell-command alacritty")
