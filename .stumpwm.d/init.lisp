(in-package :stumpwm)

(setf *mouse-focus-policy* :click)
(setf *window-border-style* :thin)
(setf *input-window-gravity* :top-right)
(setf *message-window-gravity* :top-right)
(setf *message-window-window-gravity* :top-right)
(setf *suppress-frame-indicator* t)
(setf *suppress-window-placement-indicator* t)

(set-win-bg-color "#2E3440")
(set-prefix-key (kbd "s-b"))

(define-key *root-map* (kbd "r") "loadrc") 
(define-key *root-map* (kbd "o") "only") 

(define-key *top-map* (kbd "s-d") "exec")
(define-key *top-map* (kbd "s-n") "fullscreen")
(define-key *top-map* (kbd "s-o") "pull-hidden-next") 
(define-key *top-map* (kbd "s-q") "kill")
(define-key *top-map* (kbd "s-y") "float-this")
(define-key *top-map* (kbd "s-;") "eval")

(define-key *top-map* (kbd "s-h") "move-focus left")
(define-key *top-map* (kbd "s-l") "move-focus right")

(define-key *top-map* (kbd "s-0") "vgroups")
(define-key *top-map* (kbd "s-1") "gselect 1")
(define-key *top-map* (kbd "s-2") "gselect 2")
(define-key *top-map* (kbd "s-3") "gselect 3")
(define-key *top-map* (kbd "s-4") "gselect 4")

(defcommand brave () ()
  "Start Brave or switch to it"
  (run-or-raise "brave" '(:class "Brave")))
(define-key *top-map* (kbd "s-c") "brave")
(define-key *top-map* (kbd "s-C") "run-shell-command brave")

(defcommand emacs () ()
  "Start Emacs or switch to it"
  (run-or-raise "emacs" '(:class "Emacs")))
(define-key *top-map* (kbd "s-e") "emacs") 
(define-key *top-map* (kbd "s-E") "run-shell-command emacs") 

(defcommand alacritty () ()
  "Start Alacritty or switch to it"
  (run-or-raise "alacritty" '(:class "Alacritty")))
(define-key *top-map* (kbd "s-v") "alacritty")
(define-key *top-map* (kbd "s-V") "run-shell-command alacritty")

;;      "DEFAULT"           ;; 1
(gnewbg "ALPHA")            ;; 2
(gnewbg "BETA")             ;; 3
(gnewbg "GAMMA")            ;; 4

(which-key-mode)
