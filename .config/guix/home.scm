;; This "home-environment" file can be passed to 'guix home reconfigure'
;; to reproduce the content of your profile.  This is "symbolic": it only
;; specifies package names.  To reproduce the exact same profile, you also
;; need to capture the channels being used, as returned by "guix describe".
;; See the "Replicating Guix" section in the manual.

(use-modules
  (gnu home)
  (gnu packages)
  (gnu services)
  (guix gexp)
  (gnu home services shells))

(home-environment
  (packages
    (map specification->package
         (list "mupdf"
               "xdg-utils"
               "vlang"
               "fzf"
               "git"
               "make"
               "gcc-toolchain"
               "rust"
               "emacs-native-comp"
               "ungoogled-chromium"
               "exa"
               "zoxide"
               "xset"
               "nnn"
               "gnome-tweaks"
               "xcape"
               "vim"
	       "tcc"
	       "ccls"
               "alacritty"))))
