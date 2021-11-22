(use-modules (gnu))
(use-service-modules
  cups
  desktop
  networking
  ssh
  xorg)

(operating-system
  (locale "en_US.utf8")
  (timezone "Europe/Oslo")
  (keyboard-layout
    (keyboard-layout "us" "altgr-intl"))
  (host-name "odd")
  (users (cons* (user-account
                  (name "odd")
                  (comment "Odd")
                  (group "users")
                  (home-directory "/home/odd")
                  (supplementary-groups
                    '("wheel" "netdev" "audio" "video")))
                %base-user-accounts))
  (packages
    (append
      (list (specification->package "nss-certs"))
      %base-packages))
  (services
    (append
      (list (service gnome-desktop-service-type)
            (set-xorg-configuration
              (xorg-configuration
                (keyboard-layout keyboard-layout))))
      %desktop-services))
  (bootloader
    (bootloader-configuration
      (bootloader grub-bootloader)
      (targets (list "/dev/sda"))
      (keyboard-layout keyboard-layout)))
  (swap-devices
    (list (uuid "72d632cb-7ffb-4ce8-aa71-9d162fa7091f")))
  (file-systems
    (cons* (file-system
             (mount-point "/")
             (device
               (uuid "b65f400a-b037-4b12-94d7-9769bba276e7"
                     'ext4))
             (type "ext4"))
           %base-file-systems)))
