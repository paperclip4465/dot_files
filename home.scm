(use-modules (gnu home)
	     (gnu home services)
	     (gnu home services shells)
	     (gnu home services shepherd)
	     (gnu services)
	     (gnu packages)
	     (gnu packages admin)
	     (gnu packages shells)
	     (gnu packages web-browsers)
	     (gnu packages suckless)
	     (gnu packages xdisorg)
	     (gnu packages xorg)
	     (gnu packages vim)
	     (gnu packages password-utils)
	     (gnu packages vim)
	     (gnu packages emacs)
	     (gnu packages terminals)
	     (gnu packages image-viewers)
	     (guix gexp)
	     (guix modules)
	     (srfi srfi-9)
	     (guix packages)
	     (guix build utils)
	     (guix git-download))

(home-environment
 (packages (list htop
		 password-store
		 slstatus
		 xrdb
		 emacs
		 fzf
		 feh))

 (services
  (list
   (simple-service 'pii-env-vars
		   home-environment-variables-service-type
		   `(("EMAIL" . "mitchellschmeisser@librem.one")
		     ("NAME" . "Mitchell Schmeisser")
		     ("EDITOR" . "vim")))
   (service home-zsh-service-type
	    (home-zsh-configuration
	     (zshrc `(,(local-file "./shell/zshrc")))
	     (environment-variables
	      '(("GUIX_PROFILE" . "~/.guix-profile")))))

   (simple-service 'xorg-init
		   home-run-on-first-login-service-type
		   (with-imported-modules (source-module-closure
					   '((guix build utils)))
		     #~(begin
			 (use-modules (guix build utils))
			 (invoke "xrdb" #+(local-file "./xorg/Xresources")))))
   (service home-shepherd-service-type))))
