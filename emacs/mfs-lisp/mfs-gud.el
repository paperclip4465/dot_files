;;; mfs-gud.el --- Additional GUD configuration      -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Mitchell Schmeisser

;; Keywords: tools,

;;; Commentary:

;;; Code:


(defgroup jlink-gdb-server ()
  "Jlink gdb server options."
  :group 'gud)

(defcustom jlink-gdb-server-executable "JLinkGDBServer"
  "JLink GDB Server executable to use."
  :type 'string
  :group 'jlink-gdb-server)

(defcustom jlink-gdb-server-default-device "MK21dn512axxx5"
  "Default device to use when starting JLink GDB Server."
  :type 'string
  :group 'jlink-gdb-server)

(defcustom jlink-gdb-server-default-speed 4000
  "Default device to use when starting JLink GDB Server."
  :type 'number
  :group 'jlink-gdb-server)

(defcustom jlink-gdb-server-default-port 2331
  "Default device to use when starting JLink GDB Server."
  :type 'number
  :group 'jlink-gdb-server)

(defcustom jlink-gdb-server-default-swo-port 2332
  "Default device to use when starting JLink GDB Server."
  :type 'number
  :group 'jlink-gdb-server)

(defcustom jlink-gdb-server-default-telnet-port 2333
  "Default device to use when starting JLink GDB Server."
  :type 'number
  :group 'jlink-gdb-server)


(defun jlink-gdb-server-cmd ()
  "Return a shell command to launch jlink gdb."
  (format "%s -select USB -device %s -endian little -if SWD -speed %s -noir -nologtofile -noLocalhostonly -port %s -SWOPort %s -TelnetPort %s"
	  jlink-gdb-server-executable
	  jlink-gdb-server-default-device
	  jlink-gdb-server-default-speed
	  jlink-gdb-server-default-port
	  jlink-gdb-server-default-swo-port
	  jlink-gdb-server-default-telnet-port))

;;;###autoload
(defun jlink-gdb-server-launch ()
  "Launch jlink gdb server."
  (interactive)
  (start-process "jlink-gdb-server" "*jlink-gdb-server*"
		 jlink-gdb-server-executable
		 "-select" "USB"
		 "-device" jlink-gdb-server-default-device
		 "-endian" "little"
		 "-if SWD"
		 "-speed" (number-to-string jlink-gdb-server-default-speed)
		 "-port" (number-to-string jlink-gdb-server-default-port)
		 "-SWOPort" (number-to-string jlink-gdb-server-default-swo-port)
		 "-TelnetPort" (number-to-string jlink-gdb-server-default-telnet-port)))

(provide 'mfs-gud)
;;; mfs-gud.el ends here
