#!/bin/sh
#
# Proxy the `i3-msg` command to the focused window.

# Proxy to Emacs if it is the active window
if [[ "$(xdotool getactivewindow getwindowclassname)" == "Emacs" ]]; then
    command="(my/wm-integration \"$@\")"
    if emacsclient -e "$command"; then
	exit
    fi
fi

# fallback to i3
i3-msg $@
