#!/bin/sh

SCRIPT_DIR=$XDG_CONFIG_HOME/mfs-script-picker
mkdir -p $SCRIPT_DIR

function usage {
    echo "This script provides a dmenu script executer"
    echo "Usage: $(basename $0) [-hdl]" 2>&1
    echo '   -h   show this menu'
    echo '   -d   print script location'
    exit 0
}


while getopts ":hdl" opt; do
    case $opt in
	h)
	    usage
	    ;;
	d)
	    echo $SCRIPT_DIR
	    ;;
	\?)
	    echo "Invalid option: -$OPTARG" >&2
	    ;;
    esac
done


selection=$(ls ${SCRIPT_DIR} | dmenu -p "Select Layout")

if [ "" != "${selection}" ] ; then
    exec ${SCRIPT_DIR}/${selection}
fi
