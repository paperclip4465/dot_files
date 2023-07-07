#!/bin/sh

LAYOUT_DIR=$XDG_CONFIG_HOME/mfs-screenlayouts
mkdir -p $LAYOUT_DIR

function usage {
    echo "This layout provides a dmenu script executer"
    echo "Usage: $(basename $0) [-hdl]" 2>&1
    echo '   -h   show this menu'
    echo '   -d   print layout location'
    exit 0
}


while getopts ":hdl" opt; do
    case $opt in
	h)
	    usage
	    ;;
	d)
	    echo $LAYOUT_DIR
	    exit 0;
	    ;;
	\?)
	    echo "Invalid option: -$OPTARG" >&2
	    usage
	    ;;
    esac
done


selection=$(ls ${LAYOUT_DIR} | dmenu -p "Select Layout")

if [ "" != "${selection}" ] ; then
    exec ${LAYOUT_DIR}/${selection}
fi
