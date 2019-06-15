#!/bin/bash

if [ "x$1" == "xemacs" ]; then
    [ -d /tmp/org/images ] || ln -s /small/SMALL/images /tmp/org/images
    emacs
else
    exec $@
fi

