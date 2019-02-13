#!/bin/bash

if [ "x$1" == "xemacs" ]; then
    emacs
else
    exec $@
fi

