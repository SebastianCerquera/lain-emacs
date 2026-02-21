#!/bin/bash

# Update scrum-agenda in .emacs and org-scrum-view in lain.el
if [ -n "$SCRUM_AGENDA_FILES" ]; then
    # Convert space-separated list to Elisp list format: "file1" "file2"
    ELISP_LIST=$(echo "$SCRUM_AGENDA_FILES" | sed 's/[[:space:]]\+/" "/g' | sed 's/^/"/' | sed 's/$/"/')
    
    # Update /root/.emacs
    if [ -f /root/.emacs ]; then
        sed "s|.*SCRUM_AGENDA_FILES.*|(defvar scrum-agenda-files '($ELISP_LIST)) ;; SCRUM_AGENDA_FILES|g" /root/.emacs > /tmp/emacs.tmp && cp /tmp/emacs.tmp /root/.emacs
    fi
    
    # Update /root/.emacs.d/lain/lain.el
    if [ -f /root/.emacs.d/lain/lain.el ]; then
        sed "s|.*SCRUM_AGENDA_FILES.*|(defvar lain-scrum-agenda-files '($ELISP_LIST)) ;; SCRUM_AGENDA_FILES|g" /root/.emacs.d/lain/lain.el > /tmp/lain.el.tmp && cp /tmp/lain.el.tmp /root/.emacs.d/lain/lain.el
    fi
fi

if [ "x$1" = "xemacs" ]; then
    [ -d /tmp/org/images ] || ln -s /small/SMALL/images /tmp/org/images
    shift
    exec emacs "$@"
else
    exec "$@"
fi

