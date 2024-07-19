#!/bin/sh

process_name="nyxt"

if pgrep -x "$process_name" > /dev/null; then
    if [ $# -eq 0 ]; then
	nyxt -r -e '(make-window (make-buffer :url "nyxt:new"))' -q
    else
	nyxt -r -e "(make-window (make-buffer :url \"$1\"))" -q
    fi
else
    nyxt "$1"
fi
