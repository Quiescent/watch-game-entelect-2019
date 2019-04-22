#!/bin/bash

emacs --no-init -nw --eval "(progn (load-file \"watch-game.el\") (watch \"$1\"))"
