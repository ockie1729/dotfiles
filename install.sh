#!/bin/bash

if [ ! -e "$HOME/.emacs.d" ]; then
  echo "create $HOME/.emacs.d"
  mkdir "$HOME/.emacs.d"
fi

echo "make a symbolic link to dotfiles/init.el"
ln -s "$(pwd)/init.el" "$HOME/.emacs.d/init.el"

