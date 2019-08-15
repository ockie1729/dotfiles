#!/bin/bash

if [ ! -e "$HOME/.emacs.d" ]; then
  echo "create $HOME/.emacs.d"
  mkdir "$HOME/.emacs.d"
fi


if [ ! -e "$HOME/.emacs.d/init.el" ]; then
  echo "make a symbolic link to dotfiles/init.el"
  ln -s "$(pwd)/init.el" "$HOME/.emacs.d/init.el"
fi

if [ ! -e "$HOME/.emacs.d/insert" ]; then
  echo "make a symbolic link to dotfiles/insert"
  ln -s "$(pwd)/insert" "$HOME/.emacs.d/insert"
fi
