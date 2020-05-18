#! /bin/bash

sudo ln -s "$(realpath 11-usb-keyboard.conf)" /etc/X11/xorg.conf.d/11-usb-keyboard.conf
sudo ln -s "$(realpath 70-synaptics.conf)" /etc/X11/xorg.conf.d/70-synaptics.conf
mkdir -p ~/.config/dconf && ln -s "$(realpath dconf)" ~/.config/dconf/user
ln -s "$(realpath dircolors)" ~/.dircolors
mkdir -p ~/.emacs.d && ln -s "$(realpath .emacs.d/custom.el)" ~/.emacs.d/custom.el
mkdir -p ~/.emacs.d && ln -s "$(realpath .emacs.d/init.el)" ~/.emacs.d/init.el
ln -s "$(realpath gitconfig)" ~/.gitconfig
ln -s "$(realpath gitignore)" ~/.gitignore
ln -s "$(realpath i3)" ~/.config/i3/config
#ln -s "$(realpath i3status)" ~/.config/i3status/config
ln -s "$(realpath i3status-rust)" ~/.config/i3/status.toml
ln -s "$(realpath profile)" ~/.profile
mkdir -p ~/.lein && ln -s "$(realpath profiles.clj)" ~/.lein/profiles.clj
ln -s "$(realpath xprofile)" ~/.xprofile
ln -s "$(realpath Xmodmap)" ~/.Xmodmap

ln -s "$(realpath aliases)" ~/.aliases

ln -s "$(realpath zshrc)" ~/.zshrc
ln -s "$(realpath zshenv)" ~/.zshenv
