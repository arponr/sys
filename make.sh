#!/bin/bash


# Output functions

red=$'\e[1;31m'
pur=$'\e[0;35m'
yel=$'\e[0;33m'
grn=$'\e[0;32m'
end=$'\e[0m'

msg() {
  printf "${yel} ~ ${pur}$*${end}\n"
}

suc() {
  printf "${grn}$*${end}\n"
}

warn() {
  printf "${red}$*${end}\n"
}

don() {
  printf "...done\n\n"
}


# Variables

dir=$HOME/Documents/code/sys
now=`date +%Y-%m-%d-%H:%M:%S`
olddir=$HOME/.dotfiles_old/$now
dotfiles=("emacs.d" "gitconfig" "latexmkrc" "profile")
nodotfiles=("Brewfile")


# Introduction

printf "I'm going to port system settings from $dir, ok? [Y/N] "
read a
if [[ $a == "N" || $a == "n" ]]; then
  printf "Fine then. Peace.\n\n"
  exit;
else
  printf "Great, I'll get started.\n\n"
fi


# Housekeeping

# create dotfiles_old in homedir
msg "Creating $olddir for backup of any existing dotfiles in $HOME..."
mkdir -p $olddir
don

# change to the dotfiles directory
cd $dir

# move any existing dotfiles in homedir to dotfiles_old directory, then create symlinks
msg "Moving existing dotfiles from $HOME to $olddir..."
for file in ${dotfiles[@]}; do
    mv $HOME/.$file $olddir
    ln -s $dir/$file $HOME/.$file
done

for file in ${nodotfiles[@]}; do
    mv $HOME/$file $olddir
    ln -s $dir/$file $HOME/$file
    chflags -h hidden $HOME/$file
done
don




# Package manager stuff

# Install pip
msg "Installing pip..."
printf "Admin password may be required.\n"
if [[ `uname` == "Darwin" ]]; then
  sudo easy_install pip
elif [[ `uname` == "Linux" ]]; then
  sudo apt-get install pip
fi
don

# Check presence of Homebrew, add bundler, then install our desired recipes & update them
if [[ `uname` == "Darwin" ]]; then
  msg "Installing Homebrew packages..."
  type brew &>/dev/null && printf "`brew upgrade --all && brew bundle &&
    brew update`" || echo "`ruby -e \"$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)\" && brew tap Homebrew/bundle && brew bundle && brew update`"
  don
fi


# Finish up

source $HOME/.profile

suc "All done!"
