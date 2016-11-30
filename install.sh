#/bin/bash

mkdir ~/.backup-configuration

mv ~/.bash_aliases ~/.backup-configuration 
mv ~/.tools ~/.backup-configuration 
mv ~/.byobu ~/.backup-configuration 
mv ~/.conkerorrc ~/.backup-configuration 
mv ~/.emacs ~/.backup-configuration 
mv ~/.emacs.d ~/.backup-configuration 
mv ~/.my-zsh ~/.backup-configuration 
mv ~/.spacemacs ~/.backup-configuration 
mv ~/.zshrc ~/.backup-configuration 
mv ~/.zsh-syntax-highlighting ~/.backup-configuration 

ln -s `pwd`/.bash_aliases ~
ln -s `pwd`/.tools ~
ln -s `pwd`/.byobu ~
ln -s `pwd`/.conkerorrc ~
ln -s `pwd`/.emacs ~
ln -s `pwd`/.emacs.d ~
ln -s `pwd`/.my-zsh ~
ln -s `pwd`/.spacemacs ~
ln -s `pwd`/.zshrc ~
ln -s `pwd`/.zsh-syntax-highlighting ~
