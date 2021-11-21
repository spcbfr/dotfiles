# spacebuffer's amazing dotfiles

Hello, friend. Hello, friend? That's lame. Maybe I should give you a name, but that's a slippery slope. You're only in my head. We have to remember that. Shit.

This is my dotfiles repository, it was originally on github but since November 2021, I've moved my personal projects over to gitlab.

## Features

- useful scripts for xmobar and dmenu and other stuff, located in `~/.local/bin`
- Settings for:
    - xmonad (window manager)
    - emacs (doom, specifically)
    - mpd/ncmpcpp (my music setup)
    - xmobar (the gorgous panel)
    - alacritty (terminal emulator)
    - dunst (notification client)
- clean and minimalist keyboard-driven setup
## Prerequisites
- Zsh is your default shell
- You have the programs that you want the config files for installed on your system

## Usage
 
Clone the directory as a git bare repo
```sh
git clone --separate-git-dir=~/.dotfiles https://github.com/spcbfr/dotfiles.git ~
```
Reload your terminal for the zsh configuration to take effect. this will set the config alias which is what you are going to use to manage your dotfiles

then configure the dotfiles bare repository to hide untracked files
```sh
config config status.showUntrackedFiles no
```


## Screenshots
![Emacs](.local/share/screenshots/emacs.png)
![Pipes](.local/share/screenshots/pipes.png)
