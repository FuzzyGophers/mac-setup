# My Mac Setup (14.4.1)

## System Settings

### Appearance
- Set appearance to Dark Mode

### Control Center
- Enable "Show Percentage"

### General

#### Date & Time
- Enable "24-hour time"
- Enable "Show 24-hour time on Lock Screen"

## Screenshots
Run the following to put all screenshots in '~/Documents/screenshots'

```
mkdir ~/Documents/screenshots
defaults write com.apple.screencapture location ~/Documents/screenshots
killall SystemUIServer
```

### Homebrew
[Homebrew](https://brew.sh/)

```
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
```

## Hack Font
```
brew tap homebrew/cask-fonts
brew search '/font-.*-nerd-font/' | awk '{ print $1 }' | xargs -I{} brew install --cask {} || true
```

## iTerm2
https://iterm2.com/

### iTerm2 Dracula
https://draculatheme.com/iterm

### iTerm2 Font
Preferences>Profiles>Text>16
Preferences>Profiles>Text>Font (Hack Nerd Font Mono)

### iTerm2 Change Default Shell
```
chsh -s /bin/bash
```

### Rectangle
[Rectangle](https://rectangleapp.com/)

## Git
```
brew install git
```

### Git Global Config
```
git config --global user.name "Aaron Bauman"
git config --global user.email "bauman.aaron@gmail.com"
git config --global user.signingkey 0x395FD70D6DAD263A7FC02B191611597BBBE926A9
git config --global commit.gpgsign 1
git config --global push.gpgsign 0
git config --global pull.rebase true
git config --global --unset pull.ff
git config --global push.default simple
git config --global core.editor emacs
git config --global init.defaultBranch master
```

## Yubikey
```
brew install gnupg pinentry-mac
```

## Emacs
```
brew install emacs
```

### Emacs Configs
```
cd ~
git clone git@github.com:FuzzyGophers/.emacs.d.git
```
