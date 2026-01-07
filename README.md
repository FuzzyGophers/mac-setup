# Mac Setup

Personal macOS development environment setup.

## Quick Start

```bash
git clone https://github.com/FuzzyGophers/mac-setup.git ~/repos/mac-setup
cd ~/repos/mac-setup
./setup.sh
```

## Usage

```bash
./setup.sh [options]
```

| Option | Description |
|--------|-------------|
| (none) | Install everything |
| `--all` | Install everything (explicit) |
| `--emacs` | Install Emacs and symlink ~/.emacs.d |
| `--gpg` | Install GPG and symlink ~/.gnupg |
| `--git` | Configure Git global settings |
| `--iterm` | Install iTerm2 and import preferences |
| `--rectangle` | Install Rectangle and import preferences |
| `--system` | Configure macOS, Dock, and Finder settings |
| `--shell` | Symlink ~/.bash_profile |
| `--fonts` | Install Hack Nerd Font |
| `--dev` | Install dev tools (languages, LSP servers, formatters) |
| `--claude` | Install Claude Code CLI |
| `--help` | Show help message |

### Examples

```bash
./setup.sh                    # Everything
./setup.sh --emacs --gpg      # Just Emacs and GPG
./setup.sh --system           # Just macOS settings
./setup.sh --dev --fonts      # Dev tools and fonts
```

## What Gets Installed

### Apps (via Homebrew Cask)
- iTerm2 (with Dracula theme and font pre-configured)
- Rectangle (window manager, with preferences pre-configured)
- Hack Nerd Font
- Google Cloud SDK

### Development Tools
- **Emacs**: emacs-plus@29 with native compilation
- **Languages**: Go, Rust, Node, Python, Lua, CMake
- **LSP Servers**: gopls, pyright, rust-analyzer, clangd, lua-language-server, typescript-language-server
- **Formatters**: prettier, black, ruff, gofumpt
- **Tools**: pandoc, delve (Go debugger), gnupg, pinentry-mac, tree
- **CLI**: Claude Code (AI assistant)

### Configuration Applied
- **macOS settings**: Dark mode, 24-hour time, screenshots to ~/Documents/screenshots
- **Dock**: Auto-hide, 48px icons, no magnification, no recent apps
- **Finder**: Show hidden files, path bar, status bar, extensions, list view
- **Git**: Global config with GPG signing
- **GPG**: Keyring and agent config
- **iTerm2**: Dracula theme, Hack Nerd Font Mono size 16
- **Rectangle**: Window snapping preferences

### Symlinks Created
- `~/.emacs.d` -> repo's `.emacs.d/`
- `~/.bash_profile` -> repo's `.bash_profile`
- `~/.gnupg` -> repo's `.gnupg/`

---

## Manual Steps

After running `./setup.sh`, complete these manual steps:

### Change Default Shell
```bash
chsh -s /bin/bash
```

### Restart Apps
Restart iTerm2 and Rectangle to apply imported preferences.

### System Settings (manual)
- Control Center: Enable "Show Percentage" (battery)
