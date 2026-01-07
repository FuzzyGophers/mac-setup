#!/bin/bash
#
# Mac Setup - Main Setup Script
# Installs dependencies and creates symlinks for dotfiles
#
# Usage: ./setup.sh [options]
#   --all        Install everything (default if no options)
#   --emacs      Install Emacs and symlink ~/.emacs.d
#   --gpg        Install GPG and symlink ~/.gnupg
#   --git        Configure Git global settings
#   --iterm      Install iTerm2 and import preferences
#   --rectangle  Install Rectangle and import preferences
#   --system     Configure macOS, Dock, and Finder settings
#   --shell      Symlink ~/.bash_profile
#   --fonts      Install Hack Nerd Font
#   --dev        Install dev tools (languages, LSP servers, formatters)
#   --claude     Install Claude Code CLI
#   --help       Show this help message
#

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

print_header() { echo -e "\n${BLUE}==>${NC} ${1}"; }
print_success() { echo -e "${GREEN}[OK]${NC} ${1}"; }
print_warning() { echo -e "${YELLOW}[WARN]${NC} ${1}"; }
print_error() { echo -e "${RED}[ERROR]${NC} ${1}"; }

# Backup and symlink function
create_symlink() {
    local source="$1"
    local target="$2"
    local name="$(basename "$target")"

    if [ -L "$target" ] && [ "$(readlink "$target")" = "$source" ]; then
        print_success "$name already correctly symlinked"
        return 0
    fi

    if [ -e "$target" ] || [ -L "$target" ]; then
        local backup="${target}.backup.$(date +%Y%m%d_%H%M%S)"
        mv "$target" "$backup"
        print_warning "Backed up existing $name to $backup"
    fi

    ln -s "$source" "$target"
    print_success "Symlinked $name -> $source"
}

# Brew install helper
brew_install() {
    local pkg="$1"
    local pkg_name=$(echo "$pkg" | awk '{print $1}')
    if brew list "$pkg_name" &> /dev/null; then
        print_success "$pkg_name already installed"
    else
        echo "Installing $pkg_name..."
        brew install $pkg || print_warning "Failed to install $pkg_name"
    fi
}

brew_cask_install() {
    local pkg="$1"
    if brew list --cask "$pkg" &> /dev/null; then
        print_success "$pkg already installed"
    else
        echo "Installing $pkg..."
        brew install --cask "$pkg" || print_warning "Failed to install $pkg"
    fi
}

ensure_homebrew() {
    if ! command -v brew &> /dev/null; then
        print_header "Installing Homebrew..."
        /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
        if [[ -f /opt/homebrew/bin/brew ]]; then
            eval "$(/opt/homebrew/bin/brew shellenv)"
        fi
    fi
}

# Component functions
setup_emacs() {
    print_header "Setting up Emacs..."
    ensure_homebrew
    brew tap d12frosted/emacs-plus 2>/dev/null || true
    brew_install "emacs-plus@29 --with-native-comp --with-modern-icon"

    # Link to Applications
    if [[ -d "/opt/homebrew/opt/emacs-plus@29" ]] || [[ -d "/usr/local/opt/emacs-plus@29" ]]; then
        osascript -e 'tell application "Finder" to make alias file to POSIX file "/opt/homebrew/opt/emacs-plus@29/Emacs.app" at POSIX file "/Applications"' 2>/dev/null || \
        osascript -e 'tell application "Finder" to make alias file to POSIX file "/usr/local/opt/emacs-plus@29/Emacs.app" at POSIX file "/Applications"' 2>/dev/null || true
    fi

    create_symlink "$SCRIPT_DIR/.emacs.d" "$HOME/.emacs.d"
}

setup_gpg() {
    print_header "Setting up GPG..."
    ensure_homebrew
    brew_install "gnupg"
    brew_install "pinentry-mac"
    create_symlink "$SCRIPT_DIR/.gnupg" "$HOME/.gnupg"

    # Restart GPG agent
    gpgconf --kill gpg-agent 2>/dev/null || true
    gpgconf --launch gpg-agent 2>/dev/null && \
        print_success "GPG agent restarted" || print_warning "Could not restart GPG agent"
}

setup_git() {
    print_header "Configuring Git..."
    git config --global user.name "Aaron Bauman"
    git config --global user.email "bauman.aaron@gmail.com"
    git config --global user.signingkey 0x3A50067CE39A65CFD1BBDF7D063BF83219D98FFC
    git config --global commit.gpgsign true
    git config --global push.gpgsign false
    git config --global pull.rebase true
    git config --global push.default simple
    git config --global core.editor emacs
    git config --global init.defaultBranch master
    print_success "Git configured"
}

setup_iterm() {
    print_header "Setting up iTerm2..."
    ensure_homebrew
    brew_cask_install "iterm2"

    if [ -f "$SCRIPT_DIR/prefs/com.googlecode.iterm2.plist" ]; then
        cp "$SCRIPT_DIR/prefs/com.googlecode.iterm2.plist" ~/Library/Preferences/
        print_success "iTerm2 preferences imported"
    else
        print_warning "iTerm2 preferences not found"
    fi
}

setup_rectangle() {
    print_header "Setting up Rectangle..."
    ensure_homebrew
    brew_cask_install "rectangle"

    if [ -f "$SCRIPT_DIR/prefs/com.knollsoft.Rectangle.plist" ]; then
        cp "$SCRIPT_DIR/prefs/com.knollsoft.Rectangle.plist" ~/Library/Preferences/
        print_success "Rectangle preferences imported"
    else
        print_warning "Rectangle preferences not found"
    fi
}

setup_system() {
    print_header "Configuring macOS settings..."

    # Dark mode
    defaults write -g AppleInterfaceStyle -string "Dark" 2>/dev/null && \
        print_success "Dark mode enabled" || print_warning "Could not set dark mode"

    # 24-hour time
    defaults write -g AppleICUForce24HourTime -bool true 2>/dev/null && \
        print_success "24-hour time enabled" || print_warning "Could not set 24-hour time"

    # Screenshots location
    if [ ! -d "$HOME/Documents/screenshots" ]; then
        mkdir -p "$HOME/Documents/screenshots"
    fi
    defaults write com.apple.screencapture location "$HOME/Documents/screenshots" && \
        print_success "Screenshots location set" || print_warning "Could not set screenshots location"

    # Dock settings
    print_header "Configuring Dock..."
    defaults write com.apple.dock autohide -bool true
    defaults write com.apple.dock tilesize -int 64
    defaults write com.apple.dock magnification -bool false
    defaults write com.apple.dock show-recents -bool false
    killall Dock 2>/dev/null || true
    print_success "Dock configured"

    # Finder settings
    print_header "Configuring Finder..."
    defaults write com.apple.finder AppleShowAllFiles -bool true
    defaults write com.apple.finder ShowPathbar -bool true
    defaults write com.apple.finder ShowStatusBar -bool true
    defaults write NSGlobalDomain AppleShowAllExtensions -bool true
    defaults write com.apple.finder FXPreferredViewStyle -string "Nlsv"
    defaults write com.apple.finder FXDefaultSearchScope -string "SCcf"
    killall Finder 2>/dev/null || true
    print_success "Finder configured"
}

setup_shell() {
    print_header "Setting up shell..."
    create_symlink "$SCRIPT_DIR/.bash_profile" "$HOME/.bash_profile"
}

setup_fonts() {
    print_header "Installing fonts..."
    ensure_homebrew
    brew_cask_install "font-hack-nerd-font"
}

setup_dev() {
    print_header "Installing development tools..."
    ensure_homebrew

    # Languages
    for pkg in go rust node "python@3" lua cmake; do
        brew_install "$pkg"
    done

    # LSP Servers
    for pkg in gopls pyright rust-analyzer llvm lua-language-server; do
        brew_install "$pkg"
    done

    # Formatters
    for pkg in prettier black ruff gofumpt; do
        brew_install "$pkg"
    done

    # Linters
    brew_install "luacheck"

    # Other tools
    for pkg in pandoc delve tree; do
        brew_install "$pkg"
    done

    # Development casks
    brew_cask_install "google-cloud-sdk"

    # NPM packages
    if command -v npm &> /dev/null; then
        print_header "Installing npm packages..."
        for pkg in typescript typescript-language-server prettier eslint; do
            if npm list -g "$pkg" &> /dev/null; then
                print_success "$pkg already installed"
            else
                npm install -g "$pkg" || print_warning "Failed to install $pkg"
            fi
        done
    fi

    # Python tools
    if command -v pip3 &> /dev/null; then
        pip3 install --user cmake-language-server 2>/dev/null || true
    fi
}

setup_claude() {
    print_header "Setting up Claude Code..."
    ensure_homebrew

    # Ensure Node.js is available (required for npm)
    brew_install "node"

    if command -v npm &> /dev/null; then
        if npm list -g @anthropic-ai/claude-code &> /dev/null; then
            print_success "Claude Code already installed"
        else
            echo "Installing Claude Code..."
            npm install -g @anthropic-ai/claude-code || print_warning "Failed to install Claude Code"
        fi
    else
        print_error "npm not available - cannot install Claude Code"
    fi
}

show_help() {
    echo "Mac Setup - Install and configure macOS development environment"
    echo ""
    echo "Usage: ./setup.sh [options]"
    echo ""
    echo "Options:"
    echo "  --all        Install everything (default if no options)"
    echo "  --emacs      Install Emacs and symlink ~/.emacs.d"
    echo "  --gpg        Install GPG and symlink ~/.gnupg"
    echo "  --git        Configure Git global settings"
    echo "  --iterm      Install iTerm2 and import preferences"
    echo "  --rectangle  Install Rectangle and import preferences"
    echo "  --system     Configure macOS, Dock, and Finder settings"
    echo "  --shell      Symlink ~/.bash_profile"
    echo "  --fonts      Install Hack Nerd Font"
    echo "  --dev        Install dev tools (languages, LSP servers, formatters)"
    echo "  --claude     Install Claude Code CLI"
    echo "  --help       Show this help message"
    echo ""
    echo "Examples:"
    echo "  ./setup.sh                    # Install everything"
    echo "  ./setup.sh --emacs --gpg      # Just Emacs and GPG"
    echo "  ./setup.sh --system           # Just macOS settings"
}

# macOS check
if [[ "$(uname)" != "Darwin" ]]; then
    print_error "This script is designed for macOS only."
    exit 1
fi

# Parse arguments
DO_ALL=false
DO_EMACS=false
DO_GPG=false
DO_GIT=false
DO_ITERM=false
DO_RECTANGLE=false
DO_SYSTEM=false
DO_SHELL=false
DO_FONTS=false
DO_DEV=false
DO_CLAUDE=false

if [[ $# -eq 0 ]]; then
    DO_ALL=true
fi

while [[ $# -gt 0 ]]; do
    case $1 in
        --all)       DO_ALL=true ;;
        --emacs)     DO_EMACS=true ;;
        --gpg)       DO_GPG=true ;;
        --git)       DO_GIT=true ;;
        --iterm)     DO_ITERM=true ;;
        --rectangle) DO_RECTANGLE=true ;;
        --system)    DO_SYSTEM=true ;;
        --shell)     DO_SHELL=true ;;
        --fonts)     DO_FONTS=true ;;
        --dev)       DO_DEV=true ;;
        --claude)    DO_CLAUDE=true ;;
        --help)      show_help; exit 0 ;;
        *)           print_error "Unknown option: $1"; show_help; exit 1 ;;
    esac
    shift
done

# Run selected components
echo ""
echo "=========================================="
echo "       Mac Setup - Starting"
echo "=========================================="
echo "Repository: $SCRIPT_DIR"

if $DO_ALL || $DO_DEV; then setup_dev; fi
if $DO_ALL || $DO_FONTS; then setup_fonts; fi
if $DO_ALL || $DO_EMACS; then setup_emacs; fi
if $DO_ALL || $DO_GPG; then setup_gpg; fi
if $DO_ALL || $DO_GIT; then setup_git; fi
if $DO_ALL || $DO_ITERM; then setup_iterm; fi
if $DO_ALL || $DO_RECTANGLE; then setup_rectangle; fi
if $DO_ALL || $DO_SYSTEM; then setup_system; fi
if $DO_ALL || $DO_SHELL; then setup_shell; fi
if $DO_ALL || $DO_CLAUDE; then setup_claude; fi

# Summary
print_header "Setup complete!"
echo ""
echo "Next steps:"
echo "  1. Restart your terminal (or run: source ~/.bash_profile)"
echo "  2. Log out and back in for macOS settings to take effect"
echo "  3. Run 'emacs' - packages will auto-install from MELPA/ELPA"
echo ""
echo "Manual steps remaining:"
echo "  - Change default shell: chsh -s /bin/bash"
echo "  - Restart iTerm2 and Rectangle to apply preferences"
echo ""
