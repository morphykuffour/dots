#!/usr/bin/env bash

# macOS Finder Configuration
# Sets Finder to always use list view with newest files at the top

echo "Configuring Finder preferences..."

# Set default view style to list view (Nlsv)
# Options: icnv (icon), clmv (column), glyv (gallery), Nlsv (list)
defaults write com.apple.finder FXPreferredViewStyle -string "Nlsv"

# Set default sort to "Date Modified" in descending order (newest first)
defaults write com.apple.finder FXPreferredGroupBy -string "Date Modified"

# Show all filename extensions
defaults write NSGlobalDomain AppleShowAllExtensions -bool true

# Show path bar at bottom of Finder windows
defaults write com.apple.finder ShowPathbar -bool true

# Show status bar at bottom of Finder windows
defaults write com.apple.finder ShowStatusBar -bool true

# Keep folders on top when sorting by name
defaults write com.apple.finder _FXSortFoldersFirst -bool true

# Use list view in all Finder windows by default
defaults write com.apple.finder FXPreferredViewStyle -string "Nlsv"

# Set default search scope to current folder
defaults write com.apple.finder FXDefaultSearchScope -string "SCcf"

# Disable the warning when changing a file extension
defaults write com.apple.finder FXEnableExtensionChangeWarning -bool false

echo "Removing .DS_Store files to reset view preferences..."
# Remove all .DS_Store files to force Finder to use new defaults
# This ensures the new settings apply to existing folders
find ~ -name ".DS_Store" -depth -exec rm {} \; 2>/dev/null

echo "Restarting Finder to apply changes..."
killall Finder

echo "✓ Finder configuration complete!"
echo ""
echo "Your Finder windows will now:"
echo "  - Always open in list view"
echo "  - Sort by Date Modified (newest first)"
echo "  - Show file extensions"
echo "  - Show path bar and status bar"
echo "  - Keep folders at the top"
