#!/bin/bash

# CleanShot Screenshot Path Copier
# Monitors /Users/morph/Sync/screenshots and copies new file paths to clipboard

WATCH_DIR="/Users/morph/Sync/screenshots"
LOG_FILE="$HOME/Library/Logs/cleanshot-path-copier.log"

# Function to log messages
log_message() {
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] $1" >> "$LOG_FILE"
}

# Check if fswatch is installed
if ! command -v fswatch &> /dev/null; then
    log_message "ERROR: fswatch is not installed. Install it with: brew install fswatch"
    exit 1
fi

# Check if watch directory exists
if [ ! -d "$WATCH_DIR" ]; then
    log_message "ERROR: Watch directory does not exist: $WATCH_DIR"
    exit 1
fi

log_message "Starting CleanShot path copier - monitoring $WATCH_DIR"

# Monitor the directory for new files
# -e Created flag only triggers on new file creation
# -r for recursive monitoring
# -l 0.5 for 0.5 second latency to batch events
fswatch -0 -e Created -l 0.5 "$WATCH_DIR" | while read -d "" file_path
do
    # Only process files, not directories
    if [ -f "$file_path" ]; then
        # Copy full path to clipboard
        echo -n "$file_path" | pbcopy
        log_message "Copied to clipboard: $file_path"
    fi
done
