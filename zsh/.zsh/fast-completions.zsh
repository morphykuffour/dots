#!/usr/bin/env zsh
# Ultra-fast completion system with intelligent caching and async loading

# ======================= Performance Cache System =======================

# Fast completion cache
typeset -gA _completion_cache
typeset -gA _completion_timestamps
export _COMPLETION_CACHE_TTL=300  # 5 minutes cache TTL

# Cache completion results
_cache_completion() {
    local key="$1"
    local result="$2"
    
    # Ensure arrays are initialized
    [[ -z "${_completion_cache+x}" ]] && typeset -gA _completion_cache
    [[ -z "${_completion_timestamps+x}" ]] && typeset -gA _completion_timestamps
    
    _completion_cache[$key]="$result"
    _completion_timestamps[$key]="$(date +%s)"
}

# Get cached completion
_get_cached_completion() {
    local key="$1"
    local current_time="$(date +%s)"
    
    # Ensure arrays exist
    [[ -z "${_completion_cache+x}" ]] && return 1
    [[ -z "${_completion_timestamps+x}" ]] && return 1
    
    local cache_time="${_completion_timestamps[$key]:-0}"
    
    if [[ $((current_time - cache_time)) -lt $_COMPLETION_CACHE_TTL && -n "${_completion_cache[$key]}" ]]; then
        echo "${_completion_cache[$key]}"
        return 0
    fi
    return 1
}

# ======================= Ultra-Fast File Completion =======================

# Lightning-fast file completion with smart filtering
_fast_file_completion() {
    local prefix="$1"
    local cache_key="file:$PWD:$prefix"
    local cached_result
    
    # Check cache first
    if cached_result="$(_get_cached_completion "$cache_key")"; then
        compadd ${(f)cached_result}
        return
    fi
    
    # Fast file finding with limits
    local files=()
    local max_depth=3
    local max_files=50
    
    # Use different strategies based on prefix length
    if [[ ${#prefix} -eq 0 ]]; then
        # No prefix: show immediate children only
        files=(${(f)"$(find . -maxdepth 1 -type f -not -name '.*' | head -$max_files)"})
    elif [[ ${#prefix} -lt 3 ]]; then
        # Short prefix: shallow search
        files=(${(f)"$(find . -maxdepth 2 -name "*$prefix*" -type f | head -$max_files)"})
    else
        # Longer prefix: deeper search but cached
        files=(${(f)"$(find . -maxdepth $max_depth -name "*$prefix*" -type f | head -$max_files)"})
    fi
    
    # Cache results for performance
    _cache_completion "$cache_key" "${(F)files}"
    compadd -a files
}

# ======================= Smart Command Completion =======================

# Intelligent command completion with context
_smart_command_completion() {
    local command="$1"
    local cache_key="cmd:$command:$PWD"
    local cached_result
    
    # Check cache
    if cached_result="$(_get_cached_completion "$cache_key")"; then
        compadd ${(f)cached_result}
        return
    fi
    
    local completions=()
    
    case "$command" in
        git)
            # Git subcommand completion
            if git rev-parse --git-dir &>/dev/null 2>&1; then
                completions=(
                    "add" "commit" "push" "pull" "status" "log" "diff"
                    "checkout" "branch" "merge" "rebase" "stash" "reset"
                    "show" "tag" "remote" "fetch" "clone"
                )
                
                # Add recent branches if we're in a repo
                local branches=(${(f)"$(git branch --format='%(refname:short)' 2>/dev/null | head -10)"})
                completions+=($branches)
            fi
            ;;
            
        docker)
            completions=(
                "run" "exec" "ps" "images" "build" "pull" "push"
                "logs" "stop" "start" "restart" "rm" "rmi" "inspect"
                "network" "volume" "system" "stats"
            )
            
            # Add running container names
            local containers=(${(f)"$(docker ps --format '{{.Names}}' 2>/dev/null | head -10)"})
            completions+=($containers)
            ;;
            
        npm)
            completions=(
                "install" "run" "start" "test" "build" "dev"
                "update" "audit" "fund" "outdated" "list"
            )
            
            # Add npm scripts from package.json
            if [[ -f "package.json" ]]; then
                local scripts=(${(f)"$(jq -r '.scripts | keys[]' package.json 2>/dev/null | head -10)"})
                completions+=($scripts)
            fi
            ;;
            
        yarn)
            completions=(
                "install" "add" "remove" "start" "test" "build"
                "dev" "upgrade" "audit" "why" "workspaces"
            )
            ;;
            
        cargo)
            completions=(
                "build" "run" "test" "check" "fmt" "clippy"
                "update" "install" "uninstall" "search" "publish"
            )
            ;;
            
        go)
            completions=(
                "build" "run" "test" "mod" "get" "install"
                "fmt" "vet" "doc" "version" "env" "clean"
            )
            ;;
            
        python|python3)
            # Python file completion
            local py_files=(*.py(.))
            completions+=($py_files)
            
            # Common python modules
            completions+=(
                "-m" "-c" "-u" "-O" "-i" "-v" "--version" "--help"
            )
            ;;
            
        kubectl)
            completions=(
                "get" "describe" "create" "apply" "delete" "logs"
                "exec" "port-forward" "config" "cluster-info"
            )
            ;;
            
        *)
            # For unknown commands, use PATH completion
            local path_commands=(${(f)"$(compgen -c | grep "^$command" | head -20)"})
            completions+=($path_commands)
            ;;
    esac
    
    # Cache the results
    _cache_completion "$cache_key" "${(F)completions}"
    compadd -a completions
}

# ======================= Async Completion Loading =======================

# Async completion worker
_async_completion_worker() {
    local command="$1"
    local context="$2"
    local result_file="$3"
    
    case "$command" in
        git_branches)
            git branch -a --format='%(refname:short)' 2>/dev/null | grep -v '^HEAD' > "$result_file"
            ;;
        docker_images)
            docker images --format '{{.Repository}}:{{.Tag}}' 2>/dev/null > "$result_file"
            ;;
        npm_packages)
            if [[ -f "package.json" ]]; then
                jq -r '.dependencies // {} | keys[]' package.json 2>/dev/null > "$result_file"
            fi
            ;;
        recent_files)
            find . -type f -mtime -1 -not -path '*/\.*' 2>/dev/null | head -50 > "$result_file"
            ;;
    esac
} 

# Start async completion
_start_async_completion() {
    local command="$1"
    local context="$2"
    local result_file="${TMPDIR:-/tmp}/zsh_completion_$$_$(date +%s)"
    
    # Start background worker
    _async_completion_worker "$command" "$context" "$result_file" &
    local worker_pid=$!
    
    # Store for later retrieval
    typeset -g "_async_completion_file_$worker_pid"="$result_file"
    typeset -g "_async_completion_command_$worker_pid"="$command"
}

# ======================= Intelligent History Integration =======================

# Smart history-based completion
_history_smart_completion() {
    local current_cmd="${BUFFER%% *}"
    local cache_key="hist:$current_cmd"
    local cached_result
    
    # Check cache
    if cached_result="$(_get_cached_completion "$cache_key")"; then
        compadd ${(f)cached_result}
        return
    fi
    
    # Get relevant history commands
    local history_matches=()
    local search_pattern="^[[:space:]]*$current_cmd"
    
    # Fast history search with limits
    while IFS= read -r line; do
        [[ ${#history_matches} -lt 20 ]] || break
        if [[ "$line" =~ $search_pattern ]]; then
            # Extract the full command
            local cmd="${line#*  }"  # Remove history number
            history_matches+=("$cmd")
        fi
    done < <(fc -l -200 2>/dev/null | tac | sort -u)
    
    # Cache results
    _cache_completion "$cache_key" "${(F)history_matches}"
    compadd -a history_matches
}

# ======================= Ultra-Fast Completion System =======================

# Main ultra-fast completion function (no ghosting)
_ultra_fast_complete() {
    # Clear any existing autosuggestions to prevent ghosting
    [[ -n $ZSH_AUTOSUGGEST_BUFFER ]] && unset ZSH_AUTOSUGGEST_BUFFER
    
    local current_word="${BUFFER##* }"
    local command="${BUFFER%% *}"
    local completion_type=""
    
    # Determine what type of completion we need
    if [[ "$current_word" =~ ^[./~] ]]; then
        completion_type="file"
    elif [[ "$BUFFER" =~ " $" ]]; then
        completion_type="command_arg"
    elif [[ ${#BUFFER} -eq ${#command} ]]; then
        completion_type="command"
    else
        completion_type="mixed"
    fi
    
    # Save cursor position to prevent jumping
    local cursor_pos=$CURSOR
    
    # Route to appropriate fast completion
    case "$completion_type" in
        file)
            _fast_file_completion "$current_word"
            ;;
        command)
            _smart_command_completion "$command"
            ;;
        command_arg)
            _smart_command_completion "$command"
            ;;
        mixed)
            _smart_command_completion "$command"
            _fast_file_completion "$current_word"
            ;;
    esac
    
    # Restore cursor position
    CURSOR=$cursor_pos
    
    # Force redraw to prevent visual artifacts
    zle reset-prompt
}

# ======================= Performance Optimizations =======================

# Completion performance settings
zstyle ':completion:*' use-cache yes
zstyle ':completion:*' cache-path "${XDG_CACHE_HOME:-$HOME/.cache}/zsh/ultra_fast_cache"

# Limit completion candidates for speed
zstyle ':completion:*' max-errors 1
zstyle ':completion:*:cd:*' tag-order local-directories directory-stack path-directories
zstyle ':completion:*:cd:*' ignore-parents parent pwd

# Fast matching
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Za-z}' 'r:|[._-]=* r:|=*'
zstyle ':completion:*' menu select=2

# Group and format for speed
zstyle ':completion:*' group-name ''
zstyle ':completion:*:descriptions' format '[%d]'

# ======================= Memory Management =======================

# Clean up completion cache periodically
_cleanup_completion_cache() {
    # Limit cache size
    if [[ ${#_completion_cache} -gt 1000 ]]; then
        # Remove oldest 20% of entries
        local -a keys_to_remove
        local current_time="$(date +%s)"
        
        for key in ${(k)_completion_timestamps}; do
            local cache_time="${_completion_timestamps[$key]}"
            if [[ $((current_time - cache_time)) -gt $((2 * $_COMPLETION_CACHE_TTL)) ]]; then
                keys_to_remove+=("$key")
            fi
        done
        
        for key in $keys_to_remove; do
            unset "_completion_cache[$key]"
            unset "_completion_timestamps[$key]"
        done
    fi
}

# Periodic cleanup (run in background)
_completion_cleanup_timer() {
    while true; do
        sleep 300  # 5 minutes
        _cleanup_completion_cache
    done
}

# Start cleanup timer in background
_completion_cleanup_timer &!

# ======================= FZF Integration =======================

# Enhanced fzf completion for commands and files
_fzf_complete_command() {
    setopt localoptions pipefail no_aliases 2> /dev/null
    local tokens cmd prefix
    
    # Parse command line
    tokens=(${(z)LBUFFER})
    cmd=${tokens[1]}
    
    # Check if fzf is available
    if ! command -v fzf &>/dev/null; then
        # Fallback to regular completion
        zle expand-or-complete
        return
    fi
    
    case "$cmd" in
        cd)
            # Directory completion with fzf
            local dir
            dir="$(find . -type d -not -path '*/\.*' 2>/dev/null | head -100 | fzf --height=10 --preview 'ls -la {}' --preview-window=right:50%)"
            if [[ -n "$dir" ]]; then
                LBUFFER="cd ${(q)dir}"
                CURSOR=${#LBUFFER}
            fi
            ;;
        vim|nvim|code|cat|less)
            # File completion with preview
            local file
            file="$(find . -type f -not -path '*/\.*' 2>/dev/null | head -200 | fzf --height=15 --preview 'head -50 {}' --preview-window=right:50%)"
            if [[ -n "$file" ]]; then
                LBUFFER="$cmd ${(q)file}"
                CURSOR=${#LBUFFER}
            fi
            ;;
        git)
            # Git command completion
            if [[ ${#tokens} -eq 1 ]]; then
                local git_cmd
                git_cmd="$(echo -e 'add\ncommit\npush\npull\nstatus\nlog\ncheckout\nbranch\nmerge\nrebase\ndiff\nshow' | fzf --height=10)"
                if [[ -n "$git_cmd" ]]; then
                    LBUFFER="git $git_cmd "
                    CURSOR=${#LBUFFER}
                fi
            fi
            ;;
        docker)
            # Docker command completion
            if [[ ${#tokens} -eq 1 ]]; then
                local docker_cmd
                docker_cmd="$(echo -e 'run\nexec\nps\nimages\nbuild\npull\npush\nlogs\nstop\nrm\nrmi' | fzf --height=10)"
                if [[ -n "$docker_cmd" ]]; then
                    LBUFFER="docker $docker_cmd "
                    CURSOR=${#LBUFFER}
                fi
            fi
            ;;
        "")
            # History completion with fzf
            local hist_cmd
            hist_cmd="$(fc -l -1000 2>/dev/null | cut -c 8- | sort -u | fzf --height=15 --tac)"
            if [[ -n "$hist_cmd" ]]; then
                LBUFFER="$hist_cmd"
                CURSOR=${#LBUFFER}
            fi
            ;;
        *)
            # Default: history search filtered by command
            local hist_cmd
            hist_cmd="$(fc -l -1000 2>/dev/null | cut -c 8- | sort -u | grep "^$cmd" | fzf --height=15 --tac --query="$cmd")"
            if [[ -n "$hist_cmd" ]]; then
                LBUFFER="$hist_cmd"
                CURSOR=${#LBUFFER}
            fi
            ;;
    esac
    
    zle reset-prompt
}

zle -N _fzf_complete_command

# ======================= Integration =======================

# Replace standard completion with ultra-fast version
zle -N _ultra_fast_complete

# Bind to Tab key (uncomment to activate)
# bindkey '^I' _ultra_fast_complete

# Alternative binding for testing
bindkey '^X^F' _ultra_fast_complete  # Ctrl+X, Ctrl+F for ultra-fast completion

# Cache functions are available globally (no export needed in zsh)

# Performance monitoring
typeset -g _completion_stats_file="${XDG_CACHE_HOME:-$HOME/.cache}/zsh/completion_stats.log"

_log_completion_performance() {
    local start_time="$1"
    local end_time="$(date +%s%N)"
    local duration=$((($end_time - $start_time) / 1000000))  # Convert to ms
    local command="${BUFFER%% *}"
    
    echo "$(date '+%Y-%m-%d %H:%M:%S') $command ${duration}ms" >> "$_completion_stats_file"
    
    # Rotate log if it gets too big
    if [[ -f "$_completion_stats_file" ]] && [[ $(wc -l < "$_completion_stats_file") -gt 1000 ]]; then
        tail -500 "$_completion_stats_file" > "${_completion_stats_file}.tmp"
        mv "${_completion_stats_file}.tmp" "$_completion_stats_file"
    fi
}

# Wrapper for performance monitoring
_monitored_completion() {
    local start_time="$(date +%s%N)"
    _ultra_fast_complete
    _log_completion_performance "$start_time"
}

zle -N _monitored_completion
bindkey '^X^M' _monitored_completion  # Ctrl+X, Ctrl+M for monitored completion