#!/usr/bin/env zsh
# Robust completion system without ghosting or conflicts
# Optimized for the most commonly used commands

# ======================= PERFORMANCE CACHE =======================

# Global completion cache with smart TTL - initialize properly
if [[ -z "${_robust_completion_cache+x}" ]]; then
    typeset -gA _robust_completion_cache
fi
if [[ -z "${_robust_completion_timestamps+x}" ]]; then
    typeset -gA _robust_completion_timestamps  
fi
export _ROBUST_CACHE_TTL=300  # 5 minutes

# Ensure cache directory exists
local robust_cache_dir="${XDG_CACHE_HOME:-$HOME/.cache}/zsh/robust_completions"
[[ -d "$robust_cache_dir" ]] || mkdir -p "$robust_cache_dir"

# Fast cache functions
_cache_result() {
    local key="$1" result="$2"
    _robust_completion_cache[$key]="$result"
    _robust_completion_timestamps[$key]="$(date +%s)"
}

_get_cached() {
    local key="$1" current_time="$(date +%s)"
    local cache_time="${_robust_completion_timestamps[$key]:-0}"
    
    if [[ $((current_time - cache_time)) -lt $_ROBUST_CACHE_TTL && -n "${_robust_completion_cache[$key]}" ]]; then
        echo "${_robust_completion_cache[$key]}"
        return 0
    fi
    return 1
}

# ======================= SMART COMMAND COMPLETIONS =======================

# Git completions (most used)
_robust_git_completion() {
    local subcommand="$1"
    local cache_key="git:$PWD:$subcommand"
    local cached_result
    
    if cached_result="$(_get_cached "$cache_key")"; then
        compadd ${(f)cached_result}
        return
    fi
    
    local completions=()
    
    if [[ -z "$subcommand" ]]; then
        # Git subcommands
        completions=(
            "add" "commit" "push" "pull" "status" "log" "diff"
            "checkout" "switch" "branch" "merge" "rebase" "stash"
            "reset" "show" "tag" "remote" "fetch" "clone" "init"
        )
    else
        case "$subcommand" in
            checkout|switch)
                # Recent branches (limit for speed)
                completions=(${(f)"$(git branch --format='%(refname:short)' 2>/dev/null | head -15)"})
                # Add remote branches
                local remote_branches=(${(f)"$(git branch -r --format='%(refname:short)' 2>/dev/null | sed 's|origin/||' | head -10)"})
                completions+=($remote_branches)
                ;;
            add)
                # Modified/untracked files
                completions=(${(f)"$(git ls-files --modified --others --exclude-standard 2>/dev/null | head -20)"})
                ;;
            reset|restore)
                # Staged files
                completions=(${(f)"$(git diff --name-only --cached 2>/dev/null | head -15)"})
                ;;
            merge|rebase)
                # Branches
                completions=(${(f)"$(git branch --format='%(refname:short)' 2>/dev/null | grep -v '^\\*' | head -10)"})
                ;;
        esac
    fi
    
    _cache_result "$cache_key" "${(F)completions}"
    compadd -a completions
}

# Docker completions
_robust_docker_completion() {
    local subcommand="$1"
    local cache_key="docker:$subcommand"
    local cached_result
    
    if cached_result="$(_get_cached "$cache_key")"; then
        compadd ${(f)cached_result}
        return
    fi
    
    local completions=()
    
    if [[ -z "$subcommand" ]]; then
        completions=(
            "run" "exec" "ps" "images" "build" "pull" "push"
            "logs" "stop" "start" "restart" "rm" "rmi" "inspect"
            "network" "volume" "system" "stats" "compose"
        )
    else
        case "$subcommand" in
            exec|logs|stop|start|restart|rm|inspect)
                # Running containers
                completions=(${(f)"$(docker ps --format '{{.Names}}' 2>/dev/null | head -15)"})
                ;;
            run|pull)
                # Popular images
                completions=(
                    "nginx" "alpine" "ubuntu" "node" "python" "postgres"
                    "redis" "mysql" "mongo" "elasticsearch" "debian"
                )
                ;;
            rmi)
                # Local images
                completions=(${(f)"$(docker images --format '{{.Repository}}:{{.Tag}}' 2>/dev/null | head -15)"})
                ;;
        esac
    fi
    
    _cache_result "$cache_key" "${(F)completions}"
    compadd -a completions
}

# NPM/Node completions
_robust_npm_completion() {
    local subcommand="$1"
    local cache_key="npm:$PWD:$subcommand"
    local cached_result
    
    if cached_result="$(_get_cached "$cache_key")"; then
        compadd ${(f)cached_result}
        return
    fi
    
    local completions=()
    
    if [[ -z "$subcommand" ]]; then
        completions=(
            "install" "run" "start" "test" "build" "dev"
            "update" "audit" "fund" "outdated" "list" "init"
            "publish" "version" "cache" "config"
        )
        
        # Add package.json scripts if available
        if [[ -f "package.json" ]]; then
            local scripts=(${(f)"$(jq -r '.scripts // {} | keys[]' package.json 2>/dev/null | head -10)"})
            completions+=($scripts)
        fi
    else
        case "$subcommand" in
            run)
                # Package.json scripts
                if [[ -f "package.json" ]]; then
                    completions=(${(f)"$(jq -r '.scripts // {} | keys[]' package.json 2>/dev/null)"})
                fi
                ;;
            install|add|remove)
                # Popular packages (curated list)
                completions=(
                    "react" "vue" "angular" "express" "lodash" "axios"
                    "typescript" "webpack" "babel" "eslint" "prettier"
                    "jest" "mocha" "chai" "nodemon" "concurrently"
                )
                ;;
        esac
    fi
    
    _cache_result "$cache_key" "${(F)completions}"
    compadd -a completions
}

# Python completions
_robust_python_completion() {
    local cache_key="python:$PWD"
    local cached_result
    
    if cached_result="$(_get_cached "$cache_key")"; then
        compadd ${(f)cached_result}
        return
    fi
    
    local completions=()
    
    # Python files in current directory
    local py_files=(*.py(.N))
    completions+=($py_files)
    
    # Common Python flags and modules
    completions+=(
        "-m" "-c" "-u" "-O" "-i" "-v" "--version" "--help"
        "-m pip" "-m venv" "-m pytest" "-m black" "-m flake8"
    )
    
    _cache_result "$cache_key" "${(F)completions}"
    compadd -a completions
}

# File completion optimized for speed
_robust_file_completion() {
    local prefix="$1"
    local cache_key="files:$PWD:${prefix:0:3}"  # Cache by first 3 chars
    local cached_result
    
    if cached_result="$(_get_cached "$cache_key")"; then
        compadd ${(f)cached_result}
        return
    fi
    
    local files=()
    local max_files=30
    
    if [[ ${#prefix} -eq 0 ]]; then
        # No prefix: immediate children only
        files=(${(f)"$(find . -maxdepth 1 -not -name '.*' | head -$max_files)"})
    elif [[ ${#prefix} -lt 3 ]]; then
        # Short prefix: shallow search
        files=(${(f)"$(find . -maxdepth 2 -name "*$prefix*" -not -path '*/.*' | head -$max_files)"})
    else
        # Longer prefix: deeper but limited search
        files=(${(f)"$(find . -maxdepth 3 -name "*$prefix*" -not -path '*/.*' | head -$max_files)"})
    fi
    
    _cache_result "$cache_key" "${(F)files}"
    compadd -a files
}

# ======================= MAIN COMPLETION ROUTER =======================

# Primary robust completion function
_robust_complete() {
    # Prevent autosuggestion conflicts
    [[ -n $ZSH_AUTOSUGGEST_BUFFER ]] && unset ZSH_AUTOSUGGEST_BUFFER
    
    local words=(${(z)LBUFFER})
    local cmd="${words[1]}"
    local subcmd="${words[2]}"
    local current_word="${LBUFFER##* }"
    
    # Route to specific completion handlers
    case "$cmd" in
        git)
            _robust_git_completion "$subcmd"
            ;;
        docker)
            _robust_docker_completion "$subcmd"
            ;;
        npm|yarn)
            _robust_npm_completion "$subcmd"
            ;;
        python|python3|py)
            _robust_python_completion
            ;;
        cd|ls|cat|vim|nvim|code|less|head|tail)
            _robust_file_completion "$current_word"
            ;;
        *)
            # Fallback to built-in completion for unknown commands
            zle expand-or-complete
            return
            ;;
    esac
    
    # Force clean redraw
    zle reset-prompt 2>/dev/null
}

# ======================= ZLE INTEGRATION =======================

# Register the robust completion function
zle -N _robust_complete

# Bind to Ctrl+Space for fast access (non-conflicting)
bindkey '^ ' _robust_complete

# ======================= CLEANUP =======================

# Periodic cache cleanup to prevent memory bloat
_cleanup_robust_cache() {
    local current_time="$(date +%s)"
    local keys_to_remove=()
    
    for key in ${(k)_robust_completion_timestamps}; do
        local cache_time="${_robust_completion_timestamps[$key]}"
        if [[ $((current_time - cache_time)) -gt $((2 * $_ROBUST_CACHE_TTL)) ]]; then
            keys_to_remove+=("$key")
        fi
    done
    
    for key in $keys_to_remove; do
        unset "_robust_completion_cache[$key]"
        unset "_robust_completion_timestamps[$key]"
    done
}

# Run cleanup every 10 minutes in background
(
    while true; do
        sleep 600
        _cleanup_robust_cache
    done
) &!