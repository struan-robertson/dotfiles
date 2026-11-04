# .bash_profile

# Get the aliases and functions
if [ -f ~/.bashrc ]; then
    . ~/.bashrc
fi

# User specific environment and startup programs
export PATH=$PATH:$HOME/.local/share/npm/bin

# XDG base dirs and XDG-relocated tool config. 

# XDG base dirs, in case they aren't already set
export XDG_CONFIG_HOME="${XDG_CONFIG_HOME:-$HOME/.config}"
export XDG_DATA_HOME="${XDG_DATA_HOME:-$HOME/.local/share}"
export XDG_STATE_HOME="${XDG_STATE_HOME:-$HOME/.local/state}"
export XDG_CACHE_HOME="${XDG_CACHE_HOME:-$HOME/.cache}"

export CLAUDE_CONFIG_DIR="$XDG_CONFIG_HOME/claude"
export GOPATH="$XDG_DATA_HOME/go"
export GTK2_RC_FILES="$XDG_CONFIG_HOME/gtk-2.0/gtkrc"
export RUSTUP_HOME="$XDG_DATA_HOME/rustup"
export WINEPREFIX="$XDG_DATA_HOME/wine"

export NODE_REPL_HISTORY="$XDG_STATE_HOME/node_repl_history"
export NPM_CONFIG_INIT_MODULE="$XDG_CONFIG_HOME/npm/config/npm-init.js"
export NPM_CONFIG_USERCONFIG="$XDG_CONFIG_HOME/npm/npmrc"
export NPM_CONFIG_CACHE="$XDG_CACHE_HOME/npm"
export NPM_CONFIG_PREFIX="$XDG_DATA_HOME/npm"

export PYTHON_HISTORY="$XDG_STATE_HOME/python_history"

export _JAVA_OPTIONS="-Djava.util.prefs.userRoot=$XDG_CONFIG_HOME/java"

export CARGO_HOME="$XDG_DATA_HOME/cargo"

# wget: point it at an XDG config file; the hsts-file setting lives in there.
export WGETRC="$XDG_CONFIG_HOME/wget/wgetrc"

# PATH additions, guarded so nested shells don't prepend twice
for dir in "$CARGO_HOME/bin" "$NPM_CONFIG_PREFIX/bin"; do
    case ":$PATH:" in
        *":$dir:"*) ;;
        *) PATH="$dir:$PATH" ;;
    esac
done
export PATH
