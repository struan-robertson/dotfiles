# XDG base dirs, in case they aren't already set
set -q XDG_CONFIG_HOME; or set -gx XDG_CONFIG_HOME $HOME/.config
set -q XDG_DATA_HOME;   or set -gx XDG_DATA_HOME   $HOME/.local/share
set -q XDG_STATE_HOME;  or set -gx XDG_STATE_HOME  $HOME/.local/state
set -q XDG_CACHE_HOME;  or set -gx XDG_CACHE_HOME  $HOME/.cache

set -gx CLAUDE_CONFIG_DIR $XDG_CONFIG_HOME/claude
set -gx GNUPGHOME $XDG_DATA_HOME/gnupg
set -gx GOPATH $XDG_DATA_HOME/go
set -gx GTK2_RC_FILES $XDG_CONFIG_HOME/gtk-2.0/gtkrc
set -gx KODI_DATA $XDG_DATA_HOME/kodi
set -gx RUSTUP_HOME $XDG_DATA_HOME/rustup
set -gx WINEPREFIX $XDG_DATA_HOME/wine

set -gx NODE_REPL_HISTORY $XDG_STATE_HOME/node_repl_history
set -gx NPM_CONFIG_INIT_MODULE $XDG_CONFIG_HOME/npm/config/npm-init.js
set -gx NPM_CONFIG_CACHE $XDG_CACHE_HOME/npm
set -gx NPM_CONFIG_TMP $XDG_RUNTIME_DIR/npm
set -gx PYTHON_HISTORY $XDG_STATE_HOME/python_history

set -gx _JAVA_OPTIONS -Djava.util.prefs.userRoot=$XDG_CONFIG_HOME/java

set -gx CARGO_HOME $XDG_DATA_HOME/cargo
fish_add_path -g $CARGO_HOME/bin

function wget
    command wget --hsts-file=$XDG_DATA_HOME/wget-hsts $argv
end