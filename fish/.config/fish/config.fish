# TRAMP needs a Bourne-compatible shell with a plain prompt; fish's pure
# prompt never matches TRAMP's prompt regexp, so its parser loops forever
# (pinning a core) before it can switch to sh. TRAMP connects with a tty
# but sets TERM=dumb, so hand that session off to sh. Real terminals keep a
# proper TERM, so interactive fish is unaffected.
#
# The is-interactive guard is essential: fish sources this file even for
# `fish -c CMD`, and `exec sh` drops the -c arguments, so a bare TERM test
# silently turned every Emacs `shell-command-to-string' (TERM=dumb) into an
# empty string with exit status 0. Only TRAMP's login session is interactive.
if status is-interactive; and test "$TERM" = dumb
    exec sh
end

# Hide welcome message
set -g fish_greeting

# Transient prompt: previous commands collapse to just the symbol
set -g fish_transient_prompt 1

# Keep the working directory first (default), don't shorten paths
set -g pure_begin_prompt_with_current_directory true
set -g pure_shorten_prompt_current_directory_length 0

# Show venv name (you want this for uv/venv work)
set -g pure_enable_virtualenv true

# Only show command duration when something took a while
set -g pure_threshold_command_duration 5
set -g pure_show_subsecond_command_duration false

set -g pure_show_exit_status true        # set true if you want exit codes on failure
set -g pure_check_for_new_release false    # avoid startup network check
set -g pure_enable_container_detection false # dont show when in a container, as this adds noise for working in the toolbox

# Ghostel integration
string match -qr '^ghostel(,|$)' -- "$INSIDE_EMACS"; and source "$EMACS_GHOSTEL_PATH/etc/shell/ghostel.fish"

zoxide init fish | source

