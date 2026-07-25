if status is-interactive
    # Commands to run in interactive sessions can go here
end

# Transient prompt: previous commands collapse to just the symbol
set --universal fish_transient_prompt 1

# Keep the working directory first (default), don't shorten paths
set --universal pure_begin_prompt_with_current_directory true
set --universal pure_shorten_prompt_current_directory_length 0

# Show venv name (you want this for uv/venv work)
set --universal pure_enable_virtualenv true

# Only show command duration when something took a while
set --universal pure_threshold_command_duration 5
set --universal pure_show_subsecond_command_duration false

set --universal pure_show_exit_status true        # set true if you want exit codes on failure
set --universal pure_check_for_new_release false    # avoid startup network check

# Ghostel integration
string match -qr '^ghostel(,|$)' -- "$INSIDE_EMACS"; and source "$EMACS_GHOSTEL_PATH/etc/shell/ghostel.fish"

fzf_configure_bindings \
--directory=ctrl-o \
--git_status=ctrl-g \
--git_log=alt-e \
--processes=alt-p \
--history=ctrl-r \
--variables=

fish_add_path ~/.local/bin
fish_add_path ~/.cargo/bin
fish_add_path /usr/local/texlive/2026/bin/x86_64-linux
