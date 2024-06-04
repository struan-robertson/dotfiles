if status is-interactive
    # Commands to run in interactive sessions can go here
    set -x BAT_THEME Nord

    zoxide init fish | source
end
