# Set PATH
set PATH ~/.local/share/juliaup/bin/ ~/.local/bin ~/.cargo/bin/ $PATH

if status is-interactive
    # Commands to run in interactive sessions can go here
    set -x BAT_THEME Nord

    zoxide init fish | source

end

