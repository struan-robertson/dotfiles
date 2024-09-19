# Set PATH
set PATH ~/.local/share/juliaup/bin/ ~/.local/bin ~/.cargo/bin/ $PATH

if status is-interactive
    
    set -x BAT_THEME Nord

    zoxide init fish | source

    # Improve Emacs keybindings
    bind \ep up-or-search
    bind \en down-or-search
    bind \er history-pager

end

# Allow Emacs TRAMP Connections
if test "$TERM" = "dumb"
  function fish_prompt
    echo "\$ "
  end

  function fish_right_prompt; end
  function fish_greeting; end
  function fish_title; end
end
