# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
    . /etc/bashrc
fi

# User specific environment
if ! [[ "$PATH" =~ $HOME/.local/bin:$HOME/bin: ]]; then
    PATH="$HOME/.local/bin:$HOME/bin:$PATH"
fi
export PATH

# Uncomment the following line if you don't like systemctl's auto-paging feature:
# export SYSTEMD_PAGER=


# Variables

export EDITOR=mg
export SUDO_EDITOR="$HOME/.local/bin/mg"

export SSH_AUTH_SOCK="/run/user/1000/ssh-agent.socket"


export QT_QPA_PLATFORMTHEME qt5ct
