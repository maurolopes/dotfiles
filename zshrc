HISTFILE=~/.zsh_hist
HISTSIZE=5000
SAVEHIST=5000

# The following lines were added by compinstall
zstyle :compinstall filename '/home/mauro/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

source ~/antigen/antigen.zsh

# Load the oh-my-zsh's library.
antigen use oh-my-zsh

# Bundles from the default repo (robbyrussell's oh-my-zsh).
antigen bundle command-not-found
antigen bundle compleat
antigen bundle cargo
antigen bundle git
antigen bundle golang
antigen bundle lein
antigen bundle pip
antigen bundle python
antigen bundle ripgrep
antigen bundle rust
antigen bundle sudo
antigen bundle chrissicool/zsh-256color
#antigen bundle zsh-users/zsh-autosuggestions
antigen bundle z

# Syntax highlighting bundle.
antigen bundle zsh-users/zsh-syntax-highlighting

source ~/.aliases

setopt incappendhistory
unsetopt share_history
unsetopt PROMPT_SP

source ~/.keybindings.zsh

eval $(dircolors -b $HOME/.dircolors)

source ~/.profile

# Load the theme.
antigen theme robbyrussell

# Tell antigen that you're done.
antigen apply

export PATH="/home/mauro/.local/bin:$PATH"

# Setup zsh-autosuggestions
#source ~/.antigen/repos/https-COLON--SLASH--SLASH-github.com-SLASH-zsh-users-SLASH-zsh-autosuggestions.git/zsh-autosuggestions.zsh
source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh

plugins=(fzf-zsh)

source /home/mauro/.config/broot/launcher/bash/br
fpath+=~/.zfunc
