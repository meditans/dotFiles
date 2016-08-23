# .bashrc

############################################################ Source global definitions
if [ -f /etc/bashrc ]; then
	  . /etc/bashrc
fi

############################################################ fzf related config.
[ -f ~/.fzf.bash ] && source ~/.fzf.bash
# My defaults for FZF (mostly changed to see hidden files)
export FZF_DEFAULT_COMMAND='find -L . -type f -o -type d -o -type l | sed 1d | cut -b3- | grep -v -e .git/ -e .svn/ -e .hg/'
export FZF_CTRL_T_COMMAND=$FZF_DEFAULT_COMMAND
# Enhance the default experience
export FZF_DEFAULT_OPTS='--extended --multi --inline-info --prompt="fzf> "'

############################################################ Adding ghc and ghci to PATH
PATH=~/.local/bin:~/.stack/programs/x86_64-linux/ghc-7.10.3/bin:$PATH

############################################################ stack autocompletion
eval "$(stack --bash-completion-script stack)"

# Ghc core inspecter alias
alias ghci-core="ghci -ddump-simpl -dsuppress-idinfo \
-dsuppress-coercions -dsuppress-type-applications \
-dsuppress-uniques -dsuppress-module-prefixes"

########################################################### No case distinction in shell
shopt -s nocaseglob

########################################################### A true clear command
alias clear="clear && printf '\e[3J'"
