# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

HISTFILE=~/.histfile
HISTSIZE=10000
SAVEHIST=10000
setopt nomatch
unsetopt autocd beep extendedglob notify
bindkey -v
zstyle :compinstall filename '/home/priime/.zshrc'

autoload -Uz compinit
compinit

# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH="/home/priime/.oh-my-zsh"

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
ZSH_THEME="robbyrussell"

# Set list of themes to pick from when loading at random
# Setting this variable when ZSH_THEME=random will cause zsh to load
# a theme from this variable instead of looking in $ZSH/themes/
# If set to an empty array, this variable will have no effect.
# ZSH_THEME_RANDOM_CANDIDATES=( "robbyrussell" "agnoster" )

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion.
# Case-sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to automatically update without prompting.
# DISABLE_UPDATE_PROMPT="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line if pasting URLs and other text is messed up.
# DISABLE_MAGIC_FUNCTIONS="true"

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# You can set one of the optional three formats:
# "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# or set a custom format using the strftime function format specifications,
# see 'man strftime' for details.
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load?
# Standard plugins can be found in $ZSH/plugins/
# Custom plugins may be added to $ZSH_CUSTOM/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git)

source $ZSH/oh-my-zsh.sh

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"
source ~/powerlevel10k/powerlevel10k.zsh-theme

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

export PS1="%d %% "
autoload -Uz vcs_info
precmd() { vcs_info }

# Macros
alias open="launch.sh"

alias ls="exa --sort=type"
alias ll="exa -a --sort=type"
alias lll="exa -alh --sort=type"
alias l1="exa -a1h --sort=type"
alias lsd="exa -D"
alias tree="exa -T"

alias mv="mv -i"
alias mkdir="mkdir -p"

alias v="open neovide"
alias h="helix"
alias f="fzf -i"

alias dlmp4="youtube-dl -f mp4"
alias cpr="rustc -o bin"
alias psax="ps -ax | grep -i"
alias copy="xclip -i"
alias paste="xclip -o"

alias g="grep"
alias gi="grep -i"

alias gccwarn="gcc -Wall -Wextra -Wpedantic"
alias clangwarn="clang -Wall -Wextra -Wpedantic"

alias alsamixer="alsamixer -c 0"
alias df="df -h"

alias tb="open zathura"
alias notes="open marktext"
alias kdec="kdeconnect-cli"

alias n="nnn -dQe"
alias sus="systemctl suspend"
alias lock="dm-tool lock"

alias lstar="tar -ztvf"
alias untar="tar -zxvf"
alias mktar="tar -cvzf"

alias manim="python3 -m manim"

alias weather="curl 'wttr.in/Boston?M'"

alias clear="printf '\033[2J\033[3J\033[1;1H'"

function cprog() {
    v /tmp/$((1 + $RANDOM)).cpp
}

function cd() {
    builtin cd $@ && ll
}

function psg() {
    psax | gi $1
}

function mkcd() {
    mkdir $1 && cd $1
}

function cacd() {
    cargo new $1 && cd $1
}

function spot() {
    pkill spotifyd
    spotifyd
    spt
}

function remind() {
    echo 'notify-send REMINDER $2' | at $1
}

# Meme Macros
# alias nano="echo \"fuck nano\" && sleep 2 && nvim"
# alias emacs="echo \"fuck emacs\" && sleep 2 && nvim"
# alias vim="echo \"you won't notice a difference if i...\" && sleep 3 && nvim"
alias pls="sudo"
alias fuck="sudo !!"

# Git Macros
alias ga="git add"
alias gb="git branch"
alias gc="git commit -S --signoff"
alias gco="git checkout"
alias gd="git diff"
alias gdc="git diff --cached"
alias gl="git log"
alias glf="git log --pretty=oneline --name-status"
alias gls="git log --show-signature"
alias gr="git restore"
alias grs="git restore --staged"
alias gs="git status"
alias gup="git update"

function gdd() {
    gd HEAD~$1..HEAD
}

function gddd() {
    gd HEAD~$1..HEAD~$2
}

function glp() {
    git log -S $1 --patch --reverse
}

# bonsai.sh -b 3 -L 50 -M 10

# opam configuration
[[ ! -r /home/priime/.opam/opam-init/init.zsh ]] || source /home/priime/.opam/opam-init/init.zsh  > /dev/null 2> /dev/null
