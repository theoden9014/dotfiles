# for profiler
#zmodload zsh/zprof
#
# Setup zplug
#
export ZPLUG_HOME="${HOME}/.zplug"
export ZPLUG_CACHE_DIR="${HOME}/.cache/zplug"
if [[ -s "${ZPLUG_HOME}/init.zsh" ]]; then
  source ${ZPLUG_HOME}/init.zsh
fi
#zplug 'zplug/zplug', hook-build:'zplug --self-manage'

zplug "modules/environment", from:prezto
zplug "modules/terminal", from:prezto
zplug "modules/editor", from:prezto
zplug "modules/history", from:prezto
zplug "modules/directory", from:prezto
zplug "modules/spectrum", from:prezto
zplug "modules/utility", from:prezto
zplug "modules/completion", from:prezto
zplug "modules/git", from:prezto
zplug "modules/homebrew", from:prezto
zplug "modules/ruby", from:prezto
zplug "modules/python", from:prezto
zplug "modules/autosuggestions", from:prezto
zplug "modules/tmux", from:prezto
zplug "modules/syntax-highlighting", from:prezto
zplug "moduels/history-substring-search", from:prezto

zplug 'modules/prompt', from:prezto
zstyle ':prezto:module:prompt' theme 'sorin'
zstyle ':prezto:*:*' color 'yes'

zstyle ':prezto:module:python:virtualenv' auto-switch 'yes'
#zstyle ':prezto:module:tmux:auto-start' local 'yes'

zstyle ':prezto:module:syntax-highlighting' color 'yes'

zplug "plugins/kubectl", from:oh-my-zsh
zplug "plugins/golang", from:oh-my-zsh


# Install plugins if there are plugins that have not been installed
if ! zplug check --verbose; then
    printf "Install? [y/N]: "
    if read -q; then
        echo; zplug install
    fi
fi

zplug load

#
# Configuration from here
#

# Prompt
autoload -Uz promptinit
promptinit
prompt sorin
export LSCOLORS=GxFxCxDxBxegedabagaced

# SSH Agent
{ eval "$(ssh-agent)" > /dev/null }
ssh-add -q -k ${HOME}/.ssh/id_rsa*

# Homebrew
export PATH="/usr/local/sbin:$PATH"
export HOMEBREW_GITHUB_API_TOKEN=$GITHUB_API_TOKEN

# rbenv
export PATH="$HOME/.rbenv/bin:$PATH"
{ eval "$(rbenv init -)" }

# pyenv
export PATH="$HOME/.pyenv/bin:$PATH"
{ eval "$(pyenv init -)" }

# nodenv
export PATH="$HOME/.nodenv/bin:$PATH"
{ eval "$(nodenv init -)" }
export NODE_PATH=$(npm root -g)

# Go
export GOENV_DISABLE_GOPATH=1
{ eval "$(goenv init -)" }
export GOPATH="$HOME/.local/go"
export PATH="$PATH:$GOPATH/bin"
# export GOROOT=/usr/local/opt/go
# export PATH="$PATH:$GOPATH/bin:$GOROOT/bin"
export GOPRIVATE=github.dena.jp

# Java
export JAVA_HOME="$(/usr/libexec/java_home)"

## android-sdk
export ANDROID_HOME="/usr/local/share/android-sdk"
#export ANDROID_HOME="$HOME/Library/Android/sdk"
#export ANDROID_SDK_ROOT=$ANDROID_HOME
## export ANDROID_HOME="$HOME/Library/Android/sdk"
export PATH="$ANDROID_HOME/platform-tools:$PATH"
## export PATH="$PATH:$ANDROID_HOME/tools"
export PATH="$ANDROID_HOME/tools/bin:$PATH"
export PATH="$ANDROID_HOME/emulator:$PATH"
#export PATH="$ANDROID_HOME/ndk-bundle:$PATH"

# Android
export ANDROID_SDK_ROOT="/usr/local/share/android-sdk"
export ANDROID_HOME="/usr/local/share/android-sdk"
export ANDROID_NDK_HOME="${ANDROID_HOME}/ndk-bundle/"
export PATH="${PATH}:$ANDROID_NDK_HOME"

# Emacs
alias emacs='emacs -nw'

# GCP
export CLOUDSDK_PYTHON=$(which python3)
export GOOGLE_APPLICATION_CREDENTIALS="${HOME}/.config/gcloud/application_default_credentials.json"
export PATH="${PATH}:/Users/junki.kaneko/Library/go_appengine"
# The next line updates PATH for the Google Cloud SDK.
if [ -f "${HOME}/.local/google-cloud-sdk/path.zsh.inc" ]; then . "${HOME}/.local/google-cloud-sdk/path.zsh.inc"; fi
# The next line enables shell command completion for gcloud.
if [ -f "${HOME}/.local/google-cloud-sdk/completion.zsh.inc" ]; then . "${HOME}/.local/google-cloud-sdk/completion.zsh.inc"; fi

# VScode
export PATH="${PATH}:/Applications/Visual Studio Code.app/Contents/Resources/app/bin"

# Qt
export QT_DIR=/usr/local/Cellar/qt/5.10.0
export PATH=$QT_DIR/bin:$PATH

# OpenSSL
export PATH="/usr/local/opt/openssl/bin:$PATH"
# compile options
#export LDFLAGS="-L/usr/local/opt/openssl/lib"
#export CPPFLAGS="-I/usr/local/opt/openssl/include"
# pkg-config
#export PKG_CONFIG_PATH="/usr/local/opt/openssl/lib/pkgconfig"

# Dir ENV
eval "$(direnv hook zsh)"

#
# Alias
#
alias gcd='cd $(ghq list -p | peco)'
alias ldd='otool -L'
alias lsusb='system_profiler SPUSBDataType'
alias helm2="/usr/local/opt/helm@2/bin/helm"

function adb-screenshot {
    local datetime=`date +"%Y%m%d-%H%M%S"`
    local filename=${datetime}.png
    local android_filepath=/sdcard/${filename}

    adb shell screencap -p $android_filepath
    pushd ~/Desktop
    adb pull $android_filepath
    adb shell rm -f $android_filepath
    popd
}

function yaml-validator {
    local filename=$1

    ruby -e "require 'yaml';puts YAML.load_file('./${filename}')"
}

function update-anyenv {
    local nodenv_path=$(nodenv root)
    local rbenv_path=$(rbenv root)
    local pyenv_path=$(pyenv root)

    for env_path in $nodenv_path $rbenv_path $pyenv_path;
    do
        pushd $env_path
        git pull origin master
        for plugin_path in $(ls plugins/);
        do
            pushd plugins/$plugin_path
            git pull origin master
            popd
        done
        popd
    done
}

function vpn-connect {
    networksetup -connectpppoeservice JP-VPN-MFA
    vpn_status="$(networksetup -showpppoestatus JP-VPN-MFA)"
    while [ $vpn_status != "connected" ]; do
	sleep 1
	vpn_status="$(networksetup -showpppoestatus JP-VPN-MFA)"
    done
}

function vpn-disconnect {
    networksetup -disconnectpppoeservice JP-VPN-MFA
}

function gcloudctx {
    export _PS1
    export GCLOUDCTX_NAME

    if [[ -z "$_PS1" ]]; then
	_PS1=$PS1
    fi

    case $1 in
	"" | "ls" | "list")
	    gcloud config configurations list
	    GCLOUDCTX_NAME=""
	    ;;
	"add" | "create")
	    GCLOUDCTX_NAME=$2
	    gcloud config configurations create $GCLOUDCTX_NAME
	    ;;
	del*)
	    GCLOUDCTX_NAME=$2
	    gcloud config configurations delete $GCLOUDCTX_NAME
	    GCLOUDCTX_NAME=""
	    ;;
	*)
	    GCLOUDCTX_NAME=$1
	    gcloud config configurations activate $GCLOUDCTX_NAME
	    ;;
    esac
    if [[ -n "$GCLOUDCTX_NAME" ]] && [[ ! "$PS1" =~ "^$GCLOUDCTX_NAME" ]]; then
	PS1="$GCLOUDCTX_NAME $_PS1"
    else
	PS1=$_PS1
    fi
}

# for profiler
#zprof
