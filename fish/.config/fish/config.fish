set -gx EDITOR emacs
set -gx BROWSER firefox
set -g fish_key_bindings fish_vi_key_bindings

direnv hook fish | source
any-nix-shell fish --info-right | source
cod init $fish_pid fish | source
zoxide init fish | source

# Miscellanious
abbr -a j 'just'
abbr -a pp 'xclip -i -selection c'
abbr -a em 'emacs -nw'

# Firefox search
abbr -a ffs 'firefox -asearch'
abbr -a ffsy 'firefox https://www.youtube.com/results\?search_query='

# CLI tools
abbr -a g 'rg'
abbr -a e 'eza'
abbr -a fdf 'fd -t f '
abbr -a fdd 'fd -t d '

# Navigation
abbr -a ... 'cd ../..'
abbr -a .... 'cd ../../..'
abbr -a ..... 'cd ../../../..'

abbr -a tmp 'cd ~/tmp'

# Nix-rebuild
abbr -a nrt 'pushd $OUROBOROS; just t; popd'
abbr -a nrb 'pushd $OUROBOROS; just b; popd'
abbr -a nri 'pushd $OUROBOROS; just i; popd'
abbr -a nrs 'pushd $OUROBOROS; just; popd'
abbr -a our 'emacs -nw $OUROBOROS'

# Nix related shortcuts
abbr -a nei 'nix-env -q --installed'
abbr -a ncu 'nix-channel --update'
abbr -a nsp 'nix-shell -p'
abbr -a nsb 'nix-shell '
abbr -a nrn 'nix run nixpkgs#'
