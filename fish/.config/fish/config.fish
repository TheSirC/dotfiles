set -gx EDITOR emacs

abbr -a tmp 'cd /tmp'
abbr -a pp 'xclip -i -selection c'
abbr -a em 'emacs -nw'

abbr -a ffs 'firefox -asearch'
abbr -a ffsy 'firefox https://www.youtube.com/results\?search_query='

abbr -a fi 'find . -iname'
abbr -a g 'rg'
abbr -a e 'exa'

# navigation
abbr -a ... 'cd ../..'
abbr -a .... 'cd ../../..'
abbr -a ..... 'cd ../../../..'

# Nix-rebuild
abbr -a nrt 'sudo nixos-rebuild test --fast'
abbr -a nrs 'sudo nixos-rebuild switch'
abbr -a our 'emacs -nw ~/Documents/Projets/ouroboros'
