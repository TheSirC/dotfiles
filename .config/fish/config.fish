set -gx EDITOR nvim

export PATH="$HOME/.yarn/bin:$HOME/.config/yarn/global/node_modules/.bin:$PATH"
abbr -a -U -- add clcp
abbr -a -U -- clcp 'xclip -i -selection c'
abbr -a -U -- e exa
abbr -a -U -- nv nvim
abbr -a -U -- saa 'sudo add-apt-repository -y'
abbr -a -U -- sai 'sudo apt-get install -y'
abbr -a -U -- sau 'sudo apt-get update -y'
abbr -a -U -- saup 'sudo apt-get update -y && sudo apt-get upgrade -y'
abbr -a -U -- tmp 'cd /tmp'
abbr -a -U -- tsk 'nvim +TW'
