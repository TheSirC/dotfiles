#! /usr/bin/env nix-shell
#! nix-shell -i bash -p "texlive.combine { inherit (texlive) scheme-small enumitem xifthen ifmtarg fontawesome sourcesanspro tcolorbox environ trimspaces; }"

pandoc --template template/lettre_de_motivation.tex --pdf-engine xelatex $1 -o "$(dirname $1)/${1%.md}.pdf"
