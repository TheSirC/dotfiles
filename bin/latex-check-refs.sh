#! /usr/bin/env nix-shell
#! nix-shell -i bash -p comm ripgrep

comm -12 <(grep -oP '(?<=label{)[^}]+' thèse.tex | sort) <(grep -oP '(?<=ref{)[^}]+' thèse.tex | sort) | uniq
