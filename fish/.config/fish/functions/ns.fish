function ns --description 'Build a shell with the given arguments'
    nix-shell -p $argv[1]
end
