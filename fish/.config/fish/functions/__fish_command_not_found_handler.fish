function __fish_command_not_found_handler --on-event fish_command_not_found
    echo "Le binaire n'a pas été trouvé. Dois-je aller le chercher dans les dépots ? [o/n]"
    if test (read | string match -i -v 'o') # L'argument "v" est là pour inverser la recherche
        return
    end

    set -l cmd $argv[1] # Récupération du nom de la commande
    set -l num_args (count $argv)

    if test $num_args -gt 1
        set -l args $argv[2..$num_args] # Récupération des arguments passés à la commande
    else
        set -l args ""
    end

    set -l results ( nix search nixpkgs.$cmd 2> /dev/null | rg "\*" )

    switch (count $results)
        case 0
            echo "Aucun package/binaire n'a été trouvé avec ce nom dans les dépots ¯\_(ツ)_/¯"
        case 1
            echo "Un binaire a été trouvé. Lancement via nix run. L'installation est temporaire !"
            nix run nixpkgs.$cmd -c $cmd $args
            return
        case '*'
            echo "Il semblerait que plusieurs packages existent avec ce nom :"
            echo -e ( string replace ")" ")\n" $results )
    end
end
