function mkcd --description 'Makes a new directory and goes into it'
    mkdir $argv[1] && cd $argv[1]
end
