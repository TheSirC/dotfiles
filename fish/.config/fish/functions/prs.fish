function nrap --description 'Create a new paste' \
    curl --data-binary @$argv https://paste.rs/ | xclip -i -selection c
end
