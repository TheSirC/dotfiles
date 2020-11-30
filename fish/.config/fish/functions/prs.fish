function prs
    curl --data-binary @$argv https://paste.rs/ | xclip -i -selection c
end
