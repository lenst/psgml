perl  -i.bak -p -e \
's!<a href="emacs.html#[^"]+">([^<]*)</a>!\1 in <cite>The Emacs Editor</cite>!' \
    psgml.html
    

