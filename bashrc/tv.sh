# Small utility for making/searching notes/snippets
tv () {
    root="$HOME/.local/notes/"
    res="$(find $root -type f -printf '%P\n' | fzy)"
    if [[ ! -z "$res" ]]; then
        vim "$root/$res"
    fi
}
