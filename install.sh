#!/usr/bin/bash

DOTFILES_LOCATION=$HOME'/dotfiles/'
FURTHER_ACTIONS=

function link {
    #PROGRAM_NAME, DOTFILE, TARGET, FURTHER_ACTION
    echo "Linking $1"
    ln -sf "$2" "$3"
    #Recommend later actions
    if [ $4 ]; then
        FURTHER_ACTIONS=$1': '$4
    fi
}

#array=( ('vim', '/usr/bin/vim', '~/dotfiles/vimrc/.vimrc', '~/tmp/.vimrc', ''),
       array=( 'xinit;/usr/bin/xrdb;'$DOTFILES_LOCATION'xinitrc/.xinitrc;'$HOME'/.xinitrc;' 'Xresources;/usr/bin/xrdb;'$DOTFILES_LOCATION'Xresources/.Xresources;'$HOME'/.Xresources;restart_xstart' 'Xresources;/usr/bin/xrdb;'$DOTFILES_LOCATION'Xresources/.Xresources.d;'$HOME'/.Xresources.d;restart_xstart' 'vim;/usr/bin/vim;'$DOTFILES_LOCATION'vimrc/.vimrc;'$HOME'/.vimrc;' )

#Loop through things to link
for str in "${array[@]}"; do
    PROGRAM_NAME=$(echo $str | awk -F';' '{print $1}')
    INSTALLATION=$(echo $str | awk -F';' '{print $2}')
    DOTFILE=$(echo $str | awk -F';' '{print $3}')
    TARGET=$(echo $str | awk -F';' '{print $4}')
    FURTHER_ACTION=$(echo $str | awk -F';' '{print $5}')
    
    if [ -f $INSTALLATION ]; then #Check for installation
        if [ -f $TARGET ]; then #Check for existing links/files
            read -r -p "WARNING: files/links ($TARGET) already exist, overwrite? [y/N] " response
            if [[ $response =~ ^([yY][eE][sS]|[yY])$ ]]; then #Prompt user for action 
                link $PROGRAM_NAME $DOTFILE $TARGET $FURTHER_ACTION
            else
                echo "Skipping $PROGRAM_NAME; files already exist."
            fi
        else
            read -r -p "Do you want to link dotfiles for $PROGRAM_NAME? [Y/n] " response
            if [[ $response =~ ^([nN][oO]|[nN])$ ]]; then #Prompt user for action
                echo "Skipping $PROGRAM_NAME on user request."
            else
                link $PROGRAM_NAME $DOTFILE $TARGET $FURTHER_ACTION
            fi
        fi
    else
        echo "Skipping $PROGRAM_NAME; no installation found at $INSTALLATION."
    fi
done

if [ ! -z "$FURTHER_ACTIONS" ]; then
    echo "Linking complete. It is now recommended that you do the following: "    
    echo $FURTHER_ACTIONS
else
    echo "Completed."
fi
