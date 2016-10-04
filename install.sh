#!/usr/bin/bash

DOTFILES_LOCATION=$HOME'/dotfiles/'
FURTHER_ACTIONS=

function link {
    #PROGRAM_NAME, DOTFILE, TARGET, FURTHER_ACTION
    echo "Linking $1"
    ln -sf "$2" "$3"
    #Recommend later actions
    if [ $4 ]; then
        echo $4
        FURTHER_ACTIONS=$4\n$1: $4
    fi
}

#array=( ('vim', '/usr/bin/vim', '~/dotfiles/vimrc/.vimrc', '~/tmp/.vimrc', ''),
       i=('xinit' '/usr/bin/xrdb' $DOTFILES_LOCATION'xinitrc/.xinitrc' $HOME'/.xinitrc' '')
      #)


#Loop through things to link
#for i in "${array[@]}"; do
    PROGRAM_NAME=${i[0]}
    INSTALLATION=${i[1]}
    DOTFILE=${i[2]}
    TARGET=${i[3]}
    FURTHER_ACTION=${i[4]}
    
    if [ -f $INSTALLATION ]; then #Check for installation
        if [ -f $TARGET ]; then #Check for existing links/files
            read -r -p "WARNING: files/links already exist, overwrite? [y/N] " response
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
#done

if [ $FURTHER_ACTIONS ]; then
    echo "Linking complete. It is now recommended that you do the following: "    
    echo $FURTHER_ACTIONS
else
    echo "Completed."
fi
