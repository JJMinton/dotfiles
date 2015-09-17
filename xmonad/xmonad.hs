import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run
import System.IO

--myLayout = tiled ||| Mirror tiled ||| Full
--where
    -- default proportion of screen occupied by master pane
    --ratio = 2/3
    -- percent icnrement when resizing screen
    --delta = 5/100

--Define workspaces names
myWorkspaces = ["1:main","2:reference","3:output","4:misc","5:writing","6:output","7:reading","8:media","9:social"]

main = do
xmproc <- spawnPipe "/usr/bin/xmobar ~/dotfiles/xmonad/xmobarrc" --requires the installation of xmobar (sudo pacman -S xmobar)
xmonad $ defaultConfig
    {borderWidth=2,
     layoutHook = avoidStruts $ layoutHook defaultConfig,
     --layoutHook = myLayout,
     workspaces = myWorkspaces,
     manageHook = manageDocks <+> manageHook defaultConfig,
     logHook = dynamicLogWithPP xmobarPP
        { ppOutput = hPutStrLn xmproc,
          ppTitle = xmobarColor "blue" "" . shorten 50,
          ppLayout = const "" -- to disable the layout info on xmobar
        }
     }
