import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Layout.NoBorders(smartBorders)
import XMonad.Hooks.ManageHelpers
import XMonad.Util.Run
import System.IO
import qualified XMonad.StackSet as W

--Define mod key
modm = mod1Mask

--myLayout = tiled ||| Mirror tiled ||| Full
--where
    -- default proportion of screen occupied by master pane
    --ratio = 2/3
    -- percent icnrement when resizing screen
    --delta = 5/100

--Define workspaces names
myExtraWorkspaces = [(xK_0, "0:✍"), (xK_minus, "-:"), (xK_equal, "=:☜"), (xK_grave, "`:✉")]
myWorkspaces = ["1:✎","2:✑","3:☝","4:⌨","5:✒","6:☞","7:‖/","8:♫","9:☺"] ++ (map snd myExtraWorkspaces)


-- Key bindings
myKeys = [
        ((modm, key), (windows $ W.greedyView ws)) | (key, ws) <- myExtraWorkspaces
        ] ++ [
         ((modm .|. shiftMask, key), (windows $ W.shift ws)) | (key, ws) <- myExtraWorkspaces
        ]

myConfig = defaultConfig
    { manageHook = ( isFullscreen --> doFullFloat ) <+> manageDocks <+> manageHook defaultConfig
    --, manageHook = manageDocks <+> manageHook defaultConfig --requires the installation of xmonad-contrib ? 
    , layoutHook = smartBorders (avoidStruts $ layoutHook defaultConfig)
    --, terminal = "urxvt"
    , borderWidth=1
    --, modMask = mod4Mask --sets mod key to windows button
    --, layoutHook = myLayout
    , workspaces = myWorkspaces
    } `additionalKeys` myKeys

-- The main function
main = xmonad =<< statusBar myBar myPP toggleStrutsKey myConfig

-- Launch the bar
myBar = "xmobar ~/dotfiles/xmonad/xmobarrc" --requires the installation of xmobar (sudo pacman -S xmobar)

-- Custom PP, configure it as you like. It determines what is being written ot the bar.
myPP = xmobarPP { ppLayout = const "" -- to disable the layout infor on xmobar
                --, ppCurrent = xmobarColor "green" "" . wrap "<" ">" . shorten 68
                , ppTitle = xmobarColor "blue" "" . shorten 50
                }
--myPP = xmobarPP
--        { ppOutput = hPutStrLn spawnPipe "/usr/bin/xmobar ~/dotfiles/xmonad/xmobarrc"
--        , ppTitle = xmobarColor "blue" "" . shorten 50
--        , ppLayout = const "" -- to disable the layout info on xmobar
--        }

-- Key binding to toggle the gap for the bar.
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)
