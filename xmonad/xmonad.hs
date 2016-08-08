import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run
import System.IO
import XMonad.Layout.Grid
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.NoBorders
import XMonad.Layout.MultiColumns

import Data.Monoid
import Data.List
import System.Exit
import XMonad.Actions.Warp
import XMonad.Actions.WindowGo
import XMonad.Actions.CycleRecentWS
import XMonad.Actions.Search
import XMonad.Actions.Navigation2D
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.FadeWindows

import XMonad.Util.CustomKeys

import qualified XMonad.Prompt                as P
import qualified XMonad.Actions.Submap        as SM
import qualified XMonad.Actions.Search        as S
import qualified XMonad.StackSet              as W
import qualified Data.Map                     as M
import qualified XMonad.Util.WindowProperties as WP

--myLayout = tiled ||| Mirror tiled ||| Full
--where
    -- default proportion of screen occupied by master pane
    --ratio = 2/3
    -- percent icnrement when resizing screen
    --delta = 5/100



----------------------------------------------- LAYOUT ----------------------------------------------------------
myLayout = avoidStruts $ tiled ||| Mirror (tiled) ||| noBorders Full ||| GridRatio (16/10) ||| noFrillsDeco shrinkText defaultTheme (GridRatio (16/10)) ||| noFrillsDeco shrinkText defaultTheme (smartBorders multiCol [1] 4 0.01 0.5)
  where
    -- default tiling algorithm partitions the screen into two panes
    tiled   = Tall nmaster delta ratio
    -- The default number of windows in the master pane
    nmaster = 1
    -- Default proportion of screen occupied by master pane
    ratio   = 1/2
    -- Percent of screen to increment by when resizing panes
    delta   = 3/100


myOldLayout = avoidStruts $ layoutHook defaultConfig
------------------------------------------------------------------------------------------------------------------

----------------------------------------------- KEY MAP ----------------------------------------------------------
workspaceKeys :: XConfig l -> [((KeyMask, KeySym), X ())]
workspaceKeys conf@(XConfig {XMonad.modMask = modm}) = 
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) myWorkspace_keys
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
------------------------------------------------------------------------------------------------------------------

----------------------------------------------- WORKSPACES ----------------------------------------------------------

--Define workspaces names
myWorkspaces = ["1:main","2:reference","3:output","4:misc","5:writing","6:output","7:reading","8:media","9:social","0","-","=","N0","N1","N2","N3","N4","N5","N6","N7","N8","N9", "`"]

myWorkspace_keys = [xK_1, xK_2, xK_3, xK_4, xK_5, xK_6, xK_7, xK_8, xK_9, xK_0, xK_minus, xK_equal, xK_KP_0, xK_KP_1, xK_KP_2, xK_KP_3, xK_KP_4, xK_KP_5, xK_KP_6, xK_KP_7, xK_KP_8, xK_KP_9, xK_grave]
--work1: ⌂1
--work2: ⌂2
--output: 
--reference
--
--writing: ✎
--output: 
--reference: 
--
--computeradmin: 
--reference: 
--
--
--
--readingCasual
--readingWork
--music: ♫
--email: ✉
--calendar: 
--social: ☺

----------------------------------------------- APPLY ----------------------------------------------------------

main = do
xmproc <- spawnPipe "/usr/bin/xmobar ~/dotfiles/xmonad/xmobarrc" --requires the installation of xmobar (sudo pacman -S xmobar)
xmonad $ defaultConfig
    {borderWidth=1,
     layoutHook = myLayout,
     workspaces = myWorkspaces,
     manageHook = manageDocks <+> manageHook defaultConfig, --requires the installation of xmonad-contrib ? 
     logHook = dynamicLogWithPP xmobarPP
        { ppOutput = hPutStrLn xmproc,
          ppTitle = xmobarColor "blue" "" . shorten 50,
          ppLayout = const "" -- to disable the layout info on xmobar
        }
     --, keys         = customKeys [()] myKeys
     }


