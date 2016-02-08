import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Layout.Spacing
import System.IO
myLayout = tiled ||| Mirror tiled ||| Full
 where
      -- default tiling algorithm partitions the screen into two panes
      tiled = spacing 5 $ Tall nmaster delta ratio

      -- The default number of windows in the master pane
      nmaster = 1

      -- Default proportion of screen occupied by master pane
      ratio = 2/3

      -- Percent of screen to increment by when resizing panes
      delta = 5/100

myWorkspaces = ["1:main", "2:mail", "3:web", "4:code", "5", "6"]

 main = do
    xmproc <- spawnPipe "xmobar"

    xmonad $ defaultConfig
        {workspaces = myWorkspaces
	,borderWidth = 2
	,normalBorderColor = "#abc123"
	,focusedBorderColor = "456def"
	,layoutHook = avoidStruts  $  layoutHook defaultConfig
	, manageHook=manageHook defaultConfig <+> manageDocks
	, handleEventHook = mconcat
                  [ docksEventHook
                  , handleEventHook defaultConfig ]
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "green" "" . shorten 50
                        }
        , modMask = mod4Mask     -- Rebind Mod to the Windows key
        } `additionalKeys`
        [ ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock; xset dpms force off")
        , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
        , ((0, xK_Super_L), return ())
        , ((0, xK_Print), spawn "scrot")
        ]
