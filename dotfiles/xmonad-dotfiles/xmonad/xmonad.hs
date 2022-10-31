--Base
import XMonad
import System.Directory
import System.IO (hClose, hPutStr, hPutStrLn)
import System.Exit (exitSuccess, exitWith)
import qualified XMonad.StackSet as W

--Actions
import XMonad.Actions.GridSelect
import XMonad.Actions.MouseResize

--Data
import qualified Data.Map as M 
import Data.Monoid

--Utils
import XMonad.Util.Run
import XMonad.Util.SpawnOnce 
import XMonad.Util.EZConfig  
import XMonad.Util.Loggers
import XMonad.Util.Hacks 

--layouts
import XMonad.Layout.Accordion
import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spiral
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns

--Layout mods
import XMonad.Layout.Gaps
import XMonad.Layout.Spacing
import XMonad.Layout.LayoutModifier
import XMonad.Layout.NoBorders
import XMonad.Layout.Simplest
import XMonad.Layout.IndependentScreens

--Hooks
import XMonad.Hooks.ManageDocks 
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat, doCenterFloat, isDialog)
import XMonad.Hooks.ServerMode
import XMonad.Hooks.WindowSwallowing 

main :: IO()
main = xmonad
     $ myConfig

myModMask :: KeyMask
myModMask = mod4Mask

myTerminal :: String
myTerminal = "alacritty"

myBrowser :: String
myBrowser = "firefox"

myBorderWidth :: Dimension
myBorderWidth = 1

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myClickJustFocuses :: Bool
myClickJustFocuses = False

myNormColor :: String       -- Border color of normal windows
myNormColor   = "#191917"

myFocusColor :: String      -- Border color of focused windows
myFocusColor  = "#FFFFFF"

myStartupHook :: X ()
myStartupHook = do
       spawnOnce "picom"
       spawnOnce "newq"
       spawnOnce "mpd"
       spawnOnce "fixmon"

mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

myLayoutHook =avoidStruts 
             $ mySpacing 3
       --    $ mouseResize
       --    $ windowArrange
             $ gaps [(U,5), (R,4), (D,5), (L,4)]
             $ (tiled ||| Mirror tiled ||| Full)
       --    $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) myDefaultLayout
 where 
   tiled = Tall nmaster delta ratio
   nmaster = 1
   ratio = 1/2
   delta = 3/100

toggleFull = withFocused (\windowId -> do
    { floats <- gets (W.floating . windowset);
        if windowId `M.member` floats
        then withFocused $ windows . W.sink
        else withFocused $ windows . (flip W.float $ W.RationalRect 0 0 1 1) })

myWorkspaces :: [String]
myWorkspaces = withScreens 1 ["1","2","3","4","5","6","7","8","9"]
              

myManageHook :: ManageHook
myManageHook = composeAll
    [ className =? "burpsuite.jar" --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , isDialog            --> doFloat
    ] 

myEventHook = swallowEventHook (className =? "Alacritty") (return True)

myConfig = def
      { terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormColor,
        focusedBorderColor = myFocusColor,

        keys               = myKeys,
        mouseBindings      = myMouseBindings,
        
        layoutHook         = myLayoutHook,
        manageHook         = myManageHook,
        startupHook        = myStartupHook,
        handleEventHook    = myEventHook
      }

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
  
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
    , ((modm,               xK_p     ), spawn "exec ~/.config/rofi/launchers/type-1/launcher.sh")
    , ((modm .|. shiftMask, xK_p     ), spawn "rofi-theme-selector")
    , ((modm .|. controlMask, xK_w     ), spawn $ "firefox")
    , ((modm,               xK_q     ), spawn "alacritty -e ranger")
    , ((modm .|. shiftMask, xK_c     ), kill)
    , ((modm,               xK_space ), sendMessage NextLayout)
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
    , ((modm,               xK_n     ), refresh)
    , ((modm,               xK_Tab   ), windows W.focusDown)
    , ((modm,               xK_k     ), windows W.focusUp  )
    , ((modm,               xK_m     ), windows W.focusMaster  )
    , ((modm,               xK_Return), windows W.swapMaster)
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )
    , ((modm,               xK_h     ), sendMessage Shrink)
    , ((modm,               xK_l     ), sendMessage Expand) 
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))
    , ((modm              , xK_f     ), toggleFull)
    , ((modm .|. shiftMask, xK_q     ), io (exitSuccess))
    , ((modm              , xK_i     ), spawn "xmonad --recompile; xmonad --restart") 
    ]
    ++

    --[((m .|. modm, k), windows $ f i)
      --  | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
      --  , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
      [((m .|. modm, k), windows $ onCurrentScreen f i)
        | (i, k) <- zip (workspaces' conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))
    ]


