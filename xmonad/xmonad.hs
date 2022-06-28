-------------------------------------------------------------------------------
--                  __  ____  __                       _                     --
--                  \ \/ /  \/  | ___  _ __   __ _  __| |                    --
--                   \  /| |\/| |/ _ \| '_ \ / _` |/ _` |                    --
--                   /  \| |  | | (_) | | | | (_| | (_| |                    --
--                  /_/\_\_|  |_|\___/|_| |_|\__,_|\__,_|                    --
--                                                                           --
-------------------------------------------------------------------------------
--          written by Farah Abderrazzak Aka FrhXM(https://github.com/FrhXM)  --
-------------------------------------------------------------------------------
-- Import modules                                                           {{{
-------------------------------------------------------------------------------
-- Main
import XMonad                                                                                  
import System.Exit
import Control.Monad (liftM2)                                                                 
import qualified XMonad.StackSet as W                                                        
import qualified Data.Map as M                                                              

-- Actions
import XMonad.Actions.UpdatePointer
import XMonad.Actions.FindEmptyWorkspace
import XMonad.Actions.Minimize
import XMonad.Actions.WithAll (killAll, sinkAll)
import XMonad.Actions.WindowGo (raiseBrowser)
import XMonad.Actions.RotSlaves (rotSlavesDown)
import XMonad.Actions.WindowBringer (gotoMenu, bringMenu)

-- Hooks
import XMonad.ManageHook (doFloat)                                                         
import XMonad.Hooks.ManageHelpers (doCenterFloat, doFullFloat, isFullscreen)              
import XMonad.Hooks.ManageDocks (avoidStruts, docks)                                           
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, xmobarPP, xmobarColor, wrap, shorten, PP(..))
import XMonad.Hooks.FadeInactive                                                               

-- Utilities
import XMonad.Util.NamedScratchpad
import XMonad.Util.EZConfig (additionalKeysP)                                                 
import XMonad.Util.Run (spawnPipe, hPutStrLn)                                                
import XMonad.Util.SpawnOnce (spawnOnce)                                                    
import XMonad.Util.Cursor                                                                  

-- Layouts/Modifiers 
import XMonad.Layout.ComboP
import XMonad.Layout.Master
import XMonad.Layout.PerWorkspace                                                               
import XMonad.Layout.Maximize
import XMonad.Layout.Minimize
import XMonad.Layout.LimitWindows
import XMonad.Layout.Gaps
import XMonad.Layout.Spacing
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.ShowWName
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances

-- Layouts
import XMonad.Layout.ResizableTile
import XMonad.Layout.ThreeColumns
import XMonad.Layout.OneBig
import XMonad.Layout.TwoPanePersistent
import XMonad.Layout.TwoPane
import XMonad.Layout.Tabbed
import XMonad.Layout.Simplest
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Circle
import XMonad.Layout.Dishes
import XMonad.Layout.HintedGrid
import XMonad.Layout.Dwindle                                                    
import XMonad.Layout.Accordion

------------------------------------------------------------------------
-- Color Pallatte
------------------------------------------------------------------------
--- One Pallate ---
bg            = "#11121D"
fg            = "#a9b1d6"
black         = "#32344a"
red           = "#f7768e"
green         = "#9ece6a"
yellow        = "#e0af68"
blue          = "#7aa2f7"
magenta       = "#ad8ee6"
cyan          = "#449dab"
white         = "#787c99"


--- Two Pallate ---
black_       = "#444b6a"
red_         = "#ff7a93"
green_       = "#b9f27c"
yellow_      = "#ff9e64"
blue_        = "#7da6ff"
magenta_     = "#bb9af7"
cyan_        = "#0db9d7"
white_       = "#acb0d0"

------------------------------------------------------------------------
-- VARIABLES
------------------------------------------------------------------------
myFont :: [Char]
myFont = "xft:JetBrains Mono:style=Bold:pixelsize=13"

myBigFont :: [Char]
myBigFont = "xft:FiraCode Nerd Font Mono:pixelsize=80"

myModMask :: KeyMask
myModMask = mod1Mask       

altMask :: KeyMask
altMask = mod4Mask         

myTerminal :: [Char]
myTerminal = "kitty"   

myBorderWidth :: Dimension
myBorderWidth = 2         

myNormalColor :: [Char]
myNormalColor  = black

myFocusedColor :: [Char]
myFocusedColor  = blue

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

windowCount :: X (Maybe String)
windowCount  = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset  

------ Workspaces -------
-- wsDEV           = "¹DEV"
-- wsGIT           = "²GIT"
-- wsWEB           = "³WEB"
-- wsYTB           = "⁴YTB"
-- wsCHT           = "⁵CHT"
-- wsMSC           = "⁶MSC"
-- wsVED           = "⁷VED"
-- wsSIT           = "⁸SIT"
-- wsGME           = "⁹GME"
-- myWorkspaces    = [wsDEV,wsGIT,wsWEB,wsYTB,wsCHT,wsMSC,wsVED,wsSIT, wsGME]
myWorkspaces = [" 1 ", " 2 ", " 3 ", " 4 ", " 5 ", " 6 ", " 7 ", " 8 ", " 9 "] 

------------------------------------------------------------------------
-- Startup Hooks
------------------------------------------------------------------------
myStartupHook = do
    spawnOnce "xwallpaper --zoom ~/pix/wall/wall.png"                               -- Wallpapers
    spawnOnce "dunst"                                                               -- notfiction
    spawnOnce "unclutter"                                                           -- hidden Mouse
    spawnOnce "xset r rate 255 55"                                                  -- speeds cursor in urxvt
    spawnOnce "picom --experimental-backends -b"                                    -- Compositor
    setDefaultCursor xC_left_ptr                                                    -- Default Cursor

------------------------------------------------------------------------
-- Floats
------------------------------------------------------------------------
myManageHook = composeAll 
     [ className =? "firefox"           --> doViewShift " 3 "
     , className =? "Thunar"            --> doViewShift " 9 "
     , className =? "mpv"               --> doViewShift " 9 "
     , className =? "Sxiv"              --> doCenterFloat
     , className =? "Nitrogen"          --> doCenterFloat
     , className =? "Xmessage"          --> doCenterFloat
     , className =? "download"          --> doFloat
     , className =? "error"             --> doFloat
     , className =? "Gimp"              --> doFloat
     , className =? "notification"      --> doFloat
     ] <+> namedScratchpadManageHook myScratchPads
    where
     doViewShift = doF . liftM2 (.) W.view W.shift

------------------------------------------------------------------------
-- Scratch Pads
------------------------------------------------------------------------
myScratchPads = [ NS "terminal" spawnTerm findTerm manageTerm
                , NS "cmus" spawnCmus findCmus manageCmus
                , NS "browser" spawnBrowser findBrowser manageBrowser
                ]
  where
    spawnTerm  = myTerminal ++ " -T scratchpad"
    findTerm   = title =? "scratchpad"
    manageTerm = customFloating $ W.RationalRect l t w h
               where
                 h = 0.9
                 w = 0.9
                 t = 0.95 -h
                 l = 0.95 -w
    spawnCmus   = myTerminal ++ " -T cmus -e cmus"
    findCmus    = title =? "cmus"
    manageCmus  = customFloating $ W.RationalRect l t w h
               where
                 h = 0.9
                 w = 0.9
                 t = 0.95 -h
                 l = 0.95 -w
    spawnBrowser = "chromium"
    findBrowser = className =? "Chromium"
    manageBrowser = customFloating $ W.RationalRect (1/10) (1/20) (4/5) (9/10)

------------------------------------------------------------------------
-- Tiling Layouts
------------------------------------------------------------------------
-------------------- Base Layout ------------------------
myTabTheme      = def
                { fontName              = myFont
                , activeColor           = blue
                , inactiveColor         = bg
                , activeBorderColor     = blue
                , inactiveBorderColor   = bg
                , activeTextColor       = bg
                , inactiveTextColor     = fg
                }

tabs            = renamed [Replace "TABBED"]      
                $ noBorders
                $ maximizeWithPadding 16 
                $ minimize 
                $ myGaps 
                $ tabbed shrinkText myTabTheme

threeColMid     = renamed [Replace "THREECOLMID"] 
                $ maximizeWithPadding 16 
                $ minimize 
                $ mySpacings 
                $ ThreeColMid 1 (3/100) (1/2)

oneBig          = renamed [Replace "ONEBIG"]      
                $ maximizeWithPadding 16 
                $ minimize 
                $ mySpacings 
                $ OneBig (3/4) (3/4)

tall            = renamed [Replace "TILD"]  
                $ maximizeWithPadding 16 
                $ minimize 
                $ mySpacings 
                $ ResizableTall 1 (3/100) (1/2) []

twoPane         = renamed [Replace "TWOPANE"]
                $ maximizeWithPadding 16
                $ minimize
                $ mySpacings
                $ TwoPanePersistent Nothing (3/100) (1/2)

dishes          = renamed [Replace "DISHES"]
                $ maximizeWithPadding 16
                $ minimize
                $ mySpacings
                $ Dishes 2 (1/5)

circle          = renamed [Replace "CIRCLE"]
                $ maximizeWithPadding 16
                $ minimize
                $ mySpacings
                $ Circle


floats          =  renamed [Replace "FLOAT"]    
                $ maximizeWithPadding 16 
                $ minimize 
                $ myGaps 
                $ mySpacings 
                $ simplestFloat

grid            = renamed [Replace "GRID"]
                $ mySpacings
                $ maximizeWithPadding 16
                $ maximize
                $ minimize
                $ limitWindows 12
                $ GridRatio (4/3) False

spirals         = renamed [Replace "spirals"]
                $ maximizeWithPadding 16
                $ minimize
                $ mySpacings
                $ Dwindle R CW 1.5 1.1


full            = renamed [Replace "FULL"]       
                $ maximizeWithPadding 16 
                $ minimize 
                $ mySpacings 
                $ limitWindows 20 Full

    --------- Compine Layout ------------
masterTabbed    = renamed [Replace "MASTER TABBED"]       
                $ maximizeWithPadding 16 
                $ minimize 
                $ mastered (1/100) (1/2) $ tabbed shrinkText myTabTheme

oneUp           = renamed [Replace "1UP"]
                $ mySpacings 
                $ combineTwoP (ThreeCol 1 (3/100) (1/2))
                                    (Simplest)
                                    (Tall 1 0.03 0.5)
                                    (ClassName "mpv")

twoTabbed       = renamed [Replace "TWO TABBED"]
                $ combineTwoP (TwoPane 0.03 0.5) 
                              (tabbed shrinkText myTabTheme) 
                              (tabbed shrinkText myTabTheme) 
                              (ClassName "mpv")

------------------------------------------------------------------------
-- Layout Hook
------------------------------------------------------------------------
mySpacings       = spacingRaw False (Border 0 0 0 0) True (Border 5 5 5 5) True
myGaps           = gaps [(U, 10),(D, 5),(L, 10),(R, 10)]
myShowWNameTheme = def
                { swn_font              = myBigFont
                , swn_fade              = 1.0
                , swn_bgcolor           = bg
                , swn_color             = blue
                }

myLayoutHook    = showWName' myShowWNameTheme
                $ mkToggle (NOBORDERS ?? FULL ?? EOT)
                $ limitWindows 12
                $ avoidStruts
                $ onWorkspaces [" 1 "," 2 "] codeLayouts
                $ onWorkspaces [" 3 "," 4 "] webLayouts
                $ onWorkspaces [" 5 "," 6 "] chatLayouts
                $ onWorkspaces [" 7 "," 8 "] settingsLayouts
                $ onWorkspace " 9 " mediaLayout
                $ allLayouts
               where 
    allLayouts = tall ||| full ||| twoPane ||| threeColMid ||| oneBig ||| dishes ||| grid
    webLayouts = oneBig ||| threeColMid ||| dishes ||| tall
      -- ||| floats
      -- ||| grid
      -- ||| spirals
    codeLayouts = dishes ||| twoPane ||| tabs
    chatLayouts = grid ||| threeColMid ||| tall
    youtubeLayouts = oneBig ||| full
    settingsLayouts = circle ||| grid ||| spirals ||| floats
    mediaLayout = oneUp ||| twoTabbed ||| masterTabbed ||| tabs 

------------------------------------------------------------------------
-- Custom Keys
------------------------------------------------------------------------
myKeys = 
        [
    -- Xmonad
        ("M-q", spawn "xmonad --recompile && xmonad --restart")     
      , ("M-S-q", io exitSuccess)                                  
    
    -- System 
                    --- Audio ---
      , ("<XF86AudioMute>",  spawn "pamixer -t && notify-send -t 200 'Toggle mute button!'") 	
      , ("<F9>",        spawn "pamixer -i 5 && notify-send -t 200 `pulsemixer --get-volume | awk '{print $1}'`")
      , ("<F8>",        spawn "pamixer -d 5 && notify-send -t 200 `pulsemixer --get-volume | awk '{print $1}'`")  			    
      , ("<F10>",       spawn "pamixer --default-source -t && notify-send -t 200 'Toggle mute Mic button'")   
                    --- Brightenss ---
      , ("<F5>",        spawn "xbacklight -dec 10 && notify-send -t 200 `xbacklight -get`")
      , ("<F6>",        spawn "xbacklight -inc 10 && notify-send -t 200 `xbacklight -get`")
                    --- ScreenShoot --- 
      , ("<Print>",     spawn "scrot -F ~/pix/screen/%Y-%m-%d-%T-screenshot.png && notify-send -t 800 'ScreenShot Takeen' 'Saved in ~/pix/screen/'"     )
      , ("M-<Print>",   spawn "scrot -u -F ~/pix/screen/%Y-%m-%d-%T-screenshot.png && notify-send -t 800 'ScreenShot Takeen' 'Saved in ~/pix/screen/'"  )
      , ("M-S-<Print>", spawn "scrot -s -F ~/pix/screen/%Y-%m-%d-%T-screenshot.png && notify-send -t 800 'ScreenShot Takeen' 'Saved in ~/pix/screen/'"  )
                    --- Scripts ---
      , ("M-S-w",       spawn "bash ~/prjct/scripts/rofi/wifiMenu.sh" )
      , ("M-0",         spawn "bash ~/prjct/scripts/rofi/powerMenu.sh")

    -- Run Prompt
      , ("M-S-d",       spawn "dmenu_run")                                                  
      , ("M-d",         spawn "rofi -show drun")                                           

    -- Apps
     , ("M-S-<Return>", spawn myTerminal)                                      
     , ("M-w",          spawn "firefox")                                                 
     , ("M-r",          spawn "redshift -O 3800K")                                        
     , ("M-x",          spawn "redshift -x")                                              


    -- Window navigation
    , ("M-t",           withFocused toggleFloat                 ) {-- Floating window --}
    , ("M-f",           sendMessage $ Toggle FULL               ) {--Full Screen --}
    , ("M-S-f",         withFocused (sendMessage . maximizeRestore)) {----For Maximaze With Paddings --}
    , ("M-e",           viewEmptyWorkspace                      ) {-- Find Empty Workspaces --}
    , ("M-g",           tagToEmptyWorkspace                     ) {-- Go To workspaces --}
    , ("M-n",           withFocused minimizeWindow                 ) {-- For Minimize && Action minimize --}
    , ("M-S-n",         withLastMinimized maximizeWindowAndFocus   ) {-- For Minimize && Action minimize --}
    , ("M-S-a",         killAll                                 ) {-- Quite All --}
    , ("M-S-t",         sinkAll                                 ) {-- Push ALL floating windows to tile.--}
    , ("M-S-s",         sendMessage $ SwapWindow                ) {-- Compine Two Layout [XM-comboP]--}
    , ("M-S-r",         rotSlavesDown                           ) {-- Don't Touch Layout in Master --}
    , ("M-p",           gotoMenu                                ) {-- Find Window  in dmenu --}
    , ("M-b",           bringMenu                               ) {-- swap window To Current WS --}
   
   -- Resize layout
    , ("M-a",           sendMessage MirrorExpand) {-- For Layout ResizableTile( Tiled ) -}
    , ("M-z",           sendMessage MirrorShrink) {-- For Layout ResizableTile( Tiled ) -}

    -- Increase/decrease spacing (gaps)
    , ("M-C-j",         decWindowSpacing 4     )  -- Decrease window spacing
    , ("M-C-k",         incWindowSpacing 4     )  -- Increase window spacing
    , ("M-C-h",         decScreenSpacing 4     )  -- Decrease screen spacing
    , ("M-C-l",         incScreenSpacing 4     )  -- Increase screen spacing

    -- Scratch Pads 
    , ("M-s t",         namedScratchpadAction myScratchPads "terminal") -- Terminal
    , ("M-s s",         namedScratchpadAction myScratchPads "cmus"    ) -- Cmus [Music Player]
    , ("M-s w",         namedScratchpadAction myScratchPads "browser" ) -- Chromium      

    ]
    where 
        toggleFloat w = windows (\s -> if M.member w (W.floating s)
                    then W.sink w s
                    else (W.float w (W.RationalRect (1/6) (1/6) (2/3) (2/3)) s))

------------------------------------------------------------------------
-- Main Do
------------------------------------------------------------------------
main = do
    xmproc <- spawnPipe "xmobar ~/.xmonad/xmobar/xmobar.hs"
    xmonad $ docks def {  modMask                   = myModMask
                        , terminal                  = myTerminal
                        , borderWidth               = myBorderWidth
                        , focusedBorderColor        = myFocusedColor
                        , normalBorderColor         = myNormalColor
                        , workspaces                = myWorkspaces
                        , startupHook               = myStartupHook
                        , layoutHook                = myLayoutHook
                        , manageHook                = myManageHook
                        , logHook                   = dynamicLogWithPP xmobarPP
                                                        -- XMOBAR SETTINGS
                                                       { ppOutput = hPutStrLn xmproc   -- xmobar
                                                       -- Current workspace
                                                       , ppCurrent = xmobarColor blue_ "" . wrap
                                                                 ("<box type=Bottom width=2 mb=2 color=" ++ cyan ++ ">") "</box>"
                                                       -- Visible but not current workspace
                                                       , ppVisible = xmobarColor green ""
                                                       -- Hidden workspace
                                                       , ppHidden = xmobarColor blue_ "" . wrap
                                                                  ("<box type=Top width=2 mt=2 color=" ++ cyan ++ ">") "</box>"
                                                       -- Hidden workspaces (no windows)
                                                       , ppHiddenNoWindows = xmobarColor blue_ ""
                                                       -- Title of active window
                                                       , ppTitle = xmobarColor blue_ "" . shorten 40
                                                       -- Separator character
                                                       , ppSep =  "<fc=" ++ cyan_ ++ "> <fn=1>|</fn> </fc>"
                                                       -- WS Separator 
                                                       , ppWsSep = "  "
                                                       -- Urgent workspace
                                                       , ppUrgent = xmobarColor fg "" . wrap "!" "!"
                                                       -- Adding # of windows on current workspace to the bar
                                                       , ppExtras  = [windowCount]
                                                       --  Type Of layout in xmobar
                                                       , ppLayout   = xmobarColor blue_ ""
                                                       -- order of things in xmobar
                                                       , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]
                                                       } 
                                                       >>  updatePointer (0.5, 0.5) (0, 0) -- exact centre of window
                                                       >> fadeInactiveLogHook 0.95         -- Trancperncy Window (max = 1)
                         } `additionalKeysP` myKeys
