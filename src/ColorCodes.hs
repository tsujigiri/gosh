module ColorCodes where

fgBlack :: String
fgBlack = esc "30"

fgLightBlack :: String
fgLightBlack = esc "90" 

fgRed :: String
fgRed = esc "31" 

fgLightRed :: String
fgLightRed = esc "91" 

fgGreen :: String
fgGreen = esc "32" 

fgLightGreen :: String
fgLightGreen = esc "92" 

fgYellow :: String
fgYellow = esc "33" 

fgLightYellow :: String
fgLightYellow = esc "93" 

fgBlue :: String
fgBlue = esc "34" 

fgLightBlue :: String
fgLightBlue = esc "94" 

fgPurple :: String
fgPurple = esc "35" 

fgLightPurple :: String
fgLightPurple = esc "95" 

fgCyan :: String
fgCyan = esc "36" 

fgLightCyan :: String
fgLightCyan = esc "96" 

fgWhite :: String
fgWhite = esc "37" 

fgLightWhite :: String
fgLightWhite = esc "97" 

bgBlack :: String
bgBlack = esc "40" 

bgLightBlack :: String
bgLightBlack = esc "100" 

bgRed :: String
bgRed = esc "41" 

bgLightRed :: String
bgLightRed = esc "101" 

bgGreen :: String
bgGreen = esc "42" 

bgLightGreen :: String
bgLightGreen = esc "102" 

bgYellow :: String
bgYellow = esc "43" 

bgLightYellow :: String
bgLightYellow = esc "103" 

bgBlue :: String
bgBlue = esc "44" 

bgLightBlue :: String
bgLightBlue = esc "104" 

bgPurple :: String
bgPurple = esc "45" 

bgLightPurple :: String
bgLightPurple = esc "105" 

bgCyan :: String
bgCyan = esc "46" 

bgLightCyan :: String
bgLightCyan = esc "106" 

bgWhite :: String
bgWhite = esc "47" 

bgLightWhite :: String
bgLightWhite = esc "107" 

bold :: String
bold = esc "1" 

underline :: String
underline = esc "4" 

reset :: String
reset = esc "0"

esc :: String -> String
esc str = "\ESC[" ++ str ++ "m"

