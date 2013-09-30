set activeApp to (path to frontmost application as Unicode text)
set pathToFinder to path to application "Finder" as text

--Get Screen Size
tell application "Finder"
  set screenSize to bounds of window of desktop
  set theWidth to item 3 of screenSize
  set theHeight to item 4 of screenSize
  set halfWidth to (theWidth / 2)
  set halfHeight to (theHeight / 2)
end tell

if activeApp = pathToFinder then
  set windowParameters to {0, 0, theWidth, (halfHeight - 33)}
else
  set windowParameters to {0, 0, theWidth, (halfHeight - 11)}
end if

set windowNumber to 1
if "Emacs" is in activeApp then set windowNumber to 2

--Resize Window
tell application activeApp
	activate
	set bounds of window windowNumber to windowParameters
end tell
