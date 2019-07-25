local clipboard = hs.loadSpoon("ClipboardTool")

local mod = {}

function mod.toggle()
    clipboard:toggleClipboard()
end

function mod.init()
    clipboard.show_in_menubar = false
    clipboard.paste_on_select = true
    clipboard:start()
end

return mod
