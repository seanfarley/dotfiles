// seems ace is a poor emulation of emacs
// settings.aceKeybindings = "emacs";

// having different UIs depending on number of tabs is jarring
settings.tabsThreshold = 0;
settings.hintAlign = "left";
settings.stealFocusOnLoad = false;
settings.defaultSearchEngine = "d";

imap('<Ctrl-,>', "<Ctrl-'>");
iunmap("<Ctrl-'>");

// early unmappings
unmap('<Ctrl-,>');
unmap('<Ctrl-c>');
unmap('<Ctrl-D>');
unmap('<Ctrl-r>');
unmap('<Ctrl-m>');

// remove proxy keybindings
unmap('cp');
unmap(';cp');
unmap(';ap');

unmap(';pa');
unmap(';pb');
unmap(';pd');
unmap(';ps');
unmap(';pc');

// remove the close window key
unmap('x');
unmap('X');
unmap('gx0');
unmap('gxt');
unmap('gxT');
unmap('gx$');
unmap('gxx');

// remove left / right navigate tab since macos has this
unmap('E');
unmap('R');

// read functions are also unused
unmap('gr');

// zoom is handled by macos natively
unmap('zr');
unmap('zi');
unmap('zo');

// session management is fairly unused
unmap('ZQ');
unmap('ZZ');
unmap('ZR');

// repeat
unmap('<Ctrl-u>');
unmap('<Ctrl-d>');
map('<Ctrl-.>', '.');
unmap('.');
map('<Ctrl-u><Ctrl-.>', ';ql');
unmap(';ql');

// tabs (well, windows since we're not using tabs)
unmap('g0');
unmap('g$');
unmap('gt');
unmap('gT');
unmap('on');
unmap('W');

map('<Ctrl-x>b', 'T');
map('<Ctrl-x><Ctrl-<left>>', '<Ctrl-x>b'); // TODO doesn't work
var tab_func = Normal.mappings.find('T');
imapkey('<Ctrl-x>b', tab_func.meta.annotation, tab_func.meta.code);
unmap('T');

// help
unmap('<Ctrl-h>');
map('<Ctrl-h><Ctrl-h>', '?');
map('<Ctrl-h>,', ';e');
unmap(';e');

// scrolling
unmap('u');
map('<Alt-v>', 'e');
map('<Ctrl-v>', 'd');

// jumping / goto
unmap('af');
unmap('gf');
unmap('cf');
unmap('C');

map('<Alt-g><Alt-g>', 'f');
map('<Alt-g>l', 'f');
// since we're using that anti-tab extension, opening links in a non-active tab
// isn't possible
mapkey('<Alt-g>w', '#1Open a link in active new tab', function() {
    Hints.create("", Hints.dispatchMouseClick, {tabbed: true, active: true});
});

// navigation
unmap(';u');
unmap(';U');
unmap('B');
unmap('F');
unmap('S');
unmap('D');
unmap('r');
unmap('<Ctrl-6>');

// editor
// TODO use emacs anywhere instead
unmap('i');
unmap('I');
unmap('<Ctrl-i>');

// mouse navigation
unmap('gi');
unmap(';fs');
unmap(';m');
unmap('<Ctrl-j>');
unmap('q');
unmap('O');

mapkey('<Alt-x>', 'Open omnibar', function() {
  Front.openOmnibar({type: "Commands"});}
);
unmap(':');

mapkey('<Ctrl-c>c', 'Org capture', function() {
  function replace_all(str, find, replace) {
      return str.replace(new RegExp(find, 'g'), replace);
  }

  function escapeIt(text) {
    return replace_all(replace_all(replace_all(encodeURIComponent(text), "[(]", escape("(")),
                                   "[)]", escape(")")),
                       "[']" ,escape("'"));
  }

  var selection_text = escapeIt(window.getSelection().toString());
  var encoded_url = encodeURIComponent(location.href);
  var escaped_title = escapeIt(document.title);
  var protocol = "capture";
  var template = "t";

  location.href = "org-protocol://" + protocol
        + "?template=" + template
        + '&url=' + encoded_url
        + '&title=' + escaped_title
        + '&body=' + selection_text;
});
