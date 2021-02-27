// seems ace is a poor emulation of emacs
// settings.aceKeybindings = "emacs";

// having different UIs depending on number of tabs is jarring
settings.tabsThreshold = 0;
settings.hintAlign = "left";
settings.stealFocusOnLoad = false;
settings.defaultSearchEngine = "d";

// this function is for a simple mapping map(new, old) -> copied to insert mode;
// it won't work for remapping ':' to 'alt-x'
function mapboth(newbind, oldbind, help, func) {
  // normal mapping
  map(newbind, oldbind);

  // look up the normal mode keybinding
  if (typeof func === 'undefined') {
    var tmp_meta = Normal.mappings.find(oldbind);
    if (tmp_meta) {
      help = tmp_meta.meta.annotation;
      func = tmp_meta.meta.code;
    }
  }

  if (help && func) {
    imapkey(newbind, help, func);
  } else {
    // if the normal mode keybinding didn't exist, fallback to using a simple
    // imap()
    imap(newbind, oldbind);
  }

  unmap(oldbind);
  iunmap(oldbind);
}

// early unmappings
unmap('<Ctrl-,>');
unmap('<Ctrl-c>');
unmap('<Ctrl-D>');
unmap('<Ctrl-r>');
unmap('<Ctrl-m>');

mapboth('<Ctrl-,>', "<Ctrl-'>");

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
unmap('<Ctrl-.>');

mapboth('<Ctrl-.>', '.');

mapboth('<Ctrl-u><Ctrl-.>', ';ql');

// tabs (well, windows since we're not using tabs)
unmap('g0');
unmap('g$');
unmap('gt');
unmap('gT');
unmap('on');
unmap('W');
unmap('yt');
unmap('yT');
unmap('<Alt-p>');
unmap('<Alt-m>');
unmap('>>');
unmap('<<');

mapboth('<Ctrl-x>b', 'T');

// what ze shit is preview markdown?
unmap(';pm');

// omnibar
mapboth('<Ctrl-c>fF', 't');
mapboth('<Ctrl-c>ff', 'go');
mapboth('<Ctrl-c>fi', 'oi');
mapboth('<Ctrl-c>fS', 'ab'); // save bookmark
mapboth('<Ctrl-c>fb', 'b');
mapboth('<Ctrl-c>fx', 'ox');
mapboth('<Ctrl-c>fh', 'oh');

unmap('om');
unmap('ob');
unmap('og');
unmap('od');
unmap('ow');
unmap('oy');
unmap('H');
unmap('Q');

// adjusts the position, which is useless in my workflow
unmap('<Ctrl-j>')

// searching
mapboth('<Ctrl-s>', '/');

// 'a' for assholes
mapboth('<Ctrl-c>sa', 'sg');
mapboth('<Ctrl-c>ss', 'sd');
mapboth('<Ctrl-c>sw', 'se');
mapboth('<Ctrl-c>so', 'ss');
mapboth('<Ctrl-c>sg', 'sh');
mapboth('<Ctrl-c>sy', 'sy');
mapboth('<Ctrl-c>st', ';t');

unmap('sb');
unmap('sw');

// help
unmap('<Ctrl-h>');
mapboth('<Ctrl-h><Ctrl-h>', '?');

mapboth('<Ctrl-h>,', ';e');

// NOTE can't remap alt-s since it's hardcoded as a special key
// map('<Ctrl-h>s', '<Alt-s>');
// unmap('<Alt-s>');

mapboth('<Ctrl-h>t', '<Alt-i>');
mapboth('<Ctrl-h>p', 'p');

// scrolling
// already taken care of by karabiner or default mac keybindings
// map('<Alt-v>', 'e');
// map('<Ctrl-v>', 'd');
// map('<Ctrl-f>', 'l');
// map('<Ctrl-n>', 'j');
// map('<Ctrl-p>', 'k');
// map('<Ctrl-b>', 'h');

unmap('u');
unmap('e');
unmap('d');
unmap('j');
unmap('k');
unmap('h');
unmap('l');
unmap('gg');
unmap('G');
unmap(';w');
unmap('0');
unmap('cs');
unmap('cS');
unmap('$');
unmap('%');
unmap('w');

// page navigation
mapboth('<Ctrl-c>f^', 'gu');
mapboth('<Ctrl-c>f?', 'g?');
mapboth('<Ctrl-c>f#', 'g#');
mapboth('<Ctrl-c>fa', 'gU');

// jumping / goto
unmap('gf');
unmap('cf');
unmap('C');

mapboth('<Ctrl-c>sl', 'f');
// since we're using that anti-tab extension, opening links in a non-active tab
// isn't possible
mapboth('<Ctrl-c>sL', 'af');

// cool but fairly niche (clicks on 'next' or 'prev')
unmap('[[');
unmap(']]');

// download
mapboth('<Ctrl-c>di', ';di');

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

// yanking
mapboth('<Ctrl-c>yml', 'yma'); // copy multiple links
mapboth('<Ctrl-c>ymc', 'ymc'); // copy multiple columns
mapboth('<Ctrl-c>yme', 'ymv'); // copy multiple elements

mapboth('<Ctrl-c>yl', 'ya'); // copy link
mapboth('<Ctrl-c>yc', 'yc'); // copy column
mapboth('<Ctrl-c>yq', 'yq'); // copy quoted block
mapboth('<Ctrl-c>ye', 'yv'); // copy element
mapboth('<Ctrl-c>yi', 'yi'); // copy input
mapboth('<Ctrl-c>ys', 'ys'); // copy page source
mapboth('<Ctrl-c>yd', 'yd'); // copy current download
mapboth('<Ctrl-c>fy', 'yy'); // just like doom
mapboth('<Ctrl-c>yh', 'yh'); // copy page's host
mapboth('<Ctrl-c>yt', 'yl'); // copy page title
mapboth('<Ctrl-c>yf', 'yf'); // copy form as json
mapboth('<Ctrl-c>yp', 'yp'); // copy form post

unmap('yG'); // is broken for me
unmap('yg'); // is broken for me
unmap('yS'); // is broken for me
unmap('cc'); // is broken for me
unmap('yj'); // nice but to niche
unmap('yQ'); // nice but to niche
unmap('cq'); // nice but to niche
unmap(';pj'); // nice but to niche
// pastes form data from 'yf'
unmap(';pf'); // nice but to niche
unmap(';pp'); // nice but to niche

// marks
// can't seem to wrap my head around how to manage these so disabling
unmap('m');
unmap("'");
unmap('<Ctrl-,>');
unmap('<Ctrl-m>');

// chrome urls
mapboth('<Ctrl-c>fs', 'gs'); // open page source
unmap(';j'); // doesn't seem to work in Firefox

// visual mode
// NOTE Since we're trying to map this to emacs, "visual mode"
// doesn't quite fit; instead, we'll just map the visual+selection commands e.g.
// ctrl+space in emacs
mapboth('<Ctrl-Space>', 'zv');

vmap('<Ctrl-l>', 'zz');
vunmap('zz');

vmap('<Ctrl-c>st', 't');
vunmap('t');

// NOTE can't figure out how to make navigation work :-(
// vmap('<Alt-f>', 'w');

unmap('v');
vunmap('('); // broken for me
vunmap(')'); // broken for me
vunmap('{'); // broken for me
vunmap('}'); // broken for me
vunmap('gr'); // broken for me
vunmap('q'); // broken? only opens github for me

vunmap('gg'); // handled by macos
vunmap('G'); // handled by macos

// command M-x
mapkey('<Alt-x>', 'Open omnibar', function() {
  Front.openOmnibar({type: "Commands"});}
);
unmap(':');

// org mode
function replace_all(str, find, replace) {
  return str.replace(new RegExp(find, 'g'), replace);
}

function escapeIt(text) {
  return replace_all(replace_all(replace_all(encodeURIComponent(text), "[(]", escape("(")),
                                 "[)]", escape(")")),
                     "[']" ,escape("'"));
}

function org_capture(protocol='capture', template='t') {
  var selection_text = escapeIt(window.getSelection().toString());
  var encoded_url = encodeURIComponent(location.href);
  var escaped_title = escapeIt(document.title);
  var template_url = "";
  var url_key = "url";

  if (protocol !== 'store-link') {
    template_url = "template=" + template + "&";
  }

  if (protocol === 'roam-ref') {
    url_key = "ref";
  }

  var url = "org-protocol:///" + protocol + "?"
        + template_url
        + url_key + '=' + encoded_url
        + '&title=' + escaped_title;

  if (selection_text) {
      url += '&body=' + selection_text;
  }

  console.log('LEEEEEEEEROY: ' + url);
  location.href = url;
}

// mapping to doom-style keybindings
mapkey('<Ctrl-c>nn', 'Org capture', org_capture);
mapkey('<Ctrl-c>nrc', 'Org-roam capture', function () {
  org_capture('roam-ref', 'r');
});
mapkey('<Ctrl-c>nl', 'Org store link', function () {
  // NOTE I'm not sure I like the behavior of store-link when using a browser;
  // copy+pasting the link seems to better fit
  // org_capture('store-link');

  var text = "[[" + location.href + "][" + document.title + "]]";
  navigator.clipboard.writeText(text).then(function() {
  }, function(err) {
    console.error('Async: Could not copy text: ', err);
  });
});
