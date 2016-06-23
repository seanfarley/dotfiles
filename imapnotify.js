var child_process = require('child_process');

function getStdout(cmd) {
  var stdout = child_process.execSync(cmd);
  return stdout.toString().trim();
}

exports.host = "mail.farley.io";
exports.port = 993;
exports.tls = true;
exports.tlsOptions = { "rejectUnauthorized": false };
exports.username = "sean@farley.io";
exports.password = getStdout("/usr/bin/security find-internet-password -w -a sean@farley.io -s mail.farley.io");
exports.onNewMail = "emacsclient -f ~/.emacs.d/server/server -qne \"(alert \\\"あなたのメールをチェック\\\" :title \\\"New Email\\\")\"";
exports.onNewMailPost = "";
exports.boxes = [ "INBOX" ];

exports.onSIGHUP = "ps aux | grep -v grep | grep imapnotify | awk '{print $2}' | xargs kill -9";
