set from="ghuntley@ghuntley.com"
set status_format = "[$from (%f) {M:%m%?n?|N:%n?%?o?|O:%o?%?d?|D:%d?%?t?|T:%t?%?p?|P:%p?}"
set signature = ~/.mutt/sig_ghuntley.sig

# Set the SMTP server
#source ~/.auth/mutt-ghuntley.auth

set smtp_url="smtp://ghuntley@ghuntley.com@smtp.gmail.com:587/"
set smtp_pass=`cat /run/secrets/personal-imap-password`


# vim: set ft=muttrc
