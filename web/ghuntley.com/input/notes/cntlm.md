---
layout: notes
title: cntlm
---

If you have ever been in the unfortunate situation whereby you are using a Macbook/OSX device within a Windows centric environment and are forced to use a proxy server then the words above and the image below will potentially send shivers down your spine.

![To view this page, you need to login to the proxy server.](/images/osx-ntlm-proxy-auth.png)

What's the big deal you ask? Surely it's just a matter of entering in your username (```domain\username```) and password. Two minutes later (if your lucky) that damn prompt will come back regardless if you selected "Remember this password in my Keychain" and clicking "Cancel" is about the worst possible thing you can do.....

![To view this page, you need to login to the proxy server.](/images/osx-ntlm-proxy-auth-loop.png) 

Luckily, thanks to the magic powers of open-source software there's a solution!

    # brew install cntlm

Next step is to generate a encrypted `NTLMv2` hash which will be used instead storing your password in plain text.

    # cntlm -H -c /usr/local/etc/cntlm.conf 
    
    Password: 
    PassLM          4CnopeEACnope43F5nope60C
    PassNT          4CnopeEACnope43F5nope60C
    PassNTLMv2      4CnopeEACnope43F5nope60C  

Edit the configuration file at ```/usr/local/etc/cntlm.conf```:

    Domain          $domainname
    Username        $username

    PassNTLMv2      $passNTLMv2
    Auth            NTLMv2

    # upstream proxy server hostname and port (hostname:port)
    Proxy           proxy.example.com:8080 

    # destinations which are not routed to the upstream proxy server.
    NoProxy         localhost, 127.0.0.*, 10.*, 192.168.*

    # which TCP port to bind on
    Listen          3128

    # bind to all network interfaces (yes) or bind to loopback (no)
    Gateway         yes

    # ACL of addresses allowed to connect through the proxy
    Allow           127.0.0.1
    
Replace `$username`, `$passNTLMv2`,`$domain` and configure the `NoProxy` and `Proxy` stanza's as appropriate.

Configure `cntlm` to launch automatically by creating `~/Library/LaunchAgents/cntlm.daemon.plist` with the following contents:

    <?xml version="1.0" encoding="UTF-8"?>
    <!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
    <plist version="1.0">
    <dict>
      <key>Label</key>
      <string>cntlm.daemon</string>
      <key>ProgramArguments</key>
      <array>
          <string>/usr/local/bin/cntlm</string>
      </array>
      <key>KeepAlive</key>
      <false/>
      <key>RunAtLoad</key>
      <true/>
      <key>StandardErrorPath</key>
      <string>/dev/null</string>
      <key>StandardOutPath</key>
      <string>/dev/null</string>
    </dict>
    </plist>

Start the daemon:

    # cntlm

Configure OSX to use the proxy server that is now running at ```localhost:3128```

In 31 days or whenever you windows password chasnges/expires you will need to re-run the `cntlm -H` command, update the configuration file with the new `PassNTLMv2` hash and then restart the daemon.

