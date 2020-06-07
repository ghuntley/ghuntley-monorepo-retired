---
title: ubiquiti 
---

In my household I've defined IoT to encompass the following types of devices:

    Any device with an IP address, not running a macOS, iOS, Android, Windows, or Linux operating system, that
    Does not require direct access to internal storage devices, and
    Does not store personal data

Examples of IoT devices in my home include:

    Philips Hue Lightbulbs and associated Bridge
    Kindles
    FitBit Aria Scales
    Google Home
    Google Chromecast (mine doesn't access any internal resources)
    IP Video Cameras
    Any other types of WiFi connected sensors (Smoke detectors, Temperature, Humidity, etc.)
    Anything related to a Smart Home (lights, light switches, power, etc.)


# networks

| Name           | Purpose   | Network Group | Port | Subnet          | VLAN |
|---             |---        |---            |---   |---              |---   |
| Cable WLAN     | WAN       | WAN1          | WAN1 |                 |      |
| 4G WLAN        | WAN       | WAN2          | WAN2 |                 |      |
| Infrastructure | Corp      | LAN           | LAN1 | 192.168.1.1/24  |      |
| Family         | Corp      | LAN           | LAN1 | 10.0.10.1/24    | 10   |
| Guest          | Corp      | LAN           | LAN1 | 10.0.20.1/24    | 20   |
| Homelab        | Corp      | LAN           | LAN1 | 10.0.30.1/24    | 30   |
| Work           | Corp      | LAN           | LAN1 | 10.0.40.1/24    | 40   |
| DMZ            | Corp      | LAN           | LAN1 | 10.0.50.1/24    | 50   |

# wireless networks

| Name           | Security   | Advertised     | Guest Network  | VLAN |
|---             |---         |---             |---             |---   |
| Family         | wpapsk     | Yes            | No             | 10   |
| Guest          | wpapsk     | Yes            | Yes            | 20   |

# guest network

```markdown
Terms of Use

By accessing the wireless network, you acknowledge that you're of legal age, you have read and understood and agree to be bound by this agreement.

The wireless network service is provided by the denizens of Huntley Cottage and is completely at their discretion. Your access to the network may be blocked, suspended, or terminated at any time for any reason. You agree not to use the wireless network for any purpose that is unlawful and take full responsibility for your acts.

The wireless network, like most open-source projects, is provided "as is" without warranties of any kind, either expressed or implied.

Cheese Clause: By signing in you are agreeing that in exchange for the use of our network, you owe Erin a wheel of Castello White. If you fail to pay the very reasonable price of cheese you will instead forfeit the soul of your second born.
```

# firewall
- In firewall rules are processed for packets entering a given interface.
- Out firewall rules are processed for packets exiting a given interface.
- Local firewall rules are processed for packets destined for the router itself from a given interface.

![](https://community.ubnt.com/t5/image/serverpage/image-id/65938iBBDF7A4506C545AA/image-size/original)


# vlan
- https://community.ubnt.com/t5/UniFi-Routing-Switching/A-non-expert-Guide-to-VLAN-and-Trunks-in-Unifi-Switches/td-p/1804481
- https://community.ubnt.com/t5/EdgeRouter/Home-Setup-with-4-VLANs-and-Zone-Based-Firewall-ZBF/td-p/2285642

# chromecast and google home
- https://community.ubnt.com/t5/UniFi-Wireless/Chromecast-on-Guest-Network/td-p/1520187
- https://help.ubnt.com/hc/en-us/articles/360001004034-UniFi-Best-Practices-for-Managing-Chromecast-Google-Home-on-UniFi-Network

# stale dns

```
show dns forwarding statistics
sudo vi /etc/hosts
clear dns forwarding cache
sudo /etc/init.d/dnsmasq force-reload
```
