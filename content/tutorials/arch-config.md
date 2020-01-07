+++
title = "Arch Linux Configuration"
draft = true
+++

I currently do all of my work on a Dell XPS 9370 (4k, 1 TB SSD) with an Arch Linux
installation, dual-booted alongside Windows 10.
I've documented my installation process here for myself and others
configuring similar systems.

Before starting, please refer to your system's page on the Arch Wiki:
<https://wiki.archlinux.org/index.php/Dell%5FXPS%5F13%5F(9370)>, as it will provide
information about your specific system and configuration. This guide will be
somewhat system specific, as its main motivation is to tackle the hurdles I
faced with my specific installation, but it should be fairly extensible to other systems.


## Setup {#setup}

The setup is more or less similar to that described in the Arch Linux guide.
Snag an installation from a reputable source
<https://www.archlinux.org/download/>, then verify the signature of the
installation to ensure it's legitimate and unmodified.

Write the Arch Linux .iso file you've obtained to a thumb drive using another device
you have on hand. I've used the Rufus utility on Windows 10 in the past, but any
similar flash drive formatting utility will do.


## Installation {#installation}


### Making Space {#making-space}


### WiFi {#wifi}


#### Tackling EduRoam {#tackling-eduroam}


## System Setup {#system-setup}

Congratulations - your system's successfully booted!

Now, you're going to install some useful utilities and establish a framework
upon which you can build your system the way you'd like.


### Campus Wifi Connection {#campus-wifi-connection}


## Extras {#extras}

We can benefit from some additional performance optimizations for our system.


### Power Conservation {#power-conservation}

To conserve power, we can enable some kernel parameters as follows:

```sh
$ touch /etc/modprobe.d/i915.conf
$ echo "options i915 modeset=1 enable_rc6=1 enable_fbc=1 enable_guc_loading=1 enable_guc_submission=1 enable_psr=1" >> /etc/modprobe.d/i915.conf
```

We can also undervolt our system using a utility such as TLP.
Undervolting thresholds are different for every system, and depriving power from
parts of your system can result in permanent hardware damage. This is not good
and should probably be avoided. With that said, undervolting my system seemed to
net me about 30 minutes of battery life (I was able to undervolt by about
-100mA, but this may not be the same for your CPU). For more on undervolting,
check out this guide. Though it's for Windows systems, the same guidelines apply
regardless of system, even though the parameters may be different.
<https://www.ultrabookreview.com/10167-laptop-undervolting-overcloking/>

I'll likely compose an undervolting tutorial in the future.


### Enhancing Speed {#enhancing-speed}

The CPU is substantially throttled by Dell and Intel to prevent the system
from getting too hot and kicking in the fans. This is desired to make the system
sound quieter, but it significantly reduces the actual power of our machine.

The throttled<sup>aur</sup> package:
<https://www.archlinux.org/packages/community/any/throttled/> fixes this Intel
throttling constraint handily on my machine. Register this with systemctl and
you're good to go.

We can also tweak our thermal mode profiles to tweak the default behavior of our
system. This can be done with terminal commands:

See all of the thermal modes available:

```sh
$ symbios-thermal-ctl -i
```

See the current thermal mode set:

```sh
$ symbios-thermal-ctl -g
```

Set the desired thermal mode:

```sh
$ smbios-thermal-ctl --set-thermal-mode=THERMAL_MODE
```


### Sleep Mode Efficiency {#sleep-mode-efficiency}

The default sleep mode for Linux systems on the XPS isn't the best for power
saving.


## Personal Preferences {#personal-preferences}

<https://wiki.archlinux.org/index.php/Bluetooth%5Fkeyboard> -- login w bluetooth keyboard
<https://wiki.archlinux.org/index.php/Bluetooth> -- set up bluetooth headset
// some installations require a new key - ensure you trust signature, then add
the public key that could not be found to your roster of keys
