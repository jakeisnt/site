+++
title = "Dotfiles"
draft = false
+++

I've spent a lot of time configuring my Arch Linux System.
My initial installation is catalogued in Arch Linux Configuration; this is a
description of the work I've done to make the installation mine.


## Color Scheme {#color-scheme}

I've based my entire color scheme on Doom Emacs' 'Doom-one' theme, borrowing
from the colors used for both the background and the syntax highlighting and
reverse-engineered using both the original configuration file and the
\`\`\`xcolor\`\`\` program as a last resort.

I've spent a considerable amount of time configuring the CSS of my Firefox
browser as well; taking lots of inspiration from /r/firefoxcss users, I've
removed much of the unnecessary syntax and moved the tab bar below the address
bar so that searching and typing in URLs is the focus of the experience in the
browser.


## Emacs {#emacs}

I'm relatively new to Emacs - I've used the platform for about two months, and
I'm still picking up on the keyboard shortcuts and utilities for a variety of
tools. Most of the code I've written in recent memory has been OCaml or Haskell,
and Emacs arguably provides the best set of tools to develop for both of these
languages, with well-supported major modes and a variety of integrations. I take
all of my course notes in Org mode - its ability to insert inline LaTeX and code
blocks is incredible - and writing this website in Org mode is a snap.

I'm working with the Doom Emacs distribution, and my configuration is centered
around changing my Org-mode tags. My central repository for these files is
Dropbox, and they sync seamlessly between my phone and my computer with my
private Dropbox repository - editing such files on my phone with Orgzly, a
well-supported application, even available on F-Droid for those not a fan of
Google!


## Keybindings {#keybindings}

The choices I've made that have impacted my operating system workflow the most
has been the keyboard shortcuts I've chosen. I've assigned these bindings in
both the i3 configuration file and through X with the xbindkeys program. Perhaps
the most important binding I've made is Mod4 (assigned to my Windows key on my
XPS) + D to Rofi's DMenu-like feature; I can open any program without having to
load up a terminal by interfacing with this program that analyzes all of the
programs in my /usr/bin.

I have my bindings in my configuration file for i3 assigned to various aspects
of my 'system'; that is, utilities that allow me to better navigate or use my
computer such as rofi. The bindings I have in xbindkeys are primarily geared
towards scripts I have running, such as for increasing and decreasing
brightness.


## Vim + Tmux: Developing without X {#vim-plus-tmux-developing-without-x}

Running a graphical server to interact with your computer is too much overhead
for most tasks. If only text editing, file configuration or scripting has to be
done, then why launch an entire session with a desktop environment and/or window
manager just to accomplish these tasks? After all, we already have access to a
terminal environment when we boot; it seems a bit unnecessary to start X
immediately upon login. Utilizing this terminal substantially decreases
operating system startup time - you're already at the terminal!

Though I haven't made extensive use of the setup yet, I've been working on a
basic configuration using just two programs: Vim and Tmux. Vim is a
terminal-based text editor - it's been around forever and has lots of keyboard
shortcuts as well as substantial third-party plugin systems with a variety of
useful utilites. Tmux is a terminal multiplexer - it functions exactly like a
window manager, opening multiple terminals with different preset configurations,
but does so without having to interface with any user sessions, residing solely
in the terminal. Using these two programs, we can craft the perfect environment
for writing programs, text-editing, browsing the file system (ranger is my
preferred interface) and even browsing the internet (with a TUI web browser such
as w3m). As I'm currently writing this in Emacs (which doesn't have the best
terminal user interface), I definitely have not completely accustomed to my
setup -- but such a toolchain is certainly something I can see many people usin
to enhance productivity even on devices with minimal graphical capabilities, and
can be set up on a remote server as well to avoid any need for saving files on a
local system.
