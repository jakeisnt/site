+++
title = "Building this Website"
date = 2019-12-30T11:12:00-05:00
tags = ["web"]
categories = ["updates"]
draft = false
+++

This website is written with org-mode, powered by Hugo, and hosted with Netlify.

Why?

I had several needs for making this website.

1.  Easily updatable. My last website was my first serious attempt at web
    development without using a framework - making use only of HTML, CSS and Javascript - and hosted on an archaic static site deployment platform. As impressive as the Three.js visualizations on the site were, I had to dive into the code, then drag-and-drop upload the individual files to a host. The difficulty of updating the website kept me from updating it at all, so I wanted a solution with minimal friction.
2.  Customizable. Drag-and-drop templates are restrictive, only allowing for the use of pre-made components. I want to be able to add neat animations and custom styles to have the freedom to display my website as I want it. Further, a structured templating system is ideal, as it'll save a lot of the time spent on boilerplate code.
3.  Maintained. When I'm using a static site generator such as Hugo, the framework does a lot of the heavy lifting for me; I don't have to worry about the integrity of a large codebase for my personal site and can save that concern for other projects I'm invested in.

I was looking for the easiest way for me to track and update my website. I've been getting a lot of use out or Org Mode in Emacs recently, and its ability to function across platforms (as a plain-text document format) as well as its powerful language-specific syntax highlighting and evaluation (with LaTeX support!) made it an ideal candidate for composing a website.

After finding the ox-hugo package, this became a no-brainer; I can compose my entire site in a single org mode file, then export it to a system of markdown files that I can slot into my Hugo blog.

After adding the markdown file tree to my blog, I can just commit to the repository and push to Github. I've found that Netlify offers the best free tier of any static web host, and the continuous integration with a Github repository - as well as the ability to build the Hugo website on the fly - is powerful!

Currently, I'm using a theme made for Hugo by another user, forked and added as a git submodule to the we site's repository. I'm working on adapting this to my own theme to better fit my use case - though I'm content with the minimal look of the website, I would like to better convey my projects and skills going forward.
