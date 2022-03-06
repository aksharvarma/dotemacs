HAVEN NOT UPDATED THIS IN 2 YEARS.
These are my Emacs init files. I'm putting it all up under version control so that I don't have to declare Dot Emacs Bankruptcy in the future just like I did a while ago. And maybe this will help someone else as well.

# Overview
## Control redirection
This repo sits inside `~/.emacs.d/site-lisp/ajv` on my machine. Control jumps there from `~/.emacs.d/init.el` which only has the line `(load "~/.emacs.d/site-lisp/ajv/ajv-init.el")` in it.

This is done to keep things separated out. I can plug in my repo in one place and add the single required line by myself without worrying about cloning files into different places, or about files running around all over the place. The folder structure is also a legacy thing from before MELPA days when individual scripts were plucked and put inside `site-lisp`.

# Main file: `ajv-init.el`
This is the main file that does all the heavylifting.

First it calls `ajv-pre-setup.el` to do some necessary setup, primarily get the package manager going and install any missing packages.
1. Before any of that though, it changes the garbage collection limits to improve loading times (and resets it after startup).
2. Then it disables some visual elements early on to speed up loading.
3. Then the main job: it loads up the package manager and initializes it.
4. Finally, it installs any packages that it finds are missing.

The control then comes back to `ajv-init.el` which loads all other packages:
1. We first load use-package so that other packages can be loaded nicely (and optionally lazyily).
2. Then start loading packages one by one. I first load external packages before customizations/configurations that I have written that aren't part of a package.

_Note 1: I use use-package to load up each package that I use. This allows me greater flexibility in controlling the loading as well as improves the readability of my customizations drastically._

_Note 2: Every variable/function that I have defined is prefixed with `ajv/` to differentiate from things that emacs has by default or things that are defined by other packages. Further, variables/functions that are specific to a particular feature or a mode are prefixed appropriately, for example: `ajv/dired/` or `ajv/magit/`. This makes it much easier for me to maintain the code as well as to call functions/lookup variables more easily with ido's fuzzy matching._

# Other files
Part of the modularization is done via having my functions/settings distributed across various files categorized roughly by what purpose they serve.

## My settings: `ajv-settings.el` and `ajv-sensitive-settings.el`
Any variable that might be used in multiple places or is a preference that I have or is simply data, then is stored in these files. If it is something sensitive then it goes into the latter file which is gitignored. Otherwise, it goes into the first file. Everything that is "sensitive" is prefixed `ajv/sensitive/`.

## Customizations: `ajv-customizations.el`
Everything that custom-set-variables does is put into this file. This is (mostly) empty because I have picked customizations from there and moved them into other places. The aim is to always keep this empty and add customizations elsewhere after understanding how they work. A small step towards preventing Emacs bankruptcy down the road when I cannot understand what something does and if I even need it anymore.

## My functions: `ajv-my-functions.el`
Any general purpose function that I use get defined in here. If I can categorize functions for specific purposes, then I do that and use a different file for those functions. For example, I have a function that is only useful for pdf-tools, which goes into the file `ajv-pdf.el`

## Dired: `ajv-dired.el`
I make some changes to the basic dired in Emacs, particularly for sorting certain directories by time/extension. I also make dired hide details and omit hidden files by default. The functions to do these are defined in this file, which is loaded using use-package where keybindings and other settings are done.

## Magit: `ajv-magit.el`
The only thing present in this file is a functioon used to kill off all magit buffers. Again, the keybinding is defined in the use-package call from inside the main init file. Note that Magit uses ido-completing-read+ to allow it to use ido like features.

## PDF: `ajv-pdf.el`
Settings for moving the modeline to the top when I use pdf-tools for viewing PDFs inside Emacs.

## Misc: `ajv-misc.el`
This is the place where most of the customizations from the `custom-set-variables` goes. Small, miscellaneous things that aren't part of a bigger feature are all put in this file. For example, I enable `winner-mode`, `subword-mode`, etc. in here.

## Visual: `ajv-visual.el`
Any settings that affects how Emacs looks, goes in here; except for settings specifically for the modeline which get their own file.

## Modeline: `ajv-modeline.el`
This is where I setup the modeline to show me the time and the battery status.

## Modal editing via God-mode: `ajv-god.el`

## Ibuffer: `ajv-ibuffer.el`

## Org related: `ajv-org.el`

## : `ajv-god.el`

