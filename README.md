These are my Emacs init files. I'm putting it all up under version control so that I don't have to declare Dot Emacs Bankruptcy in the future just like I did a while ago. And maybe this will help someone else as well.

# Overview
## Control redirection
This repo sits inside `~/.emacs.d/site-lisp/ajv` on my machine. Control jumps there from `~/.emacs.d/init.el` which only has the line `(load "~/.emacs.d/site-lisp/ajv/ajv-init.el")` in it.

This is done to keep things separated out. I can plug in my repo in one place and add the single required line by myself without worrying about cloning files into different places, or about files running around all over the place. The folder structure is also a legacy thing from before MELPA days when individual scripts were plucked and put inside `site-lisp`.

# Main file: `ajv-init.el`
This is the main file that does all the heavylifting.

* It first changes the garbage collection limits to improve loading times (and resets it after startup).
* Then it loads up the package manager and initializes it.
* Then require use-package so that I can load packages nicely (and optionally lazyily).
* Then start loading packages one by one. I first load packages before customizations/configurations that I have written.

_Note 1: I use use-package to load up each package that I use. This means that I can disable a use-package call and feature x and only feature x will be disabled. At least that's how it works in theory, I'm still not at such a stage with respect to modularizing my init files. Another benefit of use-package is that it allows me to get a quick grasp of what settings I have by simply looking at the use-package call for the feature instead of having to read the whole file in which I setup the functions for the feature. I only need to go into individual files for more details._

_Note 2: Every variable/function that I have defined is prefixed with `ajv/` to differentiate from things that emacs has by default or things that are defined by other packages._

# Other files
Part of the modularization is done via having my functions/settings distributed across various files categorized roughly by what purpose they serve.

## My settings: `ajv-settings.el` and `ajv-sensitive-settings.el`
Any variable that might be used in multiple places or is a preference that I have or is simply data, then is stored in these files. If it is something sensitive then it goes into the latter file which is gitignored. Otherwise, it goes into the first file.

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
