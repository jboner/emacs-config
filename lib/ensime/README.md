# ENSIME
the ENhanced Scala Interaction Mode for Emacs


## Features

- Highlight errors and warnings in your code buffers.
- Inspect the type of any expression.
- Browse packages
- Completion-on-demand for variables, methods, constructores, etc.
- Jump to symbol definitions.
- Automated Refactorings (rename, organize imports, extract method...)
- Scala REPL
- Scala Debugger
- sbt support


Check out this [video](http://www.youtube.com/watch?v=A2Lai8IjLoY) or this [one](http://www.youtube.com/watch?v=v7-G6vD42z8) showcasing debugger support


## System Requirements

- Emacs 22 or later.
- Unix-like OS or Windows. Note that you'll need to use bin/server.bat on windows.
- Java Runtime
- Scala 2.8 compatible source and libraries. ENSIME is built against the 2.8 nightly Scala releases. 


## Documentation
- [The ENSIME User Manual](http://aemon.com/file_dump/ensime_manual.html)


## Quick Start

__1) Install scala-mode__

ENSIME is designed to compliment scala-mode (or any other scala language mode). scala-mode can be found in the Scala distribution under ./misc/scala-tool-support/emacs/

__2) Install ensime-mode__

Download the ENSIME distribution from the github [downloads page](http://github.com/aemoncannon/ensime/downloads). Unpack the ENSIME distribution into a directory of your choosing. 

Add the following lines to your .emacs file:
    (require 'scala-mode)
    (add-to-list 'auto-mode-alist '("\\.scala$" . scala-mode))
    (add-to-list 'load-path "ENSIME_ROOT/elisp/")
    (require 'ensime)
    (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
    ;; MINI HOWTO: open .scala file. Ensure bin/server.sh is executable. M-x ensime


__3) Verify Permissions__

Verify that the startup script (usually bin/server.sh) has executable permissions.


__4) Create Project__

In Emacs, execute M-x ensime-config-gen. Follow directions in the mini-buffer to create a .ensime file for your project.. 


__5) Start ENSIME__

Execute M-x ensime