# ENSIME
the ENhanced Scala Interaction Mode for Emacs

## Features

- Highlight errors and warnings in your code buffers.
- Inspect the type of any expression.
- Browse packages
- Completion-on-demand for variable, methods, constructores, etc.
- Jump to symbol definitions.
- sbt support
- Check out the [video](http://www.youtube.com/watch?v=A2Lai8IjLoY)

## System Requirements

- Emacs 22 or later.
- Unix-like OS or Windows. Note that you'll need to use bin/server.bat on windows.
- Java Runtime
- Scala 2.8 compatible source and libraries. ENSIME is built against the 2.8 nightly Scala releases. 


## Installation

__scala-mode__

ENSIME is designed to compliment scala-mode. scala-mode can be found in the Scala distribution under ./misc/scala-tool-support/emacs/

__ensime-mode__

Download the latest ENSIME distribution from the github [downloads page](http://github.com/aemoncannon/ensime/downloads). Unpack the ENSIME distribution into a directory of your choosing. 

Add the following lines to your .emacs file:
    (require 'scala-mode)
    (add-to-list 'auto-mode-alist '("\\.scala$" . scala-mode))
    (add-to-list 'load-path "ENSIME_ROOT/elisp/")
    (require 'ensime)
    (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

Create an .ensime configuration file for your project. See [.ensime](http://github.com/aemoncannon/ensime/blob/master/.ensime.example) in the root of the ENSIME distribution for an up-to-date example.

Verify that the startup script (usually bin/server.sh) has executable permissions.

Finally, open one of the source files for your Scala project and type M-x ensime. Follow the minibuffer instructions to specify the location of your .ensime project file. 



__Note for sbt Users__ 

Customize the ensime-sbt-compile-on-save variable if you'd like sbt to recompile your project whenever you save. This is disabled by default as it consumes a lot of CPU without much benefit over ENSIME's built-in type checking.


__Note for Scala Standard Library (or other giant project) hackers__ 

You may want to increase the jvm heap size to give ENSIME some more breathing room. We've had some reports of ENSIME hanging when retrieving type information in huge projects. You can add the necessary flags in bin/server.sh. 




## Usage

__TAB__    - Start completing a method/variable.

__C-c t  /  Double-Click__  - Inspect the type of the expression under the cursor.

__M-.  /  Control-Click__  - Jump to definition of symbol under cursor.

__M-,__  - Pop back to previously visited position.

__Double-Click(on an import statement)__  - Inspect the package under cursor.

__Mouse Hover__    - Echo the type of the expression under the cursor.

__C-c p__  - Inspect the package of the current source file.

__C-c o__  - Inspect the package specified in .ensime as :project-package.

__.__  - Forward one page in the inspector history.

__,__  - Backward one page in the inspector history.

__M-n  /  TAB__  - Forward one link in the inspector.

__M-p__  - Backward one link in the inspector.

__C-c C-a__  - Switch to the sbt command-line (works for sbt projects only)

__C-c c__  - Type-check the current file.

__C-c a__  - Type-check all files in the project.







## Troubleshooting

You may want to examine the contents of the \*inferior-ensime-server\* buffer. This buffer collects the stdout and stderr of the server process, which is useful for debugging.


  





