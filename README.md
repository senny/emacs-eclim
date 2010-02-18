## Overview
Eclim is an Eclipse plugin which exposes Eclipse features through a
server interface.  When this server is started, the command line utility
eclim can be used to issue requests to that server.

Emacs-eclim uses the eclim server to integrate eclipse with
emacs. This project wants to bring some of the invaluable features
from eclipse to emacs.

## Installation
1. download and install eclim ([eclim download page](http://github.com/ervandew/eclim/downloads))
1. download emacs-eclim
1. add the following code to your emacs startup script
        (add-to-list 'load-path (expand-file-name "/path/to/emacs-eclim/"))
        ;; only add the vendor path when you want to use the libraries provided with emacs-eclim
        (add-to-list 'load-path (expand-file-name "~/coding/git/emacs-eclim/vendor"))
        (require 'eclim)

        (setq eclim-auto-save t)
        (global-eclim-mode)

## Usage
To get started just lunch the eclim executable that the placed in your Eclipse installation directory.

* [Projects](http://wiki.github.com/senny/emacs-eclim/projects)
* [Code Completion](http://wiki.github.com/senny/emacs-eclim/code-completion)
* [Java](http://wiki.github.com/senny/emacs-eclim/java)
* [Ant](http://wiki.github.com/senny/emacs-eclim/ant)
* [Maven](http://wiki.github.com/senny/emacs-eclim/maven)
* [Problems and Errors](http://wiki.github.com/senny/emacs-eclim/problems-and-errors)

## Contributing

The project is under active development and we are always looking for assistance.

1. [Fork][0] emacs-eclim
2. Create a topic branch - `git checkout -b my_branch`
3. Make your changes and update the History.txt file
4. Push to your branch - `git push origin my_branch`
5. Send me a pull-request for your topic branch
6. That's it!
