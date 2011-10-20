## Overview
Eclim is an Eclipse plugin which exposes Eclipse features through a
server interface.  When this server is started, the command line utility
eclim can be used to issue requests to that server.

Emacs-eclim uses the eclim server to integrate eclipse with
emacs. This project wants to bring some of the invaluable features
from eclipse to emacs.

## Installation
1. [download and install](http://eclim.org/guides/install.html) eclim
1. download emacs-eclim
1. add the following code to your emacs startup script

        (add-to-list 'load-path (expand-file-name "/path/to/emacs-eclim/"))
        ;; only add the vendor path when you want to use the libraries provided with emacs-eclim
        (add-to-list 'load-path (expand-file-name "~/coding/git/emacs-eclim/vendor"))
        (require 'eclim)

        (setq eclim-auto-save t)
        (global-eclim-mode)

## Configuration

### Eclipse installation

Emacs-eclim tries its best to locate your Eclipse installation.  If
you have Eclipse installed in a non-standard location
(i.e. ~/opt/eclipse) you have to options:

* Either customize the `eclim-executable` variable to make it point to the `eclim` executable within the Eclipe directory if necessary
* Or, you can override the lookup by adding the following to your startup script.

        (custom-set-variables
         '(eclim-eclipse-dirs '("~/opt/eclipse")))

### Displaying compilation error messages in the echo area

When the cursor is positioned on an error marker in a code buffer,
emacs-eclim uses the local help feature in emacs to display the
corresponding error message in the echo area. You can either invoke
`(display-local-help)` manually or activate automatic display of local
help by adding the following to .emacs:
        
	(setq help-at-pt-display-when-idle t)
	(setq help-at-pt-timer-delay 0.1)
	(help-at-pt-set-timer)

### Configuring auto-complete-mode

If you wish to use [auto-complete-mode] with emacs-eclim, add the
following to your .emacs:

	;; regular auto-complete initialization
	(require 'auto-complete-config)
	(ac-config-default)

	;; add the emacs-eclim source
	(require 'ac-emacs-eclim-source)
	(add-hook 'eclim-mode-hook (lambda () (add-to-list 'ac-sources 'ac-source-emacs-eclim)))

### Configuring company-mode
  
Emacs-eclim can integrate with [company-mode] to provide pop-up
dialogs for auto-completion. To activate this, you need to add the
following to your .emacs:

	(require 'company)
	(require 'company-emacs-eclim)
	(company-emacs-eclim-setup)
	(global-company-mode t)

## Optional dependencies
* A recent version (0.6.0 or later) of [yasnippet](http://code.google.com/p/yasnippet/).
* A recent version (tested with 0.5) of [company-mode]
* ido-mode (part of emacs as of version 22)

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

1. Fork emacs-eclim
2. Create a topic branch - `git checkout -b my_branch`
3. Make your changes and update the History.txt file
4. Push to your branch - `git push origin my_branch`
5. Send me a pull-request for your topic branch
6. That's it!

[company-mode]:http://nschum.de/src/emacs/company-mode/
[auto-complete-mode]:http://cx4a.org/software/auto-complete/