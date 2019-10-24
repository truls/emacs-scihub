# Fetch from Sci-Hub in emacs

This package provides functionality for fetching articles from Sci-Hub
from within Emacs. This is useful if you're away from your university
network and can't be bothered to connect via VPM. Eventually, this
package is intended to serve as a fallback for a package for
downloading articles from publishers' websites.

A notable feature this package is that it allows the user to solve
CAPTCHAs prompted by scihub from within Emacs.

While basic functionality seems to be working, the code as a whole is
still work-in-progress and is rather untested. Instructions for
usage/integration will follow. Contributions and feedback welcome.

## Installation
Add `scihub.el` somewhere in your `load-path` using
```elisp
(add-to-list 'load-path "~/path/to/scihub.el")
```

Then load the package using either `use-package`
```elisp
(use-package scihub
  :commands (scihub-get-from-publisher-url
             scihub-get-from-doi
             scihub-get-from-scihub-url))
```

**or** a plain `require`

```elisp
(require 'scihub)
```

## Usage
The interface of this package consists of three functions
    * `scihub-get-from-publisher-url`
    * `scihub-get-from-doi`
    * `scihub-get-from-scihub-url`

All of the commands are callable interactively and the only difference
between them is the type of article reference they
expect. Furthermore, the functions can operate either synchronously,
asynchronously or return an `aoi` promise. See the code docstring for
more details.
