# Fetch from Sci-Hub in emacs

This package provides functionality for fetching articles from Sci-Hub
from within Emacs. This is useful if you're away from your university
network and can't be bothered to connect via VPM. Eventually, this
package is intended to serve as a fallback for a package for
downloading articles from publishers' websites.

As a notable feature, this package support prompting for Sci-Hub
captchas from within Emacs.

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
