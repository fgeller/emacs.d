# scala-errors

## Summary

Quickly navigate to errors in a Scala project using [sbt-quickfix][].

## Usage

Compile your project with SBT and run `M-x scala-errors-show-errors`. Any errors
found will be displayed in a compilation buffer. The following commands are
available to navigate errors:

| Command name                    | Description                                |
|---------------------------------|--------------------------------------------|
| `scala-errors-goto-first-error` | Move to the first SBT error                |
| `scala-errors-goto-next-error`  | Move to the next SBT error                 |
| `scala-errors-goto-prev-error`  | Move to the previous SBT error.            |

## Installing

You will need Emacs 24+, `make` and [Cask](https://github.com/cask/cask) to
build the project.

1. Install the [sbt-quickfix][] plugin.

2. Configure Emacs to use the [MELPA][] package repository.

3. Download this project. The makefile will install the package using the Emacs package manager.

   ```sh
   cd scala-errors
   make && make install
   ```

4. Add the following to your Emacs config to improve integration with other Emacs packages.

   ```elisp
   (add-hook 'scala-mode-hook #'scala-errors-init)
   (add-hook 'scala-mode-hook #'scala-errors-spacemacs-init)
   ```

## Contributing

Yes, please do! See [CONTRIBUTING][] for guidelines.

## License

See [COPYING][]. Copyright (c) 2016 Chris Barrett.


[CONTRIBUTING]: ./CONTRIBUTING.md
[COPYING]: ./COPYING
[sbt-quickfix]: https://github.com/dscleaver/sbt-quickfix
[MELPA]: https://melpa.org/#/getting-started
