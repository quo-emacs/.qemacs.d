# .qemacs.d

## Notice

This project is an experiment, a proof of concept that while usuable is likely
to change dramatically before reaching a `v1.0.0` release. The purpose in sharing
this publicly at this time is to begin the process of raising awareness and
allowing curious pioneer minds to have something to play with while the project
matures.

Also note that no effort has been placed into supporting anything other than
Debian on ARM64 and AMD64 architectures.

## Introduction

This project is a starting point for [qemacs]-based configurations and is
intended to provide a minimum viable developer experience for editing text
files. The overall `Quo-Emacs` project is a starting point for developers to
build their own IDE (for free) and yet enjoy comparable features that commercial
IDEs are monetizing and imbuing with features like AI whether the developers
want it or not.

It can be convenient to think of the `Quo-Emacs` project with a little analogy:
in the universe of Star Wars, no Jedi ever buys an off-the-shelf light-saber,
they all must make one for themselves, by themselves, using only their knowledge
and technology available. `Quo-Emacs` is intended to provide a sane starting
point for developers to build their own light-saber - their own integrated
developer experience.

## Getting Started

It is *highly* encouraged to fork this repository (either publicly or privately)
so that one can make their own customization and fully tailor the developer
experience in ways that the creators of the `Quo-Emacs` could never anticipate.

To get started, please visit the [qemacs] project to build the latest [emacs]
(with [tree-sitter]) and install the `qemacs` shell scripts somewhere suitable
in the `PATH` environment. The last step of setting up [qemacs] is to clone this
repository as the default configuration profile directory: `~/.qemacs.d`.

There are a number of shell scripts within the [qemacs] project, the following
is a very brief summary of each of them.

| Script Name | Description                                           |
| ----------- | ----------------------------------------------------- |
|  `emacs`    | start a standalone, terminal based, emacs instance    |
|  `xemacs`   | graphical version of the `emacs` script               |
|  `qemacs`   | start a terminal based emacs server                   |
|  `qxemacs`  | graphical version of the `qemacs` script              |
|  `qide`     | start a terminal based emacs server with IDE features |
|  `qxide`    | graphical version of the `qide` script                |

The configuration setup provided by this project assumes the usage of these
scripts as they set various environment variables which this configuration takes
into account when starting up GNU Emacs instances.

There are two main variations of the developer experience provided. The first is
the "standard" mode which uses things like [neotree] and is geared to essentially
being a "slightly better than vim" experience. The second mode is the "ide" mode
which uses things like [treemacs] (which imposes a project based approach to
editing text files) and includes the latest advances in syntax highlighting as
well as formal language server protocol (LSP) and debugger access protocol (DAP)
supports.

### Makefile

This repository contains a `Makefile` providing convenience targets for the
installation of various dependencies necessary to support "everything and the
kitchen sink".

``` bash
$ make help
usage: make <help>
       make <deps|deps-debian|deps-golang|deps-nodejs|deps-python>
       make <factory-reset>
       make <reset-startup-answers|reset-transient>
       make <reset-tree-sitter|reset-elpa-straight>
```

### Startup Questions

During the first startup of this configuration, a series of questions is asked
in order to determine the specific developer experience needed. The answers are
cached and used the next time the configuration is started.

The `make reset-startup-answers` target simply removes the
`~/.qemacs.d/transient/qemacs-startup-answers` file.

The cached answers file has a very simple format, each line is a key and a value
separated by a single tab.  The keys are kebab-cased names and the values are an
integer representing the selection made: `-1` for "no", `0` for "unasked" and
`1` for "yes".

The following answers file would provide a developer experience where the slightly
better than default theme and only basic syntax highlighting systems are used.
Such a configuration is suitable for doing casual development or just basic text
editing from the command line on remote servers for example.

```
startup-support-asked	1
use-theming	-1
use-lsp-mode	-1
use-ts-mode	-1
use-go-mode	1
use-web-mode	1
```

(Note: the above configuration would result in the `qide` and `qxide` scripts
behaving exactly the same as `qemacs` and `qxemacs` respectively.)

The questions asked may depend on previous answers, and are specific to this
particular configuration. The questions are setup at the start of the `init.el`
file and the keys are used within the `~/.qemacs.d/libs/support/*.el` packages,
which then setup the actual group of features provided by those support packages.

None of these questions are "set in stone" for this project, these are all simply
experiments to ensure the system is implemented in a way that's simple and yet
flexible enough to meet future needs.

## Developer Experiences

There are two primary developer experiences `Quo-Emacs` is addressing, one is a
"slightly better than vim" experience suitable for developers, sysadmins and
general command-line things on Unix systems. The other is suitable for formal
development where "everything and the kitchen sink" would be nice to have present.

### Slightly Better Than Vim (Standard Experience)

The standard mode is used whenever the `emacs`, `xemacs`, `qemacs` or `qxemacs`
scripts are used to startup.

The following is a very high-level summary of the standard features:

- basic syntax highlighting support (no language servers required)
- has a default theme suitable for local or remote terminal installations
  and supports terminal emulators with transparent backgrounds
- optionally supports separate terminal and graphical "fancy" themes
- has a rudimentary first-run setup process to select theming and other settings
- [evil-mode] editing facilities enabled by default
- [neotree] free-form sidebar tree view bound to `<Control+F12>`
- quick-switch between visible buffers bound to `<F12>`
- quick-switch between files
  - graphical uses `<Control+Tab>`
  - terminal uses `<Control+Backtick>` because `<Control+Tab>` is often bound
    by the terminal emulator to cycle through tiled terminal sessions
- press `<F1>` for a key binding cheatsheet

### Everything and a Kitchen Sink (Developer Experience)

All the same features as the standard mode, with the following exceptions:

- [treemacs] project-based sidebar tree view bound to `<Control+F12>`
- language server protocol supports (`lsp-mode` with `tree-sitter` things)
- debugger access protocol supports (`dape` with `dape-attach` conveniences)

## Licensing

The licensing of `Quo-Emacs` is rather complicated due to the nature of the
context in which `Quo-Emacs` operates.

The [qemacs] repository is licensed under a [gpl-2.0-only] license and does not
contain any [emacs] or [tree-sitter] source code, so that particular `Quo-Emacs`
project has a relatively straight-forward situation.

The [.qemacs.d] repository however does contain third party libraries and a plethora
of packages written specifically for `Quo-Emacs` and are not distributed through
ELPA, MELPA or any other package repository.

The top-level `LICENSE` file in this repository contains the licensing for the
general project itself, namely the `Makefile`, `README.md`, `early-init.el`,
`init.el` and all of the files under the `snippets` directory.

Everything under the `libs` directory has it's own licensing specified that
supercedes the top-level project `LICENSE`. All of the packages written by and
for Quo-Emacs are generally licensed [lgpl-2.1-only] with the exceptions being
due to providing forked or otherwise modified versions of third party sources
(such as the powerline themes package which is [gpl-3.0-only]).

[.qemacsd]: https://github.com/quo-emacs/.qemacs.d
[qemacs]: https://github.com/quo-emacs/qemacs
[emacs]: https://www.gnu.org/software/emacs
[GNU Emacs]: https://www.gnu.org/software/emacs
[evil-mode]: https://github.com/emacs-evil/evil
[treemacs]: https://github.com/Alexander-Miller/treemacs
[neotree]: https://github.com/jaypei/emacs-neotree
[tree-sitter]: https://emacs-tree-sitter.github.io/
[lsp-mode]: https://emacs-lsp.github.io/lsp-mode
[dape]: https://github.com/svaante/dape
[gpl-2.0-only]: https://spdx.org/licenses/GPL-2.0-only.html
[gpl-3.0-only]: https://spdx.org/licenses/GPL-3.0-only.html
[lgpl-2.1-only]: https://spdx.org/licenses/LGPL-2.1-only.html
