# .qemacs.d/libs

This directory is for storing libraries not found on ELPA, MELPA or any other
package repository as well as for storing specific versions of third party
packages that for whatever reason cannot be updated to their source versions
at this time.

## libs/others

The "others" directory contains third party packages that are not available or
are otherwise not working with Quo-Emacs and their latest versions.

For the licensing, please see each package file for their specific licensing
and any files under the `libs/others` path are provided *unmodified*.

## libs/common

The "common" directory contains packages written by and for Quo-Emacs. These are
formal packages and libraries providing all sorts of things like `front-matter`
content support and extensions to the `dape` system for conveniently attaching to
local processes or DAP services.

All of these packages are provided under the [lgpl-2.1-only] license.

## libs/support

The "support" directory contains Quo-Emacs configuration startup features and
functionality. The top-level `init.el` file is intended to contain as little
code directly and instead require support packages to do the actual work of
setting up any given feature.

All of these packages are provided under the [lgpl-2.1-only] license.

## libs/run-commands

The "run-commands" directory contains Quo-Emacs configurations for conveniently
running builds or arbitrary `Makefile` targets.

See the individual package sources for their specific licensing.

## libs/debian

The "debian" directory contains the contents of the `dpkg-dev-el` debian
package. This is necessary because installing that package also installs the
emacs package provided by whatever version of debian is being used. As qemacs is
dedicated to staying with the latest releases of GNU Emacs, this gets a little
confusing for users to have two different installations of the same thing.

For the licensing, please see each package file for their specific licensing
and any files under the `libs/others` path are provided *unmodified*.

[lgpl-2.1-only]: https://spdx.org/licenses/LGPL-2.1-only.html
