# deb-packager

Simply create a debian package by defining an s-expression.

See the [test file](t/fixtures/test-package.lisp) for an example definition.

Right now, this is supported:

- Create a debian binary package
- Use custom control files (e.g. postinst, postrm, config, templates,
etc.)
- Define the list of files to include in the package (and to which
path to install them)
- Auto-generated changelog and package properties based on the
definition

Notably missing (yet) features:

- Creating source packages
- Creating .dsc and .changes files
- Compiling and overriding compilation options
- GPG support

### Why deb-packager instead of dpkg-buildpackage?

dpkg-buildpackage, while a full-featured solution, requires a very
specific structure of files and folders. While understandable, there
is a lot of unnecessary requirements, such as a compat file that holds
a magic number, the list of directories where the files will be
installed, and generally speaking, a lot more verbose than needed.

deb-packager aims to make it painless to create a debian package,
while reproducing everything dpkg-buildpackage supports.

Finally, dpkg-buildpackage is relatively slow because of its way of
going through the filesystem to build the package. deb-packager builds
the debian package in memory before writing it to the disk, making it
much faster. (But maybe inconvenient if the package is too big.)

### Roadmap

- Support source package creation (+ .dsc & .changes), patching, and
  building from this source package to binary package. Using pbuilder
  to handle the creation of the package means being reliant on
  devscripts being installed, and this should be avoided. (I'd like
  deb-packager to be runnable on non-debian.) So a custom solution
  using lxc to handle the chrooting, fetching build dependencies,
  running configure/make/make install, and generating a deb-packager
  definition is probably what's going to be done. To make it even more
  cross-platform, something based on vagrant is also possible. Or use
  vagrant and lxc as a backend. Hm...
- Support building python packages. In addition, see if it's possible
  to build nodejs packages, php packages, etc.
- Support gpg signatures
- Support break/conflicts

### License

MIT License.
