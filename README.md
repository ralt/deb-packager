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

### Roadmap

- Write documentation
- Support gpg signatures
- Support break/conflicts
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

### License

MIT License.
