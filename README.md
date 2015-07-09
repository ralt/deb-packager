# deb-packager

Simply create a debian package by defining an s-expression.

See the [test file](t/deb-packager.lisp) for an example definition.


### Roadmap

- Support break/conflicts
- Support gpg signatures
- Make a sub-package to handle make/make install (i.e. source
packaging, which is debian philosophy. Try to make reproducible builds
by going through a docker (or lxc?) container. Put :Depends and
:Build-Depends in the same section?

### License

MIT License.
