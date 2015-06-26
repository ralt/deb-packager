# deb-packager

Simply create a debian package by defining an s-expression.

See the [test file](t/deb-packager.lisp) for an example definition.


### Roadmap

- Support versions for Depends
- Make .tar.gz instead of .tar
- Make a sub-package to handle make/make install (i.e. source
packaging, which is debian philosophy. Try to make reproducible builds
by going through a docker (or lxc?) container.
- Support break/conflicts
- Support gpg signatures

### License

MIT License.
