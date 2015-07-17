% DEB-PACKAGER(1) deb-packager manual
% Florian Margaine <florian@margaine.com>
% July 18, 2015

# NAME

deb-packager - Simply create a debian package by defining an
s-expression.

# SYNOPSIS

deb-packager [-h|--help] [-v|--version] FILE

# DESCRIPTION

deb-packager creates a debian package based on a simple s-expression.

The binary takes 2 optional arguments:

    -h, --help
        print this help text

    -v, --version
        print the version

When any of these arguments is used, deb-packager immediately exits.

If none of these arguments is provided, the FILE argument becomes
required.

The FILE argument must be a file containing the s-expression that
defines the debian package. The s-expression must be following this
convention (anything in square brackets is a placeholder for a real
value):

    (define-deb-package [package-name]
      (:description "[package description]")
      [...])

deb-packager will then generate a proper debian package based on the
provided data.

# DETAILS

Following is a full example showing the supported parameters that the
s-expression can use. For more clarity, the package is named
"foo". Anything after ";" (if not within double quotes) is a comment.

    (define-deb-package foo
      (:architecture "amd64") ; required, can be "i386", "amd64" or "all"

      ;; Depends is optional. See more examples at
      ;; https://www.debian.org/doc/debian-policy/ch-relationships.html
      (:depends ("bar (> 2)" "baz (> 3)")) ; optional

      (:description "foobar bar qux") ; required
      (:long-description "foo bar
    bar
    qux") ; optional

      (:maintainer "Foo Bar <foo@bar.com>") ; required

      ;; The last changelog value is used as the current package version.
      (:changelog ; required
       (:version "1.0-1"
        :author "Foo Bar <foo@bar.com>"
        :message "Lorem ipsum."
        :date 1434665940) ;; unix timestamp
       (:version "1.1-1"
        :author "Bar Foo <bar@foo.com>"
        :message "Dolor sit amet."
        :date 1434665998))

      ;; Data files are the files that will be installed on the
      ;; filesystem by the package.
      (:data-files ; optional
       (:path #p"usr/bin/foo" ; path to install to, without leading slash
        ;; :content is a byte vector of the file's content.
        ;; "alexandria:read-file-into-byte-vector" is a function that
        ;; reads a file and fetches its content as a byte vector.
        :content (alexandria:read-file-into-byte-vector "path/to/foo")
        :mode 755)

      ;; Control files are the files ran by dpkg when it
      ;; installs/removes/upgrades a package. For more information,
      ;; see https://www.debian.org/doc/debian-policy/ch-maintainerscripts.html
      (:control-files ; optional
       (:path #p"postinst"
        :mode 755
        :content (alexandria:read-file-into-byte-vector "path/to/postinst"))))

# HELP

Get help and report bugs at <https://github.com/ralt/deb-packager>

# MOTIVATION

## Why deb-packager instead of dpkg-buildpackage?

dpkg-buildpackage, while a full-featured solution, requires a very
specific structure of files and folders. While understandable, there
is a lot of unnecessary requirements, such as a compat file that holds
a magic number, the list of directories where the files will be
installed, and generally speaking, a lot more verbosity than needed.

deb-packager aims to make it painless to create a debian package,
while reproducing everything dpkg-buildpackage supports.

Finally, dpkg-buildpackage is relatively slow because of its way of
going through the filesystem to build the package. deb-packager builds
the debian package in memory before writing it to the disk, making it
much faster. (But maybe inconvenient if the package is too big.)
