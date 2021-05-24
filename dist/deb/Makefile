DESTDIR     ?=
prefix      ?= /usr
bindir      ?= $(DESTDIR)/$(prefix)/bin
datarootdir ?= $(DESTDIR)/$(prefix)/share
docdir      ?= $(datarootdir)/doc/literatex-haskell
man1dir     ?= $(datarootdir)/man/man1

SHELL := bash

build:
	@stack build
.PHONY: build

install:
	$(eval LIROOT := $(shell stack path --local-install-root))
	@mkdir -p "$(bindir)"
	@install -m 0755 "$(LIROOT)/bin/literatex" "$(bindir)/literatex"
	@mkdir -p "$(man1dir)"
	@install -m 0644 -T <(gzip -c doc/literatex.1) "$(man1dir)/literatex.1.gz"
	@mkdir -p "$(docdir)"
	@install -m 0644 -T <(gzip -c README.md) "$(docdir)/README.md.gz"
	@install -m 0644 -T <(gzip -c CHANGELOG.md) "$(docdir)/changelog.gz"
	@install -m 0644 -T <(gzip -c LICENSE) "$(docdir)/LICENSE.gz"
.PHONY: install

test:
	@stack test
.PHONY: test
