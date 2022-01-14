Name:          literatex-haskell
Version:       {{VERSION}}
Release:       1%{?dist}
Summary:       Transform literate source code to Markdown
License:       MIT
URL:           https://github.com/ExtremaIS/literatex-haskell
Source0:       literatex-haskell-{{VERSION}}.tar.xz
BuildArch:     {{ARCH}}
BuildRequires: make
Requires:      glibc,gmp
#ExcludeArch:

%description
LiterateX transforms literate source code to Markdown.  Write documentation in
Markdown format in the comments of source code, and LiterateX can transform
the file to Markdown, optionally including the source code with syntax
highlighting and line numbers.

%global debug_package %{nil}

%prep
%setup -q

%build

%install
make install DESTDIR=%{buildroot} PREFIX=/usr

%check
make test

%files
%{_bindir}/literatex
%{_mandir}/man1/literatex.1.gz
%{_datadir}/doc/%{name}/

%changelog
* {{DATE}} {{RPMFULLNAME}} <{{RPMEMAIL}}> - {{VERSION}}-1
- Release {{VERSION}}
