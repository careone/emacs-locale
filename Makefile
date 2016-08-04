VERSION = 24.3
DESTDIR =
PREFIX = /usr
PACKAGE = emacs-locale

all:

install:
	install -d $(DESTDIR)$(PREFIX)/share/emacs/site-lisp/site-start.d
	install -m644 locale-init.el $(DESTDIR)$(PREFIX)/share/emacs/site-lisp/site-start.d
	cp -a locale $(DESTDIR)$(PREFIX)/share/emacs/site-lisp
	install -d $(DESTDIR)$(PREFIX)/share/doc/$(PACKAGE)
	cp -a doc/* $(DESTDIR)$(PREFIX)/share/doc/$(PACKAGE)

uninstall:
	rm -f $(DESTDIR)$(PREFIX)/share/emacs/site-lisp/site-start.d/locale-init.el
	rm -rf $(DESTDIR)$(PREFIX)/share/emacs/site-lisp/locale
	rm -rf $(DESTDIR)$(PREFIX)/share/doc/$(PACKAGE)

