#EMACS = emacs19
EMACS = emacs20
#EMACS = emacs21
#EMACS = xemacs20
#EMACS = xemacs21

EMACSEN = ../emacs-19.34/src/emacs emacs20 emacs21 xemacs21

all:
	$(EMACS) -batch -l load-cl.el -f compile-cl

#install:

check:
	for e in $(EMACSEN); do						     \
		echo CHECKING $$e;					     \
		$$e -batch -l load-cl.el -l tests.el -f test-cl 2>/dev/null; \
	done

clean:
	rm -f *.elc
