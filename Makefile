#EMACS = emacs19
EMACS = emacs20
#EMACS = emacs21
#EMACS = xemacs20
#EMACS = xemacs21

EMACSEN = emacs20 emacs21 xemacs21 # ../emacs-19.34/src/emacs

all:
	$(EMACS) -batch -l load-cl.el -f compile-cl

#install:

TESTFILES = -l load-cl.el -l batch.el -l tests.el

check:
	for e in $(EMACSEN); do						\
		echo CHECKING $$e;					\
		$$e -batch $(TESTFILES) -f test-cl 2> /dev/null;	\
	done

clean:
	rm -f *.elc
