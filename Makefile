#EMACS=emacs19
EMACS=emacs20
#EMACS=emacs21
#EMACS=xemacs20
#EMACS=xemacs21

all:
	$(EMACS) -batch -l load-cl.el -f compile-cl

#install:

clean:
	rm -f *.elc
