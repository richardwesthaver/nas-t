# makefile --- NAS-T Makefile

load=--eval '(asdf:load-asd "nas-t.asd")' --eval '(ql:quickload :nas-t)'
p1=--eval '(require :sb-sprof)' --eval '(sb-sprof:start-profiling :sample-interval 0.001)'
p2=--eval '(sb-sprof:stop-profiling)' --eval '(sb-sprof:report)'

LISP?=sbcl --noinform --non-interactive $(load) 
EL?=emacs --batch
all:compile install
compile:;$(LISP) --eval '(asdf:compile-system :nas-t)'
install:;
clean:;rm -rf */*.fasl
