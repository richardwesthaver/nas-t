# makefile --- NAS-T Makefile
## ROOT  --- run podman as root (0,1)
## EMACS --- emacs program string
## POD_NAME --- name for podman pod
## POD_IMAGE_BASE --- base image for podman containers
include infra/common.mk

ROOT?=0
EMACS?=emacsclient -n
POD_NAME?=nas-t-pod
POD_IMAGE_BASE?=docker.io/library/archlinux:latest
ll=--eval '(asdf:load-asd "nas-t.asd")' --eval '(ql:quickload :nas-t)'
lp1=--eval '(require :sb-sprof)' --eval '(sb-sprof:start-profiling :sample-interval 0.001)'
lp2=--eval '(sb-sprof:stop-profiling)' --eval '(sb-sprof:report)'
L?=sbcl --noinform --non-interactive $(ll) 
E?=$(EMACS)

ifeq ($(ROOT),1)
PM=sudo podman
else
PM=podman
endif

all::compile install
.PHONY::podman-init podman-ps hg-yolo

compile::;$(L) --eval '(asdf:compile-system :nas-t)'
test::tests/pkg.lisp;$(L) --eval '(asdf:test-system :nas-t)'
install::;$(L) --eval '(asdf:make :nas-t)'
clean::;rm -rf */*.fasl

podman-init:;$(PM) pod create --name $(POD_NAME)
podman-ps:;$(PM) ps -a --pod
hg-yolo:;hg commit -A -m make-hg-yolo-$(date);hg push;hg-fast-export.sh # babel dependency
