### makefile --- NAS-T Makefile
## ROOT  --- run podman as root (0,1)
## EMACS --- emacs program string
## POD_NAME --- name for podman pod
## POD_IMAGE_BASE --- base image for podman containers
### Code:
include infra/common.mk

ROOT?=0
DEBUG?=0
EMACS?=emacsclient -n
POD_NAME?=nas-t-pod
POD_IMAGE_BASE?=docker.io/library/archlinux:latest
ll1=--eval '(asdf:load-asd "btrfs.asd")' --eval '(ql:quickload :btrfs)'
ll2=--eval '(asdf:load-asd "nas-t.asd")' --eval '(ql:quickload :nas-t)'
lp1=--eval '(require :sb-sprof)' --eval '(sb-sprof:start-profiling :sample-interval 0.001)'
lp2=--eval '(sb-sprof:stop-profiling)' --eval '(sb-sprof:report)'
E?=$(EMACS)

PM:=podman
ifeq ($(ROOT),1)
PM=sudo $(PM)
endif

ifeq ($(DEBUG),1)
L=sbcl --noinform --non-interactive $(ll1) $(ll2)
else
L=sbcl --noinform --non-interactive $(ll2)
endif

all::compile
.PHONY::podman-init podman-ps hg-yolo compile install clean deps vars bin
deps:$(INFRA_DIR)/deps.sh;$< find_all_deps
vars:;echo -e $(VARS)

compile::;$(L) --eval '(asdf:compile-system :nas-t)'
test::tests/pkg.lisp;$(L) --eval '(asdf:test-system :nas-t)'
bin:;mkdir -pv $@
nasd:bin compile;$(L) --eval '(asdf:make :nas-t)'
build::nasd;
install::build;install -m 755 nasd /usr/local/bin
clean::;rm -rf */*.fasl

podman-init:;$(PM) pod create --name $(POD_NAME)
podman-ps:;$(PM) ps -a --pod
hg-yolo:;hg commit -A -m make-hg-yolo-$(date);hg push;hgfe # babel+cfg dependency
