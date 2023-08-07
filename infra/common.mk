# common.mk --- common GNU Make rules for NAS-T
__ := $(.VARIABLES)
COMMON_MK=$(lastword $(MAKEFILE_LIST))
INFRA_DIR=$(dir $(COMMON_MK))
ROOT_DIR:=$(abspath $(INFRA_DIR)/..)
SRC_DIR=$(abspath $(ROOT_DIR)/src)
TESTS_DIR=$(abspath $(ROOT_DIR)/tests)
DOCS_DIR=$(abspath $(ROOT_DIR)/docs)
LISP_FILES=$(shell find $(ROOT_DIR) -type f \( -name '*.asd' -o -name '*.lisp' \) )
SHELL=/bin/sh
UNAME:=$(shell uname)
HG_COMMIT:=
GIT_COMMIT:=
VERSION:=
VARS:=$(foreach v,$(filter-out $(__) __,$(.VARIABLES)),$(info $(v) = $($(v))))
deps::$(INFRA_DIR)/deps.sh;$< find_all_deps
.PHONY::vars
vars::;$(VARS) echo
