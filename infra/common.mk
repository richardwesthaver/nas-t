### common.mk --- common GNU Make rules for NAS-T

### Code:
__ := $(.VARIABLES)
COMMON_MK=$(lastword $(MAKEFILE_LIST))
INFRA_DIR=$(dir $(COMMON_MK))
ROOT_DIR:=$(abspath $(INFRA_DIR)/..)
SRC_DIR=$(abspath $(ROOT_DIR)/src)
TESTS_DIR=$(abspath $(ROOT_DIR)/tests)
DOCS_DIR=$(abspath $(ROOT_DIR)/docs)
LISP_FILES=$(shell find $(ROOT_DIR) -type f \( -name '*.asd' -o -name '*.lisp' \) )
LINUX_VERSION:=$(shell uname -r | cut -d- -f1)
SHELL=/bin/sh
UNAME=$(shell uname)
CURL:=curl
CPU_COUNT:=$(shell getconf _NPROCESSORS_ONLN)
HG_COMMIT:=$(shell hg id -i)
VERSION:=
VARS:=$(foreach v,$(filter-out $(__) __,$(.VARIABLES)),"\n$(v) = $($(v))")
