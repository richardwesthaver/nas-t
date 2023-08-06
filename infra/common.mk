# common.mk --- common GNU Make rules for NAS-T
COMMON_MK:=$(abspath $(lastword $(MAKEFILE_LIST)))
INFRA_DIR:=$(notdir $(patsubst %/,%,$(dir $(COMMON_MK))))
ROOT_DIR:=$(abspath ../$(INFRA_DIR))
SRC_DIR:=$(abspath $(ROOT_DIR)/src)
TESTS_DIR:=$(abspath $(ROOT_DIR)/tests)
DOCS_DIR:=$(abspath $(ROOT_DIR)/docs)

deps::$(INFRA_DIR)/deps.sh;$< find_all_deps
