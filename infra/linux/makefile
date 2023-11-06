### infra/linux/makefile --- Linux

### Code:
# infra/linux/makefile --- NAS-T Linux Makefile
include ../common.mk
# 'linux/###'
LINUX_TARGET:=linux-$(LINUX_VERSION)
all:$(LINUX_TARGET) config
$(LINUX_TARGET):get-tarball.sh;mkdir -pv $@
	gpg --export autosigner@ torvalds@ gregkh@ > $@/keyring.gpg
	./$< $(LINUX_VERSION) $(abspath .) $(abspath $@/keyring.gpg)
	unxz $@.tar.xz && tar -xvf $@.tar
config:$(LINUX_TARGET);cd $< && make mrproper -j && zcat /proc/config.gz > .config && make localmodconfig
clean::;rm -rf $(LINUX_TARGET) $(LINUX_TARGET).* linux-tarball-verify.*
