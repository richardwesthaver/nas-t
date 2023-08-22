#!/bin/sh
btrfs filesystem mkswapfile --size ${2:-2G} ${1:-swapfile}
