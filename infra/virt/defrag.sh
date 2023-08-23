#!/bin/sh
btrfs filesystem defrag -v -r -f -t 32M ${1:-/}
