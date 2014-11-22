#!/bin/zsh
# This script prints all prefixes and their expansions in manifest.ttl files
cd ~/instans/tests/
sed -n '/@prefix/p' data-*/**/manifest.ttl |awk '{print "PREFIX", $2, $3;}'|sort -u

