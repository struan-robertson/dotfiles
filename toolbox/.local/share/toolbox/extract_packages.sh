#!/bin/sh
# Refresh a toolbox's package list from what is currently installed.
#
# Creates a throwaway "scratch" toolbox to discover the base package set for
# this host release, diffs it against the target toolbox, and writes the
# result to lists/<name>-packages.txt.
#
#   extract_packages.sh <toolbox-name>
set -eu

name="${1:?usage: extract_packages.sh <toolbox-name>}"

dir="$(cd "$(dirname "$(readlink -f "$0")")" && pwd)"
lists="$dir/lists"
mkdir -p "$lists"

base="$(mktemp)"
installed="$(mktemp)"
cleanup() {
  rm -f "$base" "$installed"
  toolbox rm -f scratch >/dev/null 2>&1 || true
}
trap cleanup EXIT INT TERM

# A fresh toolbox gives the base user-installed set for this host release.
toolbox rm -f scratch >/dev/null 2>&1 || true
toolbox create -y scratch
toolbox run -c scratch dnf repoquery --userinstalled --qf '%{name}\n' | sort > "$base"
toolbox run -c "$name" dnf repoquery --userinstalled --qf '%{name}\n' | sort > "$installed"
comm -13 "$base" "$installed" > "$lists/$name-packages.txt"

echo "wrote $lists/$name-packages.txt ($(wc -l < "$lists/$name-packages.txt") packages)"
