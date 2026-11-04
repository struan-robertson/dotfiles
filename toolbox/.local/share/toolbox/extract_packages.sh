#!/bin/sh
# Refresh a toolbox's package list from what is currently installed.
#
# Creates a throwaway toolbox to discover the base package set for this host
# release, diffs it against the target toolbox, and writes the result to
# lists/<name>-packages.txt. Refuses to replace the list with an empty one, so
# a toolbox left broken by a failed install cannot erase the committed list.
#
#   extract_packages.sh <toolbox-name>
set -eu

name="${1:?usage: $0 <toolbox-name>}"

dir="$(cd "$(dirname "$(readlink -f "$0")")" && pwd)"
lists="$dir/lists"
list="$lists/$name-packages.txt"
mkdir -p "$lists"

container_exists() {
  toolbox list --containers 2>/dev/null | awk 'NR > 1 {print $2}' | grep -qx "$1"
}
container_exists "$name" || { echo "error: no toolbox named $name" >&2; exit 1; }

# The name is unique so an existing "scratch" toolbox is left alone.
scratch="toolbox-extract-$$"
base="$(mktemp)"
installed="$(mktemp)"
new="$(mktemp)"
cleanup() {
  rm -f "$base" "$installed" "$new"
  toolbox rm -f "$scratch" >/dev/null 2>&1 || true
}
trap cleanup EXIT INT TERM

# A fresh toolbox gives the base user-installed set for this host release.
toolbox create -y "$scratch"
toolbox run -c "$scratch" dnf repoquery --userinstalled --qf '%{name}\n' > "$base"
sort -o "$base" "$base"
toolbox run -c "$name" dnf repoquery --userinstalled --qf '%{name}\n' > "$installed"
sort -o "$installed" "$installed"
comm -13 "$base" "$installed" > "$new"

if [ ! -s "$new" ]; then
  echo "error: extracting $name produced an empty list; keeping $list" >&2
  exit 1
fi

mv "$new" "$list"
echo "wrote $list ($(wc -l < "$list") packages)"
