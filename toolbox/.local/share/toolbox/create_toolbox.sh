#!/bin/sh
# Create or rebuild a toolbox from its committed package list.
#
#   create_toolbox.sh <name>            ensure the toolbox exists and has its
#                                       packages installed (no destruction)
#   create_toolbox.sh --rebuild <name>  refresh the package list from the live
#                                       toolbox, then destroy and recreate it
#
# --rebuild is what you want after a Fedora major-version upgrade, since
# toolboxes are tied to the host release. It is deliberately not the default
# so that a stray `make toolboxes` cannot wipe working toolboxes.
set -eu

usage() {
  echo "usage: $0 [--rebuild] <toolbox-name>" >&2
  exit 2
}

rebuild=0
case "${1:-}" in
  --rebuild) rebuild=1; shift ;;
  -h|--help) usage ;;
esac
[ "$#" -eq 1 ] || usage
name="$1"

dir="$(cd "$(dirname "$(readlink -f "$0")")" && pwd)"
list="$dir/lists/$name-packages.txt"
[ -f "$list" ] || { echo "error: no package list at $list" >&2; exit 1; }

container_exists() {
  toolbox list --containers 2>/dev/null | awk 'NR > 1 {print $2}' | grep -qx "$1"
}

if [ "$rebuild" -eq 1 ] && container_exists "$name"; then
  # Snapshot what is installed now before throwing it away. If the toolbox is
  # too broken to query (e.g. right after a major upgrade), fall back to the
  # committed list.
  "$dir/extract_packages.sh" "$name" || \
    echo "warning: could not refresh $list; using committed version" >&2
  toolbox rm -f "$name"
fi

if ! container_exists "$name"; then
  toolbox create -y "$name"
fi

# NOTE: toolbox run execs directly (no shell), so the package list must be
# passed as separate arguments, not as one quoted command string.
# shellcheck disable=SC2046
toolbox run -c "$name" sudo dnf install -y $(grep -v '^#' "$list")

echo "$name toolbox ready"
