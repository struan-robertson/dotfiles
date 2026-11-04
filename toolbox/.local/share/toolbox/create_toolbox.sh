#!/bin/sh
# Create, recreate, or refresh a toolbox from its committed package list.
#
#   create_toolbox.sh <name>            ensure the toolbox exists and has its
#                                       packages installed (no destruction)
#   create_toolbox.sh --rebuild <name>  refresh the package list from the live
#                                       toolbox, then recreate it
#   create_toolbox.sh --reset <name>    recreate from the committed package
#                                       list without refreshing it
#
# --rebuild is what you want after a Fedora major-version upgrade, since
# toolboxes are tied to the host release. --reset is the recovery path when
# dnf reports a package that no longer exists: edit the list, then rerun with
# --reset.
set -eu

usage() {
  echo "usage: $0 [--rebuild|--reset] <toolbox-name>" >&2
  exit 2
}

mode=ensure
case "${1:-}" in
  --rebuild) mode=rebuild; shift ;;
  --reset) mode=reset; shift ;;
  -h|--help) usage ;;
esac
[ "$#" -eq 1 ] || usage
name="$1"

dir="$(cd "$(dirname "$(readlink -f "$0")")" && pwd)"
list="$dir/lists/$name-packages.txt"
[ -f "$list" ] || { echo "error: no package list at $list" >&2; exit 1; }

# An empty list would discard a working toolbox and build a useless one.
packages=$(sed '/^#/d' "$list")
[ -n "$packages" ] || { echo "error: $list has no packages" >&2; exit 1; }

container_exists() {
  toolbox list --containers 2>/dev/null | awk 'NR > 1 {print $2}' | grep -qx "$1"
}

if [ "$mode" = rebuild ] && container_exists "$name"; then
  # Snapshot what is installed now before throwing it away. If the toolbox is
  # too broken to query (e.g. right after a major upgrade), fall back to the
  # committed list.
  "$dir/extract_packages.sh" "$name" || \
    echo "warning: could not refresh $list; using committed version" >&2
fi

if [ "$mode" != ensure ] && container_exists "$name"; then
  toolbox rm -f "$name" || {
    echo "error: could not remove $name" >&2
    exit 1
  }
fi

if ! container_exists "$name"; then
  toolbox create -y "$name"
fi

# NOTE: toolbox run execs directly (no shell), so the package list must be
# passed as separate arguments, not as one quoted command string.
# shellcheck disable=SC2086
if ! toolbox run -c "$name" sudo dnf install -y $packages; then
  echo "error: package installation failed" >&2
  echo "fix $list, then rerun: $0 --reset $name" >&2
  exit 1
fi

echo "$name toolbox ready"
