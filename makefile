.PHONY: all stow-% delete delete-%

all:
	stow --verbose --target=$$HOME --restow */

stow-%:
	stow --verbose --target=$$HOME --restow $*

delete:
	stow --verbose --target=$$HOME --delete */

delete-%:
	stow --verbose --target=$$HOME --delete $*

