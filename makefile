# Requires GNU Make, GNU Stow and esh
.PHONY: all stow-% delete delete-% process_packages

OUTPUT_DIR := .out

PACKAGES := $(shell find . -maxdepth 1 -type d ! -name '.*')

all: process_packages system_dirs
	stow --verbose --target=$$HOME --dir=$(OUTPUT_DIR) --restow */

# Directories I do not want to be symlinked as they will be populated with other data which I do not wish to share
system_dirs:
	@mkdir -p $$HOME/.local/bin
	@mkdir -p $$HOME/.local/share
	@mkdir -p $$HOME/.var/
	@mkdir -p $$HOME/.config/emacs
	@mkdir -p $$HOME/.var/app/com.github.IsmaelMartinez.teams_for_linux/config/teams-for-linux # Stop trying to stow all flatpak stuff
	@mkdir -p $$HOME/.local/share/flatpak/overrides
	@mkdir -p $$HOME/.local/share/icons
	@mkdir -p $$HOME/.local/share/themes
	@mkdir -p $$HOME/.local/share/applications


# Process `.esh` template files and store in corresponding dir in OUTPUT_DIR
# All other files are symlinked using stow
process_packages: $(PACKAGES) 
	@mkdir -p $(OUTPUT_DIR)
	@for package in $(PACKAGES); do \
		rm -rf .out/$$package; \
		esh_files=$$(find $$package -type f -name '*.esh' -print); \
		if [ -n "$$esh_files" ]; then \
			for file in $$esh_files; do \
				echo "Processing template $$file"; \
				mkdir -p $(OUTPUT_DIR)/$$(dirname $$file); \
				new_name=$$(echo $$file | sed 's|\.esh$$||'); \
				esh -o $(OUTPUT_DIR)/$$new_name $$file; \
				if [ "$$(head -n 1 $(OUTPUT_DIR)/$$new_name | cut -c 1-2)" = "#!" ]; then \
					echo "Making $(OUTPUT_DIR)/$$new_name executable"; \
					chmod +x $(OUTPUT_DIR)/$$new_name; \
				fi \
			done; \
			package=$$(basename $$package); \
			stow --ignore=.esh --target=$(OUTPUT_DIR)/$$package $$package; \
		else \
			package=$$(basename $$package); \
			ln -s $$(pwd)/$$package $(OUTPUT_DIR)/; \
		fi \
	done

stow-%: system_dirs
	@$(MAKE) --silent process_packages PACKAGES=$*
	stow --verbose --target=$$HOME --dir=$(OUTPUT_DIR) --restow $*

delete:
	stow --verbose --target=$$HOME --dir=$(OUTPUT_DIR) --delete */
	rm -rf .out

delete-%:
	stow --verbose --target=$$HOME --dir=$(OUTPUT_DIR) --delete $*
	rm -rf .out/$*

