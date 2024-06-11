.PHONY: all stow-% delete delete-%

OUTPUT_DIR := .out

PACKAGES := $(shell find . -maxdepth 1 -type d ! -name '.*')

all: copy_files

copy_files: $(PACKAGES) 
	@mkdir -p $(OUTPUT_DIR)
	@rm -rf .out/*
	@for package in $(PACKAGES); do \
		ash_files=$$(find $$package -type f -name '*.ash' -print); \
		if [ -n "$$ash_files" ]; then \
			for file in $$ash_files; do \
				echo "Processing template $$file"; \
				mkdir -p $(OUTPUT_DIR)/$$(dirname $$file); \
				new_name=$$(echo $$file | sed 's|\.ash$$||'); \
				esh -o $(OUTPUT_DIR)/$$new_name $$file; \
			done; \
			package=$$(basename $$package); \
			echo $$package; \
			stow --ignore=.ash --target=$(OUTPUT_DIR)/$$package $$package; \
		else \
			package=$$(basename $$package); \
			ln -s $$(pwd)/$$package $(OUTPUT_DIR)/; \
		fi \
	done

stow-%:
	stow --verbose --target=$$HOME --restow $*

delete:
	stow --verbose --target=$$HOME --delete */

delete-%:
	stow --verbose --target=$$HOME --delete $*

