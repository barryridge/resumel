.PHONY: test clean

# Define test directories
TEST_DIR    = tests
RESULTS_DIR = $(TEST_DIR)/results

# Allow overriding Emacs executable and flags via environment variables
EMACS ?= emacs
EMACS_FLAGS ?= --batch -Q

# Control whether to load Emacs packages (default: false)
LOAD_EMACS_PACKAGES ?= false

# Check if we're running in CI or not
ifndef GITHUB_ACTIONS
# First attempt to locate ox-extra.el using Emacs
OX_EXTRA_PATH := $(shell $(EMACS) --batch --eval "(princ (or (locate-library \"ox-extra\") \"\"))")

# If OX_EXTRA_PATH is empty, fall back to finding it under user-emacs-directory
ifeq ($(OX_EXTRA_PATH),)
	USER_EMACS_DIRECTORY := $(shell $(EMACS) --batch -l ~/.emacs.d/init.el --eval='(princ user-emacs-directory)')
	OX_EXTRA_PATH := $(shell find $(USER_EMACS_DIRECTORY) -type f -name "ox-extra.el" -print -quit)
endif

else
# If we're on GitHub Actions, set OX_EXTRA_PATH to empty
OX_EXTRA_PATH =
endif

# Set OX_EXTRA_L_FLAGS excluding the path if not found
OX_EXTRA_L_FLAGS := $(if $(OX_EXTRA_PATH),-l $(OX_EXTRA_PATH),)

# Setup Emacs packages if required
install-packages:
	@if [ "$(LOAD_EMACS_PACKAGES)" = "true" ]; then \
		echo "Installing Emacs packages..."; \
		$(EMACS) $(EMACS_FLAGS) \
			--eval "(require 'package)" \
			--eval "(add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\") t)" \
			--eval "(add-to-list 'package-archives '(\"gnu\" . \"https://elpa.gnu.org/packages/\") t)" \
			--eval "(package-initialize)" \
			--eval "(unless package-archive-contents (package-refresh-contents))" \
			--eval "(package-install 'org)" \
			--eval "(package-install 'org-contrib)" \
			--eval "(if (require 'ox-extra nil t) nil (package-install 'ox-extra))" \
			--eval "(require 'ox-extra)" \
			--eval "(ox-extras-activate '(ignore-headlines))"; \
	fi

test: clean install-packages
	@mkdir -p $(RESULTS_DIR)

	# Run tests for each template
	@echo "Running resumel moderncv template tests..."
	@$(EMACS) $(EMACS_FLAGS) $(OX_EXTRA_L_FLAGS) -l ert -l resumel.el -l $(TEST_DIR)/test-resumel-moderncv.el -f ert-run-tests-batch-and-exit

	@echo "Running resumel altacv template tests..."
	@$(EMACS) $(EMACS_FLAGS) $(OX_EXTRA_L_FLAGS) -l ert -l resumel.el -l $(TEST_DIR)/test-resumel-altacv.el -f ert-run-tests-batch-and-exit

	@echo "Running resumel modaltacv template tests..."
	@$(EMACS) $(EMACS_FLAGS) $(OX_EXTRA_L_FLAGS) -l ert -l resumel.el -l $(TEST_DIR)/test-resumel-modaltacv.el -f ert-run-tests-batch-and-exit

clean:
	@rm -rf $(RESULTS_DIR)
