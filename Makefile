.PHONY: test clean

# Set up test directories
TEST_DIR    = tests
RESULTS_DIR = $(TEST_DIR)/results

# First attempt to locate ox-extra.el using Emacs
OX_EXTRA_PATH := $(shell emacs --batch --eval "(princ (or (locate-library \"ox-extra\") \"\"))")

# If OX_EXTRA_PATH is empty, fall back to finding it under user-emacs-directory
ifeq ($(OX_EXTRA_PATH),)
	USER_EMACS_DIRECTORY := $(shell emacs --batch -l ~/.emacs.d/init.el --eval='(princ user-emacs-directory)')
	OX_EXTRA_PATH := $(shell find $(USER_EMACS_DIRECTORY) -type f -name "ox-extra.el" -print -quit)
endif

# If still not found, error out
ifeq ($(OX_EXTRA_PATH),)
  $(error "ox-extra.el not found. Ensure it is installed.")
endif

OX_EXTRA_L_FLAGS := -l $(OX_EXTRA_PATH)

test: clean
	@mkdir -p $(RESULTS_DIR)
	@echo "Running resumel moderncv template tests..."
	@emacs -Q --batch \
	       $(OX_EXTRA_L_FLAGS) \
	       -l ert \
	       -l resumel.el \
	       -l $(TEST_DIR)/test-resumel-moderncv.el \
	       -f ert-run-tests-batch-and-exit
	@echo "Running resumel altacv template tests..."
	@emacs -Q --batch \
	       $(OX_EXTRA_L_FLAGS) \
	       -l ert \
	       -l resumel.el \
	       -l $(TEST_DIR)/test-resumel-altacv.el \
	       -f ert-run-tests-batch-and-exit
	@echo "Running resumel modaltacv template tests..."
	@emacs -Q --batch \
	       $(OX_EXTRA_L_FLAGS) \
	       -l ert \
	       -l resumel.el \
	       -l $(TEST_DIR)/test-resumel-modaltacv.el \
	       -f ert-run-tests-batch-and-exit

clean:
	@rm -rf $(RESULTS_DIR)
