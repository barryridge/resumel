.PHONY: test clean

# Set up test directories
TEST_DIR    = tests
RESULTS_DIR = $(TEST_DIR)/results

# Capture user-emacs-directory at parse time
USER_EMACS_DIRECTORY := $(shell emacs --batch -l ~/.emacs.d/init.el --eval='(princ user-emacs-directory)')

# Find all "ox-extra.el" under $(USER_EMACS_DIRECTORY). Produce "-l /path/to/ox-extra.el" for each.
OX_EXTRA_L_FLAGS := $(shell find $(USER_EMACS_DIRECTORY) -type f -name "ox-extra.el" -print0 \
                            | xargs -0 -I "{}" echo "-l {}")

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
