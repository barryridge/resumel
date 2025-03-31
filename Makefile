.PHONY: test clean

# Define test directories
TEST_DIR    = tests
RESULTS_DIR = $(TEST_DIR)/results

# Allow overriding Emacs executable and flags via environment variables
EMACS ?= emacs
EMACS_FLAGS ?= --batch -l ./test-init.el

# Set diff-pdf tolerance levels based on the environment
ifndef GITHUB_ACTIONS
# Local environment settings
DIFF_PDF_CHANNEL_TOLERANCE ?= 0
DIFF_PDF_PER_PAGE_PIXEL_TOLERANCE ?= 0
else
# GitHub Actions CI settings
DIFF_PDF_CHANNEL_TOLERANCE ?= 150
DIFF_PDF_PER_PAGE_PIXEL_TOLERANCE ?= 50000
endif

# Run tests for each template
test: clean
	@mkdir -p $(RESULTS_DIR)
	@echo "Running resumel template tests..."
	@DIFF_PDF_CHANNEL_TOLERANCE=$(DIFF_PDF_CHANNEL_TOLERANCE) \
	 	DIFF_PDF_PER_PAGE_PIXEL_TOLERANCE=$(DIFF_PDF_PER_PAGE_PIXEL_TOLERANCE) \
	 	$(EMACS) $(EMACS_FLAGS) \
	        -l ert \
	        -l resumel.el \
	        -l $(TEST_DIR)/test-resumel.el \
	        -f ert-run-tests-batch-and-exit

clean:
	@rm -rf $(RESULTS_DIR)
