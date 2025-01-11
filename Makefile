.PHONY: test clean

TEST_DIR = tests
FIXTURE_DIR = $(TEST_DIR)/fixtures
RESULTS_DIR = $(TEST_DIR)/results
EXPECTED_DIR = $(TEST_DIR)/expected

test: clean
	@mkdir -p $(RESULTS_DIR)
	@emacs -Q --batch \
		-l ert \
		-l ox-latex \
		-l resumel.el \
		-l $(TEST_DIR)/test-resumel.el \
		-f ert-run-tests-batch-and-exit

clean:
	@rm -rf $(RESULTS_DIR)
