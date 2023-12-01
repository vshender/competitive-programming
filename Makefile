# Root Makefile.  Discovers problem directories (those that include common.mk)
# and forwards targets to them.

PROBLEM_DIRS := $(dir $(shell find . -mindepth 2 -name Makefile))

.PHONY: test clean

# Run tests in all problem directories.
test:
	@for d in $(PROBLEM_DIRS); do \
		echo "=== Testing $$d ===" ; \
		$(MAKE) -C $$d test || exit 1 ; \
	done

# Clean build artifacts in all problem directories.
clean:
	@for d in $(PROBLEM_DIRS); do \
		$(MAKE) -C $$d clean ; \
	done
