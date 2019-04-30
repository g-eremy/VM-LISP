LISP_EXT := lisp
COMPILED_EXT := fas

SRC := ./src/*/*.$(LISP_EXT)

COMPILED_DIR := ./bin
SRC_FILE := $(COMPILED_DIR)/launch.$(LISP_EXT)
COMPILED_FILE := $(COMPILED_DIR)/launch.$(COMPILED_EXT)

all:
	cat $(SRC) > $(SRC_FILE)
	clisp -c $(SRC_FILE) -o $(COMPILED_FILE) --silent
