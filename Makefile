##
## EPITECH PROJECT, 2024
## PROJECT_NAME
## File description:
## Makefile
##

BINARY_PATH := $(shell stack path --local-install-root)
EXECUTABLE = mypandoc

all:
	stack build --allow-different-user
	cp $(BINARY_PATH)/bin/$(EXECUTABLE)-exe ./$(EXECUTABLE)

clean:
	stack clean --allow-different-user

fclean: clean
	rm -f $(EXECUTABLE)

re: fclean all

tests_run:
	stack build --test

run: all

init: install-hooks install-mango

install-hooks:
	@cp .githooks/commit-msg .git/hooks/commit-msg
	@chmod +x .git/hooks/commit-msg
	@echo "Hooks installed."

install-mango:
	@chmod +x ./init/install-mango.sh
	@./init/install-mango.sh

.PHONY: all clean fclean re
.SILENT: run
