BIN_PATH = $(shell stack path --local-install-root)/bin
NAME = wolfram

all:
	stack build
	cp $(BIN_PATH)/$(NAME)-exe ./$(NAME)

clean:
	stack clean
	rm -f ./$(NAME)

fclean: clean
	rm -f $(BIN_PATH)/$(NAME)-exe

re: fclean all
