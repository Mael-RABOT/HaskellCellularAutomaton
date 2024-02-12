BIN_PATH = $(shell stack path --local-install-root)/bin
NAME = wolfram-exe

all:
	stack build
	cp $(BIN_PATH)/$(NAME) ./$(NAME)

clean:
	stack clean
	rm -f ./$(NAME)

fclean: clean
	rm -f $(BIN_PATH)/$(NAME)

re: fclean all
