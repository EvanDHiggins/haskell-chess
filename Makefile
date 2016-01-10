CC = ghc
FLAGS = -o
SRC = *.hs
OUT = chess

default:
	$(CC) $(FLAGS) $(OUT) $(SRC)

.PHONY: clean
clean:
	rm *.hi *.o $(OUT)
