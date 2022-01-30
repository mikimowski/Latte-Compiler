EXECUTABLE_NAME = latc_x86
GHC_BIN = bin/
LIB_DIR = lib/

all:
	mkdir -p $(GHC_BIN)
	gcc -m32 -g -c -O $(LIB_DIR)/latte_lib.c -o $(LIB_DIR)/latte_lib.o
	ghc -O -o $(EXECUTABLE_NAME) src/Main.hs -isrc:src/:src/bnfc:src/frontend:src/utils:src/backend  -odir $(GHC_BIN) -hidir $(GHC_BIN)
clean:
	rm -rf $(GHC_BIN)
	rm -f $(EXECUTABLE_NAME)
