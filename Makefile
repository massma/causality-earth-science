# just a dummy makefile to call shake

all :
	cabal v2-run build -- # --lint --progress

clean :
	cabal v2-run build -- clean
