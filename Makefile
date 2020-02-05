# just a dummy makefile to call shake

all :
	stack build && stack exec build -- # --lint --progress

clean :
	stack build && stack exec build -- clean
