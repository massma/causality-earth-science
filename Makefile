# just a dummy makefile to call shake

all :
	stack exec ./build.sh -- # --lint --progress

clean :
	stack exec ./build.sh -- clean
