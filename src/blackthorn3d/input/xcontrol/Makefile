# This is a quick hack since writing an XBox360 library is not our main goal

#TODO: What's the standard way of doing this?
SWIG ?= C:/code/swigwin-2.0.3/swig.exe
CC = C:/mingw/bin/gcc

# these outputs should be manually moved to desired destination
all: xbox360.dll xbox360.lisp
	
XInput.h:
	$(error You must supply XInput.h from DirectX SDK yourself)
	
XInput.lib:
	$(error You must supply XInput.lib from DirectX SDK yourself)

xbox360.c : XInput.h
	
xbox360.dll : xbox360.o XInput.lib
	$(CC) -shared -o $@ $+
	
%.o : %.c
	@echo "Compiling C file: $<"
	@$(CC) -c $< -o $@
	
xbox360.lisp : xbox360.h xbox360.i
	$(SWIG) -cffi xbox360.i
	
.PHONY: clean
clean:
	rm -f xbox360.lisp
	rm -f *.o
	rm -f *.dll