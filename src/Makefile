export FC = gfortran
export FFLAGS = -O3
export bindir = /Users/jamesthorpe/local-hbar/myCol/bin
export LFLAGS=-rcs
export LC=ar
export pwd = $(shell pwd)
export obj = $(pwd)/obj
export mod = $(pwd)/mod

progdirs:= units parser Langevin myCol

.PHONY: all prog clean

all: prog

prog:
	if [ ! -d $(bindir) ]; then mkdir -p $(bindir); fi
	@set -e; \
	for i in $(progdirs); do \
		if [ -d $$i ]; then \
		  if [ -f $$i/Makefile ]; then \
		    $(MAKE) -C $$i all;\
		    fi; \
		  fi; \
	done;
	if [ ! -d $(obj) ] ; then mkdir -o $(obj) ; fi
	if [ ! -d $(mod) ] ; then mkdir -o $(mod) ; fi

clean:
	@set -e; \
	for i in $(progdirs); do \
		if [ -d $$i ]; then \
		  if [ -f $$i/Makefile ]; then \
		    $(MAKE) -C $$i clean;\
		    fi; \
		  fi; \
	done;
	rm -f $(obj)/*.o
	rm -f $(mod)/*.mod
