#makefile for myQC

.PHONY: all

objects = langevin.o

all: $(objects) 
	mv *.o $(obj)
	mv *.mod $(mod)

langevin.o : langevin.f90
	$(FC) $(FFLAGS) -c langevin.f90

clean:
	rm -f *.o *.mod
