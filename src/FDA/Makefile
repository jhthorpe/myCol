#makefile for myQC

.PHONY: all

objects = fda.o

all: $(objects) 
	mv *.o $(obj)
	mv *.mod $(mod)

fda.o : fda.f90
	$(FC) $(FFLAGS) -c fda.f90

clean:
	rm -f *.o *.mod
