#makefile for myQC

.PHONY: all

objects = ado.o

all: $(objects) 
	mv *.o $(obj)
	mv *.mod $(mod)

ado.o : ado.f90
	$(FC) $(FFLAGS) -c ado.f90

clean:
	rm -f *.o *.mod
