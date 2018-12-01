#makefile for myQC

.PHONY: all

objects = parser.o

all: $(objects) 
	mv *.o $(obj)
	mv *.mod $(mod)

parser.o : parser.f90
	$(FC) $(FFLAGS) -c parser.f90 -I$(mod) $(obj)/units.o

clean:
	rm -f *.o *.mod
