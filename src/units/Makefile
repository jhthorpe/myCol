.PHONY: all

objects = units.o

all: $(objects) 
	mv *.o $(obj)
	mv *.mod $(mod)

units.o : units.f90
	$(FC) $(FFLAGS) -c units.f90

clean:
	rm -f *.o *.mod
