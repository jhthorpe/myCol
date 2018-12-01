objects = myCol.o

all: $(objects)
	cp myCol.o $(bindir)/myCol

myCol.o : myCol.f90
	$(FC) $(FFLAGS) myCol.f90 -o myCol.o -I$(mod) $(obj)/*.o

clean:
	rm -f *.o *.mod
