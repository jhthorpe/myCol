#makefile for myQC

.PHONY: all

objects = lda.o

all: $(objects) 
	mv *.o $(obj)
	mv *.mod $(mod)

lda.o : lda.f90
	$(FC) $(FFLAGS) -c lda.f90

clean:
	rm -f *.o *.mod
