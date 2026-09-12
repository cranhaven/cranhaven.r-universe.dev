FFLAGS = -Wall
SRC_FILES = xgetua.f dlngam.f xersve.f j4save.f d1mach.f \
	d9lgit.f xgetf.f dgamlm.f i1mach.f dlgams.f d9gmic.f dgamm0.f \
	xsetf.f fdump.f dgami.f xerclr.f dcsevl.f \
	d9lgic.f dgamit.f d9lgmc.f initds.f xerprn.f xermsg.f \
	dgamr.f d9gmit.f dgamic.f


all: $(SRC_FILES)
	gfortran.exe $(FFLAGS) -c $(SRC_FILES)

.PHONY: clean
clean:
	rm -f *.o *.exe
