FFTW =  -lfftw3 -L/Users/earpwl/software/fftw3.3.3/lib
FORTRAN = ifort  
#FORTRAN = gfortran -ffree-line-length-none -I. 
LIBS = -lfftw3 -L/Users/earpwl/software/fftw3.3.3/lib -L/opt/intel/composerxe/mkl/lib -lmkl_intel_lp64 -lmkl_intel_thread -lmkl_core -liomp5 -lpthread -lm
#LIBS = -lfftw3 -L/Users/earpwl/software/fftw3.3.3/lib -L/opt/intel/composerxe/mkl/lib -lmkl_intel_lp64
subs.o : subs.F90 get_integral.o
	$(FORTRAN) -c  subs.F90

	
subs_splines.o : subs_splines.F90
	$(FORTRAN) -c subs_splines.F90

get_integral.o : get_integral.f
	$(FORTRAN)-c get_integral.f

Siberian_jet_time_evolution :  Siberian_jet_time_evolution.F90 subs.o get_integral.o subs_splines.o
	$(FORTRAN) -o Siberian_jet_time_evolution Siberian_jet_time_evolution.F90 subs.o get_integral.o subs_splines.o $(LIBS) 

