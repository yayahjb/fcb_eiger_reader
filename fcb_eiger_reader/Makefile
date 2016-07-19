
build_date	:=	$(shell date +%Y%m%d)
os	:= $(shell uname -s)
machine	:= $(shell uname -m)
version	= 1.0.0
PREFIX ?= /usr/local

FC 	=  gfortran 
CC	=  gcc
FCFLAGS = -g -finit-local-zero -O3  -Wall -w -fcray-pointer  -I$(PREFIX)/include
CFLAGS = -g -O3 -I$(PREFIX)/include

LDFLAGS =  -static-libgfortran  $(PREFIX)/lib/libcbf.a  $(PREFIX)/lib/libhdf5hl_fortran.a \
	$(PREFIX)/lib/libhdf5_fortran.a $(PREFIX)/lib/libhdf5_hl.a $(PREFIX)/lib/libhdf5.a -lz -ldl

FCBERTEST = fcb_eiger_reader_test

##
##
##

OBJSfcber   = fcb_eiger_reader_test.o fcb_eiger_reader.o cbf_register_filters.o


$(FCBERTEST) : $(OBJSfcber)
	$(FC) -o $(FCBERTEST) $(OBJSfcber) $(LDFLAGS) 

$(FCBERTEST).o: $(FCBERTEST).f fcb_eiger_reader.mod
	$(FC) $(FCFLAGS) -c $(FCBERTEST).f

fcb_eiger_reader.mod: fcb_eiger_reader.o

fcb_eiger_reader.o: fcb_eiger_reader.f
	$(FC) $(FCFLAGS) -c fcb_eiger_reader.f

cbf_register_filters.o: cbf_register_filters.c
	$(CC) $(CFLAGS) -c cbf_register_filters.c

.f.o: 
	$(FC) $(FCFLAGS) -c $<

.c.o:
	$(CC) $(INCL) -c $<

all : $(FCBERTEST)

archive : 
	tar cvfz fcb_eiger_reader${build_date}.tar.gz  README.md *.f *.c *.h5

clean :
	@-rm -f *.o *.mod

distclean : clean
	@-rm $(FCBERTEST)

install : fcb_eiger_reader.o fcb_eiger_reader.mod cbf_register_filters.o
	cp fcb_eiger_reader.mod $(PREFIX)/include
	chmod 644 $(PREFIX)/include/fcb_eiger_reader.mod
	ar cr $(PREFIX)/lib/libfcb_eiger_reader.a fcb_eiger_reader.o cbf_register_filters.o

tests :	series_1_master.h5 series_1_data_000001.h5 $(FCBERTEST)
	./$(FCBERTEST)
