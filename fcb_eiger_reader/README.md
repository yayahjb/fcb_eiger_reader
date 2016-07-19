FCB_EIGER_READER

Code to read HDF5 Eiger images in Fortran

Herbert J. Bernstein, 18 July 2016
yayahjb@gmail.com

Much of FCB_EIGER_READER is derived from
  eiger2cbf Copyright 2016 Takanori Nakane
FCB_EIGER_READER and eiger2cbf are being incorporated into
CBFlib and other open source packages.  The
following license applies to the FCB_EIGER_READER code.

Note that the operating system exception
applies to all CBFlib components, including FCB_EIGER_READER, so
there is no impact of the following FCB_EIGER_READER license
on the licensing applications that link to this code.

!======================EIGER2CBF LICENSE =============================
! eiger2cbf is licensed under the "BSD license", referencing the
! versions of the BSD license in bishuffle and lz4.  The BSD license
! version in bitshuffle says:
!
! Permission is hereby granted, free of charge, to any person obtaining
! a copy of this software and associated documentation files (the
! "Software"), to deal in the Software without restriction, including
! without limitation the rights to use, copy, modify, merge, publish,
! distribute, sublicense, and/or sell copies of the Software, and to
! permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be
! included in all copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
! EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
! OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
! NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
! BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
! AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR
! IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
! THE SOFTWARE.
!
!=====================================================================
!================== END OF EIGER2CBF LICENSE =========================
!
!===================FCB_EIGER_READER_LICENSE =========================
!*********************************************************************
!                                                                    *
! YOU MAY REDISTRIBUTE THE FCB_EIGER PACKAGE UNDER THE TERMS OF THE  *
! LGPL IN ADDITION TO THE EIGER2CBF BSD LICENSE                      *
!*********************************************************************

!************************* LGPL NOTICES ******************************
!                                                                    *
! This library is free software; you can redistribute it and/or      *
! modify it under the terms of the GNU Lesser General Public         *
! License as published by the Free Software Foundation; either       *
! version 2.1 of the License, or (at your option) any later version. *
!                                                                    *
! This library is distributed in the hope that it will be useful,    *
! but WITHOUT ANY WARRANTY; without even the implied warranty of     *
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU  *
! Lesser General Public License for more details.                    *
!                                                                    *
! You should have received a copy of the GNU Lesser General Public   *
! License along with this library; if not, write to the Free         *
! Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston,    *
! MA  02110-1301  USA                                                *
!                                                                    *
!*********************************************************************
!===============END OF FCB_EIGER_READER_LICENSE ======================


INSTALLATION

To install this kit, you first need to have installed a complete HDF5
kit with fortran support, and to have installed the bitshuffle and lz4
filters into a static library, and you need to define the environment
variable $PREFIX, so that the following paths are populated:

  $PREFIX/include     (for the hdf5 fortran modules and headers)
  $PREFIX/lib/libcbf.a  (with static copies of the bitshuffle
                        and LZ4 filters)
  $PREFIX/lib/libhdf5hl_fortran.a 
  $PREFIX/lib/libhdf5_fortran.a 
  $PREFIX/lib/libhdf5_hl.a 
  $PREFIX/lib/libhdf5.a

We have prepared the latest github version of CBFlib to do those installs.
That kit is at https://github.com/yayahjb/cbflib

Then

make all

should build FCB_EIGER_READER and the test program fcb_eiger_reader_test

then

make tests

will test the read logic.

make archive 

will make a tarball of the kit

Finally

make install will install libfcb_eiger_reader.a in $PREFIX/lib and


