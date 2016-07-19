      module FCB_EIGER_READER
!
!     fcb_eiger_reader.f
!
!     Fortran routines to read Dectris Eiger data in hdf5 containers
!       Copyright 2016 Herbert J. Bernstein
!
!     Based in part on eiger2cbf
!       Copyright 2016 Takanori Nakane
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

      contains

      subroutine fcb_eiger_read_init (filename, h5file,
     *  maxblocks, maximages, first, images,
     *  h5nimages, h5blockstart,
     *  h5entry, h5group,
     *  h5_data_blocknumber,h5data,h5dataspace,h5memspace,
     *  h5datadims,h5datamaxdims,
     *  h5err)

      use, intrinsic :: iso_c_binding
      use h5lt
      use hdf5

      implicit none


      interface
        subroutine cbf_register_filters()
     *    bind(C, name="cbf_register_filters")
        end subroutine cbf_register_filters
      end interface

!     subroutine arguments
      character*(*), intent(in) :: filename
                             ! the name of the hdf5 master file
      integer(HID_T), intent(out) :: h5file
                             ! -1 or id of hdf5 file
      integer, intent(in) :: maxblocks
                             ! the maximum number of blocks for array sizes
      integer, intent(in) ::  maximages
                             ! the maximum number of images
      integer, intent(in) ::  first
                             ! the first image number
      integer, intent(in) ::  images
                             ! the number of images
      integer, intent(out) ::  h5nimages
                             ! 0 or the number of images in the file
      integer, intent(out) ::  h5blockstart
                             ! 0 or 1
      character*6, intent(out) :: h5_data_blocknumber(maxblocks)
                             ! array of nnnnnn string in data_nnnnnn names
      integer(HID_T), intent(out) :: h5entry
                             ! -1 or id of /entry
      integer(HID_T), intent(out) :: h5group
                             ! -1 or id of /entry/data group
      integer(HID_T), intent(out) :: h5data(maxblocks)
                             ! array of -1 or ids of data blocks
      integer(HID_T), intent(out) :: h5dataspace(maxblocks)
                             ! array of -1 of dataspces of data blocks
      integer(HID_T), intent(out) :: h5memspace(maxblocks)
                             ! array of -1 of mempaces of data blocks
      integer(HSIZE_T), intent(out) :: h5datadims(3,maxblocks)
                             ! array of 0's or image dimensions
      integer(HSIZE_T), intent(out) :: h5datamaxdims(3,maxblocks)
                             ! array of 0's or image maximum dimensions
      integer, intent(out) :: h5err
                             ! error return, -1 for errors
!     end of subroutine arguments

      integer(HSIZE_T) h5dims(1)
                             ! scratch array for ranks
      integer h5timages(1), h5ndims

      integer i,j,lastchar, lastdot, kblock, tblock
      integer blocklow, blockhigh
      character*5 fileext
      logical h5valid

!     initialize all return arguments

c     print *, "filename len: ", len_trim(filename)
c     print *, "filename: ",filename(1:len_trim(filename))
      h5file = -1
      h5nimages = 0
      h5entry = -1
      h5group = -1
      h5blockstart = 1
      do i = 1,maxblocks
        h5_data_blocknumber(i)=" "
        h5data(i) = -1
        h5dataspace(i) = -1
        h5memspace(i) = -1
        h5datadims(1,i) = 0
        h5datadims(2,i) = 0
        h5datadims(3,i) = 0
        h5datamaxdims(1,i) = 0
        h5datamaxdims(2,i) = 0
        h5datamaxdims(3,i) = 0
      enddo
      h5err = 0

!     find the last non-blank character in the filename
!     and then the dot before the extension
      lastchar = len_trim(filename)
      lastdot = lastchar+1
      if ( filename(lastchar-1:lastchar-1).eq.'.')
     *      lastdot = lastchar-1
      if (filename(lastchar-2:lastchar-2).eq.'.')
     *      lastdot = lastchar-2
      if (filename(lastchar-3:lastchar-3).eq.'.')
     *      lastdot = lastchar-3
      if (filename(lastchar-4:lastchar-4).eq.'.')
     *      lastdot = lastchar-4
      if (filename(lastchar-5:lastchar-5).eq.'.')
     *      lastdot = lastchar-5
      fileext = " "
      if (lastdot .lt. lastchar)
     *  fileext = filename(lastdot+1:lastchar)

      if (fileext.ne."h5"
     *  .and. fileext.ne."hdf5"
     *  .and. fileext.ne."nx"
     *  .and. fileext.ne."nexus") then
        h5err = -1
        return
       endif
!
!      we have a file name with a good extension, try to find
!      the data blocks
!


      call h5open_f(h5err)
      if(h5err.ge.0) call cbf_register_filters
      if(h5err.ge.0) call h5fopen_f(filename,H5F_ACC_RDONLY_F, h5file,
     *        H5P_DEFAULT_F)
      h5dims(1) = 1
      if(h5err.ge.0) call h5ltread_dataset_f(h5file,
     *        "/entry/instrument/detector/detectorSpecific/nimages",
     *        H5T_NATIVE_INTEGER,
     *        h5timages,h5dims,h5err)
      h5nimages = h5timages(1)
c     print *,"h5nimages: ", h5nimages
      if (h5err.ge.0) call h5gopen_f(h5file, "/entry", h5entry,
     *        h5err, H5P_DEFAULT_F)
      if (h5err.lt.0) return
      call h5gopen_f(h5file, "/entry/data",
     *        h5group,h5err,H5P_DEFAULT_F)
      if (h5err .lt. 0) then
          h5group = h5entry
          h5entry = -1
      endif
      if (h5group .lt. 0) then
        h5err = -1
        return
      endif
      if (h5ltfind_dataset_f(h5group,
     *          "data_000000").eq.1) then
        h5blockstart = 0
        h5_data_blocknumber(1) = "000000"
        call h5dopen_f(h5group,"data_000000",
     *            h5data(1),
     *            h5err,H5P_DEFAULT_F)
      else
        h5blockstart = 1
        h5_data_blocknumber(1) = "000001"
        call h5dopen_f(h5group,"data_000001",
     *            h5data(1),
     *            h5err,H5P_DEFAULT_F)
      endif
c     call h5iis_valid_f(h5data(1),h5valid, h5err)
c     print *,"h5iis_valid(h5data(1)):", h5valid
      call h5dget_space_f(h5data(1),h5dataspace(1),h5err)
      call h5iis_valid_f(h5dataspace(1),h5valid, h5err)
c     print *,"h5iis_valid(h5dataspace(1)):",h5valid
      call h5sget_simple_extent_ndims_f(h5dataspace(1),
     *          h5ndims, h5err)
c     print *, "h5ndims: ", h5ndims
      if (h5ndims .ne. 3) then
                write(0,*)" dimension of data_"//
     *            h5_data_blocknumber(1)//" is not 3"
                h5err = -1
                return
      endif
      call h5sget_simple_extent_dims_f(h5dataspace(1),
     *          h5datadims(:,1), h5datamaxdims(:,1), h5err)
c     print *, "h5datadims(:1): ", h5datadims(1,1),
c    * h5datadims(2,1), h5datadims(3,1)
      call h5screate_simple_f(3, h5datadims(:,1),
     * h5memspace(1),h5err, h5datamaxdims(:,1))
      if (h5err .lt.0) then
        write(0,*) "Error:  memspace for ", 1,
     *  " not created "
        h5err = -1
        return
      endif

        blocklow = h5blockstart+(first-1)/h5datadims(3,1)
        blockhigh = h5blockstart+(first
     *    +images-2)/h5datadims(3,1)
        if (blocklow.eq.h5blockstart) blocklow=blocklow+1
        if (blocklow.gt.blockhigh) return
        do kblock = blocklow, blockhigh
        tblock = kblock
        do j = 1,6
          h5_data_blocknumber(kblock-h5blockstart+1)(7-j:7-j)
     *            = char(ichar('0')+mod(tblock,10))
          tblock = tblock/10
        enddo
        call h5dopen_f(h5group,"data_"
     *    //h5_data_blocknumber(kblock-h5blockstart+1),
     *    h5data(kblock-h5blockstart+1),
     *              h5err,H5P_DEFAULT_F)
        if (h5err.ge.0)  then
            call h5dget_space_f(h5data(kblock-h5blockstart+1),
     *      h5dataspace(kblock-h5blockstart+1),h5err)
            if (h5err .lt. 0)
     *        print *, "failed to get dataspace ",
     *        kblock-h5blockstart+1
            call h5sget_simple_extent_ndims_f(
     *        h5dataspace(kblock-h5blockstart+1),
     *        h5ndims, h5err)
          if (h5ndims .ne. 3) then
                write(0,*)" dimension of data_"//
     *            h5_data_blocknumber(kblock-h5blockstart+1)
     *            //" is not 3"
                h5err = -1
                return
          endif
        else
          print *, "Failed to open data_"
     *    //h5_data_blocknumber(kblock-h5blockstart+1)
        endif
        call h5sget_simple_extent_dims_f(
     *      h5dataspace(kblock-h5blockstart+1),
     *      h5datadims(:,kblock-h5blockstart+1),
     *      h5datamaxdims(:,kblock-h5blockstart+1), h5err)
        call h5screate_simple_f(3,
     *    h5datadims(:,kblock-h5blockstart+1),
     *    h5memspace(kblock-h5blockstart+1),
     *    h5err,
     *    h5datamaxdims(:,kblock-h5blockstart+1))
        if (h5err .lt.0) then
          write(0,*) "Error:  memspace for ",
     *      kblock-h5blockstart+1,
     *      " not created "
          h5err = -1
        return
      endif

      enddo
      return
      end subroutine fcb_eiger_read_init

      subroutine fcb_eiger_read_image_roi (numb,
     *  h5file,
     *  ix,iy,fastlow, fasthigh, slowlow, slowhigh,
     *  image,
     *  maxblocks, maximages, h5nimages,
     *  h5blockstart,
     *  h5entry, h5group,
     *  h5_data_blocknumber,h5data,
     *  h5dataspace,h5memspace,
     *  h5datadims, h5datamaxdims,
     *  h5err)

      use h5lt
      use hdf5

      implicit none

!     subroutine arguments
      integer, intent(in) :: numb
                             ! number of the image (counting from 1)
      integer(HID_T), intent(out) :: h5file
                             ! -1 or id of hdf5 file
      integer*4, intent(in) :: ix
                             ! the number of pixels in the fast dimension
      integer*4, intent(in) :: iy
                             ! the number of pixels in the slow dimension
      integer*4, intent(inout) :: fastlow
                             ! roi fast low pixel number (from 1)
      integer*4, intent(inout) :: fasthigh
                             ! roi fast high pixel number (from 1)
      integer*4, intent(inout) :: slowlow
                             ! roi slow low pixel number (from 1)
      integer*4, intent(inout) :: slowhigh
                             ! roi slow high pixel number (from 1)
      integer*4, intent(out) :: image(ix*iy)
                             ! the image read
      integer, intent(in) ::  maxblocks
                             ! the maximum number of blocks for array sizes
      integer, intent(in) ::  maximages
                             ! the maximum number of images
      integer, intent(in) ::  h5nimages
                             ! 0 or the number of images in the file
      integer, intent(in) ::  h5blockstart
                             ! 0 or 1
      character*6, intent(in) ::
     *    h5_data_blocknumber(maxblocks)
                             ! array of nnnnnn string in data_nnnnnn names
      integer(HID_T), intent(in) :: h5entry
                             ! -1 or id of /entry
      integer(HID_T), intent(in) :: h5group
                             ! -1 or id of /entry/data group
      integer(HID_T), intent(in) :: h5data(maxblocks)
                             ! array of -1 or ids of data blocks
      integer(HID_T), intent(in) :: h5dataspace(maxblocks)
                             ! array of -1 of dataspaces of data blocks
      integer(HID_T), intent(in) :: h5memspace(maxblocks)
                             ! array of -1 of memspaces of data blocks
      integer(HSIZE_T), intent(in) :: h5datadims(3,maxblocks)
                             ! array of 0's or image dimensions
      integer(HSIZE_T), intent(in) :: h5datamaxdims(3,maxblocks)
                             ! array of 0's or image maximum dimensions
      integer, intent(out) :: h5err
                             ! error return, -1 for errors
!     end of subroutine arguments

      integer blocknum, fib, h5ndims
      integer(HSIZE_T) file_offset(3)
      integer(HSIZE_T) mem_offset(3)
      integer(HSIZE_T) xfer_count(3)
      integer(HSIZE_T) xfer_stride(3)
      integer(HSIZE_T) xfer_block(3)

      logical h5valid


      if (numb .gt. h5nimages) then
        write(0,*) "Warning:  frame ", numb,
     *  " is greater than nimages ",
     *  h5nimages
      endif

      if (fastlow .lt. 1) fastlow=1
      if (fastlow .gt. ix) fastlow=ix
      if (fasthigh .lt. fastlow) fasthigh = fastlow
      if (fasthigh .gt. ix) fasthigh = ix
      if (slowlow .lt. 1) slowlow=1
      if (slowlow .gt. iy) slowlow=iy
      if (slowhigh .lt. slowlow) slowhigh = slowlow
      if (slowhigh .gt. iy) slowhigh = iy


      blocknum = 1+(numb-1)/h5datadims(3,1)
c     print *, "numb, blocknum:", numb, blocknum
      file_offset(3) = mod(numb-1,h5datadims(3,1))
      file_offset(2) = slowlow-1
      file_offset(1) = fastlow-1
      mem_offset(3) = 0
      mem_offset(2) = slowlow-1
      mem_offset(1) = fastlow-1
      xfer_count(3) = 1
      xfer_count(2) = slowhigh-slowlow+1
      xfer_count(1) = fasthigh-fastlow+1
      xfer_stride(3) = 1
      xfer_stride(2) = 1
      xfer_stride(1) = 1
      xfer_block(3) = 1
      xfer_block(2) = 1
      xfer_block(1) = 1

      call h5iis_valid_f(h5data(blocknum),h5valid,h5err)
      if ((.not.h5valid).or.h5err .lt.0) then
        write(0,*) "Warning:  dataset ", blocknum,
     *    " is not available "
        h5err = -1
        return
      endif
      call h5iis_valid_f(h5dataspace(blocknum),h5valid,h5err)
      if ((.not.h5valid).or.h5err .lt.0) then
        write(0,*) "Warning:  dataspace ", blocknum,
     *    " is not available "
        h5err = -1
        return
      endif

      call h5sselect_hyperslab_f(h5memspace(blocknum),
     * H5S_SELECT_SET_F, mem_offset, xfer_count,
     * h5err, xfer_stride, xfer_block)

      if (h5err .lt.0) then
        write(0,*) "Error:  memory hyperslab for ", blocknum,
     *  " not selected "
        h5err = -1
        return
      endif

      call h5sselect_hyperslab_f(h5dataspace(blocknum),
     * H5S_SELECT_SET_F, file_offset, xfer_count,
     * h5err, xfer_stride, xfer_block)

      if (h5err .lt.0) then
        write(0,*) "Error:  file hyperslab for ", blocknum,
     *  " not selected "
        h5err = -1
        return
      endif

      call h5dread_f(h5data(blocknum),
     *  h5kind_to_type(SELECTED_INT_KIND(9),H5_INTEGER_KIND),
     *  image,
     *  xfer_count, h5err, h5memspace(blocknum),
     *  h5dataspace(blocknum),
     *  H5P_DEFAULT_F)

      return

      end subroutine fcb_eiger_read_image_roi


      subroutine fcb_eiger_read_image (numb,
     *  h5file,ix,iy,image,
     *  maxblocks, maximages, h5nimages,
     *  h5blockstart,
     *  h5entry, h5group,
     *  h5_data_blocknumber,h5data,
     *  h5dataspace,h5memspace,
     *  h5datadims, h5datamaxdims,
     *  h5err)

      use h5lt
      use hdf5

      implicit none


!     subroutine arguments
      integer, intent(in) :: numb
                             ! number of the image (counting from 1)
      integer(HID_T), intent(out) :: h5file
                             ! -1 or id of hdf5 file
      integer*4, intent(in) :: ix
                             ! the number of pixels in the fast dimension
      integer*4, intent(in) :: iy
                             ! the number of pixels in the slow dimension
      integer*4, intent(out) :: image(ix*iy)
                             ! the image read
      integer, intent(in) :: maxblocks
                             ! the maximum number of blocks for array sizes
      integer, intent(in) ::  maximages
                             ! the maximum number of images
      integer, intent(in) ::  h5nimages
                             ! 0 or the number of images in the file
      integer, intent(in) ::  h5blockstart
                             ! 0 or 1
      character*6, intent(in) ::
     *    h5_data_blocknumber(maxblocks)
                             ! array of nnnnnn string in data_nnnnnn names
      integer(HID_T), intent(in) :: h5entry
                             ! -1 or id of /entry
      integer(HID_T), intent(in) :: h5group
                             ! -1 or id of /entry/data group
      integer(HID_T), intent(in) :: h5data(maxblocks)
                             ! array of -1 or ids of data blocks
      integer(HID_T), intent(in) :: h5dataspace(maxblocks)
                             ! array of -1 of dataspaces of data blocks
      integer(HID_T), intent(in) :: h5memspace(maxblocks)
                             ! array of -1 of memspaces of data blocks
      integer(HSIZE_T), intent(in) :: h5datadims(3,maxblocks)
                             ! array of 0's or image dimensions
      integer(HSIZE_T), intent(in) :: h5datamaxdims(3,maxblocks)
                             ! array of 0's or image maximum dimensions
      integer, intent(out) :: h5err
                             ! error return, -1 for errors
!     end of subroutine arguments

      integer*4 fastlow, fasthigh, slowlow, slowhigh

      fastlow = 1
      fasthigh = ix
      slowlow = 1
      slowhigh = iy

      call fcb_eiger_read_image_roi (numb,
     *  h5file,
     *  ix,iy,fastlow, fasthigh, slowlow, slowhigh,
     *  image,
     *  maxblocks, maximages, h5nimages,
     *  h5blockstart,
     *  h5entry, h5group,
     *  h5_data_blocknumber,h5data,
     *  h5dataspace,h5memspace,
     *  h5datadims, h5datamaxdims,
     *  h5err)

      return

      end subroutine fcb_eiger_read_image



      subroutine fcb_eiger_read_close (h5file,
     *  maxblocks, maximages, h5nimages,
     *  h5entry, h5group,
     *  h5_data_blocknumber,h5data,h5dataspace,h5memspace,
     *  h5err)

      use h5lt
      use hdf5


      implicit none

!     subroutine arguments
      integer(HID_T), intent(inout) :: h5file
                             ! -1 or id of hdf5 file
      integer, intent(in) :: maxblocks
                             ! the maximum number of blocks for array sizes
      integer, intent(in) :: maximages
                             ! the maximum number of images
      integer, intent(in) :: h5nimages
                             ! 0 or the number of images in the file
      character*6, intent(out) ::
     *  h5_data_blocknumber(maxblocks)
                             ! array of nnnnnn string in data_nnnnnn names
      integer(HID_T), intent(inout) :: h5entry
                             ! -1 or id of /entry
      integer(HID_T), intent(inout) :: h5group
                             ! -1 or id of /entry/data group
      integer(HID_T), intent(inout) :: h5data(maxblocks)
                             ! array of -1 or ids of data blocks
      integer(HID_T), intent(inout) :: h5dataspace(maxblocks)
                             ! array of -1 of dataspaces of data blocks
      integer(HID_T), intent(inout) :: h5memspace(maxblocks)
                             ! array of -1 of memspaces of data blocks

      integer, intent(inout) :: h5err
                             ! error return, -1 for errors
!     end of subroutine arguments

      integer i,j
      logical h5valid

      do i = 1, maxblocks
        call h5iis_valid_f(h5memspace(i),h5valid,h5err)
        if (h5valid .and. h5err .ne.-1)
     *    call h5sclose_f(h5memspace(i),h5err)
        call h5iis_valid_f(h5dataspace(i),h5valid,h5err)
        if (h5valid .and. h5err .ne.-1)
     *    call h5sclose_f(h5dataspace(i),h5err)
        call h5iis_valid_f(h5data(i),h5valid,h5err)
        if (h5valid .and. h5err .ne.-1)
     *    call h5dclose_f(h5data(i),h5err)
        h5dataspace(i) = -1
        h5data(i) = -1
       enddo

      call h5iis_valid_f(h5group,h5valid,h5err)
      if (h5valid .and. h5err .ne.-1)
     *    call h5gclose_f(h5group,h5err)
      call h5iis_valid_f(h5entry,h5valid,h5err)
      if (h5valid .and. h5err .ne.-1)
     *    call h5gclose_f(h5entry,h5err)
      h5group = -1
      h5entry = -1
      call h5iis_valid_f(h5file,h5valid,h5err)
      if (h5valid .and. h5err .ne.-1)
     *    call h5fclose_f(h5file,h5err)
      h5group = -1
      h5entry = -1

      end subroutine fcb_eiger_read_close
      end module FCB_EIGER_READER
