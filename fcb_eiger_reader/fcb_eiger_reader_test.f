      program fcb_eiger_reader_test
       use fcb_eiger_reader
       use, intrinsic :: iso_c_binding
       use hdf5
       implicit none

       interface
         subroutine cbf_register_filters()
     *     bind(C, name="cbf_register_filters")
         end subroutine cbf_register_filters
       end interface

       CHARACTER*6 data_number(500)
       integer h5blockstart
       integer(HID_T) hdf5file, h5entry, h5group
       integer(HID_T) h5data(500), h5dataspace(500)
       integer(HID_T) h5memspace(500)
       integer h5err, kblock, tblock
       integer h5nimages
       integer(HSIZE_T) h5datadims(3,500),
     *   h5datamaxdims(3,500)
       integer h5ndims, lastchar, lastdot
       integer*4 image_size, ix, iy
       integer*4, allocatable :: image(:)
       integer ii,jj,kk,itot
       integer*4 fastlow, fasthigh, slowlow, slowhigh

        call fcb_eiger_read_init (
     *  "series_1_master.h5",
     *  hdf5file,
     *  500, 5000, 1, 1,
     *  h5nimages, h5blockstart,
     *  h5entry, h5group,
     *  data_number,h5data,h5dataspace,h5memspace,
     *  h5datadims,h5datamaxdims,
     *  h5err)

        image_size = h5datadims(1,1)*h5datadims(2,1)
        ix = h5datadims(1,1)
        iy = h5datadims(2,1)
        fastlow = 10
        fasthigh = 20
        slowlow = 30
        slowhigh = 40
        allocate (image(image_size))
        call fcb_eiger_read_image_roi (1,
     *  hdf5file,
     *  ix,iy,
     *  fastlow, fasthigh, slowlow, slowhigh,
     *  image,
     *  500, 5000, h5nimages,h5blockstart,
     *  h5entry, h5group,
     *  data_number,h5data,h5dataspace,h5memspace,
     *  h5datadims,h5datamaxdims,
     *  h5err)

        itot = 0
        do ii = fastlow,fasthigh
        do jj = slowlow,slowhigh
          kk = ii+jj*h5datadims(1,1)
          itot = itot+image(kk)   
        enddo
        enddo     

        print *, itot+110

        call fcb_eiger_read_close(hdf5file,
     *  500, 5000, h5nimages,
     *  h5entry, h5group,
     *  data_number,h5data,h5dataspace,h5memspace,
     *  h5err)

        if (itot+110.ne.0) then
          print *,"incorrect sum in ROI"
          call exit(-1)
        endif
        stop

        end
