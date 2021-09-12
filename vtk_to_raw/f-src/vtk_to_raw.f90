PROGRAM main
!>-------------------------------
!> vtk to raw converter
!>
!> Author:  Benjamin Schnabel, M.Sc.
!> GitHub:  https://github.com/JoGebert/3D_Convolusional_Filtering
!> Date:    10.09.2021
!> LastMod: 10.09.2021
!>-------------------------------


USE ISO_FORTRAN_ENV
USE file_routines_mpi  
USE strings
USE standards

IMPLICIT NONE

! For use with MPI (Calls), variables must be declared the same type like MPI was built.
! MPI: Kind=32 Bit / 4 Byte / ik=4 - Change in «working_directory/f-src/mod_standards.f90 

! Parameter
INTEGER  (KIND = ik), PARAMETER                                 :: debug       = 1
INTEGER  (KIND = ik), PARAMETER                                 :: fh_data_in  = 5  
INTEGER  (KIND = ik), PARAMETER                                 :: fh_vtk__out = 10   ! write vtk
INTEGER  (KIND = ik), PARAMETER                                 :: fh_meta_out = 15   ! write meta
INTEGER  (KIND = ik), PARAMETER                                 :: fh_raw__out = 20   ! write raw

! Internal Variables
CHARACTER(LEN = mcl)                                            :: filename, filenameExportVtk, typ
INTEGER  (KIND = ik)                                            :: ii, jj, kk, ll, mm, nn

CHARACTER(LEN = mcl)                                            :: vtk, basename
REAL     (KIND = rk)           , DIMENSION(3)                   :: dims, spcng, origin
INTEGER  (KIND = ik)           , DIMENSION(:,:,:), ALLOCATABLE  :: subarray, result_subarray     ! Dealt with internally as int32

! Read Input file
CHARACTER(len=mcl)                                              :: line, parameterfile, prefix
INTEGER  (KIND=ik)                                              :: io_status, ntokens
CHARACTER(len=mcl)                                              :: tokens(100)
CHARACTER(len=mcl)                                              :: tkns(100)
INTEGER  (KIND = ik)                                            :: wr_vtk_hdr_lngth

CALL GET_COMMAND_ARGUMENT(1, vtk)
CALL GET_COMMAND_ARGUMENT(2, new_basename)       

! Read VTK file header
CALL read_vtk_meta (    fh=fh_data_in     , &
                        filename=vtk      , &
                        dims=dims         , &
                        spcng=spcng       , &
                        origin=origin     , &
                        typ=typ           , &
                        displacement=0_ik , &
                        rd_o=rd_o         , &
                        status_o=status)

! CALL write_meta (   fh=fh_data_out                          , &
                !     filename=filenamHK1_15mu-1_201125_HK1_15mu-1_G3S61-Sig20_Filter_Histogram.texeExportVtk              , & 
                !     type=TRIM(typ)                          , &
                !     atStart=.TRUE.                          , &
                !     spcng=spcng                             , &
                !     origin=origin                           , &
                !     dims=sections*subarray_dims)

INQUIRE(FILE=filenameExportVtk, SIZE=wr_vtk_hdr_lngth)

CALL write_raw_mpi (    type=TRIM(typ)                          , &
                        hdr_lngth=INT(wr_vtk_hdr_lngth, KIND=8) , &
                        filename=filenameExportVtk              , &
                        dims=sections*subarray_dims             , &
                        subarray_dims=subarray_dims             , &
                        subarray_origin=subarray_origin         , &
                        subarray4=result_subarray) ! type was hardcoded in the end!

END PROGRAM main
