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
CHARACTER(LEN = mcl)                                            :: vtk, new_basename, typ
CHARACTER(LEN = mcl)                                            :: filename_raw, filename_meta
INTEGER  (KIND = ik)                                            :: hdr_lngth
INTEGER  (KIND = ik)           , DIMENSION(3)                   :: dims
REAL     (KIND = rk)           , DIMENSION(3)                   :: spcng, origin
REAL     (KIND = REAL32)       , DIMENSION(:,:,:), ALLOCATABLE  :: rryreal4
REAL     (KIND = REAL64)       , DIMENSION(:,:,:), ALLOCATABLE  :: rryreal8
INTEGER  (KIND = INT16)        , DIMENSION(:,:,:), ALLOCATABLE  :: rryint2
INTEGER  (KIND = INT32)        , DIMENSION(:,:,:), ALLOCATABLE  :: rryint4


! Read Input file
CHARACTER(len=mcl)                                              :: line
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
                        origin=origin     , &
                        spcng=spcng       , &
                        typ=typ)

! CALL write_meta (   fh=fh_data_out                          , &
                !     filename=filenamHK1_15mu-1_201125_HK1_15mu-1_G3S61-Sig20_Filter_Histogram.texeExportVtk              , & 
                !     type=TRIM(typ)                          , &
                !     atStart=.TRUE.                          , &
                !     spcng=spcng                             , &
                !     origin=origin                           , &
                !     dims=sections*subarray_dims)

filename_raw  = TRIM(new_basename)//'.raw'
filename_meta = TRIM(new_basename)//'.meta'

INQUIRE(FILE=vtk, SIZE=hdr_lngth)

OPEN (UNIT=fh_raw__out, FILE=TRIM(filename_raw), ACCESS="stream", FORM="unformatted", STATUS="new")
IF (TRIM(typ) .EQ. 'real4') WRITE(UNIT=fh_raw__out) rryreal4(:,:,:)
IF (TRIM(typ) .EQ. 'real8') WRITE(UNIT=fh_raw__out) rryreal8(:,:,:)
IF (TRIM(typ) .EQ. 'int2')  WRITE(UNIT=fh_raw__out) rryint2 (:,:,:)
IF (TRIM(typ) .EQ. 'int4')  WRITE(UNIT=fh_raw__out) rryint4 (:,:,:)
CLOSE(UNIT=fh_raw__out)

END PROGRAM main
