PROGRAM vtk_to_raw
!>-------------------------------------------------------------------
!> vtk to raw converter
!>
!> Author:  Johannes Gebert - HLRS - NUM - »gebert@hlrs.de«
!> Date:    12.09.2021
!> LastMod: 14.09.2021
!>-------------------------------------------------------------------

USE ISO_FORTRAN_ENV
USE file_routines_mpi  
USE strings
USE standards

IMPLICIT NONE

! For use with MPI (Calls), variables must be declared the same type like MPI was built.
! MPI: Kind=32 Bit / 4 Byte / ik=4 - Change in «working_directory/f-src/mod_standards.f90 

! Parameter
INTEGER  (KIND = ik), PARAMETER                                 :: debug       = 1
INTEGER  (KIND = ik), PARAMETER                                 :: dh_di  = 5  
INTEGER  (KIND = ik), PARAMETER                                 :: fh_vo = 10   ! write vtk
INTEGER  (KIND = ik), PARAMETER                                 :: fh_mo = 15   ! write meta
INTEGER  (KIND = ik), PARAMETER                                 :: fh_ro = 20   ! write raw

! Internal Variables
CHARACTER(LEN = mcl)                                            :: vtk, new_basename, typ
CHARACTER(LEN = mcl)                                            :: filename_raw, filename_meta
INTEGER  (KIND = ik)                                            :: hdr
INTEGER  (KIND = ik)           , DIMENSION(3)                   :: dims
REAL     (KIND = rk)           , DIMENSION(3)                   :: spcng, origin
REAL     (KIND = REAL32)       , DIMENSION(:,:,:), ALLOCATABLE  :: rryrk4
REAL     (KIND = REAL64)       , DIMENSION(:,:,:), ALLOCATABLE  :: rryrk8
INTEGER  (KIND = INT16)        , DIMENSION(:,:,:), ALLOCATABLE  :: rryik2
INTEGER  (KIND = INT32)        , DIMENSION(:,:,:), ALLOCATABLE  :: rryik4


! Read Input file
CHARACTER(len=mcl)                                              :: line
INTEGER  (KIND=ik)                                              :: io_status, ntokens
CHARACTER(len=mcl)                                              :: tokens(100)
CHARACTER(len=mcl)                                              :: tkns(100)
INTEGER  (KIND = ik)                                            :: wr_vtk_hdr

CALL GET_COMMAND_ARGUMENT(1, vtk)
CALL GET_COMMAND_ARGUMENT(2, new_basename)       


! Read VTK file header
CALL read_vtk_meta (fh=dh_di      , &
                    filename=vtk  , &
                    hdr_lngth=hdr , &
                    dims=dims     , &
                    origin=origin , &
                    spcng=spcng   , &
                    typ=typ)

! Read binary data
OPEN(UNIT=dh_di, FILE=TRIM(vtk), ACCESS="stream", FORM="unformatted", STATUS="old")

SELECT CASE(TRIM(typ))
    CASE ('rk4'); ALLOCATE(rryrk4(dims(1),dims(2),dims(3))); READ(UNIT=dh_di, POS=hdr) rryrk4(:,:,:)
    CASE ('rk8'); ALLOCATE(rryrk8(dims(1),dims(2),dims(3))); READ(UNIT=dh_di, POS=hdr) rryrk8(:,:,:)
    CASE ('ik4'); ALLOCATE(rryik4(dims(1),dims(2),dims(3))); READ(UNIT=dh_di, POS=hdr) rryik4(:,:,:)
    CASE DEFAULT; ALLOCATE(rryik2(dims(1),dims(2),dims(3))); READ(UNIT=dh_di, POS=hdr) rryik2(:,:,:)
END SELECT

CLOSE(UNIT=fh_ro)

! CALL write_meta_general ()

filename_raw  = TRIM(new_basename)//'.raw'
filename_meta = TRIM(new_basename)//'.meta'

OPEN (UNIT=fh_ro, FILE=TRIM(filename_raw), ACCESS="stream", FORM="unformatted", STATUS="new")

SELECT CASE(TRIM(typ))
    CASE ('rk4'); WRITE(UNIT=fh_ro) rryrk4(:,:,:); DEALLOCATE(rryrk4)
    CASE ('rk8'); WRITE(UNIT=fh_ro) rryrk8(:,:,:); DEALLOCATE(rryrk8)
    CASE ('ik4'); WRITE(UNIT=fh_ro) rryik4(:,:,:); DEALLOCATE(rryik4)
    CASE DEFAULT; WRITE(UNIT=fh_ro) rryik2(:,:,:); DEALLOCATE(rryik2)
END SELECT

CLOSE(UNIT=fh_ro)

WRITE(*, '(A )') "------------------------------------------------"
WRITE(*, '(2A)') "Input  *.vtk:  ", TRIM(vtk)
WRITE(*, '(2A)') "Output *.raw:  ", TRIM(filename_raw)
WRITE(*, '(2A)') "Output *.meta: ", TRIM(filename_meta)
WRITE(*, '(A )') 
WRITE(*, '(A )') "Program finished succesfully."
WRITE(*, '(A )') "------------------------------------------------"

END PROGRAM vtk_to_raw
