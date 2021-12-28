PROGRAM xtometa
!>-------------------------------------------------------------------
!> vtk to raw converter
!
!> @author Johannes Gebert - HLRS - NUM - gebert@hlrs.de
!> Date:    12.09.2021
!> LastMod: 28.12.2021
!>-------------------------------------------------------------------

USE ISO_FORTRAN_ENV
USE global_std
USE messages_errors
USE meta
USE MPI
USE file_routines_mpi  

IMPLICIT NONE

! MPI variables
INTEGER(KIND=mpi_ik) :: ierr, rank_mpi, size_mpi

! Std variables
CHARACTER(LEN=mcl) :: vtk, new_basename, typ
CHARACTER(LEN=mcl) :: filename='', filename_meta
INTEGER(KIND=ik)   :: hdr

INTEGER(KIND=ik), DIMENSION(3) :: dims
REAL(KIND=rk)   , DIMENSION(3) :: spcng, origin

REAL(KIND=rk) :: start, end

! Binary blob variables
REAL   (KIND=REAL32), DIMENSION(:,:,:), ALLOCATABLE :: rryrk4
REAL   (KIND=REAL64), DIMENSION(:,:,:), ALLOCATABLE :: rryrk8
INTEGER(KIND=INT16) , DIMENSION(:,:,:), ALLOCATABLE :: rryik2
INTEGER(KIND=INT32) , DIMENSION(:,:,:), ALLOCATABLE :: rryik4

!------------------------------------------------------------------------------
! Invoke MPI 
!------------------------------------------------------------------------------
CALL mpi_init(ierr)
CALL print_err_stop(std_out, "MPI_INIT didn't succeed", INT(ierr, KIND=ik))

CALL MPI_COMM_RANK(MPI_COMM_WORLD, rank_mpi, ierr)
CALL print_err_stop(std_out, "MPI_COMM_RANK couldn't be retrieved", INT(ierr, KIND=ik))

CALL MPI_COMM_SIZE(MPI_COMM_WORLD, size_mpi, ierr)
CALL print_err_stop(std_out, "MPI_COMM_SIZE couldn't be retrieved", INT(ierr, KIND=ik))

If (size_mpi < 2) CALL print_err_stop(std_out, "We need at least 2 MPI processes to execute this program.", 1)

!------------------------------------------------------------------------------
! Start actual program
!------------------------------------------------------------------------------
CALL show_title(longname, revision)

CALL GET_COMMAND_ARGUMENT(1, filename)

IF (filename='') CALL print_err_stop(std_out, "No input file given", 1)

CALL meta_create_new(TRIM(ADJUSTL(filename)))

!------------------------------------------------------------------------------
! Read VTK file header
!------------------------------------------------------------------------------
CALL read_vtk_meta (dh_di, vtk, hdr, dims, origin, spcng, type) 

!------------------------------------------------------------------------------
! Write meta data 
!------------------------------------------------------------------------------
WRITE(fhmeo, '(A)') "# HLRS|NUM Dataset Meta Information"
WRITE(fhmeo, '(A)') ""
WRITE(fhmeo, '(A)') "* GENERAL_INFORMATION"
CALL meta_write (fhmeo, 'CT_SCAN'          , in%dataset)
CALL meta_write (fhmeo, 'OWNER',             "TBD by user")
CALL meta_write (fhmeo, 'DATE_META_CREATED', "TBD by user")
CALL meta_write (fhmeo, 'OWNER_CONTACT',     "TBD by user")
CALL meta_write (fhmeo, 'DATE_CREATED',      "TBD by user")
CALL meta_write (fhmeo, 'INTERNAL_ID',       "TBD by user")
CALL meta_write (fhmeo, 'HISTORY'          , '(-)' , 1)
CALL meta_write (fhmeo, 'TYPE'             , TRIM(ADJUSTL(type)))
CALL meta_write (fhmeo, 'DATA_BYTE_ORDER'  , "LittleEndian")
CALL meta_write (fhmeo, 'DIMENSIONALITY'   , '(-)'  , 3)
CALL meta_write (fhmeo, 'DIMENSIONS'       , '(-)'  , dims)
CALL meta_write (fhmeo, 'NO_SCALAR_CMPNNTS', '(-)'  , 1)
CALL meta_write (fhmeo, 'SPACING'          , '(mm)' , spcng)
CALL meta_write (fhmeo, 'ORIGIN_SHIFT_GLBL', '(mm)' , [0., 0., 0.])
CALL meta_write (fhmeo, 'ORIGIN'           , '(-)'  , [0, 0, 0])
CALL meta_write (fhmeo, 'FIELD_OF_VIEW'    , '(mm)' , dims*spcng)
CALL meta_write (fhmeo, 'ENTRIES'          , '(-)'  , PRODUCT(dims))

! # HLRS|NUM Dataset Batch Record
! 
! * GENERAL_INFORMATION
! * CT_SCAN               FH01s3                                       ! HLRS internal nomenclature
! * OWNER                 Johannes_Gebert
! * OWNER_CONTACT         gebert@hlrs.de
! * DATE_CREATED          04.09.2021                                   
! * INTERNAL_ID           Testszenario                                 ! Briefly encode what was studied
! 
! * HISTORY               1                                            ! Number of consecutive images within binary data
! * TYPE                  uint2                                        ! Data type of the binary image
! * DATA_BYTE_ORDER       BigEndian                                    ! BigEndian, LittleEndian
! * DIMENSIONALITY        3                                            ! Dimensions within the image
! * DIMENSIONS            2940 2940 2141                               ! (Voxels)
! * NO_SCALAR_CMPNNTS     1                                            ! Number of components per scalar (paraview requirement)
! * SPACING               1.000000       1.000000       1.000000       ! (mm)
! * ORIGIN_SHIFT_GLBL     3.575640       2.863500       2.584620       ! (mm)
! * ORIGIN                0              0              0              ! (-)
! * FIELD_OF_VIEW        43.953000      43.953000      32.007950       ! (mm)
! * ENTRIES               18505947600                                  ! Typically the amount of voxels


!------------------------------------------------------------------------------
! Read binary part of the vtk file
! Open Vtk as big endian (!) stream
!------------------------------------------------------------------------------
CALL mpi_read_raw_prepare(filename, hdr, dims, subarray_dims, subarray_origin)

ALLOCATE(subarray(subarray_dims(1), subarray_dims(2), subarray_dims(3)))

CALL MPI_FILE_READ_ALL(fh, subarray, SIZE(subarray), MPI_INTEGER, MPI_STATUS_IGNORE, ierr)

CALL mpi_read_raw_release(fh, type_subarray)


!------------------------------------------------------------------------------
! Write raw data
!------------------------------------------------------------------------------
OPEN (UNIT=fh_ro, FILE=TRIM(filename_raw), ACCESS="stream", FORM="unformatted", STATUS="new")

SELECT CASE(TRIM(typ))
    CASE ('rk4'); WRITE(UNIT=fh_ro) rryrk4(:,:,:); DEALLOCATE(rryrk4)
    CASE ('rk8'); WRITE(UNIT=fh_ro) rryrk8(:,:,:); DEALLOCATE(rryrk8)
    CASE ('ik4'); WRITE(UNIT=fh_ro) rryik4(:,:,:); DEALLOCATE(rryik4)
    CASE DEFAULT; WRITE(UNIT=fh_ro) rryik2(:,:,:); DEALLOCATE(rryik2)
END SELECT

CLOSE(UNIT=fh_ro)

!------------------------------------------------------------------------------
! Finish program
!------------------------------------------------------------------------------
IF(rank_mpi == 0) THEN
   CALL meta_signing(binary)
   CALL meta_close()

   IF (std_out/=6) CALL meta_stop_ascii(fh=std_out, suf='.std_out')

END IF ! (rank_mpi == 0)

Call MPI_FINALIZE(ierr)
CALL print_err_stop(std_out, "MPI_FINALIZE didn't succeed", INT(ierr, KIND=ik))

END PROGRAM xtometa
