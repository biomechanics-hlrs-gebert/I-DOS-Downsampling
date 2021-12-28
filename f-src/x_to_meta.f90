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
CHARACTER(LEN=mcl) :: vtk, new_basename, type_in, type_out
CHARACTER(LEN=mcl) :: filename='', filename_meta
INTEGER(KIND=ik)   :: hdr, std_out

INTEGER(KIND=ik), DIMENSION(3) :: dims
REAL(KIND=rk)   , DIMENSION(3) :: spcng, origin

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
! Rank 0 -- Init (Master) Process and broadcast init parameters 
!------------------------------------------------------------------------------
If (rank_mpi==0) Then

    !------------------------------------------------------------------------------
    ! Start actual program
    !------------------------------------------------------------------------------
    CALL std_out = determine_stout()

    CALL show_title(longname, revision)

    CALL GET_COMMAND_ARGUMENT(1, filename)

    IF (filename='') CALL print_err_stop(std_out, "No input file given", 1)

    CALL meta_create_new(TRIM(ADJUSTL(filename)))

    !------------------------------------------------------------------------------
    ! Read VTK file header
    !------------------------------------------------------------------------------
    CALL read_vtk_meta (filename, hdr, dims, origin, spcng, type_in) 

    !------------------------------------------------------------------------------
    ! Determine data types
    ! Data types are converted to integer, since the provided precision on the
    ! data is sufficient.
    !------------------------------------------------------------------------------
    SELECT CASE(type_in)
        CASE('rk4') ; type_out = 'ik4'
        CASE('rk8') ; type_out = 'ik4'
        CASE('ik4') ; type_out = 'ik4'
        CASE('ik2') ; type_out = 'ik2'
        CASE('uik2'); type_out = 'ik2'
    END SELECT

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
    CALL meta_write (fhmeo, 'HISTORY',           '(-)' , 1)
    
    ! Write original data type to file for documentaroy purposes
    CALL meta_write (fhmeo, 'TYPE_IMPORT', TRIM(ADJUSTL(type_in)))
    CALL meta_write (fhmeo, 'TYPE_RAW', TRIM(ADJUSTL(type_out)))
    
    !------------------------------------------------------------------------------
    ! Data always are written as little endian; only vtk requires big endian.
    !------------------------------------------------------------------------------
    CALL meta_write (fhmeo, 'DATA_BYTE_ORDER'  , "LittleEndian")
    CALL meta_write (fhmeo, 'DIMENSIONALITY'   , '(-)'  , 3)
    CALL meta_write (fhmeo, 'DIMENSIONS'       , '(-)'  , dims)
    CALL meta_write (fhmeo, 'NO_SCALAR_CMPNNTS', '(-)'  , 1)
    CALL meta_write (fhmeo, 'SPACING'          , '(mm)' , spcng)
    CALL meta_write (fhmeo, 'ORIGIN_SHIFT_GLBL', '(mm)' , [0., 0., 0.])
    CALL meta_write (fhmeo, 'ORIGIN'           , '(-)'  , [0, 0, 0])
    CALL meta_write (fhmeo, 'FIELD_OF_VIEW'    , '(mm)' , dims*spcng)
    CALL meta_write (fhmeo, 'ENTRIES'          , '(-)'  , PRODUCT(dims))

END IF ! rank_mpi==0

!------------------------------------------------------------------------------
! Read binary part of the vtk file - basically a *.raw file
!------------------------------------------------------------------------------
fh = give_new_unit

SELECT CASE(type_in)
    CASE('rk4') 
        CALL mpi_read_raw(fh, in%p_n_bsnm//vtk_suf, hdr, dims, subarray_dims, subarray_origin, subarray_rk4, .TRUE.)
    CASE('rk8') 
        CALL mpi_read_raw(fh, in%p_n_bsnm//vtk_suf, hdr, dims, subarray_dims, subarray_origin, subarray_rk8, .TRUE.)
    CASE('ik4') 
        CALL mpi_read_raw(fh, in%p_n_bsnm//vtk_suf, hdr, dims, subarray_dims, subarray_origin, subarray_ik4, .TRUE.)
    CASE('ik2', 'uik2') 
        CALL mpi_read_raw(fh, in%p_n_bsnm//vtk_suf, hdr, dims, subarray_dims, subarray_origin, subarray_ik2, .TRUE.)
END SELECT

!------------------------------------------------------------------------------
! Convert arrays and write raw data
!------------------------------------------------------------------------------
SELECT CASE(type_out)
    CASE('ik2') 
        CALL mpi_write_raw(fh, in%p_n_bsnm//raw_suf, 0, dims, subarray_dims, subarray_origin, subarray_ik2)
    CASE('ik4') 
        SELECT CASE(type_in)
            CASE('rk4') 
                subarray_ik4 = INT(subarray_rk4, KIND=INT32)
            CASE('rk8') 
                subarray_ik4 = INT(subarray_rk8, KIND=INT32)
        END SELECT

        CALL mpi_write_raw(fh, in%p_n_bsnm//raw_suf, 0, dims, subarray_dims, subarray_origin, subarray_ik4)
END SELECT

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
