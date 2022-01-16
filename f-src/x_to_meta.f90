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
USE user_interaction
USE meta
USE MPI
USE vtk_meta_data
USE raw_binary  

IMPLICIT NONE

! MPI variables
INTEGER(KIND=mik) :: ierr, my_rank, size_mpi

! Std variables
CHARACTER(LEN=scl) :: type_in, type_out, binary
CHARACTER(LEN=mcl) :: filename=''

INTEGER(KIND=ik) :: hdr
INTEGER(KIND=mik), DIMENSION(3) :: sections
INTEGER(KIND=ik), DIMENSION(3) :: dims, rry_dims, rank_section, sections_ik
INTEGER(KIND=ik), DIMENSION(3) :: remainder_per_dir, dims_reduced, subarray_origin
REAL   (KIND=rk), DIMENSION(3) :: spcng, origin
REAL   (KIND=rk) :: start, end

! Binary blob variables
REAL   (KIND=REAL32), DIMENSION(:,:,:), ALLOCATABLE :: rry_rk4
REAL   (KIND=REAL64), DIMENSION(:,:,:), ALLOCATABLE :: rry_rk8
INTEGER(KIND=INT16) , DIMENSION(:,:,:), ALLOCATABLE :: rry_ik2
INTEGER(KIND=INT32) , DIMENSION(:,:,:), ALLOCATABLE :: rry_ik4

!------------------------------------------------------------------------------
! Invoke MPI 
!------------------------------------------------------------------------------
CALL mpi_init(ierr)
CALL print_err_stop(std_out, "MPI_INIT didn't succeed", INT(ierr, KIND=ik))

CALL MPI_COMM_RANK(MPI_COMM_WORLD, my_rank, ierr)
CALL print_err_stop(std_out, "MPI_COMM_RANK couldn't be retrieved", INT(ierr, KIND=ik))

CALL MPI_COMM_SIZE(MPI_COMM_WORLD, size_mpi, ierr)
CALL print_err_stop(std_out, "MPI_COMM_SIZE couldn't be retrieved", INT(ierr, KIND=ik))

IF (size_mpi < 2) CALL print_err_stop(std_out, "We need at least 2 MPI processes to execute this program.", 1)

!------------------------------------------------------------------------------
! Rank 0 -- Init (Master) Process and broadcast init parameters 
!------------------------------------------------------------------------------
IF (my_rank==0) THEN

    CALL CPU_TIME(start)

    !------------------------------------------------------------------------------
    ! Start actual program
    !------------------------------------------------------------------------------
    std_out = determine_stout()

    CALL show_title()

    CALL GET_COMMAND_ARGUMENT(0, binary)
    CALL GET_COMMAND_ARGUMENT(1, filename)

    IF (filename=='') THEN
        CALL usage(binary)    
        CALL print_err_stop(std_out, "No input file given", 1)
    END IF

    WRITE(std_out, FMT_TXT) 'Creating a new meta file.'
    CALL meta_create_new(TRIM(ADJUSTL(filename)))

    !------------------------------------------------------------------------------
    ! Read VTK file header
    !------------------------------------------------------------------------------
    WRITE(std_out, FMT_TXT) 'Reading the vtk header.'
    CALL read_vtk_meta (filename, hdr, dims, origin, spcng, type_in) 

    !------------------------------------------------------------------------------
    ! Determine data types
    ! Data types are converted to integer, since the provided precision on the
    ! data is sufficient.
    !------------------------------------------------------------------------------
    WRITE(std_out, FMT_TXT) 'Determining the output data types.'
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
    WRITE(std_out, FMT_TXT) 'Writing meta information to *.meta file.'

    WRITE(fhmeo, '(A)') "# HLRS|NUM Dataset Meta Information"
    WRITE(fhmeo, '(A)') ""
    WRITE(fhmeo, '(A)') "* GENERAL_INFORMATION"
    CALL meta_write (fhmeo, 'CT_SCAN', in%dataset)
    CALL meta_write (fhmeo, 'OWNER',         "TBD by user")
    CALL meta_write (fhmeo, 'OWNER_CONTACT', "TBD by user")
    CALL meta_write (fhmeo, 'DATE_CREATED',  "TBD by user")
    CALL meta_write (fhmeo, 'INTERNAL_ID',   "TBD by user")
    CALL meta_write (fhmeo, 'HISTORY', '(-)' , 1)
    
    ! Write original data type to file for documentaroy purposes
    CALL meta_write (fhmeo, 'TYPE_IMPORT', TRIM(ADJUSTL(type_in)))
    CALL meta_write (fhmeo, 'TYPE_RAW', TRIM(ADJUSTL(type_out)))
    
    !------------------------------------------------------------------------------
    ! Data always are written as little endian; only vtk requires big endian.
    !------------------------------------------------------------------------------
    CALL meta_write (fhmeo, 'DATA_BYTE_ORDER'  , "BigEndian")
    CALL meta_write (fhmeo, 'DIMENSIONALITY'   , '(-)'  , 3)
    CALL meta_write (fhmeo, 'DIMENSIONS'       , '(-)'  , dims)
    CALL meta_write (fhmeo, 'NO_SCALAR_CMPNNTS', '(-)'  , 1)
    CALL meta_write (fhmeo, 'SPACING'          , '(mm)' , spcng)
    CALL meta_write (fhmeo, 'ORIGIN_SHIFT_GLBL', '(mm)' , [0._rk, 0._rk, 0._rk])
    CALL meta_write (fhmeo, 'ORIGIN'           , '(-)'  , [0, 0, 0])
    CALL meta_write (fhmeo, 'FIELD_OF_VIEW'    , '(mm)' , dims*spcng)
    CALL meta_write (fhmeo, 'ENTRIES'          , '(-)'  , PRODUCT(dims))
   
    FLUSH(fhmeo)

END IF ! my_rank==0

!------------------------------------------------------------------------------
! Send required variables
!------------------------------------------------------------------------------
CALL MPI_BCAST (type_in     , INT(scl, KIND=mik)     , MPI_CHAR, 0_mik, MPI_COMM_WORLD, ierr)
CALL MPI_BCAST (type_out    , INT(scl, KIND=mik)     , MPI_CHAR, 0_mik, MPI_COMM_WORLD, ierr)
CALL MPI_BCAST (in%p_n_bsnm , INT(meta_mcl, KIND=mik), MPI_CHAR, 0_mik, MPI_COMM_WORLD, ierr)
CALL MPI_BCAST (out%p_n_bsnm, INT(meta_mcl, KIND=mik), MPI_CHAR, 0_mik, MPI_COMM_WORLD, ierr)
CALL MPI_BCAST (hdr , 1_mik, MPI_INTEGER8, 0_mik, MPI_COMM_WORLD, ierr)
CALL MPI_BCAST (dims, 3_mik, MPI_INTEGER8, 0_mik, MPI_COMM_WORLD, ierr)

!------------------------------------------------------------------------------
! Get dimensions for each domain. Every processor reveives its own domain.
! Therefore, each my_rank calculates its own address/dimensions/parameters.
!
! The decomposition of an image depends on the task to be done. Therefore, 
! these calculations are not encapsuled in a routine. However, this may
! follow in an update during refactoring the 3D scalar image filter.
!
! Allocation of subarray memory is done in the read_raw routines.
!------------------------------------------------------------------------------
sections=0
CALL MPI_DIMS_CREATE (size_mpi, 3_mik, sections, ierr)
CALL get_rank_section(INT(my_rank, KIND=ik), INT(sections, KIND=ik), rank_section)

sections_ik = INT(sections, KIND=ik)
remainder_per_dir = MODULO(dims, sections_ik)

dims_reduced   = dims - remainder_per_dir

rry_dims  = (dims_reduced / sections_ik)

subarray_origin = (rank_section-1_ik) * (rry_dims)

! Add the remainder to the last domains of each dimension
IF(rank_section(1) == sections_ik(1)) rry_dims(1) = rry_dims(1) + remainder_per_dir(1)
IF(rank_section(2) == sections_ik(2)) rry_dims(2) = rry_dims(2) + remainder_per_dir(2)
IF(rank_section(3) == sections_ik(3)) rry_dims(3) = rry_dims(3) + remainder_per_dir(3)

!------------------------------------------------------------------------------
! Read binary part of the vtk file - basically a *.raw file
!------------------------------------------------------------------------------
IF(my_rank==0) WRITE(std_out, FMT_TXT) 'Reading binary information of *.vtk file.'

SELECT CASE(type_in)
    CASE('rk4') 
        ! MPI_OFFSET_KIND needs ik=8 in this case.
        CALL mpi_read_raw(TRIM(in%p_n_bsnm)//vtk_suf, INT(hdr, KIND=8), dims, rry_dims, subarray_origin, rry_rk4)
    CASE('rk8') 
        CALL mpi_read_raw(TRIM(in%p_n_bsnm)//vtk_suf, INT(hdr, KIND=8), dims, rry_dims, subarray_origin, rry_rk8)
    CASE('ik4') 
        CALL mpi_read_raw(TRIM(in%p_n_bsnm)//vtk_suf, INT(hdr, KIND=8), dims, rry_dims, subarray_origin, rry_ik4)
    CASE('ik2', 'uik2') 
        CALL mpi_read_raw(TRIM(in%p_n_bsnm)//vtk_suf, INT(hdr, KIND=8), dims, rry_dims, subarray_origin, rry_ik2)
        IF(type_in=='uik2') CALL uik2_to_ik2(rry_ik2)
END SELECT

!------------------------------------------------------------------------------
! Convert arrays and write raw data
!------------------------------------------------------------------------------
IF(my_rank==0) WRITE(std_out, FMT_TXT) 'Converting and writing binary information to *.raw file.'

SELECT CASE(type_out)
    CASE('ik2') 
        CALL mpi_write_raw(TRIM(out%p_n_bsnm)//raw_suf, 0_8, dims, rry_dims, subarray_origin, rry_ik2)
    CASE('ik4') 
        SELECT CASE(type_in)
            CASE('rk4') 
                rry_ik4 = INT(rry_rk4, KIND=INT32)
            CASE('rk8') 
                rry_ik4 = INT(rry_rk8, KIND=INT32)
        END SELECT

        CALL mpi_write_raw(TRIM(out%p_n_bsnm)//raw_suf, 0_8, dims, rry_dims, subarray_origin, rry_ik4)
END SELECT

!------------------------------------------------------------------------------
! Finish program
!------------------------------------------------------------------------------
IF(my_rank == 0) THEN
    CALL CPU_TIME(end)

    WRITE(std_out, FMT_TXT_xAF0) 'Finishing the program took', end-start,'seconds.'
    WRITE(std_out, FMT_TXT_SEP)

    CALL meta_signing(binary)
    CALL meta_close()

    IF (std_out/=6) CALL meta_stop_ascii(fh=std_out, suf='.std_out')

END IF ! (my_rank == 0)

Call MPI_FINALIZE(ierr)
CALL print_err_stop(std_out, "MPI_FINALIZE didn't succeed", INT(ierr, KIND=ik))

END PROGRAM xtometa
