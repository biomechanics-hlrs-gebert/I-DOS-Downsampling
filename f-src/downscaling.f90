!------------------------------------------------------------------------------
! MODULE: auxiliaries_of_downscaling
!------------------------------------------------------------------------------
!> @author Johannes Gebert - HLRS - NUM - gebert@hlrs.de
!
! @Description:
!> Module containing additional routines for the main program.
!------------------------------------------------------------------------------
MODULE auxiliaries_of_downscaling

USE ISO_FORTRAN_ENV
USE global_std
USE mechanical
USE user_interaction

IMPLICIT NONE
   INTERFACE downscale
      MODULE PROCEDURE downscale_ik2
      MODULE PROCEDURE downscale_ik4
   END INTERFACE downscale

CONTAINS

!------------------------------------------------------------------------------
! SUBROUTINE: downscale_ik2
!------------------------------------------------------------------------------  
!> @author Johannes Gebert, gebert@hlrs.de, HLRS/NUM
!
!> @brief
!> Reduce the resolution of a given image by a given integer factor.
!
!> @param[in] in_array Input array
!> @param[in] scale_factor Factor of high resolution voxels to get a mean ave.
!> @param[out] out_array Output array
!------------------------------------------------------------------------------  
SUBROUTINE downscale_ik2(in_array, scale_factor, out_array)

    INTEGER(KIND=INT16), DIMENSION(:,:,:), INTENT(OUT) :: out_array
    INTEGER(KIND=INT16), DIMENSION(:,:,:), INTENT(IN) :: in_array
    INTEGER(KIND=ik), DIMENSION(3), INTENT(IN) :: scale_factor

    INTEGER(KIND=ik) :: ii, jj, kk

    DO kk=1, SIZE(in_array, DIM=3), scale_factor(3)
    DO jj=1, SIZE(in_array, DIM=2), scale_factor(2)
    DO ii=1, SIZE(in_array, DIM=1), scale_factor(1)
        out_array = SUM(in_array(&
            ii:ii+scale_factor(1)-1_ik, &
            jj:jj+scale_factor(2)-1_ik, &
            kk:kk+scale_factor(3)-1_ik)) / REAL(PRODUCT(scale_factor), KIND=rk)
    END DO
    END DO
    END DO
END SUBROUTINE downscale_ik2

!------------------------------------------------------------------------------
! SUBROUTINE: downscale_ik4
!------------------------------------------------------------------------------  
!> @author Johannes Gebert, gebert@hlrs.de, HLRS/NUM
!
!> @brief
!> Reduce the resolution of a given image by a given integer factor.
!
!> @param[in] in_array Input array
!> @param[in] scale_factor Factor of high resolution voxels to get a mean ave.
!> @param[out] out_array Output array
!------------------------------------------------------------------------------  
SUBROUTINE downscale_ik4(in_array, scale_factor, out_array)

    INTEGER(KIND=INT32), DIMENSION(:,:,:), INTENT(OUT) :: out_array
    INTEGER(KIND=INT32), DIMENSION(:,:,:), INTENT(IN) :: in_array
    INTEGER(KIND=ik), DIMENSION(3), INTENT(IN) :: scale_factor

    INTEGER(KIND=ik) :: ii, jj, kk

    DO kk=1, SIZE(in_array, DIM=3), scale_factor(3)
    DO jj=1, SIZE(in_array, DIM=2), scale_factor(2)
    DO ii=1, SIZE(in_array, DIM=1), scale_factor(1)
        out_array = REAL(SUM(in_array(&
            ii:ii+scale_factor(1)-1_ik, &
            jj:jj+scale_factor(2)-1_ik, &
            kk:kk+scale_factor(3)-1_ik)), KIND=rk) / REAL(PRODUCT(scale_factor), KIND=rk)
    END DO
    END DO
    END DO
END SUBROUTINE downscale_ik4

END MODULE auxiliaries_of_downscaling


!--------------------------------------------------------------------
!> Downscaling
!
!> @author Johannes Gebert - HLRS - NUM - gebert@hlrs.de
!> Date:    15.01.2022
!> LastMod: 15.01.2022
!--------------------------------------------------------------------
PROGRAM downscaling

USE ISO_FORTRAN_ENV
USE global_std
USE user_interaction
USE meta
USE MPI
USE raw_binary
USE auxiliaries_of_downscaling

IMPLICIT NONE

! Parameter
INTEGER(KIND=ik), PARAMETER :: debug = 2   ! Choose an even integer!!

CHARACTER(LEN=mcl), DIMENSION(:), ALLOCATABLE :: m_rry
CHARACTER(LEN=scl) :: type, binary, restart, restart_cmd_arg
CHARACTER(LEN=  8) :: date
CHARACTER(LEN= 10) :: time

INTEGER(KIND=INT16), DIMENSION(:,:,:), ALLOCATABLE :: rry_ik2, rry_out_ik2
INTEGER(KIND=INT32), DIMENSION(:,:,:), ALLOCATABLE :: rry_ik4, rry_out_ik4
INTEGER(KIND=mik), DIMENSION(3) :: sections
INTEGER(KIND=ik), DIMENSION(3) :: dims, rry_dims, sections_ik=0, rank_section
INTEGER(KIND=ik), DIMENSION(3) :: scale_factor_ik, new_subarray_origin, remainder
INTEGER(KIND=ik), DIMENSION(3) :: new_lcl_rry_dims, new_glbl_rry_dims, lcl_subarray_origin
INTEGER(KIND=ik) :: correction_counter = 0, ii=0

REAL(KIND=rk) :: start, end
REAL(KIND=rk), DIMENSION(3) :: origin_glbl_shft, spcng, new_spacing, offset, scale_factor

LOGICAL :: stp

! MPI variables
INTEGER(KIND=mik) :: ierr, my_rank, size_mpi

!------------------------------------------------------------------------------
! Invoke MPI 
!------------------------------------------------------------------------------
CALL mpi_init(ierr)
CALL print_err_stop(std_out, "MPI_INIT didn't succeed", INT(ierr, KIND=ik))

CALL MPI_COMM_RANK(MPI_COMM_WORLD, my_rank, ierr)
CALL print_err_stop(std_out, "MPI_COMM_RANK couldn't be retrieved", INT(ierr, KIND=ik))

CALL MPI_COMM_SIZE(MPI_COMM_WORLD, size_mpi, ierr)
CALL print_err_stop(std_out, "MPI_COMM_SIZE couldn't be retrieved", INT(ierr, KIND=ik))

IF (size_mpi < 2) CALL print_err_stop(std_out, "At least two ranks required to execute this program.", 1)

!------------------------------------------------------------------------------
! Rank 0 -- Init (Master) Process and broadcast init parameters 
!------------------------------------------------------------------------------
IF (my_rank==0) THEN
    !------------------------------------------------------------------------------
    ! Redirect std_out into a file in case std_out is not useful by environment.
    ! Place these lines before handle_lock_file :-)
    !------------------------------------------------------------------------------
    std_out = determine_stout()

    IF(std_out/=6) CALL meta_start_ascii(std_out, '.std_out')

    CALL show_title()
 
    IF(debug >=0) WRITE(*,FMT_MSG) "Post mortem info probably in ./datasets/.temporary.std_out"

    CALL CPU_TIME(start)

    !------------------------------------------------------------------------------
    ! Parse the command arguments
    !------------------------------------------------------------------------------
    CALL get_cmd_args(binary, in%full, stp, restart, restart_cmd_arg)
    IF(stp) GOTO 1001

    !------------------------------------------------------------------------------
    ! Check and open the input file; Modify the Meta-Filename / Basename
    ! Define the new application name first
    !------------------------------------------------------------------------------
    global_meta_prgrm_mstr_app = 'DOSC' 
    global_meta_program_keyword = 'DOWNSCALING'
    CALL meta_append(m_rry)

    !------------------------------------------------------------------------------
    ! Parse input
    !------------------------------------------------------------------------------
    WRITE(std_out, FMT_TXT) 'Reading data from *.meta file.'

    CALL meta_read(std_out, 'ORIGIN_SHIFT_GLBL', m_rry, origin_glbl_shft)
    CALL meta_read(std_out, 'TYPE_RAW', m_rry, type)
    CALL meta_read(std_out, 'SPACING'   , m_rry, spcng)
    CALL meta_read(std_out, 'DIMENSIONS', m_rry, dims)

    CALL meta_read(std_out, 'SCALE_FACTOR', m_rry, scale_factor_ik)
    CALL meta_read(std_out, 'RESTART'     , m_rry, restart)
    
    IF((type /= "ik2") .AND. (type /= "ik4")) THEN
        mssg = "Program only supports ik2 and ik4 for 'TYPE_RAW'"
        CALL print_err_stop(std_out, mssg, 1)
    END IF

END IF ! my_rank==0

!------------------------------------------------------------------------------
! Send required variables
!------------------------------------------------------------------------------
CALL MPI_BCAST(in%p_n_bsnm , INT(meta_mcl, KIND=mik), MPI_CHAR, 0_mik, MPI_COMM_WORLD, ierr)
CALL MPI_BCAST(out%p_n_bsnm, INT(meta_mcl, KIND=mik), MPI_CHAR, 0_mik, MPI_COMM_WORLD, ierr)
CALL MPI_BCAST(type        , INT(scl, KIND=mik), MPI_CHAR, 0_mik, MPI_COMM_WORLD, ierr)
CALL MPI_BCAST(scale_factor_ik, 3_mik, MPI_INTEGER8, 0_mik, MPI_COMM_WORLD, ierr)
CALL MPI_BCAST(dims           , 3_mik, MPI_INTEGER8, 0_mik, MPI_COMM_WORLD, ierr)
CALL MPI_BCAST(spcng          , 3_mik, MPI_DOUBLE_PRECISION, 0_mik, MPI_COMM_WORLD, ierr)
CALL MPI_BCAST(origin_glbl_shft  , 3_mik, MPI_DOUBLE_PRECISION, 0_mik, MPI_COMM_WORLD, ierr)

!------------------------------------------------------------------------------
! Get dimensions for each domain. Every processor reveives its own domain.
! Therefore, each my_rank calculates its own address/dimensions/parameters.
!
! Allocation of subarray memory is done in the read_raw routines.
!------------------------------------------------------------------------------
! Calculation of the downscaling directly affects the mpi subarrays (!) 
!------------------------------------------------------------------------------
sections=0
CALL MPI_DIMS_CREATE (size_mpi, 3_mik, sections, ierr)

sections_ik = INT(sections, KIND=ik)

CALL get_rank_section(INT(my_rank, KIND=ik), sections_ik, rank_section)

!------------------------------------------------------------------------------
! Get new dimensions out of (field of view) / target_spcng
!------------------------------------------------------------------------------
new_spacing = spcng * scale_factor

scale_factor = REAL(scale_factor_ik, KIND=rk)

!------------------------------------------------------------------------------
! Fit local array dimensions to scale_factor
!------------------------------------------------------------------------------
remainder = MODULO(dims, scale_factor_ik)

new_lcl_rry_dims = (dims - remainder) / sections_ik

DO ii=1, 3
    DO WHILE(MODULO(new_lcl_rry_dims(ii), scale_factor_ik(ii)) /= 0_ik)


        new_lcl_rry_dims(ii) = new_lcl_rry_dims(ii) - 1_ik

    END DO
END DO 

!------------------------------------------------------------------------------
! MPI specific subarray dimensions with global offset of remainder
!------------------------------------------------------------------------------
new_glbl_rry_dims = new_lcl_rry_dims * sections_ik

lcl_subarray_origin = (rank_section-1_ik) * (new_lcl_rry_dims) + FLOOR(remainder/2._rk, KIND=ik)

offset = FLOOR(remainder/2._rk) * new_spacing

origin_glbl_shft = origin_glbl_shft + offset

new_subarray_origin = (rank_section-1_ik) * (rry_dims)

!------------------------------------------------------------------------------
! The remainder is ignored, since the spatial resolution will break with a, 
! integer based scaling, which deformes the last voxel of dims.
!------------------------------------------------------------------------------

IF(my_rank == 0) THEN
    ! DEBUG INFORMATION
    IF (debug >= 0) THEN 
        CALL DATE_AND_TIME(date, time)
        
        WRITE(std_out, FMT_TXT) "Date: "//date//" [ccyymmdd]"
        WRITE(std_out, FMT_TXT) "Time: "//time//" [hhmmss.sss]"  
        WRITE(std_out, FMT_TXT_SEP)
        WRITE(std_out, FMT_MSG_AxI0) "Debug Level:", debug
        WRITE(std_out, FMT_MSG_AxI0) "Processors:", size_mpi  
        WRITE(std_out, FMT_MSG) "Calculation of domain sectioning:"
        WRITE(std_out, FMT_MSG)
        WRITE(std_out, FMT_MSG_AxI0) "Scale factor: ", scale_factor_ik
        WRITE(std_out, FMT_MSG_AxI0) "sections: ", sections_ik
        WRITE(std_out, FMT_MSG_AxI0) "Input dims: ", dims
        WRITE(std_out, FMT_MSG_AxI0) "Local subarray dimensions: ", new_lcl_rry_dims
        WRITE(std_out, FMT_MSG_AxI0) "Output dims: ", new_glbl_rry_dims
        WRITE(std_out, FMT_MSG_AxI0) "subarray_origin: ", new_subarray_origin
        WRITE(std_out, FMT_MSG_SEP)
        FLUSH(std_out)
    END IF

END IF

!------------------------------------------------------------------------------
! Read binary part of the vtk file - basically a *.raw file
!
! Allocate memory for the downscaled array/image
!------------------------------------------------------------------------------
IF(my_rank==0) WRITE(std_out, FMT_TXT) 'Reading image.'

SELECT CASE(type)
    CASE('ik2') 
        CALL mpi_read_raw(TRIM(in%p_n_bsnm)//raw_suf, 0_8, new_glbl_rry_dims, &
            new_lcl_rry_dims, lcl_subarray_origin, rry_ik2)

        ALLOCATE(rry_out_ik2(new_lcl_rry_dims(1), new_lcl_rry_dims(2), new_lcl_rry_dims(3)))

    CASE('ik4') 
        CALL mpi_read_raw(TRIM(in%p_n_bsnm)//raw_suf, 0_8, new_glbl_rry_dims, &
            new_lcl_rry_dims, lcl_subarray_origin, rry_ik4)

        ALLOCATE(rry_out_ik4(new_lcl_rry_dims(1), new_lcl_rry_dims(2), new_lcl_rry_dims(3)))
END SELECT

!------------------------------------------------------------------------------
! Compute downscaling
!------------------------------------------------------------------------------
IF(my_rank==0) WRITE(std_out, FMT_TXT) 'Downscaling image.'
    
SELECT CASE(type)
    CASE('ik2'); CALL downscale(rry_ik2, scale_factor_ik, rry_out_ik2)
    CASE('ik4'); CALL downscale(rry_ik4, scale_factor_ik, rry_out_ik4)
END SELECT

!------------------------------------------------------------------------------
! Write raw data
!------------------------------------------------------------------------------
IF(my_rank==0) WRITE(std_out, FMT_TXT) 'Writing binary information to *.raw file.'

SELECT CASE(type)
    CASE('ik2') 
        CALL mpi_write_raw(TRIM(out%p_n_bsnm)//raw_suf, 0_8, new_glbl_rry_dims, &
            new_lcl_rry_dims, lcl_subarray_origin, rry_ik2)
        DEALLOCATE(rry_out_ik2)

    CASE('ik4') 
        CALL mpi_write_raw(TRIM(out%p_n_bsnm)//raw_suf, 0_8, new_glbl_rry_dims, &
            new_lcl_rry_dims, lcl_subarray_origin, rry_ik4)
        DEALLOCATE(rry_out_ik4)

END SELECT

!------------------------------------------------------------------------------
! Jump to end for a more gracefully ending of the program in specific cases :-)
!------------------------------------------------------------------------------
1001 CONTINUE

!------------------------------------------------------------------------------
! Finish program
!------------------------------------------------------------------------------
IF(my_rank == 0) THEN
    CALL meta_write(fhmeo, 'DIMENSIONS'   , '(-)', new_glbl_rry_dims)
    CALL meta_write(fhmeo, 'FIELD_OF_VIEW', '(-)', new_glbl_rry_dims * new_spacing)
    CALL meta_write(fhmeo, 'ENTRIES'      , '(-)', PRODUCT(new_glbl_rry_dims))
    CALL meta_write(fhmeo, 'ORIGIN_SHIFT_GLBL', '(mm)', PRODUCT(origin_glbl_shft))

    CALL CPU_TIME(end)

    WRITE(std_out, FMT_TXT_xAF0) 'Finishing the program took', end-start,'seconds.'
    WRITE(std_out, FMT_TXT_SEP)

    CALL meta_signing(binary)
    CALL meta_close()

    IF (std_out/=6) CALL meta_stop_ascii(fh=std_out, suf='.std_out')

END IF ! (my_rank == 0)

Call MPI_FINALIZE(ierr)
CALL print_err_stop(std_out, "MPI_FINALIZE didn't succeed", INT(ierr, KIND=ik))

END PROGRAM downscaling