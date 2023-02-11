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

    INTEGER(INT16), DIMENSION(:,:,:), INTENT(OUT) :: out_array
    INTEGER(INT16), DIMENSION(:,:,:), INTENT(IN) :: in_array
    INTEGER(ik), DIMENSION(3), INTENT(IN) :: scale_factor

    INTEGER(ik) :: ii, jj, kk, ll, mm, nn
    REAL(rk) :: scale_volume

    scale_volume = REAL(PRODUCT(scale_factor), rk)

    nn = 1_ik
    DO kk=1, SIZE(in_array, DIM=3) - scale_factor(3), scale_factor(3)

        mm = 1_ik
        DO jj=1, SIZE(in_array, DIM=2) - scale_factor(2), scale_factor(2)

            ll = 1_ik
            DO ii=1, SIZE(in_array, DIM=1) - scale_factor(1), scale_factor(1)

                out_array(ll, mm, nn) = INT(REAL(SUM(in_array(&
                    ii:ii+scale_factor(1)-1_ik, &
                    jj:jj+scale_factor(2)-1_ik, &
                    kk:kk+scale_factor(3)-1_ik)), rk) / scale_volume,ik)

                ll = ll + 1_ik
            END DO

            mm = mm + 1_ik
        END DO

        nn = nn + 1_ik
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

    INTEGER(INT32), DIMENSION(:,:,:), INTENT(OUT) :: out_array
    INTEGER(INT32), DIMENSION(:,:,:), INTENT(IN) :: in_array
    INTEGER(ik), DIMENSION(3), INTENT(IN) :: scale_factor
    INTEGER(ik) :: ii, jj, kk, ll, mm, nn
    REAL(rk) :: scale_volume

    scale_volume = REAL(PRODUCT(scale_factor), rk)

    nn = 1_ik
    DO kk=1, SIZE(in_array, DIM=3)-scale_factor(3), scale_factor(3)

        mm = 1_ik
        DO jj=1, SIZE(in_array, DIM=2)-scale_factor(2), scale_factor(2)

            ll = 1_ik
            DO ii=1, SIZE(in_array, DIM=1)-scale_factor(1), scale_factor(1)

                out_array(ll, mm, nn) = INT(REAL(SUM(in_array(&
                    ii:ii+scale_factor(1)-1_ik, &
                    jj:jj+scale_factor(2)-1_ik, &
                    kk:kk+scale_factor(3)-1_ik)), rk) / scale_volume,ik)

                ll = ll + 1_ik
            END DO

            mm = mm + 1_ik
        END DO

        nn = nn + 1_ik
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
USE vtk_meta_data
USE ser_binary
USE auxiliaries_of_downscaling
USE user_interaction

IMPLICIT NONE

! Parameter
CHARACTER(mcl), DIMENSION(:), ALLOCATABLE :: m_rry
CHARACTER(scl) :: type, binary, restart, restart_cmd_arg
CHARACTER(  8) :: date
CHARACTER( 10) :: time
CHARACTER(mcl) :: stat=''

INTEGER(INT16), DIMENSION(:,:,:), ALLOCATABLE :: rry_ik2, rry_out_ik2
INTEGER(INT32), DIMENSION(:,:,:), ALLOCATABLE :: rry_ik4, rry_out_ik4
INTEGER(ik), DIMENSION(3) :: dims, rry_dims, sections_ik=0, rank_section, scale_factor_ik, &
    new_subarray_origin, remainder, new_lcl_rry_in_dims, new_glbl_rry_dims
INTEGER(ik) :: ii=0

REAL(rk) :: start, end
REAL(rk), DIMENSION(3) :: origin_glbl_shft, spcng, field_of_view, new_spacing, offset, scale_factor

CALL show_title(["Johannes Gebert, M.Sc. (HLRS, NUM)"])

IF(out_amount == "DEBUG") THEN
    WRITE(std_out,FMT_MSG) "Post mortem info probably in ./datasets/.temporary.std_out"
END IF 

CALL CPU_TIME(start)

!------------------------------------------------------------------------------
! Parse the command arguments
!------------------------------------------------------------------------------
CALL get_cmd_args(binary, in%full, restart, restart_cmd_arg)

!------------------------------------------------------------------------------
! Check and open the input file; Modify the Meta-Filename / Basename
! Define the new application name first
!------------------------------------------------------------------------------
global_meta_prgrm_mstr_app = 'dos' 
global_meta_program_keyword = 'DOWNSCALING'
CALL meta_append(m_rry, 1_mik, binary, stat)

!------------------------------------------------------------------------------
! Parse input
!------------------------------------------------------------------------------
WRITE(std_out, FMT_TXT) 'Reading data from *.meta file.'

CALL meta_read('ORIGIN_SHIFT_GLBL', m_rry, origin_glbl_shft, stat)
CALL meta_read('TYPE_RAW', m_rry, type, stat)
CALL meta_read('SPACING'   , m_rry, spcng, stat)
CALL meta_read('DIMENSIONS', m_rry, dims, stat)

CALL meta_read('SCALE_FACTOR', m_rry, scale_factor_ik, stat)
CALL meta_read('RESTART'     , m_rry, restart, stat)

IF((type /= "ik2") .AND. (type /= "ik4")) THEN
    mssg = "Program only supports ik2 and ik4 for 'TYPE_RAW'"
    CALL print_err_stop(std_out, mssg, 1)
END IF


!------------------------------------------------------------------------------
! Get new dimensions out of (field of view) / target_spcng
!------------------------------------------------------------------------------
scale_factor = REAL(scale_factor_ik, rk)

new_spacing = spcng * scale_factor

remainder = MODULO(dims, scale_factor_ik)

new_lcl_rry_in_dims = (dims - remainder)
new_glbl_rry_dims = (new_lcl_rry_in_dims / scale_factor_ik) + 1_ik

!------------------------------------------------------------------------------
! Fit local array dimensions to scale_factor
!------------------------------------------------------------------------------
DO ii=1, 3
    DO WHILE(MODULO(new_lcl_rry_in_dims(ii), scale_factor_ik(ii)) /= 0_ik)

        new_lcl_rry_in_dims(ii) = new_lcl_rry_in_dims(ii) - 1_ik
    END DO
END DO 

field_of_view = new_glbl_rry_dims * new_spacing


!------------------------------------------------------------------------------
! Remainder relative to input dimensions and spacings
!------------------------------------------------------------------------------
offset = FLOOR(remainder/2._rk) * spcng

origin_glbl_shft = origin_glbl_shft + offset

new_subarray_origin = (rank_section-1_ik) * (rry_dims)

!------------------------------------------------------------------------------
! The remainder is ignored, since the spatial resolution will break with a, 
! integer based scaling, which deformes the last voxel of dims.
!------------------------------------------------------------------------------
IF(out_amount == "DEBUG") THEN
    CALL DATE_AND_TIME(date, time)
    
    WRITE(std_out, FMT_TXT) "Date: "//date//" [ccyymmdd]"
    WRITE(std_out, FMT_TXT) "Time: "//time//" [hhmmss.sss]"  
    WRITE(std_out, FMT_TXT_SEP)
    WRITE(std_out, FMT_MSG) "Calculation of domain sectioning:"
    WRITE(std_out, FMT_MSG)
    WRITE(std_out, FMT_MSG_AxI0) "Scale factor: ", scale_factor_ik
    WRITE(std_out, FMT_MSG_AxI0) "Input dims: ", dims
    WRITE(std_out, FMT_MSG_AxI0) "Output dims: ", new_glbl_rry_dims
    WRITE(std_out, FMT_MSG_SEP)
    FLUSH(std_out)
END IF


!------------------------------------------------------------------------------
! Read binary part of the vtk file - basically a *.raw file
!
! Allocate memory for the downscaled array/image
!------------------------------------------------------------------------------
WRITE(std_out, FMT_TXT) 'Reading image.'

SELECT CASE(type)
    CASE('ik2') 
        ALLOCATE(rry_ik2(dims(1), dims(2), dims(3)))

        CALL ser_read_binary(51, TRIM(in%p_n_bsnm)//raw_suf, rry_ik2)

        ALLOCATE(rry_out_ik2(new_glbl_rry_dims(1), new_glbl_rry_dims(2), new_glbl_rry_dims(3)))
        rry_out_ik2 = 0_ik

    CASE('ik4') 
        ALLOCATE(rry_ik4(dims(1), dims(2), dims(3)))

        CALL ser_read_binary(51, TRIM(in%p_n_bsnm)//raw_suf, rry_ik4)

        ALLOCATE(rry_out_ik4(new_glbl_rry_dims(1), new_glbl_rry_dims(2), new_glbl_rry_dims(3)))
        rry_out_ik4 = 0_ik
END SELECT

!------------------------------------------------------------------------------
! Compute downscaling
!------------------------------------------------------------------------------
WRITE(std_out, FMT_TXT) 'Downscaling image.'
    
SELECT CASE(type)
    CASE('ik2'); CALL downscale(rry_ik2, scale_factor_ik, rry_out_ik2) 
    CASE('ik4'); CALL downscale(rry_ik4, scale_factor_ik, rry_out_ik4) 
END SELECT


!------------------------------------------------------------------------------
! Write raw data
!------------------------------------------------------------------------------
WRITE(std_out, FMT_TXT) 'Writing binary information to *.raw file.'

SELECT CASE(type)
    CASE('ik2') 

        CALL ser_write_binary(52, TRIM(out%p_n_bsnm)//raw_suf, rry_out_ik2)
        DEALLOCATE(rry_out_ik2)

    CASE('ik4') 

        CALL ser_write_binary(52, TRIM(out%p_n_bsnm)//raw_suf, rry_out_ik4)
        DEALLOCATE(rry_out_ik4)

END SELECT

!------------------------------------------------------------------------------
! Finish program
!------------------------------------------------------------------------------
CALL meta_write('PROCESSORS'       , '(-)', 1)
CALL meta_write('SUBARRAY_SECTIONS', '(-)', sections_ik)

CALL meta_write('DIMENSIONS'   , '(-)', new_glbl_rry_dims)
CALL meta_write('SPACING'      , '(-)', new_spacing)
CALL meta_write('FIELD_OF_VIEW', '(-)', field_of_view)
CALL meta_write('ENTRIES'      , '(-)', PRODUCT(new_glbl_rry_dims))
CALL meta_write('ORIGIN_SHIFT_GLBL', '(mm)', origin_glbl_shft)

CALL CPU_TIME(end)

WRITE(std_out, FMT_TXT_xAF0) 'Finishing the program took', end-start,'seconds.'
WRITE(std_out, FMT_TXT_SEP)

CALL meta_close(m_rry)

END PROGRAM downscaling