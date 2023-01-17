!------------------------------------------------------------------------------
! MODULE: raw_binary
!------------------------------------------------------------------------------
!> @author Johannes Gebert - HLRS - NUM - gebert@hlrs.de
!
!> @brief:
!> Module for reading/writing raw binary files serially.
!------------------------------------------------------------------------------
MODULE ser_binary

USE ISO_FORTRAN_ENV
USE global_std

IMPLICIT NONE
INTERFACE ser_read_binary
   MODULE PROCEDURE ser_read_binary_ik1
   MODULE PROCEDURE ser_read_binary_ik2
   MODULE PROCEDURE ser_read_binary_ik4
   MODULE PROCEDURE ser_read_binary_ik8
   MODULE PROCEDURE ser_read_binary_rk4
   MODULE PROCEDURE ser_read_binary_rk8
END INTERFACE ser_read_binary

INTERFACE ser_write_binary
   MODULE PROCEDURE ser_write_binary_ik1
   MODULE PROCEDURE ser_write_binary_ik2
   MODULE PROCEDURE ser_write_binary_ik4
   MODULE PROCEDURE ser_write_binary_ik8
   MODULE PROCEDURE ser_write_binary_rk4
   MODULE PROCEDURE ser_write_binary_rk8
END INTERFACE ser_write_binary

CONTAINS
!------------------------------------------------------------------------------
! FUNCTION: get_grid
!------------------------------------------------------------------------------  
!> @author Johannes Gebert - HLRS - NUM - gebert@hlrs.de
!
!> @brief
!> Translate a domain number to its corresponding section
!
!> @param[in] dmn_size Size of the domain
!> @param[in] dims Voxels (x,y,z) of the domain
!> @param[in] spcng Distance between voxels
!> @param[out] grid of the image
!------------------------------------------------------------------------------  
FUNCTION get_grid(dmn_size, dims, spcng) RESULT(grid)

REAL(rk), DIMENSION(3) :: spcng, dmn_size

INTEGER(ik), DIMENSION(3) :: dims, vox_per_dmn, grid

vox_per_dmn = FLOOR(dmn_size/spcng, ik)
grid = FLOOR(REAL(dims)/REAL(vox_per_dmn), ik)

END FUNCTION get_grid

!------------------------------------------------------------------------------
! FUNCTION: domain_no_to_section
!------------------------------------------------------------------------------  
!> @author Johannes Gebert - HLRS - NUM - gebert@hlrs.de
!
!> @brief
!> Translate a domain number to its corresponding section
!
!> @param[in] domain No of the domain. 
!> @param[in] size of the domain
!> @param[out] section of the domain.
!------------------------------------------------------------------------------  
FUNCTION domain_no_to_section(domain, grid) RESULT(section)
   
INTEGER(ik) :: domain
INTEGER(ik), DIMENSION(3) :: section, grid

section(3) = FLOOR(REAL(domain, rk)/(grid(1)*grid(2)))

section(2) = FLOOR((REAL(domain, rk) - (grid(1)*grid(2)*section(3)) ) / grid(1))

section(1) = domain - (grid(1)*grid(2)*section(3)) - (grid(1)*section(2))

END FUNCTION domain_no_to_section

!------------------------------------------------------------------------------
! FUNCTION: section_to_domain_no
!------------------------------------------------------------------------------  
!> @author Johannes Gebert - HLRS - NUM - gebert@hlrs.de
!
!> @brief
!> Translate a domain number to its corresponding section
!
!> @param[in] domain No of the domain. 
!> @param[in] size of the domain
!> @param[out] section of the domain.
!------------------------------------------------------------------------------  
FUNCTION section_to_domain_no(grid, section) RESULT(domain)
   
      INTEGER(ik), DIMENSION(3) :: section, grid
      INTEGER(ik) :: domain

      domain = section(1) + (section(2)*grid(1)) + (section(3)*grid(1)*grid(2))

END FUNCTION section_to_domain_no


!------------------------------------------------------------------------------
! SUBROUTINE: get_rank_section
!------------------------------------------------------------------------------  
!> @author Johannes Gebert - HLRS - NUM - gebert@hlrs.de
!
!> @brief
!> Get the section address/number of a specific domain
!
!> @param[in] domain No of the control volume. 
!> @param[in] sections x/y/z mesh of domains
!> @param[out] rank_section position of domain in x/y/z mesh
!------------------------------------------------------------------------------  
SUBROUTINE get_rank_section(domain, sections, rank_section)
  
INTEGER(ik), INTENT(IN)  :: domain
INTEGER(ik), DIMENSION(3), INTENT(IN)  :: sections
INTEGER(ik), DIMENSION(3), INTENT(OUT) :: rank_section

INTEGER(ik) :: rank, yrmndr, zrmndr ! remainder

!------------------------------------------------------------------------------
! Power of 2 is handled here, because with the algorithm of CASE DEFAULT, Greedy suboptimality kicks in!  
! Have a look at the corresponding Matlab/Octave testing file!
! In example at size_mpi = 128 Processors, where CASE DEFAULT will deliver 125 Processors!
!------------------------------------------------------------------------------
IF ( domain == 0_ik ) THEN
   rank_section = [ 1, 1, 1 ]
ELSE
   rank = domain + 1 ! MPI starts at 0

   !------------------------------------------------------------------------------
   ! Calculate the rank_section out of my_rank and sections [ x, y, z ]
   ! Tested via Octave. Not fully implemented by 20210503
   !------------------------------------------------------------------------------
   zrmndr = MODULO(rank, sections(1)*sections(2))
   IF (zrmndr == 0_ik) THEN
      rank_section = [ sections(1), sections(2), (rank - zrmndr) / (sections(1)*sections(2)) ]
   ELSE
      rank_section(3) = (rank - zrmndr) / (sections(1) * sections(2)) 
      yrmndr = MODULO(zrmndr, sections(1))

      IF (yrmndr == 0_ik) THEN
         rank_section = [ sections(1), (zrmndr - yrmndr) / sections(1), rank_section(3)+1 ]
      ELSE
         rank_section = [ yrmndr, (zrmndr - yrmndr) / sections(1) + 1, rank_section(3) + 1 ]
      END IF
   END IF
END IF

END SUBROUTINE get_rank_section

   !------------------------------------------------------------------------------
   ! SUBROUTINE: ser_write_binary_ik1
   !------------------------------------------------------------------------------  
   !> @author Johannes Gebert, gebert@hlrs.de, HLRS/NUM
   !
   !> @brief
   !> Write raw binary data serially. 
   !
   !> @param[in] fh File handle
   !> @param[in] filename Name of the file
   !> @param[in] array Raw data
   !> @param[in] representation Optional swap of endianness
   !------------------------------------------------------------------------------
   SUBROUTINE ser_write_binary_ik1(fh, filename, array, representation)

      INTEGER(INT8), DIMENSION(:,:,:), INTENT(IN) :: array
      INCLUDE "./include_f90/ser_write_binary.aux.inc"
      
      END SUBROUTINE ser_write_binary_ik1

   !------------------------------------------------------------------------------
   ! SUBROUTINE: ser_write_binary_ik2
   !------------------------------------------------------------------------------  
   !> @author Johannes Gebert, gebert@hlrs.de, HLRS/NUM
   !
   !> @brief
   !> Write raw binary data serially. 
   !
   !> @param[in] fh File handle
   !> @param[in] filename Name of the file
   !> @param[in] array Raw data
   !> @param[in] representation Optional swap of endianness
   !------------------------------------------------------------------------------
   SUBROUTINE ser_write_binary_ik2(fh, filename, array, representation)

      INTEGER(INT16), DIMENSION(:,:,:), INTENT(IN) :: array
      INCLUDE "./include_f90/ser_write_binary.aux.inc"
      
   END SUBROUTINE ser_write_binary_ik2
   
   !------------------------------------------------------------------------------
   ! SUBROUTINE: ser_write_binary_ik4
   !------------------------------------------------------------------------------  
   !> @author Johannes Gebert, gebert@hlrs.de, HLRS/NUM
   !
   !> @brief
   !> Write raw binary data serially. 
   !
   !> @param[in] fh File handle
   !> @param[in] filename Name of the file
   !> @param[in] array Raw data
   !> @param[in] representation Optional swap of endianness
   !------------------------------------------------------------------------------
   SUBROUTINE ser_write_binary_ik4(fh, filename, array, representation)
   
   INTEGER(INT32), DIMENSION(:,:,:), INTENT(IN) :: array
   INCLUDE "./include_f90/ser_write_binary.aux.inc"
   
   END SUBROUTINE ser_write_binary_ik4
   
   !------------------------------------------------------------------------------
   ! SUBROUTINE: ser_write_binary_ik8
   !------------------------------------------------------------------------------  
   !> @author Johannes Gebert, gebert@hlrs.de, HLRS/NUM
   !
   !> @brief
   !> Write raw binary data serially. 
   !
   !> @param[in] fh File handle
   !> @param[in] filename Name of the file
   !> @param[in] array Raw data
   !> @param[in] representation Optional swap of endianness
   !------------------------------------------------------------------------------
   SUBROUTINE ser_write_binary_ik8(fh, filename, array, representation)
   
   INTEGER(INT64), DIMENSION(:,:,:), INTENT(IN) :: array
   INCLUDE "./include_f90/ser_write_binary.aux.inc"
   
   END SUBROUTINE ser_write_binary_ik8
   
   !------------------------------------------------------------------------------
   ! SUBROUTINE: ser_write_binary_rk4
   !------------------------------------------------------------------------------  
   !> @author Johannes Gebert, gebert@hlrs.de, HLRS/NUM
   !
   !> @brief
   !> Write raw binary data serially. 
   !
   !> @param[in] fh File handle
   !> @param[in] filename Name of the file
   !> @param[in] array Raw data
   !> @param[in] representation Optional swap of endianness
   !------------------------------------------------------------------------------
   SUBROUTINE ser_write_binary_rk4(fh, filename, array, representation)
   
   REAL(REAL32), DIMENSION(:,:,:), INTENT(IN) :: array
   INCLUDE "./include_f90/ser_write_binary.aux.inc"
   
   END SUBROUTINE ser_write_binary_rk4
   
   !------------------------------------------------------------------------------
   ! SUBROUTINE: ser_write_binary_rk8
   !------------------------------------------------------------------------------  
   !> @author Johannes Gebert, gebert@hlrs.de, HLRS/NUM
   !
   !> @brief
   !> Write raw binary data serially. 
   !
   !> @param[in] fh File handle
   !> @param[in] filename Name of the file
   !> @param[in] array Raw data
   !> @param[in] representation Optional swap of endianness
   !------------------------------------------------------------------------------
   SUBROUTINE ser_write_binary_rk8(fh, filename, array, representation)
   
      REAL(REAL64), DIMENSION(:,:,:), INTENT(IN) :: array
      INCLUDE "./include_f90/ser_write_binary.aux.inc"
   
   END SUBROUTINE ser_write_binary_rk8
   
   !------------------------------------------------------------------------------
   ! SUBROUTINE: ser_read_binary_ik2
   !------------------------------------------------------------------------------  
   !> @author Johannes Gebert, gebert@hlrs.de, HLRS/NUM
   !
   !> @brief
   !> Read raw binary data serially. Swap endianness if necessary.
   !
   !> @param[in] fh File handle
   !> @param[in] filename Name of the file
   !> @param[out] Array Raw data
   !> @param[in] representation Optional swap of endianness
   !------------------------------------------------------------------------------
   SUBROUTINE ser_read_binary_ik2(fh, filename, array, representation, displacement)
   
      INTEGER(INT16), DIMENSION(:,:,:), INTENT(OUT) :: array
      INCLUDE "./include_f90/ser_read_binary.aux.inc"
   
   END SUBROUTINE ser_read_binary_ik2

   !------------------------------------------------------------------------------
   ! SUBROUTINE: ser_read_binary_ik1
   !------------------------------------------------------------------------------  
   !> @author Johannes Gebert, gebert@hlrs.de, HLRS/NUM
   !
   !> @brief
   !> Read raw binary data serially. Swap endianness if necessary.
   !
   !> @param[in] fh File handle
   !> @param[in] filename Name of the file
   !> @param[out] Array Raw data
   !> @param[in] representation Optional swap of endianness
   !------------------------------------------------------------------------------
   SUBROUTINE ser_read_binary_ik1(fh, filename, array, representation, displacement)
   
      INTEGER(INT8), DIMENSION(:,:,:), INTENT(OUT) :: array
      INCLUDE "./include_f90/ser_read_binary.aux.inc"
      
   END SUBROUTINE ser_read_binary_ik1



   !------------------------------------------------------------------------------
   ! SUBROUTINE: ser_read_binary_ik4
   !------------------------------------------------------------------------------  
   !> @author Johannes Gebert, gebert@hlrs.de, HLRS/NUM
   !
   !> @brief
   !> Read raw binary data serially. 
   !
   !> @param[in] fh File handle
   !> @param[in] filename Name of the file
   !> @param[out] Array Raw data
   !> @param[in] representation Optional swap of endianness
   !------------------------------------------------------------------------------
   SUBROUTINE ser_read_binary_ik4(fh, filename, array, representation, displacement)
   
   INTEGER(INT32), DIMENSION(:,:,:), INTENT(OUT) :: array
   INCLUDE "./include_f90/ser_read_binary.aux.inc"
   
   END SUBROUTINE ser_read_binary_ik4
   
   !------------------------------------------------------------------------------
   ! SUBROUTINE: ser_read_binary_ik8
   !------------------------------------------------------------------------------  
   !> @author Johannes Gebert, gebert@hlrs.de, HLRS/NUM
   !
   !> @brief
   !> Read raw binary data serially. 
   !
   !> @param[in] fh File handle
   !> @param[in] filename Name of the file
   !> @param[out] Array Raw data
   !> @param[in] representation Optional swap of endianness
   !------------------------------------------------------------------------------
   SUBROUTINE ser_read_binary_ik8(fh, filename, array, representation, displacement)
   
   INTEGER(INT64), DIMENSION(:,:,:), INTENT(OUT) :: array
   INCLUDE "./include_f90/ser_read_binary.aux.inc"
   
   END SUBROUTINE ser_read_binary_ik8
   
   !------------------------------------------------------------------------------
   ! SUBROUTINE: ser_read_binary_rk4
   !------------------------------------------------------------------------------  
   !> @author Johannes Gebert, gebert@hlrs.de, HLRS/NUM
   !
   !> @brief
   !> Read raw binary data serially. 
   !
   !> @param[in] fh File handle
   !> @param[in] filename Name of the file
   !> @param[out] Array Raw data
   !> @param[in] representation Optional swap of endianness
   !------------------------------------------------------------------------------
   SUBROUTINE ser_read_binary_rk4(fh, filename, array, representation, displacement)
   
   REAL(REAL32), DIMENSION(:,:,:), INTENT(OUT) :: array
   INCLUDE "./include_f90/ser_read_binary.aux.inc"
   
   END SUBROUTINE ser_read_binary_rk4
   
   !------------------------------------------------------------------------------
   ! SUBROUTINE: ser_read_binary_rk8
   !------------------------------------------------------------------------------  
   !> @author Johannes Gebert, gebert@hlrs.de, HLRS/NUM
   !
   !> @brief
   !> Read raw binary data serially. 
   !
   !> @param[in] fh File handle
   !> @param[in] filename Name of the file
   !> @param[out] Array Raw data
   !> @param[in] representation Optional swap of endianness
   !------------------------------------------------------------------------------
   SUBROUTINE ser_read_binary_rk8(fh, filename, array, representation, displacement)
   
   REAL(REAL64), DIMENSION(:,:,:), INTENT(OUT) :: array
   INCLUDE "./include_f90/ser_read_binary.aux.inc"
   
   END SUBROUTINE ser_read_binary_rk8

   
!------------------------------------------------------------------------------
! SUBROUTINE: uik2_to_ik2
!------------------------------------------------------------------------------  
!> @author Johannes Gebert - HLRS - NUM - gebert@hlrs.de
!
!> @brief
!> Convert unsigned int2 to int 2 data. @Datarepresenation: 
!> NATIVE=LittleEndian on file system, EXTERNAL32 --> BigEndian. 
!> Please check and test if you need this feature!! Depends on Hardware.
!
!> @description
!> Fortran does not know this shit. Therefore a workaround...
!
!> @param[in] subarray_in Input data
!> @param[out] subarray_out Output data
!------------------------------------------------------------------------------  
SUBROUTINE uik2_to_ik2(subarray)

   INTEGER(INT16), DIMENSION (:,:,:), INTENT(INOUT) :: subarray
   INTEGER(ik) :: ii, jj, kk
   INTEGER(ik), DIMENSION(3) :: shp
   
   INTEGER(INT32), DIMENSION (:,:,:), ALLOCATABLE :: temp
   
   !------------------------------------------------------------------------------  
   ! Storing the array with + 65536 will cut off the image.
   ! At least INT32 required. All of the required variables are INT32.
   !------------------------------------------------------------------------------  
   shp = SHAPE(subarray)
   
   ALLOCATE(temp(shp(1), shp(2), shp(3)))
   temp = 0
   
   DO kk=1, shp(3)
   DO jj=1, shp(2)
   DO ii=1, shp(1)
      IF(subarray(ii,jj,kk) .LT. 0) THEN
         temp(ii,jj,kk) = INT(subarray(ii,jj,kk), INT32) + INT(65536, INT32)
      ELSE
         temp(ii,jj,kk) = INT(subarray(ii,jj,kk), INT32)
      END IF 
   END DO
   END DO
   END DO
   
   subarray = INT(temp - 32768, INT16)
   
   DEALLOCATE(temp)
   END SUBROUTINE uik2_to_ik2
   
   
   !------------------------------------------------------------------------------
   ! SUBROUTINE: uik2_to_ik4
   !------------------------------------------------------------------------------  
   !> @author Johannes Gebert - HLRS - NUM - gebert@hlrs.de
   !
   !> @brief
   !> Convert unsigned int2 to int 2 data. @Datarepresenation: 
   !> NATIVE=LittleEndian on file system, EXTERNAL32 --> BigEndian. 
   !> Please check and test if you need this feature!! Depends on Hardware.
   !
   !> @description
   !> Fortran does not know this shit. Therefore a workaround...
   !
   !> @param[in] subarray_in Input data
   !> @param[out] subarray_out Output data
   !------------------------------------------------------------------------------  
   SUBROUTINE uik2_to_ik4(subarray_in, subarray_out)
   
   INTEGER(INT16), DIMENSION (:,:,:), INTENT(IN) :: subarray_in
   INTEGER(INT32), DIMENSION (:,:,:), ALLOCATABLE, INTENT(OUT) :: subarray_out
   INTEGER(ik) :: ii, jj, kk
   INTEGER(ik), DIMENSION(3) :: shp
   
   !------------------------------------------------------------------------------  
   ! Storing the array with + 65536 will cut off the image.
   ! At least INT32 required. All of the required variables are INT32.
   !------------------------------------------------------------------------------  
   INTEGER(INT32), PARAMETER :: conv_param=0, offset=65536
   
   shp = SHAPE(subarray_in)
   
   ALLOCATE(subarray_out(shp(1), shp(2), shp(3)))
   subarray_out = INT(0, INT32)
   
   subarray_out = INT(subarray_in, INT32)
   
   DO kk=1, shp(3)
   DO jj=1, shp(2)
   DO ii=1, shp(1)
      IF(subarray_out(ii,jj,kk) .LT. conv_param) THEN
         subarray_out(ii,jj,kk) = subarray_out(ii,jj,kk) + offset
      END IF 
   END DO
   END DO
   END DO
   
   
   END SUBROUTINE uik2_to_ik4
END MODULE ser_binary