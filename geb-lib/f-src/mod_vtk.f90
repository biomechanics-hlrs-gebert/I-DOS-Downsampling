!------------------------------------------------------------------------------
! MODULE: vtk_meta_data
!------------------------------------------------------------------------------
!> @author Johannes Gebert - HLRS - NUM - gebert@hlrs.de
!
!> @brief:
!> Module for reading/writing vtk-structured-point files
!------------------------------------------------------------------------------
MODULE vtk_meta_data

USE ISO_FORTRAN_ENV
USE global_std
USE strings
USE user_interaction
USE ser_binary

IMPLICIT NONE

INTERFACE write_ser_vtk
   MODULE PROCEDURE write_ser_vtk_ik1
   MODULE PROCEDURE write_ser_vtk_ik2
   MODULE PROCEDURE write_ser_vtk_ik4
   MODULE PROCEDURE write_ser_vtk_rk8
END INTERFACE write_ser_vtk

CONTAINS

!------------------------------------------------------------------------------
! SUBROUTINE: write_vtk_struct_points_header
!------------------------------------------------------------------------------  
!> @author Johannes Gebert, gebert@hlrs.de, HLRS/NUM
!
!> @brief
!> Write a *.vtk structured points header
!
!> @param[in] fh File handle
!> @param[in] filename Name of the file
!> @param[in] type Data type
!> @param[in] spcng Distance between to voxels/scalars
!> @param[in] origin physical origin (mm)
!> @param[in] dims Amount of voxels/scalars per direction
!------------------------------------------------------------------------------
SUBROUTINE write_vtk_struct_points_header (fh, filename, type, spcng, origin, dims)

! It's HIGHLY recommended to check the existence of the output file prior to CALLing this
! Subroutine! Otherwise the program will crash. It's not double-checkd here, because this
! sequence often is placed at the very end of a program, which may run some time.

INTEGER  (ik), INTENT(IN) :: fh, dims(3)
CHARACTER(*) , INTENT(IN) :: filename, type
REAL(rk), INTENT(IN) :: spcng(3), origin(3)

CHARACTER(scl) :: datatype=''
LOGICAL :: exist

INQUIRE(UNIT=fh, exist=exist)

IF(.NOT. exist) THEN
   OPEN(UNIT=fh, FILE=TRIM(filename), ACTION='WRITE', STATUS='OLD', POSITION='APPEND')
END IF

SELECT CASE(TRIM(ADJUSTL(type)))
   CASE('ik1'); datatype = "char"
   CASE('uik2'); datatype = "unsigned_short"
   CASE('ik2'); datatype = "short"
   CASE('ik4'); datatype = "int"
   CASE('rk4'); datatype = "float"
   CASE('rk8'); datatype = "double"
   CASE DEFAULT
      CALL print_err_stop(fh, "No valid datatype given.", 1)
END SELECT

OPEN(UNIT=fh, FILE=TRIM(filename), ACTION='WRITE', STATUS='NEW')

WRITE(fh,'(A)')          "# vtk DataFile Version 4.2" ! Compatibility issue
WRITE(fh,'(A)')          "vtk output"
WRITE(fh,'(A)')          "BINARY"
WRITE(fh,'(A)')          "DATASET STRUCTURED_POINTS"
WRITE(fh,'(A,3(I5))')    "DIMENSIONS", dims
WRITE(fh,'(A,3(F11.6))') "SPACING ", spcng
WRITE(fh,'(A,3(F11.6))') "ORIGIN ", origin
WRITE(fh,'(A, I0)')      "POINT_DATA ", PRODUCT(INT(dims, INT64))
WRITE(fh,'(A)')          "SCALARS DICOMImage "//TRIM(ADJUSTL(datatype))
WRITE(fh,'(A)')          "LOOKUP_TABLE default"

CLOSE(UNIT=fh)
END SUBROUTINE write_vtk_struct_points_header

!------------------------------------------------------------------------------
! SUBROUTINE: write_vtk_struct_points_footer
!------------------------------------------------------------------------------  
!> @author Johannes Gebert, gebert@hlrs.de, HLRS/NUM
!
!> @brief
!> Write a *.vtk structured points footer
!
!> @param[in] fh File handle
!> @param[in] filename File name
!------------------------------------------------------------------------------
SUBROUTINE write_vtk_struct_points_footer (fh, filename)

INTEGER(ik), INTENT(IN) :: fh
CHARACTER(*) :: filename

LOGICAL :: opened

INQUIRE(UNIT=fh, opened=opened)

IF(.NOT. opened) THEN
   OPEN(UNIT=fh, FILE=TRIM(filename), ACTION='WRITE', STATUS='OLD', POSITION='APPEND')
END IF

WRITE(fh ,'(A)')''
WRITE(fh ,'(A)')"METADATA"
WRITE(fh ,'(A)')"INFORMATION 0"
WRITE(fh ,'(A)')

CLOSE(UNIT=fh)
END SUBROUTINE write_vtk_struct_points_footer


!------------------------------------------------------------------------------
! SUBROUTINE: read_vtk_meta
!------------------------------------------------------------------------------  
!> @author Johannes Gebert, gebert@hlrs.de, HLRS/NUM
!
!> @brief
!> Read a *.vtk structured points header (footer not relevant)
!
!> @param[in] filename File name
!> @param[out] disp Displacement, length of header (bytes)
!> @param[out] dims Voxels per direction
!> @param[out] origin Physical (mm) origin
!> @param[out] spcng Physical distance between two voxels (mm)
!> @param[out] type Data type contained in binary blob
!------------------------------------------------------------------------------
SUBROUTINE read_vtk_meta(filename, disp, dims, origin, spcng, type)

CHARACTER(*), INTENT(IN) :: filename
INTEGER(ik), INTENT(OUT) :: disp
INTEGER(ik), INTENT(OUT) :: dims(3)
REAL   (rk), INTENT(OUT) :: spcng(3), origin(3)
CHARACTER(*), INTENT(OUT) :: type

!-- Initialize variables in case they're not used
INTEGER  (ik) :: ii=0, ntokens, fh

CHARACTER(mcl) :: line
CHARACTER(mcl) :: tokens(100)
CHARACTER(mcl), DIMENSION(3) :: token

!------------------------------------------------------------------------------
! Determine a new unit
!------------------------------------------------------------------------------  
fh = give_new_unit()
OPEN(UNIT=fh, FILE=TRIM(filename), STATUS="OLD")

disp=0

DO ii=1,10
   READ(fh,'(A)') line
   disp=disp+LEN(TRIM(line))+1_ik ! eol characters, white charcter
   
   CALL parse(str=line,delims=" ",args=tokens,nargs=ntokens)

   IF (ntokens > 0) THEN
   
      SELECT CASE(tokens(1))
         CASE('DIMENSIONS'); READ(tokens(2:4),'(I15)') dims(1:3)
         CASE('SPACING'); READ(tokens(2:4),'(F15.6)') spcng(1:3)  
         CASE('ORIGIN'); READ(tokens(2:4),'(F15.6)') origin(1:3)  
         CASE('DATASET')
            IF (tokens(2) /= "STRUCTURED_POINTS") THEN
               mssg = "The input file "//TRIM(filename)//" does not contain STRUCTURED_POINTS!"
               CALL print_err_stop(std_out, mssg, 1)
            END IF

         CASE('SCALARS')
            token(3) = tokens(3)

            SELECT CASE( TRIM( token(3) ) )
               CASE('float') ; type = 'rk4'
               CASE('double'); type = 'rk8'
               CASE('int')   ; type = 'ik4'
               CASE('short') ; type = 'ik2'
               CASE('unsigned_short'); type = 'uik2'
               CASE DEFAULT
                  WRITE(*,'(A)') "No valid type given in *.vtk File." 
            END SELECT
      END SELECT
   END IF !ntokens <0
END DO

CLOSE(fh)

END SUBROUTINE read_vtk_meta

!------------------------------------------------------------------------------
! SUBROUTINE: write_ser_vtk_ik1
!------------------------------------------------------------------------------  
!> @author Johannes Gebert, gebert@hlrs.de, HLRS/NUM
!
!> @brief
!> Basically a simple wrapper
!
!> @param[in] filename File name
!> @param[in] type Data type contained in binary blob
!> @param[in] spcng Physical distance between two voxels (mm)
!> @param[in] dims Voxels per direction
!> @param[in] origin Physical (mm) origin
!> @param[in] array Data
!------------------------------------------------------------------------------
SUBROUTINE write_ser_vtk_ik1(filename, type, spcng, dims, origin, array)

INTEGER(INT8), DIMENSION(:,:,:), INTENT(IN) :: array
INCLUDE "./include_f90/write_ser_vtk.aux.inc"

END SUBROUTINE write_ser_vtk_ik1


!------------------------------------------------------------------------------
! SUBROUTINE: write_ser_vtk_ik2
!------------------------------------------------------------------------------  
!> @author Johannes Gebert, gebert@hlrs.de, HLRS/NUM
!
!> @brief
!> Basically a simple wrapper
!
!> @param[in] filename File name
!> @param[in] type Data type contained in binary blob
!> @param[in] spcng Physical distance between two voxels (mm)
!> @param[in] dims Voxels per direction
!> @param[in] origin Physical (mm) origin
!> @param[in] array Data
!------------------------------------------------------------------------------
SUBROUTINE write_ser_vtk_ik2(filename, type, spcng, dims, origin, array)

INTEGER(INT16), DIMENSION(:,:,:), INTENT(IN) :: array
INCLUDE "./include_f90/write_ser_vtk.aux.inc"

END SUBROUTINE write_ser_vtk_ik2

!------------------------------------------------------------------------------
! SUBROUTINE: write_ser_vtk_ik4
!------------------------------------------------------------------------------  
!> @author Johannes Gebert, gebert@hlrs.de, HLRS/NUM
!
!> @brief
!> Basically a simple wrapper
!
!> @param[in] filename File name
!> @param[in] type Data type contained in binary blob
!> @param[in] spcng Physical distance between two voxels (mm)
!> @param[in] dims Voxels per direction
!> @param[in] origin Physical (mm) origin
!> @param[in] array Data
!------------------------------------------------------------------------------
SUBROUTINE write_ser_vtk_ik4(filename, type, spcng, dims, origin, array)

INTEGER(INT32), DIMENSION(:,:,:), INTENT(IN) :: array
INCLUDE "./include_f90/write_ser_vtk.aux.inc"

END SUBROUTINE write_ser_vtk_ik4

!------------------------------------------------------------------------------
! SUBROUTINE: write_ser_vtk_rk8
!------------------------------------------------------------------------------  
!> @author Johannes Gebert, gebert@hlrs.de, HLRS/NUM
!
!> @brief
!> Basically a simple wrapper
!
!> @param[in] filename File name
!> @param[in] type Data type contained in binary blob
!> @param[in] spcng Physical distance between two voxels (mm)
!> @param[in] dims Voxels per direction
!> @param[in] origin Physical (mm) origin
!> @param[in] array Data
!------------------------------------------------------------------------------
SUBROUTINE write_ser_vtk_rk8(filename, type, spcng, dims, origin, array)

REAL(REAL64), DIMENSION(:,:,:), INTENT(IN) :: array
INCLUDE "./include_f90/write_ser_vtk.aux.inc"

END SUBROUTINE write_ser_vtk_rk8

END MODULE vtk_meta_data
