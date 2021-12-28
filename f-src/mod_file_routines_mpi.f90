!------------------------------------------------------------------------------
! MODULE: file_routines_mpi
!------------------------------------------------------------------------------
!> @author Johannes Gebert - HLRS - NUM - gebert@hlrs.de
!
!> @brief:
!> Module for reading/writing files parallely.
!
!> @description
!> There's one routine for reading raw blobs for each datatype. 
!> And a switch for big/little endian. From the viewpoint of clean code, it is 
!> not well done. But creating subroutines for preparing a read raw and 
!> releasing/finishing read raw results in three calls and an allocate in the 
!> main program. Otherwise, it results in an mpi_read_raw_prepare subroutine 
!> with a specific data type, that itself must call the actual 
!> MPI_FILE_READ_ALL routine. Since the preparation is the biggest part, the 
!> calls won't be shorter significantly.
!------------------------------------------------------------------------------
MODULE file_routines_mpi

USE ISO_FORTRAN_ENV
USE global_std
USE messages_errors
USE MPI

IMPLICIT NONE
Interface mpi_read_raw
   Module Procedure mpi_read_raw_rk4
   Module Procedure mpi_read_raw_rk8
   Module Procedure mpi_read_raw_ik4
   Module Procedure mpi_read_raw_ik2
   Module Procedure mpi_read_raw_uik2
End Interface mpi_read_raw

Interface mpi_write_raw
   Module Procedure mpi_write_raw_ik4
   Module Procedure mpi_write_raw_ik2
End Interface mpi_write_raw

CONTAINS

!------------------------------------------------------------------------------
! SUBROUTINE: write_vtk_struct_points_header
!------------------------------------------------------------------------------  
!> @author Johannes Gebert, gebert@hlrs.de, HLRS/NUM
!
!> @brief
!> Write a *.vtk structured points header
!
!> @param[in] application_name Name of the program
!> @param[in] revision Ravision of the program
!------------------------------------------------------------------------------
SUBROUTINE write_vtk_struct_points_header (fh, filename, type, spcng, origin, dims)

! It's HIGHLY recommended to check the existence of the output file prior to CALLing this
! Subroutine! Otherwise the program will crash. It's not double-checkd here, because this
! sequence often is placed at the very end of a program, which may run some time.

INTEGER  (KIND=ik), INTENT(IN) :: fh
CHARACTER(len=*)  , INTENT(IN) :: filename
CHARACTER(LEN=*)  , INTENT(IN) :: type
REAL     (KIND=rk), INTENT(IN), DIMENSION(3) :: spcng
REAL     (KIND=rk), INTENT(IN), DIMENSION(3) :: origin
INTEGER  (KIND=ik), INTENT(IN), DIMENSION(3) :: dims

CHARACTER(LEN=scl) :: datatype=''
LOGICAL :: exist

INQUIRE(UNIT=fh, exists=exist)

IF(.NOT. exist) THEN
   OPEN(UNIT=fh, FILE=TRIM(filename), ACTION='WRITE', STATUS='OLD', POSITION='APPEND')
END IF

SELECT CASE(TRIM(ADJUSTL(type)))
   CASE('uik2'); datatype = "unsigned_short"
   CASE('ik2'); datatype = "short"
   CASE('ik4'); datatype = "int"
   CASE('rk4'); datatype = "float"
   CASE('rk8'); datatype = "double"
   CASE DEFAULT
      CALL print_err_stop(fh, "No valid datatype given.", 1)
END SELECT

OPEN(UNIT=fh, FILE=TRIM(filename), ACTION='WRITE', STATUS='NEW')

WRITE(fh,'(A)')           "# vtk DataFile Version 4.2" ! Compatibility issue
WRITE(fh,'(A)')           "vtk output"
WRITE(fh,'(A)')           "BINARY"
WRITE(fh,'(A)')           "DATASET STRUCTURED_POINTS"
WRITE(fh,'(A,3(I5))')     "DIMENSIONS", dims
WRITE(fh,'(A,3(F11.6))')  "SPACING ", spcng
WRITE(fh,'(A,3(F11.6))')  "ORIGIN ", origin
WRITE(fh,'(A)') "SCALARS DICOMImage "//TRIM(ADJUSTL(datatype))
WRITE(fh,'(A)')            "LOOKUP_TABLE default"
WRITE(fh,'(A)')          ''

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

INTEGER(KIND=ik), INTENT(IN) :: fh
CHARACTER(len=*) :: filename

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
!> @param[in] hdr_lngth Length of header (bytes)
!> @param[in] dims Voxels per direction
!> @param[in] origin Physical (mm) origin
!> @param[in] spcng Physical distance between two voxels (mm)
!> @param[in] type Data type contained in binary blob
!------------------------------------------------------------------------------
SUBROUTINE read_vtk_meta(filename, hdr_lngth, dims, origin, spcng, type)

CHARACTER(len=*)  , INTENT(IN)  :: filename
INTEGER  (KIND=ik), INTENT(OUT) :: hdr_lngth
INTEGER  (KIND=ik), DIMENSION(3) , INTENT(OUT) :: dims
REAL     (KIND=rk), DIMENSION(3) , INTENT(OUT) :: origin
REAL     (KIND=rk), DIMENSION(3) , INTENT(OUT) :: spcng
CHARACTER(len=*), INTENT(OUT) :: type

!-- Initialize variables in case they're not used
INTEGER  (KIND=ik) :: ii=0, ntokens, fh

CHARACTER(len=mcl) :: line
CHARACTER(len=mcl) :: tokens(100)
CHARACTER(len=mcl), DIMENSION(3) :: token

!------------------------------------------------------------------------------
! Determine a new unit
!------------------------------------------------------------------------------  
fh = give_new_unit
OPEN(UNIT=fh, FILE=TRIM(filename), STATUS="OLD")

hdr_lngth=0

DO ii=1,10
   READ(fh,'(A)') line
   hdr_lngth=hdr_lngth+LEN(TRIM(line))+1_ik ! eol characters, white charcter
   
   CALL parse(str=line,delims=" ",args=tokens,nargs=ntokens)

   IF (ntokens > 0) THEN
   
      IF (tokens(1) == "DIMENSIONS") THEN
         READ(tokens(2),'(I0)')  dims(1)
         READ(tokens(3),'(I0)')  dims(2)
         READ(tokens(4),'(I0)')  dims(3)

      ELSE IF (tokens(1) == "SPACING") THEN
         READ(tokens(2),'(F15.6)') spcng(1)  
         READ(tokens(3),'(F15.6)') spcng(2)  
         READ(tokens(4),'(F15.6)') spcng(3)  

      ELSE IF ((tokens(1) == "DATASET") .AND. (tokens(2) /= "STRUCTURED_POINTS")) THEN
            mssg = "The input file "//TRIM(filename)//" does not contain STRUCTURED_POINTS!"
            CALL print_err_stop(std_out, txt, 1)
         END IF

      ELSE IF (tokens(1) == "ORIGIN") THEN
         READ(tokens(2), '(F15.6)') origin(1)  
         READ(tokens(3), '(F15.6)') origin(2)  
         READ(tokens(4), '(F15.6)') origin(3)  

      ELSE IF (tokens(1) == "SCALARS") THEN
         !-- Get data type of the vtk-file
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
      END IF
   END IF !ntokens <0
END DO

CLOSE(fh)

END SUBROUTINE read_vtk_meta


!------------------------------------------------------------------------------
! SUBROUTINE: mpi_read_raw_ik2
!------------------------------------------------------------------------------  
!> @author Johannes Gebert - HLRS - NUM - gebert@hlrs.de
!
!> @brief
!> Read the raw int2 data of a binary blob
!
!> @param[in] fh File handle
!> @param[in] filename File name
!> @param[in] disp Length of the header (bytes)
!> @param[in] dims Amount of voxels per direction
!> @param[in] subarray_dims Amount of voxels per direction of the subarray
!> @param[in] subarray_origin Physical origin of the data set
!> @param[in] subarray int4 data
!> @param[in] bigendian true or false (little endian)
!------------------------------------------------------------------------------  
SUBROUTINE mpi_read_raw_ik2(fh, filename, disp, dims, subarray_dims, subarray_origin, subarray, bigendian)

CHARACTER(LEN=*)             , INTENT(IN) :: filename
INTEGER(KIND=ik)             , INTENT(IN) :: fh
INTEGER(KIND=MPI_OFFSET_KIND), INTENT(IN) :: disp
INTEGER(KIND=ik),DIMENSION(3), INTENT(IN) :: dims, subarray_dims, subarray_origin
INTEGER(KIND=INT16), DIMENSION (:,:,:), ALLOCATABLE, INTENT(OUT) :: subarray
LOGICAL, INTENT(IN) :: bigendian

INTEGER(KIND=ik) :: ierr, type_subarray
CHARACTER(LEN=scl) :: datarep

datarep = 'INTERNAL'
IF (bigendian) datarep = 'EXTERNAL32'

CALL MPI_FILE_OPEN(MPI_COMM_WORLD, TRIM(filename), MPI_MODE_RDONLY, MPI_INFO_NULL, fh, ierr)

CALL MPI_TYPE_CREATE_SUBARRAY (3_mik, dims, subarray_dims, subarray_origin - 1_mik, &
   MPI_ORDER_FORTRAN, MPI_INTEGER2, type_subarray,ierr)

CALL MPI_TYPE_COMMIT(type_subarray, ierr)

CALL MPI_FILE_SET_VIEW(fh, disp, MPI_INTEGER2, type_subarray, TRIM(datarep), MPI_INFO_NULL, ierr)

ALLOCATE(subarray(subarray_dims(1), subarray_dims(2), subarray_dims(3)))

CALL MPI_FILE_READ_ALL(fh, subarray, SIZE(subarray), MPI_INTEGER, MPI_STATUS_IGNORE, ierr)

CALL MPI_TYPE_FREE(type_subarray, ierr)

CALL MPI_FILE_CLOSE(fh, ierr)

END SUBROUTINE mpi_read_raw_ik2

!------------------------------------------------------------------------------
! SUBROUTINE: mpi_read_raw_ik4
!------------------------------------------------------------------------------  
!> @author Johannes Gebert - HLRS - NUM - gebert@hlrs.de
!
!> @brief
!> Read the raw int4 data of a binary blob
!
!> @param[in] fh File handle
!> @param[in] filename File name
!> @param[in] disp Length of the header (bytes)
!> @param[in] dims Amount of voxels per direction
!> @param[in] subarray_dims Amount of voxels per direction of the subarray
!> @param[in] subarray_origin Physical origin of the data set
!> @param[in] subarray int4 data
!> @param[in] bigendian true or false (little endian)
!------------------------------------------------------------------------------  
SUBROUTINE mpi_read_raw_ik4(fh, filename, disp, dims, subarray_dims, subarray_origin, subarray, bigendian)

CHARACTER(LEN=*)             , INTENT(IN) :: filename
INTEGER(KIND=ik)             , INTENT(IN) :: fh
INTEGER(KIND=MPI_OFFSET_KIND), INTENT(IN) :: disp
INTEGER(KIND=ik),DIMENSION(3), INTENT(IN) :: dims, subarray_dims, subarray_origin
INTEGER(KIND=INT32), DIMENSION (:,:,:), ALLOCATABLE, INTENT(OUT) :: subarray
LOGICAL, INTENT(IN) :: bigendian

INTEGER(KIND=ik) :: ierr, type_subarray
CHARACTER(LEN=scl) :: datarep

datarep = 'INTERNAL'
IF (bigendian) datarep = 'EXTERNAL32'

CALL MPI_FILE_OPEN(MPI_COMM_WORLD, TRIM(filename), MPI_MODE_RDONLY, MPI_INFO_NULL, fh, ierr)

CALL MPI_TYPE_CREATE_SUBARRAY (3_mik, dims, subarray_dims, subarray_origin - 1_mik, &
   MPI_ORDER_FORTRAN, MPI_INTEGER, type_subarray,ierr)

CALL MPI_TYPE_COMMIT(type_subarray, ierr)

CALL MPI_FILE_SET_VIEW(fh, disp, MPI_INTEGER, type_subarray, TRIM(datarep), MPI_INFO_NULL, ierr)

ALLOCATE(subarray(subarray_dims(1), subarray_dims(2), subarray_dims(3)))

CALL MPI_FILE_READ_ALL(fh, subarray, SIZE(subarray), MPI_INTEGER, MPI_STATUS_IGNORE, ierr)

CALL MPI_TYPE_FREE(type_subarray, ierr)

CALL MPI_FILE_CLOSE(fh, ierr)

END SUBROUTINE mpi_read_raw_ik4


!------------------------------------------------------------------------------
! SUBROUTINE: mpi_read_raw_uik2
!------------------------------------------------------------------------------  
!> @author Johannes Gebert - HLRS - NUM - gebert@hlrs.de
!
!> @brief
!> Read the raw int2 data of a binary blob
!
!> @description
!> Unsigned short. Fortran does not know this shit. Therefore a workaround...
!
!> @param[in] fh File handle
!> @param[in] filename File name
!> @param[in] disp Length of the header (bytes)
!> @param[in] dims Amount of voxels per direction
!> @param[in] subarray_dims Amount of voxels per direction of the subarray
!> @param[in] subarray_origin Physical origin of the data set
!> @param[in] subarray int4 data
!> @param[in] bigendian true or false (little endian)
!------------------------------------------------------------------------------  
SUBROUTINE mpi_read_raw_uik2(fh, filename, disp, dims, subarray_dims, subarray_origin, subarray, bigendian)

CHARACTER(LEN=*)             , INTENT(IN) :: filename
INTEGER(KIND=ik)             , INTENT(IN) :: fh
INTEGER(KIND=MPI_OFFSET_KIND), INTENT(IN) :: disp
INTEGER(KIND=ik),DIMENSION(3), INTENT(IN) :: dims, subarray_dims, subarray_origin
INTEGER(KIND=INT16), DIMENSION (:,:,:), ALLOCATABLE, INTENT(OUT) :: subarray
LOGICAL, INTENT(IN) :: bigendian

INTEGER(KIND=ik) :: ierr, type_subarray
CHARACTER(LEN=scl) :: datarep

datarep = 'INTERNAL'
IF (bigendian) datarep = 'EXTERNAL32'

CALL MPI_FILE_OPEN(MPI_COMM_WORLD, TRIM(filename), MPI_MODE_RDONLY, MPI_INFO_NULL, fh, ierr)

CALL MPI_TYPE_CREATE_SUBARRAY (3_mik, dims, subarray_dims, subarray_origin - 1_mik, &
   MPI_ORDER_FORTRAN, MPI_INTEGER2, type_subarray,ierr)

CALL MPI_TYPE_COMMIT(type_subarray, ierr)

CALL MPI_FILE_SET_VIEW(fh, disp, MPI_INTEGER2, type_subarray, TRIM(datarep), MPI_INFO_NULL, ierr)

ALLOCATE(subarray(subarray_dims(1), subarray_dims(2), subarray_dims(3)))

CALL MPI_FILE_READ_ALL(fh, subarray, SIZE(subarray), MPI_INTEGER, MPI_STATUS_IGNORE, ierr)

CALL MPI_TYPE_FREE(type_subarray, ierr)

CALL MPI_FILE_CLOSE(fh, ierr)

! Not so pretty workaround
DO kk=1, subarray_dims(1)
DO jj=1, subarray_dims(2)
DO ii=1, subarray_dims(3)
   IF(subarray(ii,jj,kk)<=0) subarray(ii,jj,kk) = subarray(ii,jj,kk) + 65536_ik
END DO
END DO
END DO

subarray = subarray - 32768_ik

END SUBROUTINE mpi_read_raw_uik2

!------------------------------------------------------------------------------
! SUBROUTINE: mpi_read_raw_rk4
!------------------------------------------------------------------------------  
!> @author Johannes Gebert - HLRS - NUM - gebert@hlrs.de
!
!> @brief
!> Read the raw real 4 (single precision) data of a binary blob
!
!> @param[in] fh File handle
!> @param[in] filename File name
!> @param[in] disp Length of the header (bytes)
!> @param[in] dims Amount of voxels per direction
!> @param[in] subarray_dims Amount of voxels per direction of the subarray
!> @param[in] subarray_origin Physical origin of the data set
!> @param[in] subarray int4 data
!> @param[in] bigendian true or false (little endian)
!------------------------------------------------------------------------------  
SUBROUTINE mpi_read_raw_rk4(fh, filename, disp, dims, subarray_dims, subarray_origin, subarray, bigendian)

CHARACTER(LEN=*)             , INTENT(IN) :: filename
INTEGER(KIND=ik)             , INTENT(IN) :: fh
INTEGER(KIND=MPI_OFFSET_KIND), INTENT(IN) :: disp
INTEGER(KIND=ik),DIMENSION(3), INTENT(IN) :: dims, subarray_dims, subarray_origin
INTEGER(KIND=REAL32), DIMENSION (:,:,:), ALLOCATABLE, INTENT(OUT) :: subarray
LOGICAL, INTENT(IN) :: bigendian

INTEGER(KIND=ik) :: ierr, type_subarray
CHARACTER(LEN=scl) :: datarep

datarep = 'INTERNAL'
IF (bigendian) datarep = 'EXTERNAL32'

CALL MPI_FILE_OPEN(MPI_COMM_WORLD, TRIM(filename), MPI_MODE_RDONLY, MPI_INFO_NULL, fh, ierr)

CALL MPI_TYPE_CREATE_SUBARRAY (3_mik, dims, subarray_dims, subarray_origin - 1_mik, &
   MPI_ORDER_FORTRAN, MPI_REAL, type_subarray,ierr)

CALL MPI_TYPE_COMMIT(type_subarray, ierr)

CALL MPI_FILE_SET_VIEW(fh, disp, MPI_REAL, type_subarray, TRIM(datarep), MPI_INFO_NULL, ierr)

ALLOCATE(subarray(subarray_dims(1), subarray_dims(2), subarray_dims(3)))

CALL MPI_FILE_READ_ALL(fh, subarray, SIZE(subarray), MPI_INTEGER, MPI_STATUS_IGNORE, ierr)

CALL MPI_TYPE_FREE(type_subarray, ierr)

CALL MPI_FILE_CLOSE(fh, ierr)

END SUBROUTINE mpi_read_raw_rk4

!------------------------------------------------------------------------------
! SUBROUTINE: mpi_read_raw_rk8
!------------------------------------------------------------------------------  
!> @author Johannes Gebert - HLRS - NUM - gebert@hlrs.de
!
!> @brief
!> Read the raw real 8 (double precision) data of a binary blob
!
!> @param[in] fh File handle
!> @param[in] filename File name
!> @param[in] disp Length of the header (bytes)
!> @param[in] dims Amount of voxels per direction
!> @param[in] subarray_dims Amount of voxels per direction of the subarray
!> @param[in] subarray_origin Physical origin of the data set
!> @param[in] subarray int4 data
!> @param[in] bigendian true or false (little endian)
!------------------------------------------------------------------------------  
SUBROUTINE mpi_read_raw_rk8(fh, filename, disp, dims, subarray_dims, subarray_origin, subarray, bigendian)

CHARACTER(LEN=*)             , INTENT(IN) :: filename
INTEGER(KIND=ik)             , INTENT(IN) :: fh
INTEGER(KIND=MPI_OFFSET_KIND), INTENT(IN) :: disp
INTEGER(KIND=ik),DIMENSION(3), INTENT(IN) :: dims, subarray_dims, subarray_origin
INTEGER(KIND=REAL32), DIMENSION (:,:,:), ALLOCATABLE, INTENT(OUT) :: subarray
LOGICAL, INTENT(IN) :: bigendian

INTEGER(KIND=ik) :: ierr, type_subarray
CHARACTER(LEN=scl) :: datarep

datarep = 'INTERNAL'
IF (bigendian) datarep = 'EXTERNAL32'

CALL MPI_FILE_OPEN(MPI_COMM_WORLD, TRIM(filename), MPI_MODE_RDONLY, MPI_INFO_NULL, fh, ierr)

CALL MPI_TYPE_CREATE_SUBARRAY (3_mik, dims, subarray_dims, subarray_origin - 1_mik, &
   MPI_ORDER_FORTRAN, MPI_DOUBLE_PRECISION, type_subarray,ierr)

CALL MPI_TYPE_COMMIT(type_subarray, ierr)

CALL MPI_FILE_SET_VIEW(fh, disp, MPI_DOUBLE_PRECISION, type_subarray, TRIM(datarep), MPI_INFO_NULL, ierr)

ALLOCATE(subarray(subarray_dims(1), subarray_dims(2), subarray_dims(3)))

CALL MPI_FILE_READ_ALL(fh, subarray, SIZE(subarray), MPI_INTEGER, MPI_STATUS_IGNORE, ierr)

CALL MPI_TYPE_FREE(type_subarray, ierr)

CALL MPI_FILE_CLOSE(fh, ierr)

END SUBROUTINE mpi_read_raw_rk8


!------------------------------------------------------------------------------
! SUBROUTINE: mpi_write_raw_ik2
!------------------------------------------------------------------------------  
!> @author Johannes Gebert - HLRS - NUM - gebert@hlrs.de
!
!> @brief
!> Write the raw data of a vtk file
!
!> @description
!
!> @param[in] fh File handle
!> @param[in] disp Length of the header (bytes) - position to write to
!> @param[in] filename File name
!> @param[in] dims Voxels per direction
!> @param[in] subarray_dims Voxels per direction of the subarray
!> @param[in] subarray_origin Physical origin of the subarray
!> @param[in] subarray Scalar field / Image data
!------------------------------------------------------------------------------  
 SUBROUTINE mpi_write_raw_ik2 (fh, filename, disp, dims, subarray_dims, subarray_origin, subarray)
! type = 'int2', 'int4'
! IF type = uint2 - send an int4 and let it convert into int2 (!) Have a look at the src for details

CHARACTER(LEN=*)             , INTENT(IN) :: filename
INTEGER(KIND=ik)             , INTENT(IN) :: fh
INTEGER(KIND=MPI_OFFSET_KIND), INTENT(IN) :: disp
INTEGER(KIND=ik),DIMENSION(3), INTENT(IN) :: dims, subarray_dims, subarray_origin
INTEGER(KIND=INT16), DIMENSION (:,:,:), ALLOCATABLE, INTENT(OUT) :: subarray

INTEGER  (KIND=ik) :: ierr, type_subarray
CHARACTER(LEN=scl) :: datarep = 'INTERNAL'

CALL MPI_FILE_OPEN(MPI_COMM_WORLD, TRIM(filename), MPI_MODE_WRONLY+MPI_MODE_CREATE, MPI_INFO_NULL, fh, ierr)

CALL MPI_TYPE_CREATE_SUBARRAY(3_mik, dims, subarray_dims, subarray_origin - 1_mik, &
   MPI_ORDER_FORTRAN, MPI_INTEGER2, type_subarray, ierr)

CALL MPI_TYPE_COMMIT(type_subarray, ierr)

CALL MPI_FILE_SET_VIEW( fh, disp, MPI_INTEGER2, type_subarray, TRIM(datarep), MPI_INFO_NULL, ierr)

CALL MPI_FILE_WRITE_ALL(fh, subarray, SIZE(subarray), MPI_INTEGER2, MPI_STATUS_IGNORE, ierr)

CALL MPI_TYPE_FREE(type_subarray, ierr)
CALL MPI_FILE_CLOSE(fh, ierr)

END SUBROUTINE mpi_write_raw_ik2


!------------------------------------------------------------------------------
! SUBROUTINE: mpi_write_raw_ik4
!------------------------------------------------------------------------------  
!> @author Johannes Gebert - HLRS - NUM - gebert@hlrs.de
!
!> @brief
!> Write the raw data of a vtk file
!
!> @description
!
!> @param[in] fh File handle
!> @param[in] disp Length of the header (bytes) - position to write to
!> @param[in] filename File name
!> @param[in] dims Voxels per direction
!> @param[in] subarray_dims Voxels per direction of the subarray
!> @param[in] subarray_origin Physical origin of the subarray
!> @param[in] subarray Scalar field / Image data
!------------------------------------------------------------------------------  
 SUBROUTINE mpi_write_raw_ik4 (fh, filename, disp, dims, subarray_dims, subarray_origin, subarray)
! type = 'int2', 'int4'
! IF type = uint2 - send an int4 and let it convert into int2 (!) Have a look at the src for details

CHARACTER(LEN=*)             , INTENT(IN) :: filename
INTEGER(KIND=ik)             , INTENT(IN) :: fh
INTEGER(KIND=MPI_OFFSET_KIND), INTENT(IN) :: disp
INTEGER(KIND=ik),DIMENSION(3), INTENT(IN) :: dims, subarray_dims, subarray_origin
INTEGER(KIND=INT32), DIMENSION (:,:,:), ALLOCATABLE, INTENT(OUT) :: subarray

INTEGER  (KIND=ik) :: ierr, type_subarray
CHARACTER(LEN=scl) :: datarep = 'INTERNAL'

CALL MPI_FILE_OPEN(MPI_COMM_WORLD, TRIM(filename), MPI_MODE_WRONLY+MPI_MODE_CREATE, MPI_INFO_NULL, fh, ierr)

CALL MPI_TYPE_CREATE_SUBARRAY(3_mik, dims, subarray_dims, subarray_origin - 1_mik, &
   MPI_ORDER_FORTRAN, MPI_INTEGER, type_subarray, ierr)

CALL MPI_TYPE_COMMIT(type_subarray, ierr)

CALL MPI_FILE_SET_VIEW( fh, disp, MPI_INTEGER, type_subarray, TRIM(datarep), MPI_INFO_NULL, ierr)

CALL MPI_FILE_WRITE_ALL(fh, subarray, SIZE(subarray), MPI_INTEGER, MPI_STATUS_IGNORE, ierr)

CALL MPI_TYPE_FREE(type_subarray, ierr)
CALL MPI_FILE_CLOSE(fh, ierr)

END SUBROUTINE mpi_write_raw_ik4
END MODULE file_routines_mpi
