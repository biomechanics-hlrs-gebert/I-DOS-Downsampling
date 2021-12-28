!------------------------------------------------------------------------------
! MODULE: file_routines_mpi
!------------------------------------------------------------------------------
!> @author Johannes Gebert - HLRS - NUM - gebert@hlrs.de
!
! DESCRIPTION: 
!> Module containing all recurring definitions of kinds and vmbers.
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
   Module Procedure mpi_write_raw_rk4
   Module Procedure mpi_write_raw_rk8
   Module Procedure mpi_write_raw_ik4
   Module Procedure mpi_write_raw_ik2
   Module Procedure mpi_write_raw_uik2
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
!> @param[in] fh File handle
!> @param[in] filename File name
!> @param[in] hdr_lngth Length of header (bytes)
!> @param[in] dims Voxels per direction
!> @param[in] origin Physical (mm) origin
!> @param[in] spcng Physical distance between two voxels (mm)
!> @param[in] type Data type contained in binary blob
!------------------------------------------------------------------------------
SUBROUTINE read_vtk_meta(fh, filename, hdr_lngth, dims, origin, spcng, type)

INTEGER  (KIND=ik), INTENT(IN)  :: fh
CHARACTER(len=*)  , INTENT(IN)  :: filename
INTEGER  (KIND=ik), INTENT(OUT) :: hdr_lngth
INTEGER  (KIND=ik), DIMENSION(3) , INTENT(OUT) :: dims
REAL     (KIND=rk), DIMENSION(3) , INTENT(OUT) :: origin
REAL     (KIND=rk), DIMENSION(3) , INTENT(OUT) :: spcng
CHARACTER(len=*), INTENT(OUT) :: type

!-- Initialize variables in case they're not used
INTEGER  (KIND=ik) :: ii=0, ntokens

CHARACTER(len=mcl) :: line
CHARACTER(len=mcl) :: tokens(100)
CHARACTER(len=mcl), DIMENSION(3) :: token

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
! SUBROUTINE: mpi_read_raw_ik4
!------------------------------------------------------------------------------  
!> @author Johannes Gebert - HLRS - NUM - gebert@hlrs.de
!
!> @brief
!> Read the raw int4 data of a binary blob
!
!> @param[in] filename File name
!> @param[in] hdr_lngth Length of the header (bytes)
!> @param[in] dims Amount of voxels per direction
!> @param[in] subarray_dims Amount of voxels per direction of the subarray
!> @param[in] subarray_origin Physical origin of the data set
!> @param[in] subarray int4 data
!------------------------------------------------------------------------------  
SUBROUTINE mpi_read_raw_ik4(filename, hdr_lngth, dims, subarray_dims, subarray_origin, subarray)

CHARACTER(LEN=*), INTENT(IN)  :: filename
INTEGER(KIND=MPI_OFFSET_KIND) :: hdr_lngth
INTEGER(KIND=ik),    DIMENSION(3), INTENT(IN) :: dims
INTEGER(KIND=ik),    DIMENSION(3), INTENT(IN) :: subarray_dims
INTEGER(KIND=ik),    DIMENSION(3), INTENT(IN) :: subarray_origin
INTEGER(KIND=INT32), DIMENSION (:,:,:), ALLOCATABLE, INTENT(OUT) :: subarray

! Internal Variables
INTEGER(KIND=ik) :: fh

! MPI
INTEGER  (KIND=ik) :: ierr, type_subarray


CALL MPI_FILE_OPEN(MPI_COMM_WORLD, TRIM(filename), MPI_MODE_RDONLY, MPI_INFO_NULL, fh, ierr)

CALL MPI_TYPE_CREATE_SUBARRAY (3_mik, dims, subarray_dims, subarray_origin - 1_mik, &
   MPI_ORDER_FORTRAN, MPI_INTEGER, type_subarray,ierr)

CALL MPI_TYPE_COMMIT(type_subarray, ierr)

CALL MPI_FILE_SET_VIEW(fh, hdr_lngth, MPI_INTEGER, type_subarray, &
   'EXTERNAL32', MPI_INFO_NULL, ierr)

ALLOCATE(subarray(subarray_dims(1), subarray_dims(2), subarray_dims(3)))

CALL MPI_FILE_READ_ALL(fh, subarray, SIZE(subarray), MPI_INTEGER, MPI_STATUS_IGNORE, ierr)

CALL MPI_TYPE_FREE(type_subarray, ierr)

CALL MPI_FILE_CLOSE(fh, ierr)

END SUBROUTINE mpi_read_raw_ik4


!------------------------------------------------------------------------------
! SUBROUTINE: mpi_read_raw_ik2
!------------------------------------------------------------------------------  
!> @author Johannes Gebert - HLRS - NUM - gebert@hlrs.de
!
!> @brief
!> Read the raw int2 data of a binary blob
!
!> @param[in] filename File name
!> @param[in] hdr_lngth Length of the header (bytes)
!> @param[in] dims Amount of voxels per direction
!> @param[in] subarray_dims Amount of voxels per direction of the subarray
!> @param[in] subarray_origin Physical origin of the data set
!> @param[in] subarray int2 data
!------------------------------------------------------------------------------  
SUBROUTINE mpi_read_raw_ik2(filename, hdr_lngth, dims, subarray_dims, subarray_origin, subarray)

CHARACTER(LEN=*), INTENT(IN)  :: filename
INTEGER(KIND=MPI_OFFSET_KIND) :: hdr_lngth
INTEGER(KIND=ik), DIMENSION(3), INTENT(IN) :: dims
INTEGER(KIND=ik), DIMENSION(3), INTENT(IN) :: subarray_dims
INTEGER(KIND=ik), DIMENSION(3), INTENT(IN) :: subarray_origin
INTEGER(KIND=INT16), DIMENSION (:,:,:), ALLOCATABLE, INTENT(OUT) :: subarray 

! Internal Variables
INTEGER(KIND=ik) :: fh

! MPI
INTEGER  (KIND=ik) :: ierr, type_subarray


CALL MPI_FILE_OPEN(MPI_COMM_WORLD, TRIM(filename), MPI_MODE_RDONLY, MPI_INFO_NULL, fh, ierr)

CALL MPI_TYPE_CREATE_SUBARRAY (3_mik, dims, subarray_dims, subarray_origin - 1_mik, &
   MPI_ORDER_FORTRAN, MPI_INTEGER, type_subarray,ierr)

CALL MPI_TYPE_COMMIT(type_subarray, ierr)

CALL MPI_FILE_SET_VIEW(fh, hdr_lngth, MPI_INTEGER, type_subarray, &
   'EXTERNAL32', MPI_INFO_NULL, ierr)

! >>>>>>>>>>>>>>>>>>>>>>>> ONLY THAT FOR INTERFACE <<<<<<<<<<<<<<<<<<<<
ALLOCATE(subarray(subarray_dims(1), subarray_dims(2), subarray_dims(3)))

CALL MPI_FILE_READ_ALL(fh, subarray, SIZE(subarray), MPI_INTEGER, MPI_STATUS_IGNORE, ierr)
! >>>>>>>>>>>>>>>>>>>>>>>> ONLY THAT FOR INTERFACE <<<<<<<<<<<<<<<<<<<<

CALL MPI_TYPE_FREE(type_subarray, ierr)

CALL MPI_FILE_CLOSE(fh, ierr)

END SUBROUTINE mpi_read_raw_ik2


!------------------------------------------------------------------------------
! SUBROUTINE: read_raw_mpi
!------------------------------------------------------------------------------  
!> @author Johannes Gebert - HLRS - NUM - gebert@hlrs.de
!
!> @brief
!> Read the raw data of a binary blob
!
!> @description
!> !!! Routine needs extensive rework due to clean code issues
!> !!! Provide an interface like @meta file format
!> !!! Reduce horizontal alignment
!
!> @param[in] filename File name
!> @param[in] type_in type requested
!> @param[in] type_out type returned
!> @param[in] hdr_lngth Length of the header (bytes)
!> @param[in] dims Amount of voxels per direction
!> @param[in] subarray_dims Amount of voxels per direction of the subarray
!> @param[in] subarray_origin Physical origin of the data set
!> @param[in] subarray int4 data
!> @param[in] displacement of the header (bytes)
!> @param[in] log_un Log unit
!> @param[in] status_o returned status
!------------------------------------------------------------------------------  
SUBROUTINE read_raw_mpi(filename, type_in, type_out, hdr_lngth, dims, subarray_dims, &
   subarray_origin, subarray, displacement, log_un, status_o)
! MPI Parallel read always reads subarrays.
! log_un exists means "print log"!
! type = 'real4', 'real8, 'int2', 'int4'

CHARACTER(LEN=*), INTENT(IN)  :: filename
CHARACTER(LEN=*), INTENT(IN)  :: type_in
CHARACTER(LEN=*), INTENT(OUT) :: type_out
INTEGER(KIND=MPI_OFFSET_KIND) :: hdr_lngth
INTEGER(KIND=ik),    DIMENSION(3), INTENT(IN) :: dims
INTEGER(KIND=ik),    DIMENSION(3), INTENT(IN) :: subarray_dims
INTEGER(KIND=ik),    DIMENSION(3), INTENT(IN) :: subarray_origin
INTEGER(KIND=INT32), DIMENSION (:,:,:), ALLOCATABLE, INTENT(OUT) :: subarray
INTEGER(KIND=ik), OPTIONAL, INTENT(IN)     :: displacement
INTEGER(KIND=ik), OPTIONAL, INTENT(IN)     :: log_un
INTEGER(KIND=ik), OPTIONAL, INTENT(OUT)    :: status_o

! Internal Variables
INTEGER(KIND=ik) :: status=0, rd_o
INTEGER(KIND=INT16) , DIMENSION (:,:,:), ALLOCATABLE :: array_i_two
INTEGER(KIND=INT32) , DIMENSION (:,:,:), ALLOCATABLE :: array_i_four
REAL   (KIND=REAL64), DIMENSION (:,:,:), ALLOCATABLE :: array_r_eight
REAL   (KIND=REAL32), DIMENSION (:,:,:), ALLOCATABLE :: array_r_four
INTEGER(KIND=ik) :: fh, ii, jj, kk

! MPI
INTEGER  (KIND=ik) :: ierr
INTEGER  (KIND=ik) :: type_subarray

IF (PRESENT(displacement)) hdr_lngth = displacement
IF (PRESENT(log_un))            rd_o = log_un

CALL MPI_FILE_OPEN(MPI_COMM_WORLD, TRIM(filename), MPI_MODE_RDONLY, MPI_INFO_NULL, fh, ierr)

IF (TRIM(type_in) .EQ. 'real4') THEN

   type_out="real4"

   CALL MPI_TYPE_CREATE_SUBARRAY (3_mik, &
   dims                                , &
   subarray_dims                       , &
   subarray_origin - 1_mik             , &
   MPI_ORDER_FORTRAN                   , &
   MPI_REAL                            , &
   type_subarray                       , &
   ierr)

   CALL MPI_TYPE_COMMIT(type_subarray, ierr)

   CALL MPI_FILE_SET_VIEW( fh , &
   hdr_lngth                  , &
   MPI_REAL                   , &
   type_subarray              , &
   'EXTERNAL32'               , &
   MPI_INFO_NULL              , &
   ierr)

   ALLOCATE( array_r_four( subarray_dims(1), subarray_dims(2), subarray_dims(3) ))
   CALL MPI_FILE_READ_ALL(fh, array_r_four, SIZE(array_r_four), MPI_REAL, MPI_STATUS_IGNORE, ierr)

   subarray = INT(FLOOR(array_r_four), KIND=INT32)
   DEALLOCATE(array_r_four)

ELSE IF (TRIM(type_in) .EQ. 'real8') THEN

   type_out="real8"

   CALL MPI_TYPE_CREATE_SUBARRAY (3_mik, &
   dims                                , &
   subarray_dims                       , &
   subarray_origin - 1_mik             , &
   MPI_ORDER_FORTRAN                   , &
   MPI_DOUBLE_PRECISION                , &
   type_subarray                       , &
   ierr)

   CALL MPI_TYPE_COMMIT(type_subarray, ierr)

   CALL MPI_FILE_SET_VIEW( fh , &
   hdr_lngth                  , &
   MPI_DOUBLE_PRECISION       , &
   type_subarray              , &
   'EXTERNAL32'               , &
   MPI_INFO_NULL              , &
   ierr)

   ALLOCATE( array_r_eight( subarray_dims(1), subarray_dims(2), subarray_dims(3) ))
  
   CALL MPI_FILE_READ_ALL(fh, array_r_eight, SIZE(array_r_eight), MPI_DOUBLE_PRECISION, MPI_STATUS_IGNORE, ierr)

   subarray = INT(FLOOR(array_r_four), KIND=INT32)
   DEALLOCATE(array_r_four)

   WRITE(rd_o,'(A)') 'WARNING: Converted real 8 to integer 4 during file read. Check validity.'
   FLUSH(rd_o)

ELSE IF ((TRIM(type_in) .EQ. 'int2') .OR. (TRIM(type_in) .EQ. 'uint2')) THEN

   CALL MPI_TYPE_CREATE_SUBARRAY (3_mik, &
   dims                                , &
   subarray_dims                       , &
   subarray_origin                     , & ! - 1_mik 
   MPI_ORDER_FORTRAN                   , &
   MPI_INTEGER2                        , &
   type_subarray                       , &
   ierr)

   CALL MPI_TYPE_COMMIT(type_subarray, ierr)

   CALL MPI_FILE_SET_VIEW( fh , &
   hdr_lngth                  , &
   MPI_INTEGER2               , &
   type_subarray              , &
   'EXTERNAL32'               , &
   MPI_INFO_NULL              , &
   ierr)

   ALLOCATE( array_i_two( subarray_dims(1), subarray_dims(2), subarray_dims(3)) )

   CALL MPI_FILE_READ_ALL(fh, array_i_two, SIZE(array_i_two), MPI_INTEGER2, MPI_STATUS_IGNORE, ierr)

   subarray = INT(array_i_two, KIND=INT32)

   ! Not so pretty workaround
   IF (TRIM(type_in) .EQ. 'uint2') THEN
      DO ii=1, subarray_dims(1)
         DO jj=1, subarray_dims(2)
            DO kk=1, subarray_dims(3)
               IF (subarray(ii,jj,kk) .LT. 0_ik) subarray(ii,jj,kk) = INT(array_i_two(ii,jj,kk), KIND=INT32) + 65536_ik
            END DO
         END DO
      END DO
      
      subarray = subarray - 32768_ik
      type_out="int2"
   END IF

   IF (TRIM(type_in) .EQ. 'int2') type_out="int2"


   DEALLOCATE(array_i_two)

ELSE IF (TRIM(type_in) .EQ. 'int4') THEN

   type_out="int4"

   CALL MPI_TYPE_CREATE_SUBARRAY (3_mik, &
   dims                                , &
   subarray_dims                       , &
   subarray_origin - 1_mik             , &
   MPI_ORDER_FORTRAN                   , &
   MPI_INTEGER                         , &
   type_subarray                       , &
   ierr)

   CALL MPI_TYPE_COMMIT(type_subarray, ierr)

   CALL MPI_FILE_SET_VIEW( fh, &
   hdr_lngth                  , &
   MPI_INTEGER                , &
   type_subarray              , &
   'EXTERNAL32'               , &
   MPI_INFO_NULL              , &
   ierr)

   ALLOCATE( array_i_four( subarray_dims(1), subarray_dims(2), subarray_dims(3)) )
   CALL MPI_FILE_READ_ALL(fh, array_i_four, SIZE(array_i_four), MPI_INTEGER, MPI_STATUS_IGNORE, ierr)

   subarray = INT(array_i_four, KIND=INT16)

   IF (MINVAL(array_i_four) .LT. -32768_ik .OR. MAXVAL(array_i_four) .GT. 32767_ik) THEN
      WRITE(log_un,'(A)') 'WARNING: INVALID CONVERSION FROM INT4 TO INT2. CHECK DATA.'
      status = 1_ik
   END IF

   DEALLOCATE(array_i_four)
ELSE 
   status = 1_ik
END IF

CALL MPI_TYPE_FREE(type_subarray, ierr)
CALL MPI_FILE_CLOSE(fh, ierr)

IF (PRESENT(status_o)) status_o = status
END SUBROUTINE read_raw_mpi


!------------------------------------------------------------------------------
! SUBROUTINE: write_raw_mpi
!------------------------------------------------------------------------------  
!> @author Johannes Gebert - HLRS - NUM - gebert@hlrs.de
!
!> @brief
!> Write the raw data of a vtk file
!
!> @description
!> !!! Routine needs extensive rework due to clean code issues
!> !!! Provide an interface like @meta file format
!> !!! Reduce horizontal alignment
!
!> @param[in] type Data type
!> @param[in] hdr_lngth Length of the header (bytes) - position to write to
!> @param[in] filename File name
!> @param[in] dims Voxels per direction
!> @param[in] subarray_dims Voxels per direction of the subarray
!> @param[in] subarray_origin Physical origin of the subarray
!> @param[in] subarray2 int2 image data
!> @param[in] subarray4 int4 image data
!------------------------------------------------------------------------------  
 SUBROUTINE write_raw_mpi (type, hdr_lngth, filename, dims, subarray_dims, subarray_origin, subarray2, subarray4)
! type = 'int2', 'int4'
! IF type = uint2 - send an int4 and let it convert into int2 (!) Have a look at the src for details
CHARACTER(LEN=*)                :: type
INTEGER  (KIND=MPI_OFFSET_KIND) :: hdr_lngth
CHARACTER(LEN=*), INTENT(IN)    :: filename
INTEGER(KIND=ik), DIMENSION(3), INTENT(IN) :: dims
INTEGER(KIND=ik), DIMENSION(3), INTENT(IN) :: subarray_dims
INTEGER(KIND=ik), DIMENSION(3), INTENT(IN) :: subarray_origin
INTEGER(KIND=INT16), DIMENSION (:,:,:), OPTIONAL    :: subarray2
INTEGER(KIND=INT16), DIMENSION (:,:,:), ALLOCATABLE :: subarray2_a
INTEGER(KIND=INT32), DIMENSION (:,:,:), OPTIONAL    :: subarray4

! Internal Variables
INTEGER  (KIND=ik) :: fh

! MPI
INTEGER  (KIND=ik) :: my_rank, size_mpi, ierr
INTEGER  (KIND=ik) :: type_subarray

CALL MPI_COMM_RANK(MPI_COMM_WORLD, my_rank, ierr)
CALL MPI_ERR(ierr,"MPI_COMM_RANK couldn't be retrieved")

CALL MPI_COMM_SIZE(MPI_COMM_WORLD, size_mpi, ierr)
CALL MPI_ERR(ierr,"MPI_COMM_SIZE couldn't be retrieved")

CALL MPI_FILE_OPEN(MPI_COMM_WORLD, TRIM(filename), MPI_MODE_WRONLY+MPI_MODE_CREATE, MPI_INFO_NULL, fh, ierr)

IF (TRIM(type) .EQ. 'int2') THEN

   CALL MPI_TYPE_CREATE_SUBARRAY (3_mik, &
   dims                                , &
   subarray_dims                       , &
   subarray_origin                     , & ! - 1_mik
   MPI_ORDER_FORTRAN                   , &
   MPI_INTEGER2                        , &
   type_subarray                       , &
   ierr)

   CALL MPI_TYPE_COMMIT(type_subarray, ierr)

   CALL MPI_FILE_SET_VIEW( fh , &
   hdr_lngth                  , &
   MPI_INTEGER2               , &
   type_subarray              , &
   'EXTERNAL32'               , &
   MPI_INFO_NULL              , &
   ierr)

   IF (TRIM(type) .EQ. 'int2') THEN
      CALL MPI_FILE_WRITE_ALL(fh, subarray2, SIZE(subarray2), MPI_INTEGER2, MPI_STATUS_IGNORE, ierr)
   ELSE ! could only be uint2
      CALL MPI_FILE_WRITE_ALL(fh, subarray2_a, SIZE(subarray2_a), MPI_INTEGER2, MPI_STATUS_IGNORE, ierr)
   END IF
ELSE IF (TRIM(type) .EQ. 'int4') THEN
   ! CHANGE TYPE DEFINITION FIRST!

   CALL MPI_TYPE_CREATE_SUBARRAY (3_mik, &
   dims                                , &
   subarray_dims                       , &
   subarray_origin - 1_mik             , &
   MPI_ORDER_FORTRAN                   , &
   MPI_INTEGER                         , &
   type_subarray                       , &
   ierr)

   CALL MPI_TYPE_COMMIT(type_subarray, ierr)

   CALL MPI_FILE_SET_VIEW( fh , &
   hdr_lngth                  , &
   MPI_INTEGER                , &
   type_subarray              , &
   'EXTERNAL32'               , &
   MPI_INFO_NULL              , &
   ierr)

   CALL MPI_FILE_WRITE_ALL(fh, subarray4, SIZE(subarray4), MPI_INTEGER, MPI_STATUS_IGNORE, ierr)
END IF

CALL MPI_TYPE_FREE(type_subarray, ierr)
CALL MPI_FILE_CLOSE(fh, ierr)

END SUBROUTINE write_raw_mpi



END MODULE file_routines_mpi
