!------------------------------------------------------------------------------
! MODULE: raw_binary
!------------------------------------------------------------------------------
!> @author Johannes Gebert - HLRS - NUM - gebert@hlrs.de
!
!> @brief:
!> Module for reading/writing raw binary files parallely.
!> PureDat provides an extended funcitonality and calls a binary blob a
!> stream file (for example .int4.st).
!------------------------------------------------------------------------------
MODULE mpi_binary

   USE ISO_FORTRAN_ENV
   USE MPI
   USE global_std
   USE user_interaction
   USE ser_binary
   
   IMPLICIT NONE

   INTERFACE mpi_read_raw
      MODULE PROCEDURE mpi_read_raw_rk4
      MODULE PROCEDURE mpi_read_raw_rk8
      MODULE PROCEDURE mpi_read_raw_ik4
      MODULE PROCEDURE mpi_read_raw_ik2
   END INTERFACE mpi_read_raw
   
   INTERFACE mpi_write_raw
      MODULE PROCEDURE mpi_write_raw_ik2
      MODULE PROCEDURE mpi_write_raw_ik4
   END INTERFACE mpi_write_raw

CONTAINS

!------------------------------------------------------------------------------
! SUBROUTINE: mpi_read_raw_ik2
!------------------------------------------------------------------------------  
!> @author Johannes Gebert - HLRS - NUM - gebert@hlrs.de
!
!> @brief
!> Read the raw int2 data of a binary blob. @Datarepresenation: 
!> NATIVE=LittleEndian on file system, EXTERNAL32 --> BigEndian. 
!> Please check and test if you need this feature!! Depends on Hardware.
!
!> @param[in] filename File name
!> @param[in] disp Length of the header (bytes)
!> @param[in] dims Amount of voxels per direction
!> @param[in] subarray_dims Amount of voxels per direction of the subarray
!> @param[in] subarray_origin Physical origin of the data set
!> @param[out] subarray data
!> @param[in] dtrep Datarepresentation 
!------------------------------------------------------------------------------  
SUBROUTINE mpi_read_raw_ik2(filename, disp, dims, subarray_dims, subarray_origin, subarray, dtrep)

CHARACTER(*), INTENT(IN) :: filename
INTEGER(MPI_OFFSET_KIND), INTENT(IN) :: disp
INTEGER(ik),DIMENSION(3), INTENT(IN) :: dims, subarray_dims, subarray_origin
INTEGER(INT16), DIMENSION (:,:,:), ALLOCATABLE, INTENT(INOUT) :: subarray

!------------------------------------------------------------------------------  
! file handle fh is provided by mpi itself and mustn't be given by the program/call/user
!------------------------------------------------------------------------------  
INTEGER(mik) :: ierr, type_subarray, fh
CHARACTER(scl) :: datarep
CHARACTER(*), INTENT(IN), OPTIONAL :: dtrep

datarep = 'NATIVE'

IF(PRESENT(dtrep)) THEN
   IF (dtrep /= "") datarep = TRIM(dtrep)
END IF 

CALL MPI_FILE_OPEN(MPI_COMM_WORLD, TRIM(filename), MPI_MODE_RDONLY, MPI_INFO_NULL, fh, ierr)

CALL MPI_TYPE_CREATE_SUBARRAY (3_mik, INT(dims, mik), INT(subarray_dims, mik), &
   INT(subarray_origin, mik), MPI_ORDER_FORTRAN, MPI_INTEGER2, type_subarray, ierr)

CALL MPI_TYPE_COMMIT(type_subarray, ierr)

CALL MPI_FILE_SET_VIEW(fh, disp, MPI_INTEGER2, type_subarray, TRIM(datarep), MPI_INFO_NULL, ierr)

IF(.NOT. ALLOCATED(subarray)) ALLOCATE(subarray(subarray_dims(1), subarray_dims(2), subarray_dims(3)))

CALL MPI_FILE_READ(fh, subarray, INT(SIZE(subarray), mik), MPI_INTEGER2, MPI_STATUS_IGNORE, ierr)

CALL MPI_TYPE_FREE(type_subarray, ierr)

CALL MPI_FILE_CLOSE(fh, ierr)

END SUBROUTINE mpi_read_raw_ik2

!------------------------------------------------------------------------------
! SUBROUTINE: mpi_read_raw_ik4
!------------------------------------------------------------------------------  
!> @author Johannes Gebert - HLRS - NUM - gebert@hlrs.de
!
!> @brief
!> Read the raw int4 data of a binary blob. @Datarepresenation: 
!> NATIVE=LittleEndian on file system, EXTERNAL32 --> BigEndian. 
!> Please check and test if you need this feature!! Depends on Hardware.
!
!> @param[in] filename File name
!> @param[in] disp Length of the header (bytes)
!> @param[in] dims Amount of voxels per direction
!> @param[in] subarray_dims Amount of voxels per direction of the subarray
!> @param[in] subarray_origin Physical origin of the data set
!> @param[out] subarray data
!> @param[in] dtrep Whether the input file is big or little endian. 
!------------------------------------------------------------------------------  
SUBROUTINE mpi_read_raw_ik4(filename, disp, dims, subarray_dims, subarray_origin, subarray, dtrep)

CHARACTER(*), INTENT(IN) :: filename
INTEGER(MPI_OFFSET_KIND), INTENT(IN) :: disp
INTEGER(ik),DIMENSION(3), INTENT(IN) :: dims, subarray_dims, subarray_origin
INTEGER(INT32), DIMENSION (:,:,:), ALLOCATABLE, INTENT(INOUT) :: subarray

! file handle fh is provided by mpi itself and mustn't be given by the program/call/user
INTEGER(mik) :: ierr, type_subarray, fh
CHARACTER(scl) :: datarep
CHARACTER(*), INTENT(IN), OPTIONAL :: dtrep

datarep = 'NATIVE'

IF(PRESENT(dtrep)) THEN
   IF (dtrep /= "") datarep = TRIM(dtrep)
END IF 

CALL MPI_FILE_OPEN(MPI_COMM_WORLD, TRIM(filename), MPI_MODE_RDONLY, MPI_INFO_NULL, fh, ierr)

CALL MPI_TYPE_CREATE_SUBARRAY (3_mik, INT(dims, mik), INT(subarray_dims, mik), & 
   INT(subarray_origin, mik), MPI_ORDER_FORTRAN, MPI_INTEGER4, type_subarray,ierr)

CALL MPI_TYPE_COMMIT(type_subarray, ierr)

CALL MPI_FILE_SET_VIEW(fh, disp, MPI_INTEGER4, type_subarray, TRIM(datarep), MPI_INFO_NULL, ierr)


CALL MPI_FILE_READ(fh, subarray, INT(SIZE(subarray), mik), MPI_INTEGER4, MPI_STATUS_IGNORE, ierr)

CALL MPI_TYPE_FREE(type_subarray, ierr)

CALL MPI_FILE_CLOSE(fh, ierr)

END SUBROUTINE mpi_read_raw_ik4


!------------------------------------------------------------------------------
! SUBROUTINE: mpi_read_raw_rk4
!------------------------------------------------------------------------------  
!> @author Johannes Gebert - HLRS - NUM - gebert@hlrs.de
!
!> @brief
!> Read the raw real 4 (single precision) data of a binary blob. @Datarepresenation: 
!> NATIVE=LittleEndian on file system, EXTERNAL32 --> BigEndian. 
!> Please check and test if you need this feature!! Depends on Hardware.
!
!> @param[in] filename File name
!> @param[in] disp Length of the header (bytes)
!> @param[in] dims Amount of voxels per direction
!> @param[in] subarray_dims Amount of voxels per direction of the subarray
!> @param[in] subarray_origin Physical origin of the data set
!> @param[out] subarray data
!> @param[in] dtrep Whether the input file is big or little endian. 
!------------------------------------------------------------------------------  
SUBROUTINE mpi_read_raw_rk4(filename, disp, dims, subarray_dims, subarray_origin, subarray, dtrep)

CHARACTER(*), INTENT(IN) :: filename
INTEGER(MPI_OFFSET_KIND), INTENT(IN) :: disp
INTEGER(ik),DIMENSION(3), INTENT(IN) :: dims, subarray_dims, subarray_origin
REAL(REAL32), DIMENSION (:,:,:), ALLOCATABLE, INTENT(OUT) :: subarray
CHARACTER(*), INTENT(IN), OPTIONAL :: dtrep

! file handle fh is provided by mpi itself and mustn't be given by the program/call/user
INTEGER(mik) :: ierr, type_subarray, fh
CHARACTER(scl) :: datarep

datarep = 'NATIVE'

IF(PRESENT(dtrep)) THEN
   IF (dtrep /= "") datarep = TRIM(dtrep)
END IF 

CALL MPI_FILE_OPEN(MPI_COMM_WORLD, TRIM(filename), MPI_MODE_RDONLY, MPI_INFO_NULL, fh, ierr)
  
CALL MPI_TYPE_CREATE_SUBARRAY (3_mik, INT(dims, mik), INT(subarray_dims, mik), & 
   INT(subarray_origin, mik), MPI_ORDER_FORTRAN, MPI_REAL, type_subarray,ierr)

CALL MPI_TYPE_COMMIT(type_subarray, ierr)

CALL MPI_FILE_SET_VIEW(fh, disp, MPI_REAL, type_subarray, TRIM(datarep), MPI_INFO_NULL, ierr)

ALLOCATE(subarray(subarray_dims(1), subarray_dims(2), subarray_dims(3)))

CALL MPI_FILE_READ_ALL(fh, subarray, INT(SIZE(subarray), mik), MPI_REAL, MPI_STATUS_IGNORE, ierr)

CALL MPI_TYPE_FREE(type_subarray, ierr)

CALL MPI_FILE_CLOSE(fh, ierr)

END SUBROUTINE mpi_read_raw_rk4

!------------------------------------------------------------------------------
! SUBROUTINE: mpi_read_raw_rk8
!------------------------------------------------------------------------------  
!> @author Johannes Gebert - HLRS - NUM - gebert@hlrs.de
!
!> @brief
!> Read the raw real 8 (double precision) data of a binary blob. @Datarepresenation: 
!> NATIVE=LittleEndian on file system, EXTERNAL32 --> BigEndian. 
!> Please check and test if you need this feature!! Depends on Hardware.
!
!> @param[in] fh File handle
!> @param[in] filename File name
!> @param[in] disp Length of the header (bytes)
!> @param[in] dims Amount of voxels per direction
!> @param[in] subarray_dims Amount of voxels per direction of the subarray
!> @param[in] subarray_origin Physical origin of the data set
!> @param[out] subarray data
!> @param[in] dtrep Whether the input file is big or little endian. 
!------------------------------------------------------------------------------  
SUBROUTINE mpi_read_raw_rk8(filename, disp, dims, subarray_dims, subarray_origin, subarray, dtrep)

CHARACTER(*), INTENT(IN) :: filename
INTEGER(MPI_OFFSET_KIND), INTENT(IN) :: disp
INTEGER(ik),DIMENSION(3), INTENT(IN) :: dims, subarray_dims, subarray_origin
REAL(REAL64), DIMENSION (:,:,:), ALLOCATABLE, INTENT(OUT) :: subarray

! file handle fh is provided by mpi itself and mustn't be given by the program/call/user
INTEGER(mik) :: ierr, type_subarray, fh
CHARACTER(scl) :: datarep
CHARACTER(*), INTENT(IN), OPTIONAL :: dtrep

datarep = 'NATIVE'

IF(PRESENT(dtrep)) THEN
   IF (dtrep /= "") datarep = TRIM(dtrep)
END IF 

! file handle fh is provided by mpi itself and mustn't be given by the program/call/user
CALL MPI_FILE_OPEN(MPI_COMM_WORLD, TRIM(filename), MPI_MODE_RDONLY, MPI_INFO_NULL, fh, ierr)

CALL MPI_TYPE_CREATE_SUBARRAY (3_mik, INT(dims, mik), INT(subarray_dims, mik), & 
   INT(subarray_origin, mik), MPI_ORDER_FORTRAN, MPI_DOUBLE_PRECISION, type_subarray,ierr)

CALL MPI_TYPE_COMMIT(type_subarray, ierr)

CALL MPI_FILE_SET_VIEW(fh, disp, MPI_DOUBLE_PRECISION, type_subarray, TRIM(datarep), MPI_INFO_NULL, ierr)

ALLOCATE(subarray(subarray_dims(1), subarray_dims(2), subarray_dims(3)))

CALL MPI_FILE_READ_ALL(fh, subarray, INT(SIZE(subarray), mik), MPI_DOUBLE_PRECISION, MPI_STATUS_IGNORE, ierr)

CALL MPI_TYPE_FREE(type_subarray, ierr)

CALL MPI_FILE_CLOSE(fh, ierr)

END SUBROUTINE mpi_read_raw_rk8


!------------------------------------------------------------------------------
! SUBROUTINE: mpi_write_raw_ik2
!------------------------------------------------------------------------------  
!> @author Johannes Gebert - HLRS - NUM - gebert@hlrs.de
!
!> @brief
!> Write raw binary data. @Datarepresenation: 
!> NATIVE=LittleEndian on file system, EXTERNAL32 --> BigEndian. 
!> Please check and test if you need this feature!! Depends on Hardware.
!
!> @param[in] fh File handle
!> @param[in] disp Length of the header (bytes) - position to write to
!> @param[in] filename File name
!> @param[in] dims Voxels per direction
!> @param[in] subarray_dims Voxels per direction of the subarray
!> @param[in] subarray_origin Physical origin of the subarray
!> @param[in] subarray Scalar field / Image data
!> @param[in] dtrep Whether the input file is big or little endian. 
!------------------------------------------------------------------------------  
 SUBROUTINE mpi_write_raw_ik2 (filename, disp, dims, subarray_dims, subarray_origin, subarray, dtrep)
! type = 'int2', 'int4'
! IF type = uint2 - send an int4 and let it convert into int2 (!) Have a look at the src for details

CHARACTER(*), INTENT(IN) :: filename
INTEGER(MPI_OFFSET_KIND), INTENT(IN) :: disp
INTEGER(ik),DIMENSION(3), INTENT(IN) :: dims, subarray_dims, subarray_origin
INTEGER(INT16), DIMENSION (:,:,:), INTENT(IN) :: subarray
CHARACTER(*), INTENT(IN), OPTIONAL :: dtrep

! file handle fh is provided by mpi itself and mustn't be given by the program/call/user
INTEGER(mik)  :: fh, ierr, type_subarray
CHARACTER(scl) :: datarep = 'NATIVE'

datarep = 'NATIVE'

IF(PRESENT(dtrep)) THEN
   IF (dtrep /= "") datarep = TRIM(dtrep)
END IF 

CALL MPI_FILE_OPEN(MPI_COMM_WORLD, TRIM(filename), MPI_MODE_WRONLY+MPI_MODE_CREATE, MPI_INFO_NULL, fh, ierr)

CALL MPI_TYPE_CREATE_SUBARRAY (3_mik, INT(dims, mik), INT(subarray_dims, mik), & 
   INT(subarray_origin, mik), MPI_ORDER_FORTRAN, MPI_INTEGER2, type_subarray,ierr)

CALL MPI_TYPE_COMMIT(type_subarray, ierr)

CALL MPI_FILE_SET_VIEW(fh, disp, MPI_INTEGER2, type_subarray, TRIM(datarep), MPI_INFO_NULL, ierr)

CALL MPI_FILE_WRITE(fh, subarray, INT(SIZE(subarray), mik), MPI_INTEGER2, MPI_STATUS_IGNORE, ierr)

CALL MPI_TYPE_FREE(type_subarray, ierr)

CALL MPI_FILE_CLOSE(fh, ierr)

END SUBROUTINE mpi_write_raw_ik2


!------------------------------------------------------------------------------
! SUBROUTINE: mpi_write_raw_ik4
!------------------------------------------------------------------------------  
!> @author Johannes Gebert - HLRS - NUM - gebert@hlrs.de
!
!> @brief
!> Write raw binary data. @Datarepresenation: 
!> NATIVE=LittleEndian on file system, EXTERNAL32 --> BigEndian. 
!> Please check and test if you need this feature!! Depends on Hardware.
!
!> @description
!
!> @param[in] disp Length of the header (bytes) - position to write to
!> @param[in] filename File name
!> @param[in] dims Voxels per direction
!> @param[in] subarray_dims Voxels per direction of the subarray
!> @param[in] subarray_origin Physical origin of the subarray
!> @param[in] subarray Scalar field / Image data
!> @param[in] dtrep Whether the input file is big or little endian. 
!------------------------------------------------------------------------------  
 SUBROUTINE mpi_write_raw_ik4 (filename, disp, dims, subarray_dims, subarray_origin, subarray, dtrep)
! type = 'int2', 'int4'
! IF type = uint2 - send an int4 and let it convert into int2 (!) Have a look at the src for details

CHARACTER(*), INTENT(IN) :: filename
INTEGER(MPI_OFFSET_KIND), INTENT(IN) :: disp
INTEGER(ik),DIMENSION(3), INTENT(IN) :: dims, subarray_dims, subarray_origin
INTEGER(INT32), DIMENSION (:,:,:), INTENT(IN) :: subarray
CHARACTER(*), INTENT(IN), OPTIONAL :: dtrep

! file handle fh is provided by mpi itself and mustn't be given by the program/call/user
INTEGER(mik)  :: fh, ierr, type_subarray
CHARACTER(scl) :: datarep = 'NATIVE'

datarep = 'NATIVE'

IF(PRESENT(dtrep)) THEN
   IF (dtrep /= "") datarep = TRIM(dtrep)
END IF 

CALL MPI_FILE_OPEN(MPI_COMM_WORLD, TRIM(filename), &
   MPI_MODE_WRONLY+MPI_MODE_CREATE, MPI_INFO_NULL, fh, ierr)

CALL MPI_TYPE_CREATE_SUBARRAY (3_mik, INT(dims, mik), INT(subarray_dims, mik), & 
   INT(subarray_origin, mik), MPI_ORDER_FORTRAN, MPI_INTEGER4, type_subarray,ierr)

CALL MPI_TYPE_COMMIT(type_subarray, ierr)

CALL MPI_FILE_SET_VIEW(fh, disp, MPI_INTEGER4, type_subarray, &
   TRIM(datarep), MPI_INFO_NULL, ierr)

CALL MPI_FILE_WRITE(fh, subarray, INT(SIZE(subarray), mik), &
   MPI_INTEGER4, MPI_STATUS_IGNORE, ierr)

CALL MPI_TYPE_FREE(type_subarray, ierr)
CALL MPI_FILE_CLOSE(fh, ierr)

END SUBROUTINE mpi_write_raw_ik4

END MODULE mpi_binary
