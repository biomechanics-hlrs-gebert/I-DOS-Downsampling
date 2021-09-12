!---------------------------------------------------------------------------------------------------
! mod_file_routines_mpi.f90
!
! author Johannes Gebert
! date 04.01.2021
! date 05.08.2021

! subroutine mpi_err(ierr, mssg)
! SUBROUTINE check_file_exist(filename, must_exist, mpi)
! SUBROUTINE write_vtk_meta (fh, filename, type, atStart, spcng, origin, dims, sections)
! SUBROUTINE read_vtk_meta(fh, filename, dims, origin, spcng, typ, displacement, sze_o, fov_o, bnds_o, rd_o, status_o)
! SUBROUTINE write_raw_mpi (type, hdr_lngth, filename, dims, subarray_dims, subarray_origin, subarray)
! SUBROUTINE write_matrix(matrix, title, u, frmwrk)

MODULE file_routines_mpi

USE ISO_FORTRAN_ENV
USE standards
USE strings

IMPLICIT NONE

CONTAINS

 !---------------------------------------------------------------------------------------------------
 SUBROUTINE write_meta (fh, filename, type, atStart, spcng, origin, dims)

   ! It's HIGHLY recommended to check the existence of the output file prior to CALLing this
   ! Subroutine! Otherwise the program will crash. It's not double-checkd here, because this
   ! sequence often is placed at the very end of a program, which may run some time.

   INTEGER  (KIND=ik), INTENT(IN)                             :: fh
   CHARACTER(len=*)                                           :: filename
   CHARACTER(LEN=*)  , INTENT(IN)              , OPTIONAL     :: type
   LOGICAL           , INTENT(IN)              , OPTIONAL     :: atStart  ! optional cause start/end of vtk file possible
   REAL     (KIND=rk), INTENT(IN), DIMENSION(3), OPTIONAL     :: spcng    ! same
   REAL     (KIND=rk), INTENT(IN), DIMENSION(3), OPTIONAL     :: origin   ! same
   INTEGER  (KIND=ik), INTENT(IN), DIMENSION(3), OPTIONAL     :: dims     ! same

   REAL     (KIND=rk)            , DIMENSION(3)               :: orgn
   INTEGER  (KIND=INT64)                                      :: sze

   IF (atStart .EQV. .TRUE.) THEN

      IF (PRESENT(sections)) THEN
         orgn = (sections-1) * dims * spcng + origin
      ELSE
         orgn = origin
      END IF

      OPEN(UNIT=fh, FILE=TRIM(filename), ACTION='WRITE', STATUS='NEW')

      WRITE(fh,'(A)')           "# vtk DataFile Version 4.2" ! Compatibility issue
      WRITE(fh,'(A)')           "vtk output"
      WRITE(fh,'(A)')           "BINARY"
      WRITE(fh,'(A)')           "DATASET STRUCTURED_POINTS"
      WRITE(fh,'(A,3(I5))')     "DIMENSIONS", dims
      WRITE(fh,'(A,3(F11.6))')  "SPACING ", spcng
      WRITE(fh,'(A,3(F11.6))')  "ORIGIN ", orgn
      sze = PRODUCT(INT(dims, KIND=INT64))
      WRITE(fh,'(A, I15)')      "POINT_DATA", sze
      
      IF (TRIM(type) .EQ. 'uint2') WRITE(fh,'(A)') "SCALARS DICOMImage unsigned_short"    
      IF (TRIM(type) .EQ. 'int2')  WRITE(fh,'(A)') "SCALARS DICOMImage short"    
      IF (TRIM(type) .EQ. 'int4')  WRITE(fh,'(A)') "SCALARS DICOMImage int"

      WRITE(fh,'(A)')            "LOOKUP_TABLE default"
      WRITE(fh ,'(A)')          ''
   ELSE
      OPEN(UNIT=fh, FILE=TRIM(filename), ACTION='WRITE', STATUS='OLD', POSITION='APPEND')
      WRITE(fh ,'(A)')          ''
      WRITE(fh ,'(A)')          "METADATA"
      WRITE(fh ,'(A)')          "INFORMATION 0"
      WRITE(fh ,'(A)')
   END IF

   CLOSE(UNIT=fh)
 END SUBROUTINE write_vtk_meta

 !---------------------------------------------------------------------------------------------------
 
 SUBROUTINE write_raw (fh, type, filename, dims, array2, array4)
! type = 'int2', 'int4'
! IF type = uint2 - send an int4 and let it convert into int2 (!) Have a look at the src for details
CHARACTER(LEN=*)                        , INTENT(IN)                         :: type
CHARACTER(LEN=*)                        , INTENT(IN)                         :: filename
INTEGER  (KIND=ik)   , DIMENSION(3)     , INTENT(IN)                         :: dims
INTEGER  (KIND=INT16), DIMENSION (:,:,:)            , OPTIONAL               :: array2
INTEGER  (KIND=INT32), DIMENSION (:,:,:)            , OPTIONAL               :: array4

! Internal Variables
INTEGER  (KIND=ik)                                                           :: fh

IF (TRIM(type) .EQ. 'int2') THEN

ELSE IF (TRIM(type) .EQ. 'int4') THEN

END IF

END SUBROUTINE write_raw

!---------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------

SUBROUTINE read_vtk_meta(fh, filename, dims, origin, spcng, typ, displacement, &
   sze_o, fov_o, bnds_o, rd_o, status_o)
! log_un exists means "print log"!

INTEGER  (KIND=ik)                                                            :: fh
CHARACTER(len=*)                                                , INTENT(IN)  :: filename
INTEGER  (KIND=ik)    , DIMENSION(3)                            , INTENT(OUT) :: dims
REAL     (KIND=rk)    , DIMENSION(3)                            , INTENT(OUT) :: origin
REAL     (KIND=rk)    , DIMENSION(3)                            , INTENT(OUT) :: spcng
CHARACTER(len=*)                                                , INTENT(OUT) :: typ
INTEGER  (KIND=ik)                                    , OPTIONAL, INTENT(OUT) :: displacement
INTEGER  (KIND=ik)                                    , OPTIONAL, INTENT(OUT) :: sze_o ! int32 mpi!
REAL     (KIND=rk)    , DIMENSION(3)                  , OPTIONAL, INTENT(OUT) :: fov_o
INTEGER  (KIND=ik)    , DIMENSION(3,2)                , OPTIONAL, INTENT(OUT) :: bnds_o

!-- Initialize variables in case they're not used
INTEGER  (KIND=INT64)                                                         :: sze
REAL     (KIND=rk)    , DIMENSION(3)                                          :: fov
INTEGER  (KIND=ik)    , DIMENSION(3,2)                                        :: bnds
INTEGER  (KIND=ik)                                                            :: status=0, ii=0, hdr_lngth, lui=6, ntokens

CHARACTER(len=mcl)                                                            :: line
CHARACTER(len=mcl)                                                            :: tokens(100)
CHARACTER(len=mcl)    , DIMENSION(3)                                          :: token

!-- Check existence of optional variables
IF (PRESENT(sze_o)   ) sze    = sze_o
IF (PRESENT(fov_o)   ) fov    = fov_o
IF (PRESENT(bnds_o)  ) bnds   = bnds_o
IF (PRESENT(status_o)) status = status_o
IF (PRESENT(rd_o)    ) lui=rd_o

OPEN(UNIT=fh, FILE=TRIM(filename), STATUS="OLD")

hdr_lngth=0

DO ii=1,10
   READ(fh,'(A)') line
   hdr_lngth=hdr_lngth+LEN(TRIM(line))+1_ik                   ! eol characters, whitechar
   CALL parse(str=line,delims=" ",args=tokens,nargs=ntokens)
   IF (ntokens > 0) THEN
      IF (tokens(1) .EQ. "DIMENSIONS") THEN
         READ(tokens(2),'(I10)')  dims(1)
         READ(tokens(3),'(I10)')  dims(2)
         READ(tokens(4),'(I10)')  dims(3)
         bnds(:,1) = 1
         bnds(:,2) = dims(:)
         sze       = PRODUCT(INT(dims, KIND=INT64))
      ELSEIF (tokens(1) .EQ. "SPACING") THEN
         READ(tokens(2),'(F15.6)') spcng(1)  
         READ(tokens(3),'(F15.6)') spcng(2)  
         READ(tokens(4),'(F15.6)') spcng(3)  
      ELSEIF (tokens(1) .EQ. "DATASET") THEN
         IF (tokens(2) /= "STRUCTURED_POINTS") THEN
            WRITE(lui,'(3A)') "The input file ",filename," does not contain STRUCTURED_POINTS!"
            status = 3_ik
         ENDIF
      ELSEIF (tokens(1) .EQ. "ORIGIN") THEN
         READ(tokens(2),'(F15.6)') origin(1)  
         READ(tokens(3),'(F15.6)') origin(2)  
         READ(tokens(4),'(F15.6)') origin(3)  
      ELSEIF (tokens(1) .EQ. "SCALARS") THEN
         !-- Get data type of the vtk-file
         token(3) = tokens(3)

         SELECT CASE( TRIM( token(3) ) )
         CASE('float');          typ = 'real4'
         CASE('double');         typ = 'real8'
         CASE('int');            typ = 'int4'
         CASE('short');          typ = 'int2'
         CASE('unsigned_short'); typ = 'uint2'
         CASE DEFAULT
            WRITE(*,'(A)') "No valid type given in *.vtk File." 
            status = 1_ik   
         END SELECT
      END IF
   END IF !ntokens <0
END DO

CLOSE(fh)


!-- Check existence of optional variables
IF (PRESENT(sze_o)   ) sze_o    = sze
IF (PRESENT(fov_o)   ) fov_o    = fov
IF (PRESENT(bnds_o)  ) bnds_o   = bnds
IF (PRESENT(status_o)) status_o = status
IF (PRESENT(displacement)) displacement = hdr_lngth

END SUBROUTINE read_vtk_meta

!---------------------------------------------------------------------------------------------------

SUBROUTINE read_raw(filename, type, hdr_lngth, dims, subarray_dims, subarray_origin, subarray, displacement, log_un, status_o)
! MPI Parallel read always reads subarrays.
! log_un exists means "print log"!
! type = 'real4', 'real8, 'int2', 'int4'

CHARACTER(LEN=*)                                                , INTENT(IN)     :: filename
CHARACTER(LEN=*)                                                , INTENT(IN)     :: type
INTEGER  (KIND=MPI_OFFSET_KIND)                                                  :: hdr_lngth
INTEGER  (KIND=ik)    , DIMENSION(3)                            , INTENT(IN)     :: dims
INTEGER  (KIND=ik)    , DIMENSION(3)                            , INTENT(IN)     :: subarray_dims
INTEGER  (KIND=ik)    , DIMENSION(3)                            , INTENT(IN)     :: subarray_origin
INTEGER  (KIND=INT32) , DIMENSION (:,:,:), ALLOCATABLE          , INTENT(OUT)    :: subarray
INTEGER  (KIND=ik)                                    , OPTIONAL, INTENT(IN)     :: displacement
INTEGER  (KIND=ik)                                    , OPTIONAL, INTENT(IN)     :: log_un
INTEGER  (KIND=ik)                                    , OPTIONAL, INTENT(OUT)    :: status_o

! Internal Variables
INTEGER  (KIND=INT16) , DIMENSION (:,:,:), ALLOCATABLE                           :: array_i_two
INTEGER  (KIND=INT32) , DIMENSION (:,:,:), ALLOCATABLE                           :: array_i_four
REAL     (KIND=REAL64), DIMENSION (:,:,:), ALLOCATABLE                           :: array_r_eight
REAL     (KIND=REAL32), DIMENSION (:,:,:), ALLOCATABLE                           :: array_r_four
INTEGER  (KIND=ik)                                                               :: status=0, rd_o
INTEGER  (KIND=ik)                                                               :: fh, ii, jj, kk

IF (PRESENT(displacement)) hdr_lngth = displacement
IF (PRESENT(log_un))            rd_o = log_un


IF (TRIM(type) .EQ. 'real4') THEN

ELSE IF (TRIM(type) .EQ. 'real8') THEN

ELSE IF ((TRIM(type) .EQ. 'int2') .OR. (TRIM(type) .EQ. 'uint2')) THEN

ELSE IF (TRIM(type) .EQ. 'int4') THEN

ELSE 
   status = 1_ik
END IF

END SUBROUTINE read_raw_mpi

END MODULE file_routines_mpi
