!>-------------------------------------------------------------------
!> mod_file_routines_mpi.f90
!>
!> Author:  Johannes Gebert - HLRS - NUM - »gebert@hlrs.de«
!> Date:    12.09.2021
!> LastMod: 14.09.2021
!>-------------------------------------------------------------------

MODULE file_routines_mpi

USE ISO_FORTRAN_ENV
USE standards
USE strings

IMPLICIT NONE

CONTAINS

 !---------------------------------------------------------------------------------------------------
 SUBROUTINE write_meta_general (fh, filename, type, atStart, spcng, origin, dims)

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

   IF (atStart .EQV. .TRUE.) THEN

      OPEN(UNIT=fh, FILE=TRIM(filename), ACTION='WRITE', STATUS='NEW')

      WRITE(fh,'(A)')           "# vtk DataFile Version 4.2" ! Compatibility issue
      WRITE(fh,'(A)')           "vtk output"
      WRITE(fh,'(A)')           "BINARY"
      WRITE(fh,'(A)')           "DATASET STRUCTURED_POINTS"
      WRITE(fh,'(A,3(I5))')     "DIMENSIONS", dims
      WRITE(fh,'(A,3(F11.6))')  "SPACING ", spcng
      WRITE(fh,'(A,3(F11.6))')  "ORIGIN ", origin
      
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
 END SUBROUTINE write_meta_general

!---------------------------------------------------------------------------------------------------

SUBROUTINE read_vtk_meta(fh, filename, hdr_lngth, dims, origin, spcng, typ)
! log_un exists means "print log"!

INTEGER  (KIND=ik)               , INTENT(IN)  :: fh
CHARACTER(len=*)                 , INTENT(IN)  :: filename
INTEGER  (KIND=ik)               , INTENT(OUT) :: hdr_lngth
INTEGER  (KIND=ik), DIMENSION(3) , INTENT(OUT) :: dims
REAL     (KIND=rk), DIMENSION(3) , INTENT(OUT) :: origin
REAL     (KIND=rk), DIMENSION(3) , INTENT(OUT) :: spcng
CHARACTER(len=*)                 , INTENT(OUT) :: typ

!-- Initialize variables in case they're not used
INTEGER  (KIND=ik) :: ii=0, lui=6, ntokens

CHARACTER(len=mcl) :: line
CHARACTER(len=mcl) :: tokens(100)
CHARACTER(len=mcl), DIMENSION(3) :: token

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
      ELSEIF (tokens(1) .EQ. "SPACING") THEN
         READ(tokens(2),'(F15.6)') spcng(1)  
         READ(tokens(3),'(F15.6)') spcng(2)  
         READ(tokens(4),'(F15.6)') spcng(3)  
      ELSEIF (tokens(1) .EQ. "DATASET") THEN
         IF (tokens(2) /= "STRUCTURED_POINTS") THEN
            WRITE(lui,'(3A)') "The input file ",filename," does not contain STRUCTURED_POINTS!"
         ENDIF
      ELSEIF (tokens(1) .EQ. "ORIGIN") THEN
         READ(tokens(2),'(F15.6)') origin(1)  
         READ(tokens(3),'(F15.6)') origin(2)  
         READ(tokens(4),'(F15.6)') origin(3)  
      ELSEIF (tokens(1) .EQ. "SCALARS") THEN
         !-- Get data type of the vtk-file
         token(3) = tokens(3)

         SELECT CASE( TRIM( token(3) ) )
            CASE('float')         ; typ = 'rk4'
            CASE('double')        ; typ = 'rk8'
            CASE('int')           ; typ = 'ik4'
            CASE('short')         ; typ = 'ik2'
            CASE('unsigned_short'); typ = 'uik2'
            CASE DEFAULT          ; WRITE(*,'(A)') "No valid type given in *.vtk File." 
         END SELECT
      END IF
   END IF !ntokens <0
END DO

CLOSE(fh)

END SUBROUTINE read_vtk_meta

END MODULE file_routines_mpi
