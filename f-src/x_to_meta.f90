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
USE messages_errors
USE meta
USE MPI
USE file_routines_mpi  

IMPLICIT NONE

! MPI variables
INTEGER(KIND=mpi_ik) :: ierr, rank_mpi, size_mpi

! Std variables
CHARACTER(LEN=mcl) :: vtk, new_basename, typ
CHARACTER(LEN=mcl) :: filename='', filename_meta
INTEGER(KIND=ik)   :: hdr

INTEGER(KIND=ik), DIMENSION(3) :: dims
REAL(KIND=rk)   , DIMENSION(3) :: spcng, origin

REAL(KIND=rk) :: start, end

! Binary blob variables
REAL   (KIND=REAL32), DIMENSION(:,:,:), ALLOCATABLE :: rryrk4
REAL   (KIND=REAL64), DIMENSION(:,:,:), ALLOCATABLE :: rryrk8
INTEGER(KIND=INT16) , DIMENSION(:,:,:), ALLOCATABLE :: rryik2
INTEGER(KIND=INT32) , DIMENSION(:,:,:), ALLOCATABLE :: rryik4

!------------------------------------------------------------------------------
! Invoke MPI 
!------------------------------------------------------------------------------
CALL mpi_init(ierr)
CALL print_err_stop(std_out, "MPI_INIT didn't succeed", INT(ierr, KIND=ik))

CALL MPI_COMM_RANK(MPI_COMM_WORLD, rank_mpi, ierr)
CALL print_err_stop(std_out, "MPI_COMM_RANK couldn't be retrieved", INT(ierr, KIND=ik))

CALL MPI_COMM_SIZE(MPI_COMM_WORLD, size_mpi, ierr)
CALL print_err_stop(std_out, "MPI_COMM_SIZE couldn't be retrieved", INT(ierr, KIND=ik))

If (size_mpi < 2) CALL print_err_stop(std_out, "We need at least 2 MPI processes to execute this program.", 1)


!------------------------------------------------------------------------------
! Start actual program
!------------------------------------------------------------------------------
CALL show_title(longname, revision)

CALL GET_COMMAND_ARGUMENT(1, filename)

IF (filename='') CALL print_err_stop(std_out, "No input file given", 1)

CALL meta_create_new(TRIM(ADJUSTL(filename)))

WRITE(std_out, FMT_TXT)
WRITE(std_out, FMT_TXT) "Input:   "//TRIM(ADJUSTL(filename))
WRITE(std_out, FMT_TXT) "Output:  "//in%p_n_bsnm//meta_suf
WRITE(std_out, FMT_TXT)

!------------------------------------------------------------------------------
! Read VTK file header
!------------------------------------------------------------------------------
CALL read_vtk_meta (dh_di, vtk, hdr, dims, origin, spcng, type) 

!------------------------------------------------------------------------------
! Open Vtk as big endian (!) stream
!------------------------------------------------------------------------------
OPEN(UNIT=dh_di, FILE=TRIM(vtk), ACCESS="stream", FORM="unformatted", STATUS="old", CONVERT='SWAP')


! >>>>>>>>>>>>>>>>>>>>>>>>>>DEBUG<<<
! BUILD AN INTERFACE
SELECT CASE(TRIM(typ))
    CASE ('rk4'); ALLOCATE(rryrk4(dims(1),dims(2),dims(3))); READ(UNIT=dh_di, POS=hdr) rryrk4(:,:,:)
    CASE ('rk8'); ALLOCATE(rryrk8(dims(1),dims(2),dims(3))); READ(UNIT=dh_di, POS=hdr) rryrk8(:,:,:)
    CASE ('ik4'); ALLOCATE(rryik4(dims(1),dims(2),dims(3))); READ(UNIT=dh_di, POS=hdr) rryik4(:,:,:)
    CASE DEFAULT; ALLOCATE(rryik2(dims(1),dims(2),dims(3))); READ(UNIT=dh_di, POS=hdr) rryik2(:,:,:)
END SELECT

CLOSE(UNIT=fh_ro)




OPEN (UNIT=fh_ro, FILE=TRIM(filename_raw), ACCESS="stream", FORM="unformatted", STATUS="new")

SELECT CASE(TRIM(typ))
    CASE ('rk4'); WRITE(UNIT=fh_ro) rryrk4(:,:,:); DEALLOCATE(rryrk4)
    CASE ('rk8'); WRITE(UNIT=fh_ro) rryrk8(:,:,:); DEALLOCATE(rryrk8)
    CASE ('ik4'); WRITE(UNIT=fh_ro) rryik4(:,:,:); DEALLOCATE(rryik4)
    CASE DEFAULT; WRITE(UNIT=fh_ro) rryik2(:,:,:); DEALLOCATE(rryik2)
END SELECT

CLOSE(UNIT=fh_ro)

IF(rank_mpi == 0) THEN
   CALL meta_signing(binary)
   CALL meta_close()

   IF (std_out/=6) CALL meta_stop_ascii(fh=std_out, suf='.std_out')

END IF ! (rank_mpi == 0)

Call MPI_FINALIZE(ierr)
CALL print_err_stop(std_out, "MPI_FINALIZE didn't succeed", INT(ierr, KIND=ik))

END PROGRAM xtometa
