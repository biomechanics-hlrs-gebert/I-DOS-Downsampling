!------------------------------------------------------------------------------
! MODULE: mpi_user_interaction
!------------------------------------------------------------------------------
!> @author Johannes Gebert - HLRS - NUM - gebert@hlrs.de
!
! @Description:
!> Module containing the mpi user messages.
!------------------------------------------------------------------------------
MODULE mpi_user_interaction

USE ISO_FORTRAN_ENV
USE global_std
USE strings
USE user_interaction
USE MPI

IMPLICIT NONE

CONTAINS
    
!------------------------------------------------------------------------------
! SUBROUTINE: mpi_err
!------------------------------------------------------------------------------  
!> @author Ralf Schneider - HLRS - NUM - schneider@hlrs.de
!
!> @brief
!> Evaluates allocation errors
!
!> @param[in] ierr Errorcode 
!> @param[out] text Message to print
!------------------------------------------------------------------------------  
subroutine mpi_err(ierr, text)

    !-- Dummy parameters
    integer(mik), intent(in) :: ierr
    character(*), intent(in) :: text
    
    if (ierr /= MPI_SUCCESS) THEN
        write(*, "(100('!'))")
        write(*, '(A,I0,A)') 'MPI ERROR :', ierr,";"
        write(*, '(A)') trim(text)
        write(*, "(100('!'))")
        write(*, *) 'Exit ...'
        stop
    end if
    
end subroutine mpi_err

!------------------------------------------------------------------------------
! SUBROUTINE: mest
!------------------------------------------------------------------------------  
!> @author Johannes Gebert - HLRS - NUM - gebert@hlrs.de
!
!> @brief
!> Stop program if meta format returns error. Only print error if rank == 0
!> Otherwise, the cmd line/std_out are carnage
!
!> @param[in] stat Status integer
!> @param[out] abrt Whether to abort the program.
!------------------------------------------------------------------------------
SUBROUTINE mest(stat, abrt)

CHARACTER(*), INTENT(IN) :: stat
LOGICAL, INTENT(INOUT) :: abrt

INTEGER(mik) :: ierr, my_rank

CALL MPI_COMM_RANK(MPI_COMM_WORLD, my_rank, ierr)

IF(stat /= "") THEN
IF (my_rank==0) WRITE(std_out, FMT_ERR) "Error in keyword '"//TRIM(stat)//"'."
    abrt = .TRUE.
END IF 

END SUBROUTINE mest

END MODULE mpi_user_interaction