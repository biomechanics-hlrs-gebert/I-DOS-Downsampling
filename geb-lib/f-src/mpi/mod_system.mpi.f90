!------------------------------------------------------------------------------
! MODULE: mpi_system
!------------------------------------------------------------------------------
!> @author Johannes Gebert - HLRS - NUM - gebert@hlrs.de
!
!> @brief:
!> Module for reading/writing system information
!------------------------------------------------------------------------------
MODULE mpi_system

    USE ISO_FORTRAN_ENV
    USE MPI
    USE global_std
    USE system
    
    IMPLICIT NONE
 
    CONTAINS
    
    !------------------------------------------------------------------------------
    ! SUBROUTINE: mpi_system_mem_usage
    !------------------------------------------------------------------------------  
    !> @author Johannes Gebert - HLRS - NUM - gebert@hlrs.de
    !
    !> @brief
    !> Determine the memory usage of an MPI communicator
    !
    !> @param[in] communicator Communicator of this process
    !> @param[in] mem_global Global memory usage of this mpi comm (kb)
    !> @param[in] status_global if status_global == size_mpi, everything is fine
    !------------------------------------------------------------------------------  
    SUBROUTINE mpi_system_mem_usage(communicator, mem_global, status_global) 
        
        INTEGER(mik) :: communicator, ierr
        INTEGER(ik) :: mem_global, mem_this_process, status, status_global

        status = 1_mik

        !------------------------------------------------------------------------------  
        ! Get the memory usage of the processes by themselves.
        !------------------------------------------------------------------------------  
        mem_this_process = system_mem_usage()

        !------------------------------------------------------------------------------  
        ! Check for each process whether the memory usage was retrieved correctly.
        !------------------------------------------------------------------------------  
        IF(mem_this_process < 0_mik) status = 0_mik
        
        !------------------------------------------------------------------------------  
        ! Collect the feedbacks. If status_global < size_mpi of the communicator
        ! --> Proper memory readout missing
        !------------------------------------------------------------------------------  
        CALL MPI_REDUCE(status, status_global, 1_mik, &
            MPI_INTEGER8, MPI_SUM, 0_mik, communicator, ierr)
        
        !------------------------------------------------------------------------------  
        ! Collect the global memory usage.
        !------------------------------------------------------------------------------  
        CALL MPI_REDUCE(mem_this_process, mem_global, 1_mik, &
            MPI_INTEGER8, MPI_SUM, 0_mik, communicator, ierr)

    END SUBROUTINE mpi_system_mem_usage
    
END MODULE mpi_system
 