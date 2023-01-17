!------------------------------------------------------------------------------
! MODULE: system
!------------------------------------------------------------------------------
!> @author Johannes Gebert - HLRS - NUM - gebert@hlrs.de
!
! @Description:
!> Module containing the formatting of all (error) messages
!------------------------------------------------------------------------------
MODULE system

    USE global_std
    
    IMPLICIT NONE
      
    CONTAINS
    
    !------------------------------------------------------------------------------
    ! FUNCTION: system_mem_usage
    !------------------------------------------------------------------------------
    !> @author Johannes Gebert - HLRS - NUM - gebert@hlrs.de
    !
    ! @description: 
    !> Read the memory usage of the thread calling this function and return it (kb)
    !
    !> @brief
    !> Translate a domain number to its corresponding section
    !
    !> @param[in] domain No of the domain. 
    !> @param[in] size of the domain
    !> @param[out] section of the domain.
    !------------------------------------------------------------------------------
    FUNCTION system_mem_usage() RESULT(RAM_kb)

#ifdef __INTEL_COMPILER
            USE ifport
#endif

        INTEGER :: RAM_kb
        
        CHARACTER(mcl):: filename=' ', line
        CHARACTER(10)  :: pid_char=' '
        INTEGER(ik) :: pid
        LOGICAL :: fex
        
        !------------------------------------------------------------------------------
        ! Reset value to -1 if not found.
        !------------------------------------------------------------------------------
        RAM_kb = -1
        
        !------------------------------------------------------------------------------
        ! Ask the system for the process ID
        !------------------------------------------------------------------------------
        pid = getpid()

        WRITE(pid_char,'(I8)') pid
        filename='/proc/'//TRIM(ADJUSTL(pid_char))//'/status'
        
        !------------------------------------------------------------------------------
        ! Read the file containing the amount of memory used
        !------------------------------------------------------------------------------
        INQUIRE(file=filename, exist=fex)

        IF (.NOT. fex) RETURN
            
        OPEN(unit=100, file=filename, action='read')

        DO
            READ (100, '(A)') line

            IF(line(1:6) == 'VmRSS:') THEN
                READ (line(7:), *) RAM_kb
                EXIT
            END IF
        END DO

        CLOSE(100)
        
    END FUNCTION system_mem_usage

    !------------------------------------------------------------------------------
    ! SUBROUTINE: exec_cmd_line
    !------------------------------------------------------------------------------  
    !> @author Johannes Gebert - HLRS - NUM - gebert@hlrs.de
    !
    !> @brief
    !> Try to execute a command on the cli
    !
    !> @param[in] fh to print to
    !> @param[in] instring Input string
    !> @param[out] outstring Output string
    !------------------------------------------------------------------------------  
    SUBROUTINE exec_cmd_line (cmd, stat, timeout)

        CHARACTER(*), INTENT(IN) :: cmd
        INTEGER(ik), INTENT(OUT) :: stat
        INTEGER(ik), INTENT(IN), OPTIONAL :: timeout

        INTEGER(ik) :: ii, timeout_default, sleep_for
        REAL(ik) :: fraction

        stat = 0

        timeout_default = 10

        IF(PRESENT(timeout)) timeout_default = timeout

        DO ii=1, timeout_default
            !------------------------------------------------------------------------------
            ! Creating the directory may work after several attempts.
            !------------------------------------------------------------------------------
            CALL system(trim(cmd), STATUS=stat)

            IF((stat /= 0) .AND. (ii /= timeout_default)) THEN
                CALL random_number(fraction)

                !------------------------------------------------------------------------------
                ! The sys calls (system and execute_command_line) are not thread safe.
                ! The random number ensures the system call to be made sequentially with 
                ! a high probability, thus failing not as often.
                !------------------------------------------------------------------------------
                sleep_for = ANINT(20._rk*fraction)
                CALL sleep(sleep_for) 
            ELSE
                EXIT
            END IF 
        END DO

    END SUBROUTINE exec_cmd_line

END MODULE system