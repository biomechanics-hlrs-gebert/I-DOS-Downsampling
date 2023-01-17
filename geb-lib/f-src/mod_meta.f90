!------------------------------------------------------------------------------
! MODULE: meta
!------------------------------------------------------------------------------
!> @author Johannes Gebert - HLRS - NUM - gebert@hlrs.de
!
! @Brief:
!> Module containing all meta file read/write routines.
!
! @Description:
!> It is strongly advised, to check the existance of the file before calling 
!> these functions and routines. In case of an error, this library will 
!> invoke an STOP and therefore MPI_ABORT operation with the program exiting
!> improperly.
!------------------------------------------------------------------------------
MODULE meta

   USE ISO_FORTRAN_ENV
   USE strings
   USE user_interaction

IMPLICIT NONE

   INTEGER, PARAMETER :: meta_mik = 4
   INTEGER, PARAMETER :: meta_ik = 8
   INTEGER, PARAMETER :: meta_rk = 8
   INTEGER, PARAMETER :: meta_sk = 4
   INTEGER, PARAMETER :: meta_mcl = 512
   INTEGER, PARAMETER :: meta_scl = 64

   ! Character lengths
   INTEGER, PARAMETER :: kcl    = 25   ! Keyword character  length
   INTEGER, PARAMETER :: ucl    = 8    ! Unit    character  length
   INTEGER, PARAMETER :: stdspc = 39   ! Keyword standard space

   REAL(meta_rk) :: meta_start, meta_end
   
   CHARACTER(kcl) :: global_meta_program_keyword
   CHARACTER(kcl) :: global_meta_prgrm_mstr_app

   LOGICAL :: meta_provenance_compute = .FALSE.

   ! Standard files
   INTEGER(meta_ik), PARAMETER :: fh_meta_in  = 20, fhmei  = 20
   INTEGER(meta_ik), PARAMETER :: fh_meta_put = 21, fhmeo  = 21
   INTEGER(meta_ik), PARAMETER :: fh_mon      = 22, fhmon  = 22
   INTEGER(meta_ik), PARAMETER :: fh_out      = 23, fho    = 23
   INTEGER(meta_ik), PARAMETER :: fh_log      = 24, fhl    = 24
   INTEGER(meta_ik), PARAMETER :: fh_res      = 25, fhr    = 25
   INTEGER(meta_ik), PARAMETER :: fh_csv      = 26, fhc    = 26
   INTEGER(meta_ik), PARAMETER :: fh_head     = 27, fhh    = 27
   INTEGER(meta_ik), PARAMETER :: fh_tex      = 28, fht    = 28
   INTEGER(meta_ik), PARAMETER :: fh_vtk      = 29, fhv    = 29
   INTEGER(meta_ik), PARAMETER :: fh_raw      = 30, fhra   = 30
   CHARACTER(*), PARAMETER :: log_suf  = '.log'
   CHARACTER(*), PARAMETER :: lock_suf = '.lock'
   CHARACTER(*), PARAMETER :: head_suf = '.head'
   CHARACTER(*), PARAMETER :: meta_suf = '.meta'
   CHARACTER(*), PARAMETER :: mon_suf  = '.mon'
   CHARACTER(*), PARAMETER :: res_suf  = '.result'
   CHARACTER(*), PARAMETER :: csv_suf  = '.csv'
   CHARACTER(*), PARAMETER :: tex_suf  = '.tex'
   CHARACTER(*), PARAMETER :: vtk_suf  = '.vtk'
   CHARACTER(*), PARAMETER :: raw_suf  = '.raw'

   ! Meta data basename handling
   TYPE basename
      ! For the use in filenames, a max. length of a part of a basename of kcl characters must suffice.
      ! Nomenclature: dataset_type_purpose_app_features
      CHARACTER(meta_mcl) :: full     = '' ! Including suffix and path
      CHARACTER(meta_mcl) :: path     = '' ! Only the path to the file
      CHARACTER(meta_mcl) :: p_n_bsnm = '' ! Just the path and the basename
      CHARACTER(meta_mcl) :: bsnm     = '' ! Just the basename
      CHARACTER(kcl) :: dataset  = '' ! For example FH01-1 (Femoral Head 1, Scan1)
      CHARACTER(2)   :: type     = '' ! 'cl' - clinical or 'mu' - microfocus
      CHARACTER(3)   :: purpose  = '' ! 'Dev' or 'Pro' (Development or Production)
      CHARACTER(kcl) :: app      = '' ! Application. For example "Binarization"
      CHARACTER(kcl) :: features = '' ! Features. For example the parametrization
   END TYPE basename

   ! Always provide in/out for meta driven environments
   TYPE(basename) :: in, out

   !> Interface: meta_rea, statd
   !> \author Johannes Gebert
   !> \date 10.11.2021
   INTERFACE meta_read
      MODULE PROCEDURE meta_read_C 
      MODULE PROCEDURE meta_read_I0D_mik
      MODULE PROCEDURE meta_read_I0D_ik 
      MODULE PROCEDURE meta_read_I1D_mik
      MODULE PROCEDURE meta_read_I1D_ik
      MODULE PROCEDURE meta_read_R0D_rk
      MODULE PROCEDURE meta_read_R1D_rk
   END INTERFACE meta_read

   !> Interface: meta_write
   !> \author Johannes Gebert
   !> \date 10.11.2021
   INTERFACE meta_write
      MODULE PROCEDURE meta_write_C 
      MODULE PROCEDURE meta_write_I0D 
      MODULE PROCEDURE meta_write_R0D 
      MODULE PROCEDURE meta_write_I1D
      MODULE PROCEDURE meta_write_R1D
   END INTERFACE meta_write

CONTAINS

!------------------------------------------------------------------------------
! SUBROUTINE: meta_handle_lock_file
!------------------------------------------------------------------------------  
!> @author Johannes Gebert, gebert@hlrs.de, HLRS/NUM
!
!> @brief
!> Subroutine to encapsule the lock file handling. The lock file might be used 
!> as an activity tracker for large computations.
!
!> @param[in] restart Whether to restart or not to.
!> @param[in] restart_cmdarg Possible cmd argument override
!------------------------------------------------------------------------------  
SUBROUTINE meta_handle_lock_file(restart, restart_cmdarg)

CHARACTER, INTENT(INOUT) :: restart
CHARACTER, INTENT(IN), OPTIONAL :: restart_cmdarg

LOGICAL :: exist=.FALSE.
INTEGER  (meta_ik) :: ios
CHARACTER(meta_mcl) :: lockname

!------------------------------------------------------------------------------
! Restart handling
! Done after meta_io to decide based on keywords
!------------------------------------------------------------------------------
IF(PRESENT(restart_cmdarg)) THEN
   CALL meta_compare_restart(restart, restart_cmdarg)
END IF

!------------------------------------------------------------------------------
! Automatically aborts if there is no input file found on the drive
!------------------------------------------------------------------------------
lockname=TRIM(in%path)//'.'//TRIM(in%bsnm)//lock_suf

INQUIRE (FILE = TRIM(lockname), EXIST = exist)

IF(((restart == 'N') .OR. (restart == 'NO')) .AND. (exist)) THEN
   mssg='The .*.lock file is set and a restart prohibited by default or the user.'

   INQUIRE (FILE = out%full, EXIST = exist)

   ! Delete out meta if the lock file was set. Otherwise, deleting it manually
   ! would be cumbersome
   IF (exist) CALL execute_command_line ('rm '//TRIM(out%full))

   CALL print_err_stop(std_out, TRIM(ADJUSTL(mssg)), 1_meta_ik)
END IF

!------------------------------------------------------------------------------
! Create a new lock file.
!------------------------------------------------------------------------------
IF(((restart == 'Y') .AND. (.NOT. exist)) .OR. ((restart == 'N') .AND. (.NOT. exist))) THEN
   CALL execute_command_line ('touch '//TRIM(lockname), CMDSTAT=ios)
   CALL print_err_stop(std_out, 'The .*.lock file could not be set.', ios)
END IF

IF((restart == 'Y') .AND. (exist)) CONTINUE

END SUBROUTINE meta_handle_lock_file

!------------------------------------------------------------------------------
! SUBROUTINE: meta_compare_restart
!------------------------------------------------------------------------------  
!> @author Johannes Gebert, gebert@hlrs.de, HLRS/NUM
!
!> @brief
!> compare restart arguments.
!
!> @param[in] restart Whether to restart or not to.
!> @param[in] restart_cmdarg Possible cmd argument override
!------------------------------------------------------------------------------  
SUBROUTINE meta_compare_restart(restart, restart_cmdarg)

CHARACTER, INTENT(INOUT) :: restart
CHARACTER, INTENT(IN)    :: restart_cmdarg

IF ((restart_cmdarg /= '') .AND. (restart_cmdarg /= 'U'))THEN
   mssg = "The keyword »restart« was overwritten by the command flag --"
   IF (restart_cmdarg == 'N') THEN
      restart = restart_cmdarg
      mssg=TRIM(mssg)//"no-"
   ELSE IF (restart_cmdarg == 'Y') THEN
      restart = restart_cmdarg
   END IF

   mssg=TRIM(mssg)//"restart"
   WRITE(std_out, FMT_WRN) TRIM(mssg)
   WRITE(std_out, FMT_SEP)
END IF

END SUBROUTINE meta_compare_restart


!------------------------------------------------------------------------------
! SUBROUTINE: meta_append
!------------------------------------------------------------------------------  
!> @author Johannes Gebert, gebert@hlrs.de, HLRS/NUM
!
!> @brief
!> Subroutine to open a meta file to append data/ keywords
!
!> @param[inout] meta_as_rry Meta data written into a character array
!> @param[in]    size_mpi Size of MPI_COMM
!> @param[inout] stat Status variable
!------------------------------------------------------------------------------  
SUBROUTINE meta_append(meta_as_rry, size_mpi, binary, stat)

CHARACTER(meta_mcl), DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: meta_as_rry      
INTEGER(meta_mik), INTENT(IN) :: size_mpi
CHARACTER(meta_mcl), INTENT(INOUT) :: stat
CHARACTER(*), INTENT(IN) :: binary


!------------------------------------------------------------------------------
! Open/read the input meta file, no matter whether it is a true input or 
! an output file with provenance information
!
! It is important, that this input file already contains all 
! the - for the job relevant - information.
!------------------------------------------------------------------------------  
CALL meta_invoke(meta_as_rry)

!------------------------------------------------------------------------------
! Check how to go on with the input information
!------------------------------------------------------------------------------  
CALL meta_continue(meta_as_rry, size_mpi, binary, stat)

CALL CPU_TIME(meta_start)
END SUBROUTINE meta_append


!------------------------------------------------------------------------------
! SUBROUTINE: meta_create_new
!------------------------------------------------------------------------------  
!> @author Johannes Gebert, gebert@hlrs.de, HLRS/NUM
!
!> @brief
!> Subroutine to create a new meta file
!
!> @param[inout] basename_requested Input basename
!------------------------------------------------------------------------------  
SUBROUTINE meta_create_new(filename_with_suffix)

CHARACTER(*), INTENT(IN) :: filename_with_suffix      

INTEGER  (meta_ik) :: ntokens
CHARACTER(meta_mcl) :: tokens(30)
LOGICAL :: exist

!------------------------------------------------------------------------------
! Automatically aborts if there is no input file found on the drive
!------------------------------------------------------------------------------
INQUIRE (FILE = TRIM(filename_with_suffix), EXIST = exist)
IF (.NOT. exist) THEN
   mssg = "The file "//TRIM(filename_with_suffix)//" does not exist."
   CALL print_err_stop(std_out, TRIM(mssg), 1)
END IF

CALL parse( str=filename_with_suffix, delims=".", args=tokens, nargs=ntokens)

!------------------------------------------------------------------------------
! Accepts any input suffix
!------------------------------------------------------------------------------
CALL parse_basename(filename_with_suffix, "."//tokens(ntokens))

!------------------------------------------------------------------------------
! Create the meta input file
!------------------------------------------------------------------------------
INQUIRE (FILE = TRIM(in%p_n_bsnm)//meta_suf, EXIST = exist)
IF (exist) THEN
   mssg = "The file "//TRIM(in%p_n_bsnm)//meta_suf//" already exists."
   CALL print_err_stop(std_out, TRIM(mssg), 1)
END IF

OPEN(UNIT=fhmeo, FILE=TRIM(in%p_n_bsnm)//meta_suf, &
   ACTION='READWRITE', ACCESS='SEQUENTIAL', STATUS='NEW')

END SUBROUTINE meta_create_new

!------------------------------------------------------------------------------
! SUBROUTINE: meta_invoke
!------------------------------------------------------------------------------  
!> @author Johannes Gebert, gebert@hlrs.de, HLRS/NUM
!
!> @brief
!> Subroutine to open and prepare a meta file for use
!
!> @param[inout] meta_as_rry Meta data written into a character array
!------------------------------------------------------------------------------  
SUBROUTINE meta_invoke(meta_as_rry)

CHARACTER(meta_mcl), DIMENSION(:), INTENT(INOUT), ALLOCATABLE :: meta_as_rry      

! Internal Variables
INTEGER  (meta_ik) :: lines, ii, ntokens
CHARACTER(meta_mcl) :: tokens(30)
LOGICAL :: exist

!------------------------------------------------------------------------------
! Automatically aborts if there is no input file found on the drive
!------------------------------------------------------------------------------
INQUIRE (FILE = TRIM(in%full), EXIST = exist)
IF (.NOT. exist) CALL print_err_stop(std_out, "The file "//TRIM(in%full)//" does not exist.", 1)

CALL parse( str=in%full, delims=".", args=tokens, nargs=ntokens)

IF ( '.'//TRIM(tokens(ntokens)) == meta_suf) THEN
   CALL parse_basename(in%full, meta_suf)
ELSE
   ! File is not a meta file
   CALL print_err_stop(std_out, "The input file is not a *"//meta_suf//" file.", 1)
END IF

!------------------------------------------------------------------------------
! Open the meta input file
!------------------------------------------------------------------------------
OPEN(UNIT=fhmei, FILE=TRIM(in%full), ACTION='READWRITE', ACCESS='SEQUENTIAL', STATUS='OLD')

lines = count_lines(fhmei)

ALLOCATE(meta_as_rry(lines))
!------------------------------------------------------------------------------
! Read all lines into the file
!------------------------------------------------------------------------------
DO ii=1, lines
   READ(fhmei,'(A)') meta_as_rry(ii)
END DO


END SUBROUTINE meta_invoke


!------------------------------------------------------------------------------
! SUBROUTINE: meta_continue
!------------------------------------------------------------------------------  
!> @author Johannes Gebert, gebert@hlrs.de, HLRS/NUM
!
!> @brief
!> Subroutine to continue withe the mode requested by the user.
!
!> @param[in]    m_in Meta data written into a character array
!> @param[in]    size_mpi Size of MPI_COMM
!> @param[in]    binary Name of the programs executable
!> @param[inout] stat Status variable
!------------------------------------------------------------------------------  
SUBROUTINE meta_continue(m_in, size_mpi, binary, stat)

CHARACTER(meta_mcl), DIMENSION(:), INTENT(IN) :: m_in      
INTEGER(meta_mik), INTENT(IN) :: size_mpi
CHARACTER(*), INTENT(IN) :: binary
CHARACTER(meta_mcl), INTENT(INOUT) :: stat
INTEGER(meta_ik) :: ios

CHARACTER(meta_mcl) :: provenance_p_n_bsnm

LOGICAL :: provenance_is_meta = .FALSE., kw_in_programs_scope

CALL meta_read ('NEW_BSNM_FEATURE', m_in, out%features, stat)
CALL meta_read ('NEW_BSNM_PURPOSE', m_in, out%purpose, stat)

!------------------------------------------------------------------------------  
! Check provenance of the data set
! by provenance, you can give the input data as the result data set. 
! The program will check the provenance, read and compute the preceeding 
! data set with the parameters given in the meta file.
!------------------------------------------------------------------------------  
! The "forked" MeRaDat set has to be a copy of the input data set to read from!
! Otherwise, the Parameters may not fit to the dataset. However this is not 
! check by this library to give way more flexibility (!)
!------------------------------------------------------------------------------  
! The forked dataset can then be used for reproducing the results.  
!------------------------------------------------------------------------------  
CALL meta_read ('PROVENANCE_P_N_BSNM', m_in, provenance_p_n_bsnm, stat, kw_in_programs_scope)

IF(kw_in_programs_scope) THEN
   IF(stat/="") THEN
         WRITE(std_out,FMT_MSG) "Keyword 'PROVENANCE_P_N_BSNM' was not given."
   ELSE
      IF( (TRIM(ADJUSTL(in%bsnm)) /= TRIM(ADJUSTL(provenance_p_n_bsnm))))THEN
         INQUIRE(FILE=TRIM(provenance_p_n_bsnm)//TRIM(meta_suf), EXIST=meta_provenance_compute)
      ELSE
         WRITE(std_out,FMT_WRN) "The file specified  by 'PROVENANCE_P_N_BSNM' and the input are the same."
         provenance_is_meta = .TRUE.
      END IF 
   END IF 
END IF 

IF ((.NOT. meta_provenance_compute) .AND. (stat=="") .AND. (kw_in_programs_scope))THEN
   stat = "The data set specified by 'PROVENANCE_P_N_BSNM' was not found. Program has to abort."
ELSE

   IF((meta_provenance_compute) .AND. (.NOT. provenance_is_meta) .AND. (kw_in_programs_scope)) THEN

      !------------------------------------------------------------------------------
      ! This path enables forking from one input to several output meta files
      !------------------------------------------------------------------------------
      out = in

      !------------------------------------------------------------------------------
      ! Close the input file which has to be treated as an output/result file
      ! This works out because the content of the meta file already 
      !------------------------------------------------------------------------------
      ! No copy of the input/output files are necessary, becaues the output file
      ! was already created by the user, prior to calling this program.
      !------------------------------------------------------------------------------
      CLOSE(fhmei)

      WRITE(std_out,FMT_TXT) "Parsing the provenance basename now."
      CALL parse_basename(provenance_p_n_bsnm, meta_suf)

   ELSE
      !------------------------------------------------------------------------------
      ! Copy in to out and assign the new values to update the input meta file.
      !------------------------------------------------------------------------------
      out = in  

      !------------------------------------------------------------------------------
      ! This path behaves like a standard MeRaDat path without a fork
      !------------------------------------------------------------------------------
      ! Build the new outfile path
      !------------------------------------------------------------------------------
      ! Nomenclature: dataset_type_purpose_app_features
      ! This assignment requres the out = in assignment before
      !------------------------------------------------------------------------------
      out%bsnm = TRIM(out%dataset)//&
         '_'//TRIM(out%type)//&
         '_'//TRIM(out%purpose)//&
         '_'//TRIM(global_meta_prgrm_mstr_app)//&
         '_'//TRIM(out%features)

      out%p_n_bsnm = TRIM(out%path)//&
         TRIM(out%bsnm)

      out%full = TRIM(out%p_n_bsnm)//meta_suf

      !------------------------------------------------------------------------------
      ! System call to update the file name of the meta file
      !------------------------------------------------------------------------------
      CALL execute_command_line ('cp '//TRIM(in%full)//' '//TRIM(out%full), CMDSTAT=ios)
      CALL print_err_stop(std_out, 'The update of the meta filename went wrong.', ios)

   END IF

   !------------------------------------------------------------------------------
   ! Open the meta output file
   !------------------------------------------------------------------------------
   INQUIRE(FILE=TRIM(out%full), EXIST=meta_provenance_compute)

   IF (meta_provenance_compute) THEN
      OPEN(UNIT=fhmeo, FILE=TRIM(out%full), ACTION='WRITE', ACCESS='APPEND', STATUS='OLD')
   
      IF ((out%purpose == in%purpose) .AND. (out%features == in%features)) THEN
         WRITE(std_out,FMT_WRN) 'The basename (in part) did not change.'
      END IF


      !------------------------------------------------------------------------------
      ! Only write if the job is not finished
      !------------------------------------------------------------------------------
      CALL meta_read ('JOB_FINISHED', m_in, provenance_p_n_bsnm, stat)

      !------------------------------------------------------------------------------
      ! If the keyword was not found --> Job not finished
      !------------------------------------------------------------------------------
      IF(stat == "JOB_FINISHED" ) THEN
         stat =""
      
         WRITE(fhmeo, '(A)')
         CALL meta_write('PROCESSORS', '(-)', INT(size_mpi, meta_ik))
         CALL meta_write('JOB_STARTED' , '(-)', binary)
         CALL meta_write_sha256sum(binary)

      END IF 
   ELSE
      stat = "The input meta file "//TRIM(out%full)//" was not found."
   END IF
END IF

END SUBROUTINE meta_continue


!------------------------------------------------------------------------------
! SUBROUTINE: meta_start_ascii
!------------------------------------------------------------------------------  
!> @author Johannes Gebert, gebert@hlrs.de, HLRS/NUM
!
!> @brief
!> Subroutine to deal with the logging and renaming of the log file in context 
!> of the meta data approach. 
!
!> @Description
!> This routine only gets called __after__ meta append or meta_close 
!> respectively.
!
!> If the variable restart is not explicitly set .TRUE., the program will not 
!> restart.
!> If the variable in/out are not set, the program will not start/stop 
!> accordingly.
!
!> During computation (before meta_stop_ascii), the files are called
!> 'temporary.suffix' to show which ones are subject to modifications.
!
!> @param[in] fh File handle of the input
!> @param[in] suf Suffix of the file
!> @param[in] access optional type of access
!------------------------------------------------------------------------------  
SUBROUTINE meta_start_ascii(fh, suf, access)

INTEGER(meta_ik), INTENT(IN) :: fh
CHARACTER(*), INTENT(IN) :: suf
CHARACTER(*), INTENT(IN), OPTIONAL :: access

CHARACTER(3) :: status_of_file
CHARACTER(meta_mcl) :: perm_f_suf, access_u
INTEGER(meta_ik) :: ios

LOGICAL :: exist_perm, opened

perm_f_suf = TRIM(out%p_n_bsnm)//TRIM(suf)
  
!------------------------------------------------------------------------------
! Check for a permanent file
!------------------------------------------------------------------------------
INQUIRE (FILE = TRIM(perm_f_suf), OPENED=opened)
INQUIRE (FILE = TRIM(perm_f_suf), EXIST = exist_perm)

!------------------------------------------------------------------------------
! Check whether file needs to be deleted.
!------------------------------------------------------------------------------
access_u = 'APPEND'
IF(exist_perm) THEN
   status_of_file='OLD'
ELSE
   status_of_file='NEW'
!   CALL execute_command_line ('rm -r '//TRIM(out%p_n_bsnm)//TRIM(suf), CMDSTAT=ios)
!   CALL print_err_stop(std_out, '»'//TRIM(out%full)//'« not deletable.', ios)
END IF

IF(PRESENT(access)) access_u = TRIM(access)

IF(.NOT. opened) THEN
      OPEN(UNIT=fh, FILE=TRIM(perm_f_suf), ACTION='WRITE', ACCESS=TRIM(access_u), STATUS=status_of_file)
END IF 

END SUBROUTINE meta_start_ascii

!------------------------------------------------------------------------------
! SUBROUTINE: meta_stop_ascii
!------------------------------------------------------------------------------  
!> @author Johannes Gebert, gebert@hlrs.de, HLRS/NUM
!
!> @brief
!> Subroutine to stop the logging and renaming of additional ascii files.
!
!> @param[in] fh File handle of the input
!> @param[in] suf Suffix of the file
!------------------------------------------------------------------------------  
SUBROUTINE meta_stop_ascii(fh, suf)

INTEGER(meta_ik), INTENT(IN) :: fh
CHARACTER(*), INTENT(IN) :: suf

CHARACTER(meta_mcl) :: perm_f_suf
INTEGER  (meta_ik) :: ios
LOGICAL :: opened, fex

opened = .FALSE.
fex = .FALSE.

perm_f_suf = TRIM(out%p_n_bsnm)//TRIM(suf)

INQUIRE(UNIT=fh, OPENED=opened)
IF(opened) CLOSE(fh)

!------------------------------------------------------------------------------
! In case of an existing ascii file, the in%p_n_bsnm is relevant.
!------------------------------------------------------------------------------
INQUIRE(FILE = TRIM(in%p_n_bsnm)//TRIM(suf), EXIST=fex)

IF(fex) THEN 
   CALL execute_command_line ('cp '//TRIM(in%p_n_bsnm)//TRIM(suf)//' '&
      //TRIM(out%p_n_bsnm)//TRIM(suf), CMDSTAT=ios)

   IF(ios /= 0_meta_ik) THEN
      mssg='Can not copy the suffix_file from »'//TRIM(in%p_n_bsnm)//TRIM(suf)//'« to '&
         //TRIM(out%p_n_bsnm)//TRIM(suf)//'.'
      CALL print_err_stop(std_out, mssg, 0)
   END IF
END IF 

END SUBROUTINE meta_stop_ascii


!------------------------------------------------------------------------------
! SUBROUTINE: meta_existing_ascii
!------------------------------------------------------------------------------  
!> @author Johannes Gebert, gebert@hlrs.de, HLRS/NUM
!
!> @brief
!> Subroutine to check and open ascii files which must exist, for example to
!> read input data.
!
!> @description
!> Stop the file with meta_stop_ascii
!
!> @param[in] fh File handle of the input
!> @param[in] suf Suffix of the file
!> @param[out] number_lines Amount of lines in file
!------------------------------------------------------------------------------  
SUBROUTINE meta_existing_ascii(fh, suf, number_lines)

INTEGER(meta_ik), INTENT(IN) :: fh
CHARACTER(*), INTENT(IN) :: suf
INTEGER(meta_ik), INTENT(OUT) :: number_lines

LOGICAL :: fex

fex = .FALSE.

INQUIRE(FILE = TRIM(in%p_n_bsnm)//TRIM(ADJUSTL(suf)), EXIST=fex)

IF(fex) THEN
   OPEN(UNIT=fh, FILE=TRIM(in%p_n_bsnm)//TRIM(ADJUSTL(suf)),&
      ACTION='READWRITE', STATUS='OLD')

   number_lines = count_lines(fh)
ELSE
   mssg = "The input file requested does not exist: "//&
      TRIM(in%p_n_bsnm)//TRIM(ADJUSTL(suf))
   CALL print_err_stop(std_out, mssg, 1)
END IF

END SUBROUTINE meta_existing_ascii



!------------------------------------------------------------------------------
! SUBROUTINE: count_lines
!------------------------------------------------------------------------------  
!> @author Ralf Schneider, schneider@hlrs.de, HLRS/NUM
!
!> @brief
!> Truncate a keyword which was too long. Could do other stuff as well.
!
!> @param[in] un Unit of the file to read from. 
!> @return no_lines Number of lines
!------------------------------------------------------------------------------  
function count_lines(un) result(no_lines)

Integer, Intent(in) :: un
Integer(ik) :: no_lines

Integer :: io_stat
CHARACTER(2) :: temp_char

io_stat  = 0
no_lines = 0
 
Rewind(un)

Do While (io_stat == 0)

   Read(un,'(A)', End=1000, iostat=io_stat) temp_char
   no_lines = no_lines + 1

End Do

1000 Continue

Rewind(un)

End function count_lines

!------------------------------------------------------------------------------
! SUBROUTINE: check_keyword
!------------------------------------------------------------------------------  
!> @author Johannes Gebert, gebert@hlrs.de, HLRS/NUM
!
!> @brief
!> Truncate a keyword which was too long. Could do other stuff as well.
!
!> @param[in] keyword Keyword to check
!------------------------------------------------------------------------------  
SUBROUTINE check_keyword(keyword)

CHARACTER(*) :: keyword
CHARACTER(kcl) :: kywd_lngth

kywd_lngth = '' 

IF(LEN_TRIM(keyword) .GT. LEN(kywd_lngth)) THEN

   WRITE(std_out,FMT_WRN) "The keyword »"//TRIM(keyword)//"« is longer"
   WRITE(std_out,FMT_WRN) "than the convention allows and therefore truncated!"
   
   kywd_lngth = keyword(1:LEN(kywd_lngth))
ELSE
   kywd_lngth = keyword
END IF

END SUBROUTINE check_keyword


!------------------------------------------------------------------------------
! SUBROUTINE: parse_basename
!------------------------------------------------------------------------------  
!> @author Johannes Gebert, gebert@hlrs.de, HLRS/NUM
!
!> @brief
!> Parse the basename
!
!> @param[in] filename Full name of the file
!> @param[in] suf Expected suffix
!------------------------------------------------------------------------------  
SUBROUTINE parse_basename(filename, suf)

CHARACTER(*) :: filename, suf
INTEGER  (meta_ik) :: ntokens
CHARACTER(meta_mcl) :: tokens(30), sgmnts

in%full = TRIM(ADJUSTL(filename))

IF(in%full(LEN_TRIM(in%full)-4:LEN_TRIM(in%full)) /= meta_suf) THEN
   in%full = TRIM(ADJUSTL(in%full))//meta_suf
END IF 

!------------------------------------------------------------------------------
! Parse all basename and path details.
!------------------------------------------------------------------------------
in%p_n_bsnm = in%full(1:LEN_TRIM(in%full)-LEN_TRIM(TRIM(ADJUSTL(suf)))) 

CALL parse( str=TRIM(in%p_n_bsnm), delims="/", args=tokens, nargs=ntokens)

in%path = in%p_n_bsnm(1:LEN_TRIM(in%p_n_bsnm) - LEN_TRIM(tokens(ntokens)))     
in%bsnm = TRIM(tokens(ntokens))

CALL parse( str=TRIM(in%bsnm), delims="_", args=tokens, nargs=ntokens)

IF (ntokens /= 5) THEN
   write(sgmnts, '(I0)') ntokens
   mssg = "Basename with "//TRIM(sgmnts)//" segments given, which is invalid: "//TRIM(in%p_n_bsnm)
   CALL print_err_stop(std_out, mssg, 1)
END IF

in%dataset = TRIM(tokens(1))
in%type    = TRIM(tokens(2))
in%purpose = TRIM(tokens(3))
in%app      = TRIM(tokens(4))
in%features = TRIM(tokens(5))

END SUBROUTINE parse_basename

!------------------------------------------------------------------------------
! SUBROUTINE: check_unit
!------------------------------------------------------------------------------  
!> @author Johannes Gebert, gebert@hlrs.de, HLRS/NUM
!
!> @brief
!> Routine to truncate a keyword which was too long. Could do other stuff 
!> as well.
!
!> @param[in] keyword Keyword to check
!------------------------------------------------------------------------------  
SUBROUTINE check_unit(unit)

CHARACTER(*) :: unit
CHARACTER(ucl) :: unit_lngth

! Check unit length for convention and proper formatting
IF(LEN_TRIM(unit) .GT. LEN(unit_lngth)) THEN

   WRITE(std_out, '(A)') ''

   WRITE(std_out,FMT_WRN) "The unit "//TRIM(unit)//" is longer than"
   WRITE(std_out,FMT_WRN) "the convention allows and therefore truncated!"

   unit_lngth = unit(1:LEN(unit_lngth))
ELSE
   unit_lngth = unit
END IF

END SUBROUTINE check_unit


!------------------------------------------------------------------------------
! SUBROUTINE: meta_check_contains_program
!------------------------------------------------------------------------------  
!> @author Johannes Gebert, gebert@hlrs.de, HLRS/NUM
!
!> @brief
!> Check if the requested program is documented in the meta file
!
!> @param[in] program_id program_id to read
!> @param[in] dims Dimensions requested
!> @param[in] m_in Array of lines of ascii meta file
!> @param[in] chars Datatype to read in
!------------------------------------------------------------------------------
SUBROUTINE meta_check_contains_program (program_id, m_in, success)
   
CHARACTER(*), INTENT(IN) :: program_id
CHARACTER(meta_mcl), DIMENSION(:), INTENT(IN) :: m_in
LOGICAL :: success, prog_id_found

INTEGER(meta_ik) :: ii, ntokens
CHARACTER(meta_mcl) :: tokens(30)

success = .FALSE.
prog_id_found = .FALSE.

!------------------------------------------------------------------------------
! Parse Data out of the input array
!------------------------------------------------------------------------------
DO ii =1, SIZE(m_in) 
   CALL parse(str=m_in(ii), delims=' ', args=tokens, nargs=ntokens)

   !------------------------------------------------------------------------------
   ! Check for program id
   !------------------------------------------------------------------------------
   IF (tokens(1) == 'p') THEN
      IF (tokens(2) == TRIM(program_id)) prog_id_found = .TRUE.
   END IF

   !------------------------------------------------------------------------------
   ! If program id was found - check for the finished tag.
   ! Now, we can safely assume, that the requested program run successfully.
   !------------------------------------------------------------------------------
   IF ((prog_id_found) .AND. (tokens(2) == "JOB_FINISHED")) success = .TRUE.
END DO

END SUBROUTINE meta_check_contains_program


!------------------------------------------------------------------------------
! SUBROUTINE: meta_extract_keyword_data
!------------------------------------------------------------------------------  
!> @author Johannes Gebert, gebert@hlrs.de, HLRS/NUM
!
!> @brief
!> Module to extract the data string of keywords. 
!
!> @Description
!> Module to parse information of keywords. 
!> An arbitrary Keyword with up to »kcl« characters may be specified.
!> The program reads keywords as long as they are before or withing the owns 
!> programs scope.
! 
!> @param[in] keyword Keyword to read
!> @param[in] dims Dimensions requested
!> @param[in] m_in Array of lines of ascii meta file
!> @param[in] res_tokens Parsed data
!> @param[in] stat Status of the file
!------------------------------------------------------------------------------
SUBROUTINE meta_extract_keyword_data (keyword, dims, m_in, res_tokens, kw_in_programs_scope, stat)
   
CHARACTER(*), INTENT(IN) :: keyword
INTEGER(meta_ik), INTENT(IN) :: dims
CHARACTER(meta_mcl), DIMENSION(:), INTENT(IN) :: m_in
CHARACTER(meta_scl), INTENT(INOUT) :: stat
LOGICAL, INTENT(OUT) :: kw_in_programs_scope

CHARACTER(meta_mcl) :: res_tokens(30)
INTEGER(meta_ik) :: res_ntokens

! Internal variables
INTEGER(meta_ik) :: ii, ntokens
CHARACTER(meta_mcl) :: tokens(30)
LOGICAL :: kywd_found, programs_scope

stat = ""
kywd_found = .FALSE.
programs_scope = .FALSE.
kw_in_programs_scope = .FALSE.

CALL check_keyword(keyword)

!------------------------------------------------------------------------------
! Parse Data out of the input array
!------------------------------------------------------------------------------
DO ii =1, SIZE(m_in) 
   CALL parse(str=m_in(ii), delims=' ', args=tokens, nargs=ntokens)

   SELECT CASE(tokens(1))
      CASE('*', 'd', 'r', 'w')

         IF (tokens(2) == TRIM(keyword)) THEN
            kywd_found = .TRUE.

            !------------------------------------------------------------------------------
            ! Store the keywords data.
            ! Following m_in(ii) - lines -  will overwrite this information.
            !------------------------------------------------------------------------------
            res_tokens = tokens
            res_ntokens = ntokens

            !------------------------------------------------------------------------------
            ! Exit, if the keyword appears the first time in the programs scope.
            !------------------------------------------------------------------------------
            IF ((programs_scope) .AND. (tokens(1) /= 'w')) THEN
               kw_in_programs_scope = .TRUE.
               EXIT
            END IF
         END IF
      CASE('p')
         !------------------------------------------------------------------------------
         ! The scope of another program begins.
         !------------------------------------------------------------------------------
         IF ((.NOT. programs_scope) .AND. (tokens(2) == TRIM(global_meta_program_keyword))) THEN
            programs_scope = .TRUE.
         ELSE IF (programs_scope) THEN
            !------------------------------------------------------------------------------
            ! Leave, if the scope of the next program begins.
            !------------------------------------------------------------------------------
            EXIT
         END IF

   END SELECT
END DO

IF((res_ntokens < dims+2) .AND. (.NOT. kywd_found)) stat = keyword
IF (.NOT. kywd_found) stat = keyword

END SUBROUTINE meta_extract_keyword_data

!------------------------------------------------------------------------------
! SUBROUTINE: meta_read_C
!------------------------------------------------------------------------------  
!> @author Johannes Gebert, gebert@hlrs.de, HLRS/NUM
!
!> @brief
!> Wrapper to parse character Keywords.
!
!> @param[in] keyword Keyword to read
!> @param[in] m_in Array of lines of ascii meta file
!> @param[in] chars Datatype to read in
!> @param[in] stat Status of the routine
!------------------------------------------------------------------------------
SUBROUTINE meta_read_C (keyword, m_in, chars, stat, kw_in_programs_scope)
   
CHARACTER(*), INTENT(IN) :: keyword
CHARACTER(meta_mcl), DIMENSION(:), INTENT(IN)  :: m_in      
CHARACTER(*), INTENT(OUT) :: chars 
CHARACTER(meta_scl), INTENT(INOUT) :: stat
LOGICAL, INTENT(INOUT), OPTIONAL :: kw_in_programs_scope

CHARACTER(meta_mcl) :: tokens(30)
LOGICAL :: prgrm_scp = .FALSE.

CALL meta_extract_keyword_data (keyword, 1, m_in, tokens, prgrm_scp, stat)

IF(PRESENT(kw_in_programs_scope)) kw_in_programs_scope = prgrm_scp

chars = TRIM(ADJUSTL(tokens(3)))

END SUBROUTINE meta_read_C

!------------------------------------------------------------------------------
! SUBROUTINE: meta_read_I0D_ik
!------------------------------------------------------------------------------  
!> @author Johannes Gebert, gebert@hlrs.de, HLRS/NUM
!
!> @brief
!> Wrapper to parse Keywords with 0D integer data.
! 
!> @param[in] keyword Keyword to read
!> @param[in] m_in Array of lines of ascii meta file
!> @param[in] int_0D Datatype to read in
!> @param[in] stat Status of the routine
!------------------------------------------------------------------------------
SUBROUTINE meta_read_I0D_ik (keyword, m_in, int_0D, stat, kw_in_programs_scope)
     
CHARACTER(*), INTENT(IN) :: keyword
CHARACTER(meta_mcl), DIMENSION(:), INTENT(IN) :: m_in      
INTEGER(meta_ik), INTENT(OUT) :: int_0D 
CHARACTER(meta_scl), INTENT(INOUT) :: stat
LOGICAL, INTENT(INOUT), OPTIONAL :: kw_in_programs_scope

CHARACTER(meta_mcl) :: tokens(30)
LOGICAL :: prgrm_scp = .FALSE.

IF(PRESENT(kw_in_programs_scope)) prgrm_scp = kw_in_programs_scope

CALL meta_extract_keyword_data (keyword, 1, m_in, tokens, prgrm_scp, stat)

READ(tokens(3), '(I12)') int_0D 

END SUBROUTINE meta_read_I0D_ik

!------------------------------------------------------------------------------
! SUBROUTINE: meta_read_I0D_mik
!------------------------------------------------------------------------------  
!> @author Johannes Gebert, gebert@hlrs.de, HLRS/NUM
!
!> @brief
!> Wrapper to parse Keywords with 0D integer data.
! 
!> @param[in] keyword Keyword to read
!> @param[in] m_in Array of lines of ascii meta file
!> @param[in] int_0D Datatype to read in
!> @param[in] stat Status of the routine
!------------------------------------------------------------------------------
SUBROUTINE meta_read_I0D_mik (keyword, m_in, int_0D, stat, kw_in_programs_scope)
     
CHARACTER(*), INTENT(IN) :: keyword
CHARACTER(meta_mcl), DIMENSION(:), INTENT(IN) :: m_in      
INTEGER(meta_mik), INTENT(OUT) :: int_0D 
CHARACTER(meta_scl), INTENT(INOUT) :: stat
LOGICAL, INTENT(INOUT), OPTIONAL :: kw_in_programs_scope

CHARACTER(meta_mcl) :: tokens(30)
LOGICAL :: prgrm_scp = .FALSE.

IF(PRESENT(kw_in_programs_scope)) prgrm_scp = kw_in_programs_scope

CALL meta_extract_keyword_data (keyword, 1, m_in, tokens, prgrm_scp, stat)

READ(tokens(3), '(I12)') int_0D 

END SUBROUTINE meta_read_I0D_mik


!------------------------------------------------------------------------------
! SUBROUTINE: meta_read_R0D_rk
!------------------------------------------------------------------------------  
!> @author Johannes Gebert, gebert@hlrs.de, HLRS/NUM
!
!> @brief
!> Wrapper to parse Keywords with 0D floating point data.
! 
!> @param[in] keyword Keyword to read
!> @param[in] m_in Array of lines of ascii meta file
!> @param[in] real_0D Datatype to read in
!> @param[in] stat Status of the routine
!------------------------------------------------------------------------------
SUBROUTINE meta_read_R0D_rk (keyword, m_in, real_0D, stat, kw_in_programs_scope)
     
CHARACTER(*), INTENT(IN) :: keyword
CHARACTER(meta_mcl), DIMENSION(:), INTENT(IN) :: m_in      
REAL(meta_rk), INTENT(OUT) :: real_0D 
CHARACTER(meta_scl), INTENT(INOUT) :: stat
LOGICAL, INTENT(INOUT), OPTIONAL :: kw_in_programs_scope

CHARACTER(meta_mcl) :: tokens(30)
LOGICAL :: prgrm_scp = .FALSE.

IF(PRESENT(kw_in_programs_scope)) prgrm_scp = kw_in_programs_scope

CALL meta_extract_keyword_data (keyword, 1, m_in, tokens, prgrm_scp, stat)

READ(tokens(3), '(F39.10)') real_0D 

END SUBROUTINE meta_read_R0D_rk

!------------------------------------------------------------------------------
! SUBROUTINE: meta_read_I1D_ik
!------------------------------------------------------------------------------  
!> @author Johannes Gebert, gebert@hlrs.de, HLRS/NUM
!
!> @brief
!> Wrapper to parse Keywords with 1D integer data.
! 
!> @param[in] fh File handle to read a keyword from.
!> @param[in] keyword Keyword to read
!> @param[in] m_in Array of lines of ascii meta file
!> @param[in] int_1D Datatype to read in
!> @param[in] stat Status of the routine
!------------------------------------------------------------------------------
SUBROUTINE meta_read_I1D_ik (keyword, m_in, int_1D, stat, kw_in_programs_scope)

CHARACTER(*), INTENT(IN) :: keyword
CHARACTER(meta_mcl), DIMENSION(:), INTENT(IN)  :: m_in      
INTEGER(meta_ik), DIMENSION(:), INTENT(OUT) :: int_1D 
CHARACTER(meta_scl), INTENT(INOUT) :: stat
LOGICAL, INTENT(INOUT), OPTIONAL :: kw_in_programs_scope

CHARACTER(meta_mcl) :: tokens(30), analayze_tokens(30), args(30)
INTEGER(meta_ik) :: nargs, ii
LOGICAL :: prgrm_scp = .FALSE.

IF(PRESENT(kw_in_programs_scope)) prgrm_scp = kw_in_programs_scope

CALL meta_extract_keyword_data (keyword, SIZE(int_1D), m_in, tokens, prgrm_scp, stat)

analayze_tokens = tokens

!------------------------------------------------------------------------------
! Check if it is a float
!------------------------------------------------------------------------------
DO ii=3, 2+SIZE(int_1D)
   CALL parse(analayze_tokens(ii), ".", args, nargs)
   IF(nargs > 1) stat = "Invalid Keyword content! -> "//TRIM(keyword)
END DO

IF (stat == "") READ(tokens(3:2+SIZE(int_1D)), '(I12)') int_1D

END SUBROUTINE meta_read_I1D_ik


!------------------------------------------------------------------------------
! SUBROUTINE: meta_read_I1D_mik
!------------------------------------------------------------------------------  
!> @author Johannes Gebert, gebert@hlrs.de, HLRS/NUM
!
!> @brief
!> Wrapper to parse Keywords with 1D integer data.
! 
!> @param[in] fh File handle to read a keyword from.
!> @param[in] keyword Keyword to read
!> @param[in] m_in Array of lines of ascii meta file
!> @param[in] int_1D Datatype to read in
!> @param[in] stat Status of the routine
!------------------------------------------------------------------------------
SUBROUTINE meta_read_I1D_mik (keyword, m_in, int_1D, stat, kw_in_programs_scope)

CHARACTER(*), INTENT(IN) :: keyword
CHARACTER(meta_mcl), DIMENSION(:), INTENT(IN)  :: m_in      
INTEGER(meta_mik), DIMENSION(:), INTENT(OUT) :: int_1D 
CHARACTER(meta_scl), INTENT(INOUT) :: stat
LOGICAL, INTENT(INOUT), OPTIONAL :: kw_in_programs_scope

CHARACTER(meta_mcl) :: tokens(30)
LOGICAL :: prgrm_scp = .FALSE.

IF(PRESENT(kw_in_programs_scope)) prgrm_scp = kw_in_programs_scope

CALL meta_extract_keyword_data (keyword, SIZE(int_1D), m_in, tokens, prgrm_scp, stat)

READ(tokens(3:2+SIZE(int_1D)), '(I12)') int_1D

END SUBROUTINE meta_read_I1D_mik

!------------------------------------------------------------------------------
! SUBROUTINE: meta_read_R1D_rk
!------------------------------------------------------------------------------  
!> @author Johannes Gebert, gebert@hlrs.de, HLRS/NUM
!
!> @brief
!> Wrapper to parse Keywords with 1D integer data.
! 
!> @param[in] keyword Keyword to read
!> @param[in] m_in Array of lines of ascii meta file
!> @param[in] real_1D Datatype to read in
!> @param[in] stat Status of the routine
!------------------------------------------------------------------------------
SUBROUTINE meta_read_R1D_rk (keyword, m_in, real_1D, stat, kw_in_programs_scope)

CHARACTER(*), INTENT(IN) :: keyword
CHARACTER(meta_mcl), DIMENSION(:), INTENT(IN) :: m_in      
REAL(meta_rk), DIMENSION(:), INTENT(OUT) :: real_1D 
CHARACTER(meta_scl), INTENT(INOUT) :: stat
LOGICAL, INTENT(INOUT), OPTIONAL :: kw_in_programs_scope

CHARACTER(meta_mcl) :: tokens(30)
LOGICAL :: prgrm_scp = .FALSE.

IF(PRESENT(kw_in_programs_scope)) prgrm_scp = kw_in_programs_scope

CALL meta_extract_keyword_data (keyword, SIZE(real_1D), m_in, tokens, prgrm_scp, stat)

READ(tokens(3:2+SIZE(real_1D)), '(F39.10)') real_1D

END SUBROUTINE meta_read_R1D_rk


!------------------------------------------------------------------------------
! SUBROUTINE: meta_write_keyword
!------------------------------------------------------------------------------  
!> @author Johannes Gebert, gebert@hlrs.de, HLRS/NUM
!
!> @brief
!> Module to write finalized strings of keywords
!
!> @param[in] keyword Keyword to write
!> @param[in] stdspcfill String with data
!> @param[in] unit Unit of the value
!------------------------------------------------------------------------------
SUBROUTINE meta_write_keyword (keyword, stdspcfill, unit) ! fh
   
CHARACTER(*), INTENT(IN) :: keyword
CHARACTER(*), INTENT(IN) :: stdspcfill 
CHARACTER(*), INTENT(IN) :: unit

INTEGER(ik) :: rpt
CHARACTER(meta_scl) :: frmt, str
CHARACTER(8)  :: date
CHARACTER(10) :: time
CHARACTER(5)  :: timezone

CALL check_keyword(keyword)
CALL check_unit(unit)

WRITE(frmt, '(A,I0,A)') "(2A, T", kcl, ")"
WRITE(fhmeo, frmt, ADVANCE='NO') "w ", keyword

WRITE(frmt, '(A,I0,A)') "(A, T", stdspc+1, ")"
WRITE(fhmeo, frmt, ADVANCE='NO') TRIM(ADJUSTL(stdspcfill))

!------------------------------------------------------------------------------
! Only write if stdspcfill (are of actual information/data) was not overflowed
! < instead of <= to get 1 space clearance
!------------------------------------------------------------------------------  
IF(LEN_TRIM(ADJUSTL(stdspcfill)) <  stdspc) THEN
   WRITE(frmt, '(A,I0,A)') "(A, T", ucl+1, ")"
   WRITE(fhmeo, frmt, ADVANCE='NO') unit
   rpt=0
ELSE
   rpt = stdspc+ucl-LEN_TRIM(ADJUSTL(stdspcfill))
END IF

! Same as comment before
IF(LEN_TRIM(ADJUSTL(stdspcfill)) <  stdspc+ucl) THEN
   CALL DATE_AND_TIME(DATE=date, TIME=time, ZONE=timezone)

   str = '' ! Clear string
   str = REPEAT(' ', rpt)//date(7:8)//'.'//date(5:6)//'.'//date(1:4)
   str = TRIM(str)//' '//time(1:2)//':'//time(3:4)//':'//time(5:10)
   str = TRIM(str)//' '//timezone

   WRITE(fhmeo, '(A)') TRIM(str)
ELSE
   WRITE(fhmeo, '(A)') "" ! Basically a newline
END IF
END SUBROUTINE meta_write_keyword


!------------------------------------------------------------------------------
! SUBROUTINE: meta_write_sha256sum
!------------------------------------------------------------------------------  
!> @author Johannes Gebert, gebert@hlrs.de, HLRS/NUM
!
!> @brief
!> Module to write finalized strings of keywords
!
!> @param[in] binary_name Name of the programs executable
!------------------------------------------------------------------------------
SUBROUTINE meta_write_sha256sum(binary_name)
   
CHARACTER(*), INTENT(IN) :: binary_name

CHARACTER(kcl-1) :: keyword = ''
CHARACTER(meta_scl) :: frmt, stdspcfill
INTEGER(meta_ik), DIMENSION(5) :: stat = 0
INTEGER(meta_ik) :: ios

LOGICAL :: exist

!------------------------------------------------------------------------------
! Write "Keyword"
!------------------------------------------------------------------------------
keyword = "w SHA256SUM_OF_BINARY"

WRITE(frmt, '(A,I0,A)') "(2A, T", kcl, ")"
WRITE(fhmeo, frmt, ADVANCE='NO') keyword

!------------------------------------------------------------------------------
! Check the buffer file
!------------------------------------------------------------------------------
INQUIRE(FILE = 'temp_buffer', EXIST = exist)

IF (exist) THEN
   CALL EXECUTE_COMMAND_LINE ('rm -r temp_buffer', CMDSTAT=ios)   

   IF(ios /= 0_meta_ik) THEN
      mssg='Can not delete the temp_buffer'
      CALL print_err_stop(std_out, mssg, 0)
      stat(1) = 1      
   END IF
END IF

!------------------------------------------------------------------------------
! Check for auxiliary programs
!------------------------------------------------------------------------------
CALL EXECUTE_COMMAND_LINE("which cut > /dev/null 2> /dev/null", CMDSTAT=stat(2))
CALL EXECUTE_COMMAND_LINE("which sha256sum > /dev/null 2> /dev/null", CMDSTAT=stat(3))

!------------------------------------------------------------------------------
! Deal with the buffer file
!------------------------------------------------------------------------------
IF(SUM(stat)==0) THEN
   OPEN(UNIT=9, FILE='temp_buffer', ACTION='READWRITE', STATUS='NEW')

   CALL EXECUTE_COMMAND_LINE("sha256sum "//TRIM(ADJUSTL(binary_name))//" | cut -d ' ' -f 1 >> 'temp_buffer'", CMDSTAT=stat(4))

   READ(9, '(A)', iostat=stat(5)) stdspcfill

   CLOSE(9)
END IF

IF (SUM(stat) == 0) THEN
   WRITE(fhmeo, frmt) TRIM(ADJUSTL(stdspcfill))
ELSE
   WRITE(fhmeo, frmt) "Could not get sha256sum. One of the previous system calls failed."
END IF

INQUIRE(FILE='temp_buffer', EXIST=exist)

IF (exist) CALL EXECUTE_COMMAND_LINE ('rm -r temp_buffer', CMDSTAT=ios)   

END SUBROUTINE meta_write_sha256sum


!------------------------------------------------------------------------------
! SUBROUTINE: meta_write_C
!------------------------------------------------------------------------------  
!> @author Johannes Gebert, gebert@hlrs.de, HLRS/NUM
!
!> @brief
!> Module to write keywords with character output. 
!
!> @param[in] keyword Keyword to write
!> @param[in] stdspcfill Characters to write
!------------------------------------------------------------------------------
SUBROUTINE meta_write_C (keyword, unit, stdspcfill)
   
CHARACTER(*), INTENT(IN) :: keyword, unit, stdspcfill 

CALL meta_write_keyword (keyword, stdspcfill, unit)

END SUBROUTINE meta_write_C

!------------------------------------------------------------------------------
! SUBROUTINE: meta_write_I0D
!------------------------------------------------------------------------------  
!> @author Johannes Gebert, gebert@hlrs.de, HLRS/NUM
!
!> @brief
!> Module to write keywords of type integer dim 0.
!
!> @param[in] keyword Keyword to write
!> @param[in] unit Unit of the value
!> @param[in] int_0D Datatype to read in
!------------------------------------------------------------------------------
SUBROUTINE meta_write_I0D (keyword, unit, int_0D)
   
CHARACTER(*), INTENT(IN) :: keyword, unit
INTEGER(meta_ik), INTENT(IN) :: int_0D 

CHARACTER(meta_scl) :: stdspcfill

WRITE(stdspcfill, '(I0)') int_0D

CALL meta_write_keyword (keyword, stdspcfill, unit)

END SUBROUTINE meta_write_I0D


!------------------------------------------------------------------------------
! SUBROUTINE: meta_write_I0D_long
!------------------------------------------------------------------------------  
!> @author Johannes Gebert, gebert@hlrs.de, HLRS/NUM
!
!> @brief
!> Module to write keywords of type integer dim 0. Specific version to 
!> deal with numbers greater than INT32 can deal with.
!
!> @param[in] keyword Keyword to write
!> @param[in] unit Unit of the value
!> @param[in] int_0D Datatype to read in
!------------------------------------------------------------------------------
SUBROUTINE meta_write_I0D_long (keyword, unit, int_0D)
   
CHARACTER(*), INTENT(IN) :: keyword, unit
INTEGER(INT64), INTENT(IN) :: int_0D 

CHARACTER(meta_scl) :: stdspcfill

WRITE(stdspcfill, '(I0)') int_0D

CALL meta_write_keyword (keyword, stdspcfill, unit)

END SUBROUTINE meta_write_I0D_long

!------------------------------------------------------------------------------
! SUBROUTINE: meta_write_R0D
!------------------------------------------------------------------------------  
!> @author Johannes Gebert, gebert@hlrs.de, HLRS/NUM
!
!> @brief
!> Module to write keywords of type Real dim 0.
!
!> @param[in] keyword Keyword to write
!> @param[in] unit Unit of the value
!> @param[in] real_0D Datatype to read in
!------------------------------------------------------------------------------
SUBROUTINE meta_write_R0D (keyword, unit, real_0D)
   
CHARACTER(*), INTENT(IN) :: keyword, unit
REAL(meta_ik), INTENT(IN) :: real_0D 

CHARACTER(meta_scl) :: stdspcfill, frmt

WRITE(frmt, '(A,I0,A)') "(F", stdspc, ".7)"
WRITE(stdspcfill, frmt) real_0D ! '(F30.7)'

CALL trimzero(stdspcfill)

CALL meta_write_keyword (keyword, stdspcfill, unit)

END SUBROUTINE meta_write_R0D


!------------------------------------------------------------------------------
! SUBROUTINE: meta_write_I1D
!------------------------------------------------------------------------------  
!> @author Johannes Gebert, gebert@hlrs.de, HLRS/NUM
!
!> @brief
!> Module to write keywords of type integer dim 1.
!
!> @param[in] keyword Keyword to write
!> @param[in] unit Unit of the value
!> @param[in] int_0D Datatype
!------------------------------------------------------------------------------
SUBROUTINE meta_write_I1D (keyword, unit, int_1D)
   
CHARACTER(*), INTENT(IN) :: keyword, unit
INTEGER(meta_ik), INTENT(IN), DIMENSION(:) :: int_1D 

CHARACTER(meta_scl) :: stdspcfill, str, frmt
INTEGER  (meta_ik) :: ii

stdspcfill = ''
str = ''

DO ii=1, SIZE(int_1D)
   str = ''
   WRITE(frmt, '(A,I0,A)') "(I", (stdspc/3)-1, ")"
   WRITE(str, frmt) int_1D(ii) ! '(I0)'
   stdspcfill = TRIM(stdspcfill)//' '//TRIM(str)
END DO

CALL meta_write_keyword (keyword, stdspcfill, unit)

END SUBROUTINE meta_write_I1D

!------------------------------------------------------------------------------
! SUBROUTINE: meta_write_R1D
!------------------------------------------------------------------------------  
!> @author Johannes Gebert, gebert@hlrs.de, HLRS/NUM
!
!> @brief
!> Module to write keywords of type Real dim 1.
!
!> @param[in] keyword Keyword to write
!> @param[in] unit Unit of the value
!> @param[in] real_1D Datatype
!------------------------------------------------------------------------------
SUBROUTINE meta_write_R1D (keyword, unit, real_1D)
   
CHARACTER(*), INTENT(IN) :: keyword, unit
REAL(meta_rk), INTENT(IN), DIMENSION(:) :: real_1D 

CHARACTER(meta_scl) :: stdspcfill, str, frmt
INTEGER  (meta_ik) :: ii

stdspcfill = ''
str = ''

DO ii=1, SIZE(real_1D)
   str = ''
   WRITE(frmt, '(A,I0,A)') "(F", (stdspc/3)-1, ".7)"
   WRITE(str, frmt) real_1D(ii)
   ! CALL trimzero(str) ! Choose preferred formatting

   stdspcfill = TRIM(stdspcfill)//' '//TRIM(str)
END DO

CALL meta_write_keyword (keyword, stdspcfill, unit)

END SUBROUTINE meta_write_R1D

!------------------------------------------------------------------------------
! SUBROUTINE: meta_close
!------------------------------------------------------------------------------
!> @author Johannes Gebert, gebert@hlrs.de, HLRS/NUM
!
!> @brief
!> Subroutine to close a meta file.
!------------------------------------------------------------------------------
SUBROUTINE meta_close(meta_as_rry, no_restart_required)

LOGICAL :: opened
CHARACTER(meta_mcl), DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: meta_as_rry      
LOGICAL, INTENT(INOUT), OPTIONAL :: no_restart_required
REAL(rk) :: job_finished
CHARACTER(meta_scl) :: stat

LOGICAL :: no_restart = .TRUE.

IF(PRESENT(no_restart_required)) no_restart = no_restart_required

stat = ""
CALL CPU_TIME(meta_end)

!------------------------------------------------------------------------------
! Only write if the job is finished and the Keyword was not already written.
!------------------------------------------------------------------------------
CALL meta_read ('JOB_FINISHED', meta_as_rry, job_finished, stat)

IF((no_restart) .AND. (stat/="")) THEN

   WRITE(fhmeo, '(A)')
   CALL meta_write('JOB_FINISHED' , '(-)', (meta_end))

   WRITE(fhmeo, '(A)')
   WRITE(fhmeo, "(100('-'))")
END IF 

!------------------------------------------------------------------------------
! Check and close files - Routine: (fh, filename, abrt, stat)
!------------------------------------------------------------------------------
INQUIRE(UNIT=fhmei, OPENED=opened)
IF(opened) CLOSE (fhmei)

INQUIRE(UNIT=fhmeo, OPENED=opened)
IF(opened) CLOSE (fhmeo)
 
END SUBROUTINE meta_close


!------------------------------------------------------------------------------
! SUBROUTINE: meta_delete_empty_file
!------------------------------------------------------------------------------
!> @author Johannes Gebert, gebert@hlrs.de, HLRS/NUM
!
!> @brief
!> Check filesize and delete if 0.
!
!> @param(in) filename Path and name of the file to be checked and deleted.
!------------------------------------------------------------------------------
SUBROUTINE meta_delete_empty_file(filename)

CHARACTER(*), INTENT(IN) :: filename

INTEGER(meta_ik) :: filesize = 0, ios

!------------------------------------------------------------------------------
! Check and close files - Routine: (fh, filename, abrt, stat)
!------------------------------------------------------------------------------
INQUIRE(FILE=TRIM(ADJUSTL(filename)), SIZE=filesize)

IF(filesize == 0) THEN
   CALL execute_command_line ('rm -r '//TRIM(ADJUSTL(filename)), CMDSTAT=ios)
END IF
 
END SUBROUTINE meta_delete_empty_file

END MODULE meta


!------------------------------------------------------------------------------
! MODULE: meta_puredat_interface
!------------------------------------------------------------------------------
!> @author Johannes Gebert, gebert@hlrs.de, HLRS/NUM
!
! @Description:
!> Module to convert a .raw/.meta data description into a PureDat description.
!
! REVISION HISTORY:
! 27 11 2021 - Initial version
!------------------------------------------------------------------------------
MODULE meta_puredat_interface

   USE meta
   USE user_interaction

IMPLICIT NONE

CONTAINS

!------------------------------------------------------------------------------
! SUBROUTINE: convert_meta_to_puredat
!------------------------------------------------------------------------------
!> @author Johannes Gebert, gebert@hlrs.de, HLRS/NUM
!
!> @brief
!> One-off special purpose interface to convert the meta to the PureDat format
!> The routine requires an opened meta file!
!
!> @param[in] free_file_handle File handle for use in this routine
!> @param[in] p_n_bsnm Path and basename of the *.meta and the *.raw file
!------------------------------------------------------------------------------
SUBROUTINE convert_meta_to_puredat(free_file_handle, m_rry)

INTEGER(meta_ik), INTENT(IN) :: free_file_handle
CHARACTER(meta_mcl), DIMENSION(:), ALLOCATABLE, INTENT(INOUT), OPTIONAL :: m_rry      

CHARACTER(meta_mcl) :: suf, datatype, branch_description, field_content_desc, osagcs, stat

REAL(meta_rk), DIMENSION(3) :: grid_spacings, origin_shift

! INTEGER(meta_ik), DIMENSION(7) :: bytesizes = [ 1, 2, 4, 8, 8, 1, 1]
INTEGER(meta_ik), DIMENSION(7,3) :: stda ! Stream data, 7 streams; no_of_data, lb, ub
INTEGER(meta_ik), DIMENSION(3) :: vox_per_dim, origin
INTEGER(meta_ik) :: stdout, rawsize, rawdata, ii, my_size, my_pos, ios

LOGICAL :: opened, fex

!------------------------------------------------------------------------------
! PureDat/Meta Module Formatters
!------------------------------------------------------------------------------
CHARACTER(*), PARAMETER :: PDM_arrowsA  = "('<',A,'> ', A)"
CHARACTER(*), PARAMETER :: PDM_arrowsL  = "('<',A,'> ', L)"
CHARACTER(*), PARAMETER :: PDM_arrowsI0 = "('<',A,'> ', I0)"
CHARACTER(*), PARAMETER :: PDM_branch   = "('<==branch==>')"

!------------------------------------------------------------------------------
! Initialize variables
!------------------------------------------------------------------------------
stdout = 6_meta_ik
stda = 0_meta_ik
opened = .FALSE.
fex = .FALSE.

!------------------------------------------------------------------------------
! Check whether the meta file is opened and establish the proper status
!------------------------------------------------------------------------------
INQUIRE(UNIT=fhmei, OPENED=opened)

IF(.NOT. opened) THEN
   OPEN(UNIT=fhmei, FILE=TRIM(in%full), ACTION='WRITE', ACCESS='APPEND', STATUS='OLD')
END IF 

!------------------------------------------------------------------------------
! Gather the size of the resulting stream
! Existance of the file is not necessarily required.
!------------------------------------------------------------------------------
INQUIRE(FILE=TRIM(in%p_n_bsnm)//raw_suf, EXIST=fex, SIZE=rawsize)
! IF(.NOT. fex) CALL print_err_stop(stdout, TRIM(in%p_n_bsnm)//raw_suf//" does not exist.", 1)

!------------------------------------------------------------------------------
! Read all required keywords to write the information ino the PureDat format
! stdout will be given to redirect errors to the command line or error file
!------------------------------------------------------------------------------
CALL meta_read('CT_SCAN', m_rry, branch_description, stat)

branch_description = TRIM(branch_description)//" scalar data structure"
 
CALL meta_read('INTERNAL_ID', m_rry, field_content_desc, stat)
CALL meta_read('DIMENSIONS', m_rry, vox_per_dim, stat)
CALL meta_read('SPACING', m_rry, grid_spacings, stat)
CALL meta_read('ORIGIN', m_rry, origin, stat)
CALL meta_read('ORIGIN_SHIFT_GLBL', m_rry, origin_shift, stat)
CALL meta_read('TYPE_RAW', m_rry, datatype, stat)

!------------------------------------------------------------------------------
! Handle the stream files
!------------------------------------------------------------------------------
suf=''
rawdata=0
SELECT CASE(TRIM(datatype))
   CASE('ik1')
      suf = ".int1.st"
      rawdata = 1
      stda(rawdata,:) = [rawsize, 1_meta_ik, rawsize] 
   CASE('ik2')
      suf = ".int2.st"
      rawdata = 2 
      stda(rawdata,:) = [rawsize/2, 1_meta_ik, rawsize/2] 
   CASE('ik4')
      suf = ".int4.st"
      rawdata = 3 
      stda(rawdata,:) = [rawsize/4, 1_meta_ik, rawsize/4] 
   CASE('ik8')
      suf = ".int8.st"
      rawdata = 4 
      stda(rawdata,:) = [rawsize/8, 1_meta_ik, rawsize/8] 
   CASE('rk8')
      suf = ".real8.st"
      rawdata = 5 
      stda(rawdata,:) = [rawsize/8, 1_meta_ik, rawsize/8] 
END SELECT

IF(suf /= '') THEN
    INQUIRE(FILE=TRIM(in%p_n_bsnm)//TRIM(suf), EXIST=fex)

    IF(fex) THEN 
        CONTINUE
    ELSE
        INQUIRE(FILE=TRIM(in%p_n_bsnm)//".raw ", EXIST=fex)
    
        IF (fex) THEN
            CALL EXECUTE_COMMAND_LINE &
                ("cp -r "//TRIM(in%p_n_bsnm)//".raw "//TRIM(in%p_n_bsnm)//TRIM(suf), CMDSTAT=ios)

            WRITE(std_out, FMT_WRN) "Raw file copied to integer stream! May take/took a while..."

            IF(ios /= 0) CALL print_err_stop(stdout, &
                "Renaming "//TRIM(in%p_n_bsnm)//".raw to "//TRIM(in%p_n_bsnm)//TRIM(suf)//" failed.", 1)
        END IF 
    END IF

ELSE
   CALL print_err_stop(stdout, 'No datatype given to convert meta/raw to PureDat.', 1)
END IF

!------------------------------------------------------------------------------
! Invoke the header file
!------------------------------------------------------------------------------
INQUIRE(FILE=TRIM(in%p_n_bsnm)//head_suf, EXIST=fex)
IF(fex) CALL print_err_stop(stdout, TRIM(in%p_n_bsnm)//head_suf//" already exists.", 1)

OPEN(UNIT=fhh, FILE=TRIM(in%p_n_bsnm)//head_suf, &
   ACTION='WRITE', ACCESS='SEQUENTIAL', STATUS='NEW')

!------------------------------------------------------------------------------
! Write the header header :-) 
! This stuff is hardcoded and not flexible yet.
!------------------------------------------------------------------------------
WRITE(fhh, PDM_branch)
WRITE(fhh, PDM_arrowsA) "description", "'"//TRIM(branch_description)//"'"
WRITE(fhh, PDM_arrowsI0) "no_of_branches", 0
WRITE(fhh, PDM_arrowsI0) "no_of_leaves", 6 
WRITE(fhh, PDM_arrowsL)  "streams_allocated", .TRUE.  
WRITE(fhh, PDM_arrowsI0) "size_int1_stream", stda(1,1)
WRITE(fhh, PDM_arrowsI0) "size_int2_stream", stda(2,1)
WRITE(fhh, PDM_arrowsI0) "size_int4_stream", stda(3,1) + 6 ! origin, d)
WRITE(fhh, PDM_arrowsI0) "size_int8_stream", stda(4,1)
WRITE(fhh, PDM_arrowsI0) "size_real_stream", stda(5,1) + 6 ! origin_shift, sp)
WRITE(fhh, PDM_arrowsI0) "size_char_stream", stda(6,1) + LEN_TRIM(field_content_desc)
WRITE(fhh, PDM_arrowsI0) "size_log_stream" , stda(7,1)

!------------------------------------------------------------------------------
! Write the meta data into the stream files
! Only int4, real8 and chars do get additional data.
!------------------------------------------------------------------------------
DO ii=1, 6 
   ! Not integrated with 2nd SELECT CASE(ii) to get a proper CONTINUE and suf
   SELECT CASE(ii)
      CASE(1); suf = ".int1.st"
      CASE(2); suf = ".int2.st"
      CASE(3); suf = ".int4.st"
      CASE(4); suf = ".int8.st"
      CASE(5); suf = ".real8.st"
      CASE(6); suf = ".char.st"
   END SELECT

   IF(ii == rawdata) THEN

      INQUIRE(FILE=TRIM(in%p_n_bsnm)//TRIM(suf), SIZE=my_size)
      

      ! IF ((my_size/PRODUCT(vox_per_dim)) /= bytesizes(ii)) THEN
      !    WRITE(std_out,'(A,I0)') "Size of scalar data input: ", my_size
      !    WRITE(std_out,'(A,I0)') "Amount of points in image: ", PRODUCT(vox_per_dim)
      !    WRITE(std_out,'(A,I0)') "Expected size per entry: ", bytesizes(ii), " Byte(s)"
      !    WRITE(std_out,'(2A)') "Filename: ", TRIM(in%p_n_bsnm)//TRIM(suf)

      !    mssg = ""
      !    mssg = "Filesize of the raw data does not match the dimensions and data type.&
      !       & Please check the raw data or the fortran source code of the meta/raw converter.&
      !       & Converter assumes that only one blob of data set is containted in the raw file!&
      !       & Converter cannot deal with multiple binary images within 1 raw file at the moment."
      !    CALL print_err_stop(stdout, mssg, 1) 
      ! END IF

      ! It is the file of the original *.raw data
      OPEN(UNIT=free_file_handle, FILE=TRIM(in%p_n_bsnm)//TRIM(suf), &
         ACCESS='STREAM', STATUS='OLD')


   ELSE IF (ii == 6) THEN
      ! It is the character stream file
      INQUIRE(FILE=TRIM(in%p_n_bsnm)//TRIM(suf), EXIST=fex)
      IF(fex) CALL print_err_stop(stdout, TRIM(in%p_n_bsnm)//TRIM(suf)//" already exists.", 1)

      OPEN(UNIT=free_file_handle, FILE=TRIM(in%p_n_bsnm)//TRIM(suf), &
         ACCESS='STREAM', STATUS='NEW')
   ELSE
      ! It is another stream file
      INQUIRE(FILE=TRIM(in%p_n_bsnm)//TRIM(suf), EXIST=fex)
      IF(fex) CALL print_err_stop(stdout, TRIM(in%p_n_bsnm)//TRIM(suf)//" already exists.", 1)

      OPEN(UNIT=free_file_handle, FILE=TRIM(in%p_n_bsnm)//TRIM(suf), &
         ACCESS='STREAM', STATUS='NEW')
   END IF

   INQUIRE(UNIT=free_file_handle, SIZE=my_size)

   my_pos = my_size+1

   !------------------------------------------------------------------------------
   ! Write the binary information ('phi') to file
   !------------------------------------------------------------------------------
   SELECT CASE(TRIM(datatype))
   CASE('ik1')
      IF(ii == 1) CALL write_leaf_to_header(fhh, "Scalar data", ii, stda(ii,:))
   CASE('ik2')
      IF(ii == 2) CALL write_leaf_to_header(fhh, "Scalar data", ii, stda(ii,:))
   CASE('ik4')
      IF(ii == 3) CALL write_leaf_to_header(fhh, "Scalar data", ii, stda(ii,:))
   END SELECT

   !------------------------------------------------------------------------------
   ! Write specific information to file
   !------------------------------------------------------------------------------
   SELECT CASE(ii)
      CASE(3)
         stda(ii,:) = [3, stda(ii,3)+1, stda(ii,3)+3]
         CALL write_leaf_to_header(fhh, "Number of voxels per direction", ii, stda(ii,:))           
         WRITE(free_file_handle, POS=my_pos) INT(vox_per_dim, 4)

         ! stda relative to values before (!)
         stda(ii,:) = [3, stda(ii,3)+1, stda(ii,3)+3]
         CALL write_leaf_to_header(fhh, "Origin", ii, stda(ii,:))
         WRITE(free_file_handle, POS=my_pos+4*3) INT(origin, 4)

      CASE(5)
         stda(ii,:) = [3, stda(ii,3)+1, stda(ii,3)+3]
         CALL write_leaf_to_header(fhh, "Grid spacings", ii, stda(ii,:))
         WRITE(free_file_handle, POS=my_pos) grid_spacings

         stda(ii,:) = [3, stda(ii,3)+1, stda(ii,3)+3]

         osagcs = "Origin shift against global coordinate system"
         CALL write_leaf_to_header(fhh, TRIM(osagcs), ii, stda(ii,:))
         WRITE(free_file_handle, POS=my_pos+8*3) origin_shift

      CASE(6)
         osagcs = "Field content description"
         CALL write_leaf_to_header(fhh, TRIM(osagcs), &
            ii, stda(ii,:)+[LEN_TRIM(field_content_desc), &
            stda(ii,3)+1, stda(ii,3)+LEN_TRIM(field_content_desc)])
         WRITE(free_file_handle, POS=my_pos) TRIM(field_content_desc)
   END SELECT

   CLOSE (free_file_handle)

   CALL meta_delete_empty_file(TRIM(in%p_n_bsnm)//TRIM(suf))
END DO

CLOSE(fhh)

END SUBROUTINE convert_meta_to_puredat


!------------------------------------------------------------------------------
! SUBROUTINE: write_leaf_to_header
!------------------------------------------------------------------------------
!> @author Johannes Gebert, gebert@hlrs.de, HLRS/NUM
!
!> @brief
!> Write the leaf data to the PureDat header.
!
!> @param[in] fh File handle
!> @param[in] desc Description of the leaf
!> @param[in] type Type of data o.t.l.
!> @param[in] stda Stream data o.t.l.
!------------------------------------------------------------------------------
SUBROUTINE write_leaf_to_header(fh, desc, type, stda)

CHARACTER(*), INTENT(IN) :: desc
INTEGER(meta_ik), INTENT(IN) :: fh, type
INTEGER(meta_ik), DIMENSION(3), INTENT(IN):: stda ! no_of_data, lb, ub

WRITE(fh, "('<--leaf-->')")
WRITE(fh, "('<description> ', A)") "'"//TRIM(desc)//"'"
WRITE(fh, "('<no_of_data> ', I0)") stda(1)
WRITE(fh, "('<type_of_data> ', I0)") type
WRITE(fh, "('<lower_bound> ', I0)") stda(2)
WRITE(fh, "('<upper_bound> ', I0)") stda(3)

END SUBROUTINE write_leaf_to_header

END MODULE meta_puredat_interface
