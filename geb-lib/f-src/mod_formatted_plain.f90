!------------------------------------------------------------------------------
! MODULE: formatted_plain
!------------------------------------------------------------------------------
!> @author Johannes Gebert - HLRS - NUM - gebert@hlrs.de
!
! @description: 
!> "Formatted I/O for plain ascii files that are not related to PureDat, Meta, 
!> and log/mon messages. 
!------------------------------------------------------------------------------
MODULE formatted_plain

USE ISO_FORTRAN_ENV
USE global_std
USE mechanical
USE strings
USE user_interaction
USE math

IMPLICIT NONE


!------------------------------------------------------------------------------  
! The formats are independent of other plain text formats to extract a higher
! number of digits, which maintains accuracy better.
!------------------------------------------------------------------------------  
CHARACTER(*), PARAMETER :: FINT = "(I0, A)"
CHARACTER(*), PARAMETER :: FREAL = "(F0.3, A)"
CHARACTER(*), PARAMETER :: FSCI  = "(E17.10, A)"
CHARACTER(*), PARAMETER :: FCHAR = "(2A)"

INTEGER(ik), PARAMETER :: table_data_hdr         = 90
INTEGER(ik), PARAMETER :: table_data_hdr_eff_num = 581

INTERFACE csv_append
    MODULE PROCEDURE csv_append_int 
    MODULE PROCEDURE csv_append_real
    MODULE PROCEDURE csv_append_chars
END INTERFACE csv_append

CONTAINS

!------------------------------------------------------------------------------
! SUBROUTINE: write_matrix
!------------------------------------------------------------------------------  
!> @author Johannes Gebert - HLRS - NUM - gebert@hlrs.de
!> @author Ralf Schneider - HLRS - NUM - schneider@hlrs.de
!
!> @brief
!> Subroutine to print regular tensors respectively matrices.
!
!> @Description
!> Please provide mat_real OR mat_in :-)
!> Automatically writes "sym" if hide_zeros .EQV. .TRUE. and left triangle = 0
!> Hide zeroes is set as default.
!> Accepted formats: 'std'/'standard' for scientific formatting and
!> 'spl'/'simple' for traditional formatting
!
!> @param[in] fh Handle of file to print to
!> @param[in] name Name of the object to print
!> @param[in] mat Actual matrix
!> @param[in] fmti Formatting of the data
!> @param[in] unit Physical unit of the information to print
!------------------------------------------------------------------------------
SUBROUTINE write_matrix(fh, name, mat, fmti, unit)

INTEGER(INT64), INTENT(IN) :: fh   
REAL(rk), INTENT(IN), DIMENSION(:, :) :: mat    
CHARACTER(*), INTENT(IN) :: name 
CHARACTER(*), INTENT(IN), OPTIONAL :: fmti, unit 

! Internal variables 
CHARACTER(mcl) :: fmt_a, sep, nm_fmt, text, fmt_u
INTEGER(ik) :: prec, fw, nm_fmt_lngth, ii, jj, kk, dim1, dim2
REAL(rk) :: sym_out
LOGICAL :: sym

!------------------------------------------------------------------------------
! Initialize and check for presence of the variables
!------------------------------------------------------------------------------
dim1 = SIZE(mat, 1)
dim2 = SIZE(mat, 2)
fmt_u = 'standard'
mssg = '' 
text = ''

prec = PRECISION(mat)
fw = prec+8
sym = .FALSE.


IF (PRESENT(unit)) THEN
    IF (unit /= '') text = " Unit: ("//TRIM(unit)//")"
END IF

!------------------------------------------------------------------------------
! Check symmetry
!------------------------------------------------------------------------------
IF (dim1 == dim2) THEN
    sym = .TRUE.
    sym_out = check_sym(mat)
END IF     
!------------------------------------------------------------------------------
! Generate formats
!------------------------------------------------------------------------------
IF(PRESENT(fmti)) THEN
    IF(fmti /="") fmt_u = fmti
END IF

SELECT CASE (TRIM(fmt_u))
   CASE ('std', 'standard')
        WRITE(fmt_a, "(3(A,I0),A)") "(",dim2,"(E",fw,".",prec,"E2))"
        WRITE(sep , "(A,I0,A)")    "(",fw*dim2,"('-'))"

        ! Calculate text and unit length. If name to long - overflow formaming to the right
        nm_fmt_lngth  = fw*dim2-4-LEN_TRIM(name)-LEN_TRIM(text)

   CASE ('spl', 'simple')
        WRITE(fmt_a,  "(3(A,I0),A)") "(",dim2,"(F15.4))"
        WRITE(sep ,  "(A,I0,A)")    "(",dim2*15,"('-'))"        

        ! Calculate text and unit length. If name to long - overflow formaming to the right
        nm_fmt_lngth  = dim2*15-4-LEN_TRIM(name)-LEN_TRIM(text) 

   CASE('wxm', 'wxmaxima')
       WRITE(fmt_a, '(5(A,I0),A)')  "(' [',",dim2-1,"(E",fw,".",prec,"E2,','),E",fw,".",prec,"E2,'],' )"

       WRITE(fh,"(A,A)")TRIM(name),": matrix("

       DO kk = 1, dim1 - 1
          WRITE(fh, fmt_a) mat(kk,:)
       END DO

       WRITE(fmt_a,'(5(A,I0),A)')  "(' [',",dim2-1,"(E",fw,".",prec,"E2,','),E",fw,".",prec,"E2,']);' )"

       WRITE(fh, fmt_a) mat(dim1, :)
END SELECT

IF (nm_fmt_lngth .LT. 1_ik) nm_fmt_lngth = 1_ik
WRITE(nm_fmt, "(A,I0,A)")  "(2('-') ,3A,", nm_fmt_lngth ,"('-'), A)"    



!------------------------------------------------------------------------------
! Write output
!------------------------------------------------------------------------------
WRITE(fh, sep)                                    ! Separator
WRITE(fh, nm_fmt) ' ',TRIM(name), ' ', TRIM(text) ! Named separator

DO ii=1, dim1
    DO jj=1, dim2

        IF ((sym) .AND. (ii==dim1) .AND. (jj==1)) THEN
            SELECT CASE(fmt_u)
            CASE('spl', 'simple')
                WRITE(fh, '(A)', ADVANCE='NO') "symmetric      "
            CASE('std', 'standard')
                WRITE(fh, '(A)', ADVANCE='NO') "   symmetric           "
            END SELECT

        ELSE IF ((sym) .AND. (ii==dim1) .AND. (jj==2)) THEN
            !------------------------------------------------------------------------------
            ! Numbers are returned = 0._rk, but formatting is special here.
            !------------------------------------------------------------------------------
            IF (ABS(sym_out) <=  num_zero) THEN
                sym_out = 0._rk
            END IF

            WRITE(fh, fmt_a, ADVANCE='NO') sym_out          
        ELSE
            IF ((ABS(mat(ii,jj)) >=  num_zero) .AND. ((.NOT. sym) .OR. ((sym) .AND. (jj .GE. ii)))) THEN 
                WRITE(fh, fmt_a, ADVANCE='NO') mat (ii,jj)
            ELSE
                SELECT CASE(fmt_u)
                CASE('spl', 'simple')
                    WRITE(fh, '(A)', ADVANCE='NO') "          .    "
                CASE('std', 'standard')
                    WRITE(fh, '(A)', ADVANCE='NO') "   .                   "
                END SELECT
            END IF
        END IF

    END DO

    WRITE(fh,'(A)') ''
END DO        

WRITE(fh, '(A)') '' ! Newline & Carriage return
END SUBROUTINE write_matrix

!------------------------------------------------------------------------------
! SUBROUTINE: write_matrix_int
!------------------------------------------------------------------------------  
!> @author Johannes Gebert - HLRS - NUM - gebert@hlrs.de
!> @author Ralf Schneider - HLRS - NUM - schneider@hlrs.de
!
!> @brief
!> Subroutine to print matrices of integers. Lean functionality. 
!> Choose reals for other functionality.
!
!> @Description
!> Please provide mat_real OR mat_in :-)
!> Automatically writes "sym" if hide_zeros .EQV. .TRUE. and left triangle = 0
!> Hide zeroes is set as default.
!> Accepted formats: 'std'/'standard' for scientific formatting and
!> 'spl'/'simple' for traditional formatting
!
!> @param[in] fh Handle of file to print to
!> @param[in] name Name of the object to print
!> @param[in] mat Actual matrix
!> @param[in] fmti Formatting of the data
!> @param[in] unit Physical unit of the information to print
!------------------------------------------------------------------------------
SUBROUTINE write_matrix_int(fh, name, mat, unit)

INTEGER(ik), INTENT(IN) :: fh, mat(:,:)
CHARACTER(*), INTENT(IN) :: name, unit

CHARACTER(mcl) :: fmt_a, sep, nm_fmt, text
INTEGER(ik) :: fw, nm_fmt_lngth, ii, jj, dim2

!------------------------------------------------------------------------------
! Initialize and check for presence of the variables
!------------------------------------------------------------------------------
text = ''
dim2 = SIZE(mat, 2)
fw=8_ik
IF (unit /= '') text = " Unit: ("//TRIM(unit)//")"

!------------------------------------------------------------------------------
! Generate formats
!------------------------------------------------------------------------------
WRITE(fmt_a, "(2(A,I0),A)") "(",dim2,"(I", fw, "))s"
WRITE(sep,   "(A,I0,A)")    "(",dim2*fw,"('-'))"

! Calculate text and unit length. If name to long - overflow formaming to the right
nm_fmt_lngth  = fw*dim2-4-LEN_TRIM(name)-LEN_TRIM(text)

IF (nm_fmt_lngth <= 1_ik) nm_fmt_lngth = 1_ik
WRITE(nm_fmt, "(A,I0,A)")  "(2('-') ,3A,", nm_fmt_lngth ,"('-'), A)"    

!------------------------------------------------------------------------------
! Write output
!------------------------------------------------------------------------------
WRITE(fh, sep)                                    ! Separator
WRITE(fh, nm_fmt) ' ',TRIM(name), ' ', TRIM(text) ! Named separator

DO ii=1, SIZE(mat, 1)
    DO jj=1, SIZE(mat, 2)
        WRITE(fh, fmt_a, ADVANCE='NO') mat (ii,jj)
    END DO

    WRITE(fh,'(A)') ''
END DO        

WRITE(fh, '(A)') '' ! Newline & Carriage return
END SUBROUTINE write_matrix_int

!------------------------------------------------------------------------------
! SUBROUTINE: underscore_to_blank
!------------------------------------------------------------------------------  
!> @author Johannes Gebert - HLRS - NUM - gebert@hlrs.de
!
!> @brief
!> Replaces underscores with blanks.
!
!> @param[in] instring Input string
!> @param[out] outstring Output string
!------------------------------------------------------------------------------  
SUBROUTINE underscore_to_blank (instring, outstring)
  ! This whole subroutine is a workaround :-)
  CHARACTER(*) :: instring, outstring
  INTEGER(ik) :: ii

  outstring=instring
  DO ii=1, LEN_TRIM(instring)
    IF (instring(ii:ii) == '_')  outstring(ii:ii) = ' '
  END DO

  outstring=ADJUSTL(TRIM(outstring))
END SUBROUTINE underscore_to_blank

!------------------------------------------------------------------------------
! SUBROUTINE: wih
!------------------------------------------------------------------------------  
!> @author Johannes Gebert - HLRS - NUM - gebert@hlrs.de
!
!> @brief
!> Write invalid header
!
!> @param[in] inv Logical whether the token is invalid or not
!> @param[in] tkn Token
!------------------------------------------------------------------------------  
SUBROUTINE wih (inv, tkn)

    CHARACTER(*), INTENT(IN) :: tkn
    LOGICAL, INTENT(INOUT) :: inv
  
    IF(inv) WRITE(std_out, FMT_WRN) TRIM(ADJUSTL(tkn))//" invalid."
    inv = .FALSE.

END SUBROUTINE wih

!------------------------------------------------------------------------------
! SUBROUTINE: check_tensor_2nd_rank_R66_header
!------------------------------------------------------------------------------  
!> @author Johannes Gebert, gebert@hlrs.de, HLRS/NUM
!
!> @brief
!> Validate the header of a tensor_2nd_rank_R66 file
!
!> @param[in] header Header of the tensor_2nd_rank_R66 file
!> @param[out] inv Feedback whether to stop the program
!------------------------------------------------------------------------------  
SUBROUTINE check_tensor_2nd_rank_R66_header(header, inv)

CHARACTER(*), INTENT(IN) :: header
LOGICAL, INTENT(OUT) :: inv

CHARACTER(mcl) :: tkn(100)
INTEGER(ik) :: ntkn, ii, jj, kk
CHARACTER(scl) :: tdhdr_string
CHARACTER(3) :: char

char = ""
inv = .FALSE.

CALL parse(TRIM(ADJUSTL(header)), ",", args=tkn, nargs=ntkn)

IF(ntkn /= table_data_hdr) inv = .TRUE.

WRITE(tdhdr_string, '(I12)') table_data_hdr 

CALL wih(inv,"ntkn /= "//TRIM(ADJUSTL(tdhdr_string))) 

!------------------------------------------------------------------------------
! Implementation might look silly. But calculating the indices during runtime, 
! to translate them into string and compare them with an if-function or 
! parsing the strings to numbers for comparison will consume way more time.
!------------------------------------------------------------------------------
! Changes here may require changes in MPI_TYPE_CREATE_STRUCT!
!------------------------------------------------------------------------------
CALL check_base_header(tkn(1:5))

IF(TRIM(ADJUSTL(tkn( 6))) /= "Phys. x lo") inv=.TRUE.; CALL wih(inv,tkn( 6))
IF(TRIM(ADJUSTL(tkn( 7))) /= "Phys. x hi") inv=.TRUE.; CALL wih(inv,tkn( 7))
IF(TRIM(ADJUSTL(tkn( 8))) /= "Phys. y lo") inv=.TRUE.; CALL wih(inv,tkn( 8))
IF(TRIM(ADJUSTL(tkn( 9))) /= "Phys. y hi") inv=.TRUE.; CALL wih(inv,tkn( 9))
IF(TRIM(ADJUSTL(tkn(10))) /= "Phys. z lo") inv=.TRUE.; CALL wih(inv,tkn(10))

IF(TRIM(ADJUSTL(tkn(11))) /= "Phys. z hi") inv=.TRUE.; CALL wih(inv,tkn(11))
IF(TRIM(ADJUSTL(tkn(12))) /= "Opt. Info")  inv=.TRUE.; CALL wih(inv,tkn(12))
IF(TRIM(ADJUSTL(tkn(13))) /= "Opt. Res.")  inv=.TRUE.; CALL wih(inv,tkn(13))
IF(TRIM(ADJUSTL(tkn(14))) /= "Pos. alpha") inv=.TRUE.; CALL wih(inv,tkn(14))
IF(TRIM(ADJUSTL(tkn(15))) /= "Pos. eta")   inv=.TRUE.; CALL wih(inv,tkn(15))
IF(TRIM(ADJUSTL(tkn(16))) /= "Pos. phi")   inv=.TRUE.; CALL wih(inv,tkn(16))
IF(TRIM(ADJUSTL(tkn(17))) /= "Sym. Dev.")  inv=.TRUE.; CALL wih(inv,tkn(17))
IF(TRIM(ADJUSTL(tkn(18))) /= "DA")         inv=.TRUE.; CALL wih(inv,tkn(18))
IF(TRIM(ADJUSTL(tkn(19))) /= "BVTV")       inv=.TRUE.; CALL wih(inv,tkn(19))
IF(TRIM(ADJUSTL(tkn(20))) /= "Gray Dens.") inv=.TRUE.; CALL wih(inv,tkn(20))

IF(TRIM(ADJUSTL(tkn(21))) /= "DoA Zener")  inv=.TRUE.; CALL wih(inv,tkn(21))
IF(TRIM(ADJUSTL(tkn(22))) /= "DoA Gebert") inv=.TRUE.; CALL wih(inv,tkn(22))
IF(TRIM(ADJUSTL(tkn(23))) /= "mps")        inv=.TRUE.; CALL wih(inv,tkn(23))
IF(TRIM(ADJUSTL(tkn(24))) /= "Spec. Norm") inv=.TRUE.; CALL wih(inv,tkn(24))

IF(TRIM(ADJUSTL(tkn(25))) /= "Micro El. Type") inv=.TRUE.; CALL wih(inv,tkn(25))
IF(TRIM(ADJUSTL(tkn(26))) /= "Macro El. Type") inv=.TRUE.; CALL wih(inv,tkn(26))
IF(TRIM(ADJUSTL(tkn(27))) /= "No. Elements")   inv=.TRUE.; CALL wih(inv,tkn(27))
IF(TRIM(ADJUSTL(tkn(28))) /= "No. Nodes")      inv=.TRUE.; CALL wih(inv,tkn(28))
IF(TRIM(ADJUSTL(tkn(29))) /= "t-start")        inv=.TRUE.; CALL wih(inv,tkn(29))
IF(TRIM(ADJUSTL(tkn(30))) /= "t-duration")     inv=.TRUE.; CALL wih(inv,tkn(30))

IF(TRIM(ADJUSTL(tkn(31))) /= "ts_InitoDmn")        inv=.TRUE.; CALL wih(inv,tkn(31))
IF(TRIM(ADJUSTL(tkn(32))) /= "ts_pre-P-preallo")   inv=.TRUE.; CALL wih(inv,tkn(32))
IF(TRIM(ADJUSTL(tkn(33))) /= "ts_post-P-preallo")  inv=.TRUE.; CALL wih(inv,tkn(33))
IF(TRIM(ADJUSTL(tkn(34))) /= "ts_Bef-Mat-as")      inv=.TRUE.; CALL wih(inv,tkn(34))
IF(TRIM(ADJUSTL(tkn(35))) /= "ts_Aft-Mat-as")      inv=.TRUE.; CALL wih(inv,tkn(35))
IF(TRIM(ADJUSTL(tkn(36))) /= "ts_Bef-Solve")       inv=.TRUE.; CALL wih(inv,tkn(36))
IF(TRIM(ADJUSTL(tkn(37))) /= "ts_Aft-Solve")       inv=.TRUE.; CALL wih(inv,tkn(37))
IF(TRIM(ADJUSTL(tkn(38))) /= "mem_InitoDmn")       inv=.TRUE.; CALL wih(inv,tkn(38))
IF(TRIM(ADJUSTL(tkn(39))) /= "mem_pre-P-preallo")  inv=.TRUE.; CALL wih(inv,tkn(39))
IF(TRIM(ADJUSTL(tkn(40))) /= "mem_post-P-preallo") inv=.TRUE.; CALL wih(inv,tkn(40))
IF(TRIM(ADJUSTL(tkn(41))) /= "mem_Bef-Mat-as")     inv=.TRUE.; CALL wih(inv,tkn(41))
IF(TRIM(ADJUSTL(tkn(42))) /= "mem_Aft-Mat-as")     inv=.TRUE.; CALL wih(inv,tkn(42))
IF(TRIM(ADJUSTL(tkn(43))) /= "mem_Bef-Solve")      inv=.TRUE.; CALL wih(inv,tkn(43))
IF(TRIM(ADJUSTL(tkn(44))) /= "mem_Aft-Solve")      inv=.TRUE.; CALL wih(inv,tkn(44))
IF(TRIM(ADJUSTL(tkn(45))) /= "pR_InitoDmn")        inv=.TRUE.; CALL wih(inv,tkn(45))
IF(TRIM(ADJUSTL(tkn(46))) /= "pR_pre-P-preallo")   inv=.TRUE.; CALL wih(inv,tkn(46))
IF(TRIM(ADJUSTL(tkn(47))) /= "pR_post-P-preallo")  inv=.TRUE.; CALL wih(inv,tkn(47))
IF(TRIM(ADJUSTL(tkn(48))) /= "pR_Bef-Mat-as")      inv=.TRUE.; CALL wih(inv,tkn(48))
IF(TRIM(ADJUSTL(tkn(49))) /= "pR_Aft-Mat-as")      inv=.TRUE.; CALL wih(inv,tkn(49))
IF(TRIM(ADJUSTL(tkn(50))) /= "pR_Bef-Solve")       inv=.TRUE.; CALL wih(inv,tkn(50))
IF(TRIM(ADJUSTL(tkn(51))) /= "pR_Aft-Solve")       inv=.TRUE.; CALL wih(inv,tkn(51))
IF(TRIM(ADJUSTL(tkn(52))) /= "size_mpi_dmn")       inv=.TRUE.; CALL wih(inv,tkn(52))
IF(TRIM(ADJUSTL(tkn(53))) /= "worker_main_rank")   inv=.TRUE.; CALL wih(inv,tkn(53))
IF(TRIM(ADJUSTL(tkn(54))) /= "pids_returned")      inv=.TRUE.; CALL wih(inv,tkn(54))

kk=55
DO ii=1,6
    DO jj=1,6
        IF((ii==6) .AND. (jj==6)) CYCLE

        WRITE(char,'(A,2I0)') "S",jj,ii

        IF(TRIM(ADJUSTL(tkn(kk))) /= char) THEN
            inv=.TRUE.
        END IF

        CALL wih(inv,tkn(kk))
        
        kk=kk+1
    END DO
END DO

END SUBROUTINE check_tensor_2nd_rank_R66_header

!------------------------------------------------------------------------------
! SUBROUTINE: check_eff_numm_stiff_header
!------------------------------------------------------------------------------  
!> @author Johannes Gebert, gebert@hlrs.de, HLRS/NUM
!
!> @brief
!> Validate the header of an eff_stiff file
!
!> @param[in] header Header of the tensor_2nd_rank_R66 file
!> @param[out] inv Feedback whether to stop the program
!------------------------------------------------------------------------------  
SUBROUTINE check_eff_numm_stiff_header(header, inv)

    CHARACTER(*), INTENT(IN) :: header
    LOGICAL, INTENT(OUT) :: inv
    
    CHARACTER(mcl) :: tkn(1000)
    INTEGER(ik) :: ntkn, ii, jj, kk
    CHARACTER(scl) :: tdhdr_string
    CHARACTER(3) :: char
    
    char = ""
    inv = .FALSE.
    
    CALL parse(TRIM(ADJUSTL(header)), ",", args=tkn, nargs=ntkn)
    
    IF(ntkn /= table_data_hdr) inv = .TRUE.
    
    WRITE(tdhdr_string, '(I12)') table_data_hdr 
    
    CALL wih(inv,"ntkn /= "//TRIM(ADJUSTL(tdhdr_string))) 
    
    !------------------------------------------------------------------------------
    ! Implementation might look silly. But calculating the indices during runtime, 
    ! to translate them into string and compare them with an if-function or 
    ! parsing the strings to numbers for comparison will consume way more time.
    !------------------------------------------------------------------------------
    ! Changes here may require changes in MPI_TYPE_CREATE_STRUCT!
    !------------------------------------------------------------------------------
    CALL check_base_header(tkn(1:5))
   
    kk=6
    DO ii=1,24
        DO jj=1,24
            WRITE(char,'(2(A,I0))') "n",jj,".",ii
    
            IF(TRIM(ADJUSTL(tkn(kk))) /= char) THEN
                inv=.TRUE.
            END IF
    
            CALL wih(inv,tkn(kk))
            
            kk=kk+1
        END DO
    END DO
    
END SUBROUTINE check_eff_numm_stiff_header

!------------------------------------------------------------------------------
! SUBROUTINE: check_base_header
!------------------------------------------------------------------------------  
!> @author Johannes Gebert, gebert@hlrs.de, HLRS/NUM
!
!> @brief
!> Validate the base header of a tensor file
!
!> @param[in] tkn tokens 1 to 5
!------------------------------------------------------------------------------  
SUBROUTINE check_base_header(tkn)
    
    CHARACTER(*), INTENT(IN) :: tkn(5)
    LOGICAL :: inv

    inv=.FALSE.

    IF(TRIM(ADJUSTL(tkn(1))) /= "Domain")      inv=.TRUE.; CALL wih(inv,tkn(1)) 
    IF(TRIM(ADJUSTL(tkn(2))) /= "Domain Size") inv=.TRUE.; CALL wih(inv,tkn(2))
    IF(TRIM(ADJUSTL(tkn(3))) /= "Section x")   inv=.TRUE.; CALL wih(inv,tkn(3))
    IF(TRIM(ADJUSTL(tkn(4))) /= "Section y")   inv=.TRUE.; CALL wih(inv,tkn(4))
    IF(TRIM(ADJUSTL(tkn(5))) /= "Section z")   inv=.TRUE.; CALL wih(inv,tkn(5))
    
END SUBROUTINE check_base_header


!------------------------------------------------------------------------------
! FUNCTION: write_tensor_2nd_rank_R66_header
!------------------------------------------------------------------------------  
!> @author Johannes Gebert, gebert@hlrs.de, HLRS/NUM
!
!> @brief
!> Write the header of a tensor_2nd_rank_R66 file.
!
!> @return string Resulting string
!------------------------------------------------------------------------------  
FUNCTION write_tensor_2nd_rank_R66_header() RESULT(string)

    CHARACTER(10*mcl) :: string

    INTEGER(ik) :: ii, jj
    CHARACTER(scl) :: chars

    !------------------------------------------------------------------------------
    ! Implementation might look silly. But calculating the indices during runtime, 
    ! to translate them into string and compare them with an if-function or 
    ! parsing the strings to numbers for comparison will consume way more time.
    !------------------------------------------------------------------------------
    ! Changes here may require changes in MPI_TYPE_CREATE_STRUCT!
    !------------------------------------------------------------------------------
    string = ""
    string = write_base_header()

    string = TRIM(string)//" Phys. x lo, "
    string = TRIM(string)//" Phys. x hi, "
    string = TRIM(string)//" Phys. y lo, "
    string = TRIM(string)//" Phys. y hi, "
    string = TRIM(string)//" Phys. z lo, "
    string = TRIM(string)//" Phys. z hi, "
    string = TRIM(string)//" Opt. Info, "
    string = TRIM(string)//" Opt. Res., "
    string = TRIM(string)//" Pos. alpha, "
    string = TRIM(string)//" Pos. eta, "
    string = TRIM(string)//" Pos. phi, "
    string = TRIM(string)//" Sym. Dev., "
    string = TRIM(string)//" DA, "
    string = TRIM(string)//" BVTV, "
    string = TRIM(string)//" Gray Dens., "
    string = TRIM(string)//" DoA Zener, "
    string = TRIM(string)//" DoA Gebert, "
    string = TRIM(string)//" mps, "
    string = TRIM(string)//" Spec. Norm, "
    string = TRIM(string)//" Micro El. Type, "
    string = TRIM(string)//" Macro El. Type, "
    string = TRIM(string)//" No. Elements, "
    string = TRIM(string)//" No. Nodes, "
    string = TRIM(string)//" t-start, "
    string = TRIM(string)//" t-duration, "

    string = TRIM(string)//" ts_InitoDmn, "
    string = TRIM(string)//" ts_pre-P-preallo, "
    string = TRIM(string)//" ts_post-P-preallo, "
    string = TRIM(string)//" ts_Bef-Mat-as, "
    string = TRIM(string)//" ts_Aft-Mat-as, "
    string = TRIM(string)//" ts_Bef-Solve, "
    string = TRIM(string)//" ts_Aft-Solve, "

    string = TRIM(string)//" mem_InitoDmn, "
    string = TRIM(string)//" mem_pre-P-preallo, "
    string = TRIM(string)//" mem_post-P-preallo, "
    string = TRIM(string)//" mem_Bef-Mat-as, "
    string = TRIM(string)//" mem_Aft-Mat-as, "
    string = TRIM(string)//" mem_Bef-Solve, "
    string = TRIM(string)//" mem_Aft-Solve, "

    string = TRIM(string)//" pR_InitoDmn, "
    string = TRIM(string)//" pR_pre-P-preallo, "
    string = TRIM(string)//" pR_post-P-preallo, "
    string = TRIM(string)//" pR_Bef-Mat-as, "
    string = TRIM(string)//" pR_Aft-Mat-as, "
    string = TRIM(string)//" pR_Bef-Solve, "
    string = TRIM(string)//" pR_Aft-Solve, "

    string = TRIM(string)//" size_mpi_dmn, "
    string = TRIM(string)//" worker_main_rank, "
    string = TRIM(string)//" pids_returned, "

    DO ii=1,6
        DO jj=1,6
            chars=""
            WRITE(chars,'(A,2I0,A)') " S",jj,ii,","
            string = TRIM(string)//TRIM(chars)
        END DO
    END DO

    !------------------------------------------------------------------------------
    ! Remove the last comma
    !------------------------------------------------------------------------------
    string = string(1:LEN_TRIM(string)-1)

END FUNCTION write_tensor_2nd_rank_R66_header

!------------------------------------------------------------------------------
! FUNCTION: write_eff_num_stiff_header
!------------------------------------------------------------------------------  
!> @author Johannes Gebert, gebert@hlrs.de, HLRS/NUM
!
!> @brief
!> Write the header of a tensor_2nd_rank_R66 file.
!
!> @return string Resulting string
!------------------------------------------------------------------------------  
FUNCTION write_eff_num_stiff_header() RESULT(string)

    CHARACTER(10*mcl) :: string

    INTEGER(ik) :: ii, jj
    CHARACTER(scl) :: chars
    
    !------------------------------------------------------------------------------
    ! Implementation might look silly. But calculating the indices during runtime, 
    ! to translate them into string and compare them with an if-function or 
    ! parsing the strings to numbers for comparison will consume way more time.
    !------------------------------------------------------------------------------
    ! Changes here may require changes in MPI_TYPE_CREATE_STRUCT!
    !------------------------------------------------------------------------------
    string = ""
    string = write_base_header()

    DO ii=1,24
        DO jj=1,24
            chars=""
            WRITE(chars,'(2(A,I0),A)') " n",ii,".",jj,","
            string = TRIM(string)//TRIM(chars)
        END DO
    END DO

    !------------------------------------------------------------------------------
    ! Remove the last comma
    !------------------------------------------------------------------------------
    string = string(1:LEN_TRIM(string)-1)

END FUNCTION write_eff_num_stiff_header

!------------------------------------------------------------------------------
! FUNCTION: write_base_header
!------------------------------------------------------------------------------  
!> @author Johannes Gebert, gebert@hlrs.de, HLRS/NUM
!
!> @brief
!> Write the base header
!------------------------------------------------------------------------------  
FUNCTION write_base_header() RESULT(string)

    CHARACTER(10*mcl) :: string

    string = ""
    string = TRIM(string)//"Domain, "
    string = TRIM(string)//" Domain Size,"
    string = TRIM(string)//" Section x,"
    string = TRIM(string)//" Section y,"
    string = TRIM(string)//" Section z,"
    
END FUNCTION write_base_header

!------------------------------------------------------------------------------
! SUBROUTINE: parse_tensor_2nd_rank_R66_row
!------------------------------------------------------------------------------  
!> @author Johannes Gebert, gebert@hlrs.de, HLRS/NUM
!
!> @brief
!> Parse the header of a tensor_2nd_rank_R66 file
!
!> @description
!> Function returns a negative first column (domain numer) if the data 
!> within a row does not meet the only valid amount of entries.
!
!> @param[in] row An unparsed row of the tensor_2nd_rank_R66 file
!> @param[out] tensor_of_row The resulting TYPE tensor_2nd_rank_R66 file
!> @param[out] invalid Whether the data is corrupt
!------------------------------------------------------------------------------  
SUBROUTINE parse_tensor_2nd_rank_R66_row(row, tensor_of_row, invalid)

CHARACTER(*), INTENT(IN) :: row
TYPE(domain_data), INTENT(OUT) :: tensor_of_row
LOGICAL, INTENT(OUT) :: invalid

CHARACTER(mcl) :: tokens(100)
INTEGER(ik) :: ntokens, ii, jj, tkn

invalid = .FALSE.

CALL parse(TRIM(ADJUSTL(row)), ",", args=tokens, nargs=ntokens)

IF(ntokens /= table_data_hdr) invalid = .TRUE.

!------------------------------------------------------------------------------
! Required, even if row is invalid. Does not check whether tokens(1) actually
! is an integer/domain number.
!------------------------------------------------------------------------------
IF(.NOT. invalid) THEN   
    CALL parse_base_row(tokens(1:5), tensor_of_row)

    tkn=6
    DO ii=1, 3         ! x lo/hi, y lo/hi, z lo/hi
        DO jj=1, 2     ! 6, 7 ,8, 9, 10, 11
            READ(tokens(tkn), '(F39.2)') tensor_of_row%phy_dmn_bnds(ii,jj)
            tkn=tkn+1
        END DO
    END DO
    
    READ(tokens(12), '(A)')      tensor_of_row%opt_crit
    READ(tokens(13), '(F39.10)') tensor_of_row%opt_res 

    tkn=14
    DO ii=1, 3     ! 14, 15, 16
        READ(tokens(tkn), '(F39.10)') tensor_of_row%pos(ii)
        tkn = tkn + 1_ik
    END DO

    READ(tokens(17), '(F39.10)') tensor_of_row%sym 
    READ(tokens(18), '(F39.10)') tensor_of_row%DA 
    READ(tokens(19), '(F39.10)') tensor_of_row%bvtv 
    READ(tokens(20), '(F39.10)') tensor_of_row%gray_density 
    READ(tokens(21), '(F39.10)') tensor_of_row%doa_zener 
    READ(tokens(22), '(F39.10)') tensor_of_row%doa_gebert 
    READ(tokens(23), '(F39.10)') tensor_of_row%mps
    READ(tokens(24), '(F39.10)') tensor_of_row%spec_norm
    
    tensor_of_row%mi_el_type = tokens(25)
    tensor_of_row%ma_el_type = tokens(26)
    
    READ(tokens(27), '(I20)') tensor_of_row%no_elems
    READ(tokens(28), '(I20)') tensor_of_row%no_nodes

    READ(tokens(29), '(F39.10)') tensor_of_row%t_start
    READ(tokens(30), '(F39.10)') tensor_of_row%t_duration

    tkn=31
    DO ii=1, 24 
        READ(tokens(tkn), '(I20)') tensor_of_row%collected_logs(ii)
        tkn = tkn + 1_ik
    END DO

    tkn = 55_ik
    DO jj=1, 6
        DO ii=1, 6
            READ(tokens(tkn), '(F39.10)') tensor_of_row%mat(ii, jj)
            tkn = tkn + 1_ik
        END DO
    END DO
END IF

END SUBROUTINE parse_tensor_2nd_rank_R66_row

!------------------------------------------------------------------------------
! SUBROUTINE: parse_eff_numm_stiff_row
!------------------------------------------------------------------------------  
!> @author Johannes Gebert, gebert@hlrs.de, HLRS/NUM
!
!> @brief
!> Parse the header of a tensor_2nd_rank_R66 file
!
!> @description
!> Function returns a negative first column (domain numer) if the data 
!> within a row does not meet the only valid amount of entries.
!
!> @param[in] row An unparsed row of the tensor_2nd_rank_R66 file
!> @param[out] tensor_of_row The resulting TYPE tensor_2nd_rank_R66 file
!> @param[out] invalid Whether the data is corrupt
!------------------------------------------------------------------------------  
SUBROUTINE parse_eff_numm_stiff_row(row, tensor_of_row, invalid)

    CHARACTER(*), INTENT(IN) :: row
    TYPE(domain_data), INTENT(OUT) :: tensor_of_row
    LOGICAL, INTENT(OUT) :: invalid
    
    !------------------------------------------------------------------------------
    ! scl used to minimize size of the stack
    !------------------------------------------------------------------------------
    CHARACTER(scl) :: tokens(600)
    INTEGER(ik) :: ntokens, ii, jj, tkn
    
    invalid = .FALSE.
    
    CALL parse(TRIM(ADJUSTL(row)), ",", args=tokens, nargs=ntokens)
    
    IF(ntokens /= table_data_hdr_eff_num) invalid = .TRUE.
    
    !------------------------------------------------------------------------------
    ! Required, even if row is invalid. Does not check whether tokens(1) actually
    ! is an integer/domain number.
    !------------------------------------------------------------------------------
    
    IF(.NOT. invalid) THEN   
        CALL parse_base_row(tokens(1:5), tensor_of_row)
                
        tkn = 6_ik
        DO jj=1, 24
            DO ii=1, 24
                READ(tokens(tkn), '(F39.10)') tensor_of_row%num(ii, jj)
                tkn = tkn + 1_ik
            END DO
        END DO
    END IF
    
END SUBROUTINE parse_eff_numm_stiff_row

!------------------------------------------------------------------------------
! SUBROUTINE: parse_base_row
!------------------------------------------------------------------------------  
!> @author Johannes Gebert, gebert@hlrs.de, HLRS/NUM
!
!> @brief
!> Parse the base row of a tensor file
!
!> @param[in] tokens Whether the data is corrupt
!> @param[out] tensor_of_row The resulting TYPE tensor_2nd_rank_R66 file
!------------------------------------------------------------------------------  
SUBROUTINE parse_base_row(tokens, tensor_of_row)

    CHARACTER(*), INTENT(IN) :: tokens(5)
    TYPE(domain_data), INTENT(OUT) :: tensor_of_row

    INTEGER(ik) :: ii
    
    READ(tokens(1), '(I20)')   tensor_of_row%dmn
    READ(tokens(2), '(F39.2)') tensor_of_row%dmn_size 

    DO ii=1, 3 ! 3, 4 ,5
        READ(tokens(ii+2), '(I20)') tensor_of_row%section(ii)
    END DO
    
END SUBROUTINE parse_base_row

!------------------------------------------------------------------------------
! SUBROUTINE: write_tensor_2nd_rank_R66_row
!------------------------------------------------------------------------------  
!> @author Johannes Gebert, gebert@hlrs.de, HLRS/NUM
!
!> @brief
!> Write a tensor_2nd_rank_R66 into a row of a tensor_2nd_rank_R66 file.
!
!> @param[in] tensor_of_row The resulting TYPE tensor_2nd_rank_R66 file
!> @param[out] outstring Return the assembled string
!------------------------------------------------------------------------------  
SUBROUTINE write_tensor_2nd_rank_R66_row(tensor_of_row, outstring)

TYPE(domain_data), INTENT(IN) :: tensor_of_row
CHARACTER(10*mcl), INTENT(OUT) :: outstring

CHARACTER(10*mcl) :: string

INTEGER(ik) :: ii, jj

string = base_write_eff_num_stiff_row(tensor_of_row)

DO jj=1, 3
    DO ii=1, 2
        CALL csv_append(tensor_of_row%phy_dmn_bnds(jj,ii), string)
    END DO
END DO

CALL csv_append(tensor_of_row%opt_crit, string)
CALL csv_append(tensor_of_row%opt_res, string)

DO ii=1, 3
    CALL csv_append(tensor_of_row%pos(ii), string)
END DO

CALL csv_append(tensor_of_row%sym, string)
CALL csv_append(tensor_of_row%DA, string)
CALL csv_append(tensor_of_row%bvtv, string)
CALL csv_append(tensor_of_row%gray_density, string)
CALL csv_append(tensor_of_row%doa_zener, string)
CALL csv_append(tensor_of_row%doa_gebert, string)
CALL csv_append(tensor_of_row%mps, string)
CALL csv_append(tensor_of_row%spec_norm, string)

CALL csv_append(tensor_of_row%mi_el_type, string)
CALL csv_append(tensor_of_row%ma_el_type, string)
CALL csv_append(tensor_of_row%no_elems, string)
CALL csv_append(tensor_of_row%no_nodes, string)
CALL csv_append(tensor_of_row%t_start, string)
CALL csv_append(tensor_of_row%t_duration, string)

DO ii=1, 24
    CALL csv_append(tensor_of_row%collected_logs(ii), string)
END DO
    
DO jj=1, 6
    DO ii=1, 6
        CALL csv_append(tensor_of_row%mat(ii,jj), string)
    END DO
END DO

!------------------------------------------------------------------------------
! Remove the trailing comma
!------------------------------------------------------------------------------  
outstring = string(1:LEN_TRIM(string)-1)

END SUBROUTINE write_tensor_2nd_rank_R66_row

!------------------------------------------------------------------------------
! SUBROUTINE: csv_append_real
!------------------------------------------------------------------------------  
!> @author Johannes Gebert, gebert@hlrs.de, HLRS/NUM
!
!> @param[in] real Variable to append to a string
!> @param[inout] string The resulting string
!------------------------------------------------------------------------------  
SUBROUTINE csv_append_real(real, string)

    REAL(rk), INTENT(IN) :: real
    CHARACTER(10*mcl), INTENT(INOUT) :: string

    CHARACTER(scl) :: chars

    chars = ""
    WRITE(chars, FREAL) real, ","
    string = TRIM(string)//TRIM(chars)
    
END SUBROUTINE csv_append_real

!------------------------------------------------------------------------------
! SUBROUTINE: csv_append_int
!------------------------------------------------------------------------------  
!> @author Johannes Gebert, gebert@hlrs.de, HLRS/NUM
!
!> @param[in] int Variable to append to a string
!> @param[inout] string The resulting string
!------------------------------------------------------------------------------  
SUBROUTINE csv_append_int(int, string)

    INTEGER(ik), INTENT(IN) :: int
    CHARACTER(10*mcl), INTENT(INOUT) :: string

    CHARACTER(scl) :: chars

    chars = ""
    WRITE(chars, FINT) int, ","
    string = TRIM(string)//TRIM(chars)
    
END SUBROUTINE csv_append_int

!------------------------------------------------------------------------------
! SUBROUTINE: csv_append_chars
!------------------------------------------------------------------------------  
!> @author Johannes Gebert, gebert@hlrs.de, HLRS/NUM
!
!> @param[in] chars Variable to append to a string
!> @param[inout] string The resulting string
!------------------------------------------------------------------------------  
SUBROUTINE csv_append_chars(chars, string)

    CHARACTER(scl), INTENT(IN) :: chars
    CHARACTER(10*mcl), INTENT(INOUT) :: string

    string = TRIM(string)//TRIM(chars)//","
    
END SUBROUTINE csv_append_chars

!------------------------------------------------------------------------------
! SUBROUTINE: write_eff_num_stiff_row
!------------------------------------------------------------------------------  
!> @author Johannes Gebert, gebert@hlrs.de, HLRS/NUM
!
!> @brief
!> Write a tensor_2nd_rank_R66 into a row of a tensor_2nd_rank_R66 file.
!
!> @param[in] tensor_of_row The resulting TYPE tensor_2nd_rank_R66 file
!> @param[out] tensor_of_row The resulting TYPE tensor_2nd_rank_R66 file
!------------------------------------------------------------------------------  
SUBROUTINE write_eff_num_stiff_row(tensor_of_row, string)

    TYPE(domain_data), INTENT(IN) :: tensor_of_row
    CHARACTER(10*mcl), INTENT(OUT) :: string
    INTEGER(ik) :: ii, jj

    string = base_write_eff_num_stiff_row(tensor_of_row)

    DO jj=1, 24
        DO ii=1, 24
            CALL csv_append(tensor_of_row%num(ii,jj), string)
        END DO
    END DO
       
    !------------------------------------------------------------------------------
    ! Remove the trailing comma
    !------------------------------------------------------------------------------  
    string = string(1:LEN_TRIM(string)-1)

END SUBROUTINE write_eff_num_stiff_row

!------------------------------------------------------------------------------
! FUNCTION: base_write_eff_num_stiff_row
!------------------------------------------------------------------------------  
!> @author Johannes Gebert, gebert@hlrs.de, HLRS/NUM
!
!> @brief
!> Write the base of a to file.
!
!> @param[in] fh File handle, the header will be written to
!> @param[in] tensor_of_row The resulting TYPE tensor_2nd_rank_R66 file
!------------------------------------------------------------------------------  
FUNCTION base_write_eff_num_stiff_row(tensor_of_row) RESULT (string)

    TYPE(domain_data), INTENT(IN) :: tensor_of_row

    CHARACTER(10*mcl) :: string
    INTEGER(ik) :: ii
    
    string = ""

    CALL csv_append(tensor_of_row%dmn, string)
    CALL csv_append(tensor_of_row%dmn_size, string)

    DO ii=1, 3
        CALL csv_append(tensor_of_row%section(ii), string)
    END DO
    
END FUNCTION base_write_eff_num_stiff_row

!------------------------------------------------------------------------------
! SUBROUTINE: parse_tensor_2nd_rank_R66
!------------------------------------------------------------------------------  
!> @author Johannes Gebert, gebert@hlrs.de, HLRS/NUM
!
!> @brief
!> Parse a "csv" file with the structure of a tensor_2nd_rank_R66 data type.
!
!> @param[in] fh Input file handle (file existane was checked before).
!> @param[in] filename Filename of the input file
!> @param[in] number_of_lines Number of lines contained in the file
!> @param[out] tensors_in Array of all input tensors
!> @param[out] invalid_entries Amount of invalid entries
!------------------------------------------------------------------------------  
SUBROUTINE parse_tensor_2nd_rank_R66(fh, filename, number_of_lines, tensors_in, invalid_entries)

INTEGER(ik), INTENT(IN) :: fh, number_of_lines
CHARACTER(*), INTENT(IN) :: filename
TYPE(domain_data), DIMENSION(:), INTENT(OUT) :: tensors_in
INTEGER(ik), INTENT(OUT) :: invalid_entries
!------------------------------------------------------------------------------
! Line of csv may be pretty long (>40 floats)
!------------------------------------------------------------------------------
CHARACTER(10_ik*mcl) :: header, line
INTEGER(ik) :: ii
LOGICAL :: abort, invalid

invalid_entries = 0_ik
invalid = .FALSE. 

!------------------------------------------------------------------------------
! Parse the header of the "csv" data
! Error if the header does not indicate 43 entries (determined by TYPE)
!------------------------------------------------------------------------------
READ(fh, '(A)') header
CALL check_tensor_2nd_rank_R66_header(header, abort)

IF(abort) THEN
    mssg = "The input file "//TRIM(ADJUSTL(filename))//&
      &" is not a valid 'tensor_2nd_rank_R66' file."
    CALL print_err_stop(std_out, mssg, 1)
END IF

DO ii = 2_ik, number_of_lines
    READ(fh, '(A)') line

    CALL parse_tensor_2nd_rank_R66_row(line, tensors_in(ii-1_ik), invalid)
    IF(invalid) invalid_entries = invalid_entries + 1_ik 
END DO

END SUBROUTINE parse_tensor_2nd_rank_R66

!------------------------------------------------------------------------------
! SUBROUTINE: write_eff_num_stiff
!------------------------------------------------------------------------------  
!> @author Johannes Gebert, gebert@hlrs.de, HLRS/NUM
!
!> @brief
!> Write a "csv" file with the structure of a tensor_2nd_rank_R66 data type.
!
!> @param[in] fh Input file handle (file existane was checked before).
!> @param[in] number_of_lines Number of lines contained in the file
!> @param[out] tensors_in Array of all input tensors
!------------------------------------------------------------------------------  
SUBROUTINE write_eff_num_stiff(fh, number_of_lines, tensors_in)

INTEGER(ik), INTENT(IN) :: fh, number_of_lines
TYPE(domain_data), DIMENSION(:), INTENT(IN) :: tensors_in

CHARACTER(10*mcl) :: string
INTEGER(ik) :: ii

string = write_eff_num_stiff_header()
WRITE(fh, '(A)') string

!------------------------------------------------------------------------------  
! Written this way (2...number_of_lines and ii-1 as indice) to remember how to  
! deal with the header. Must be kept in mind.
!------------------------------------------------------------------------------  
DO ii = 2_ik, number_of_lines
    CALL write_eff_num_stiff_row(tensors_in(ii-1_ik), string)
END DO

WRITE(fh, '(A)')

END SUBROUTINE write_eff_num_stiff

!------------------------------------------------------------------------------
! SUBROUTINE: write_tensor_2nd_rank_R66
!------------------------------------------------------------------------------  
!> @author Johannes Gebert, gebert@hlrs.de, HLRS/NUM
!
!> @brief
!> Write a "csv" file with the structure of a tensor_2nd_rank_R66 data type.
!
!> @param[in] fh Input file handle (file existane was checked before).
!> @param[in] number_of_lines Number of lines contained in the file
!> @param[out] tensors_in Array of all input tensors
!------------------------------------------------------------------------------  
SUBROUTINE write_tensor_2nd_rank_R66(fh, number_of_lines, tensors_in)

    INTEGER(ik), INTENT(IN) :: fh, number_of_lines
    TYPE(domain_data), DIMENSION(:), INTENT(IN) :: tensors_in

    CHARACTER(10*mcl) :: string
    
    INTEGER(ik) :: ii
    
    string = write_tensor_2nd_rank_R66_header()
    WRITE(fh, '(A)') TRIM(string)

    !------------------------------------------------------------------------------  
    ! Written this way (2...number_of_lines and ii-1 as indice) to remember how to  
    ! deal with the header. Must be kept in mind.
    !------------------------------------------------------------------------------  
    DO ii = 2_ik, number_of_lines
        CALL write_tensor_2nd_rank_R66_row(tensors_in(ii-1_ik), string)
        WRITE(fh, '(A)') TRIM(string)
    END DO
    
END SUBROUTINE write_tensor_2nd_rank_R66

!------------------------------------------------------------------------------
! SUBROUTINE: std_stop
!------------------------------------------------------------------------------  
!> @author Johannes Gebert - HLRS - NUM - gebert@hlrs.de
!
!> @brief
!> Stop program if a subroutine signals an error.
!
!> @param[in] stat Status integer
!> @param[out] abrt Whether to abort the program.
!------------------------------------------------------------------------------
SUBROUTINE std_stop(stat, abrt)

CHARACTER(*), INTENT(INOUT) :: stat
LOGICAL, INTENT(INOUT) :: abrt

IF(stat /= "") THEN
    WRITE(std_out, FMT_ERR) "Error in keyword '"//TRIM(stat)//"'."
    abrt = .TRUE.
    stat = ''
END IF 

END SUBROUTINE std_stop



!------------------------------------------------------------------------------
! SUBROUTINE: plain_file
!---------------------------------------------------------------------------
!> @author
!> Johannes Gebert
!
! DESCRIPTION: 
!> Create a plain file.
!
!> @param[in] fn
!> @param[out] stat
!---------------------------------------------------------------------------
SUBROUTINE plain_file(fn, stat, fh)

IMPLICIT NONE

CHARACTER(*), INTENT(IN) :: fn
INTEGER(ik), INTENT(OUT) :: stat, fh
LOGICAL :: exist

stat = 1
fh = give_new_unit()

INQUIRE(FILE=TRIM(fn), EXIST = exist)

IF (exist) THEN
    stat = -1
    OPEN(UNIT = fh, FILE = TRIM(fn), STATUS = 'OLD', POSITION = 'append', ACTION = 'readwrite')
ELSE
    stat = 0
    OPEN(UNIT = fh, FILE = TRIM(fn), STATUS = 'new', ACTION = 'readwrite')
END IF

END SUBROUTINE plain_file


END MODULE formatted_plain
