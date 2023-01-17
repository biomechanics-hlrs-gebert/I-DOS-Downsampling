INTEGER(ik), INTENT(IN) :: fh
CHARACTER(*), INTENT(IN) :: filename
CHARACTER(*), INTENT(IN), OPTIONAL :: representation
INTEGER(ik), INTENT(IN), OPTIONAL :: displacement

CHARACTER(scl) :: cnvrt

cnvrt = 'NATIVE'
IF(PRESENT(representation)) cnvrt = TRIM(representation)

! Can't determine size of the array. Even if the displacement and the number of bytes 
! per datum are known, the binary blob might not reach the end of the file and therefore 
! basically is of unknown size.

OPEN (UNIT=fh, FILE=TRIM(filename), ACCESS="STREAM", FORM="UNFORMATTED", &
   CONVERT=TRIM(cnvrt), STATUS="UNKNOWN")                                       
READ(UNIT=fh, POS=displacement) array
CLOSE(UNIT=fh)