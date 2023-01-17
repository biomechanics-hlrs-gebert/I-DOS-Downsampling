
INTEGER(ik), INTENT(IN) :: threshold
INTEGER(ik) :: ii, jj, kk, shp(3)

shp = SHAPE(array)

DO kk = 1, shp(3)
DO jj = 1, shp(2)
DO ii = 1, shp(1)
    IF (array(ii,jj,kk) >= threshold) THEN
        array(ii,jj,kk) = 1
    ELSE
        array(ii,jj,kk) = 0
    END IF
END DO
END DO
END DO