!------------------------------------------------------------------------------
! Initialize:
! cnt for regular voxels of input array
! rest_cnt for fractions of the last cnt, which were not taken into account 
!   for the subsequent new_voxel.
!------------------------------------------------------------------------------
rest_cnt(3)=0._rk
cnt(3) = 1_ik
!------------------------------------------------------------------------------
! Related to new dimensions (voxel)
!------------------------------------------------------------------------------
DO kk=1, new_dims(3)

    !------------------------------------------------------------------------------
    ! Related to new dimensions (mm)
    !------------------------------------------------------------------------------
    new_dims_real(3) = kk * target_spcng(3)

    !------------------------------------------------------------------------------
    ! Upper fraction of preceding new_voxel must be added to upcoming new_voxel.
    ! Must be done once and in front of loop.
    !------------------------------------------------------------------------------
    sum_new_vox = 0._rk

    ! DEVELOPMENT -----------------------------------------------------------------
    ! How to possibly get the arithmetic mean of floating point scaling.
    ! Postponed in the absence of compelling necessity.
    !------------------------------------------------------------------------------
    ! Z Direction:
    !------------------------------------------------------------------------------
    sum_new_vox = sum_new_vox + rest_cnt(3) * rry_ik2(cnt(1), cnt(2), cnt(3))

    DO WHILE(cnt(3) <= new_dims_real(3))
        sum_new_vox = sum_new_vox + rry_ik2(cnt(1), cnt(2), cnt(3))
        cnt(3) = cnt(3) + 1_ik
    END DO
    sum_new_vox = sum_new_vox + &
        (new_dims_real(3) - (cnt(3)-1_ik)) * rry_ik2(cnt(1)+1_ik, cnt(2)+1_ik, cnt(3)+1_ik)

    rest_cnt(3) = 1._rk - REAL(cnt(3)-1_ik, KIND=rk)
    !------------------------------------------------------------------------------
    ! to possibly get the arithmetic mean of floating point scaling.
    ! Postponed in the absence of compelling necessity.
    ! DEVELOPMENT -----------------------------------------------------------------

END  DO