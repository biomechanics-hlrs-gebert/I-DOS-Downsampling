!------------------------------------------------------------------------------
! MODULE: mechanical
!------------------------------------------------------------------------------
!> @author Johannes Gebert - HLRS - NUM - gebert@hlrs.de
!
! DESCRIPTION: 
!> Module containing all recurring definitions of kinds and numbers.
!------------------------------------------------------------------------------
MODULE mechanical

USE global_std

IMPLICIT NONE

!------------------------------------------------------------------------------
! Describe a tensor and its state in respect to the position of the control
! volume. Required to fully trace the origin of a stiffness matrix (tensor).
! 
! It is expected that all other information like domain size, filter options 
! etc. are described by the meta file format!
!------------------------------------------------------------------------------
! Changes here may require changes in MPI_TYPE_CREATE_STRUCT!
!------------------------------------------------------------------------------
TYPE domain_data
   INTEGER(ik)    :: section(3)         = 0  ! Position within the CT image
   INTEGER(ik)    :: dmn                = 0  ! Number of the control volume
   INTEGER(ik)    :: no_elems           = 0  ! Number of elements of the domain
   INTEGER(ik)    :: no_nodes           = 0  ! Number of nodes of the domain
   INTEGER(ik)    :: collected_logs(24) = 0  ! Timestamps during domain computation
   REAL(rk)       :: dmn_size           = 0. ! Size of the control volume
   REAL(rk)       :: t_start            = 0. ! Start of the domain after program start (s)
   REAL(rk)       :: t_duration         = 0. ! Duration of the computation of the domain (s)
   REAL(rk)       :: phy_dmn_bnds(3,2)  = 0. ! Physical domain boundaries (x,y,z - lo,hi)
   REAL(rk)       :: opt_res            = 0. ! Resolution the covo was optimized with
   REAL(rk)       :: pos(3)             = 0. ! Position (deg) of alpha, eta, phi
   REAL(rk)       :: sym                = 0. ! Symmetry deviation (quotient)
   REAL(rk)       :: DA                 = 0. ! Degree of anisotropy - Bone gold standard
   REAL(rk)       :: bvtv               = 0. ! Bone volume/total volume
   REAL(rk)       :: gray_density       = 0. ! Density based on grayscale values.
   REAL(rk)       :: doa_zener          = 0. ! Zener degree of anisotropy
   REAL(rk)       :: doa_gebert         = 0. ! Gebert degree of anisotropy (modified Zener)
   REAL(rk)       :: mps                = 0. ! Mean principal stiffness
   REAL(rk)       :: spec_norm          = 0. ! Gebert degree of anisotropy (modified Zener)
   REAL(rk)       :: num(24,24)         = 0. ! An actual numerical stiffness tensor
   REAL(rk)       :: mat(6,6)           = 0. ! An actual stiffness tensor
   CHARACTER(scl) :: opt_crit           = "" ! Optimization information (e.g. criteria)
   CHARACTER(scl) :: ma_el_type         = "" ! Optimization information (e.g. criteria)
   CHARACTER(scl) :: mi_el_type         = "" ! Optimization information (e.g. criteria)
END TYPE domain_data

!------------------------------------------------------------------------------
! Characterize a material
!------------------------------------------------------------------------------
TYPE materialcard
    REAL(rk) :: E
    REAL(rk) :: nu
    ! For use with effective nummerical stiffness calculations 
    REAL(rk), DIMENSION(3) :: phdsize ! Physical domain/ Macro element size 
    REAL(rk), DIMENSION(3) :: delta   ! Spacing of a given discretized material
END TYPE materialcard

CONTAINS

!------------------------------------------------------------------------------
! FUNCTION: doa_zener
!------------------------------------------------------------------------------  
!> @author Johannes Gebert - HLRS - NUM - gebert@hlrs.de
!
!> @brief
!> Function to calculate the zener ratio, an anisotropy measure for 
!> orthotropic cases. https://en.wikipedia.org/wiki/Zener_ratio
!
!> @param[in] mat Young moduluss     
!> @return doa Degree of Zener-anisotropy
!------------------------------------------------------------------------------  
FUNCTION doa_zener(mat) RESULT (doa)

   REAL(rk), DIMENSION(6,6), INTENT(IN) :: mat
   REAL(rk) :: doa

   doa = 2*mat(4,4)/(mat(1,1)-mat(1,2))

END FUNCTION doa_zener


!------------------------------------------------------------------------------
! FUNCTION: doa_gebert
!------------------------------------------------------------------------------  
!> @author Johannes Gebert - HLRS - NUM - gebert@hlrs.de
!
!> @brief
!> Function to calculate the gebert ratio, an anisotropy measure for 
!> anisotropic cases.
!> FIRST DRAFT
!
!> @param[in] mat Young moduluss     
!> @return doa Degree of Gebert-anisotropy
!------------------------------------------------------------------------------  
FUNCTION doa_gebert(mat) RESULT (doa)

   REAL(rk), DIMENSION(6,6), INTENT(IN) :: mat
   REAL(rk) :: doa

   doa = ((mat(4,4)+mat(5,5)+mat(6,6))/3._rk + &
         (SUM(mat(1:3, 4:6)) + SUM(mat(4, 5:6)) + mat(5,6) / 12._rk)) &
            / (mat(1,1)-mat(1,2)) 

END FUNCTION doa_gebert

!------------------------------------------------------------------------------
! mps
!------------------------------------------------------------------------------  
!> @author Johannes Gebert - HLRS - NUM - gebert@hlrs.de
!
!> @brief
!> Mean principal stiffness.
!
!> @param[in]  matin Input Matrix
!> @param[out] mps
!------------------------------------------------------------------------------  
FUNCTION mps(matin) RESULT(mean_principal_stiffness)

REAL(rk), DIMENSION(6,6) :: matin
REAL(rk) :: mean_principal_stiffness

mean_principal_stiffness = (matin(1,1)+matin(2,2)+matin(3,3))/3.0_rk
END FUNCTION mps



!------------------------------------------------------------------------------
! FUNCTION: lamee_lambda
!------------------------------------------------------------------------------
!> @author Johannes Gebert - HLRS - NUM - gebert@hlrs.de
!
!> @brief
!> Function to return the lamé constant lambda
!
!> @param[in] E Young moduluss     
!> @param[in] v      
!> @return lambda
!------------------------------------------------------------------------------  
FUNCTION lamee_lambda(E, v) RESULT (lambda)
 
   REAL (rk) :: E, v
   REAL (rk), DIMENSION(6,6) :: lambda

   lambda = E*v/((1._rk+v)*(1._rk-2._rk*v))

END FUNCTION lamee_lambda

!------------------------------------------------------------------------------
! FUNCTION: lamee_mu_shear
!------------------------------------------------------------------------------  
!> @author Johannes Gebert - HLRS - NUM - gebert@hlrs.de
!
!> @brief
!> Function to return the lamé constant µ / the shear modulus G
!
!> @param[in] E Young moduluss     
!> @param[in] v      
!> @return G
!------------------------------------------------------------------------------  
FUNCTION lamee_mu_shear(E, v) RESULT (G)

   REAL (rk) :: E, v
   REAL (rk), DIMENSION(6,6) :: G ! (shear modulus) 

   G = E / (2._rk*(1._rk+v))

   ! Just for info
   ! mu = G 

END FUNCTION lamee_mu_shear


!------------------------------------------------------------------------------
! FUNCTION: bulk_modulus
!------------------------------------------------------------------------------  
!> @author Johannes Gebert - HLRS - NUM - gebert@hlrs.de
!
!> @brief
!> Function to return the bulk modulus
!
!> @param[in] E Young moduluss     
!> @param[in] v      
!> @return k
!------------------------------------------------------------------------------  
FUNCTION bulk_modulus(E, v) RESULT (k)

   REAL (rk) :: E, v
   REAL (rk), DIMENSION(6,6) :: k ! (shear modulus) 

   k = E / (3._rk*(1._rk-(2._rk*v)))

END FUNCTION bulk_modulus


!------------------------------------------------------------------------------
! FUNCTION: iso_compliance_voigt
!------------------------------------------------------------------------------  
!> @author Johannes Gebert - HLRS - NUM - gebert@hlrs.de
!
!> @brief
!> Function to quickly generate an isotropic 2nd rank compliance tensor
!
!> @param[in] E Young moduluss     
!> @param[in] v      
!> @return t_iso_inv
!------------------------------------------------------------------------------  
FUNCTION iso_compliance_voigt(E, v) RESULT (t_iso_inv)

   REAL (rk) :: E, v
   REAL (rk), DIMENSION(6,6) :: t_iso_inv

   REAL (rk) :: fctr

   fctr = 1._rk/E

!  sum of symmetric components - therefore: eps_12+eps_21 => 2*eps_12 etc.
   t_iso_inv(:,1)=[ 1._rk,    -v,    -v,  .0_rk,           .0_rk,                  .0_rk    ]
   t_iso_inv(:,2)=[    -v, 1._rk,    -v,  .0_rk,           .0_rk,                  .0_rk    ]
   t_iso_inv(:,3)=[    -v,    -v, 1._rk,  .0_rk,           .0_rk,                  .0_rk    ]
   t_iso_inv(:,4)=[ .0_rk, .0_rk, .0_rk,  2._rk*(1._rk+v), .0_rk,                  .0_rk    ]
   t_iso_inv(:,5)=[ .0_rk, .0_rk, .0_rk,  .0_rk,           2._rk*(1._rk+v),        .0_rk    ]
   t_iso_inv(:,6)=[ .0_rk, .0_rk, .0_rk,  .0_rk,           .0_rk,           2._rk*(1._rk+v) ]

   t_iso_inv = t_iso_inv*fctr

END FUNCTION iso_compliance_voigt


!------------------------------------------------------------------------------
! FUNCTION: iso_compliance_kelvin
!------------------------------------------------------------------------------  
!> @author Johannes Gebert - HLRS - NUM - gebert@hlrs.de
!
!> @brief
!> Function to quickly generate an isotropic 2nd rank compliance tensor
!
!> @param[in] E Young moduluss     
!> @param[in] v      
!> @return t_iso_inv
!------------------------------------------------------------------------------  
FUNCTION iso_compliance_kelvin(E, v) RESULT (t_iso_inv)

   REAL (rk) :: E, v
   REAL (rk), DIMENSION(6,6) :: t_iso_inv

   REAL (rk) :: fctr

   fctr = 1._rk/E

   t_iso_inv(:,1)=(/ 1._rk,    -v,    -v,  .0_rk,   .0_rk,   .0_rk   /)
   t_iso_inv(:,2)=(/    -v, 1._rk,    -v,  .0_rk,   .0_rk,   .0_rk   /)
   t_iso_inv(:,3)=(/    -v,    -v, 1._rk,  .0_rk,   .0_rk,   .0_rk   /)
   t_iso_inv(:,4)=(/ .0_rk, .0_rk, .0_rk,  1._rk+v, .0_rk,   .0_rk   /)
   t_iso_inv(:,5)=(/ .0_rk, .0_rk, .0_rk,  .0_rk,   1._rk+v, .0_rk   /)
   t_iso_inv(:,6)=(/ .0_rk, .0_rk, .0_rk,  .0_rk,   .0_rk,   1._rk+v /)

   t_iso_inv = t_iso_inv*fctr

END FUNCTION iso_compliance_kelvin



!------------------------------------------------------------------------------
! FUNCTION: iso_stiffness_voigt
!------------------------------------------------------------------------------  
!> @author Johannes Gebert - HLRS - NUM - gebert@hlrs.de
!
!> @brief
!> Function to quickly generate an isotropic 2nd rank stiffness tensor
!
!> @param[in] E Young moduluss     
!> @param[in] v      
!> @return t_iso
!------------------------------------------------------------------------------  
FUNCTION iso_stiffness_voigt(E, v) RESULT (t_iso)

   REAL (rk) :: E, v
   REAL (rk), DIMENSION(6,6) :: t_iso

   REAL (rk) :: fctr, fctr_shear
   
   fctr       = E / ((1._rk+v)*(1._rk-(2._rk*v)))
   fctr_shear = 1._rk-(2._rk*v)
   t_iso(:,1)=[ 1._rk-v,  v,     v,       .0_rk, .0_rk, .0_rk ]
   t_iso(:,2)=[    v , 1._rk-v,  v,       .0_rk, .0_rk, .0_rk ]
   t_iso(:,3)=[    v ,     v, 1._rk-v,    .0_rk, .0_rk, .0_rk ]
   t_iso(:,4)=[ .0_rk, .0_rk, .0_rk, fctr_shear/2, .0_rk, .0_rk ]
   t_iso(:,5)=[ .0_rk, .0_rk, .0_rk, .0_rk, fctr_shear/2, .0_rk ]
   t_iso(:,6)=[ .0_rk, .0_rk, .0_rk, .0_rk, .0_rk, fctr_shear/2 ]

   t_iso = t_iso*fctr ! Elementwise operation
END FUNCTION iso_stiffness_voigt


!------------------------------------------------------------------------------
! FUNCTION: iso_stiffness_kelvin
!------------------------------------------------------------------------------  
!> @author Johannes Gebert - HLRS - NUM - gebert@hlrs.de
!
!> @brief
!> Function to quickly generate an isotropic 2nd rank stiffness tensor
!
!> @param[in] E Young moduluss     
!> @param[in] v      
!> @return t_iso
!------------------------------------------------------------------------------  
FUNCTION iso_stiffness_kelvin(E, v) RESULT (t_iso)

   REAL (rk) :: E, v
   REAL (rk), DIMENSION(6,6) :: t_iso

   REAL (rk) ::fctr, fctr_shear
   
   fctr       = E / ((1._rk+v)*(1._rk-(2._rk*v))) 
   fctr_shear = 1._rk-(2._rk*v)

   ! Factor of 2 not required as for normal components: sigma=E* eps
   ! Factor of 2 not required as for shear  components: sigma=E*2eps
   t_iso(:,1)=[ 1._rk-v,  v,     v,       .0_rk, .0_rk, .0_rk ]
   t_iso(:,2)=[    v , 1._rk-v,  v,       .0_rk, .0_rk, .0_rk ]
   t_iso(:,3)=[    v ,     v, 1._rk-v,    .0_rk, .0_rk, .0_rk ]
   t_iso(:,4)=[ .0_rk, .0_rk, .0_rk, fctr_shear, .0_rk, .0_rk ]
   t_iso(:,5)=[ .0_rk, .0_rk, .0_rk, .0_rk, fctr_shear, .0_rk ]
   t_iso(:,6)=[ .0_rk, .0_rk, .0_rk, .0_rk, .0_rk, fctr_shear ]

   t_iso = t_iso*fctr ! Elementwise operation

END FUNCTION iso_stiffness_kelvin


!------------------------------------------------------------------------------
! FUNCTION: gebert_density_voigt
!------------------------------------------------------------------------------  
!> @author Johannes Gebert - HLRS - NUM - gebert@hlrs.de
!
!> @brief
!> Calculates the density of the control volume by its mechanical performance
!> in respect to its monolithical stiffness.
!
!> @param[in] mat Input tensor/matrix     
!> @param[in] E Young modulus of the monolithic material
!> @param[in] v Poissions ratio of the monolithic material
!> @return density Returns the gebert-density
!------------------------------------------------------------------------------  
FUNCTION gebert_density_voigt(mat, E, v) RESULT (density)

   REAL (rk), DIMENSION(6,6) :: mat
   REAL (rk) :: E, v, density

   REAL (rk), DIMENSION(6,6) :: dmat, voigt_mat

   voigt_mat = iso_stiffness_voigt(E, v)

   !------------------------------------------------------------------------------  
   ! Minor diagonals/zero entries of the density matrix dmat are inf.  
   !------------------------------------------------------------------------------    
   dmat = mat / voigt_mat 

   density =((dmat(1,1)+dmat(2,2)+dmat(3,3))/3._rk + &     
             (dmat(4,4)+dmat(5,5)+dmat(6,6))/3._rk + &     
             (dmat(1,2)+dmat(1,3)+dmat(2,3))/3._rk) / 3._rk

   ! density = SUM(mat)/36._rk / E

END FUNCTION gebert_density_voigt

END MODULE mechanical