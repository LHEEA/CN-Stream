type  :: RF_type
    integer :: dimen          ! dimension
    integer :: current
    integer :: lorT           ! input wavelength (=1) or period (=0)

    real(RP) :: g             ! gravity acceleration
    real(RP) :: hdepth        ! depth
    real(RP) :: H             ! wave height
    real(RP) :: k             ! wave number
    real(RP) :: lambda        ! wavelength
    real(RP) :: T             ! period
    real(RP) :: U             !
    real(RP) :: C

    real(RP), dimension (:), allocatable :: a
    real(RP), dimension (:), allocatable :: b
    real(RP), dimension (:), allocatable :: eta
    real(RP), dimension (:), allocatable :: slope

    real(RP) :: R
    real(RP) :: Q
    real(RP) :: C_E, C_S
end type RF_type

type :: Option_type
    integer  :: n_H
    integer  :: err_type
    real(RP) :: eps_err
    real(RP) :: err_max
    real(RP) :: eps_inc
    real(RP) :: eps_N1
    integer  :: itermax
    integer  :: increment_type  ! 0 linear ; 1 exponentiel
    integer  :: N1              ! number of modes for stream function
    integer  :: N2
    integer  :: N1_eff
    integer  :: N2_eff
    integer  :: modes
    integer  :: printonscreen   ! 1 yes ; 0 no
    integer  :: writeoutput   ! 1 yes ; 0 no
    type(typDictionaryPtr) :: optionDict
end type Option_type
