type :: Output_type
    real(RP) :: eta
    real(RP) :: pressure
    real(RP) :: Vx, Vy, Vz

    real(RP) :: detadt, detadx,detady
    real(RP) :: dVxdx, dVxdy, dVxdz
    real(RP) :: dVydx, dVydy, dVydz
    real(RP) :: dVzdx, dVzdy, dVzdz
end type Output_type