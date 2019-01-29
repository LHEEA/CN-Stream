

Integer, parameter :: nGaussMax = 5

!! Gauss Point

Real(RP), parameter,dimension(1) :: gaussXi1 = (/ 0.0_RP /)

Real(RP), parameter,dimension(2) :: gaussXi2 = (/ -dsqrt(1.0_RP / 3.0_RP), &
                                                   dsqrt(1.0_RP / 3.0_RP) /)

Real(RP), parameter,dimension(3) :: gaussXi3 = (/ -dsqrt(3.0_RP / 5.0_RP), &
                                                   0.0_RP                , &
                                                   dsqrt(3.0_RP / 5.0_RP) /)

Real(RP), parameter,dimension(4) :: gaussXi4 = (/ &
                                    -dsqrt( ( 3.0_RP + 2.0_RP *dsqrt(1.2_RP) ) / 7.0_RP ), &
                                    -dsqrt( ( 3.0_RP - 2.0_RP *dsqrt(1.2_RP) ) / 7.0_RP ), &
                                     dsqrt( ( 3.0_RP - 2.0_RP *dsqrt(1.2_RP) ) / 7.0_RP ), &
                                     dsqrt( ( 3.0_RP + 2.0_RP *dsqrt(1.2_RP) ) / 7.0_RP ) /)

Real(RP), parameter,dimension(5) :: gaussXi5 = (/ &
                                    -dsqrt( 5.0_RP + 2.0_RP * dsqrt(10.0_RP / 7.0_RP) ) / 3.0_RP ,&
                                    -dsqrt( 5.0_RP - 2.0_RP * dsqrt(10.0_RP / 7.0_RP) ) / 3.0_RP ,&
                                     0.0_RP, &
                                     dsqrt( 5.0_RP - 2.0_RP * dsqrt(10.0_RP / 7.0_RP) ) / 3.0_RP ,&
                                     dsqrt( 5.0_RP + 2.0_RP * dsqrt(10.0_RP / 7.0_RP) ) / 3.0_RP /)

Real(RP), parameter,dimension(1) :: gaussW1 = (/ 2.0_RP /)

Real(RP), parameter,dimension(2) :: gaussW2 = (/ 1.0_RP, 1.0_RP /)

Real(RP), parameter,dimension(3) :: gaussW3 = (/ 5.0_RP / 9.0_RP, &
                                                 8.0_RP / 9.0_RP, &
                                                 5.0_RP / 9.0_RP /)

Real(RP), parameter,dimension(4) :: gaussW4 = (/ 0.5_RP - dsqrt(30.0_RP) / 36.0_RP, &
                                                 0.5_RP + dsqrt(30.0_RP) / 36.0_RP, &
                                                 0.5_RP + dsqrt(30.0_RP) / 36.0_RP, &
                                                 0.5_RP - dsqrt(30.0_RP) / 36.0_RP /)

Real(RP), parameter,dimension(5) :: gaussW5 = (/ (322.0_RP - 13.0_RP * dsqrt(70.0_RP))/900.0_RP, &
                                                 (322.0_RP + 13.0_RP * dsqrt(70.0_RP))/900.0_RP, &
                                                 128.0_RP / 225.0_RP, &
                                                 (322.0_RP + 13.0_RP * dsqrt(70.0_RP))/900.0_RP, &
                                                 (322.0_RP - 13.0_RP * dsqrt(70.0_RP))/900.0_RP /)
