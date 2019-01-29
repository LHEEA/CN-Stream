!! --------------------------------------------------------------------------
!!
!!                            FyMcPack Example
!!
!!  Example : testDict
!!
!!      Initiaize dictionary with input file and get variables
!!
!!  Authors
!!      YoungMyung Choi, Ecole Centrale de Nantes
!!
!! --------------------------------------------------------------------------
    Program main

        use mfpGlobal
        use mfpGeneral
        use mfpGeometry

    Implicit None
        type(typDictionaryPtr)  :: surfDict
        class(typSurfMesh), allocatable :: surfMesh
        integer  :: iPanel, iLine, iGauss
        real(rp),dimension(3) :: moment
        Real(rp),dimension(3) :: tmpMoment
        real(rp) :: mx, my, mz

        !!... Intialize Dictionary
        Call surfDict%initialize("./", "surfMesh", ".dict")

        !!... Initialize SurfMesh
        Call initializeSurfMesh(surfMesh, surfDict%subDict("surfaceMesh"))

        !!... Test Gauss Point and Weight
        do iPanel = 1, surfMesh%nPanel
        ! do iPanel = 1, 1

            moment = 0.0_RP

            do iGauss = 1, surfMesh%panel(iPanel)%nGauss

                tmpMoment = surfMesh%panel(iPanel)%gWeight(iGauss) &
                          * surfMesh%panel(iPanel)%gPoint(iGauss)%vec()

                moment = moment + tmpMoment

            enddo

            mx = surfMesh%panel(iPanel)%area * surfMesh%panel(iPanel)%center%x()
            my = surfMesh%panel(iPanel)%area * surfMesh%panel(iPanel)%center%y()
            mz = surfMesh%panel(iPanel)%area * surfMesh%panel(iPanel)%center%z()

            write(*,*) moment(1), mx, (mx - moment(1)) / mx
            write(*,*) moment(2), my, (my - moment(2)) / my
            write(*,*) moment(3), mz, (mz - moment(3)) / mz
            write(*,*) " "

        enddo

    End Program
