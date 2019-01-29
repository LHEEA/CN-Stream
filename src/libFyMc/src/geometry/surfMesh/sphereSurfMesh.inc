!! ------------------------------------------------------------------ !!
!! Procedures : Type sphereSurfMesh
!! ------------------------------------------------------------------ !!
    Subroutine initializeSphereMeshDict(this, dict)
        Implicit None
        !! - Input & Output Variables ------------------------------------------
        class(typSphereSurfMesh), intent(inout) :: this
        Type(typDictionaryPtr)                  :: dict
        !! - Local Variables ---------------------------------------------------
        Real(RP)    :: radius
        Integer     :: nR, nZ
        Real(RP)    :: x0, y0, z0
        Integer     :: nGaussPoint
        Logical     :: writeLog

        radius = dict%getReal("radius")

        nR = dict%getInt("nR")
        nZ = dict%getInt("nZ")

        x0 = dict%getRealOrDefault("center", 0.0_RP, 1)
        y0 = dict%getRealOrDefault("center", 0.0_RP, 2)
        z0 = dict%getRealOrDefault("center", 0.0_RP, 3)

        nGaussPoint = dict%getIntOrDefault("nGauss", 1)
        writeLog = dict%getLogicalOrDefault("writeLog",.TRUE.)

        Call this%initialize(radius, nR, nZ, x0, y0, z0, nGaussPoint, writeLog)

    End Subroutine

    Subroutine initializeSphereMesh(this, radius, nR, nZ, x0, y0, z0, nGaussPoint, writeLog)
        Implicit None
        !! - Input & Output Variables ------------------------------------------
        class(typSphereSurfMesh), intent(inout) :: this
        Real(RP), intent(in) :: radius
        Integer,  intent(in) :: nR, nZ
        Real(RP),optional    :: x0, y0, z0
        Integer, optional    :: nGaussPoint
        Logical, optional    :: writeLog
        !! - Local Variables ---------------------------------------------------
        Real(RP) :: Lx, Ly, Lz
        Integer  :: Nx, Ny
        Type(typParaPipeMesh) :: tmpParaPipeMesh
        integer  :: inode, iPanel
        Real(RP) :: xx, yy, zz, RR
        Real(RP) :: pRx, pRy, pRz
        Real(RP) :: dtheta, dx1, dx2
        Integer  :: nX2

        !! Check Parameters
        if (radius.le.0.0) then
        write(*,*) "    [ERROR] : initializeSphereMesh, radius should be positive"
        write(*,*) "        - Given radius = ", radius
        STOP
        end if

        !! Check Parameters
        if ((nR.le.2).OR.(nZ.le.2)) then
        write(*,*) "    [ERROR] : initializeSphereMesh, nR, NZ > 2"
        write(*,*) "        - nR, nZ = ", nR, nZ
        STOP
        end if

        !! Set Parameters
        this%radius_ = radius
        this%nR_     = nR
        this%nZ_     = nZ

        this%x0_ = 0.0_RP;  this%y0_ = 0.0_RP;  this%z0_ = 0.0_RP
        if (present(x0)) this%x0_ = x0
        if (present(y0)) this%y0_ = y0
        if (present(z0)) this%z0_ = z0

        !! Create paraPipe Surf Mesh to Project (O - Mesh)
        Lx = 1;     Ly = 1;     Lz = 1
        Nx = nR;    Ny = nR;

        !! Original Parallel Pipe Mesh Spacing - x and y
        if (mod(nX-1,2).eq.0) then
            nX2 = ( nX - 1 ) / 2
            dtheta = 0.25_RP * pi / nX2
            dx2 = 0.5_RP * Ly * dtan(dtheta)
        else
            nX2 = nX / 2
            dtheta = 0.25_RP * pi / (nX2 - 0.5_RP)
            dx2 = Ly * dtan(0.5_RP * dtheta)
        endif

        dx1 = 0.5_RP * Lx - 0.5_RP * Ly * dtan(0.25_RP * pi - dtheta)

        pRx = (dx2 / dx1)**(1.0_RP / (nX2 - 1.0_RP))
        pRy = pRx

        !! Original Parallel Pipe Mesh Spacing - z
        if (mod(nZ-1,2).eq.0) then
            nX2 = ( nZ - 1 ) / 2
            dtheta = 0.25_RP * pi / nX2
            dx2 = 0.5_RP * Lz * dtan(dtheta)
        else
            nX2 = nZ / 2
            dtheta = 0.25_RP * pi / (nX2 - 0.5_RP)
            dx2 = Lz * dtan(0.5_RP * dtheta)
        endif

        dx1 = 0.5_RP * Lz - 0.5_RP * Lz * dtan(0.25_RP * pi - dtheta)

        pRz = (dx2 / dx1)**(1.0_RP/ (nX2 - 1.0_RP) )

        !! - Call ParaPipe Mesh
        Call tmpParaPipeMesh%initialize(Lx, Ly, Lz, Nx, Ny, Nz, &
                                        0.0_RP, 0.0_RP, 0.0_RP, &
                                        pRx, pRy, pRz)

        !! - Set Default Mesh
        this = tmpParaPipeMesh

        do iNode = 1, this%nNode
            xx = this%node(iNode)%x()
            yy = this%node(iNode)%y()
            zz = this%node(iNode)%z()
            RR = dsqrt(xx * xx + yy * yy + zz *zz)

            xx = xx * this%radius_ / RR + this%x0_
            yy = yy * this%radius_ / RR + this%y0_
            zz = zz * this%radius_ / RR + this%z0_

            Call this%node(iNode)%move(xx, yy, zz)
        Enddo

        !! - Reset Surface Mesh
        do iPanel = 1, this%nPanel
            Call this%panel(iPanel)%correct(this%node)

            if (present(nGaussPoint)) then
                Call this%panel(iPanel)%correctGaussPoint(nGaussPoint)
            else
                Call this%panel(iPanel)%correctGaussPoint(1)
            end if

        Enddo

        !! Write Input Parameters ----------------------------------------------
        if (present(writeLog)) then
        if (writeLog.EQV..TRUE.) then
        write(*,1001) " "
        write(*,1001) " mkSurfaceMesh : Generate Sphere Surface Mesh"
        write(*,1001) " "
        write(*,1002) "    - Radius                       : ", radius
        write(*,1003) "    - Nr, Nz, (Number of Nodes)    : ", Nr, Nz
        write(*,1002) "    - x0, y0, z0 (Center Position) : ", this%x0_, this%y0_, this%z0_
        write(*,1001) " "
        end if
        end if

        1001 FORMAT(a)
        1002 FORMAT(a, 3(f15.6))
        1003 FORMAT(a, 3(8x,i7))
        !! ---------------------------------------------------------------------

    End Subroutine

!! ------------------------------------------------------------------ !!
!! End Procedures
!! ------------------------------------------------------------------ !!
