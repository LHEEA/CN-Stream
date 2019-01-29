!! ------------------------------------------------------------------ !!
!! Procedures : Type typSemiEllipsoidSurfMesh
!! ------------------------------------------------------------------ !!

    Subroutine initializeSemiEllipsoidSurfMeshDict(this, dict)
        Implicit None
        class(typSemiEllipsoidSurfMesh), intent(inout) :: this
        Type(typDictionaryPtr)                :: dict
        Real(RP)    :: rX, rY, rZ
        Integer     :: nX, nY, nZ
        Real(RP)    :: x0, y0, z0
        Integer     :: nLineSub
        Integer     :: nGaussPoint
        Logical     :: writeLog

        rX = dict%getReal("halfAxis", 1)
        rY = dict%getReal("halfAxis", 2)
        rZ = dict%getReal("halfAxis", 3)

        nX = dict%getInt("numberOfMesh", 1)
        nY = dict%getInt("numberOfMesh", 2)
        nZ = dict%getInt("numberOfMesh", 3)

        x0 = dict%getRealOrDefault("center", 0.0_RP, 1)
        y0 = dict%getRealOrDefault("center", 0.0_RP, 2)
        z0 = dict%getRealOrDefault("center", 0.0_RP, 3)

        nLineSub = dict%getIntOrDefault("nSubLine", 1)
        nGaussPoint = dict%getIntOrDefault("nGauss", 1)
        writeLog = dict%getLogicalOrDefault("writeLog",.TRUE.)

        Call this%initialize(rX, rY, rZ, nX, nY, nZ, x0, y0, z0, nLineSub, nGaussPoint, writeLog)

    End Subroutine

    Subroutine initializeSemiEllipsoidSurfMesh(this, rX, rY, rZ, &
                                                     nX, nY, nZ, &
                                                     x0, y0, z0, &
                                                     nLineSub,   &
                                                     nGaussPoint,&
                                                     writeLog)
        Implicit None
        !! - Input & Output Variables ------------------------------------------
        class(typSemiEllipsoidSurfMesh), intent(inout) :: this
        Real(RP), intent(in) :: rX, rY, rZ
        Integer,  intent(in) :: nX, nY, nZ
        Real(RP),optional    :: x0, y0, z0
        Integer, optional    :: nLineSub
        Integer, optional    :: nGaussPoint
        Logical, optional    :: writeLog
        !! - Local Variables ---------------------------------------------------
        Real(RP) :: Lx, Ly, Lz
        Type(typParaPipeMesh) :: tmpParaPipeMesh
        integer  :: inode, iPanel, nX2
        Real(RP) :: xx, yy, zz, theta, psi
        Real(RP) :: Rxx, Ryy, Rzz, R1
        Real(RP),Dimension(3) :: refVec
        Real(RP) :: pRx, pRy, pRz
        Real(RP) :: dx1, dx2, phi, dtheta

        refVec(1) = 0.0_RP
        refVec(2) = 0.0_RP
        refVec(3) = 1.0_RP

        !! Check Parameters
        if ((rX.le.0.0).OR.(rY.le.0.0).OR.(rZ.le.0.0)) then
        write(*,*) "    [ERROR] : initializeEllipsoidSurfMesh, "
        write(*,*) "              half length of principal axes should be positive"
        write(*,*) ""
        write(*,*) "        - Given rX, rY, rZ = ", rX, rY, rZ
        STOP
        end if

        !! Check Parameters
        if ((nX.le.2).OR.(nY.le.2).OR.(nZ.le.2)) then
        write(*,*) "    [ERROR] : initializeEllipsoidSurfMesh, nX, nY, nZ > 2"
        write(*,*) "        - nX, nY, nZ = ", nX, nY, nZ
        STOP
        end if

        !! Set Parameters
        this%rX_ = rX;  this%rY_ = rY;  this%rZ_ = rZ;
        this%nX_ = nX;  this%nY_ = nY;  this%nZ_ = nZ;

        this%x0_ = 0.0_RP;  this%y0_ = 0.0_RP;  this%z0_ = 0.0_RP
        this%nLineSub = 1
        if (present(x0)) this%x0_ = x0
        if (present(y0)) this%y0_ = y0
        if (present(z0)) this%z0_ = z0
        if (present(nLineSub)) this%nLineSub = nLineSub

        !! Create paraPipe Surf Mesh to Project (O - Mesh)
        Lx = rX;     Ly = rY;     Lz = rZ

        !! Original Parallel Pipe Mesh Spacing - x
        phi = datan2(Lx, Ly)
        if (mod(nX-1,2).eq.0) then
            nX2 = ( nX - 1 ) / 2
            dtheta = phi / nX2
            dx2 = 0.5_RP * Ly * dtan(dtheta)
        else
            nX2 = nX / 2
            dtheta = phi / (nX2 - 0.5_RP)
            dx2 = Ly * dtan(0.5_RP * dtheta)
        endif
        dx1 = 0.5_RP * Lx - 0.5_RP * Ly * dtan(phi - dtheta)
        pRx = (dx2 / dx1)**(1.0_RP / (nX2 - 1.0_RP))

        !! Original Parallel Pipe Mesh Spacing - y
        phi = datan2(Ly, Lx)
        if (mod(nY-1,2).eq.0) then
            nX2 = ( nY - 1 ) / 2
            dtheta = phi / nX2
            dx2 = 0.5_RP * Lx * dtan(dtheta)
        else
            nX2 = nY / 2
            dtheta = phi / (nX2 - 0.5_RP)
            dx2 = Lx * dtan(0.5_RP * dtheta)
        endif
        dx1 = 0.5_RP * Ly - 0.5_RP * Lx * dtan(phi - dtheta)
        pRy = (dx2 / dx1)**(1.0_RP / (nX2 - 1.0_RP))

        !! Original Parallel Pipe Mesh Spacing - z
        phi = datan2(Lz, Lx)
        dtheta = phi / ( nZ - 1.0_RP )
        dx2 = 0.5_RP * Lx * dtan(dtheta)
        dx1 = 0.5_RP * Lz - 0.5_RP * Lx * dtan(phi - dtheta)
        pRz = (dx2 / dx1)**(1.0_RP / (nZ - 2.0_RP))

        !! - Call ParaPipe Mesh
        Call tmpParaPipeMesh%initialize(2.0_RP * Lx, 2.0_RP * Ly, 2.0_RP * Lz, &
                                        Nx, Ny, Nz, &
                                        0.0_RP, 0.0_RP, - Lz, &
                                        pRx, pRy, pRz, &
                                        nGaussPoint, &
                                        .TRUE., this%nLineSub)

        !! - Set Default Mesh
        this = tmpParaPipeMesh

        do iNode = 1, this%nNode
            xx = this%node(iNode)%x()
            yy = this%node(iNode)%y()
            zz = this%node(iNode)%z()

            R1 = dsqrt(xx*xx + yy*yy)

            theta = datan2( zz , R1 )
            psi   = datan2( yy , xx )

            xx = Rx * dcos(theta) * dcos(psi) + this%x0_
            yy = Ry * dcos(theta) * dsin(psi) + this%y0_
            zz = Rz * dsin(theta) + this%z0_

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

        !! Projection Line Segment
        do iNode = 1, this%nLineNode
            xx = this%lineNode(iNode)%x()
            yy = this%lineNode(iNode)%y()
            zz = this%lineNode(iNode)%z()

            R1 = dsqrt(xx*xx + yy*yy)

            theta = datan2( zz , R1 )
            psi   = datan2( yy , xx )

            xx = Rx * dcos(theta) * dcos(psi) + this%x0_
            yy = Ry * dcos(theta) * dsin(psi) + this%y0_
            zz = Rz * dsin(theta) + this%z0_

            Call this%lineNode(inode)%move(xx, yy, zz)
        Enddo

        !! - Reset Surface Mesh
        do iPanel = 1, this%nLine
            Call this%linePanel(iPanel)%correct(this%lineNode, refVec)

            if (present(nGaussPoint)) then
                Call this%linePanel(iPanel)%correctGaussPoint(nGaussPoint)
            else
                Call this%linePanel(iPanel)%correctGaussPoint(1)
            end if

        Enddo


        !! Write Input Parameters ----------------------------------------------
        if (present(writeLog)) then
        if (writeLog.EQV..TRUE.) then
        write(*,1001) " "
        write(*,1001) " mkSurfaceMesh : Generate Semi-Ellipsoid Surface Mesh"
        write(*,1001) " "
        write(*,1002) "    - a, b, c (Half length of axes) : ", rX, rY, rZ
        write(*,1003) "    - Nx, Ny, Nz (Number of Nodes)  : ", Nx, Ny, Nz
        write(*,1002) "    - x0, y0, z0 (Center Position)  : ", this%x0_, this%y0_, this%z0_
        write(*,1003) "    - Number of Sub Line Segment    : ", this%nLineSub
        write(*,1001) " "
        end if
        end if

        1001 FORMAT(a)
        1002 FORMAT(a, 3(f15.6))
        1003 FORMAT(a, 3(8x,i7))

    End Subroutine

!! ------------------------------------------------------------------ !!
!! End Procedures
!! ------------------------------------------------------------------ !!
