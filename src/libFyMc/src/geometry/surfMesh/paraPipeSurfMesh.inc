!! ------------------------------------------------------------------ !!
!! Procedures : Type paraPipeSurfMesh
!! ------------------------------------------------------------------ !!
    Subroutine initializeParaPipeMeshDict(this, dict)
        Implicit None
        class(typParaPipeMesh), intent(inout) :: this
        Type(typDictionaryPtr)                :: dict
        Real(RP)    :: Lx, Ly, Lz
        Integer     :: Nx, Ny, Nz
        Real(RP)    :: x0, y0, z0
        Real(RP)    :: rX, rY, rZ
        Logical     :: isOpenTop
        Integer     :: nLineSub
        Integer     :: nGaussPoint
        Logical     :: writeLog

        Lx = dict%getReal("length", 1)
        Ly = dict%getReal("length", 2)
        Lz = dict%getReal("length", 3)

        nX = dict%getInt("numberOfMesh", 1)
        nY = dict%getInt("numberOfMesh", 2)
        nZ = dict%getInt("numberOfMesh", 3)

        rX = dict%getRealOrDefault("meshRatio", 1.0_RP, 1)
        rY = dict%getRealOrDefault("meshRatio", 1.0_RP, 2)
        rZ = dict%getRealOrDefault("meshRatio", 1.0_RP, 3)

        x0 = dict%getRealOrDefault("center", 0.0_RP, 1)
        y0 = dict%getRealOrDefault("center", 0.0_RP, 2)
        z0 = dict%getRealOrDefault("center", 0.0_RP, 3)

        isOpenTop = dict%getLogicalOrDefault("isTopOpen",.FALSE.)
        nLineSub  = dict%getIntOrDefault("nSubLine", 1)
        nGaussPoint = dict%getIntOrDefault("nGauss", 1)
        writeLog  = dict%getLogicalOrDefault("writeLog",.TRUE.)

        Call this%initialize(Lx, Ly, Lz, Nx, Ny, Nz, x0, y0, z0, &
                             rX, rY, rZ, nGaussPoint, isOpenTop, nLineSub, writeLog)


    End Subroutine

    Subroutine initializeParaPipeMesh(this, Lx, Ly, Lz, &
                                            Nx, Ny, Nz, &
                                            x0, y0, z0, &
                                            rX, rY, rZ, &
                                            nGaussPoint,&
                                            isOpenTop,  &
                                            nLineSub,   &
                                            writeLog)
        implicit none
        Real(RP),Dimension(3), parameter :: refVector = [0.0_RP, 0.0_RP, 1.0_RP]
        !! - Input & Output Variables
        class(typParaPipeMesh), intent(inout) :: this
        Real(RP),intent(in) :: Lx, Ly, Lz
        Integer, intent(in) :: Nx, Ny, Nz
        Real(RP),optional   :: x0, y0, z0
        Real(RP),optional   :: rX, rY, rZ
        Logical, optional   :: isOpenTop
        Integer, optional   :: nLineSub
        Integer, optional   :: nGaussPoint
        Logical, optional   :: writeLog
        !! - Local Variables
        Real(RP)            :: xMin, xMax, yMin, yMax, zMin, zMax
        Real(RP)            :: dx, dy, dz, xx, yy, zz, xxx, yyy
        Integer             :: iNode, ix, iy, iz, iPanel

        Integer, Dimension(4) :: nodeLabel
        Integer, Dimension(2) :: lineLabel

        Integer :: nNodeP1, nNodeP2, nNodeP3, nNodeP4, nNodeP5, nNodeP6
        Integer :: nLNodeP1, nLNodeP2, nLNodeP3

        Integer  :: nXp, nYp, nZp, nXh, nYh, nZh, nXLine, nYLine, iLine
        Logical  :: xEven, yEven, zEven
        Real(RP) :: rSumX, rSumY, rSumZ

        Real(RP) :: tmpX, tmpY, tmpZ

        !! - Set & Check Parameters

        if ( (Lx.le.0.0).or.(Ly.le.0.0).or.(Lz.le.0.0)) then
            write(*,*) "    [Error] initializeParaPipeMesh : Lx, Ly, Lz should be positive."
            write(*,*) "    Lx, Ly, Lz : ", Lx, Ly, Lz
            stop
        end if

        if ( (Nx.le.2).or.(Ny.le.2).or.(Nz.le.2)) then
            write(*,*) "    [Error] initializeParaPipeMesh : Nx, Ny, Nz should be larger than 2."
            write(*,*) "    Nx, Ny, Nz : ", Nx, Ny, Nz
            stop
        end if

        this%Lx = Lx
        this%Ly = Ly
        this%Lz = Lz

        this%Nx = Nx
        this%Ny = Ny
        this%Nz = Nz

        this%x0 = 0.0_RP;   this%y0 = 0.0_RP;   this%z0 = 0.0_RP
        this%isOpenTop = .FALSE.
        this%nLineSub  = 1
        this%rX        = 1.0_RP
        this%rY        = 1.0_RP
        this%rZ        = 1.0_RP
        if (present(x0))        this%x0 = x0
        if (present(y0))        this%y0 = y0
        if (present(z0))        this%z0 = z0
        if (present(isOpenTop)) this%isOpenTop = isOpenTop
        if (present(nLineSub))  this%nLineSub = nLineSub
        if (present(rX))        this%rX = rX
        if (present(rY))        this%rY = rY
        if (present(rZ))        this%rZ = rZ

        nXp = nX - 1; nYp = nY - 1; nZp = nZ - 1

        xEven = .FALSE.;    yEven = .FALSE.;    zEven = .FALSE.

        if (mod(nXp, 2).eq.0) xEven = .TRUE.
        if (mod(nYp, 2).eq.0) yEven = .TRUE.
        if (mod(nZp, 2).eq.0) zEven = .TRUE.

        if (xEven) then
            nXh = nXp / 2
        else
            nXh = (nXp - 1) / 2
        endif

        if (xEven) then
            nXh = nXp / 2
        else
            nXh = (nXp - 1) / 2
        endif

        if (yEven) then
            nYh = nYp / 2
        else
            nYh = (nYp - 1) / 2
        endif

        if (zEven) then
            nZh = nZp / 2
        else
            nZh = (nZp - 1) / 2
        endif

        if ( dabs(this%rX-1.0_RP).ge.0.01 ) then
            if (xEven) then
                rSumX = 2.0_RP * (this%rX**nXh-1.0_RP) / (this%rX-1.0_RP)
            else
                rSumX = 2.0_RP * (this%rX**nXh-1.0_RP) / (this%rX-1.0_RP) + this%rX**nXh
            endif
            dx = Lx / rSumX
        else
            this%rX = 1.0_RP
            dx      = Lx / ( Nx - 1.0_RP )
        end if

        if ( dabs(this%rY-1.0_RP).ge.0.01 ) then
            if (yEven) then
                rSumY = 2.0_RP * (this%rY**nYh-1.0_RP) / (this%rY-1.0_RP)
            else
                rSumY = 2.0_RP * (this%rY**nYh-1.0_RP) / (this%rY-1.0_RP) + this%rY**nYh
            endif
            dy = Ly / rSumY
        else
            this%rY = 1.0_RP
            dy      = Ly / ( Ny - 1.0_RP )
        end if

        if ( dabs(this%rZ-1.0_RP).ge.0.01 ) then
            if (this%isOpenTop) then
                rSumZ = (this%rZ**nZp-1.0_RP) / (this%rZ-1.0_RP)
                dz    = Lz / rSumZ
            else
            if (zEven) then
                rSumZ = 2.0_RP * (this%rZ**nZh-1.0_RP) / (this%rZ-1.0_RP)
            else
                rSumZ = 2.0_RP * (this%rZ**nZh-1.0_RP) / (this%rZ-1.0_RP) + this%rZ**nZh
            endif
            dz = Lz / rSumZ
            endif
        else
            this%rZ = 1.0_RP
            dz      = Lz / ( Nz - 1.0_RP )
        end if

        xMin = -Lx / 2.0_RP + this%x0
        xMax =  Lx / 2.0_RP + this%x0

        yMin = -Ly / 2.0_RP + this%y0
        yMax =  Ly / 2.0_RP + this%y0

        zMin = -Lz / 2.0_RP + this%z0
        zMax =  Lz / 2.0_RP + this%z0

        !! - Allocate Nodes and Panels

        this%nNode  = 2 * Nz * (Nx + Ny - 2)  + (Nx - 2) * (Ny - 2) * 2
        this%nPanel = this%nNode - 2

        this%nLineNode = 0
        this%nLine     = 0
        if (this%isOpenTop.eqv..true.) then
            this%nNode     = this%nNode  - (Nx - 2) * (Ny - 2)
            this%nPanel    = this%nPanel - (Nx - 1) * (Ny - 1)
            this%nLine     = 2 * this%nLineSub * ( (Nx - 1) + (Ny - 1) )
            this%nLineNode = 2 * this%nLineSub * ( Nx + Ny - 2 ) + 2
            allocate(this%lineNode(this%nLineNode))
            allocate(this%linePanel(this%nLine))
        end if

        allocate(this%node(this%nNode))
        allocate(this%panel(this%nPanel))

        nNodeP1 = 0; nNodeP2 = 0; nNodeP3 = 0; nNodeP4 = 0; nNodeP5 = 0; nNodeP6 =0

        !! - Construct Surface Mesh
        iNode  = 0
        iPanel = 0

        !! y = -Ly/2 Plane

        !! - Construct Panel
        do iz = 1, Nz
            inode = inode + 1

            if (iz.eq.1) then
                Call this%node(inode)%set( inode, xMin, yMin, zMin )
            else
                if (this%isOpenTop) then
                     zz = dz*this%rZ**(iz - 2)
                else
                    if (iz.le.nZh + 1) then
                        zz = dz * this%rZ**(iz - 2)
                    else
                        zz = dz * this%rZ**(nZ - iz)
                    endif
                endif

                Call this%node(inode)%set( inode, xMin, yMin, this%node(inode-1)%z() + zz )

            endif

            do ix = 1, Nx - 1
                inode = inode + 1

                if (ix.le.nXh) then
                    xx = dx * this%rX**(ix - 1)
                else
                    xx = dx * this%rX**(nX - ix - 1)
                endif

                Call this%node(inode)%set( inode, this%node(inode-1)%x() + xx, &
                                                  this%node(inode-1)%y(), &
                                                  this%node(inode-1)%z() )
            enddo
        enddo
        nNodeP1 = iNode

        !! - Construct Node
        do iz = 1, Nz - 1
            iPanel = iPanel + 1
            nodeLabel(1) = (iz - 1) * Nx + 1
            nodeLabel(2) = iz       * Nx + 1
            nodeLabel(3) = iz       * Nx + 2
            nodeLabel(4) = (iz - 1) * Nx + 2

            Call this%panel(iPanel)%set(4, this%node, nodeLabel)

            do ix = 2, Nx - 1
                iPanel = iPanel + 1
                nodeLabel(1) = this%panel(iPanel - 1 )%nodeLabel(1) + 1
                nodeLabel(2) = this%panel(iPanel - 1 )%nodeLabel(2) + 1
                nodeLabel(3) = this%panel(iPanel - 1 )%nodeLabel(3) + 1
                nodeLabel(4) = this%panel(iPanel - 1 )%nodeLabel(4) + 1
                Call this%panel(iPanel)%set(4, this%node, nodeLabel)
            enddo
        enddo

        !! x = Lx/2 Plane
        do iz = 1, Nz
            inode = inode + 1

            if (iz.eq.1) then
                Call this%node(inode)%set( inode, xMax, yMin + dy, zMin )
            else
                if (this%isOpenTop) then
                     zz = dz*this%rZ**(iz - 2)
                else
                    if (iz.le.nZh + 1) then
                        zz = dz * this%rZ**(iz - 2)
                    else
                        zz = dz * this%rZ**(nZ - iz)
                    endif
                endif

                Call this%node(inode)%set( inode, xMax, yMin + dy, this%node(inode-1)%z() + zz )

            endif

            do iy = 2, Ny - 1
                inode = inode + 1

                if (iy.le.nYh) then
                    yy = dy * this%rY**( iy - 1 )
                else
                    yy = dy * this%rY**( nY - iy - 1)
                endif

                Call this%node(inode)%set( inode, this%node(inode-1)%x(), &
                                                  this%node(inode-1)%y() + yy, &
                                                  this%node(inode-1)%z() )
            enddo

        enddo
        nNodeP2 = iNode

        !! - Construct Node
        do iz = 1, Nz - 1
            iPanel = iPanel + 1
            nodeLabel(1) = iz     * Nx
            nodeLabel(2) = (iz+1) * Nx
            nodeLabel(3) = nNodeP1  + iz       * (Ny-1) + 1
            nodeLabel(4) = nNodeP1  + (iz - 1) * (Ny-1) + 1

            Call this%panel(iPanel)%set(4, this%node, nodeLabel)

            iPanel = iPanel + 1
            nodeLabel(1) = nNodeP1 + (iz-1) * (Ny-1) + 1
            nodeLabel(2) = nNodeP1 + iz     * (Ny-1) + 1
            nodeLabel(3) = nNodeP1 + iz     * (Ny-1) + 2
            nodeLabel(4) = nNodeP1 + (iz-1) * (Ny-1) + 2

            Call this%panel(iPanel)%set(4, this%node, nodeLabel)

            do iy = 3, Ny - 1
                iPanel = iPanel + 1
                nodeLabel(1) = this%panel(iPanel - 1 )%nodeLabel(1) + 1
                nodeLabel(2) = this%panel(iPanel - 1 )%nodeLabel(2) + 1
                nodeLabel(3) = this%panel(iPanel - 1 )%nodeLabel(3) + 1
                nodeLabel(4) = this%panel(iPanel - 1 )%nodeLabel(4) + 1
                Call this%panel(iPanel)%set(4, this%node, nodeLabel)
            enddo
        enddo

        !! y = Ly/2 Plane

        !! - Construct Node
        do iz = 1, Nz
            inode = inode + 1

            if (iz.eq.1) then
                Call this%node(inode)%set( inode, xMax - dx, yMax, zMin )
            else
                if (this%isOpenTop) then
                     zz = dz*this%rZ**(iz - 2)
                else
                    if (iz.le.nZh + 1) then
                        zz = dz * this%rZ**(iz - 2)
                    else
                        zz = dz * this%rZ**(nZ - iz)
                    endif
                endif

                Call this%node(inode)%set( inode, xMax - dx, yMax, this%node(inode-1)%z() + zz )

            endif

            do ix = 2, Nx - 1
                inode = inode + 1

                if (ix.le.nXh) then
                    xx = dx * this%rX**( ix - 1 )
                else
                    xx = dx * this%rX**( nX - ix - 1)
                endif

                Call this%node(inode)%set( inode, this%node(inode-1)%x() - xx, &
                                                  this%node(inode-1)%y(), &
                                                  this%node(inode-1)%z() )

            enddo
        enddo
        nNodeP3 = iNode

        !! - Construct Panel
        do iz = 1, Nz - 1
            iPanel = iPanel + 1
            nodeLabel(1) = nNodeP1 + iz     * (Ny-1)
            nodeLabel(2) = nNodeP1 + (iz+1) * (Ny-1)
            nodeLabel(3) = nNodeP2 + iz     * (Nx-1) + 1
            nodeLabel(4) = nNodeP2 + (iz-1) * (Nx-1) + 1
            Call this%panel(iPanel)%set(4, this%node, nodeLabel)

            iPanel = iPanel + 1
            nodeLabel(1) = nNodeP2 + (iz-1) * (Nx-1) + 1
            nodeLabel(2) = nNodeP2 + iz     * (Nx-1) + 1
            nodeLabel(3) = nNodeP2 + iz     * (Nx-1) + 2
            nodeLabel(4) = nNodeP2 + (iz-1) * (Nx-1) + 2
            Call this%panel(iPanel)%set(4, this%node, nodeLabel)

            do iy = 3, Nx - 1
                iPanel = iPanel + 1
                nodeLabel(1) = this%panel(iPanel - 1 )%nodeLabel(1) + 1
                nodeLabel(2) = this%panel(iPanel - 1 )%nodeLabel(2) + 1
                nodeLabel(3) = this%panel(iPanel - 1 )%nodeLabel(3) + 1
                nodeLabel(4) = this%panel(iPanel - 1 )%nodeLabel(4) + 1
                Call this%panel(iPanel)%set(4, this%node, nodeLabel)
            enddo
        enddo

        !! x = -Lx/2 Plane

        !! - Construct Node
        do iz = 1, Nz
            inode = inode + 1

            if (iz.eq.1) then
                Call this%node(inode)%set( inode, xMin, yMax - dy, zMin )
            else
                if (this%isOpenTop) then
                     zz = dz*this%rZ**(iz - 2)
                else
                    if (iz.le.nZh + 1) then
                        zz = dz * this%rZ**(iz - 2)
                    else
                        zz = dz * this%rZ**(nZ - iz)
                    endif
                endif

                Call this%node(inode)%set( inode, xMin, yMax - dy, this%node(inode-1)%z() + zz )

            endif

            do iy = 2, Ny - 2
                inode = inode + 1

                if (iy.le.nYh) then
                    yy = dy * this%rY**( iy - 1 )
                else
                    yy = dy * this%rY**( nY - iy - 1)
                endif

                Call this%node(inode)%set( inode, this%node(inode-1)%x(), &
                                                  this%node(inode-1)%y() - yy, &
                                                  this%node(inode-1)%z() )

            enddo
        enddo
        nNodeP4 = iNode

        !! - Construct Panel
        do iz = 1, Nz - 1
            if (Ny.eq.2) then
                iPanel   = iPanel + 1
                nodeLabel(1) = nNodeP2 + iz     * (Nx-1)
                nodeLabel(2) = nNodeP2 + (iz+1) * (Nx-1)
                nodeLabel(3) = iz      * (Nx-1)+ 1
                nodeLabel(4) = (iz-1)  * (Nx-1)+ 1
                Call this%panel(iPanel)%set(4, this%node, nodeLabel)

            else if (Ny.eq.3) then
                iPanel   = iPanel + 1
                nodeLabel(1) = nNodeP2 + iz     * (Nx-1)
                nodeLabel(2) = nNodeP2 + (iz+1) * (Nx-1)
                nodeLabel(3) = nNodeP3 + iz     * (Ny-2) + 1
                nodeLabel(4) = nNodeP3 + (iz-1) * (Ny-2) + 1
                Call this%panel(iPanel)%set(4, this%node, nodeLabel)

                iPanel  = iPanel + 1
                nodeLabel(1) = nNodeP3 + (iz-1) * (Ny-2) + 1
                nodeLabel(2) = nNodeP3 + iz     * (Ny-2) + 1
                nodeLabel(3) = iz      * Nx + 1
                nodeLabel(4) = (iz-1)  * Nx + 1
                Call this%panel(iPanel)%set(4, this%node, nodeLabel)
            else
                iPanel   = iPanel + 1
                nodeLabel(1) = nNodeP2 + iz     * (Nx-1)
                nodeLabel(2) = nNodeP2 + (iz+1) * (Nx-1)
                nodeLabel(3) = nNodeP3 + iz     * (Ny-2) + 1
                nodeLabel(4) = nNodeP3 + (iz-1) * (Ny-2) + 1
                Call this%panel(iPanel)%set(4, this%node, nodeLabel)

                iPanel   = iPanel + 1
                nodeLabel(1) = nNodeP3 + (iz-1) * (Ny-2) + 1
                nodeLabel(2) = nNodeP3 + iz     * (Ny-2) + 1
                nodeLabel(3) = nNodeP3 + iz     * (Ny-2) + 2
                nodeLabel(4) = nNodeP3 + (iz-1) * (Ny-2) + 2
                Call this%panel(iPanel)%set(4, this%node, nodeLabel)

                do iy = 3, Ny - 2
                    iPanel   = iPanel + 1
                    nodeLabel(1) = this%panel(iPanel - 1 )%nodeLabel(1) + 1
                    nodeLabel(2) = this%panel(iPanel - 1 )%nodeLabel(2) + 1
                    nodeLabel(3) = this%panel(iPanel - 1 )%nodeLabel(3) + 1
                    nodeLabel(4) = this%panel(iPanel - 1 )%nodeLabel(4) + 1
                    Call this%panel(iPanel)%set(4, this%node, nodeLabel)
                enddo

                iPanel   = iPanel + 1
                nodeLabel(1) = this%panel(iPanel - 1 )%nodeLabel(1) + 1
                nodeLabel(2) = this%panel(iPanel - 1 )%nodeLabel(2) + 1
                nodeLabel(3) = iz     * Nx + 1
                nodeLabel(4) = (iz-1) * Nx + 1

                Call this%panel(iPanel)%set(4, this%node, nodeLabel)
            endif
        enddo

        yy = 0.0_RP
        !! Bottom
        do iy = 1, Ny -2

            if (iy.le.nYh) then
                yy = yy + dy * this%rY**( iy - 1 )
            else
                yy = yy + dy * this%rY**( nY - iy - 1)
            endif

            xx = 0.0_RP
            do ix = 1, Nx -2
                inode = inode + 1

                if (ix.le.nXh) then
                    xx = xx + dx * this%rX**( ix - 1 )
                else
                    xx = xx + dx * this%rX**( nX - ix - 1)
                endif

                Call this%node(inode)%set(iNode, xMin + xx, yMin + yy, zMin)

            enddo
        enddo
        nNodeP5 = inode

        do iy = 1, Ny - 1
            if (iy.eq.1) then

                iPanel   = iPanel + 1
                nodeLabel(1) = 1
                nodeLabel(2) = 2
                nodeLabel(3) = nNodeP4 + 1
                nodeLabel(4) = nNodeP3 + (Ny-1) - iy

                Call this%panel(iPanel)%set(4, this%node, nodeLabel)

                do ix = 2, Nx - 2
                    iPanel   = iPanel + 1
                    nodeLabel(1) = this%panel(iPanel-1)%nodeLabel(1) + 1
                    nodeLabel(2) = this%panel(iPanel-1)%nodeLabel(2) + 1
                    nodeLabel(3) = this%panel(iPanel-1)%nodeLabel(3) + 1
                    nodeLabel(4) = nodeLabel(3) - 1

                    Call this%panel(iPanel)%set(4, this%node, nodeLabel)
                end do

                iPanel   = iPanel + 1
                nodeLabel(1) = this%panel(iPanel-1)%nodeLabel(1) + 1
                nodeLabel(2) = this%panel(iPanel-1)%nodeLabel(2) + 1
                nodeLabel(3) = nNodeP1 + iy
                nodeLabel(4) = this%panel(iPanel-1)%nodeLabel(3)

                Call this%panel(iPanel)%set(4, this%node, nodeLabel)

            else if (iy.eq.Ny-1) then

                iPanel   = iPanel + 1
                nodeLabel(1) = this%panel(iPanel-(Nx-1))%nodeLabel(4)
                nodeLabel(2) = this%panel(iPanel-(Nx-1))%nodeLabel(3)
                nodeLabel(3) = nNodeP2 + (Nx-2)
                nodeLabel(4) = nNodeP2 + (Nx-1)

                Call this%panel(iPanel)%set(4, this%node, nodeLabel)

                do ix = 2, Nx - 2
                    iPanel   = iPanel + 1
                    nodeLabel(1) = this%panel(iPanel-1)%nodeLabel(2)
                    nodeLabel(2) = this%panel(iPanel-1)%nodeLabel(2) + 1
                    nodeLabel(3) = this%panel(iPanel-1)%nodeLabel(3) - 1
                    nodeLabel(4) = this%panel(iPanel-1)%nodeLabel(3)
                    Call this%panel(iPanel)%set(4, this%node, nodeLabel)
                end do

                iPanel   = iPanel + 1
                nodeLabel(1) = this%panel(iPanel-1)%nodeLabel(2)
                nodeLabel(2) = nNodeP1 + Ny - 2
                nodeLabel(3) = nNodeP1 + Ny - 1
                nodeLabel(4) = this%panel(iPanel-1)%nodeLabel(3)

                Call this%panel(iPanel)%set(4, this%node, nodeLabel)

            else

                iPanel   = iPanel + 1
                nodeLabel(1) = this%panel(iPanel-(Nx-1))%nodeLabel(4)
                nodeLabel(2) = this%panel(iPanel-(Nx-1))%nodeLabel(3)
                nodeLabel(3) = nNodeP4 + (iy-1)*(Nx-2) + 1
                nodeLabel(4) = nNodeP3 + (Ny-1) - iy

                Call this%panel(iPanel)%set(4, this%node, nodeLabel)

                do ix = 2, Nx - 2
                    iPanel   = iPanel + 1
                    nodeLabel(1) = this%panel(iPanel-1)%nodeLabel(2)
                    nodeLabel(2) = this%panel(iPanel-1)%nodeLabel(2) + 1
                    nodeLabel(3) = this%panel(iPanel-1)%nodeLabel(3) + 1
                    nodeLabel(4) = this%panel(iPanel-1)%nodeLabel(3)

                    Call this%panel(iPanel)%set(4, this%node, nodeLabel)
                end do

                iPanel   = iPanel + 1
                nodeLabel(1) = this%panel(iPanel-1)%nodeLabel(2)
                nodeLabel(2) = nNodeP1 + iy - 1
                nodeLabel(3) = nNodeP1 + iy
                nodeLabel(4) = this%panel(iPanel-1)%nodeLabel(3)

                Call this%panel(iPanel)%set(4, this%node, nodeLabel)

            endif
        end do

        !! Top (if top is closed)
        if (this%isOpenTop.EQV..FALSE.) then

        !! Generate Surface Mesh
        yy = 0.0_RP
        do iy = 1, Ny - 2

            if (iy.le.nYh) then
                yy = yy + dy * this%rY**( iy - 1 )
            else
                yy = yy + dy * this%rY**( nY - iy - 1)
            endif

            xx = 0.0_RP
            do ix = 1, Nx - 2
                inode = inode + 1

                if (ix.le.nXh) then
                    xx = xx + dx * this%rX**( ix - 1 )
                else
                    xx = xx + dx * this%rX**( nX - ix - 1)
                endif

                Call this%node(inode)%set(iNode, xMin + xx, yMin + yy, zMax)
            enddo
        enddo
        nNodeP6 = inode

        do iy = 1, Ny-1
            if (iy.eq.1) then

                iPanel   = iPanel + 1
                nodeLabel(1) = (Nz-1)*Nx + 1
                nodeLabel(2) = nNodeP4
                nodeLabel(3) = nNodeP5 + 1
                nodeLabel(4) = nodeLabel(1) + 1

                Call this%panel(iPanel)%set(4, this%node, nodeLabel)

                do ix = 2, Nx - 2
                    iPanel   = iPanel + 1
                    nodeLabel(1) = this%panel(iPanel-1)%nodeLabel(1) + 1
                    nodeLabel(2) = this%panel(iPanel-1)%nodeLabel(3)
                    nodeLabel(3) = this%panel(iPanel-1)%nodeLabel(3) + 1
                    nodeLabel(4) = this%panel(iPanel-1)%nodeLabel(4) + 1

                    Call this%panel(iPanel)%set(4, this%node, nodeLabel)
                end do

                iPanel   = iPanel + 1
                nodeLabel(1) = this%panel(iPanel-1)%nodeLabel(1) + 1
                nodeLabel(2) = this%panel(iPanel-1)%nodeLabel(3)
                nodeLabel(3) = nNodeP2 - (Ny - 2)
                nodeLabel(4) = nNodeP1

                Call this%panel(iPanel)%set(4, this%node, nodeLabel)

            else if (iy.eq.Ny-1) then

                iPanel   = iPanel + 1
                nodeLabel(1) = this%panel(iPanel-(Nx-1))%nodeLabel(2)
                nodeLabel(2) = nNodeP3
                nodeLabel(3) = nNodeP3 - 1
                nodeLabel(4) = this%panel(iPanel-(Nx-1))%nodeLabel(3)

                Call this%panel(iPanel)%set(4, this%node, nodeLabel)

                do ix = 2, Nx - 2
                    iPanel   = iPanel + 1
                    nodeLabel(1) = this%panel(iPanel-1)%nodeLabel(4)
                    nodeLabel(2) = this%panel(iPanel-1)%nodeLabel(3)
                    nodeLabel(3) = nNodeP3 - ix
                    nodeLabel(4) = nodeLabel(1) + 1

                    Call this%panel(iPanel)%set(4, this%node, nodeLabel)
                end do

                iPanel   = iPanel + 1
                nodeLabel(1) = this%panel(iPanel-1)%nodeLabel(4)
                nodeLabel(2) = this%panel(iPanel-1)%nodeLabel(3)
                nodeLabel(3) = nNodeP2
                nodeLabel(4) = nNodeP2 - 1

                Call this%panel(iPanel)%set(4, this%node, nodeLabel)

            else

                iPanel   = iPanel + 1
                nodeLabel(1) = nNodeP4 + 2 - iy
                nodeLabel(2) = nNodeP4 + 1 - iy
                nodeLabel(3) = nNodeP5 + (Nx-2)*(iy-1) + 1
                nodeLabel(4) = nNodeP5 + (Nx-2)*(iy-2) + 1

                Call this%panel(iPanel)%set(4, this%node, nodeLabel)

                do ix = 2, Nx - 2
                    iPanel   = iPanel + 1
                    nodeLabel(1) = this%panel(iPanel-1)%nodeLabel(4)
                    nodeLabel(2) = this%panel(iPanel-1)%nodeLabel(3)
                    nodeLabel(3) = this%panel(iPanel-1)%nodeLabel(3) + 1
                    nodeLabel(4) = this%panel(iPanel-1)%nodeLabel(4) + 1

                    Call this%panel(iPanel)%set(4, this%node, nodeLabel)
                end do

                iPanel   = iPanel + 1
                nodeLabel(1) = this%panel(iPanel-1)%nodeLabel(4)
                nodeLabel(2) = this%panel(iPanel-1)%nodeLabel(3)
                nodeLabel(3) = nNodeP2 - (Ny - 2) + (iy-1)
                nodeLabel(4) = nNodeP2 - (Ny - 2) + (iy-2)

                Call this%panel(iPanel)%set(4, this%node, nodeLabel)

            end if
        Enddo

        else
        !! Generate Line Mesh

        nXLine = nLineSub * (Nx - 1)
        nYLine = nLineSub * (Ny - 1)

        !! y = -Ly / 2
        iNode  = 0
        iPanel = 0

        do ix = 1, Nx - 1

            if (ix.eq.1) then
                iNode = iNode + 1
                Call this%lineNode(iNode)%set(iNode, xMin , yMin, zMax)
            end if

            if (ix.le.nXh) then
                xx = dx * this%rX**( ix - 1 )
            else
                xx = dx * this%rX**( nX - ix - 1 )
            endif

            xxx = xx / this%nLineSub

            do iline = 1, this%nLineSub
                iNode = iNode + 1
                Call this%lineNode(iNode)%set(iNode, this%lineNode(iNode-1)%x() + xxx , yMin, zMax)
            enddo

        enddo

        nLNodeP1 = iNode

        do ix = 1, nXLine
            iPanel   = iPanel + 1
            lineLabel(1) = ix
            lineLabel(2) = ix + 1
            Call this%linePanel(iPanel)%set(2, this%lineNode, lineLabel, refVector)
        enddo

        !! x = Lx / 2
        do iy = 1, Ny - 1

            if (iy.le.nYh) then
                yy = dy * this%rY**( iy - 1 )
            else
                yy = dy * this%rY**( nY - iy - 1 )
            endif

            yyy = yy / this%nLineSub

            do iline = 1, this%nLineSub
                iNode = iNode + 1
                Call this%lineNode(iNode)%set(iNode, xMax , this%lineNode(iNode-1)%y() + yyy, zMax)
            enddo

        enddo
         nLNodeP2 = iNode

        do iy = 1, nYLine
            iPanel   = iPanel + 1
            lineLabel(1) = nLNodeP1 + iy - 1
            lineLabel(2) = nLNodeP1 + iy
            Call this%linePanel(iPanel)%set(2, this%lineNode, lineLabel, refVector)
        enddo

        !! y = Ly / 2
        do ix = 1, nX - 1

            if (ix.le.nXh) then
                xx = dx * this%rX**( ix - 1 )
            else
                xx = dx * this%rX**( nX - ix - 1 )
            endif

            xxx = xx / this%nLineSub

            do iline = 1, this%nLineSub
                iNode = iNode + 1
                Call this%lineNode(iNode)%set(iNode, this%lineNode(iNode-1)%x() - xxx , yMax, zMax)
            enddo

        enddo
        nLNodeP3 = iNode

        do ix = 1, nXLine
            iPanel   = iPanel + 1
            lineLabel(1) = nLNodeP2 + ix - 1
            lineLabel(2) = nLNodeP2 + ix
            Call this%linePanel(iPanel)%set(2, this%lineNode, lineLabel, refVector)
        enddo

        !! x = -Lx/2
        do iy = 1, nY - 1

            if (iy.le.nYh) then
                yy = dy * this%rY**( iy - 1 )
            else
                yy = dy * this%rY**( nY - iy - 1 )
            endif

            yyy = yy / this%nLineSub

            do iline = 1, this%nLineSub
                iNode = iNode + 1
                Call this%lineNode(iNode)%set(iNode, xMin, this%lineNode(iNode-1)%y() - yyy, zMax)
            enddo

        enddo

        do iy = 1, nYLine
            iPanel   = iPanel + 1
            lineLabel(1) = nLNodeP3 + iy - 1
            if (iy.eq. this%nLineSub * (Ny-1)) then
                lineLabel(2) = 1
            else
                lineLabel(2) = nLNodeP3 + iy
            endif
            Call this%linePanel(iPanel)%set(2, this%lineNode, lineLabel, refVector)

        enddo

        this%nLine = iPanel
        this%nLineNode = iNode

        end if

        do iPanel = 1, this%nPanel
            if (present(nGaussPoint)) then
                Call this%panel(iPanel)%correctGaussPoint(nGaussPoint)
            else
                Call this%panel(iPanel)%correctGaussPoint(1)
            end if
        end do

        do iPanel = 1, this%nLine
            if (present(nGaussPoint)) then
                Call this%linePanel(iPanel)%correctGaussPoint(nGaussPoint)
            else
                Call this%linePanel(iPanel)%correctGaussPoint(1)
            end if
        end do

        !! Write Input Parameters ----------------------------------------------
        if (present(writeLog)) then
        if (writeLog.EQV..TRUE.) then
        write(*,1001) " "
        write(*,1001) " mkSurfaceMesh : Generate Parallel Pipe Surface Mesh"
        write(*,1001) " "
        write(*,1002) "    - Lx, Ly, Lz (Length, Width, Height) : ", Lx, Ly, Lz
        write(*,1003) "    - Nx, Ny, Nz (Number of Nodes)       : ", Nx, Ny, Nz
        write(*,1002) "    - x0, y0, z0 (Center Position)       : ", this%x0, this%y0, this%z0
        write(*,1002) "    - rX, rY, rZ (Mesh Ratio)            : ", this%rX, this%rY, this%rZ
        if (this%isOpenTop) then
        write(*,1001) "    - Top is Opened. "
        write(*,1003) "    - Number of Sub Line Segment         : ", this%nLineSub
        else
        write(*,1001) "    - Top is closed. "
        end if
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
