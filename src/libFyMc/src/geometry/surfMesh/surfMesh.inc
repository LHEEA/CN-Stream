!! ------------------------------------------------------------------ !!
!! Procedures : Type surfMesh
!! ------------------------------------------------------------------ !!

Subroutine initializeSurfMesh(surfMesh, dict)
    Implicit None
    class(typSurfMesh), allocatable :: surfMesh
    Type(typDictionaryPtr)          :: dict
    !!... Local Variables
    Type(typParaPipeMesh)           :: paraPipeMesh
    Type(typSphereSurfMesh)         :: sphereMesh
    Type(typHemiSphereSurfMesh)     :: hSphereMesh
    Type(typEllipsoidSurfMesh)      :: ellipsoidMesh
    Type(typSemiEllipsoidSurfMesh)  :: semiEllipsoidMesh
    Type(typCircularCylinderShellSurfMesh)  :: circularCylinderShell

    Character(len=CHAR_LEN) :: surfaceType
    Logical :: isWriteVTK

    if (allocated(surfMesh)) deallocate(surfMesh)

    surfaceType = dict%getChar("type")

    !!... ParaPipe Mesh
    if (isStringEqual(surfaceType,"paraPipe")) then

        !! Initialize with dictionary
        Call paraPipeMesh%initialize(dict%subDict("paraPipe"))

        !! Surface Mesh Pointer (Up-casting)
        allocate(surfMesh, source = paraPipeMesh)

    !!... Sphere Mesh
    else if (isStringEqual(surfaceType,"Sphere")) then

        !! Initialize with dictionary
        Call sphereMesh%initialize(dict%subDict("sphere"))

        !! Surface Mesh Pointer (Up-casting)
        allocate(surfMesh, source = sphereMesh)

    !!... HemiSphere Mesh
    else if (isStringEqual(surfaceType,"hemiSphere")) then

        !! Initialize with dictionary
        Call hSphereMesh%initialize(dict%subDict("hemiSphere"))

        !! Surface Mesh Pointer (Up-casting)
        allocate(surfMesh, source = hSphereMesh)

    !!... Ellipsoid Mesh
    else if (isStringEqual(surfaceType,"Ellipsoid")) then

        !! Initialize with dictionary
        Call ellipsoidMesh%initialize(dict%subDict("ellipsoid"))

        !! Surface Mesh Pointer (Up-casting)
        allocate(surfMesh, source = ellipsoidMesh)

    !!... Semi - Ellipsoid Mesh
    else if (isStringEqual(surfaceType,"SemiEllipsoid")) then

        !! Initialize with dictionary
        Call semiEllipsoidMesh%initialize(dict%subDict("semiEllipsoid"))

        !! Surface Mesh Pointer (Up-casting)
        allocate(surfMesh, source = semiEllipsoidMesh)

    !!... Semi - Ellipsoid Mesh
    else if (isStringEqual(surfaceType,"circularCylinderShell")) then

        !! Initialize with dictionary
        Call circularCylinderShell%initialize(dict%subDict("circularCylinderShell"))

        !! Surface Mesh Pointer (Up-casting)
        allocate(surfMesh, source = circularCylinderShell)

    else
        write(*,*) " "
        write(*,*) "    [ERROR] initializeSurfMesh(this, dict)."
        write(*,*) "        no Surface Type is selected."
        write(*,*) "    given type : ", trim(surfaceType)
        write(*,*) " "
        stop
    end if

    !!... Write VTk
    if (allocated(surfMesh)) then
        isWriteVTK = dict%getLogicalOrDefault("writeVTK",.TRUE.)
        Call SYSTEM('mkdir -p surfaceMeshVTK')
        Call surfMesh%writeVTK("surfaceMeshVTK/"//trim(surfaceType))
    end if

End Subroutine

Subroutine writeVTK(this, filePath)
    Implicit None
    class(typSurfMesh), intent(inout) :: this
    Character(len=*),intent(in) :: filePath
    Character(len=CHAR_LEN) :: surfMeshPath, lineMeshPath
    Character(len=CHAR_LEN) :: surfMeshGaussPath, lineMeshGaussPath

    surfMeshPath        = trim(filePath)//".vtk"
    surfMeshGaussPath   = trim(filePath)//"_gauss.vtk"
    lineMeshPath        = trim(filePath)//"_line.vtk"
    lineMeshGaussPath   = trim(filePath)//"_line_gauss.vtk"

    Call writeVTKSurfMesh(this, surfMeshPath)

    Call writeVTKSurfGaussPoint(this, surfMeshGaussPath)

    if (this%isOpenTop) then
        Call writeVTKLineMesh(this, lineMeshPath)
        Call writeVTKLineMeshGaussPoint(this, lineMeshGaussPath)
    end if

End Subroutine

Subroutine writeVTKSurfMesh(this, filePath)
    Implicit None
    class(typSurfMesh), intent(inout) :: this
    Character(len=*)           :: filePath
    integer :: ioUnit
    integer :: i,j
    Integer :: nConnect

    if ((this%nNode.ge.1).and.(this%nPanel.ge.1)) then
        ioUnit = FIO_UNIT + 1

        Open(unit   = ioUnit,   &
             file   = trim(filePath), &
             status = "replace")

        write(ioUnit,1001) "# vtk DataFile Version 3.0"
        write(ioUnit,1001) "vtk output"
        write(ioUnit,1001) "ASCII"
        write(ioUnit,1001) "DATASET UNSTRUCTURED_GRID"

        write(ioUnit,1002) "POINTS ", this%nNode, " float"
        do i = 1, this%nNode
        write(ioUnit,1003) this%node(i)%x(), this%node(i)%y(), this%node(i)%z()
        enddo

        nConnect = 0
        do i =1, this%nPanel
            nConnect = nConnect + this%panel(i)%nNode() + 1
        enddo

        write(ioUnit,1004) "CELLS ", this%nPanel, nConnect
        do i = 1, this%nPanel
        write(ioUnit,1005) this%panel(i)%nNode(), &
                          (this%panel(i)%nodeLabel(j) - 1, j = 1,this%panel(i)%nNode())
        enddo

        write(ioUnit,1002) "CELL_TYPES ", this%nPanel
        do i = 1, this%nPanel
        write(ioUnit,1006) this%panel(i)%vtkCellType()
        enddo

        write(ioUnit,1002) "CELL_DATA ", this%nPanel
        write(ioUnit,1001) "SCALARS Panel_Area float"
        write(ioUnit,1001) "LOOKUP_TABLE default"
        do i = 1, this%nPanel
        write(ioUnit,1003) this%panel(i)%area
        enddo
        write(ioUnit,1001) "NORMALS Panel_Normal float"
        do i = 1, this%nPanel
        write(ioUnit,1003) this%panel(i)%normal%x(), this%panel(i)%normal%y(), this%panel(i)%normal%z()
        enddo

        Close(ioUnit)
    else
        write(*,*) " "
        write(*,*) "    [WARNING] writeVTKSurfMesh(typSurfMesh, path)"
        write(*,*) "        Number of Node and Panel should be larger than 1."
        write(*,*) " "
        write(*,*) "        - Number of Node  : ", this%nNode
        write(*,*) "        - Number of Panel : ", this%nPanel
        write(*,*) " "
    end if

    1001 format(a)
    1002 format(a,i15,a)
    1003 format(3(e15.7))
    1004 format(a,2(i15))
    1005 format(i5,99(i8))
    1006 format(i5)

End Subroutine

Subroutine writeVTKSurfGaussPoint(this, filePath)
    Implicit None
    class(typSurfMesh), intent(inout) :: this
    Character(len=*)           :: filePath
    integer :: ioUnit
    integer :: i,j
    Integer :: nConnect

    if ((this%nNode.ge.1).and.(this%nPanel.ge.1)) then
        ioUnit = FIO_UNIT + 1

        Open(unit   = ioUnit,   &
             file   = trim(filePath), &
             status = "replace")

        write(ioUnit,1001) "# vtk DataFile Version 3.0"
        write(ioUnit,1001) "vtk output"
        write(ioUnit,1001) "ASCII"
        write(ioUnit,1001) "DATASET UNSTRUCTURED_GRID"

        write(ioUnit,1002) "POINTS ", this%nPanel * this%panel(1)%nGauss, " float"
        do i = 1, this%nPanel
            do j = 1, this%panel(1)%nGauss
                write(ioUnit,1003) this%panel(i)%gPoint(j)%x(), this%panel(i)%gPoint(j)%y(), this%panel(i)%gPoint(j)%z()
            enddo
        enddo

        ! write(ioUnit,1002) "CELL_DATA ", this%nPanel
        ! write(ioUnit,1001) "SCALARS Panel_Area float"
        ! write(ioUnit,1001) "LOOKUP_TABLE default"
        ! do i = 1, this%nPanel
        ! write(ioUnit,1003) this%panel(i)%area
        ! enddo
        ! write(ioUnit,1001) "NORMALS Panel_Normal float"
        ! do i = 1, this%nPanel
        ! write(ioUnit,1003) this%panel(i)%normal%x(), this%panel(i)%normal%y(), this%panel(i)%normal%z()
        ! enddo

        Close(ioUnit)
    else
        write(*,*) " "
        write(*,*) "    [WARNING] writeVTKSurfGaussPoint(typSurfMesh, path)"
        write(*,*) "        Number of Node and Panel should be larger than 1."
        write(*,*) " "
        write(*,*) "        - Number of Node  : ", this%nNode
        write(*,*) "        - Number of Panel : ", this%nPanel
        write(*,*) " "
    end if

    1001 format(a)
    1002 format(a,i15,a)
    1003 format(3(e15.7))
    1004 format(a,2(i15))
    1005 format(i5,99(i8))
    1006 format(i5)

End Subroutine

Subroutine writeVTKLineMesh(this, filePath)
    Implicit None
    class(typSurfMesh), intent(inout) :: this
    Character(len=*)           :: filePath
    integer :: ioUnit
    integer :: i,j
    Integer :: nConnect

    if ((this%nLineNode.ge.1).and.(this%nLine.ge.1)) then
        ioUnit = FIO_UNIT + 1

        Open(unit   = ioUnit,   &
             file   = trim(filePath), &
             status = "replace")

        write(ioUnit,1001) "# vtk DataFile Version 3.0"
        write(ioUnit,1001) "vtk output"
        write(ioUnit,1001) "ASCII"
        write(ioUnit,1001) "DATASET UNSTRUCTURED_GRID"

        write(ioUnit,1002) "POINTS ", this%nLineNode, " float"
        do i = 1, this%nLineNode
        write(ioUnit,1003) this%lineNode(i)%x(), this%lineNode(i)%y(), this%lineNode(i)%z()
        enddo

        write(ioUnit,1004) "CELLS ", this%nLine, this%nLine * 3
        do i = 1, this%nLine
        write(ioUnit,1005) this%linePanel(i)%nNode(),           &
                           (this%linePanel(i)%nodeLabel(j)-1, j = 1, this%linePanel(i)%nNode())
        enddo

        write(ioUnit,1002) "CELL_TYPES ", this%nLine
        do i = 1, this%nLine
        write(ioUnit,1006) this%linePanel(i)%vtkCellType()
        enddo

        write(ioUnit,1002) "CELL_DATA ", this%nLine
        write(ioUnit,1001) "SCALARS Panel_Area float"
        write(ioUnit,1001) "LOOKUP_TABLE default"
        do i = 1, this%nLine
        write(ioUnit,1003) this%linePanel(i)%area
        enddo
        write(ioUnit,1001) "NORMALS Panel_Normal float"
        do i = 1, this%nLine
        write(ioUnit,1003) this%linePanel(i)%normal%x(), this%linePanel(i)%normal%y(), this%linePanel(i)%normal%z()
        enddo

        Close(ioUnit)
    else
        write(*,*) " "
        write(*,*) "    [WARNING] writeVTKLineMesh(typSurfMesh, path)"
        write(*,*) "        Number of Line Node and Line should be larger than 1."
        write(*,*) " "
        write(*,*) "        - Number of Line Node  : ", this%nLineNode
        write(*,*) "        - Number of Line Panel : ", this%nLine
        write(*,*) " "
    end if

    1001 format(a)
    1002 format(a,i15,a)
    1003 format(3(e15.7))
    1004 format(a,2(i15))
    1005 format(i5,4(i8))
    1006 format(i5)

End Subroutine

Subroutine writeVTKLineMeshGaussPoint(this, filePath)
    Implicit None
    class(typSurfMesh), intent(inout) :: this
    Character(len=*)           :: filePath
    integer :: ioUnit
    integer :: i,j
    Integer :: nConnect

    if ((this%nLineNode.ge.1).and.(this%nLine.ge.1)) then
        ioUnit = FIO_UNIT + 1

        Open(unit   = ioUnit,   &
             file   = trim(filePath), &
             status = "replace")

        write(ioUnit,1001) "# vtk DataFile Version 3.0"
        write(ioUnit,1001) "vtk output"
        write(ioUnit,1001) "ASCII"
        write(ioUnit,1001) "DATASET UNSTRUCTURED_GRID"

        write(ioUnit,1002) "POINTS ", this%nLine * this%linePanel(1)%nGauss, " float"
        do i = 1, this%nLine
        do j = 1, this%linePanel(1)%nGauss
        write(ioUnit,1003) this%linePanel(i)%gPoint(j)%vec()
        enddo
        enddo

        Close(ioUnit)
    else
        write(*,*) " "
        write(*,*) "    [WARNING] writeVTKLineMesh(typSurfMesh, path)"
        write(*,*) "        Number of Line Node and Line should be larger than 1."
        write(*,*) " "
        write(*,*) "        - Number of Line Node  : ", this%nLineNode
        write(*,*) "        - Number of Line Panel : ", this%nLine
        write(*,*) " "
    end if

    1001 format(a)
    1002 format(a,i15,a)
    1003 format(3(e15.7))
    1004 format(a,2(i15))
    1005 format(i5,4(i8))
    1006 format(i5)

End Subroutine

Subroutine assignCopySurfMesh(to, from)
    class(typSurfMesh), intent(inout) :: to
    class(typSurfMesh), intent(in)    :: from

    to%isOpenTop = from%isOpenTop
    to%nNode     = from%nNode
    to%nLineNode = from%nLineNode
    to%nPanel    = from%nPanel
    to%nLine     = from%nLine
    to%nLineSub  = from%nLineSub

    if (to%nNode.gt.0) then
        if (allocated(to%node)) deallocate(to%node)
        allocate(to%node( to%nNode ))
        to%node = from%node
    end if

    if (to%nPanel.gt.0) then
        if (allocated(to%panel)) deallocate(to%panel)
        allocate(to%panel( to%nPanel ))
        to%panel = from%panel
    end if

    if (to%nLineNode.gt.0) then
        if (allocated(to%lineNode)) deallocate(to%lineNode)
        allocate(to%lineNode( to%nLineNode ))
        to%lineNode = from%lineNode
    endif

    if (to%nLine.gt.0) then
        if (allocated(to%linePanel)) deallocate(to%linePanel)
        allocate(to%linePanel( to%nLine ))
        to%linePanel = from%linePanel
    end if

End Subroutine

Subroutine destroySurfMesh(this)
    Implicit None
    Class(typSurfMesh), intent(inout) :: this
    integer :: i

    ! if (allocated(this%panel)) then
    !     do i = 1, this%nPanel
    !         Call this%panel(i)%destroy()
    !     enddo
    !     deallocate(this%panel)
    ! end if
    !
    ! if (allocated(this%linePanel)) then
    !     do i = 1, this%nPanel
    !         Call this%linePanel(i)%destroy()
    !     enddo
    !     deallocate(this%linePanel)
    ! end if

    this%isOpenTop = .FALSE.
    this%nNode     = 0;     this%nLineNode = 0;     this%nPanel     = 0;
    this%nLine = 0;         this%nLineSub = 0;

End Subroutine

Subroutine finalSurfMesh(this)
    Implicit None
    Type(typSurfMesh), intent(inout) :: this
    Call this%destroy()
End Subroutine

!! ------------------------------------------------------------------ !!
!! End Procedures
!! ------------------------------------------------------------------ !!


!! ------------------------------------------------------------------ !!
!! Derived Class Procedure
!! ------------------------------------------------------------------ !!

    !!- paraPipeSurfMesh
    Include "surfMesh/paraPipeSurfMesh.inc"

    !!- Sphere Mesh
    Include "surfMesh/sphereSurfMesh.inc"

    !!- Hemisphere Mesh
    Include "surfMesh/hemiSphereSurfMesh.inc"

    !!- Ellipsoid Mesh
    Include "surfMesh/ellipsoidSurfMesh.inc"

    !!- Semi-Ellipsoid Mesh
    Include "surfMesh/semiEllipsoidSurfMesh.inc"

    !!- Circular Cylinder Shell Mesh
    Include "surfMesh/circularCylinderShell.inc"

!! ------------------------------------------------------------------ !!
!! Derived Class Procedure
!! ------------------------------------------------------------------ !!
