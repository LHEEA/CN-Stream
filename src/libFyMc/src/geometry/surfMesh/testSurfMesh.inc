Subroutine testSurfMesh
    Implicit None

    ! Call testEllipsoid()

    ! Call testHemiSphere()

    ! Call testParaPipe()

    ! Call testSemiEllipsoid()
    !
    ! Call testSphere()

    Call testSurfMeshDict()

End Subroutine

Subroutine testSurfMeshDict
    Implicit None
    Class(typSurfMesh), allocatable :: surfMesh
    Type(typDictionaryPtr)          :: fileDict

    Call fileDict%initialize("./", "surfMesh", ".dict")

    Call initializeSurfMesh(surfMesh, fileDict%subDict("surfaceMesh"))

End Subroutine

Subroutine testEllipsoid
    Implicit None
    Type(typEllipsoidSurfMesh) :: ellipsoid
    Type(typDictionaryPtr)  :: fileDict, surfDict

    Call fileDict%initialize("./", "surfMesh", ".dict")

    surfDict = fileDict%subDict("surfaceMesh")
    Call ellipsoid%initialize(surfDict%subdict("ellipsoid"))

    Call ellipsoid%writeVTK("ellipsoid")

End Subroutine


Subroutine testHemiSphere
    Implicit None
    Type(typHemiSphereSurfMesh) :: hemisphere
    Type(typDictionaryPtr)  :: fileDict, surfDict

    Call fileDict%initialize("./", "surfMesh", ".dict")

    surfDict = fileDict%subDict("surfaceMesh")
    Call hemisphere%initialize(surfDict%subDict("hemisphere"))

    Call hemisphere%writeVTK("hemisphere")

End Subroutine

Subroutine testParaPipe
    Implicit None
    Type(typParaPipeMesh)   :: paraPipe
    Type(typDictionaryPtr)  :: fileDict, surfDict

    Call fileDict%initialize("./", "surfMesh", ".dict")

    surfDict = fileDict%subDict("surfaceMesh")
    Call paraPipe%initialize(surfDict%subDict("paraPipe"))

    Call paraPipe%writeVTK("paraPipe")

End Subroutine

Subroutine testSemiEllipsoid
    Implicit None
    Type(typSemiEllipsoidSurfMesh)   :: semiEllipsoid
    Type(typDictionaryPtr)  :: fileDict, surfDict

    Call fileDict%initialize("./", "surfMesh", ".dict")

    surfDict = fileDict%subDict("surfaceMesh")
    Call semiEllipsoid%initialize(surfDict%subDict("semiEllipsoid"))

    Call semiEllipsoid%writeVTK("semiEllipsoid")

End Subroutine

Subroutine testSphere
    Implicit None
    Type(typSphereSurfMesh) :: sphere
    Type(typDictionaryPtr)  :: fileDict, surfDict

    Call fileDict%initialize("./", "surfMesh", ".dict")

    surfDict = fileDict%subDict("surfaceMesh")
    Call sphere%initialize(surfDict%subDict("sphere"))

    Call sphere%writeVTK("sphere")

End Subroutine
