Module modComm_CN_Stream

use iso_c_binding
use modVariablesRF

Implicit None



!! Dummy Character Length
Integer,parameter :: procNChar = 1000

!! Shared Object Path
Character(len=procNChar) :: libCNStreamPath = "libCNStream.so"

!! Is Shared Library is loaded
logical :: isCNStreamLoad = .FALSE.

!! Fortran Subroutine C Header
!Character(len=procNChar),parameter :: headerG2G = "__modgrid2grid_MOD_"


! interface to linux API
integer(c_int), parameter :: rtld_lazy=1 ! value extracte from the C header file
integer(c_int), parameter :: rtld_now=2  ! value extracte from the C header file

interface
    function dlopen(filename, mode) bind(c,name="dlopen")
    ! void *dlopen(const char *filename, int mode);
    use iso_c_binding
    implicit none
    type(c_ptr) :: dlopen
    character(c_char), intent(in) :: filename(*)
    integer(c_int), value :: mode
    end function

    function dlsym(handle, name) bind(c,name="dlsym")
    ! void *dlsym(void *handle, const char *name);
    use iso_c_binding
    implicit none
    type(c_funptr) :: dlsym
    type(c_ptr), value :: handle
    character(c_char), intent(in) :: name(*)
    end function

    function dlclose(handle) bind(c,name="dlclose")
    ! int dlclose(void *handle);
    use iso_c_binding
    implicit none
    integer(c_int) :: dlclose
    type(c_ptr), value :: handle
    end function
end interface


! Define interface of call-back routine.
abstract interface

    subroutine proc_calcRF(configFile, RF, option)
        use modVariablesRF
        character(*), intent(in) :: configFile
        type(RF_type), intent(out)   :: RF
        type(Option_type), intent(out)  :: option
    end subroutine

    subroutine proc_initAiry(configFile, RF, option)
        use modVariablesRF
        character(*), intent(in) :: configFile
        type(RF_type), intent(out)   :: RF
        type(Option_type), intent(out)  :: option
    end subroutine

    subroutine proc_reconstructRF(RF, option, x, y, z, time, thetaincident, hydrostatic, output, max_exp)
        use modVariablesRF
        type(RF_type), intent(in)   :: RF
        type(Option_type), intent(in)  :: option
        real(RP),  intent(in)          :: x, y, z !have to be dimensional
        real(RP), intent(in)           :: time, thetaincident !,x, y, z have to be dimensional
        type(Output_type), intent(out) :: output
        real(RP),  intent(in),optional :: max_exp
        logical, intent(in)            :: hydrostatic
    end subroutine

    subroutine proc_airy(RF, option, x, y, z, time, thetaincident, hydrostatic, output, max_exp)
        use modVariablesRF
        type(RF_type), intent(in)   :: RF
        type(Option_type), intent(in)  :: option
        real(RP),  intent(in)          :: x, y, z !have to be dimensional
        real(RP), intent(in)           :: time, thetaincident !,x, y, z have to be dimensional
        type(Output_type), intent(out) :: output
        real(RP),  intent(in),optional :: max_exp
        logical, intent(in)            :: hydrostatic
    end subroutine

end interface

type(c_ptr) :: handle
procedure(proc_calcRF), pointer       :: ptr_calcRF
procedure(proc_initAiry), pointer       :: ptr_initAiry
procedure(proc_reconstructRF), pointer   :: ptr_reconstructRF
procedure(proc_airy), pointer   :: ptr_airy

contains

subroutine linkCNStreamSubroutine(proc_ptr,subName)
    Implicit None
    procedure(), pointer,intent(inout)  :: proc_ptr
    Character(len=procNChar),intent(in) :: subName

    type(c_funptr) :: ptr

    ptr=dlsym(handle, trim(subName)//c_null_char)

    if (.not. c_associated(ptr))then
        write(*,*) 'Unable to load the procedure :', trim(subName)
        stop
    end if

    call c_f_procpointer(ptr, proc_ptr)
End Subroutine


subroutine callCNStream(CNStreamPath)
    character(len=*), intent(in) :: CNStreamPath
    character(len=procNChar)   :: subroutineName

    libCNStreamPath = ''
    libCNStreamPath = CNStreamPath

    handle=dlopen(trim(libCNStreamPath)//c_null_char, RTLD_LAZY)
    if (.not. c_associated(handle))then
        write(*,*) 'Unable to load shared library : ', trim(libCNStreamPath)
        isCNStreamLoad = .FALSE.
        stop
    else
        write(*,*) 'Load shared library : ', trim(libCNStreamPath)
        isCNStreamLoad = .TRUE.

        subroutineName = "__modcns_MOD_calcrf"
        Call linkCNStreamSubroutine(ptr_calcRF, subroutineName)

        subroutineName = "__modcns_MOD_initairy"
        Call linkCNStreamSubroutine(ptr_initAiry, subroutineName)

        subroutineName = "__modcns_MOD_recrf"
        Call linkCNStreamSubroutine(ptr_reconstructRF, subroutineName)

        subroutineName = "__modcns_MOD_airy"
        Call linkCNStreamSubroutine(ptr_airy, subroutineName)
    end if
end subroutine

subroutine calcRF(configFile, RF, option)
    character(*), intent(in) :: configFile
    type(RF_type), intent(inout)   :: RF
    type(Option_type), intent(out)  :: option

    Call ptr_calcRF(configFile, RF, option)
end subroutine

subroutine initAiry(configFile, RF, option)
    character(*), intent(in) :: configFile
    type(RF_type), intent(inout)   :: RF
    type(Option_type), intent(out)  :: option

    Call ptr_initAiry(configFile, RF, option)
end subroutine

subroutine reconstructRF(RF, option, x, y, z, time, thetaincident, hydrostatic, output, max_exp)

    use modVariablesRF

    type(RF_type), intent(in)   :: RF
    type(Option_type), intent(in)  :: option
    real(RP),  intent(in)          :: x, y, z !have to be dimensional
    real(RP), intent(in)           :: time, thetaincident !,x, y, z have to be dimensional
    type(Output_type), intent(out) :: output
    real(RP),  intent(in),optional :: max_exp
    logical, intent(in)            :: hydrostatic


    Call ptr_reconstructRF(RF, option, x, y, z, time, thetaincident, hydrostatic, output, max_exp)
end subroutine

subroutine airy(RF, option, x, y, z, time, thetaincident, hydrostatic, output, max_exp)
    type(RF_type), intent(in)   :: RF
    type(Option_type), intent(in)  :: option
    real(RP),  intent(in)          :: x, y, z !have to be dimensional
    real(RP), intent(in)           :: time, thetaincident !,x, y, z have to be dimensional
    type(Output_type), intent(out) :: output
    real(RP),  intent(in),optional :: max_exp
    logical, intent(in)            :: hydrostatic

    Call ptr_airy(RF, option, x, y, z, time, thetaincident, hydrostatic, output, max_exp)
end subroutine

End Module
