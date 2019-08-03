! Here is an example of subroutine that create the configuation file for non-dimentional parameters and a defined WaveLength.

SUBROUTINE write_RF_input_wavelength_imposed (prof,H,wavelength)

IMPLICIT NONE
REAL, INTENT(in) :: prof,H,wavelength
DOUBLE PRECISION :: PI

PI    = 3.141592653589793238462643383279502888419


OPEN(UNIT=213,FILE='input_RF.dict',STATUS='UNKNOWN')

WRITE(213,'(a)') 'waveInput waveStreamIcare;'
WRITE(213,'(a)') ' '
WRITE(213,'(a)') 'waveStreamIcare'
WRITE(213,'(a)') '{'
WRITE(213,'(a,i1,a1,a100)') 'GeneralDimension ',0,';','// Dimensional (=1) or Non-dimensional (=0)'
WRITE(213,'(a,e15.7,a1)') 'GeneralDepth',2*PI*prof/wavelength,';'
WRITE(213,'(a,i5,a1,a100)') 'GeneralModes',20,';','//Number of modes for first evaluation'
WRITE(213,'(a,a,a1,a100)') 'WaveInput ','WaveLength',';','//Period or WaveLength'
WRITE(213,'(a,e15.7,a1,a100)') 'WaveLength ',1.0,';','// WaveLength if "WaveInput WaveLength" Period if "WaveInput Period"'
WRITE(213,'(a,e15.7,a1)') 'WaveHeight ',2*PI*H/wavelength,';'
WRITE(213,'(a,e15.7,a1,a100)') 'CurrentValue',0.0d0,';','// value of current '!! dimensional if input%GeneralDimension=1 / non-dimensional (see report_R&F.pdf) if input%GeneralDimension=1'
WRITE(213,'(a,i1,a1,a100)') 'CurrentType',0,';','// type of current ; 1 mass transport / 0 eulerian' !!ian current'
WRITE(213,'(a)') '}'
WRITE(213,'(a)') 'Options'
WRITE(213,'(a)') '{'
WRITE(213,'(a,i5,a1,a100)') 'n_H ', 100,';','// Number of steps in wave height'
WRITE(213,'(a,i1,a1,a100)') 'err_type ', 1,';','// Error type / 0 absolute ; 1 relative'
WRITE(213,'(a,e15.7,a1,a100)') 'eps_err  ', 1d-10,';','// Tolerance on the equations'
WRITE(213,'(a,e15.7,a1,a100)') 'err_max  ', 1.0d7,';','// Divergence criteria'
WRITE(213,'(a,e15.7,a1,a100)') 'eps_inc  ', 1d-10,';','// Convergence criteria on unknowns'
WRITE(213,'(a,e15.7,a1,a100)') 'eps_N1   ', 1d-10,';','// Decision criteria on the modes'
WRITE(213,'(a,i5,a1,a100)') 'itermax  ', 999,';','// Maximum number of iterations'
WRITE(213,'(a,i1,a1,a100)') 'increment_type ', 1,';','// Increment type for wave height / 0 linear ; 1 exp' !! onential'
WRITE(213,'(a,i1,a1,a100)') 'printonscreen ', 1,';','// Print on screen =1 / do not print on screen = 0'
WRITE(213,'(a,i1,a1,a100)') 'writeoutput ', 0,';','// Write output files =1 / do not Write output files = 0'
WRITE(213,*) '}'

CLOSE(213)

END SUBROUTINE
