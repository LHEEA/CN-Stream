Module mfpGlobal
Implicit None

    Integer,Parameter :: FIO_UNIT = 100

    Integer,parameter :: RP = kind(1.d0)

    Integer,parameter :: CHAR_LEN = 300

    Real(RP),parameter :: pi = 4.d0 * datan(1.d0)

    Real(RP),parameter :: gravi = 9.81_RP

    Complex(RP), Parameter :: ai = (0.0_RP, 1.0_RP)

End Module
