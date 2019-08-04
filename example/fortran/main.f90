Program Main
    use modVariablesRF
    use modComm_CN_Stream
    Implicit None

    !! Variables --------------------------------------------------------
    Integer,Parameter      :: nChar = 200    !! Default Character Length
    Character(nChar) :: lib_CN_stream_Path
    Character(nChar) :: waveConfigFile

    type(RF_type) :: RF
    type(Option_type) :: option
    type(Output_type) :: output

    lib_CN_stream_Path = "libCNStream.so"
    waveConfigFile = "RF_input.dict"

    Call callCNStream(lib_CN_stream_Path)
    Call calcRF(waveConfigFile,RF,option)
    Call reconstructRF(RF, option, 0._rp, 0._rp, 0._rp, 0._rp, 0._rp, .FALSE., output)

End Program
