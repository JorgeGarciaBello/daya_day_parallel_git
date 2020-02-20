real(8) function fittingSpectrumFunction(alpha)
    use neu_osc_parameters
    use db_data, only: NBIN,farExp
    implicit none
    real(8) :: alpha   
   
    real(8) :: expectedNeutrinoSpectrumByBinFar
    real(8) :: expectedNumberNeutrinoByBinFar
    integer :: i   

    t12=0.0d0!0.5837630475918688D0!PI/4.0D0
    t13=0.14795088706d0!0.13023999999999999d0!
    t23=0.0d0!0.7853981631d0!0.698356658079 !0.0D0!PI/
    t14=0.0D0
    t24=0.0D0
    t34=0.0D0    
    delta1=0.0D0
    delta2=0.0D0
    delta3=0.0D0
    dm21=0.0d0!7.53D-5
    dm31=2.5D-3!2.8560000000000000d-003!2.5D-3
    dm41=0.0d0

    fittingSpectrumFunction=0.0d0
    do i=1,NBIN
        fittingSpectrumFunction=fittingSpectrumFunction   &
                               +(farExp(i)-alpha*expectedNeutrinoSpectrumByBinFar(i))**2/farExp(i)
    enddo
    print*, fittingSpectrumFunction
    return
end function fittingSpectrumFunction
