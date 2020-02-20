!#################################################################################################################
!
!       expectedNeutrinoSpectrumByBinFar: is the function that return 
!           the expected neutirno spectrum bay energy-bin and by far-hall
!
!#################################################################################################################
real(8) function expectedNeutrinoSpectrumByBinFar(i)
    use db_data, only: bines,NBIN
    implicit none   
    integer :: i                          ! is the number of energy bin

    real(8) :: expectedNeutrinoSpectrum_H !function that return the expected neutirno spectrum by hall    
    integer,parameter :: hall=3           ! number of hall
    real(8) :: a,b                        ! limits of the bin
    real(8) :: h
    integer :: k,n=100
    real(8) :: x

    real(8) :: corrections(NBIN)
    real(8) :: corrections_percetage(NBIN)
    real(8) :: ALPHA(NBIN)

    !PROBAR CON LA SIGUIETE ALPHA POR BIN
    ALPHA=(/1.02,1.026,1.034,1.03,1.03,1.02,1.01,1.01,1.01,1.01,   &
            1.00,1.01,1.00,1.01,1.00,1.01,1.00,1.00,1.01,0.99,     &
            0.95,0.98,1.00,1.00,0.90,0.70/)
    corrections_percetage=(/1.32477148644256,1.5500858788633,1.41907408490622,1.34533759811376,1.30442442923759,   &
                            1.30059002819114,1.29469740363737,1.28807123120626,1.30459540986711,1.31625507801924,  &
                            1.31011244550608,1.30624498764775,1.34262402548813,1.4067285619341,1.47400688596567,   &
                            1.51171390358526,1.53910448100627,1.54718696058569,1.5226560040757,1.53483904964685,   &
                            1.57304367810321,1.5615914239757,1.56683898112058,1.29239009110244,1.79612504926691,   &
                            1.18819852607375/)    
        
    a=bines(i,1); b=bines(i,2)
    expectedNeutrinoSpectrumByBinFar=0.0d0
    h=(b-a)/real(n)
    do k=1,n
        x=a+h*real(k-1)
        expectedNeutrinoSpectrumByBinFar=expectedNeutrinoSpectrumByBinFar      &
                                    + h*( expectedNeutrinoSpectrum_H(hall,x)   &
                                         +expectedNeutrinoSpectrum_H(hall,x+h) &
                                        )/2.0d0
    enddo 
    expectedNeutrinoSpectrumByBinFar=ALPHA(i)*corrections_percetage(i)*(expectedNeutrinoSpectrumByBinFar/(b-a))    
    return
end function expectedNeutrinoSpectrumByBinFar