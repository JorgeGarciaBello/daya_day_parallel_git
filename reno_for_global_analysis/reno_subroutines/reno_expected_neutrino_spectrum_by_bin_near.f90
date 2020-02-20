!####################################################################
!
!   renoExpectedNeutrinoSpectrumByBinNear is the function that return the
!       expected neutirno spectrum by hall
!
!####################################################################
real(8) function renoExpectedNeutrinoSpectrumByBinNear(i)
    use reno_data, only: NBIN,bines
    implicit none
    integer,parameter :: d=1      ! the number of detector far
    integer :: i                  ! is the number of bin

    real(8) :: renoExpectedNeutrinoSpectrum_d !is the expected neutirno spectrum in the detector "d" from all reactors

    real(8) :: a,b               ! limits of the bin
    real(8) :: h
    integer :: k,n=100
    real(8) :: x

    real(8) :: binCalibration(NBIN)

    binCalibration=(/   1.20646988240638d0,1.08688176573643d0,1.03626673205756d0,0.995117418219248d0,0.988100588246127d0,   &
                        0.980691159540664d0,0.971441275791411d0,0.969133730563585d0,0.968089269959276d0,0.973790578945201d0,&  
                        0.978903460001357d0,0.974529139163897d0,0.967824966514772d0,0.957945292946237d0,0.989494239825906d0,&
                        1.00952735643843d0,1.05194665150937d0,1.07537146331854d0,1.08712249235161d0,1.10096188951287d0,     &
                        1.08237600363564d0,1.05637098405902d0,1.0430204051962d0,1.01015632862815d0,0.941903431449936d0,     &
                        0.207850108593314d0/)

    a=bines(i,1); b=bines(i,2);
    renoExpectedNeutrinoSpectrumByBinNear=0.0d0
    h=(b-a)/real(n)
    do k=1,n
        x=a+h*real(k-1)   
        renoExpectedNeutrinoSpectrumByBinNear=renoExpectedNeutrinoSpectrumByBinNear       &
                                          + h*(  renoExpectedNeutrinoSpectrum_d(x,d)    &
                                                +renoExpectedNeutrinoSpectrum_d(x+h,d)  &                                                
                                        )/2.0d0
    enddo
    renoExpectedNeutrinoSpectrumByBinNear= binCalibration(i) &
                                          *(0.418945599575372d0)*(0.2)*(renoExpectedNeutrinoSpectrumByBinNear/(b-a))!(0.2)*(renoExpectedNeutrinoSpectrumByBinNear/(b-a))
                                          
    return    
end function renoExpectedNeutrinoSpectrumByBinNear