!####################################################################
!
!   renoExpectedNeutrinoSpectrumByBinFar is the function that return the
!       expected neutirno spectrum by hall
!
!####################################################################
function renoExpectedNeutrinoSpectrumByBinFar(i,t13,dmee)
    use types
    use reno_data, only: NBIN,bines,detector_calibration
    implicit none
    real(dp) :: renoExpectedNeutrinoSpectrumByBinFar !is the expected neutirno spectrum in the detector "d" from all reactors
    integer,parameter :: d=2      ! the number of detector far
    integer  :: i                  ! is the number of bin   
    real(dp) :: t13,dmee
    real(dp) ::renoExpectedNeutrinoSpectrum_d !is the expected neutirno spectrum in the detector "d" from all reactors
    real(dp) :: a,b,h               ! limits of the bin    
    integer  :: k,n=10
    real(dp) :: x, result
   
    a=bines(i,1); b=bines(i,2);h=(b-a)/real(n);
    result=0.0d0
    do k=1,n
        x=a+h*real(k-1)
        if (x<1.8010001) x=1.8010001
        result=result+h*(  renoExpectedNeutrinoSpectrum_d(x,d,t13,dmee)        &
                          +renoExpectedNeutrinoSpectrum_d(x+h,d,t13,dmee))/2.0d0
    enddo    
    renoExpectedNeutrinoSpectrumByBinFar=1.08102772018778d0*detector_calibration(2)*0.2d0*(result/(b-a))
    !renoExpectedNeutrinoSpectrumByBinFar=detector_calibration(2)*0.2d0*(result/(b-a))    
    return    
end function renoExpectedNeutrinoSpectrumByBinFar