!####################################################################
!
!   renoExpectedNeutrinoSpectrumByBinNear is the function that return the
!       expected neutirno spectrum by hall
!
!####################################################################
function renoExpectedNeutrinoSpectrumByBinNear(i,t13,dmee)
    use types
    use reno_data, only: NBIN,bines,detector_calibration
    implicit none
    integer,parameter :: d=1      ! the number of detector far
    real(dp) :: renoExpectedNeutrinoSpectrumByBinNear
    real(dp) :: t13,dmee
    integer  :: i                  ! is the number of bin
    real(dp) :: renoExpectedNeutrinoSpectrum_d !is the expected neutirno spectrum in the detector "d" from all reactors
    real(dp) :: a,b,h               ! limits of the bin    
    integer  :: k,n=10
    real(dp) :: x
    real(dp) :: binCalibration(NBIN)
    real(dp) :: result

    a=bines(i,1); b=bines(i,2); h=(b-a)/real(n)
    result=0.0d0
    do k=1,n
        x=a+h*real(k-1)
        if (x<1.8010001) x=1.8010001
        result=result+h*(  renoExpectedNeutrinoSpectrum_d(x,d,t13,dmee)          &
                          +renoExpectedNeutrinoSpectrum_d(x+h,d,t13,dmee) )/2.0d0
    enddo    
    !renoExpectedNeutrinoSpectrumByBinNear=detector_calibration(1)*0.2d0*(result/(b-a))
    renoExpectedNeutrinoSpectrumByBinNear=1.01267474277184d0*detector_calibration(1)*0.2d0*(result/(b-a))
    return    
end function renoExpectedNeutrinoSpectrumByBinNear