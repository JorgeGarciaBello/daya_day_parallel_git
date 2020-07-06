!##########################################################
!
!   renoExpectedNumberNeutrinosDetector is the function 
!       that return the number of expected neutrinos  
!       per detector
!
!##########################################################
real(8) function renoExpectedNumberNeutrinosDetector(d)
    implicit none     
    integer :: d                  ! is the number of detector   

    real(8) :: x                  ! Neutrino energy in [MeV]    
    real(8) :: renoExpectedNeutrinoSpectrum_d!is the expected neutirno spectrum in the detector "d"
    real(8) :: a,b,h              ! parameters for the integral
    integer :: i,n=200

    a=1.801001d0; b=9.3!78D0
    h=(b-a)/real(n)
    renoExpectedNumberNeutrinosDetector=0.0d0
    do i=1,n
        x=a+h*real(i-1)
        renoExpectedNumberNeutrinosDetector=renoExpectedNumberNeutrinosDetector           &
                                         + h*(  renoExpectedNeutrinoSpectrum_d(x,d)   &
                                               +renoExpectedNeutrinoSpectrum_d(x+h,d) &
                                             )/2.0d0
    enddo
    renoExpectedNumberNeutrinosDetector=renoExpectedNumberNeutrinosDetector
    return
end function renoExpectedNumberNeutrinosDetector