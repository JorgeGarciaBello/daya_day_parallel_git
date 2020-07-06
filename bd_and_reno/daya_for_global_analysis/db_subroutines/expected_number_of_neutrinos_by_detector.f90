!##########################################################
!
!   expectedNumberNeutrinosDetector is the function 
!       that return the number of expected neutrinos  
!       per detector
!
!##########################################################
real(8) function expectedNumberNeutrinosDetector(d)
    implicit none     
    integer :: d                  ! is the number of detector   

    real(8) :: x                  ! Neutrino energy in [MeV]    
    real(8) :: expectedNeutrinoSpectrum_d!is the expected neutirno spectrum in the detector "d"
    real(8) :: a,b,h              ! parameters for the integral
    integer :: i,n=200

    a=1.801001d0; b=12.8!78D0
    h=(b-a)/real(n)
    expectedNumberNeutrinosDetector=0.0d0
    do i=1,n
        x=a+h*real(i-1)
        expectedNumberNeutrinosDetector=expectedNumberNeutrinosDetector           &
                                         + h*(  expectedNeutrinoSpectrum_d(x,d)   &
                                               +expectedNeutrinoSpectrum_d(x+h,d) &
                                             )/2.0d0
    enddo
    expectedNumberNeutrinosDetector=expectedNumberNeutrinosDetector!*(1.316707311261006d0)
    return
end function expectedNumberNeutrinosDetector