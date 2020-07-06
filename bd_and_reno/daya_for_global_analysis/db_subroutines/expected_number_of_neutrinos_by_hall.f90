!##########################################################
!
!   expectedNumberNeutrinosHall is the function 
!       that return the number of expected neutrinos  
!       per experimental hall
!
!##########################################################
real(8) function expectedNumberNeutrinosHall(hall)
    implicit none    
    integer :: hall               ! is the number of experimental hall    
    
    real(8) :: x                  ! Neutrino energy in [MeV]
    real(8) :: expectedNeutrinoSpectrum_H!is the function that return the expected neutirno spectrum by hall
    real(8) :: a,b,h              ! parameters for the integral
    integer :: i,n=400

    a=1.801001d0; b=12.78d0
    h=(b-a)/real(n)
    expectedNumberNeutrinosHall=0.0d0
    do i=1,n
        x=a+h*real(i-1)   
        expectedNumberNeutrinosHall=expectedNumberNeutrinosHall &
                                    + h*( expectedNeutrinoSpectrum_H(hall,x)    &
                                         +expectedNeutrinoSpectrum_H(hall,x+h)  &
                                        )/2.0d0
    enddo
    return
end function expectedNumberNeutrinosHall