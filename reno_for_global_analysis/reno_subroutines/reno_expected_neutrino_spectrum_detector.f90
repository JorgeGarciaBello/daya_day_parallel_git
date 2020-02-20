!####################################################################
!
!   renoExpectedNeutrinoSpectrum_d is the function that return the
!       expected neutirno spectrum in the detector "d" from 
!       all reactors
!
!####################################################################
real(8) function renoExpectedNeutrinoSpectrum_d(x,d)
    use reno_data, only: RCTS,LT_d,eps,eta,fr
    implicit none
    real(8) :: x                  ! Neutrino energy in [MeV]
    integer :: d,r                ! the number ot detector
    real(8) :: oe(2)                 ! oe is the Overarl efficiency per detetor 
    
    real(8) :: renoExpectedNeutrinoSpectrum_dr! Is the expected neutirno spectrum in the detector "d" from the reactor "r"

    !oe=(/0.4d0,0.7d0/)
    oe=(/0.7647d0,0.7647d0/)
    renoExpectedNeutrinoSpectrum_d = 0.0
    x = x !+ x*eta
    if(x.gt.1.801001d0) then
    else
     x=1.801001d0
    endif
    do r = 1,RCTS
        renoExpectedNeutrinoSpectrum_d =   renoExpectedNeutrinoSpectrum_d                   &
                                         + renoExpectedNeutrinoSpectrum_dr(x,d,r) ![#/MeV]
                                         !+ ( 1.0d0 + fr(r) )*renoExpectedNeutrinoSpectrum_dr(x,d,r) ![#/MeV]
    enddo 
    renoExpectedNeutrinoSpectrum_d = 86400.0d0*oe(d)*LT_d(d)*renoExpectedNeutrinoSpectrum_d! Al multiplicar por 86400 se transforman los días a segundos
    renoExpectedNeutrinoSpectrum_d = (1.0d0 + eps)*renoExpectedNeutrinoSpectrum_d
    return
end function renoExpectedNeutrinoSpectrum_d