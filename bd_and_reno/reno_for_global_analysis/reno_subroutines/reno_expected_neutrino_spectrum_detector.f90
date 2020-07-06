!####################################################################
!
!   renoExpectedNeutrinoSpectrum_d is the function that return the
!       expected neutirno spectrum in the detector "d" from 
!       all reactors
!
!####################################################################
function renoExpectedNeutrinoSpectrum_d(x,d,t13,dmee)
    use types
    use reno_data, only: RCTS,LT_d,eps,eta,fr
    implicit none
    real(dp) :: renoExpectedNeutrinoSpectrum_d
    real(dp) :: x,t13,dmee                     ! Neutrino energy in [MeV]
    integer  :: d,r                   ! the number ot detector
    real(dp) :: oe(2)                 ! oe is the Overarl efficiency per detetor    
    real(dp) :: renoExpectedNeutrinoSpectrum_dr! Is the expected neutirno spectrum in the detector "d" from the reactor "r"
    real(dp) :: result

    !oe=(/0.4d0,0.7d0/)
    oe=(/0.7647d0,0.7647d0/)
    result = 0.0    
    do r = 1,RCTS
        result=result+renoExpectedNeutrinoSpectrum_dr(x,d,r,t13,dmee) ![#/MeV]
    enddo 
    renoExpectedNeutrinoSpectrum_d = 86400.0d0*oe(d)*LT_d(d)*result! Al multiplicar por 86400 se transforman los días a segundos    
    return
end function renoExpectedNeutrinoSpectrum_d