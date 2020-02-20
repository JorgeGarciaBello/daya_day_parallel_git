!####################################################################
!
!   expectedNeutrinoSpectrum_H is the function that return the
!       expected neutirno spectrum by hall
!
!####################################################################
real(8) function expectedNeutrinoSpectrum_H(hall,x)
    implicit none     
    integer :: hall               ! Is the hallber of the Hall
    real(8) :: x                  ! Neutrino energy in [MeV]        

    real(8) :: expectedNeutrinoSpectrum_d!is the expected neutirno spectrum in the detector "d" from all reactors
    integer :: d                  ! the hallber ot detector

    expectedNeutrinoSpectrum_H=0.0D0
    select case(hall)
        case(1)
            do d=1,2
                expectedNeutrinoSpectrum_H =   expectedNeutrinoSpectrum_H      &
                                             + expectedNeutrinoSpectrum_d(x,d)
            enddo
        case(2)
            do d=3,4
                expectedNeutrinoSpectrum_H =   expectedNeutrinoSpectrum_H      &
                                             + expectedNeutrinoSpectrum_d(x,d)
            enddo
        case(3)
            do d=5,8
                expectedNeutrinoSpectrum_H =  expectedNeutrinoSpectrum_H       &
                                            + expectedNeutrinoSpectrum_d(x,d)
            enddo
    end select
    return    
end function expectedNeutrinoSpectrum_H