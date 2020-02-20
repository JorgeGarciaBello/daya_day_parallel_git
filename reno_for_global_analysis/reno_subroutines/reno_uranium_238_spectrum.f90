!################################################
!
!   spectrum238U is the spectrum of Uranium-238
!
!################################################
real(8) function spectrum238U(x) ![#/(MeV fission)]
    implicit none
    real(8) :: x      ! Energía de neutinros
    real(8) :: a(6)   ! Array of the coefficients

    select case(1)
        case(1)! A. Mueller - Isotopos spectrum - “ Th. A. Mueller et al., Phys. Rev. C 83 054615 (2011).“
            a=(/4.833d-1,1.927d-1,-1.283d-1,-6.762d-3,2.2233d-3,-1.536d-4/)
        case(2)
            a=(/0.483300D0,0.192700D0,-0.128300D0,-0.00676200D0,0.00223300D0,-0.000153600D0/)
    end select

    !Espectros de Mueller
    spectrum238U = EXP(a(1) + a(2)*x + a(3)*x**2 + a(4)*x**3 + a(5)*x**4 + a(6)*x**5)
end function spectrum238U
