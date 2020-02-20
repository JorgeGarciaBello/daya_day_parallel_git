!################################################
!
!   spectrum235U is the spectrum of Uranium-235
!
!################################################
real(8) function spectrum235U(x) ![#/(MeV fission)]
    implicit none
    real(8) ::varAux
    real(8) :: x      ! Energía de neutinros
    real(8) :: a(6)   ! Array of the coefficients

    select case(1)
        case(1)
            a=(/4.367d0,-4.577d0,2.10d0,-5.294d-1,6.186d-2,-2.777d-3/)
        case(2)
            a=(/3.21700D0,-3.11100D0,1.39500D0,-0.369000D0,0.0444500D0,-0.00205300D0/)
    end select

    !Espectros de Mueller
    spectrum235U = EXP(a(1) + a(2)*x + a(3)*x**2 + a(4)*x**3 + a(5)*x**4 + a(6)*x**5)
end function spectrum235U
