!################################################
!
!   spectrum241Pu is the spectrum of 
!                 Plutonium-241
!
!################################################
real(8) function spectrum241Pu(x) ![#/(MeV fission)]
    implicit none
    real(8) :: x      ! Energía de neutinros
    real(8) :: a(6)      ! Array of the coefficients
    select case(1)
        case(1)! Patrick Huber -Isotopos spectrum - “ P. Huber, Phys. Rev. C 84, 024617 (2011).“ 
            a=(/2.99d0,-2.882d0,1.278d0,-3.343d-1,3.905d-2,-1.754d-3/)
        case(2)
            a=(/3.25100D0,-3.20400D0,1.42800D0,-0.367500D0,0.0425400D0,-0.00189600D0/)
    end select
    !Espectros de Mueller
    spectrum241Pu = EXP(a(1) + a(2)*x + a(3)*x**2 + a(4)*x**3 + a(5)*x**4 + a(6)*x**5)
end function spectrum241Pu