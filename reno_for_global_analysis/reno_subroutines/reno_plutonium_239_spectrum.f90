!################################################
!
!   spectrum239Pu is the spectrum of 
!                 Plutonium-239
!
!################################################
real(8) function spectrum239Pu(x) ![#/(MeV fission)]
    implicit none
    real(8) :: x      ! Energía de neutinros
    real(8) :: a(6)   ! Array of the coefficients

    select case(1)
        case(1)! Patrick Huber -Isotopos spectrum - “ P. Huber, Phys. Rev. C 84, 024617 (2011).“ 
            a=(/4.757d0,-5.392d0,2.56d0,-6.596d-1,7.82d-2,-3.536d-3/)
        case(2)
            a=(/6.41300D0,-7.43200D0,3.53500D0,-0.882000D0,0.102500D0,-0.00455000D0/)
        case(3)
            a=(/4.757d0,-5.392d0,2.56d0,-6.596d-1,7.82d-2,-3.536d-3/)
    end select
    
    !Espectros de Mueller
    spectrum239Pu = EXP(a(1) + a(2)*x + a(3)*x**2 + a(4)*x**3 + a(5)*x**4 + a(6)*x**5)
end function spectrum239Pu

