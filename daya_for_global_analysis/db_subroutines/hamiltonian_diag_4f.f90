!################################################
!
!   hamiltonianDiag_4f: is a subroutine that 
!       load the hamiltonian-diag-matrix 
!       4-framework 
!
!################################################
subroutine hamiltonianDiag_4f(Hdiag)
    implicit none
    real(8) :: Hdiag(4,4)         ! Hdiag is the hamiltonian differential 
    real(8) :: E1                 ! E1 is the energy of the mass eigenstate two
    E1=0.0d0
    
    Hdiag(1,1)=E1;     Hdiag(1,2)=0.0d0;  Hdiag(1,3)=0.0d0; Hdiag(1,4)=0.0d0
    Hdiag(2,1)=0.0d0;  Hdiag(2,2)=E1;     Hdiag(2,3)=0.0d0; Hdiag(2,4)=0.0d0
    Hdiag(3,1)=0.0d0;  Hdiag(3,2)=0.0d0;  Hdiag(3,3)=E1;    Hdiag(3,4)=0.0d0
    Hdiag(4,1)=0.0d0;  Hdiag(4,2)=0.0d0;  Hdiag(4,3)=0.0d0; Hdiag(4,4)=E1
    return
end subroutine hamiltonianDiag_4f