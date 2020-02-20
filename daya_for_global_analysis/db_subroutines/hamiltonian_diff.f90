!################################################
!
!   hamiltonianDiff: is a subroutine that 
!       load the hamiltonian-diff-matrix 
!
!################################################
subroutine hamiltonianDiff(Hdiff,sm,aM,P)
    implicit none
    real(8) :: Hdiff(3,3)  ! Hdiff is the hamiltonian differential 
    real(8) :: sm,aM              ! sm,aM are the squared mass difference m=m_21 y M=m_32
    real(8) :: P                  ! P es el momento del neutrino o la enerǵia total
    real(8),parameter :: unityFactor=2.5399811853d0! unityFactor is the factor to obtein correct unities

    !Hdiff(1,1)=(-sm*unityFactor)/P;  Hdiff(1,2)=0.0d0;  Hdiff(1,3)=0.0d0
    !Hdiff(2,1)=0.0d0;                Hdiff(2,2)=0.0d0;  Hdiff(2,3)=0.0d0
    !Hdiff(3,1)=0.0d0;                Hdiff(3,2)=0.0d0;  Hdiff(3,3)=(aM*unityFactor)/P

    Hdiff(1,1)=0.0d0;   Hdiff(1,2)=0.0d0;               Hdiff(1,3)=0.0d0
    Hdiff(2,1)=0.0d0;   Hdiff(2,2)=(sm*unityFactor)/P;  Hdiff(2,3)=0.0d0
    Hdiff(3,1)=0.0d0;   Hdiff(3,2)=0.0d0;               Hdiff(3,3)=(aM*unityFactor)/P
    return
end subroutine hamiltonianDiff