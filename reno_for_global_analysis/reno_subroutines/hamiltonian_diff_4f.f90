!################################################
!
!   hamiltonianDiff_4f: is a subroutine that 
!       load the hamiltonian-diff-matrix 
!       in a 4-framework
!
!################################################
subroutine hamiltonianDiff_4f(Hdiff,dm21,dm31,dm41,P)
    implicit none
    real(8) :: Hdiff(4,4)         ! Hdiff is the hamiltonian differential 
    real(8) :: dm21,dm31,dm41     ! dm21,dm31,dm41 are the squared mass differences
    real(8) :: P                  ! P es el momento del neutrino o la enerǵia total
    real(8),parameter :: uFac=2.5399811853d0! uFac is the factor to obtein correct unities

    Hdiff(1,1)=0.0d0; Hdiff(1,2)=0.0d0;         Hdiff(1,3)=0.0d0;         Hdiff(1,4)=0.0d0
    Hdiff(2,1)=0.0d0; Hdiff(2,2)=(dm21*uFac)/P; Hdiff(2,3)=0.0d0;         Hdiff(2,4)=0.0d0
    Hdiff(3,1)=0.0d0; Hdiff(3,2)=0.0d0;         Hdiff(3,3)=(dm31*uFac)/P; Hdiff(3,4)=0.0d0
    Hdiff(4,1)=0.0d0; Hdiff(4,2)=0.0d0;         Hdiff(4,3)=0.0d0;         Hdiff(4,4)=(dm41*uFac)/P
    return
end subroutine hamiltonianDiff_4f