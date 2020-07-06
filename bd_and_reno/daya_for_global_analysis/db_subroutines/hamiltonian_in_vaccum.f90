subroutine hamiltonianInVaccum(Hm,sm,aM,P)
    implicit none
    real(8) :: Hm(3,3)            ! Hm is the Hamiltonian in vaccum
    real(8) :: sm,aM              ! sm,aM are the squared mass difference m=m_21 y M=m_32
    real(8) :: P                  ! P es el momento del neutrino o la enerǵia total    

    real(8) :: Hdiff(3,3)  ! Hdiff is the hamiltonian differential
    real(8) :: Hdiag(3,3)  ! Hdiag is the hamiltonian differential
    
    call hamiltonianDiff(Hdiff,sm,aM,P)
    call hamiltonianDiag(Hdiag)
    Hm=Hdiff+Hdiag    
    return
end subroutine hamiltonianInVaccum