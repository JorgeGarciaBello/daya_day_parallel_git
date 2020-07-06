subroutine hamiltonianInVaccum_4f(Hm,dm21,dm31,dm41,P)
    implicit none
    real(8) :: Hm(4,4)            ! Hm is the Hamiltonian in vaccum
    real(8) :: dm21,dm31,dm41     ! dm21,dm31,dm41 are the squared mass differences
    real(8) :: P                  ! P es el momento del neutrino o la enerǵia total    

    real(8) :: Hdiff(4,4)  ! Hdiff is the hamiltonian differential
    real(8) :: Hdiag(4,4)  ! Hdiag is the hamiltonian differential
    
    call hamiltonianDiff_4f(Hdiff,dm21,dm31,dm41,P)
    call hamiltonianDiag_4f(Hdiag)
    Hm=Hdiff+Hdiag
    return
end subroutine hamiltonianInVaccum_4f