subroutine neutrinoOscillationInVaccum()
    implicit none
    real(8), parameter :: PI=3.141592653589793238462643d0
    real(8) :: t12,t13,t23,t14,t24,t34! Are the mixing angles
    real(8) :: delta1,delta2,delta3              ! delta is the CP violation phase factor
    real(8) :: sm,aM              ! sm,aM are the squared mass difference m=m_21 y M=m_32
    real(8) :: dm21, dm31, dm41
    double complex :: U13(3,3)
    real(8) :: U23(3,3),U12(3,3)
    double complex :: U(3,3)
    double complex :: U_1(3,3)    ! Inverse mixing matrix
    double complex :: U_4f(4,4)
    double complex :: U_4f_1(4,4)
    real(8) :: Hm_4f(4,4)
    double complex ::Hm_4f_1(4,4)
    real(8) :: Hm(3,3)            ! Hm is the Hamiltonian in vaccum
    double complex ::Hm_1(3,3)
    real(8) :: L                  ! L is the length between the source of neutrinos an the position    
    real(8) :: P                  ! P es el momento del neutrino o la enerǵia total

    double complex :: temporalflavourVector(3)! temporalflavourVector is the vector of the flavour state
    double complex :: amplitudeOfTransition
    real(8) :: probabilityOfTransition
    real(8) :: probabilityOfTransition_4f
    real(8) :: probability_of_transition_2_flavour
    real(8) :: angle
    integer ::i
    
    !open(22, file='oscillation_model/neutrino_oscillation_in_vacumm/results/_probabilityOfTransition2Frame.dat')

    print*, 'Estas en mixing matrix test'

    t12=PI/4.0D0
    t13=PI/4.0D0 !0.0D0!PI/
    t23=PI/4.0d0 !0.0D0!PI/
    t14=0.0d0
    t24=0.0d0
    t34=0.0d0
    delta1=0.0
    delta2=0.0
    delta3=0.0
    sm=1E-4
    aM=1E-3 !1.0e-3
    dm21=1E-4
    dm31=1E-3
    dm41=0.0d0
    L=1.0D0                        ! Units in [Km]
    P=1.0D0                        ! Units in [GeV]

    !print*, 'Hamiltoniano'
    !call hamiltonianInVaccum(Hm,sm,aM,P)
    !print*, Hm(1,:)
    !print*, Hm(2,:)
    !print*, Hm(3,:)
    !print*,'U'
    !call mixingMatrix(U,t12,t13,t23,delta1)
    !print*, U(1,:)
    !print*, U(2,:)
    !print*, U(3,:)
    !print*,''
    !call mixingMatrix_4f(U_4f,t12,t13,t14,t23,t24,t34,delta1,delta2,delta3)
    !print*, U_4f(1,:)
    !print*, U_4f(2,:)
    !print*, U_4f(3,:)
    !print*, U_4f(4,:)
    !print*,''



    !print*,''
    !call inverseMatrix(U,U_1)
    !print*, U_1(1,:)
    !print*, U_1(2,:)
    !print*, U_1(3,:)
    !call hamiltonianInVaccum_4f(Hm_4f,dm21,dm31,dm41,P)
    !print*,Hm_4f(1,:)
    !print*,Hm_4f(2,:)
    !print*,Hm_4f(3,:)
    !print*,Hm_4f(4,:)
    !print*,''
    !print*,''
    !print*,''

    
    !call inverseMatrix_4_4(U_4f,U_4f_1)
    !print*,U_4f_1(1,:)
    !print*,U_4f_1(2,:)
    !print*,U_4f_1(3,:)
    !print*,U_4f_1(4,:)

    !call hamiltonianInVaccum(Hm,sm,aM,P)
    !print*,Hm(1,:)
    !print*,Hm(2,:)
    !print*,Hm(3,:)
    !print*,''

        
    !open(21, file='oscillation_model/neutrino_oscillation_in_vacumm/results/test_probabilityOfTransition3Frame.dat')
    !open(22, file='oscillation_model/neutrino_oscillation_in_vacumm/results/test_probabilityOfTransition4Frame.dat')
    !do i=1,10000
    !    write(21,*) L/P,  probabilityOfTransition(1,2,t12,t13,t23,delta1,sm,aM,L,P)
    !    write(22,*) L/P,  probabilityOfTransition_4f(1,2,t12,t13,t14,t23,t24,t34,delta1,delta2,delta3, &
    !                                                    dm21,dm31,dm41,L,P)
        !write(22,*), (L/P),  probability_of_transition_2_flavour(angle,sm,L,P)
    !    L=L + 9.9999d0
    !enddo
    !close(21)    
    !close(22)



end subroutine neutrinoOscillationInVaccum