subroutine hallTest()
    use neu_osc_parameters
    use db_data, only: NBIN,farObs,farExp
    implicit none
    real(8), parameter :: PI=3.141592653589793238462643d0    

    real(8) :: expectedNumberNeutrinosDetector
    real(8) :: expectedNeutrinoSpectrum_H
    real(8) :: expectedNumberNeutrinosHall
    real(8) :: model
    integer :: num

    real(8) :: x    
    real(8) :: L                  ! L is the length between the source of neutrinos an the position
    real(8) :: P                  ! P es el momento del neutrino o la enerǵia total

    !real(8) :: m21=7.53D-5, m32= 2.44D-3 !Jerarquia normal ! m32= 2.52E-3 !Jerarquia invertida
    !real(8) :: s2_2t12=0.846D0, c2_t12=0.6962D0, s2_t12=0.3038D0

    real(8) :: expectedNeutrinoSpectrumByBinFar
    real(8) :: expectedNumberNeutrinoByBinFar
    integer :: d
    integer :: i
    real(8) :: corrections(NBIN)

    print*, ' In hallTest'

    t12=0.0d0!0.5837630475918688D0!PI/4.0D0
    t13=0.14795088706d0!0.13023999999999999d0!
    t23=0.0d0!0.7853981631d0!0.698356658079 !0.0D0!PI/
    t14=0.0D0
    t24=0.0D0
    t34=0.0D0    
    delta1=0.0D0
    delta2=0.0D0
    delta3=0.0D0
    dm21=0.0d0!7.53D-5
    dm31=2.5D-3!2.8560000000000000d-003!2.5D-3
    dm41=0.0d0

  
    do i=1,NBIN
        !print*, farObs(i), farExp(i), model(i), model(i)*farExp(i)
        !print*, expectedNumberNeutrinoByBinFar(i)!,farObs(i)!, farExp(i),
        print*, expectedNeutrinoSpectrumByBinFar(i)!, farExp(i)
    enddo

    !print*,'farObs(i),farExp(i)*model(i)'
    !do i=1,NBIN
    !    print*,farObs(i),farExp(i)*model(i),farObs(i)/(farExp(i)*model(i)),farObs(i)-(farExp(i)*model(i)),&
    !    (farObs(i)-(farExp(i)*model(i)))**2/farObs(i)
    !enddo

    !do i=1,NBIN
    !print*,(farObs(i)-(farExp(i)*model(i)))**2
    !enddo

    
    
    !print *,''
    !do d=1,ADS
    !print*, expectedNumberNeutrinosDetector(d)
    !enddo
    
    return
end subroutine hallTest