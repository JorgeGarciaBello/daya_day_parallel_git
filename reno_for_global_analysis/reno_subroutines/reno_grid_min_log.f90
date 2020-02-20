!#####################################################
!
!   gridMinLog: is a subroutine that make a grid 
!       in sen^2(2*t13) vs Delta_m^2_{ee} 
!       minimizing the pulls in each point over
!       the plane.
!
!#####################################################
subroutine gridMinLog()
    use reno_data, only: NDIM,pulls    
    implicit none    
    integer,parameter :: MP=NDIM+1                    ! MP is the number of points for polygon of ABOEBA
    integer,parameter :: NP=NDIM                      ! NP is the number of pulls
    real(8) :: P(MP,NP)

    real(8) :: t13
    real(8) :: dm31


    real(8) :: sen2_2t_13
    real(8) :: m_jump,t_jump
    real(8) :: t_0=0.0d0
    real(8) :: t_1=0.785398163d0
    real(8) :: exp_m_0=-4.d0
    real(8) :: exp_m_1=0.d0
    
    integer :: n=50                       ! Number of partitions in the interest interval    
    integer :: i,j   

    real(8) :: chi_min=1e+10
    real(8) :: t13_min
    real(8) :: mee_min
    real(8) :: chi2_min
    real(8) :: Z(12)
    real(8) :: time_0, time_1

    print*, 'making grid min log . . . '
    open(101,file='statistical_analysis/grid_min/results/grid_min_log_data_ji_6.dat')
    open(103,file='statistical_analysis/grid_min/results/grid_min_log_all_info_ji_6.dat')
    open(45, file='statistical_analysis/grid_min/results/grid_min_log_min_results_ji_6.dat')

    call cpu_time(time_0)
    t_jump = (t_1-t_0)/real(n)              ! For t13
    m_jump = (exp_m_1-exp_m_0)/real(n)      ! 4.0d0 Es el exponente de la potencia de la grafica logaritmica
    do i=0,n        
        dm31=exp(2.302585d0*(exp_m_0 + m_jump*real(i)))
        t13=0.0d0
        do j=0,n
            Z=(/0.0d0,t13,0.0d0,0.0d0,0.0d0,0.0d00,  &
                0d0,0.0d0,0.0d0,                     &
                0.0d0,dm31,0.0d0/)
            call renoChi2(Z,chi2_min)
            sen2_2t_13 = sin(2.0d0*t13)**2
            write(101,*)       sen2_2t_13, dm31, chi2_min
            write(103,*) j, i, sen2_2t_13, dm31, chi2_min, pulls


            if(chi_min.GE.chi2_min) then 
                chi_min = chi2_min
                t13_min = t13
                mee_min = dm31
            endif
            t13=t13+t_jump
            print*, i,j,'%'
        enddo
        write(101,*)
        write(103,*)
        print*, i,'%'
    enddo

    write(45,*) t13_min, sin(2.d0*t13_min)**2,mee_min,chi_min

    close(101)
    close(103)
    close(45)
    call cpu_time(time_1)
    print*, 'Time', time_1-time_0
    return
end subroutine gridMinLog