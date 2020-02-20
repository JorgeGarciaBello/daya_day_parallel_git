!#####################################################
!
!   gridMinLog: is a subroutine that make a grid 
!       in sen^2(2*t13) vs Delta_m^2_{ee} 
!       minimizing the pulls in each point over
!       the plane.
!
!#####################################################
subroutine gridMinLog()
    use db_data, only: NDIM,pull_min
    implicit none    
    integer,parameter :: MP=NDIM+1       ! MP is the number of points for polygon of ABOEBA
    integer,parameter :: NP=NDIM         ! NP is the number of pulls    
    real(8) :: Y(12)                     ! Arreglo con los parámetros de oscilación
    real(8) :: t13
    real(8) :: dm31
    
    real(8) :: m_jump,t_jump
    
    real(8) :: t_0=0.1d0
    real(8) :: t_1=0.3d0!785398163d0
    real(8) :: exp_m_0=-3.0d0!4.d0
    real(8) :: exp_m_1=-1.d0

    integer :: i,j
    real(8) :: chi2_min
    
    real(8) :: ji_min=1e+10,t13_min,mee_min    
    real(8) :: t1,t2    
    integer,parameter :: n=50                  ! Number of partitions in the interest interval    

    print*, 'Daya Bay: making grid min log . . . '
    !open(101,file='db_data/chi2_grd3_grid_min_log_1809.dat')
    open(101,file='db_data/chi2_prop_log_9PULL_sigmaF_Rtrn.dat')
    open(102, file='db_data/chi2_prop_log_min_results_9PULL_sigmaF_Rtrn.dat')
    open(103, file='db_data/chi2_prop_log_all_info_9PULL_sigmaF_Rtrn.dat')

    t_jump = (t_1-t_0)/real(n)              ! For t13
    m_jump = (exp_m_1-exp_m_0)/real(n)      ! 4.0d0 Es el exponente de la potencia de la grafica logaritmica

    call cpu_time(t1)

    do i=1,n   
        dm31=exp(2.302585d0*(exp_m_0 + m_jump*real(i-1)))        
        do j=1,n
            t13=t_jump*(j)

            Y=(/0.0d0,   &    !t12
                t13, &        !t13
                0.d0,    &    !t14
                0.0d0,   &    !t23
                0.0d0,   &    !t24
                0.0d0,   &    !t34
                0.0d0,   &    !delta1
                0.0d0,   &    !delta2
                0.0d0,   &    !delta3
                0.0d0,   &    !dm21
                dm31,    &    !dm31
                0.0d0/)       !dm41
            call  chi2_db(Y,chi2_min)
            write(101,*) (sin(2.0d0*t13))**2,dm31,chi2_min
            write(103,*) (sin(2.0d0*t13))**2,dm31,chi2_min,pull_min
            if(ji_min.GE.chi2_min) then 
                ji_min  = chi2_min
                t13_min = t13
                mee_min = dm31
            endif
            print*, i,j
        enddo        
        write(101,*)
        print*, i,'%'
    enddo   
    write(102,*) t13_min, sin(2.d0*t13_min)**2,mee_min,ji_min
    close(101)
    close(102)
    close(103)
    call cpu_time(t2)
    print*,'time; ',t2-t1
    return
end subroutine gridMinLog