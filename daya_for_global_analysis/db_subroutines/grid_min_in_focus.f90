!###############################################################
!
!   gridMinInFocus: is a subroutine that make a grid 
!       in sen^2(2*t13) vs Delta_m^2_{ee} 
!       minimizing the pulls in each point over
!       a region close to the CHI minimum, it runs
!       after grid_min_log.
!
!###############################################################
subroutine gridMinInFocus()
    use db_data, only: NDIM,pull_min    
    implicit none    
    integer,parameter :: MP=NDIM+1
    integer,parameter :: NP=NDIM
    real(8) :: Y(12)                     ! Arreglo con los parámetros de oscilación
    real(8) :: t13
    real(8) :: dm31    
    
    real(8) :: m_jump,t_jump

    real(8) :: t_0= 0.1d0
    real(8) :: t_1=0.176870803!0.198849708d0!0.785398163d0
    real(8) :: m_0=0.0022d0
    real(8) :: m_1=0.0029d0 
    
    integer :: i,j
    real(8) :: chi2_min

    real(8) :: ji_min=1e+10,t13_min,mee_min    
    real(8) :: t1,t2
    integer,parameter :: n=60                    ! Number of partitions in the interest interval

        
    print*, 'Daya Bay: making grid min in focus . . . '
    open(104, &
        file='db_data/chiP_spctrm_grid_min_in_focus_1809_9PULL_sigmaFar_glob_bb.dat')
    open(105, &
        file='db_data/chiP_spctrm_grid_min_in_focus_all_info_1809_9PULL_sigmaFar_glob_bb.dat')
    open(106, &
        file='db_data/chiP_spctrm_grid_min_in_focus_min_results_1809_9PULL_sigmaFar_glob_bb.dat')
    
    t_jump = (t_1-t_0)/real(n)    ! For t13
    m_jump = (m_1-m_0)/real(n)    ! 4.0d0 Es el exponente de la potencia de la grafica logaritmica
    
    call cpu_time(t1)
    
    do i=1,n
        dm31=m_0 + m_jump*(i-1)
        do j=1,n
            t13=t_0 + t_jump*(j-1)
            Y=(/0.0d0,   &    !t12
                t13, &    !t13
                0.d0,    &    !t14
                0.0d0,   &    !t23
                0.0d0,   &    !t24
                0.0d0,   &    !t34
                0.0d0,   &    !delta1
                0.0d0,   &    !delta2
                0.0d0,   &    !delta3
                0.0d0,   &    !dm21
                dm31,    &      !dm31
                0.0d0/)       !dm41
            call  chi2_db(Y,chi2_min)
            write(104,*)       (sin(2.0d0*t13))**2, dm31, chi2_min
            write(105,*) j, i, (sin(2.0d0*t13))**2, dm31, chi2_min, pull_min
            if(ji_min.GE.chi2_min) then 
                ji_min  = chi2_min
                t13_min = t13
                mee_min = dm31
             endif
             print*, i,j,'%'
        enddo
        write(104,*)
        write(105,*)
    enddo
    write(106,*) t13_min, sin(2.d0*t13_min)**2, mee_min, ji_min
    call cpu_time(t2)
    print*, 'Tiempo estimado:', t2-t1  

    close(104)
    close(105)
    close(106)    
    call confidenceRegions(n**2)
    return
end subroutine gridMinInFocus