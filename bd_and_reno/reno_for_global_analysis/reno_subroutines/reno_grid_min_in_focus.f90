!###############################################################
!
!   reno_gridMinInFocus: is a subroutine that make a grid 
!       in sen^2(2*t13) vs Delta_m^2_{ee} 
!       minimizing the pulls in each point over
!       a region close to the CHI minimum, it runs
!       after grid_min_log.
!
!###############################################################
subroutine reno_gridMinInFocus()
    use reno_data, only: NDIM,pulls
    !use neu_osc_parameters, only: t13,dm31
    implicit none    
    integer,parameter :: MP=NDIM+1
    integer,parameter :: NP=NDIM        ! NP is the number of pulls, MP is the number of points for polygon of ABOEBA
    real(8) :: P(MP,NP), Y(MP)          ! Array con es que se construye AMOEBA    
    real(8) :: Z(12)
    real(8) :: chi2_min

    real(8) :: timeI, timeF
    
    integer :: i,j
    real(8) :: m_jump,t_jump
    real(8) :: t_0= 0.09112756703d0   ! t_0= 0.112756703d0
    real(8) :: t_1=0.23176870803      !0.198849708d0!0.785398163d0
    real(8) :: m_0=0.0019d0,m_1=0.0030d0
    integer :: n=100                   ! Number of partitions in the interest interval

    real(8) :: sen2_2t_13, chi_2
    real(8) :: min_val =1e+10,t13_min,mee_min
    real(8) :: t13
    real(8) :: dm31

    print*, 'making grid min in focus . . . '

    open(104,file='reno_results/grid_min_chi5_in_focus.dat')
    open(105,file='reno_results/grid_min_chi5_in_focus_all_info.dat')
    open(44 ,file='reno_results/grid_min_chi5_in_focus_min_results.dat')

    t_jump = (t_1-t_0)/real(n)    ! For t13
    m_jump = (m_1-m_0)/real(n)    ! 4.0d0 Es el exponente de la potencia de la grafica logaritmica
    call cpu_time(timeI)
    dm31=m_0
    do i=0,n
        t13=t_0
        do j=0,n            
            Z=(/0.0d0,t13,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,dm31,0.0d0/)
            call renoChi2(Z,chi2_min)
            sen2_2t_13 = sin(2.0d0*t13)**2
            write(104,*)       sen2_2t_13, dm31, chi2_min
            if(min_val.GE.chi2_min) then 
                min_val = chi2_min
                t13_min = t13
                mee_min = dm31
             endif
            write(105,*) j, i, sen2_2t_13, dm31, chi2_min, pulls
            t13=t13+t_jump
        enddo
        dm31=dm31+m_jump
        write(104,*)
        write(105,*)
        print*, i,'%'
    enddo
    call cpu_time(timeF)
    print*, 'Time: ', timeF-timeI
    write(44,*) t13_min, sin(2.d0*t13_min)**2, mee_min, min_val
    close(104)
    close(105)
    close(44)    
    call reno_confidenceRegions((n+1)**2)
    call chiDistributionT13(n)
    call chiDistributionDmee(n)
    return
end subroutine reno_gridMinInFocus