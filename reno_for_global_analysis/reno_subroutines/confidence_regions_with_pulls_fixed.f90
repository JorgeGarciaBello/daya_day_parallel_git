!#####################################################
!
!   confidenceRegionsWithPullsFixed: es una subroutina que 
!       genera los archivos de datos para las 
!       crear las regiones de confianza
!
!#####################################################
subroutine confidenceRegionsWithPullsFixed()
    use neu_osc_parameters
    implicit none

    real(8) :: pulls(4)
    real(8) :: min_values(4)
    real(8) :: min_val
    integer :: i,j
    real(8) ::chi_2
    real(8) :: s13
    real(8) :: m_jump,t_jump    
    real(8) :: t_0= 0.08112756703d0! t_0= 0.112756703d0
    real(8) :: t_1=0.25176870803   !0.198849708d0!0.785398163d0
    real(8) :: m_0=0.001d0
    real(8) :: m_1=0.004d0
    integer :: n=500              ! Number of partitions in the interest interval

    real(8) :: FUNC
    
    print*, 'making confidence regions with confidenceRegionsWithPullsFixed() . . . '
    ! Reading the pulls belonging with the minimum  value for Chi-Square
    pulls=(/7.2562141432712195d-005,-9.4862977314653333d-005,7.1118465862495599d-005,2.5057164246061847d-005/)
    open(107,file='statistical_analysis/grid_min/results/new_b_fine_bkg_grid_min_in_focus_min_results.dat',status='old')
        read(107,*) min_values
    close(107)
    print*,'min_values',min_values(4)    
    min_val=min_values(4)
    

    t_jump = (t_1-t_0)/real(n)    ! For t13
    m_jump = (m_1-m_0)/real(n)    ! 4.0d0 Es el exponente de la potencia de la grafica logaritmica
    open(191,file='statistical_analysis/confidence_regions/results/pulls_fixed_b_fine_bkg_cr_99_73.dat')
    open(192,file='statistical_analysis/confidence_regions/results/pulls_fixed_b_fine_bkg_cr_95.45.dat')
    open(193,file='statistical_analysis/confidence_regions/results/pulls_fixed_b_fine_bkg_cr_68_27.dat')
    dm31=m_0
    do i=0,n
        t13=t_0
        do j=0,n
            s13 = sin(2.0d0*t13)**2
            chi_2 = FUNC(pulls)-min_val
                !Confidence region 1 - 68.27% - 2.30 - (1-sigma)
                if((2.45d0.GE.chi_2).AND.(chi_2.GE.2.15d0)) then
                    write(191,*) s13, dm31
                    print*,chi_2
                endif
                !Confidence region 2 - 95.45% - 6.18 - (2-sigma)
                if((6.33d0.GE.chi_2).AND.(chi_2.GE.6.03d0)) then
                    write(192,*) s13, dm31
                    print*,chi_2
                endif
                !Confidence region 3 - 99.73% - 11.83 (3-sigma)
                if((11.98d0.GE.chi_2).AND.(chi_2.GE.11.68d0)) then
                    write(193,*) s13, dm31
                    print*,chi_2
                endif            
            t13=t13+t_jump
        enddo
        dm31=dm31+m_jump
        print*,i,'%'
    enddo
    close(191)
    close(192)
    close(193)
    return
end subroutine confidenceRegionsWithPullsFixed