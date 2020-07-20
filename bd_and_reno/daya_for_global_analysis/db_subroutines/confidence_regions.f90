!#####################################################
!
!   confidenceRegions: es una subroutina que 
!       genera los archivos de datos para las 
!       crear las regiones de confianza
!
!#####################################################
subroutine confidenceRegions(dim_grid)
    implicit none
    integer :: dim_grid

    real(8) :: min_values(4)
    real(8) :: min_val    
    real(8) :: data(dim_grid,3)
    integer :: i,j
    real(8) :: s13,m_ee,chi_2
    
    print*, 'Daya Bay: making confidence regions . . . '
    open(107,                                                                                      &
         file='db_data/chiP_spctrm_grid_min_in_focus_min_results_1809_9PULL_sigmaFar_glob_bb.dat', &
         status='old')
        read(107,*) min_values
    close(107)


    min_val=min_values(4)
    open(106,file='db_data/chiP_spctrm_grid_min_in_focus_1809_9PULL_sigmaFar_glob_bb.dat', &
        status='old')
        read(106,*) ( (data(i,j), j=1,3), i=1,dim_grid )
    close(106)

    open(41,file='db_data/crP_sFar_db_99_73_2018_bb_and_asimovD.dat')
    open(42,file='db_data/crP_sFar_db_95_45_2018_bb_and_asimovD.dat')
    open(43,file='db_data/crP_sFar_db_68_27_2018_bb_and_asimovD.dat')

    do i=1,dim_grid
        s13=data(i,1); m_ee=data(i,2); chi_2=data(i,3)-min_val
        !Confidence region 1 - 68.27% - 2.30 - (1-sigma)
        if((2.45d0.GE.chi_2).AND.(chi_2.GE.2.15d0)) then
            write(41,*) s13, m_ee
        endif
        !Confidence region 2 - 95.45% - 6.18 - (2-sigma)
        if((6.33d0.GE.chi_2).AND.(chi_2.GE.6.03d0)) then
            write(42,*) s13, m_ee
        endif
        !Confidence region 3 - 99.73% - 11.83 (3-sigma)
        if((11.98d0.GE.chi_2).AND.(chi_2.GE.11.68d0)) then
            write(43,*) s13, m_ee
        endif
    enddo
    close(41)
    close(42)
    close(43)
    return
end subroutine confidenceRegions