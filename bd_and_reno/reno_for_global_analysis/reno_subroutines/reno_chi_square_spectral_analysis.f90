function reno_chi_square_spectral_analysis(t13,dmee)
    use types
    use reno_data, only: NBIN,num_experiments
    implicit none
    real(dp) :: reno_chi_square_spectral_analysis
    real(dp) :: t13,dmee    
    real(dp) :: V(NBIN,NBIN),Ubar(NBIN)    
    real(dp) :: chi, chi_m(NBIN,NBIN)    
    character(len=200) :: filename,count_bin,count_perc,format_string,format_string_2
    integer :: i,j,u
    real    :: t1,t2
    
    call reno_create_M_stat_signal_bkg(t13,dmee,Ubar,V)
    !filename='db_ji_vs_bines.dat'
    chi=0.0d0
    do i=1, NBIN                                            ! (# de biines)*(# de combinaciones FAR/NEAR)*(# de periodos de colecta de datos)        
        do j=1,NBIN                                         ! (# de biines)*(# de combinaciones FAR/NEAR)*(# de periodos de colecta de datos)                
                chi_m(i,j)=Ubar(j)*V(i,j)*Ubar(i)
                chi=chi+chi_m(i,j)
        enddo
    enddo
    !open(newunit=u, file='db_data/'//filename)
    !do i=1, NBIN*2*PD
    !    write(u,*) i,chi_m(i,i)
    !enddo
    !close(u)
    reno_chi_square_spectral_analysis=chi
    return
end function reno_chi_square_spectral_analysis