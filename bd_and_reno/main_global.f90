program main_global
    use data_settings, only: n,t13_M_data,dm_M_data
    implicit none
    real(8) :: chi_dayabay, chi_reno, chi_doubleCHOOZ, chi_min_global
    real(8) :: Y(12)                                          !Y=( t12 , t13 , t14 , t23 , t24 , t34 , d13 , d24 , d34 , dm21 , dm31 , dm41 )    
    real(8) :: dm_min,dm_max    
    real(8) :: db_chi_square_spectral_analysis2_period
    real(8) :: RENO_data(n,n),dmee,t13,P(11)
    real(8) :: reno_FUNC,hola
    real(8) :: val(3),data(n**2,3)
    integer :: i,j,u


    call grid_setting()
    call readDBData()      ! Lee datos de Dayabay
    call readRENOData()    ! Lee datos de RENO
    CALL ReadDC()          ! Lee datos de Double CHOOZ

    call reno_test()

end program main_global