program main_global
    use data_settings, only: n,t13_M_data,dm_M_data    
    implicit none
    real(8) :: chi_dayabay, chi_reno, chi_doubleCHOOZ, chi_min_global
    real(8) :: Y(12)                                          !Y=( t12 , t13 , t14 , t23 , t24 , t34 , d13 , d24 , d34 , dm21 , dm31 , dm41 )    
    real(8) :: dm_min,dm_max    
    real(8) :: db_chi_square_spectral_analysis2_period
    real(8) :: DC_FUNC
    real(8) :: RENO_data(n,n),DC_data(n,n),dmee,t13,P(11)
    real(8) :: reno_FUNC,hola
    real(8) :: reno_chi_square_spectral_analysis
    real(8) :: val(3),data(n**2,3)
    integer :: i,j,u   
    

    call grid_setting()
    call readDBData()      ! Lee datos de Dayabay
    call readRENOData()    ! Lee datos de RENO
    CALL ReadDC()          ! Lee datos de Double CHOOZ
    call DC_read_data()

    call DC_test()    
    stop
    !call reno_generate_MC()
    !dmee=0.0d0
    !t13=0.0d0    
    
    Y=0.0
    !open(336,file='global_data.dat')
    !open(337,file='db_data.dat')
    !open(338,file='reno_data_cal.dat')
    open(339,file='DC_data.dat')
    !print*, 'inicia análisis'
    do i=1,n
        do j=1,n            
          Y(2)=t13_M_data(i,j); Y(11)=dm_M_data(i,j)
            !Y(2)=asin(sqrt(8.9609538498276506E-002))/2.0d0;   Y(11)=2.6849999999999999E-003               
            !call chi2_D_C(Y,chi_doubleCHOOZ) ! Subroutina que dado Y, regresa el valor de la chi-cuadrada para Double CHOOZ                
            chi_doubleCHOOZ=DC_FUNC(Y(2),Y(11))
            DC_data(i,j)=chi_doubleCHOOZ

            !resul_M_DB(i,j)  = db_chi_square_spectral_analysis2_period(t13_M_data(i,j),dm_M_data(i,j))            
            !call renoChi2(Y,chi_reno)        ! Subroutina que dado Y, regresa el valor de la chi-cuadrada para RENO}           
            !chi_reno=reno_chi_square_spectral_analysis(Y(2),Y(11))
            
            !RENO_data(i,j) = chi_reno
    !        RENO_data = chi_reno

                !chi_min_global=chi_doubleCHOOZ+chi_reno+resul_M_DB(i,j)
                !write (336,*)  sin(2.0d0*Y(2))**2, Y(11), chi_min_global ! Escrotura de los datos del análisis global
                !write (337,*)  sin(2.0d0*Y(2))**2, Y(11), resul_M_DB(i,j) ! Escrotura de los datos del análisis db                
                write (339,*)  sin(2.0d0*Y(2))**2, Y(11), chi_doubleCHOOZ ! Escrotura de los datos del análisis dc
                !write (338,*)  sin(2.0d0*Y(2))**2, Y(11), chi_reno ! Escrotura de los datos del análisis reno                

       enddo
        !write (336,*) ' '
        !write (337,*) ' '
        !write (338,*)  ' '
        write (339,*)  ' '
        print*, i
    enddo
    !close(336)
    !close(337)
    !close(338)
    close(339)
    
    call write_results(n,DC_data,'DC_data.dat')
    call get_min_from_data(n,'DC_data.dat',val)
    call get_parabola_from_data(n,DC_data,'DC_data.dat')
    call reno_confidenceRegions(n,val,DC_data,'DC_data.dat')


    !call write_results(n,RENO_data,'reno_data_cal.dat')
    !call get_min_from_data(n,'reno_data_cal.dat',val)
    !call get_parabola_from_data(n,data,'reno_data_cal.dat')
    !call reno_confidenceRegions(n,val,RENO_data,'reno_data_cal.dat')    
end program main_global