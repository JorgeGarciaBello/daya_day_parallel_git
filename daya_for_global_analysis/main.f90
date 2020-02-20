program main_global    
    implicit none
    real(8) :: chi_dayabay
    real(8) :: Y(12)                           !Y=( t12 , t13 , t14 , t23 , t24 , t34 , d13 , d24 , d34 , dm21 , dm31 , dm41 )
    integer :: n=80                            ! Es el tamaño del grid
    real(8) :: dm_min,dm_max
    real(8) :: t13_min,t13_max
    real(8) :: dm_jump,t13_jump
    integer :: i,j

    call readDBData()      ! Lee datos de Dayabay   

    print*,'EStas en el programa de dayabay **+ '    
    Y=0.0

    dm_min=2.3d-3
    dm_max=3.8d-3

    t13_min=0.08d0
    t13_max=0.19d0

    dm_jump=(dm_max-dm_min)/real(n)
    t13_jump=(t13_max-t13_min)/real(n)
    !################################################
    !
    !   En el siguiente proceso de evalua cada punto 
    !   del grid en la chi de Daya Bay, se minimiza la 
    !   estadística internamente y como resultado final 
    !   se entrega el valor minimo de chi => chi_dayabay
    !
    !##################################################
    
    open(337,file='db_data.dat')   
    do i=1,n
        Y(2)=t13_min + t13_jump*(i-1)
        do j=1,n
            Y(11)=dm_min + dm_jump*(j-1)            
            call chi2_db(Y,chi_dayabay)      ! Subroutina que dado Y, regresa el valor de la chi-cuadrada para Daya Bay
            write (337,*)  sin(2.0d0*Y(2))**2, Y(11), chi_dayabay ! Escrotura de los datos del análisis db            
        enddo
        print*,i
        write(337,*)
    enddo    
    close(337)
    call get_chi_min_from_data_grid(n)    
    call confidenceRegions(n)
    !call db_chiDistributionT13(200)
    !call db_chiDistributionDmee(200)
end program main_global