subroutine grid_setting()
    implicit none
    real(8) :: dm_min,dm_max
    real(8) :: t13_min,t13_max
    print*, 'Making data-grid  . . .'
    !################################################
    !
    !   Inicialización de los limites del grid 
    !
    !##################################################
    !dm_min=2.15d-3           ! 2.2d-3
    !dm_max=3.25d-3           ! 3.d-3
    !t13_min=asin(sqrt(0.06d0))/2.0d0               !0.08d0
    !t13_max=asin(sqrt(0.124d0))/2.0d0               !0.19d0        

    dm_min=2.15d-3           ! 2.2d-3
    dm_max=3.6d-3           ! 3.d-3
    t13_min=asin(sqrt(0.045d0))/2.0d0               !0.08d0
    t13_max=asin(sqrt(0.124d0))/2.0d0               !0.19d0        

    call create_grid(t13_min,t13_max,dm_min,dm_max)
    call read_grid()    
    return
end subroutine grid_setting