subroutine sk_data_plot_sk_db_reno(case_plotter)
    use types
    use sk_data, only: sknhji,dbdm32s2t13s2t23prueba3D,renodm32s2t13s2t23prueba3DRENO
    implicit none
    real(dp) :: chi2, delta_CP, dm_32, sin2t13, sin2t23
    integer  :: i,j,u,n=44800
    integer :: case_plotter,  min_idx(1)
    real(dp) :: data(44800,4)
    real(dp) :: db_min, reno_min
    character(len=20) :: data_file    
    
    select case(2)
        case(1)            
            data_file='ih'
        case(2)            
            data_file='nh'      
    end select

    data = sknhji
    do i=1,n
        data(i,4) = sknhji(i,4) + dbdm32s2t13s2t23prueba3D(i,4) + renodm32s2t13s2t23prueba3DRENO(i,4)
    enddo
    chi2 = minval(data(:,4))
    data(:,4)=data(:,4)-chi2

    open(newunit=u, file='sk-db-reno-ji.dat')
        do i=1,n
            write(u,*) data(i,:)
        enddo
    close(u)
   
    min_idx  = minloc(data(:,4))    
    dm_32    = data(min_idx(1),1)    
    sin2t13  = data(min_idx(1),2)
    sin2t23  = data(min_idx(1),3)    

    select case(case_plotter)    
        case(1) ! Opción para generar datos de la gráfica (dm_32,chi)
            open(newunit=u, file='sk-data-files/sdr_dm_32_vs_chi_'//trim(data_file)//'.dat')
            do i=1,n
                if((sin2t13==data(i,2)).and.( sin2t23==data(i,3))) then
                    write(u,*) data(i,1), data(i,4)
                endif
            enddo
            close(u)
        case(2) ! Opción para generar datos de la gráfica (sin2t13,chi)
            open(newunit=u, file='sk-data-files/sdr_sin2t13_vs_chi_'//trim(data_file)//'.dat')
            do i=1,n
                if(   ( dm_32 == data(i,1) ).and.( sin2t23 == data(i,3) )  ) then
                    write(u,*) data(i,2), data(i,4)                    
                endif
            enddo
            close(u)            
        case(3) ! Opción para generar datos de la gráfica (sin2t23,chi)
            open(newunit=u, file='sk-data-files/sdr_sin2t23_vs_chi_'//trim(data_file)//'.dat')
            do i=1,n
                if((dm_32==data(i,1)).and.(sin2t13==data(i,2))) then
                    write(u,*) data(i,3), data(i,4)
                endif
            enddo
            close(u)
    end select
    return
end subroutine sk_data_plot_sk_db_reno