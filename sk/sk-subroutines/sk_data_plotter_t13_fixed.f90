!####################################################################
!
!   subroutine sk_data_plotter_t13_fixed()
!   
!   This subroutine generates a file with the data needed to 
!   make a plotter of the some neutrino oscillation parameter
!   versus its correspondig chi^2. The  generated file depends 
!   on the 'case_plotter' input  as follows:
!   
!   case_plotter = 1  => (delta_CP,chi) - file
!   case_plotter = 2  => (dm_32,chi)    - file
!   case_plotter = 3  => (sin2t13,chi)  - file
!   case_plotter = 4  => (sin2t23,chi)  - file
!
!   data_number = 1   =>  Mixing angle t13 fixed in  Inverted Hierarchy  
!   data_number = 2   =>  Mixing angle t13 fixed in  Normal Hierarchy  
!
!####################################################################
subroutine sk_data_plotter_t13_fixed(case_plotter,data_number)
    use types
    use sk_data, only: skt13fxd2018ih, skt13fxd2018nh
    implicit none
    real(dp) :: delta_CP, dm_32, sin2t13, sin2t23, chi2 ! valores minimios
    real(dp) :: data(26600,5)
    integer  :: i, u, case_plotter,data_number,min_idx(1)
    character(len=20) :: data_file
    
    select case(data_number)
        case(1)            
            data=skt13fxd2018ih
            data_file='skt13fxd2018ih'
        case(2)
            data=skt13fxd2018nh
            data_file='skt13fxd2018nh'
    end select

    chi2 = minval(data(:,5))
    min_idx=minloc(data(:,5))    
    delta_CP = data(min_idx(1),1)
    dm_32    = data(min_idx(1),2)
    sin2t13  = data(min_idx(1),3)
    sin2t23  = data(min_idx(1),4)
    select case(case_plotter)
        case(1) ! Opción para generar datos de la gráfica (delta_CP,chi)
            open(newunit=u, file='sk-data-files/delta_CP_vs_chi_'//trim(data_file)//'.dat')
            do i=1,26600
                if((dm_32==data(i,2)).and.(sin2t13==data(i,3)).and.( sin2t23==data(i,4))) then
                    write(u,*) data(i,1), data(i,5)
                endif
            enddo
            close(u)
        case(2) ! Opción para generar datos de la gráfica (dm_32,chi)
            open(newunit=u, file='sk-data-files/dm_32_vs_chi_'//trim(data_file)//'.dat')
            do i=1,26600
                if((delta_CP==data(i,1)).and.(sin2t13==data(i,3)).and.( sin2t23==data(i,4))) then
                    write(u,*) data(i,2), data(i,5)
                endif
            enddo
            close(u)
        case(3) ! Opción para generar datos de la gráfica (sin2t13,chi)
            open(newunit=u, file='sk-data-files/sin2t13_vs_chi_'//trim(data_file)//'.dat')
            do i=1,26600
                if((delta_CP==data(i,1)).and.(dm_32==data(i,2)).and.( sin2t23==data(i,4))) then
                    write(u,*) data(i,3), data(i,5)
                endif
            enddo
            close(u)            
        case(4) ! Opción para generar datos de la gráfica (sin2t23,chi)
            open(newunit=u, file='sk-data-files/sin2t23_vs_chi_'//trim(data_file)//'.dat')
            do i=1,26600
                if((delta_CP==data(i,1)).and.(dm_32==data(i,2)).and.(sin2t13==data(i,3))) then
                    write(u,*) data(i,4), data(i,5)
                endif
            enddo
            close(u)            
    end select    
end subroutine sk_data_plotter_t13_fixed