subroutine reno_thermal_power_per_reactor_test()
    use types
    use data_settings, only: n,t13_M_data,dm_M_data
    use reno_data, only: i_name,j_name,RCTS
    implicit none
    integer  :: i,j,u,r
    real(dp) :: values(RCTS)
    real(dp) :: percentage(10)
    character(len=200) :: filename='reno_thermal_power_per_reactor_test'

    open(newunit=u,file='reno_for_global_analysis/reno_data/reno_thermal_power_by_reactor.dat', status='old')
        read(u,*) values
    close(u)

    percentage=(/0.7d0,0.8d0,0.9d0,0.95d0,0.99d0,1.01d0,1.05d0,1.10d0,1.20d0,1.30d0/)

    do r=1,RCTS
        j_name=r
        do i=1,10
            i_name=i
            values(r)=percentage(i)*values(r)
            call reno_statistical_analysis_U(filename)
            values=2.73d0
            open(newunit=u,file='reno_for_global_analysis/reno_data/reno_thermal_power_by_reactor.dat', status='old')
                read(u,*) values
            close(u)
        enddo
    enddo
    return
end subroutine reno_thermal_power_per_reactor_test