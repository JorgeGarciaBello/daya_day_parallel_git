subroutine reno_statistical_analysis_U(name)    
    use data_settings, only: n,t13_M_data,dm_M_data
    use reno_data, only: i_name,j_name
    implicit none
    character(len=200) :: name
    real(8) :: Y(12)
    real(8) :: chi_reno
    real(8) :: RENO_data_g(n,n)
    character(len=200) :: filename,count_bin,count_perc,count_perc1,format_string,format_string_2,format_string_3
    integer  :: i,j

    if (i_name < 10) then
        format_string = "(I1)"
    else
        format_string = "(I2)"
    endif
     if (j_name < 10) then
        format_string_2 = "(I1)"
    else
        format_string_2 = "(I2)"
    endif
    write (count_perc, format_string)  i_name
    write (count_perc1, format_string_2)  j_name    
    filename=trim(name)//'_reactor_'//trim(count_perc1)//'_percentage_'//trim(count_perc)//'.dat'    
    print*,'************     ', filename
    Y=0.0d0
    do i=1,n
        do j=1,n            
            Y(2)=t13_M_data(i,j); Y(11)=dm_M_data(i,j)            
            call renoChi2(Y,chi_reno)                  ! Subroutina que dado Y, regresa el valor de la chi-cuadrada para RENO}           
            RENO_data_g(i,j) = chi_reno
       enddo        
        print*, i
    enddo
    call write_results(n,RENO_data_g,filename)
    call get_min_from_data(n,filename)
    return
end subroutine reno_statistical_analysis_U