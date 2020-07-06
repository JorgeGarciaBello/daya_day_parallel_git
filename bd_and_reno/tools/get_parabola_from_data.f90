subroutine get_parabola_from_data(n,data,filename)
    use types
    implicit none
    integer :: n
    real(dp) :: data(n**2,3)
    character(len=13) :: filename
    real(dp) :: RESULT(3)
    integer :: i,j,u

    filename=trim(filename)

    open(newunit=u,file=filename,status='old')
        read(u,*) ((data(i,j), j=1,3), i=1,n**2)
    close(u)

    
    RESULT(1) = MINVAL(data(:,3))
    do i=1,n**2
        if (data(i,3).eq.RESULT(1)) then
            RESULT(2)=data(i,1)
            RESULT(3)=data(i,2)
        end if
    enddo
    open(newunit=u,file=filename,status='old')
        read(u,*) ((data(i,j), j=1,3), i=1,n**2)
    close(u)
    open(newunit=u, file="data/parabola_dmee_"//filename)
        write(u,*) ' '
    close(u)
    open(newunit=u, file="data/parabola_s22t13_"//filename)
        write(u,*) ' '
    close(u)
    do i=1,n**2
        if (data(i,1).eq.RESULT(2)) then            
            open(newunit=u, file="data/parabola_dmee_"//filename, position="append", status="old")
                write(u, *) data(i,2), data(i,3)-RESULT(1)
            close(u)
        end if
        if (data(i,2).eq.RESULT(3)) then            
            open(newunit=u, file="data/parabola_s22t13_"//filename, position="append", status="old")
                write(u, *) data(i,1), data(i,3)-RESULT(1)
            close(u)
        end if
    enddo
    return
end subroutine get_parabola_from_data