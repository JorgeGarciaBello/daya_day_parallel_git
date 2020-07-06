SUBROUTINE get_min_from_data(m,filename,val)
    implicit none
    integer :: m
    Character(len=*) :: filename
    real(8) :: val(3)
    integer :: n
    real(8) :: data(m**2,3)

    real(8) :: ARRAY(m**2),RESULT(3)
    integer :: i,j,u

    n=m**2
    open(newunit=u,file='data/'//filename,status='old')
        read(u,*) ((data(i,j),j=1,3),i=1,n)
    close(u)

    ARRAY=data(:,3)
    RESULT(1) = MINVAL(ARRAY)
    print*, RESULT(1)

    do i=1,n
        if (data(i,3).eq.RESULT(1)) then
            RESULT(2)=data(i,1)
            RESULT(3)=data(i,2)
        end if
    enddo
    open(newunit=u,file='data/min_param_'//filename)
        write(u,*) RESULT
    close(u)
    val=RESULT

    return
end SUBROUTINE get_min_from_data