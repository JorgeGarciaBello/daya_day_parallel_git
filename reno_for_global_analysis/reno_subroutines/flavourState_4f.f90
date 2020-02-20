subroutine flavourState_4f(flvr,fVector)
    implicit none
    integer :: flvr
    real(8) :: fVector(4)

    select case(flvr)
     case(1)
        fVector=(/1.0d0,0.0d0,0.0d0,0.0d0/)
     case(2)
        fVector=(/0.0d0,1.0d0,0.0d0,0.0d0/)
     case(3)
        fVector=(/0.0d0,0.0d0,1.0d0,0.0d0/)
     case(4)
        fVector=(/0.0d0,0.0d0,0.0d0,1.0d0/)
     case DEFAULT
        print*, 'Error: No existe tal estado de sabor'
        print*, flvr
    end select

    return
end subroutine flavourState_4f