subroutine DBDetectorsTest()
    use db_data 
    use neu_osc_parameters
    implicit none
    real(8) :: Y(12)              ! Arreglo con los parámetros de oscilación
    integer :: d
    real(8) :: expectedNumberNeutrinosDetector
    Y=(/0.0d0,   &    !t12
        0.1471134189d0, &    !t13
        0.d0,    &    !t14
        0.0d0,   &    !t23
        0.0d0,   &    !t24
        0.0d0,   &    !t34
        0.0d0,   &    !delta1
        0.0d0,   &    !delta2
        0.0d0,   &    !delta3
        0.0d0,   &    !dm21
        2.50d-3,    &      !dm31
        0.0d0/)       !dm41

    t12=Y(1)
    t13=Y(2)
    t14=Y(3)
    t23=Y(4)
    t24=Y(5)
    t34=Y(6)
    delta1=Y(7)
    delta2=Y(8)
    delta3=Y(9)
    dm21=Y(10)
    dm31=Y(11)
    dm41=Y(12)

    print*, 'You are in the detector testing subrputine'
    print*, 'IBD OBS '//' AND '//' IBD THEORY '    
    do d=1,ADS        
        print*,Md_1607(d),expectedNumberNeutrinosDetector(d)
    enddo    
    return
end subroutine DBDetectorsTest
