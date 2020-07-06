subroutine chiSquareRENOTest()
    use reno_data, only: NDIM
    use neu_osc_parameters, only: t13,dm31
    implicit none    
    real(8) :: P(NDIM)              ! Parámetros de oscilacióna determinar
    real(8) :: FUNC,result

    t13=0.0d0!0.147d0
    dm31=0.0d0!2.5d-3
    P=100.0d0

    result=FUNC(P)
    return
end subroutine chiSquareRENOTest