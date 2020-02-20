subroutine fittingSpectrum()
    use neu_osc_parameters
    use db_data, only: NBIN,farObs,farExp
    implicit none
    real(8) :: fittingSpectrumFunction
    real(8) :: alpha
    integer :: i,m
    
    integer,parameter :: NDIM=1
    integer,parameter :: NP=NDIM
    integer,parameter :: MP=NDIM+1
    real(8),dimension(MP,NP) :: P          ! P is the set of MP points with NP coordenates that generate the polygon of AMOEBA
    real(8),dimension(MP)    :: Y
    real(8),dimension(NDIM)  :: PT
    real(8) :: FTOL=1.D-5               ! Required tolerance
    integer :: ITER                     ! Número de iteraciones realizadas por AMOEBA

    print*, ' Fitting Spectrum'
    !######################################
    !
    !   Reading the points that 
    !   defined the simplex
    !
    !######################################
    P(1,1)=1.400d0
    P(2,1)=1.461d0

    DO m=1, NDIM+1
        ! Se carga la i-ésima coordenada del m-ésimo punto al arreglo PT    
        do i=1,NDIM
            PT(i)=P(m,i)
        enddo        
        Y(m)=fittingSpectrumFunction(PT)    
    END DO
    CALL AMOEBAFF(P,Y,MP,NP,NDIM,FTOL,ITER)
    print*,'Result'
    print*,P

    return
end subroutine fittingSpectrum
