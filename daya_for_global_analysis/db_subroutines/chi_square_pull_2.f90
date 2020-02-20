!###############################################################################################
!
!   chiSquarePull2: is the chi-square with 26 parameters to be minimize 
!                   (t13,dm_31,parR,eps,alphaR,epsD,etaD)
!
!
!###############################################################################################
real(8)  function chiSquarePull2(P)
    use db_data, only:NDIM,ADS,RCTS,Md_1607,Bd
    implicit none
    real(8) :: P(NDIM)
    
    real(8) :: eps                ! is the parameter of absolute normalization
    real(8) :: alphaR(RCTS)       ! is an array with pull parameters reactor
    real(8) :: epsD(ADS)         ! is an array with pull parameters detector
    real(8) :: etaD(ADS)         ! is an array with pull parameters background-detector
    real(8) :: parR               ! is a free parameter obteined from the minimization

    real(8) :: sigmaR             ! is the uncorrelated reactor uncertainty
    real(8) :: sigmaD             ! is the uncrrelated detection uncertainty
    real(8) :: sigmaB(ADS)        ! is the uncorrelated background uncertainty
    real(8) :: sigmaDD            ! is the correlated detection uncertainty

    real(8) :: expectedNumberNeutrinosDetector
    real(8) :: Td
    real(8) :: sum_W_alp
    integer :: d,r

    chiSquarePull2=0.0d0
    ! all information from paper 1607.05378
    sigmaR=0.008d0;
    sigmaD=0.002d0;
    ! in IBD's cause etaD is in unity of IDB
    ! Datos del paper 1607
    !sigmaB=(/688.6714252541D0,686.4482358545D0,720.2925052959D0,479.8539162828D0,   &
    !         422.5327845981D0,815.6867364404D0,815.1865154834D0,540.7028804284D0/);
    ! Datos del paper 1809
    sigmaB=(/1040,1176,875,782,155,150,150,133/);
    sigmaDD=0.0193d0;           ! from paper 1607.05378

    eps=P(1)
!    alphaR=(/P(2),P(3),P(4),P(5),P(6),P(7)/)
 !   epsD=(/P(8),P(9),P(10),P(11),P(12),P(13),P(14),P(15)/)
  !  etaD=(/P(16),P(17),P(18),P(19),P(20),P(21),P(22),P(23)/)
   ! parR=P(24)

    do d=1,ADS
        Td=expectedNumberNeutrinosDetector(d)
        chiSquarePull2=chiSquarePull2   &
                       +( Md_1607(d)                                        &
                         -parR*Td*(1.0d0+eps+sum_W_alp(d,alphaR)+epsD(d))   &
                         +etaD(d)                                           &
                         )**2/(Md_1607(d)+Bd(d))
                       !+(Md_1607(d)-Td*(1.0d0+eps+sum_W_alp(d,alphaR)+epsD(d))+etaD(d))**2/(Md_1607(d)+Bd(d))
    enddo        
    do r=1,RCTS
        chiSquarePull2=chiSquarePull2+(alphaR(r)/sigmaR)**2
    enddo
    do d=1,ADS
        chiSquarePull2=chiSquarePull2+( (epsD(d)/sigmaD)**2 + (etaD(d)/sigmaB(d))**2 )
    enddo

    chiSquarePull2=chiSquarePull2+(eps/sigmaDD)**2
    
    return
end function chiSquarePull2