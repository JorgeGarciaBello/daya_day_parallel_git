    !   40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63
    !   64 65
    !
    !   No está funcionando bien. Revisar término off, N y SecE, hacer positivo el simplex
    !
    !   El vector de parámetros de oscilación Y está definido como sigue:
    !
    !           6 ángulos de mezcla             3 Fases         3 Diferencias de masas cuadradas
    !           theta_ij,           d_ij             dm21, dm31 y dm41.
    !   Y=( t12 , t13 , t14 , t23 , t24 , t34 , d13 , d24 , d34 , dm21 , dm31 , dm41 )
    !
    module DC_condiciones
    implicit none
        real*8 deltachi
        integer ciclos,diagon,mm
        real*8, dimension(4) :: region
    contains
        Subroutine parametros(deltachi,ciclos,region,diagon,mm)  !Calcula las integrales Ppromedio
            implicit none
            real*8 deltachi
            integer ciclos,diagon,mm,cambio
            real*8, dimension(4) :: region
            !
!            open(62,file='double_chooz_for_global_analysis/dc_data/region.dat')  ! Región define el dominio de los parámetros de oscilación que se buscan
!                read (62,*) region
!            close(62)
!            cambio=0
        !
!            print *         ! Restriccion de dominio de parámetros de oscilación
!            write(*,*) 'El angulo de mezcla se busca entre'
            !print *
!            write(*,*) region(1), 'y', region(2)
!            print * 
!            write(*,*) 'El dominio completo es de 0 a 0.7853981634'
!            print *
!            print *
!            print * 
!            write(*,*) 'La diferencia de masas cuadradas se busca entre'
            !print *
!        write(*,*) region(3), 'y', region(4)
!            print * 
!            write(*,*) 'El dominio completo es de 1E-4 a 1'
!            print *
!            print *
!            print * 
!            write(*,*) '1) Conservar'
!            write(*,*) '2) Cambiar'
!            print *
!        read(*,*) cambio
!        print *
!            if(cambio==2)then
!                print *
!                write(*,*) 'theta min='
!                read(*,*) region(1)
!                print *
!                !
 !               write(*,*) 'theta max='
 !               read(*,*) region(2)
 !               print *
                !
!                write(*,*) 'dm2 min='
!                read(*,*) region(3)
!                print *
!                !
!                write(*,*) 'dm2 max='
!                read(*,*) region(4)
!                print *
                !
!                open(62,file='double_chooz_for_global_analysis/dc_data/region.dat')
!                write (62,*) region
!                close(62)
!            end if
            !
            deltachi=2000.d0

            !################################################
            !
            !   Matriz completa
            !
            !   Para usar la matriz completa => diagon =2
            !   para usar la matriz diagonal => diagon=1
            !
            !################################################
            diagon=2
            
            !################################################
            !
            !   Restricción para el parámetro de masa
            !
            !   mm => 1: Sin restricción del experimento MINOS
            !   mm => 2: Con restricción del experimento MINOS
            !
            !#################################################
            mm=1
            return
        end Subroutine parametros
    end module
!    
!
!
!
!
    module DC_espectros
        implicit none
        real*8, dimension(40) :: Nobs,Nexp,acc,LiHe
        real*8, dimension(41) :: Enu
    end module

    Subroutine ReadDC()   !Calcula las integrales Ppromedio
        use DC_espectros
        implicit none
        open(49,file='double_chooz_for_global_analysis/dc_data/DC_far_obs.dat')
            read (49,*) Nobs
        close(49)
            !
        open(50,file='double_chooz_for_global_analysis/dc_data/DC_far_exp.dat')
            read (50,*) Nexp
        close(50)
        !
        open(51,file='double_chooz_for_global_analysis/dc_data/DC_acc.dat')
            read (51,*) acc
        close(51)
        !
        open(52,file='double_chooz_for_global_analysis/dc_data/DC_LiHe.dat')
            read (52,*) LiHe
        close(52)
        !
        open(53,file='double_chooz_for_global_analysis/dc_data/DC_bins.dat')
            read (53,*) Enu
        close(53)
            !
            return
    end Subroutine ReadDC
!
!
!
!
!
    module matriz
    use DC_condiciones
    implicit none
    real*8, dimension(40,40) :: M
    contains
        Subroutine InverMat(M,diagon)
            implicit none
        integer i,j,k,l,n,diagon!,rep
        real*8, dimension(40,40) :: Inv,Maux,Mst,Mrf,Mde,Mac,Mli,M
        real*8 aux
        CALL ReadMat(Mst,Mrf,Mde,Mac,Mli)
            Maux=Mst+Mrf+Mde+Mac+Mli                ! Estadistico! Reactor! Deteccion! Accidental! Li+ He
            if(diagon==1)then
            do i=1,40
                M(i,i)=1.d0/(M(i,i))
            end do
        else
                M=Maux
            Inv=0.d0
                do i=1,40
                Inv(i,i)=1.d0         ! Generación de matriz unidad
                end do
                do j=1,40
                    aux=M(j,j)
                    do n=1,40
                    if((abs(aux) > 1E-10))then! .and. (flag==0))then
                        Inv(n,j)=Inv(n,j)/aux
                        M(n,j)=M(n,j)/aux   ! elemento diagonal distinto de cero es unitario
                        end if
                end do
!
                    if((j<40))then! .and. (flag==0))then
                    do k=j+1,40
                        aux=M(j,k)
                        do l=1,40
                                Inv(l,k)=Inv(l,k)-Inv(l,j)*aux
                            M(l,k)=M(l,k)-M(l,j)*aux    ! Convierte la matriz en triangular inferior
                        end do
            end do
                end if
                end do
!    
                do i=1,40
                    do k=i,39
                        aux=M(41-i,40-k)
                        do l=1,40
                            Inv(l,40-k)=Inv(l,40-k)-Inv(l,41-i)*aux ! Diagonalizar matriz triangular 
                            M(l,40-k)=M(l,40-k)-M(l,41-i)*aux
                        end do
                end do
                end do
!
                M=Inv          !    Se sustituye la matriz por su inversa
        end if
        return
    end Subroutine InverMat
    end module
!
!
!
!
!
!##################################################
!
!   function chi2_DC: función Chi-cuadrada del 
!       experimento Double CHOOZ. 
!       Recibe como entradas: un vector Y de 
!       parámetros de pscilación y los pulls 
!       asociados a las incertidumbres del 
!       experiemnto.
!       Regresa: un valor real*8 del valor de 
!       la estadística.
!
!##################################################
Function chi2_DC(Y,pulls_dc)
    use DC_condiciones
    use DC_espectros
    use matriz  
    implicit none
    integer :: i,j,h!,rep
    real*8 :: aux,chialfa,chinum,pullsterm,chiOn,L,nm,POff,Toff,B!,peso,x
    real*8 :: ep1,ep2,ep3,epa,epb,epc,ChiOff,NExpOff,NObsOff,NTot,TOn,ep4,ep5!,
    real*8 :: chi2_DC,MRes!,sct!,P
    real*8, dimension(40) :: Pprom
    real*8, dimension(8) :: pulls_dc   !Valores a minimizar
    real*8, dimension(12) :: Y
    ep1=pulls_dc(1)
    ep2=pulls_dc(2)
    ep3=pulls_dc(3)
    ep4=pulls_dc(4)
    ep5=pulls_dc(5)
    epa=pulls_dc(6)
    epb=pulls_dc(7)
    epc=pulls_dc(8)
    L=1050.d0
    B=1.56d0
    !sct=(sin(2.d0*Y(2)))**2
    CALL Promoff(Y,POff,epa,epb,epc)
    !P=1-0.75*sct
    Toff=7.24d0
    NTot=17304.d0
    TOn=460.67
    NExpOff=(Toff*NTot/TOn)*(1+ep4)*POff+B*Toff
    NObsOff=7.d0
    !Maux=0.d0
    !x=0.5d0 !Energía del primer bin
    nm=3.d0
    do h=1,40
        call Ppromedio(Enu,pulls_dc,L,Y,Pprom(h),h)     ! (Calcula y guarda las 40 integrales de Pprom)                     
    end do
!
! Chi On
!
    ChiOn=0.d0
    if(diagon==1)then
        do i=1,40
            pullsterm=ep1*(LiHe(i))+ep2*(nm)+ep3*(acc(i))
            aux=Nobs(i)-(Nexp(i))*(Pprom(i))*(1.d0+ep4)-pullsterm  ! Con off
            !aux=Nobs(i)-(Nexp(i))*(Pprom(i))-pullsterm ! Sin off
            aux=aux*aux
            aux=aux*M(i,i)
            chiOn=chiOn+aux  
        end do
    else
        do i=1,40
        do j=1,40
            pullsterm=ep1*(LiHe(i))+ep2*(nm)+ep3*(acc(i))
            !aux=Nobs(i)-Nexp(i)*Pprom(i)-pullsterm                                             !   Sin off
            aux=Nobs(i)-Nexp(i)*Pprom(i)-(((1.d0+ep4)*TOn*(Nexp(i))*Pprom(i))/(Toff*NTot))-pullsterm    !   Con off
            pullsterm=ep1*(LiHe(j))+ep2*(nm)+ep3*(acc(j))
            !aux=aux*(Nobs(j)-Nexp(j)*Pprom(j)-pullsterm)           !   Sin off
            aux=aux*(Nobs(j)-Nexp(j)*Pprom(j)-(((ep4+1.d0)*Pprom(j)*TOn*(Nexp(j)))/(Toff*NTot))-pullsterm)          !   Con off
            aux=aux*M(i,j)
            chiOn=chiOn+aux
        end do
        end do
    end if
!
!   Chi off
!   
    if(NObsOff/NExpOff>0.d0)then
        ChiOff=NObsOff*log(NObsOff/NExpOff) + NExpOff - NObsOff 
        ChiOff=ChiOff*2.d0
    else
        ChiOff=(NObsOff)*1000.d0
    end if
!
!   Pulls   
    chialfa=30.76064368d0*epa**2+18.89069749d0*epb**2+3056.249126d0*epc**2
    chialfa=chialfa+15.0095921d0*epa*epb+53.66907806d0*epa*epc+145.0223795d0*epb*epc
    chialfa=chialfa*1000.d0
!    chialfa=0.d0
!
!   Pulls numericos
!   1: Li+He   2: n-mu   3: acc   4: resid   5: masas
!
    chinum=(ep1/0.13d0)**2+(ep2/0.038d0)**2+(ep3/0.0026d0)**2
    chinum=chinum+(ep4/0.47d0)**2+(ep5/0.095)**2
!
!   Chi total
!
    if(mm==1)then
        chi2_DC=ChiOn+chialfa+chinum+ChiOff
    else
!
!   Restriccion de masas
!
        MRes=((0.00244-Y(11))**2)*(1.108E8)
        chi2_DC=ChiOn+chialfa+chinum+MRes+ChiOff
    end if
    return
    end function chi2_DC

!
!
!
!
!
!
!
!
!

    function P(Enu,Y,L)
    implicit none
    real*8 Enu,L,sct12,dm221,P,t13,dm2
    real*8, dimension(12) :: Y
    sct12=0.851004d0
    dm221=0.0000753d0
    t13=Y(2)
    dm2=Y(11)
    P=1.0d0-((sin(2.d0*t13))**2)*(sin((1.267d0*dm2*L)/Enu))**2
    P=P-((cos(t13))**4)*(sct12)*((sin((1.267d0*dm221*L)/Enu))**2)
    return
    end function P
    !
!
!
!
!
!
!
!
!            

    !
!
!
!
!
!
!
!
    !
!
!
!
!
!
!
!
!   
    Subroutine Promoff(Y,POff,epa,epb,epc)  
    implicit none
    real*8 POff,Enu,L,dE,DeltaE,epa,epb,epc,x
    integer i
    real*8, dimension(12) :: Y
    DeltaE=19.5d0
    dE=DeltaE/1000.d0
    Enu=2.3d0+(dE*0.5d0)
    POff=0.d0
    L=1050.d0
    do i=1,1000
        x=Enu+epa+epb*Enu+epc*Enu*Enu
        POff=POff+((sin((1.27d0*Y(11)*L)/(x)))**2)*dE
        Enu=Enu+dE
    end do
    POff=POff/DeltaE
!    print *, 'Poff:',POff
    POff=1.d0-(((sin(2.d0*Y(2)))**2)*POff)
    return
    end Subroutine Promoff
!
!
!
!
!
!
!
!
!            
    Subroutine Ppromedio(Enu,pulls_dc,L,Y,Pprom,k)  !Calcula las integrales Ppromedio
    implicit none
    real*8 Pprom,L,P,Eaux,dE,norma,DEA,epa,epb,epc,part,N,SecE
    real*8, dimension(12) :: Y
    real*8, dimension(41) :: Enu
    real*8, dimension(8) :: pulls_dc
    integer k,i
    epa=pulls_dc(6)
    epb=pulls_dc(7)
    epc=pulls_dc(8)
    part=100.d0
    norma=0.d0
    dE=(Enu(k+1)-Enu(k))/part
    Eaux=Enu(k)+(dE*0.5d0)
    Pprom=0.d0
    do i=1,int(part)
    !
    DEA=epa+epb*Eaux+epc*Eaux*Eaux !Corrección en energía
        !
        Pprom=Pprom+P(Eaux+DEA,Y,L)*dE!*N(Eaux+DEA)*SecE(Eaux+DEA)
        norma=norma+dE!*N(Eaux+DEA)*SecE(Eaux+DEA)
        Eaux=Eaux+dE
    end do
    if(norma>0.d0)then
        Pprom=Pprom/norma
    else
        Pprom=1000.d0
    end if
    return
    end Subroutine Ppromedio

Subroutine ReadMat(Mst,Mrf,Mde,Mac,Mli)  !Calcula las integrales Ppromedio
    implicit none
    real*8, dimension(40,40) :: Mst,Mrf,Mde,Mac,Mli
    !
    open(44,file='double_chooz_for_global_analysis/dc_data/DC_Mst.dat')
    read (44,*) Mst
    close(44)
    !
    open(45,file='double_chooz_for_global_analysis/dc_data/DC_Mrf.dat')
    read (45,*) Mrf
    close(45)
    !
    open(46,file='double_chooz_for_global_analysis/dc_data/DC_Mde.dat')
    read (46,*) Mde
    close(46)
    !
    open(47,file='double_chooz_for_global_analysis/dc_data/DC_Mli.dat')
    read (47,*) Mli
    close(47)
    !
    open(48,file='double_chooz_for_global_analysis/dc_data/DC_Mac.dat')
    read (48,*) Mac
    close(48)
    !
    return
    end Subroutine ReadMat
!
!
!
!
!
!
    Subroutine SaveP_DC(PP)
    implicit none
    real*8, dimension(9,8) :: PP
    integer i
    real*8 r
    do i=1,8
    call RANDOM_NUMBER(r)
      PP(i,i)=(r*4.d0)-2.d0
      !PP(i,i)=(RANDOM@()*4.d0)-2.d0
      PP(i+1,i)=0.d0
    end do
    open(42,file='double_chooz_for_global_analysis/dc_data/DC_simplex.dat')
    write (42,59) PP
    close(42)
    59 format (9(E16.10,12X))
    End Subroutine SaveP_DC
    
    !
    !
    !
    !
    !
    function SecE(Enu) ! Sección eficaz, no se está usando ya que no funciona correctamente.
    implicit none
    real*8 SecE,Enu
    SecE=Enu**2
    return
    end function SecE
!
!
!
!
!
!
!
!  
SUBROUTINE AMOEBA_DC(PP,YY,Y)
    use DC_espectros
    IMPLICIT REAL*8 (A-H,O-Z)
    real*8, dimension(12) :: Y
!    real*8, dimension(5) :: NN
    PARAMETER(NMAX=20,ALPHA=1.d0,BETA=0.5d0,GAMMA=2.d0,ITMAX=600,NDIM=8,MP=9,NP=8)
! Expected maximum number of dimensions, three parameters which define
! the expansions and contractions, and maximum allowed number of
! iterations.
    DIMENSION PP(MP,NP), YY(MP), PR(NMAX), PRR(NMAX), PBAR(NMAX)
    FTOL=1.D-8  ! precisión exigida a amoeba
    ITER=9000
  MPTS=NDIM+1
  ITER=0
    1 ILO=1
  IF(YY(1).GT.YY(2)) THEN
    IHI=1
    INHI=2
  ELSE
    IHI=2
    INHI=1
  ENDIF
  DO I=1, MPTS
    IF(YY(I).LT.YY(ILO)) ILO=I
    IF(YY(I).GT.YY(IHI)) THEN
      INHI=IHI
      IHI=I
    ELSE IF (YY(I).GT.YY(INHI)) THEN
      IF(I.NE.IHI) INHI=I
    END IF
  END DO
! Compute the fractional range from highest to lowest and return if
! satisfactory.
  RTOL=2.d0*DABS(YY(IHI)-YY(ILO))/(DABS(YY(IHI))+DABS(YY(ILO)))
  IF(RTOL.LT.FTOL) RETURN
  IF(ITER.EQ.ITMAX) THEN
    Print *, 'Se ha excedido el numero de iteraciones'
    read(*,*)
  END IF
  ITER=ITER+1
  DO J=1, NDIM
    PBAR(J)=0.d0
  END DO
  DO I=1, MPTS
    IF(I.NE.IHI) THEN
      DO J=1,NDIM
        PBAR(J)=PBAR(J) + PP(I,J)
      END DO
    END IF   
  END DO
  DO J=1, NDIM
    PBAR(J)=PBAR(J)/NDIM
    PR(J)=(1.d0+ALPHA)*PBAR(J) - ALPHA*PP(IHI,J)
  END DO
  YPR=chi2_DC(Y,PR)
  IF(YPR.LE.YY(ILO)) THEN
    DO J=1,NDIM
      PRR(J)=GAMMA*PR(J) + (1.d0-GAMMA)*PBAR(J)
    END DO
    YPRR=chi2_DC(Y,PRR)
    IF(YPRR.LT.YY(ILO)) THEN
      DO J=1, NDIM
        PP(IHI,J)=PRR(J)
      END DO
      YY(IHI)=YPRR
    ELSE
      DO J=1, NDIM
        PP(IHI,J)=PR(J)
      END DO
      YY(IHI)=YPR     
    END IF
  ELSE IF(YPR.GE.YY(INHI)) THEN
    IF(YPR.LT.YY(IHI)) THEN
      DO J=1, NDIM
        PP(IHI,J)=PR(J)
      END DO
      YY(IHI)=YPR
    END IF
    DO J=1, NDIM
      PRR(J)=BETA*PP(IHI,J) + (1.d0-BETA)*PBAR(J)
    END DO
    YPRR=chi2_DC(Y,PRR)
    IF(YPRR.LT.YY(IHI)) THEN
      DO J=1, NDIM
        PP(IHI,J)=PRR(J)
      END DO
      YY(IHI)=YPRR
    ELSE
      DO I=1, MPTS
        IF(I.NE.ILO) THEN
          DO J=1,NDIM
            PR(J)=0.5d0*(PP(I,J) + PP(ILO,J))
            PP(I,J)=PR(J)
      END DO
          YY(I)=chi2_DC(Y,PR)
    END IF
      END DO
    END IF
  ELSE
    DO J=1, NDIM
      PP(IHI,J)=PR(J)
    END DO
    YY(IHI)=YPR
  END IF
  GO TO 1
  END subroutine amoeba_DC
