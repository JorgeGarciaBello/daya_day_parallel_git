!################################################################
!
!       Amoeba Function
!
!################################################################
SUBROUTINE reno_AMOEBA(t13,dmee,P,Y,MP,NP,NDIM,FTOL,ITER)
IMPLICIT REAL*8 (A-H,O-Z)
PARAMETER(NMAX=25,ALPHA=1.d0,BETA=0.5d0,GAMMA=2.d0,ITMAX=20)
! Expected maximum number of dimensions, three parameters which define
! the expansions and contractions, and maximum allowed number of
! iterations.
DIMENSION P(MP,NP), Y(MP), PR(NMAX), PRR(NMAX), PBAR(NMAX)
  MPTS=NDIM+1
  ITER=0
1 ILO=1
  IF(Y(1).GT.Y(2)) THEN
    IHI=1
    INHI=2
  ELSE
    IHI=2
    INHI=1
  ENDIF
  DO I=1, MPTS
    IF(Y(I).LT.Y(ILO)) ILO=I
    IF(Y(I).GT.Y(IHI)) THEN
      INHI=IHI
      IHI=I
    ELSE IF (Y(I).GT.Y(INHI)) THEN
      IF(I.NE.IHI) INHI=I
    END IF
  END DO
! Compute the fractional range from highest to lowest and return if
! satisfactory.
  RTOL=2.d0*DABS(Y(IHI)-Y(ILO))/(DABS(Y(IHI))+DABS(Y(ILO)))
  IF(RTOL.LT.FTOL) RETURN
!  IF(ITER.EQ.ITMAX) Pause ' Amoeba exceeding maximum iterations.'

  ITER=ITER+1
  DO J=1, NDIM
    PBAR(J)=0.d0
  END DO
  DO I=1, MPTS
    IF(I.NE.IHI) THEN
      DO J=1,NDIM
        PBAR(J)=PBAR(J) + P(I,J)
      END DO
    END IF   
  END DO
  DO J=1, NDIM
    PBAR(J)=PBAR(J)/NDIM
    PR(J)=(1.d0+ALPHA)*PBAR(J) - ALPHA*P(IHI,J)
  END DO      
  YPR=reno_FUNC(t13,dmee,PR)
  IF(YPR.LE.Y(ILO)) THEN
    DO J=1,NDIM
      PRR(J)=GAMMA*PR(J) + (1.d0-GAMMA)*PBAR(J)
    END DO
    YPRR=reno_FUNC(t13,dmee,PRR)
    IF(YPRR.LT.Y(ILO)) THEN
      DO J=1, NDIM
        P(IHI,J)=PRR(J)
      END DO
      Y(IHI)=YPRR
    ELSE
      DO J=1, NDIM
        P(IHI,J)=PR(J)
      END DO
      Y(IHI)=YPR      
    END IF
  ELSE IF(YPR.GE.Y(INHI)) THEN
    IF(YPR.LT.Y(IHI)) THEN
      DO J=1, NDIM
        P(IHI,J)=PR(J)
      END DO
      Y(IHI)=YPR
    END IF
    DO J=1, NDIM
      PRR(J)=BETA*P(IHI,J) + (1.d0-BETA)*PBAR(J)
    END DO
    YPRR=reno_FUNC(t13,dmee,PRR)
    IF(YPRR.LT.Y(IHI)) THEN
      DO J=1, NDIM
        P(IHI,J)=PRR(J)
      END DO
      Y(IHI)=YPRR
    ELSE
      DO I=1, MPTS
        IF(I.NE.ILO) THEN
          DO J=1,NDIM
            PR(J)=0.5d0*(P(I,J) + P(ILO,J))
      P(I,J)=PR(J)
      END DO
          Y(I)=reno_FUNC(t13,dmee,PR)
    END IF
      END DO
    END IF
  ELSE
    DO J=1, NDIM
      P(IHI,J)=PR(J)
    END DO
    Y(IHI)=YPR
  END IF
  GO TO 1
  END
  
!end of file tamoeba.f90