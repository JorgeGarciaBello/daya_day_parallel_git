subroutine testChiPull1()
    use db_data, only: NDIM
    use neu_osc_parameters
    implicit none
    real(8), parameter :: PI=3.141592653589793238462643d0

    real(8) :: P(NDIM)
    real(8) :: s2_2t12=0.846D0, c2_t12=0.6962D0, s2_t12=0.3038D0
    real(8) :: chiSquarePull1
    
    print*, ' In testChiPull1'    

    t12=0.0D0!0.5837630475918688D0!PI/4.0D0
    t13=0.0D0!0.14702331669d0!0.0D0!PI/
    t23=0.0D0!0.7853981631d0!0.698356658079 !0.0D0!PI/
    t14=0.0D0
    t24=0.0D0
    t34=0.0D0

    delta1=0.0D0
    delta2=0.0D0
    delta3=0.0d0

    dm21=0.0D0!7.53D-5 !7.53D-5
    dm31=0.0D0!2.45D-3 !1E-3 !1.0e-3
    dm41=0.0d0

    !P(1)=   ;   P(11)=  ;   P(21)=  ;
    !P(2)=   ;   P(12)=  ;   P(22)=  ;
    !P(3)=   ;   P(13)=  ;   P(23)=  ;
    !P(4)=   ;   P(14)=  ;       
    !P(5)=   ;   P(15)=  ;       
    !P(6)=   ;   P(16)=  ;       
    !P(7)=   ;   P(17)=  ;       
    !P(8)=   ;   P(18)=  ;       
    !P(9)=   ;   P(19)=  ;       
    !P(10)=  ;   P(20)=  ;       

    P=0.d0
    
    print*, chiSquarePull1(P)

    return
end subroutine testChiPull1