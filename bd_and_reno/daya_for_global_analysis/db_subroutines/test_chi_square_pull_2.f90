!##################################################################
!   
!       Con parR=1.0d0 el valor de Chi es 231588.72240116150
!
!       Con parR=0.0d0 el valor de Chi es 2489165.5926215062
!
!##########3#######################################################

subroutine testChiSquarePull2()
    use db_data,only:NDIM
    use neu_osc_parameters
    implicit none
    real(8) :: P(NDIM)

    real(8) :: chiSquarePull2
    real(8) :: chi
    
    t12=0.5837630476d0! 784519d0!0.57636d0 ! 0.5837785d0 
    t13=0.14d0
    t14=0.0d0
    t23=0.71011D0 ! Normal Hierarchy 0.71011D0 ! Inverted Hierarchy 0.87487d0
    t24=0.0d0
    t34=0.0d0
    delta1=0.0d0
    delta2=0.0d0
    delta3=0.0d0
    dm21=7.53D-5 !7.37D-5  7.53D-5
    dm31=2.5d-3!153D-3
    dm41=0.0d0    !

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

    chi=chiSquarePull2(P)
    !print*, chi
    return
end subroutine testChiSquarePull2