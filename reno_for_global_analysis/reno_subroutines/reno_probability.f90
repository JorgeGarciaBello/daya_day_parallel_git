real(8) function probability(t13,dm31,l,x)

 implicit none
 real(8) :: t13
 real(8) :: dm31
 real(8) :: l,x

 real(8) :: t12=0.5872523687d0 !0.5837630476d0
 real(8) :: dm32=2.44D-3
 real(8) :: dm21=7.53D-5 !7.53D-5

 dm32=dm31+dm21

!probability= 1.0d0 -cos(t13)**4*sin(2.0d0*t12)**2*sin(1.267d0*dm21*l/x)**2   &
!                   -sin(2.0d0*t13)**2*(cos(t12)**2*sin(1.267d0*dm31*l/x)**2  &
!                                     +sin(t12)**2*sin(1.267d0*dm32*l/x)**2)
probability= 1.0d0 -cos(t13)**4*sin(2.0d0*t12)**2*sin(1.267d0*dm21*l/x)**2   &
                   -sin(2.0d0*t13)**2*(sin(1.267d0*dm31*l/x)**2)

return

print*,'probability',probability

end function probability