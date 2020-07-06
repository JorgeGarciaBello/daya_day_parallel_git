!##################################################################
!
!   expectedFarToNearRatioIBD: is the expected fat-to-near
!                              ratio of IBD candidates in the 
!                              i-th bin after background 
!                              subtraction
!
!##################################################################
real(8) function reno_expectedFarToNearRatioIBD(i)
    use reno_data, only: NBIN, nearExp,nearObs,farExp
    implicit none
    integer,parameter :: near=1   ! near is one for the near detector
    integer,parameter :: far=2    ! near is two for the far detector
    integer :: i                  ! is the number of the bin
    
    real(8) :: reno_model
    
    reno_expectedFarToNearRatioIBD=0.0
    reno_expectedFarToNearRatioIBD=(  ( farExp(i)*reno_model(far,i))            &
                                /( nearObs(i)*(1.0d0/reno_model(near,i)))   &
                                !/( blackNear(i) ) &
                              )

    return
end function reno_expectedFarToNearRatioIBD