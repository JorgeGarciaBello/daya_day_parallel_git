!#####################################################################################
!
!       weigth_w: is a weight where the model is included
!
!#####################################################################################
real(8) function  weigthW(i)
    implicit none    
    integer :: i                                 ! is the number of energy bin
    real(8) :: expectedNeutrinoSpectrumByBinFar  ! expected neutirno spectrum bay energy-bin and by far-hall
    real(8) :: expectedNeutrinoSpectrumByBinNear ! expected neutirno spectrum bay energy-bin and by near-hall

    weigthW = 0.0D0
    weigthW = expectedNeutrinoSpectrumByBinFar(i)/expectedNeutrinoSpectrumByBinNear(i)
    return
end function weigthW