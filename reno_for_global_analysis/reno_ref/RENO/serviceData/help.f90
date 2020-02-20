program help
    implicit none
    real(8) :: x1(26,2)!,x2(26,2),x3(26,2),x4(26,2),x5(26,2),x6(26,2)
    real(8) :: result(26)
    integer :: i,j, reason

    open(20,file='expectedFarSpectrumData.dat',status='old')
    !open(21,file='expectedFarSpectrumWithoutBumbAndOscillations.dat',status='old')
    !open(22,file='expectedNearSpectrumData.dat',status='old')
    !open(23,file='expextedFarSpectrumWithoutBumbWithOscillations.dat',status='old')
    !open(24,file='observedFarSpectrumData.dat',status='old')
    !open(25,file='observedNearSpectrumData.dat',status='old')
      read(20,*,IOSTAT=reason) ((x1(i,j),j=1,26),i=1,2)
      !read(21,*,IOSTAT=reason) ((x2(i,j),j=1,26),i=1,2)
      !read(22,*,IOSTAT=reason) ((x3(i,j),j=1,26),i=1,2)
      !read(23,*,IOSTAT=reason) ((x4(i,j),j=1,26),i=1,2)
      !read(24,*,IOSTAT=reason) ((x5(i,j),j=1,26),i=1,2)
      !read(25,*,IOSTAT=reason) ((x6(i,j),j=1,26),i=1,2)                                
    close(20)
    !close(21)
    !close(22)
    !close(23)
    !close(24)
    !close(25)

    !do i=1,26
    !    result(i) = x1(i,1) + x2(i,1) + x3(i,1) + x4(i,1) + x5(i,1) + x6(i,1)
    !enddo
    !do i=1,26
    !    result(i) = result(i)/6.0d0
    !enddo

    !do i=1,26
    !    print*, result(i)
    !enddo
end program help