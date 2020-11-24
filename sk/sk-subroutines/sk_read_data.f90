subroutine sk_read_data()
    use types
    use sk_data
    implicit none
    integer :: u,i,j

    open(newunit=u, file='sk-data-files/sk.complete.201803.ih.dat', status='old')
        read(u,*)   ((sk2018ih(i,j), j=1,5), i=1,851200)
    close(u)
    open(newunit=u, file='sk-data-files/sk.complete.201803.nh.dat', status='old')
        read(u,*)   ((sk2018nh(i,j), j=1,5), i=1,851200)
    close(u)
    open(newunit=u, file='sk-data-files/sk.q13-fixed.201803.ih.dat', status='old')
        read(u,*)   ((skt13fxd2018ih(i,j), j=1,5), i=1,26600)
    close(u)
    open(newunit=u, file='sk-data-files/sk.q13-fixed.201803.nh.dat', status='old')
        read(u,*)   ((skt13fxd2018nh(i,j), j=1,5), i=1,26600)
    close(u)
    
    open(newunit=u, file='sk-data-files/sk.atm.data.release/sk-nh-ji.dat', status='old')
        read(u,*)   ((sknhji(i,j), j=1,4), i=1,44800)
    close(u)
    open(newunit=u, file='sk-data-files/sk.atm.data.release/db.dm32.s2t13.s2t23.prueba3D.dat', status='old')
        read(u,*)   ((dbdm32s2t13s2t23prueba3D(i,j), j=1,4), i=1,44800)
    close(u)
    open(newunit=u, file='sk-data-files/sk.atm.data.release/reno.dm32.s2t13.s2t23.prueba3D.RENO.dat', status='old')
        read(u,*)   ((renodm32s2t13s2t23prueba3DRENO(i,j), j=1,4), i=1,44800)
    close(u)

    open(newunit=u, file='sk-data-files/minos.3D.grid.of.sk.dm32.s2t13.s2t23.nh', status='old')
        read(u,*)   ((minos_nh(i,j), j=1,4), i=1,44800)
    close(u)
    open(newunit=u, file='sk-data-files/minos.3D.grid.of.sk.dm32.s2t13.s2t23.ih', status='old')
        read(u,*)   ((minos_ih(i,j), j=1,4), i=1,44800)
    close(u)
    return
end subroutine sk_read_data