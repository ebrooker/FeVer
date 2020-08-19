PROGRAM test
    USE globals
    USE io_parameterFile, ONLY : io_setupParameterFile,&
                                 io_readParameterFile
    USE grid, ONLY : init_grid
    IMPLICIT NONE
        INTEGER :: i
        WRITE(*,*)
        CALL io_setupParameterFile("test_data/test.par")
        CALL io_readParameterFile
        CALL init_grid
        CALL init_states
        WRITE(*,*) ""
        IF(nx.gt.1) WRITE(*,'(A,I2,3(X,ES8.2))') " nx,xmin,xmax,dx = ", nx,xmin,xmax,dx
        ! WRITE(*,*) (x(i), i=1+ng,nx+ng)
        IF(ny.gt.1) WRITE(*,'(A,I2,3(X,ES8.2))') " ny,ymin,ymax,dy = ", ny,ymin,ymax,dy
        ! WRITE(*,*) (y(i), i=1+ng,ny+ng)
        IF(nz.gt.1) WRITE(*,'(A,I2,3(X,ES8.2))') " nz,zmin,zmax,dz = ", nz,zmin,zmax,dz
        ! WRITE(*,*) (z(i), i=1+ng,nz+ng)
        WRITE(*,*) ""
END PROGRAM test