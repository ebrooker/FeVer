MODULE globals

    INTEGER :: ng, nx, ny, nz
    INTEGER :: nxl,nxr,nyl,nyr,nzl,nzr


    REAL :: xmin, xmax, dx
    REAL :: ymin, ymax, dy
    REAL :: zmin, zmax, dz

    REAL, DIMENSION(:), ALLOCATABLE :: x, y, z

END MODULE globals