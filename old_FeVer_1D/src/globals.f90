MODULE GLOBALS
    IMPLICIT NONE

    REAL(KIND=8),    PARAMETER              :: PI = 4.d0*ATAN(1.d0),dtmin=1.d-6, dtmax=1.d-1
    CHARACTER(LEN=200)                      :: filename,velfn,rhofn,posfn
    CHARACTER(LEN=64)                       :: nxstr
    CHARACTER(LEN=32)                       :: line(3)
    CHARACTER(LEN=32)                       :: icflag,bcflag,interpflag,gridflag,velflag,difsflag,sourceflag

    REAL(KIND=8)                            :: xmin,xmax,cfl,difs,tmax,t,dt,l1,l2,tvd,v
    REAL(KIND=8)                            :: sourceA,sourceX,sourceTi,sourceTf,theta
    REAL(KIND=8), DIMENSION(:), ALLOCATABLE :: c,c0,cl,cr,c_init,f,u,x,dx,tmpvar
    REAL(KIND=8), DIMENSION(:), ALLOCATABLE :: sources

    INTEGER(KIND=4)                         :: nx,ng,nl,nr,nmax,ndim,nlines,ncomments,nx_base
    INTEGER(KIND=4)                         :: step,i,j,k
    INTEGER(KIND=4)                         :: IOstatus,eofstat

    INTEGER(KIND=4), PARAMETER              :: MAXCHARS=2000

    LOGICAL                                 :: monoflag,readflag,convtestflag,sqpltestflag,adrtestflag,useImplicitSolve

    CONTAINS
    SUBROUTINE ALLOC_ARRAYS
        IMPLICIT NONE
            PRINT *, "[ARRAYS]: Allocating"
            ALLOCATE(sources(nx+2*ng), source=0.d0)
            ALLOCATE(c_init(nx+2*ng),  source=0.d0)
            ALLOCATE(tmpvar(nx+2*ng),  source=0.d0)
            ALLOCATE(dx(nx+2*ng),      source=0.d0)
            ALLOCATE(cl(nx+2*ng),      source=0.d0)
            ALLOCATE(cr(nx+2*ng),      source=0.d0)
            ALLOCATE(c0(nx+2*ng),      source=0.d0)
            ALLOCATE(x(nx+2*ng),       source=0.d0)
            ALLOCATE(c(nx+2*ng),       source=0.d0)
            ALLOCATE(f(nx+2*ng),       source=0.d0)
            ALLOCATE(u(nx+2*ng),       source=0.d0)
            RETURN
    END SUBROUTINE ALLOC_ARRAYS

    SUBROUTINE SET_ZEROS
        IMPLICIT NONE
            PRINT *, "[ARRAYS]: Initializing to zero"
            sources = 0.d0
            c_init = 0.d0
            tmpvar = 0.d0
            cl = 0.d0
            cr = 0.d0
            c0 = 0.d0
            dx = 0.d0
            x = 0.d0
            c = 0.d0
            f = 0.d0
            u = 0.d0
            RETURN
    END SUBROUTINE SET_ZEROS

    SUBROUTINE DEALLOC_ARRAYS
        IMPLICIT NONE
            PRINT *, "[ARRAYS]: Deallocating"
            DEALLOCATE(sources)
            DEALLOCATE(c_init)
            DEALLOCATE(tmpvar)
            DEALLOCATE(cl)
            DEALLOCATE(cr)
            DEALLOCATE(c0)
            DEALLOCATE(dx)
            DEALLOCATE(x) 
            DEALLOCATE(c)
            DEALLOCATE(f)
            DEALLOCATE(u)
            RETURN
    END SUBROUTINE DEALLOC_ARRAYS
END MODULE GLOBALS
