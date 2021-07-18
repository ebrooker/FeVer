SUBROUTINE DIFFUSION_IMPLICIT ( sol )
    USE GLOBALS, ONLY: i,nx,ng,nl,nr,dt,dx,u,theta,x,difs,bcflag
    USE LINEAR_SOLVERS
    IMPLICIT NONE
    INTEGER(KIND=4)             :: nn
    REAL(KIND=8), INTENT(INOUT) :: sol(nx+2*ng)
    REAL(KIND=8), DIMENSION(nx) :: a, b, c, d, xsol
    REAL(KIND=8)                :: coeff1,coeff2,delta(0:nx+1),diffusivity

!! Need to setup for coeff1(i-1), coeff1(i), and coeff(i+1)

	DO i = 0,nx+1
		nn = ng + i
		delta(i) = diffusivity(nn-1,nn) * dt / dx(nn)**2.d0
	ENDDO
	
	CALL BOUNDARY_CONDITIONS ( sol )

    DO i = 1,nx
    	nn = ng + i
    	!! Build coefficients

    	!! Build diffusion linear operator
    	a(i) = -theta*delta(i-1)
    	b(i) = 1.d0 + theta*delta(i)
    	c(i) = theta*delta(i+1)

    	!! Build RHS vector from previous timestep solution
    	d(i) = ((1.d0 - theta)*delta(i-1)) * sol(nn-1)  &
    	       + (1.d0 - (1.d0-theta)*delta(i)) * sol(nn) &
    	       + ((1.d0 - theta)*delta(i+1)) * sol(nn+1)
    ENDDO


    xsol = 0.d0
    IF ( TRIM(bcflag) .eq. 'periodic' ) THEN
    	CALL SHERMAN_MORRISON ( nx, a, b, c, d, xsol )
        !CALL tricyc(a,b,c,d,nx)
    ELSE
    	CALL TDMA ( nx, a, b, c, d, xsol )
        !CALL tricyc(a,b,c,d,nx)
    ENDIF

    do i = nl,nr
        nn = i-ng
        sol(i) = xsol(nn)
    enddo

END SUBROUTINE DIFFUSION_IMPLICIT

