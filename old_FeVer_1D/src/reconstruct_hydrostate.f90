SUBROUTINE RECONSTRUCT_HYDROSTATE ( a, al, ar )
    USE GLOBALS, ONLY: nl,nr,nx,ng,i,j,k,dt,dx,u,interpflag,monoflag
    IMPLICIT NONE
    REAL(KIND=8), INTENT(IN)  :: a(nx+2*ng)
    REAL(KIND=8), INTENT(OUT) :: al(nx+2*ng), ar(nx+2*ng)
    REAL(KIND=8)              :: slope(nx+2*ng),ap(nx+2*ng),am(nx+2*ng)
    REAL(KIND=8)              :: minmod,maxmod,s1,s2,d0,dp

    !! RECONSTRUCT HYDROSTATE

    SELECT CASE (TRIM(interpflag))
    	CASE ('pcm')

	        ! Pam, PIECEWISE CONSTANT, DONOR CELL, MANY NAMES
	        ! INACCURATE BUT EASY TO CODE UP
	      	DO i = nl,nr+1
	        	al(i) = a(i-1)
	        	ar(i) = a(i)
	  		ENDDO

      !======================================================================!

    	CASE('plm')

	        ! PLM, USED THE SUPERBEE SLOPE LIMITER IN THIS TO GO ABOVE AND
	        ! BEYOND THE CALL OF PROGRAMMING
	        
	        !DO i = nl-1,nr+2
	        !	slope(i) = (a(i) - a(i-1)) / dx(nl)
	        !ENDDO

	        DO i = nl-1,nr+1
	        	s1 = minmod( (a(i+1) - a(i))/dx(nl), 2.d0*(a(i) - a(i-1))/dx(nl) )
	        	s2 = minmod( (2.d0*(a(i+1) - a(i))/dx(nl)), (a(i) - a(i-1))/dx(nl) )
	        	slope(i) = maxmod(s1,s2)
	        ENDDO

	        DO i = nl,nr+1
	        	al(i) = a(i-1) + 0.5d0*dx(nl)*(1.d0 - u(i)*(dt/dx(nl)))*slope(i-1)
	        	ar(i) = a(i) - 0.5d0*dx(nl)*(1.d0 + u(i)*(dt/dx(nl)))*slope(i)
	        ENDDO

      !======================================================================!

    	CASE('ppm')
	        ! PPM AS DESarIBED IN THE COLELLA+84 PAPER
	  	    DO i = nl-2,nr+1
				d0 = 0.5*(a(i+1) - a(i-1))
				dp = 0.5*(a(i+2) - a(i))

				IF ((a(i+1) - a(i)) * (a(i) - a(i-1)) > 0) THEN
				  d0 =SIGN(1.0d0,d0)*MIN(ABS(d0),&
				           2.0*ABS(a(i) - a(i-1)),2.0*ABS(a(i+1) - a(i)) )
				ELSE
				  d0 = 0.0
				ENDIF

				IF ((a(i+2) - a(i+1))*(a(i+1) - a(i)) > 0) THEN
				  dp = SIGN(1.0d0,dp)*MIN(ABS(dp),&
				            2.0*ABS(a(i+1) - a(i)),2.0*ABS(a(i+2) - a(i+1)) )
				ELSE
				  dp = 0.0
				ENDIF

				ap(i)   = 0.5*(a(i) + a(i+1)) - (1.0/6.0)*(dp-d0)
				am(i+1) = ap(i)
	        ENDDO

	        !======================================================================!
	        IF ( monoflag ) THEN
	          !! THIS IS THE MONOTONIZATION SCHEME EQN(1.10) FROM COLELLA+84 PPM
	          !! PAPER, A TOGGLE HAS BEEN PROVIDED FOR SELECTING IT ON/OFF
		        DO i = nl-1,nr+1
		        	IF ((ap(i) - a(i))*(a(i) - am(i)) <= 0.0) THEN
	              		am(i) = a(i) 
	            		ap(i) = a(i)

	          		ELSE IF ((ap(i)-am(i))*(a(i)-0.5*(am(i)+ap(i))) > (ap(i)-am(i))**2/6.0) THEN
	            		am(i) = 3.0*a(i) - 2.0*ap(i)

	            	ELSE IF (-(ap(i)-am(i))**2/6.0 > (ap(i)-am(i))*(a(i)-0.5*(am(i)+ap(i)))) THEN
	            		ap(i) = 3.0*a(i) - 2.0*am(i)
	            	ENDIF
	        	ENDDO
	        ENDIF

	        !======================================================================!

			DO i = nl, nr+1
				al(i) = ap(i-1)-0.5*u(i)*(dt/dx(nl))*((ap(i-1)-am(i-1))-(1.0-(2.0/3.0)*&
			                  u(i)*(dt/dx(nl)))*6.0*(a(i-1)-0.5*(am(i-1)+ap(i-1))))

				ar(i) = am(i)+0.5*u(i)*(dt/dx(nl))*((ap(i)-am(i))+(1.0-(2.0/3.0)*&
			                u(i)*(dt/dx(nl)))*6.0*(a(i)-0.5*(am(i)+ap(i))))

				!print *, ar(i), al(i)
			ENDDO
  	END SELECT
  	RETURN
END SUBROUTINE RECONSTRUCT_HYDROSTATE

!======================================================================!

FUNCTION minmod(a,b)
	IMPLICIT NONE
	REAL(KIND=8) :: a, b
	REAL(KIND=8) :: minmod
	IF (ABS(a) < ABS(b) .AND. a*b > 0.d0) THEN
    	minmod = a
	ELSE IF (ABS(b) < ABS(a) .AND. a*b > 0) THEN
    	minmod = b
	ELSE
    	minmod = 0.d0
	ENDIF
 	RETURN
END FUNCTION minmod

!======================================================================!

FUNCTION maxmod(a,b)
	IMPLICIT NONE
	REAL(KIND=8) :: a, b
	REAL(KIND=8) :: maxmod
	IF (ABS(a) > ABS(b) .AND. a*b > 0.d0) THEN
    	maxmod = a
	ELSE IF (ABS(b) > ABS(a) .AND. a*b > 0) THEN
    	maxmod = b
	ELSE
    	maxmod = 0.d0
	ENDIF 
	RETURN
END FUNCTION maxmod
