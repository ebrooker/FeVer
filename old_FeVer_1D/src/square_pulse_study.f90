SUBROUTINE SQUARE_PULSE_STUDY
	USE GLOBALS
	IMPLICIT NONE

	INTEGER(KIND=4),PARAMETER :: ti=2,tf=2,ri=5,rf=5
	INTEGER(KIND=4)           :: mt,mr,mc,cellcount
	REAL(KIND=8)              :: norms(ri:rf,ti:tf,2)
	CHARACTER(LEN=99)         :: sqplfname
	LOGICAL                   :: exist


	PRINT *, "[SQUARE PULSE STUDY]: Initiating spatio-temporal square pulse study"
	! temporal convergence

	gridflag = 'ug'
	icflag = 'square_pulse'
	tmax = 10.d0
	DO mt = ti,tf

		! adjust time step limiter, cfl
		cfl = 1.0d0/2.0d0**DBLE(mt-1)

		! spatial convergence
		DO mr = ri,rf

			! adjust spatial resolution, nx
			nx = nx_base*(2**(mr-1)) 

			nl = 1+ng
    		nr = nx+ng

			CALL ALLOC_ARRAYS
		    CALL SET_ZEROS
		    CALL INIT_DATA_MESH
		    CALL INIT_DATA_VELOCITY
		    CALL INIT_DATA_STATES
		    CALL INIT_OUTPUT_SETTINGS
		    
		    !! START EVOLVING SYSTEM
		    c_init = c
		    t = 0.d0
		    step = 0
		    WRITE(nxstr,'(I0.4)') nx
		    WRITE(*,*) '|--- nx ---|--- numc ---|--- time ---|--- cfls ---|'
		    DO WHILE (t < tmax .and. step < nmax)

		        !! FILL IN GUARD CELLS, LHS, THEN, RHS
		        CALL BOUNDARY_CONDITIONS( c )

		        !! GET TIMESTEP
		        CALL TIMESTEP

		        !! ADVANCE STEPS AND COPY OLD SOLUTION
		        step = step + 1
		        t = t + dt
		        c0 = c

		        !! RECONSTRUCT HYDROSTATE
		        CALL RECONSTRUCT_HYDROSTATE ( c, cl, cr  )
		        
		        !! COMPUTE FLUXES
		        CALL COMPUTE_FLUXES ( cl, cr, f )

		        !! UPDATE STATE
		        CALL UPDATE_HYDROSTATE ( f, c )

		        !CALL OUTPUT_STATE
		        !CALL OUTPUT_STEP

		        !PRINT *, cfl, nx, t, dt, step
		        cellcount = 0
		        DO k = nl,nr
		        	IF ( c(k) .gt. 1.05d0 .and. c(k) .lt. 1.95d0 ) THEN
		        		cellcount = cellcount + 1
		        	ENDIF
		        ENDDO

	       		IF ( monoflag .and. interpflag .eq. 'ppm' ) THEN
		    		filename = '../data/sqpl_study_'//TRIM(nxstr)//"_"//TRIM(interpflag)//"_monot_"//TRIM(icflag)//"_"//TRIM(bcflag)//".txt"
		    	ELSE
		    		filename = '../data/sqpl_study_'//TRIM(nxstr)//"_"//TRIM(interpflag)//"_"//TRIM(icflag)//"_"//TRIM(bcflag)//".txt"
		    	ENDIF
				INQUIRE(file=filename,exist=exist)
	    			IF (exist) THEN
			    	OPEN(UNIT=21,FILE=filename,STATUS="OLD",POSITION="APPEND",ACTION="WRITE")
			    ELSE
			    	OPEN(UNIT=21,FILE=filename,STATUS="NEW",ACTION="WRITE")
			    	!WRITE(21,*) '|--- nx ---|--- numc ---|--- time ---|--- cfls ---|'
			    ENDIF
			    !WRITE(21,"(1x,'|',3x,I0.4,3x,'|',4x,I0.4,4x,'|',1x,ES10.4,1x,'|',1x,ES10.4,1x,'|')") nx, cellcount,t,cfl
                            WRITE(21,"(I6,1x,I6,1x,ES14.8,1x,ES14.8,1x)") nx, cellcount,t,cfl
			    CLOSE(UNIT=21)
			    WRITE(*, "(1x,'|',3x,I0.4,3x,'|',4x,I0.4,4x,'|',1x,ES10.4,1x,'|',1x,ES10.4,1x,'|')") nx, cellcount,t,cfl

			ENDDO
			WRITE(*,*) '|--- nx ---|--- numc ---|--- time ---|--- cfls ---|'
			WRITE(*,*)

			IF ( monoflag .and. interpflag .eq. 'ppm' ) THEN
		        filename = '../data/final_advect_state_'//TRIM(nxstr)//'_'//TRIM(interpflag)//"_monot_"//&
		        TRIM(icflag)//"_"//TRIM(bcflag)//".txt"
		    ELSE
		        filename = '../data/final_advect_state_'//TRIM(nxstr)//'_'//TRIM(interpflag)//"_"//TRIM(icflag)//"_"//TRIM(bcflag)//".txt"
		    ENDIF
		    OPEN(UNIT=16,FILE=filename,STATUS='UNKNOWN')
		    DO k = nl,nr
		        WRITE(16,*) x(k),c(k)
		    ENDDO
		    CLOSE(UNIT=16)
    		CALL DEALLOC_ARRAYS

		ENDDO
		WRITE(*,*) ""
	ENDDO
	WRITE(*,*) ""
END SUBROUTINE SQUARE_PULSE_STUDY
