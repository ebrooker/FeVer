MODULE TESTS
IMPLICIT NONE

CONTAINS
SUBROUTINE CONVERGENCE_STUDY
	USE GLOBALS
	IMPLICIT NONE
	INTEGER(KIND=4),PARAMETER :: ti=2,tf=7,ri=2,rf=7
	INTEGER(KIND=4)           :: mt,mr
	REAL(KIND=8)              :: norms(ri:rf,ti:tf,2)
	LOGICAL                   :: exist

	PRINT *, "[CONVERGENCE STUDY]: Initiating spatio-temporal convergence study"
	! temporal convergence
	gridflag = 'ug'
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
		    DO WHILE (t < tmax .and. step < nmax)
		        !! FILL IN GUARD CELLS, LHS, THEN, RHS
		        CALL BOUNDARY_CONDITIONS( c )
		        !! GET TIMESTEP
		        CALL TIMESTEP
		        !! ADVANCE STEPS AND COPY OLD SOLUTION
		        step = step + 1
		        t = t + dt
		        c0 = c
		        
                !! EVOLVE
                CALL EVOLVE
			ENDDO

			IF ( monoflag .and. interpflag .eq. 'ppm' ) THEN
		        WRITE(nxstr,'(I2)') nx
		        filename = '../data/final_advect_state_'//TRIM(nxstr)//'_'//TRIM(interpflag)//"_mono"//".txt"
		    ELSE
		        WRITE(nxstr,'(I2)') nx
		        filename = '../data/final_advect_state_'//TRIM(nxstr)//'_'//TRIM(interpflag)//".txt"
		    ENDIF
		    OPEN(UNIT=16,FILE=filename,STATUS='UNKNOWN')
		    DO k = nl,nr
		        WRITE(16,*) x(k),c(k)
		    ENDDO
		    CLOSE(UNIT=16)

			! calculate L1 and L2 norms
			norms(mr,mt,1) = SUM(dx(nl:nr)*ABS((c(nl:nr)-c_init(nl:nr))))
    		norms(mr,mt,2) = SQRT(SUM(dx(nl:nr)*(c(nl:nr)-c_init(nl:nr))**2))
    		CALL DEALLOC_ARRAYS

    		IF ( monoflag .and. interpflag .eq. 'ppm' ) THEN
	    		filename = '../data/convergence_study_'//TRIM(interpflag)//"_monotonized_"//TRIM(icflag)//"_"//TRIM(bcflag)//".txt"
	    	ELSE
	    		filename = '../data/convergence_study_'//TRIM(interpflag)//"_"//TRIM(icflag)//"_"//TRIM(bcflag)//".txt"
	    	ENDIF
			INQUIRE(file=filename,exist=exist)
    			IF (exist) THEN
		    	OPEN(UNIT=21,FILE=filename,STATUS="OLD",POSITION="APPEND",ACTION="WRITE")
		    ELSE
		    	OPEN(UNIT=21,FILE=filename,STATUS="NEW",ACTION="WRITE")
		    	WRITE(21,*) '|--- nx ---|--- L1 ---|--- L2 ---|'
		    ENDIF
		    WRITE(21,"(1x,'|',3x,I0.4,3x,'|',1x,ES8.2,1x,'|',1x,ES8.2,1x,'|',1x,ES8.2,1x,'|')") nx, norms(mr,mt,1), norms(mr,mt,2), cfl
		    CLOSE(UNIT=21)
		    WRITE(*, "(1x,'|',3x,I0.4,3x,'|',1x,es8.2,1x,'|',1x,ES8.2,1x,'|',1x,ES8.2,1x,'|')") nx, norms(mr,mt,1), norms(mr,mt,2), cfl
		ENDDO
	ENDDO
	CALL PRINT_NORMS_AND_RATES(	norms, ri, rf, ti, tf )
END SUBROUTINE CONVERGENCE_STUDY


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
                
                !! EVOLVE
                CALL EVOLVE

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
                ENDIF
                WRITE(21,"(I6,1x,I6,1x,ES14.8,1x,ES14.8,1x)") nx,cellcount,t,cfl
                CLOSE(UNIT=21)
                WRITE(*,"(1x,'|',3x,I0.4,3x,'|',4x,I0.4,4x,'|',1x,ES10.4,1x,'|',1x,ES10.4,1x,'|')") nx,cellcount,t,cfl
            ENDDO
            WRITE(*,*) '|--- nx ---|--- numc ---|--- time ---|--- cfls ---|'
            WRITE(*,*)

            IF ( monoflag .and. interpflag .eq. 'ppm' ) THEN
                filename = '../data/final_advect_state_'//TRIM(nxstr)//'_'//TRIM(interpflag)//"_monot_"//TRIM(icflag)//"_"//TRIM(bcflag)//".txt"
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


SUBROUTINE ADR_STUDY
    USE GLOBALS
    IMPLICIT NONE

    INTEGER(KIND=4),PARAMETER :: ti=1,tf=3,ri=3,rf=5
    INTEGER(KIND=4)           :: mt,mr,mc,cellcount
    REAL(KIND=8)              :: tvds(ri:rf,ti:tf),cfls(ti:tf)
    CHARACTER(LEN=99)         :: adrfname
    LOGICAL                   :: exist

    PRINT *, "[SQUARE PULSE STUDY]: Initiating spatio-temporal square pulse study"
    ! temporal convergence
    gridflag = 'ug'
    icflag = 'square_pulse_adr'
    xmin = 0.d0
    xmax = 2.d0 * PI
    tmax = 4.d0 * PI
    !tmax = 1.d0

    cfls(1) = 0.5d0
    cfls(2) = 0.95d0
    cfls(3) = 1.05d0

    DO mc = 1,5

        IF ( mc .eq. 1 ) THEN
            ! ADVECTION
            v = 1.d0
            difs = 0.d0
            sourceA = 0.d0

       ELSE  IF ( mc .eq. 2 ) THEN
            ! ADVECTION-DIFFUSION
            v = 1.d0
            difs = 0.03d0
            sourceA = 0.d0

        ELSE IF ( mc .eq. 3 ) THEN
            ! ADVECTION-DIFFUSION-REACTION
            v = 1.d0
            difs = 0.03d0
            sourceA = 5.d0

        ELSE IF ( mc .eq. 4 ) THEN
            ! DIFFUSION-REACTION
            v = 0.d0
            difs = 0.03d0
            sourceA = 5.d0

        ELSE IF (mc .eq. 5) THEN
            v = 1.d0
            difs = 0.018d0
            sourceA = 0.d0
        ELSE 
            RETURN
        ENDIF

        DO mt = ti,tf

            ! adjust time step limiter, cfl
            cfl = cfls(mt)
            ! spatial convergence
            DO mr = ri,rf
                
                ! adjust spatial resolution, nx
                nx = nx_base*(2**(mr-1))
                nl = 1+ng
                nr = nx+ng

                WRITE(nxstr,'(F5.3,3(A,F5.3),A,I0.3)') cfl, "_vel", v, "_kappa", difs, "_src", sourceA, "_nx",nx

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

                IF ( monoflag .and. interpflag .eq. 'ppm' ) THEN
                    filename = '../data/init_advect_state_'//TRIM(nxstr)//'_'//TRIM(interpflag)//"_monot_"//&
                    TRIM(icflag)//"_"//TRIM(bcflag)//".txt"
                ELSE
                    filename = '../data/init_advect_state_'//TRIM(nxstr)//'_'//TRIM(interpflag)//"_"//TRIM(icflag)//"_"//TRIM(bcflag)//".txt"
                ENDIF
                OPEN(UNIT=16,FILE=filename,STATUS='UNKNOWN')
                DO k = nl,nr
                    WRITE(16,*) x(k),c_init(k)
                ENDDO
                CLOSE(UNIT=16)

                DO WHILE (t < tmax .and. step < nmax)
                    !! FILL IN GUARD CELLS, LHS, THEN, RHS
                    CALL BOUNDARY_CONDITIONS( c )
                    !! GET TIMESTEP
                    CALL TIMESTEP
                    !! ADVANCE STEPS AND COPY OLD SOLUTION
                    step = step + 1
                    c0 = c

                    !! EVOLVE
                    CALL EVOLVE

                    IF ( mc .eq. 5 ) THEN
                        cellcount = 0 
                        DO k = nl,nr
    !                        print *, c(k)
                            IF (c(k) .gt. 1.05 .and. c(k) .lt. 1.95) THEN
                                cellcount =  cellcount + 1
                            ENDIF
    !                        print *, k-ng,c(k),f(k),v,difs,sourceA
                        ENDDO
                        PRINT *, "OUTPUT: ", cellcount, t, dt, v, difs, sourceA

                        filename = '../data/'//TRIM(interpflag)//'_'//TRIM(nxstr)//'_cellcount.txt'
                        INQUIRE(file=filename,exist=exist)
                        IF (exist) THEN
                            OPEN(UNIT=21,FILE=filename,STATUS="OLD",POSITION="APPEND",ACTION="WRITE")
                        ELSE
                            OPEN(UNIT=21,FILE=filename,STATUS="NEW",ACTION="WRITE")
                        ENDIF
                        WRITE(21,'(1i3,1x)') cellcount
                    ENDIF

                    IF ( monoflag .and. interpflag .eq. 'ppm' ) THEN
                        filename = '../data/adr_study_cfl'//TRIM(nxstr)//"_"//TRIM(interpflag)//"_monot_"//TRIM(icflag)//"_"//TRIM(bcflag)//".txt"
                    ELSE
                        filename = '../data/adr_study_cfl'//TRIM(nxstr)//"_"//TRIM(interpflag)//"_"//TRIM(icflag)//"_"//TRIM(bcflag)//".txt"
                    ENDIF
                    INQUIRE(file=filename,exist=exist)
                    IF (exist) THEN
                        OPEN(UNIT=21,FILE=filename,STATUS="OLD",POSITION="APPEND",ACTION="WRITE")
                    ELSE
                        OPEN(UNIT=21,FILE=filename,STATUS="NEW",ACTION="WRITE")
                    ENDIF

                    CALL TOT_VAR_DIM( c )
                    WRITE(21,"(I6,7(1x,ES14.8)1x,ES14.8,1x,ES14.8)") nx,t,dt,cfl,tvd,v,difs,sourceA
                    CLOSE(UNIT=21)

                    t = t + dt
                ENDDO

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
    ENDDO
    WRITE(*,*) ""
END SUBROUTINE ADR_STUDY


END MODULE TESTS
