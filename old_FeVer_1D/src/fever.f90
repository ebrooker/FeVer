PROGRAM fever
    USE GLOBALS
    IMPLICIT NONE

    CALL INIT_SIMULATION_DRIVER

    c_init = c
    OPEN(UNIT=17, FILE='../data/init_advect_state.txt')
    DO k = nl,nr
        WRITE(17,*) x(k),c_init(k)
    ENDDO    
    CLOSE(UNIT=17) 


    !! START EVOLVING SYSTEM
    t = 0.d0
    step = 0
    DO WHILE (t < tmax .and. step < nmax)
    	
        !! FILL IN GUARD CELLS, LHS, THEN, RHS
        CALL BOUNDARY_CONDITIONS( c )

        !======================================================================!

        !! GET TIMESTEP
        CALL TIMESTEP

        !======================================================================!

        !! ADVANCE STEPS AND COPY OLD SOLUTION
        step = step + 1
        t = t + dt
        c0 = c

        !======================================================================!

        !! EVOLVE
        CALL EVOLVE

        !======================================================================!


        !! WRITE OUT THE STATE SOLUTION
	    CALL OUTPUT_STATE     

        !! WRITE OUT PARTICULARS OF THE STEP
        CALL OUTPUT_STEP

        IF (readflag .neqv. .true.) THEN
            CALL EXACT(tmpvar)
            l1 = SUM(dx(nl)*ABS((c(nl:nr)-tmpvar(nl:nr))))
            l2 = SQRT(SUM(dx(nl)*(c(nl:nr)-tmpvar(nl:nr))**2))
            !WRITE(*,*) l1, l2, step, t,c(nl),tmpvar(nl)
            !WRITE(*,*) c(nl),tmpvar(nl), "|", c(nl+4),tmpvar(nl+4), "|", c(nr),tmpvar(nr)
        ENDIF  
	ENDDO

    !======================================================================!

    IF ( monoflag .and. interpflag .eq. 'ppm' ) THEN
        WRITE(nxstr,'(I0.4)') nx
        filename = '../data/final_advect_state_'//TRIM(nxstr)//'_'//TRIM(interpflag)//"_mono"//".txt"
    ELSE
        WRITE(nxstr,'(I0.4)') nx
        filename = '../data/final_advect_state_'//TRIM(nxstr)//'_'//TRIM(interpflag)//".txt"
    ENDIF
    OPEN(UNIT=16,FILE=filename,STATUS='UNKNOWN')
    DO k = nl,nr
        WRITE(16,*) x(k),c(k)
    ENDDO
    CLOSE(UNIT=16)

    l1 = SUM(dx(nl)*ABS((c(nl:nr)-c_init(nl:nr))))
    l2 = SQRT(SUM(dx(nl)*(c(nl:nr)-c_init(nl:nr))**2))

    WRITE(*,*) ""
    WRITE(*,*) "FOR nx:", nx
    WRITE(*,*) "L1:", l1
    WRITE(*,*) "L2:", l2
    print *, dx(nl)
    WRITE(*,*) ""
    CALL DEALLOC_ARRAYS

    WRITE(*,*) ""
    WRITE(*,*) "[SIMULATION DRIVER]: TERMINATING RUN"
    WRITE(*,*) ""
    CALL FEVER_MESSAGE

END PROGRAM fever