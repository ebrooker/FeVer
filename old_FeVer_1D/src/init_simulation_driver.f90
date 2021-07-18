SUBROUTINE INIT_SIMULATION_DRIVER
	USE GLOBALS
    IMPLICIT NONE
    CALL INIT_DRIVER_MESSAGE(0)
    CALL INIT_PARAMETERS_READ
    CALL INIT_STUDY_MODULE
    CALL ALLOC_ARRAYS
    CALL SET_ZEROS
    CALL INIT_DATA_MESH
    CALL INIT_DATA_VELOCITY
    CALL INIT_DATA_STATES
    CALL INIT_OUTPUT_SETTINGS
    CALL INIT_DRIVER_MESSAGE(1)
    RETURN
END SUBROUTINE INIT_SIMULATION_DRIVER

SUBROUTINE INIT_DRIVER_MESSAGE( int )
	IMPLICIT NONE
	integer(kind=4), intent(in) :: int
	IF ( int .eq. 0 ) THEN
		PRINT *, ""
	    PRINT *, "[SIMULATION DRIVER]: Setting up simulation"
	ELSE
    	PRINT *, "[SIMULATION DRIVER]: Finished setting up"
    ENDIF
    RETURN
END SUBROUTINE INIT_DRIVER_MESSAGE

SUBROUTINE INIT_STUDY_MODULE
	USE GLOBALS
    USE TESTS
	IMPLICIT NONE
	IF ( convtestflag .and. .not. readflag ) THEN
		CALL CONVERGENCE_STUDY
		CALL FEVER_MESSAGE
		STOP
	ELSEIF ( sqpltestflag .and. .not. readflag ) THEN
		CALL SQUARE_PULSE_STUDY
		CALL FEVER_MESSAGE
		STOP
	ELSEIF ( adrtestflag .and. .not. readflag ) THEN
		CALL ADR_STUDY
		CALL FEVER_MESSAGE
		STOP
	ENDIF
END SUBROUTINE INIT_STUDY_MODULE

SUBROUTINE INIT_OUTPUT_SETTINGS
	USE GLOBALS
	IMPLICIT NONE

	IF ( monoflag .and. interpflag .eq. 'ppm' ) THEN
		IF ( nx > 0 .and. nx < 10 ) THEN
			WRITE(filename, '("../data/sim_n",I1.1,"_",A3,"_monotonized.txt")') nx, TRIM(interpflag)
		ELSE IF ( nx > 9 .and. nx < 100 ) THEN
			WRITE(filename, '("../data/sim_n",I2.2,"_",A3,"_monotonized.txt")') nx, TRIM(interpflag)
		ELSE IF ( nx > 99 .and. nx < 1000 ) THEN
			WRITE(filename, '("../data/sim_n",I3.3,"_",A3,"_monotonized.txt")') nx, TRIM(interpflag)
		ELSE IF ( nx > 999 .and. nx < 10000 ) THEN
			WRITE(filename, '("../data/sim_n",I4.4,"_",A3,"_monotonized.txt")') nx, TRIM(interpflag)
		ENDIF
	ELSE
		IF ( nx > 0 .and. nx < 10 ) THEN
			WRITE(filename, '("../data/sim_n",I1.1,"_",A3,".txt")') nx, TRIM(interpflag)
		ELSE IF ( nx > 9 .and. nx < 100 ) THEN
			WRITE(filename, '("../data/sim_n",I2.2,"_",A3,".txt")') nx, TRIM(interpflag)
		ELSE IF ( nx > 99 .and. nx < 1000 ) THEN
			WRITE(filename, '("../data/sim_n",I3.3,"_",A3,".txt")') nx, TRIM(interpflag)
		ELSE IF ( nx > 999 .and. nx < 10000 ) THEN
			WRITE(filename, '("../data/sim_n",I4.4,"_",A3,".txt")') nx, TRIM(interpflag)
		ENDIF
	ENDIF

!!----- TO LOGFILE ------!!

	OPEN(UNIT=19,FILE=filename,STATUS="UNKNOWN")
	WRITE(19,*) "!!---------------------------!!"
	WRITE(19,*) "!!--- SIMULATION SETTINGS ---!!"
	WRITE(19,*) "!!---------------------------!!"

	WRITE(19,*) ""
	WRITE(19,*) "!!--- Cell Parameters ---!!"
	WRITE(19,'(" nx = ", I4)') nx
	WRITE(19,'(" ng = ", I4)') ng
	WRITE(19,'(" nl = ", I4)') nl
	WRITE(19,'(" nr = ", I4)') nr
	WRITE(19,'(" nmax = ", I5)') nmax
	
	WRITE(19,*) ""
	WRITE(19,*) "!!--- Domain Parameters ---!!"
	WRITE(19,*) "gridflag = ", gridflag
	
	WRITE(19,*) "bcflag = ", bcflag
	WRITE(19,'(" xmin =", ES10.3)') xmin
	WRITE(19,'(" xmax =", ES10.3)') xmax

	WRITE(19,*) ""
	WRITE(19,*) "!!--- Evolution Parameters ---!!"
	WRITE(19,*) "interpflag = ", interpflag
	WRITE(19,'(" monoflag = ", L1)') monoflag

	WRITE(19,'(" cfl  =", ES10.3)') cfl
	WRITE(19,'(" tmax =", ES10.3)') tmax
	WRITE(19,'(" vel  =", ES10.3)') v

	WRITE(19,*) ""
	WRITE(19,*) "!!--- Data Parameters ---!!"
	WRITE(19,'(" readflag = ", L1)') readflag
	IF ( readflag ) THEN
		WRITE(19,*) "Initial conditions read from files..."
		WRITE(19,*) "rhofn = ", rhofn
		WRITE(19,*) "velfn = ", velfn
		WRITE(19,*) "posfn = ", posfn
	ELSE
		WRITE(19,*) "Using preset initial conditions"
		WRITE(19,*) "icflag = ", icflag
	ENDIF
	WRITE(19,*) ""
	WRITE(19,*) "|=====================================================================|"
	WRITE(19,*) "|                         INITIAL DATA STATES                         |"
	WRITE(19,*) "|=====================================================================|"
	WRITE(19,*) "|------------|------------------|---------------------|---------------|"
	WRITE(19,*) "|--- cell ---|--- x-position ---|--- cell velocity ---|--- density ---|"
	WRITE(19,*) "|------------|------------------|---------------------|---------------|"
	DO i=nl,nr
		WRITE(19,'(" | ",3x, I4, 4x,"|",4x, ES10.3, 4x,"|",5x, ES10.3, 6x,"|",2x, ES10.3, 3x,"|")') i-ng, x(i), u(i), c(i)
	ENDDO
	WRITE(19,*) "|=====================================================================|"
	WRITE(*,*) ""
	CLOSE(UNIT=19)

!!----- TO SCREEN ------!!

	WRITE(*,*) ""
	WRITE(*,*) "!!---------------------------!!"
	WRITE(*,*) "!!--- SIMULATION SETTINGS ---!!"
	WRITE(*,*) "!!---------------------------!!"

	WRITE(*,*) ""
	WRITE(*,*) "!!--- Cell Parameters ---!!"
	WRITE(*,'(" nx = ", I4)') nx
	WRITE(*,'(" ng = ", I4)') ng
	WRITE(*,'(" nl = ", I4)') nl
	WRITE(*,'(" nr = ", I4)') nr
	WRITE(*,'(" nmax = ", I5)') nmax
	
	WRITE(*,*) ""
	WRITE(*,*) "!!--- Domain Parameters ---!!"
	WRITE(*,*) "gridflag = ", gridflag
	
	WRITE(*,*) "bcflag = ", bcflag
	WRITE(*,'(" xmin =", ES10.3)') xmin
	WRITE(*,'(" xmax =", ES10.3)') xmax

	WRITE(*,*) ""
	WRITE(*,*) "!!--- Evolution Parameters ---!!"
	WRITE(*,*) "interpflag = ", interpflag
	WRITE(*,'(" monoflag = ", L1)') monoflag

	WRITE(*,'(" cfl  =", ES10.3)') cfl
	WRITE(*,'(" tmax =", ES10.3)') tmax
	WRITE(*,'(" vel  =", ES10.3)') v

	WRITE(*,*) ""
	WRITE(*,*) "!!--- Data Parameters ---!!"
	WRITE(*,'(" readflag = ", L1)') readflag
	IF ( readflag ) THEN
		WRITE(*,*) "Initial conditions read from files..."
		WRITE(*,*) "rhofn = ", rhofn
		WRITE(*,*) "velfn = ", velfn
		WRITE(*,*) "posfn = ", posfn
	ELSE
		WRITE(*,*) "Using preset initial conditions"
		WRITE(*,*) "icflag = ", icflag
	ENDIF
	WRITE(*,*) ""
	WRITE(*,*) "|=====================================================================|"
	WRITE(*,*) "|                         INITIAL DATA STATES                         |"
	WRITE(*,*) "|=====================================================================|"
	WRITE(*,*) "|------------|------------------|---------------------|---------------|"
	WRITE(*,*) "|--- cell ---|--- x-position ---|--- cell velocity ---|--- density ---|"
	WRITE(*,*) "|------------|------------------|---------------------|---------------|"
	DO i=nl,nr
		WRITE(*,'(" | ",3x, I4, 4x,"|",4x, ES10.3, 4x,"|",5x, ES10.3, 6x,"|",2x, ES10.3, 3x,"|")') i-ng, x(i), u(i), c(i)
	ENDDO
	WRITE(*,*) "|=====================================================================|"
	WRITE(*,*) ""
	RETURN
END SUBROUTINE INIT_OUTPUT_SETTINGS

SUBROUTINE FEVER_MESSAGE
	IMPLICIT NONE
	WRITE(*,*)""
	WRITE(*,*)"" 
	WRITE(*,*)"    |============================================|"
	WRITE(*,*)"    |            THANK YOU FOR USING             |"
	WRITE(*,*)"    |                                            |"
	WRITE(*,*)"    |  FFFFFFF        VV       VV                |"	
	WRITE(*,*)"    |  F        EEEE   VV     VV  EEEE    RRRR   |"
	WRITE(*,*)"    |  FFFFF   E    E   VV   VV  E    E  R    R  |"
	WRITE(*,*)"    |  F       EEEEE     VV VV   EEEEE   R    R  |"
	WRITE(*,*)"    |  F       E          VVV    E       R       |"
	WRITE(*,*)"    |  F       EEEEEE      V     EEEEEE  R       |"
	WRITE(*,*)"    |                                            |"
	WRITE(*,*)"    |            FinitE VolumE solveR            |"
	WRITE(*,*)"    |============================================|"
	WRITE(*,*)""
	WRITE(*,*)""
END SUBROUTINE FEVER_MESSAGE
