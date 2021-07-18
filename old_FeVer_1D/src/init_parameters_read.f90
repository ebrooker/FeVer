SUBROUTINE INIT_PARAMETERS_READ
	USE GLOBALS
	USE INIT_SIMULATION_INTERFACE, ONLY: INIT_PARAMETER_GET
	IMPLICIT NONE

	INTEGER(KIND=4) :: ncount, erra, io
	CHARACTER(LEN=MAXCHARS), DIMENSION(:,:), ALLOCATABLE :: lines
	CHARACTER(LEN=MAXCHARS)                              :: cmmts,words(3)

 	OPEN(UNIT=25, FILE="fever.par", STATUS="OLD", ACTION="READ",IOSTAT=io)

	nlines = 0
	ncomments = 0

    DO
        READ(25,'(A)',iostat=eofstat) cmmts
        IF (eofstat .ne. 0) EXIT
        nlines = nlines + 1
        IF (cmmts(1:1) .eq. '#' .or. cmmts(1:1) .eq. '!') THEN
                ncomments = ncomments + 1
        ENDIF
    ENDDO

    ALLOCATE(lines(nlines - ncomments,2))
    CLOSE(UNIT=25)
    OPEN(UNIT=25, FILE="fever.par", STATUS="OLD", ACTION="READ",IOSTAT=io)
    DO WHILE ( ncount .lt. nlines )
        READ(25,*,iostat=eofstat) words
        
        IF ( eofstat .ne. 0 ) EXIT
        IF ( words(1)(1:1) .ne. '#' .and. words(1)(1:1) .ne. '!' ) THEN
            ncount = ncount + 1
            lines(ncount,1) = TRIM(words(1))
            lines(ncount,2) = TRIM(words(3))
        ENDIF
    ENDDO
    CLOSE(UNIT=25)

    !! Look for testing flags first [ALWAYS]
    CALL INIT_PARAMETER_GET(lines,'adrtestflag', adrtestflag, .false.)
    CALL INIT_PARAMETER_GET(lines,'convtestflag',convtestflag,.false.)
    CALL INIT_PARAMETER_GET(lines,'sqpltestflag',sqpltestflag,.false.)

    !! call in parameters, # mesh zones, domain bounds, cfl condition, simulation end time, etc.

    CALL INIT_PARAMETER_GET(lines,'nx',nx,16)
    CALL INIT_PARAMETER_GET(lines,'nx_base',nx_base,8)
    CALL INIT_PARAMETER_GET(lines,'nmax',nmax,99999)
    CALL INIT_PARAMETER_GET(lines,'tmax',tmax,2.0d0)
    CALL INIT_PARAMETER_GET(lines,'xmin',xmin,0.0d0)
    CALL INIT_PARAMETER_GET(lines,'xmax',xmax,1.0d0)
    CALL INIT_PARAMETER_GET(lines,'difs',difs,0.0d0)
    CALL INIT_PARAMETER_GET(lines,'cfl',cfl,0.5d0)
    CALL INIT_PARAMETER_GET(lines,'v',v,0.5d0)

    CALL INIT_PARAMETER_GET(lines,'theta',theta,0.d0)

    CALL INIT_PARAMETER_GET(lines,'sourceA',sourceA,0.d0)
    CALL INIT_PARAMETER_GET(lines,'sourceTi',sourceTi,0.d0)
    CALL INIT_PARAMETER_GET(lines,'sourceTf',sourceTf,1.d0)
    CALL INIT_PARAMETER_GET(lines,'sourceX',sourceX,0.8d0)


    !! call in all flags before setting initial data of the simulation, need to know if using 
    !! presets provided for testing or if user is providing data files for the simulation.

    CALL INIT_PARAMETER_GET(lines,'readflag',readflag,.false.)
    CALL INIT_PARAMETER_GET(lines,'bcflag',bcflag,'periodic')
    CALL INIT_PARAMETER_GET(lines,'interpflag',interpflag,'ppm')
    CALL INIT_PARAMETER_GET(lines,'monoflag',monoflag,.true.)
   
    CALL INIT_PARAMETER_GET(lines,'useImplicitSolve',useImplicitSolve,.true.) 

    !! call in any data files used if readflag==0
    IF ( readflag ) THEN
        CALL INIT_PARAMETER_GET(lines,'rhofn',rhofn,'density.dat')
        CALL INIT_PARAMETER_GET(lines,'velfn',velfn,'velocity.dat')
        CALL INIT_PARAMETER_GET(lines,'posfn',posfn,'position.dat')
    ELSE
        CALL INIT_PARAMETER_GET(lines,'icflag',icflag,'sine')
        CALL INIT_PARAMETER_GET(lines,'velflag',velflag,'unpos')
        CALL INIT_PARAMETER_GET(lines,'gridflag',gridflag,'ug')
        CALL INIT_PARAMETER_GET(lines,'difsflag',difsflag,'const')
        CALL INIT_PARAMETER_GET(lines,'sourceflag',sourceflag,'delta')
    ENDIF

    !! using the interpflag, set the number of ghost cells needed
    !! then set left bound and right bound indices for real data
    IF ( TRIM(interpflag) .eq. 'pcm' ) THEN
        ng = 2
    ELSE IF (interpflag .eq. 'plm') THEN
        ng = 3
    ELSE IF (interpflag .eq. 'ppm') THEN
        ng = 4
    ENDIF
    nl = 1+ng
    nr = nx+ng


    DEALLOCATE(lines)
    RETURN
END SUBROUTINE INIT_PARAMETERS_READ
