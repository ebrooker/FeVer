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

		        !! RECONSTRUCT HYDROSTATE
		        CALL RECONSTRUCT_HYDROSTATE ( c, cl, cr  )
		        
		        !! COMPUTE FLUXES
		        CALL COMPUTE_FLUXES ( cl, cr, f )

		        !! UPDATE STATE
		        CALL UPDATE_HYDROSTATE ( f, c )

		        !CALL OUTPUT_STATE
		        !CALL OUTPUT_STEP

		        !PRINT *, cfl, nx, t, dt, step
		             
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


subroutine print_norms_and_rates( norm, mnri, mnrf, mcoi, mcof )

  use globals

  ! formal arguments

  real(kind=8), intent(in) :: norm(mnri:mnrf,mcoi:mcof,2)
  integer(kind=4), intent(in) :: mnri, mnrf, mcoi, mcof

  ! local variables

  integer(kind=4) inorm, inr, ico

  do inorm = 1,2

     write(*,*)
     write(*,'(a)')      '###########'
     write(*,'(a,i1,a)') '# Norm L',inorm,' #'
     write(*,'(a)')      '###########'

     do inr = mnri,mnrf
        write(*,'(i4,10es10.2)') nx_base*(2**(inr-1)),(norm(inr,ico,inorm),ico=mcoi,mcof)
     end do

     if ( mcof > 1 ) then

        write(*,'(/a)') 'time convergence rates at fixed spatial resolution'

        do inr = mnri,mnrf
           write(*,'(i4,10f10.2)') nx_base*(2**(inr-1)), (log(norm(inr,ico,inorm)/norm(inr,ico+1,inorm))/log(2.e0),ico=mcoi,mcof-1)
        end do

     end if

     if ( mnrf > mnri ) then

        write(*,'(/a)') 'spatial convergence rates at variable temporal resolution (fixed Courant factor)'
        write(*,'( a)') 'CFL      nr'
        write(*,'(9x,10i10)') (nx_base*(2**(inr-1)),inr=mnri,mnrf)

        do ico = mcoi,mcof
           write(*,'(es9.2,5x,10f10.2)') &
             1.d0 / 2.d0**(ico-1), (log(norm(inr,ico,inorm)/norm(inr+1,ico,inorm))/log(2.e0),inr=mnri,mnrf-1)
        end do

     end if

  end do

end subroutine print_norms_and_rates