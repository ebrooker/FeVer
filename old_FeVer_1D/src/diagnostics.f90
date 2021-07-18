SUBROUTINE TOT_VAR_DIM ( st )
	USE GLOBALS
	IMPLICIT NONE
	REAL(KIND=8), INTENT(IN) :: st(nx*2*ng)

	tvd = ABS(st(nr)-st(1))
	DO i = nl,nr-1
		tvd = tvd + ABS(st(i+1) - st(i))
	ENDDO
	RETURN
END SUBROUTINE TOT_VAR_DIM



! TOMEK's CODE

subroutine print_norms_and_rates( norm, mnri, mnrf, mcoi, mcof )
  use globals
  real(kind=8), intent(in) :: norm(mnri:mnrf,mcoi:mcof,2)
  integer(kind=4), intent(in) :: mnri, mnrf, mcoi, mcof
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