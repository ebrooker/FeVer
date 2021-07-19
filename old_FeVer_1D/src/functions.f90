MODULE FUNCTIONS
IMPLICIT NONE

CONTAINS

	FUNCTION diffusivity( il, ir, difs, x )
	    IMPLICIT NONE
	    REAL(KIND=8)                :: diffusivity
	    INTEGER(KIND=4), INTENT(IN) :: il,ir
	    REAL(KIND=8),    INTENT(IN) :: difs, x(:)
	    REAL(KIND=8)                :: xmid
	    IF ( il == ir) THEN
	        xmid = x(il)
	    ELSE
	        xmid = x(il) + 0.5d0 * (x(ir)-x(il))
	    ENDIF
	    diffusivity = difs * 0.5d0 * (TANH(3.d0*(xmid-2.2d0)) + 1.d0)
	END FUNCTION diffusivity

END MODULE FUNCTIONS