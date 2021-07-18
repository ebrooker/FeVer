REAL(KIND=8) FUNCTION diffusivity( il, ir )
    USE GLOBALS, ONLY: difs, xmin, xmax, x
    IMPLICIT NONE

    INTEGER(KIND=4), INTENT(IN) :: il,ir
    REAL(KIND=8)                :: xmid

    IF ( il == ir) THEN
        xmid = x(il)
    ELSE
        xmid = x(il) + 0.5d0 * (x(ir)-x(il))
    ENDIF

    diffusivity = difs * 0.5d0 * (TANH(3.d0*(xmid-2.2d0)) + 1.d0)

END FUNCTION diffusivity