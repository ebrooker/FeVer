MODULE global_constants
USE kind_settings
IMPLICIT NONE


    REAL(wp), PARAMETER :: ZERO = 0.0e0_wp
    REAL(wp), PARAMETER :: ONE  = 1.0e0_wp
    REAL(wp), PARAMETER :: TWO  = 2.0e0_wp
    REAL(wp), PARAMETER :: FOUR = 4.0e0_wp
    REAL(wp), PARAMETER :: HALF = 0.5e0_wp
    REAL(wp), PARAMETER :: PI   = FOUR * ATAN(ONE)

END MODULE global_constants