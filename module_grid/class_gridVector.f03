MODULE class_gridVector
    USE class_vector
    IMPLICIT NONE
    PRIVATE

    PUBLIC gridVector

    TYPE :: gridVector
        TYPE(vector) :: nodes, centers, delta, delta2
        INTEGER      :: n,m
        CONTAINS
            PROCEDURE :: init => gridVector_init
            PROCEDURE :: del => gridVector_delete
    END TYPE gridVector

    CONTAINS

        SUBROUTINE gridVector_init(this,n,lower,upper)
            CLASS(gridVector)   :: this
            REAL, INTENT(IN)    :: lower, upper
            INTEGER, INTENT(IN) :: n
            this%n = n
            this%m = n+1
            CALL this%nodes%init(this%m)
            CALL this%delta%init(this%m-1)
            CALL this%delta2%init(this%n-1)
            CALL this%centers%init(this%n)

            CALL this%nodes%linspace(lower,upper)
            this%delta%vec = this%nodes%diff()
            this%centers%vec = this%nodes%cca()
            this%delta2%vec = this%centers%diff()

        END SUBROUTINE gridVector_init

        SUBROUTINE gridVector_delete(this)
            CLASS(gridVector) :: this
            this%n = 0
            this%m = 0
            CALL this%nodes%del
            CALL this%delta%del
            CALL this%delta2%del
            CALL this%centers%del
        END SUBROUTINE gridVector_delete

END MODULE class_gridVector