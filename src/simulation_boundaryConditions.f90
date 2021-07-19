SUBMODULE (simulation_class) simulation_boundaryConditions

CONTAINS

    MODULE SUBROUTINE boundary_conditions(values, ng, nx, bc_type)
        REAL(wp),         INTENT(INOUT) :: values(:)
        INTEGER(smInt),   INTENT(IN   ) :: ng,nx
        CHARACTER(LEN=*), INTENT(IN   ) :: bc_type


        SELECT CASE(TRIM(bc_type))

        CASE("periodic")
            values(1:ng)            = values(nx+1:nx+ng) !! Left guard cells
            values(nx+ng+1:nx+2*ng) = values(ng+1:2*ng)

        CASE DEFAULT
            STOP "[BoundaryConditions] Unknown Boundary Condition. Aborting..."

        END SELECT
        
    END SUBROUTINE boundary_conditions

END SUBMODULE simulation_boundaryConditions