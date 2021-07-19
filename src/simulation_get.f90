SUBMODULE (simulation_class) simulation_get

CONTAINS

    SUBROUTINE get(nl, nr, state_vals, temp_vals)
        INTEGER(smInt), INTENT(IN   ) :: nl,nr
        REAL(wp),       INTENT(IN   ) :: state_vals(:)
        REAL(wp),       INTENT(INOUT) :: temp_vals(:)

        temp_vals(nl:nr) = state_vals(:)

    END SUBROUTINE get

END SUBMODULE simulation_get