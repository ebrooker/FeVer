SUBMODULE (simulation_class) simulation_put

CONTAINS

    SUBROUTINE put(nl, nr, temp_vals, state_vals)
        INTEGER(smInt), INTENT(IN   ) :: nl,nr
        REAL(wp),       INTENT(IN   ) :: temp_vals(:)
        REAL(wp),       INTENT(INOUT) :: state_vals(:)

        state_vals(:) = temp_vals(nl:nr)

    END SUBROUTINE put

END SUBMODULE simulation_put