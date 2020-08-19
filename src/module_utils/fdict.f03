! Fortran string dictionary


MODULE fdict
    IMPLICIT NONE
    INTEGER, PARAMETER :: tbl_size = 50

    TYPE sllist
        TYPE(sllist), POINTER :: child=>NULL()
        CHARACTER(len=:), ALLOCATABLE :: key,val

        CONTAINS
            PROCEDURE :: put  => put_sll
            PROCEDURE :: get  => get_sll
            PROCEDURE :: free => free_sll
    END TYPE sllist

    TYPE hash_tbl_sll
        TYPE(sllist), DIMENSION(:), ALLOCATABLE :: vec
        INTEGER                                 :: vec_len = 0
        LOGICAL                                 :: is_init = .FALSE.

        CONTAINS
            PROCEDURE :: init => init_hash_tbl_sll
            PROCEDURE :: put  => put_hash_tbl_sll
            PROCEDURE :: get  => get_hash_tbl_sll
            PROCEDURE :: free => free_hash_tbl_sll
    END TYPE hash_tbl_sll

    PUBLIC :: hash_tbl_sll

    CONTAINS

        RECURSIVE SUBROUTINE put_sll(list,key,val)
            CLASS(sllist), INTENT(INOUT) :: list
            CHARACTER(len=*), INTENT(IN) :: key,val
            INTEGER                      :: key_len,val_len
            key_len = LEN(key)
            val_len = LEN(val)
            IF (ALLOCATED(list%key)) THEN
                IF (list%key /= key) THEN
                    IF ( .NOT. ASSOCIATED(list%child) ) ALLOCATE(list%child)
                    CALL put_sll(list%child,key,val)
                ENDIF
            ELSE
                IF ( .NOT. ALLOCATED(list%key) ) ALLOCATE(CHARACTER(len=key_len) :: list%key)
                list%key = key
                IF (ALLOCATED(list%val)) DEALLOCATE(list%val)
                ALLOCATE(CHARACTER(len=val_len) :: list%val)
                list%val = val
            ENDIF
        END SUBROUTINE put_sll

        RECURSIVE SUBROUTINE get_sll(list,key,val)
            CLASS(sllist), INTENT(IN)                  :: list
            CHARACTER(len=*), INTENT(IN)               :: key
            CHARACTER(len=:), ALLOCATABLE, INTENT(OUT) :: val
            INTEGER                                    :: val_len
            val_len = 0
            IF (ALLOCATED(list%key) .AND. (list%key .eq. key)) THEN
                val_len =LEN(list%val)
                IF (ALLOCATED(val)) DEALLOCATE(val)
                ALLOCATE(CHARACTER(len=val_len) :: val)
                val = list%val
            ELSE IF (ASSOCIATED(list%child)) THEN
                CALL get_sll(list%child,key,val)
            ELSE
                IF (ALLOCATED(val)) DEALLOCATE(val)
                RETURN
            ENDIF
        END SUBROUTINE get_sll

        RECURSIVE SUBROUTINE free_sll(list)
            CLASS(sllist), INTENT(INOUT) :: list
            IF (ASSOCIATED(list%child)) THEN
                CALL free_sll(list%child)
                DEALLOCATE(list%child)
            END IF
            list%child => NULL()
            IF (ALLOCATED(list%key)) DEALLOCATE(list%key)
            IF (ALLOCATED(list%val)) DEALLOCATE(list%val)
        END SUBROUTINE free_sll

        SUBROUTINE init_hash_tbl_sll(tbl, tbl_len)
            CLASS(hash_tbl_sll), INTENT(INOUT) :: tbl
            INTEGER,      OPTIONAL, INTENT(IN) :: tbl_len
            IF (ALLOCATED(tbl%vec)) DEALLOCATE(tbl%vec)
            IF (PRESENT(tbl_len)) THEN
                ALLOCATE(tbl%vec(0:tbl_len-1))
                tbl%vec_len = tbl_len
            ELSE
                ALLOCATE(tbl%vec(0:tbl_size-1))
                tbl%vec_len = tbl_size
            ENDIF
            tbl%is_init = .TRUE.
        END SUBROUTINE init_hash_tbl_sll

        ELEMENTAL FUNCTION sum_string(str) RESULT(sig)
            CHARACTER(len=*), INTENT(IN)   :: str
            INTEGER                        :: sig, i
            CHARACTER, DIMENSION(LEN(str)) :: tmp
            FORALL (i=1:LEN(str))
                tmp(i) = str(i:i)
            END FORALL
            sig = SUM(ICHAR(tmp))
        END FUNCTION sum_string

        SUBROUTINE put_hash_tbl_sll(tbl,key,val)
            CLASS(hash_tbl_sll), INTENT(INOUT) :: tbl
            CHARACTER(len=*), INTENT(IN)       :: key,val
            INTEGER                            :: hash
            hash = MOD(sum_string(key),tbl%vec_len)
            CALL tbl%vec(hash)%put(key=key,val=val)
        END SUBROUTINE put_hash_tbl_sll

        FUNCTION get_hash_tbl_sll(tbl,key) RESULT(val)
            CLASS(hash_tbl_sll), INTENT(IN) :: tbl
            CHARACTER(len=*),    INTENT(IN) :: key
            CHARACTER(len=:),   ALLOCATABLE :: val
            INTEGER                         :: hash
            hash = MOD(sum_string(key),tbl%vec_len)
            CALL tbl%vec(hash)%get(key=key,val=val)
        END FUNCTION get_hash_tbl_sll

        SUBROUTINE free_hash_tbl_sll(tbl)
            CLASS(hash_tbl_sll), INTENT(INOUT) :: tbl
            INTEGER                            :: i,low,high
            low  = LBOUND(tbl%vec,dim=1)
            high = UBOUND(tbl%vec,dim=1)
            IF (ALLOCATED(tbl%vec)) THEN
                DO i=low,high
                    CALL tbl%vec(i)%free
                ENDDO
                DEALLOCATE(tbl%vec)
            ENDIF
            tbl%is_init = .FALSE.
        END SUBROUTINE free_hash_tbl_sll
END MODULE fdict