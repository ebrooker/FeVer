!!*********************************************
!!
!! Author: Ezra Brooker
!! 2021
!! 
!! src/Utils/parameterListType.f90
!!
!! This module defines a parameter list derived
!! type object that one can use to pass an N
!! number of parameters to a function, provided
!! the function knows it will be receiving the
!! derived type list. This is a crude way to
!! achieve the **kwargs style function input of
!! Python functions.
!!
!! The primary procedures one needs to use with
!! an instance of this parameter list type are
!! the init, addEntry, getEntry procedures
!!
!!*********************************************
MODULE dictionary_class
USE kind_settings
USE entry_class
IMPLICIT NONE
PRIVATE

    PUBLIC :: Dictionary_t

    TYPE Dictionary_t
        PRIVATE
        INTEGER(lgInt)             :: length         !! length of list
        INTEGER(lgInt)             :: lastIndex = 0  !! last index filled
        TYPE(entry_t), ALLOCATABLE :: entries(:)      !! list of entry_t types

    CONTAINS

        !! List initializer
        PROCEDURE :: init

        !! Add key-value entry routines
        PROCEDURE, PRIVATE :: addSmInt
        PROCEDURE, PRIVATE :: addLgInt
        PROCEDURE, PRIVATE :: addReal
        PROCEDURE, PRIVATE :: addBool
        PROCEDURE, PRIVATE :: addChar
        GENERIC            :: addEntry => addLgInt,addSmInt,addReal,addBool,addChar

        !! Get key-value routines
        PROCEDURE, PRIVATE :: getSmInt
        PROCEDURE, PRIVATE :: getLgInt
        PROCEDURE, PRIVATE :: getReal
        PROCEDURE, PRIVATE :: getBool
        PROCEDURE, PRIVATE :: getChar
        GENERIC            :: getEntry => getLgInt,getSmInt,getReal,getBool,getChar

        !! Assign value from key routines
        ! PROCEDURE          :: assignEntry
        PROCEDURE, PRIVATE :: assignSmInt
        PROCEDURE, PRIVATE :: assignLgInt
        PROCEDURE, PRIVATE :: assignReal
        PROCEDURE, PRIVATE :: assignBool
        PROCEDURE, PRIVATE :: assignChar
        GENERIC            :: assignEntry => assignLgInt,assignSmInt,assignReal,assignBool,assignChar

        !! Some utility functions
        PROCEDURE          :: getLength
        PROCEDURE          :: getLastIndex
        PROCEDURE          :: printEntries
        PROCEDURE, PRIVATE :: isKeyInList

    END TYPE Dictionary_t


    INTERFACE Dictionary_t
        PROCEDURE :: constructor
    END INTERFACE


CONTAINS


    FUNCTION constructor(length) RESULT(this)
        TYPE(Dictionary_t)         :: this
        INTEGER(lgInt), INTENT(IN) :: length
        
        CALL this%init(length)

    END FUNCTION constructor


    SUBROUTINE init(this,length)
        !! Initialize the list with list length
        CLASS(Dictionary_t)            :: this
        INTEGER(lgInt),     INTENT(IN) :: length

        this%length = length

        IF (ALLOCATED(this%entries)) DEALLOCATE(this%entries)
        ALLOCATE(this%entries(this%length))

    END SUBROUTINE init

    FUNCTION getLength(this)
        !! Get the length of this list
        CLASS(Dictionary_t), INTENT(IN) :: this
        INTEGER(lgInt)                  :: getLength

        getLength = this%length

    END FUNCTION getLength

    FUNCTION getLastIndex(this)
        !! Get the last index of the list filled
        CLASS(Dictionary_t), INTENT(IN) :: this
        INTEGER(lgInt)                  :: getLastIndex

        getLastIndex = this%lastIndex

    END FUNCTION getLastIndex

    SUBROUTINE printEntries(this)
        !! Print all entries in the list
        CLASS(Dictionary_t), INTENT(IN) :: this
        INTEGER(lgInt)                  :: i
        CHARACTER(LEN=:), ALLOCATABLE   :: key

        DO i=1,this%lastIndex
      
            key = this%entries(i)%getKey()
      
            SELECT TYPE(value => this%entries(i)%getValue())

            TYPE IS(INTEGER(lgInt))
                WRITE(*,'(A14,A15,A10,I9)'    ) " INTEGER key = ", ADJUSTL(key), ", value = ", value
      
            TYPE IS(INTEGER(smInt))
                WRITE(*,'(A14,A15,A10,I9)'    ) " INTEGER key = ", ADJUSTL(key), ", value = ", value
            
            TYPE IS(REAL(wp))
                WRITE(*,'(A14,A15,A10,ES15.4)') " REAL    key = ", ADJUSTL(key), ", value = ", value
      
            TYPE IS(CHARACTER(LEN=*))
                WRITE(*,'(A14,A15,A10,A)'     ) " STRING  key = ", ADJUSTL(key), ", value = ", value
      
            TYPE IS(LOGICAL)
                WRITE(*,'(A14,A15,A10,L)'     ) " BOOLEAN key = ", ADJUSTL(key), ", value = ", value
      
            CLASS DEFAULT
                WRITE(*,'(A14,A15,A)'         ) " UNKNOWN key = ", ADJUSTL(key), ", value = UNKNOWN DATA TYPE"
      
            END SELECT

        END DO

    END SUBROUTINE printEntries


    SUBROUTINE isKeyInList(this,key,inlist,index)
        !! Check if key is in list and return index
        !! if it is
        CLASS(Dictionary_t), INTENT(IN)  :: this
        CHARACTER(LEN=*),    INTENT(IN)  :: key
        LOGICAL,             INTENT(OUT) :: inlist
        INTEGER(lgInt),      INTENT(OUT) :: index
        INTEGER(lgInt)                   :: i

        inlist = .false.
        index  = 0 
        DO i = 1, this%lastIndex
            IF (this%entries(i)%getKey() == key) THEN
                inlist = .true.
                index = i
            END IF
        END DO

    END SUBROUTINE isKeyInList


    !!********************************!!
    !! Define type-dependent routines !!
    !! for adding key-value pairs     !!
    !!********************************!!

    SUBROUTINE addSmInt(this,key,value)
        !! Add integer parameter
        CLASS(Dictionary_t), INTENT(INOUT) :: this
        CHARACTER(LEN=*),    INTENT(IN)    :: key
        INTEGER(smInt)                     :: value
        CLASS(*),            ALLOCATABLE   :: newval
        INTEGER(lgInt)                     :: ierr
        INTEGER(lgInt)                     :: index
        LOGICAL                            :: inlist

        ALLOCATE(newval, source=value)
        CALL this%isKeyInList(key,inlist,index) ! Check if key in list

        IF (inlist) THEN
            ! overwrite existing key-value entry
            CALL this%entries(index)%addKeyVal(key,newval)
        
        ELSE
            ! add new key-value entry
            this%lastIndex = this%lastIndex + 1
            CALL this%entries(this%lastIndex)%addKeyVal(key,newval)
        
        END IF

    END SUBROUTINE addSmInt


    SUBROUTINE addLgInt(this,key,value)
        !! Add integer parameter
        CLASS(Dictionary_t), INTENT(INOUT) :: this
        CHARACTER(LEN=*),    INTENT(IN)    :: key
        INTEGER(lgInt)                     :: value
        CLASS(*),            ALLOCATABLE   :: newval
        INTEGER(lgInt)                     :: ierr
        INTEGER(lgInt)                     :: index
        LOGICAL                            :: inlist

        ALLOCATE(newval, source=value)
        CALL this%isKeyInList(key,inlist,index) ! Check if key in list

        IF (inlist) THEN
            ! overwrite existing key-value entry
            CALL this%entries(index)%addKeyVal(key,newval)
        
        ELSE
            ! add new key-value entry
            this%lastIndex = this%lastIndex + 1
            CALL this%entries(this%lastIndex)%addKeyVal(key,newval)
        
        END IF

    END SUBROUTINE addLgInt

    SUBROUTINE addReal(this,key,value)
        !! Add real parameter
        CLASS(Dictionary_t), INTENT(INOUT) :: this
        CHARACTER(LEN=*),    INTENT(IN)    :: key
        REAL(wp)                           :: value
        CLASS(*),            ALLOCATABLE   :: newval
        INTEGER(lgInt)                     :: ierr
        INTEGER(lgInt)                     :: index
        LOGICAL                            :: inlist

        ALLOCATE(newval, source=value)
        CALL this%isKeyInList(key,inlist,index)

        IF (inlist) THEN
        
            CALL this%entries(index)%addKeyVal(key,newval)
        
        ELSE
            this%lastIndex = this%lastIndex + 1
            CALL this%entries(this%lastIndex)%addKeyVal(key,newval)
        
        END IF

    END SUBROUTINE addReal

    SUBROUTINE addBool(this,key,value)
        !! Add boolean parameter
        CLASS(Dictionary_t), INTENT(INOUT) :: this
        CHARACTER(LEN=*),    INTENT(IN)    :: key
        LOGICAL                            :: value
        CLASS(*),            ALLOCATABLE   :: newval
        INTEGER(lgInt)                     :: ierr
        INTEGER(lgInt)                     :: index
        LOGICAL                            :: inlist

        ALLOCATE(newval, source=value)
        CALL this%isKeyInList(key,inlist,index)

        IF (inlist) THEN
        
            CALL this%entries(index)%addKeyVal(key,newval)
        
        ELSE
            this%lastIndex = this%lastIndex + 1
            CALL this%entries(this%lastIndex)%addKeyVal(key,newval)
        
        END IF

    END SUBROUTINE addBool

    SUBROUTINE addChar(this,key,value)
        !! Add string parameter
        CLASS(Dictionary_t), INTENT(INOUT) :: this
        CHARACTER(LEN=*),    INTENT(IN)    :: key
        CHARACTER(LEN=*),    INTENT(IN)    :: value
        CLASS(*),            ALLOCATABLE   :: newval
        INTEGER(lgInt)                     :: ierr
        INTEGER(lgInt)                     :: index
        LOGICAL                            :: inlist

        ALLOCATE(newval, source=value)
        CALL this%isKeyInList(key,inlist,index)

        IF (inlist) THEN
        
            CALL this%entries(index)%addKeyVal(key,newval)
        
        ELSE
            this%lastIndex = this%lastIndex + 1
            CALL this%entries(this%lastIndex)%addKeyVal(key,newval)
        
        END IF

    END SUBROUTINE addChar


    !!********************************!!
    !! Define type-dependent routines !!
    !! for fetching key-values        !!
    !!********************************!!

    SUBROUTINE getSmInt(this,key,value)
        CLASS(Dictionary_t), INTENT(IN)  :: this
        CHARACTER(LEN=*),    INTENT(IN)  :: key
        INTEGER(smInt),      INTENT(OUT) :: value
        INTEGER(lgInt)                   :: ierr
        INTEGER(lgInt)                   :: index
        LOGICAL                          :: inlist

        CALL this%isKeyInList(key,inlist,index)
        IF (inlist) THEN
            SELECT TYPE (temp => this%entries(index)%getValue())
            TYPE IS (INTEGER(lgInt))
                value = temp
            CLASS DEFAULT
                stop "[getEntry ERROR] Entry VALUE not type INTEGER"
            END SELECT
        ELSE
            stop "[getEntry ERROR] Entry KEY not in LIST"
        ENDIF
    END SUBROUTINE getSmInt

    SUBROUTINE getLgInt(this,key,value)
        CLASS(Dictionary_t), INTENT(IN)  :: this
        CHARACTER(LEN=*),    INTENT(IN)  :: key
        INTEGER(lgInt),      INTENT(OUT) :: value
        INTEGER(lgInt)                   :: ierr
        INTEGER(lgInt)                   :: index
        LOGICAL                          :: inlist

        CALL this%isKeyInList(key,inlist,index)
        IF (inlist) THEN
            SELECT TYPE (temp => this%entries(index)%getValue())
            TYPE IS (INTEGER(lgInt))
                value = temp
            CLASS DEFAULT
                stop "[getEntry ERROR] Entry VALUE not type INTEGER"
            END SELECT
        ELSE
            stop "[getEntry ERROR] Entry KEY not in LIST"
        ENDIF
    END SUBROUTINE getLgInt


    SUBROUTINE getReal(this,key,value)
        CLASS(Dictionary_t), INTENT(IN)  :: this
        CHARACTER(LEN=*),    INTENT(IN)  :: key
        REAL(wp),            INTENT(OUT) :: value
        INTEGER(lgInt)                   :: ierr
        INTEGER(lgInt)                   :: index
        LOGICAL                          :: inlist

        CALL this%isKeyInList(key,inlist,index)
        IF (inlist) THEN
            SELECT TYPE (temp => this%entries(index)%getValue())
            TYPE IS (REAL(wp)) 
                value = temp
            CLASS DEFAULT
                stop "[getEntry ERROR] Entry value not type REAL"
            END SELECT
        ELSE
            stop "[getEntry ERROR] Entry KEY not in LIST"
        ENDIF
    END SUBROUTINE getReal


    SUBROUTINE getChar(this,key,value)
        CLASS(Dictionary_t),           INTENT(IN)  :: this
        CHARACTER(LEN=*),              INTENT(IN)  :: key
        CHARACTER(LEN=:), ALLOCATABLE, INTENT(OUT) :: value
        INTEGER(lgInt)                             :: ierr
        INTEGER(lgInt)                             :: index
        LOGICAL                                    :: inlist

        CALL this%isKeyInList(key,inlist,index)
        IF (inlist) THEN
            SELECT TYPE (temp => this%entries(index)%getValue())
            TYPE IS (CHARACTER(LEN=*))
                value = temp
            CLASS DEFAULT
                stop "[getEntry ERROR] Entry value not type REAL"
            END SELECT
        ELSE
            stop "[getEntry ERROR] Entry KEY not in LIST"
        ENDIF
    END SUBROUTINE getChar


    SUBROUTINE getBool(this,key,value)
        CLASS(Dictionary_t), INTENT(IN)  :: this
        CHARACTER(LEN=*),    INTENT(IN)  :: key
        LOGICAL,             INTENT(OUT) :: value
        INTEGER(lgInt)                   :: ierr
        INTEGER(lgInt)                   :: index
        LOGICAL                          :: inlist

        CALL this%isKeyInList(key,inlist,index)
        IF (inlist) THEN
            SELECT TYPE (temp => this%entries(index)%getValue())
            TYPE IS (LOGICAL)
                value = temp
            CLASS DEFAULT
                stop "[getEntry ERROR] Entry value not type LOGICAL"
            END SELECT
        ELSE
            stop "[getEntry ERROR] Entry KEY not in LIST"
        ENDIF
    END SUBROUTINE getBool


    !!********************************!!
    !! Define type-dependent routines !!
    !! for fetching key-values        !!
    !!********************************!!

    SUBROUTINE assignLgInt(this,key,value,default_value)
        CLASS(Dictionary_t),      INTENT(IN ) :: this
        CHARACTER(LEN=*),         INTENT(IN ) :: key
        CHARACTER(LEN=:),         ALLOCATABLE :: val_str
        INTEGER(LgInt),           INTENT(OUT) :: value
        INTEGER(LgInt), OPTIONAL, INTENT(IN ) :: default_value
        INTEGER(lgInt)                        :: ierr
        INTEGER(lgInt)                        :: index
        LOGICAL                               :: inlist

        CALL this%isKeyInList(key,inlist,index)
        IF (inlist) THEN
            CALL this%getEntry(key,val_str)
            READ(val_str,*,iostat=ierr) value
            IF ( PRESENT(default_value) .and. ierr/=0) THEN
                value = default_value
                CALL warning_defaultValue( key )
            ENDIF
        ELSE
            stop "[assignEntry ERROR] Entry KEY not in LIST"
        ENDIF

    END SUBROUTINE assignLgInt

    SUBROUTINE assignSmInt(this,key,value,default_value)
        CLASS(Dictionary_t),      INTENT(IN ) :: this
        CHARACTER(LEN=*),         INTENT(IN ) :: key
        CHARACTER(LEN=:),         ALLOCATABLE :: val_str
        INTEGER(smInt),           INTENT(OUT) :: value
        INTEGER(smInt), OPTIONAL, INTENT(IN ) :: default_value
        INTEGER(lgInt)                        :: ierr
        INTEGER(lgInt)                        :: index
        LOGICAL                               :: inlist

        CALL this%isKeyInList(key,inlist,index)
        IF (inlist) THEN
            CALL this%getEntry(key,val_str)
            READ(val_str,*,iostat=ierr) value
            IF ( PRESENT(default_value) .and. ierr/=0) THEN
                value = default_value
                CALL warning_defaultValue( key )
            ENDIF
        ELSE
            stop "[assignEntry ERROR] Entry KEY not in LIST"
        ENDIF

    END SUBROUTINE assignSmInt


    SUBROUTINE assignReal(this,key,value,default_value)
        CLASS(Dictionary_t),     INTENT(IN ) :: this
        CHARACTER(LEN=*),        INTENT(IN ) :: key
        CHARACTER(LEN=:),        ALLOCATABLE :: val_str
        REAL(KIND=wp),           INTENT(OUT) :: value
        REAL(KIND=wp), OPTIONAL, INTENT(IN ) :: default_value
        INTEGER(lgInt)                       :: ierr
        INTEGER(lgInt)                       :: index
        LOGICAL                              :: inlist

        CALL this%isKeyInList(key,inlist,index)
        IF (inlist) THEN
            CALL this%getEntry(key,val_str)
            READ(val_str,*,iostat=ierr) value
            IF ( PRESENT(default_value) .and. ierr/=0) THEN
                value = default_value
                CALL warning_defaultValue( key )
            ENDIF
        ELSE
            stop "[assignEntry ERROR] Entry KEY not in LIST"
        ENDIF

    END SUBROUTINE assignReal


    SUBROUTINE assignChar(this,key,value,default_value)
        CLASS(Dictionary_t),        INTENT(IN ) :: this
        CHARACTER(LEN=*),           INTENT(IN ) :: key
        CHARACTER(LEN=:),           ALLOCATABLE :: val_str
        CHARACTER(LEN=*),           INTENT(OUT) :: value
        CHARACTER(LEN=*), OPTIONAL, INTENT(IN ) :: default_value
        INTEGER(lgInt)                          :: ierr
        INTEGER(lgInt)                          :: index
        LOGICAL                                 :: inlist

        CALL this%isKeyInList(key,inlist,index)
        IF (inlist) THEN
            CALL this%getEntry(key,val_str)
            READ(val_str,*,iostat=ierr) value
            IF ( PRESENT(default_value) .and. ierr/=0) THEN
                value = default_value
                CALL warning_defaultValue( key )
            ENDIF
        ELSE
            stop "[assignEntry ERROR] Entry KEY not in LIST"
        ENDIF
    END SUBROUTINE assignChar


    SUBROUTINE assignBool(this,key,value,default_value)
        CLASS(Dictionary_t), INTENT(IN ) :: this
        CHARACTER(LEN=*),    INTENT(IN ) :: key
        CHARACTER(LEN=:),    ALLOCATABLE :: val_str
        LOGICAL,             INTENT(OUT) :: value
        LOGICAL,   OPTIONAL, INTENT(IN ) :: default_value
        INTEGER(lgInt)                   :: ierr
        INTEGER(lgInt)                   :: index
        LOGICAL                          :: inlist

        CALL this%isKeyInList(key,inlist,index)
        IF (inlist) THEN
            CALL this%getEntry(key,val_str)
            READ(val_str,*,iostat=ierr) value
            IF ( PRESENT(default_value) .and. ierr/=0) THEN
                value = default_value
                CALL warning_defaultValue( key )
            ENDIF
        ELSE
            stop "[assignEntry ERROR] Entry KEY not in LIST"
        ENDIF
    END SUBROUTINE assignBool


    SUBROUTINE warning_DefaultValue ( str )
        IMPLICIT NONE
        CHARACTER(LEN=*), INTENT(IN) :: str
        WRITE(*,*) "           USING DEFAULT VALUE FOR ", TRIM(str)
        RETURN
    END SUBROUTINE warning_DefaultValue

END MODULE dictionary_class