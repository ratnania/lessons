! ...................................................
MODULE MODULE_FIELD
IMPLICIT NONE
   PRIVATE
   PUBLIC :: FIELD, FIELD_DIMENSION, FIELD_PRINT 

   TYPE FIELD
      INTEGER  :: n_dim
      INTEGER  :: n_size 
   END TYPE FIELD

   CONTAINS
   ! ...................................................
   FUNCTION FIELD_DIMENSION(self) RESULT(val)
   IMPLICIT NONE
      TYPE(FIELD), INTENT(IN) :: self
      INTEGER :: val

      val = self % n_dim 
   END FUNCTION FIELD_DIMENSION 
   ! ...................................................

   ! ...................................................
   FUNCTION FIELD_SIZE(self) RESULT(val)
   IMPLICIT NONE
      TYPE(FIELD), INTENT(IN) :: self
      INTEGER :: val

      val = self % n_size 
   END FUNCTION FIELD_SIZE
   ! ...................................................

   ! ...................................................
   SUBROUTINE FIELD_PRINT(self)
   IMPLICIT NONE
      TYPE(FIELD), INTENT(IN) :: self
     
      PRINT *, 'Field : dim   ', self % n_dim
      PRINT *, '        size  ', self % n_size
   END SUBROUTINE FIELD_PRINT
   ! ...................................................
END MODULE MODULE_FIELD
! ...................................................

! ...................................................
PROGRAM EX_4
USE MODULE_FIELD
IMPLICIT NONE
   TYPE(FIELD) :: U

   U = FIELD(1, 10)       ! Use the implicit constructor
   CALL FIELD_PRINT(U)  ! Call a class subroutine

END PROGRAM EX_4 
! ...................................................
