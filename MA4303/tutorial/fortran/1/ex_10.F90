! ...................................................
MODULE MODULE_FIELD
IMPLICIT NONE
   PRIVATE
   PUBLIC :: FIELD, FIELD_DIMENSION, FIELD_PRINT, FIELD_SET_COEFFICIENTS  

   TYPE FIELD
      INTEGER      :: d
      REAL(KIND=8) :: coefficients
   END TYPE FIELD

   CONTAINS
   ! ...................................................
   FUNCTION FIELD_DIMENSION(self) RESULT(val)
   IMPLICIT NONE
      TYPE(FIELD), INTENT(IN) :: self
      INTEGER :: val

      val = 1 
   END FUNCTION FIELD_DIMENSION 
   ! ...................................................

   ! ...................................................
   SUBROUTINE FIELD_PRINT(self)
   IMPLICIT NONE
      TYPE(FIELD), INTENT(IN) :: self
      ! LOCAL
      INTEGER :: max_i  
     
      PRINT *, 'Field: dim  = ', self%d

      max_i = MIN(self%d, 5)
      PRINT *, 'Field: coef = ', self%coefficients !(1:max_i)

   END SUBROUTINE FIELD_PRINT
   ! ...................................................

   ! ...................................................
   SUBROUTINE FIELD_SET_COEFFICIENTS(self, coefficients)
   IMPLICIT NONE
      TYPE(FIELD), INTENT(INOUT) :: self
      REAL(KIND=8), INTENT(IN)   :: coefficients
     
      self % coefficients = coefficients

   END SUBROUTINE FIELD_SET_COEFFICIENTS
   ! ...................................................
END MODULE MODULE_FIELD
! ...................................................

! ...................................................
PROGRAM EX_4
USE MODULE_FIELD
IMPLICIT NONE
   TYPE(FIELD) :: U

!   U = FIELD(1)       ! Use the implicit constructor
   CALL FIELD_PRINT(U)  ! Call a class subroutine

END PROGRAM EX_4 
! ...................................................
