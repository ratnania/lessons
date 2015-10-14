##########################################################################
SET(CURRENT_DIR ${PROJECT_SOURCE_DIR}/fortran/2)

ADD_EXECUTABLE(2_ex_1 ex_1.F90)
ADD_EXECUTABLE(2_ex_2 ex_2.F90)
ADD_EXECUTABLE(2_ex_3 ex_3.F90)

IF(BUILD_TESTING)
	ADD_TEST(NAME 2_unit_test_1 COMMAND 2_ex_1)
	ADD_TEST(NAME 2_unit_test_2 COMMAND 2_ex_2)
	ADD_TEST(NAME 2_unit_test_3 COMMAND 2_ex_3)
ENDIF(BUILD_TESTING)
##########################################################################

