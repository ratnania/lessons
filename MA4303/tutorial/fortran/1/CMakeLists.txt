##########################################################################
SET(CURRENT_DIR ${PROJECT_SOURCE_DIR}/fortran/1)

ADD_EXECUTABLE(1_ex_1 ex_1.F90)
ADD_EXECUTABLE(1_ex_2 ex_2.F90)
ADD_EXECUTABLE(1_ex_3 ex_3.F90)
ADD_EXECUTABLE(1_ex_4 ex_4.F90)
ADD_EXECUTABLE(1_ex_5 ex_5.F90)
ADD_EXECUTABLE(1_ex_6 ex_6.F90)
ADD_EXECUTABLE(1_ex_7 ex_7.F90)
ADD_EXECUTABLE(1_ex_8 ex_8.F90)

IF(BUILD_TESTING)
	ADD_TEST(NAME 1_unit_test_1 COMMAND 1_ex_1)
	ADD_TEST(NAME 1_unit_test_2 COMMAND 1_ex_2)
	ADD_TEST(NAME 1_unit_test_3 COMMAND 1_ex_3)
	ADD_TEST(NAME 1_unit_test_4 COMMAND 1_ex_4)
	ADD_TEST(NAME 1_unit_test_5 COMMAND 1_ex_5)
	ADD_TEST(NAME 1_unit_test_6 COMMAND 1_ex_6)
	ADD_TEST(NAME 1_unit_test_7 COMMAND 1_ex_7)
	ADD_TEST(NAME 1_unit_test_8 COMMAND 1_ex_8)
ENDIF(BUILD_TESTING)
##########################################################################

