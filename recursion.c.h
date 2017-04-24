/* Generated by           cobc 2.0.0 */
/* Generated from         RECURSION.cbl */
/* Generated at           Apr 24 2017 19:12:42 */
/* GnuCOBOL build date    Nov 13 2016 01:30:50 */
/* GnuCOBOL package date  Nov 06 2016 22:36:19 UTC */
/* Compile command        cobc.exe -o bin\RECURSION.dll -std=default -Wall -g -LC:\Users\laury\Documents\Computer School\Recurse Center\Cisp\subroutines\bin -L\subroutines\bin -L..\subroutines\bin RECURSION.cbl */


/* Module path */
static const char		*cob_module_path = NULL;

/* Number of call parameters */
static int		cob_call_params = 0;

/* Attributes */

static const cob_field_attr a_1 =	{0x21,   0,   0, 0x0000, NULL};
static const cob_field_attr a_2 =	{0x01,   0,   0, 0x0000, NULL};
static const cob_field_attr a_3 =	{0x10,   5,   0, 0x0000, NULL};
static const cob_field_attr a_4 =	{0x10,  20,   0, 0x0000, NULL};


/* Constants */
static const cob_field c_1	= {4, (cob_u8_ptr)"INIT", &a_1};
static const cob_field c_2	= {17, (cob_u8_ptr)"ADD-TO-CALL-STACK", &a_1};
static const cob_field c_3	= {4, (cob_u8_ptr)"PEEK", &a_1};
static const cob_field c_4	= {14, (cob_u8_ptr)"POP-CALL-STACK", &a_1};
static const cob_field c_5	= {8, (cob_u8_ptr)"IS-EMPTY", &a_1};
static const cob_field c_6	= {16, (cob_u8_ptr)"PRINT-CALL-STACK", &a_1};
static const cob_field c_7	= {5, (cob_u8_ptr)"CLOSE", &a_1};
static const cob_field c_8	= {17, (cob_u8_ptr)"STACK-FILE-STATUS", &a_1};
static const cob_field c_9	= {14, (cob_u8_ptr)"RECURSION:INIT", &a_1};
static const cob_field c_10	= {22, (cob_u8_ptr)"Initialized Call Stack", &a_1};
static const cob_field c_11	= {24, (cob_u8_ptr)"RECURSION:POP-CALL-STACK", &a_1};
static const cob_field c_12	= {22, (cob_u8_ptr)"Closed call-stack file", &a_1};
static const cob_field c_13	= {26, (cob_u8_ptr)"PRINT-CALL-STACK-PROCEDURE", &a_1};
static const cob_field c_14	= {6, (cob_u8_ptr)"ID    ", &a_1};
static const cob_field c_15	= {22, (cob_u8_ptr)"NAME                  ", &a_1};
static const cob_field c_16	= {20, (cob_u8_ptr)"RESULT              ", &a_1};
static const cob_field c_17	= {20, (cob_u8_ptr)"RESULT (NUMERIC)    ", &a_1};
static const cob_field c_18	= {14, (cob_u8_ptr)" RETURN ID    ", &a_1};
static const cob_field c_19	= {7, (cob_u8_ptr)"DELETED", &a_1};
static const cob_field c_20	= {1, (cob_u8_ptr)" ", &a_1};
static const cob_field c_21	= {11, (cob_u8_ptr)"STACK-EMPTY", &a_1};
static const cob_field c_22	= {9, (cob_u8_ptr)"NOT-EMPTY", &a_1};
static const cob_field c_23	= {9, (cob_u8_ptr)"stack.dat", &a_1};


/* Strings */
static const char st_1[]	= "RECURSION.cbl";
static const char st_2[]	= "MAIN-PROCEDURE";
static const char st_3[]	= "EVALUATE";
static const char st_4[]	= "PERFORM";
static const char st_5[]	= "GOBACK";
static const char st_6[]	= "LOG-INIT-CALL-STACK";
static const char st_7[]	= "MOVE";
static const char st_8[]	= "CALL";
static const char st_9[]	= "LOG-ADD-TO-CALL-STACK";
static const char st_10[]	= "STRING";
static const char st_11[]	= "LOG-PEEK-CALL-STACK";
static const char st_12[]	= "LOG-POP-FROM-CALL-STACK";
static const char st_13[]	= "LOG-IS-EMPTY-CALL-STACK";
static const char st_14[]	= "LOG-CLOSE-CALL-STACK";
static const char st_15[]	= "LOG-DELETE-FROM-CALL-STACK";
static const char st_16[]	= "INIT-CALL-STACK-PROCEDURE";
static const char st_17[]	= "OPEN";
static const char st_18[]	= "CLOSE";
static const char st_19[]	= "SET";
static const char st_20[]	= "CLOSE-CALL-STACK-PROCEDURE";
static const char st_21[]	= "DELETE";
static const char st_22[]	= "CALL-STACK-ADD-PROCEDURE";
static const char st_23[]	= "WRITE";
static const char st_24[]	= "IF";
static const char st_25[]	= "ADD";
static const char st_26[]	= "PRINT-CALL-STACK-PROCEDURE";
static const char st_27[]	= "DISPLAY";
static const char st_28[]	= "READ";
static const char st_29[]	= "CALL-STACK-GET-TOP-PROCEDURE";
static const char st_30[]	= "POP-CALL-STACK-PROCEDURE";
static const char st_31[]	= "IS-STACK-EMPTY-PROCEDURE";
static const char st_32[]	= "CHECK-FILE-STATUS-PROCEDURE";

