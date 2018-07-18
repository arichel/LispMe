/**********************************************************************/
/*                                                                    */
/* error.h: All possible error codes                                  */
/*                                                                    */
/* LispMe System (c) FBI Fred Bayer Informatics                       */
/*                                                                    */
/* Distributed under the GNU General Public License;                  */
/* see the README file. This code comes with NO WARRANTY.             */
/*                                                                    */
/**********************************************************************/
#ifndef INC_ERROR_H
#define INC_ERROR_H

#include "store.h"

#define ERR_S1_INVALID_CHAR        1
#define ERR_S2_INVALID_SEXP        2
#define ERR_S3_MULTI_DOT           3
#define ERR_S4_TOKEN_LEN           4
#define ERR_S6_INVALID_REAL        5
#define ERR_S8_INVALID_HASH        6
#define ERR_S9_UNTERM_STRING       7
#define ERR_S10_INVALID_COMP       8
#define ERR_S11_NO_COMPLEX         9

#define ERR_C1_UNDEF              10
#define ERR_C2_NUM_ARGS           11
#define ERR_C3_NOT_SYMBOL         12
#define ERR_C4_INVALID_LET        13
#define ERR_C5_INVALID_LAMBDA     14
#define ERR_C6_IMPROPER_ARGS      15
#define ERR_C7_DUPLICATE_NAME     16
#define ERR_C8_INV_LET_LIST       17
#define ERR_C9_WRONG_DEFINE       18
#define ERR_C10_EMPTY_SEQUENCE    19
#define ERR_C11_INVALID_COND      20
#define ERR_C13_INV_UNQUOTE       21
#define ERR_C14_SPLICE_NONLIST    22
#define ERR_C15_INVALID_DEFINE    23
#define ERR_C16_NOT_DEFINE        24
#define ERR_C17_TOO_MANY_VARS     25
 
#define ERR_R1_WRONG_TYPE         26
#define ERR_R2_INVALID_INDEX      27
#define ERR_R3_NUM_ARGS           28
#define ERR_R7_ILLEGAL_OP         29
#define ERR_R8_DIV_BY_ZERO        30
#define ERR_R9_BLACK_HOLE         31
#define ERR_R10_COND_CLAUSE       32
#define ERR_R14_INVALID_COMP      33
#define ERR_R15_INVALID_PARM      34
#define ERR_R16_CREATE_FILE       35
#define ERR_R17_OPEN_FILE         36
#define ERR_R18_LOST_FILE         37
#define ERR_R19_WRITE_FILE        38

#define ERR_M1_STRING_FULL        39
#define ERR_M2_INVALID_PTR        40
#define ERR_M4_NO_HEAP            41
#define ERR_M7_NO_VECTOR_MEM      42
#define ERR_M8_NO_STRING_MEM      43
#define ERR_M9_NO_REALS           44

#define ERR_O1_STACK_EMPTY        45 
#define ERR_O2_INTERRUPT          46
#define ERR_O7_STACK_OVER         47
#define ERR_O9_INVALID_CMD        48

#define ERR_D1_NO_MEMORY          49

#define ERR_USER_ERROR            50

void error     (int err, IPTR p, ...);
void typeError (IPTR p, char* type);
void parmError (IPTR p, char* func);
void fatal     (char* p);

#endif
