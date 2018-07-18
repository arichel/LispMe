/**********************************************************************/
/*                                                                    */
/* error.c: Error messages                                            */
/*                                                                    */
/* LispMe System (c) FBI Fred Bayer Informatics                       */
/*                                                                    */
/* Distributed under the GNU General Public License;                  */
/* see the README file. This code comes with NO WARRANTY.             */
/*                                                                    */
/**********************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <setjmp.h>
#include <stdarg.h>

#include "io.h"
#include "error.h"

extern jmp_buf catcher;

/**********************************************************************/
/* module data                                                        */
/**********************************************************************/
static const char* msgs[] =
{
    NULL,
    "S01: line %d: invalid char %c",
    "S02: line %d: invalid expression",
    "S03: line %d: multiple dots in list",
    "S04: line %d: max. token length exceeded",
    "S06: line %d: invalid real number",
    "S08: line %d: invalid # constant %c",
    "S09: line %d: unterminated string",
    "S10: line %d: invalid complex number",
    "S11: line %d: complex numbers not supported",
    "C01: undefined name %%s",
    "C02: wrong number of arguments for %%s",
    "C03: %%s is not a symbol",
    "C04: invalid let binding %%s",
    "C05: invalid lambda args %%s",
    "C06: improper arg list %%s",
    "C07: duplicate name %%s",
    "C08: invalid let list %%s",
    "C09: definition %%s in invalid context",
    "C10: empty expression sequence",
    "C11: invalid case/cond clause %%s",
    "C13: %%s invalid outside of quasiquote",
    "C14: unquote-splicing allowed in list or vector template only",
    "C15: invalid define %%s",
    "C16: not only defines in loaded file",
    "C17: too many variables, can't address %%s",
    "R01: %%s is not a %s",
    "R02: invalid index %%s",
    "R03: function expected %d args, but got %d as follows: %%s",
    "R07: illegal opcode %d",
    "R08: division by zero",
    "R09: sucked into black hole",
    "R10: no matching case/cond clause",
    "R14: non-matching types in comparison",
    "R15: invalid parameter %%s for %s",
    "R16: can't create file %%s",
    "R17: can't open file %%s",
    "R18: file %s vanished",
    "R19: can't write to file %s",
    "M01: atom space exhausted",
    "M02: invalid pointer 0x%lx",
    "M04: heap space exhausted",
    "M07: no memory for vector",
    "M08: no memory for string",
    "M09: FP storage exhausted",
    "O01: no more sources to pop",
    "O02: execution interrupted",
    "O07: stack overflow",
    "O09: invalid command",
    "D01: no memory for SQL access",
    "U01: user error: %%s",
};
/**********************************************************************/
/* Display error message and continue REP loop                        */
/**********************************************************************/
void error(int err, IPTR p, ...)
{
    static char temp[64];
    static char buf1[128];
    static char buf2[256];

    va_list argPtr;
    va_start(argPtr,p);

    if (err != ERR_O2_INTERRUPT)
        printSEXP(temp, sizeof(temp), p, PRT_ESCAPE);

    vsprintf(buf1,msgs[err],argPtr);
    sprintf(buf2,buf1,temp);

    fputs("Error ", stderr);
    fputs(buf2, stderr);
    longjmp(catcher, 1);
}
/**********************************************************************/
/* Build type error object                                            */
/**********************************************************************/
void typeError(IPTR p, char* type)
{
    error(ERR_R1_WRONG_TYPE, p, type);
}
/**********************************************************************/
/* Build parm error object                                            */
/**********************************************************************/
void parmError(IPTR p, char* func)
{
    error(ERR_R15_INVALID_PARM,p,func);
}
/**********************************************************************/
/* Fatal error, leave immediately                                     */
/**********************************************************************/
void fatal(char* p)
{
    fprintf(stderr,"%s\n",p);
    exit(EXIT_FAILURE);
}
