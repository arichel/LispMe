/**********************************************************************/
/*                                                                    */
/* comp.c:  LISP to SECD compiler                                     */
/*                                                                    */
/* LispMe System (c) FBI Fred Bayer Informatics                       */
/*                                                                    */
/* Distributed under the GNU General Public License;                  */
/* see the README file. This code comes with NO WARRANTY.             */
/*                                                                    */
/**********************************************************************/
#include "comp.h"
#include "error.h"
#include "store.h"
#include "vm.h"
#include "io.h"

#ifdef DUMP_STEPS
   extern FILE* logFile;
#endif /*DUMP_STEPS*/

/**********************************************************************/
/* Hacks to fit variable arity and constant into opcode table         */
/**********************************************************************/
#define VARIABLE      42
#define EMPTY_LIST    57
#define EMPTY_STRING  69

bool macroExpand;

static IPTR   joinCont;
static IPTR   rtnCont;
static int   qqNest;
static char* a;

static struct BuiltIns
{
    char*          func;
    IPTR            atom;
    char           arity;
    char           swap;
    unsigned char  opcodes[4];

} builtIns[] = {
    {"read",             0, 0, false, {READ, LC0,  NOP}},
    {"read-char",        0, 0, false, {REAC, LC0,  NOP}},
    {"read-line",        0, 0, false, {REDL, LC0,  NOP}},
    {"peek-char",        0, 0, false, {PEEK, LC0,  NOP}},
    {"-",                0, 1, false, {SUB,  LC0,  NOP}},
    {"/",                0, 1, false, {DIV,  LC1,  NOP}},
    {"display",          0, 1, false, {DSPL, LC0,  NOP}},
    {"write",            0, 1, false, {WRIT, LC0,  NOP}},
    {"atan",             0, 1, false, {ATAN, NOP}},

    {"+",                0, VARIABLE, false, {ADD,  0}},
    {"*",                0, VARIABLE, false, {MUL,  1}},
    {"append",           0, VARIABLE, false, {APND, EMPTY_LIST}},
    {"string-append",    0, VARIABLE, false, {SAPP, EMPTY_STRING}},
    {"bit-and",          0, VARIABLE, false, {LAND, -1}},
    {"bit-or",           0, VARIABLE, false, {LIOR, 0}},
    {"bit-xor",          0, VARIABLE, false, {LXOR, 0}},

    {"gensym",           0, 0, false, {GSYM, NOP}},
    {"gc",               0, 0, false, {DOGC, NOP}},

    {"boolean?",         0, 1, false, {BOOL, NOP}},
    {"pair?",            0, 1, false, {PAIR, NOP}},
    {"null?",            0, 1, false, {NUL,  NOP}},
    {"number?",          0, 1, false, {CPLP, NOP}},
    {"complex?",         0, 1, false, {CPLP, NOP}},
    {"real?",            0, 1, false, {REAP, NOP}},
    {"integer?",         0, 1, false, {INTP, NOP}},
    {"char?",            0, 1, false, {CHR,  NOP}},
    {"string?",          0, 1, false, {STRG, NOP}},
    {"symbol?",          0, 1, false, {SYMB, NOP}},
    {"none?",            0, 1, false, {NONE, NOP}},
    {"macro?",           0, 1, false, {MACP, NOP}},
    {"not",              0, 1, false, {NOT,  NOP}},
    {"car",              0, 1, false, {CAR,  NOP}},
    {"cdr",              0, 1, false, {CDR,  NOP}},
    {"error",            0, 1, false, {UERR, NOP}},
    {"force",            0, 1, false, {AP0,  NOP}},
    {"string-length",    0, 1, false, {SLEN, NOP}},
    {"string->list",     0, 1, false, {S2L,  NOP}},
    {"list->string",     0, 1, false, {L2S,  NOP}},
    {"vector->list",     0, 1, false, {V2L,  NOP}},
    {"list->vector",     0, 1, false, {L2V,  NOP}},
    {"char->integer",    0, 1, false, {C2I,  NOP}},
    {"integer->char",    0, 1, false, {I2C,  NOP}},
    {"procedure?",       0, 1, false, {PROC, NOP}},
    {"continuation?",    0, 1, false, {CONT, NOP}},
    {"promise?",         0, 1, false, {PROM, NOP}},
    {"object->string",   0, 1, false, {O2S,  NOP}},
    {"string->object",   0, 1, false, {S2O,  NOP}},
    {"read",             0, 1, false, {READ, NOP}},
    {"read-char",        0, 1, false, {REAC, NOP}},
    {"read-line",        0, 1, false, {REDL, NOP}},
    {"peek-char",        0, 1, false, {PEEK, NOP}},
    {"random",           0, 1, false, {RAND, NOP}},
    {"sqrt",             0, 1, false, {SQRT, NOP}},
    {"sin",              0, 1, false, {SIN,  NOP}},
    {"cos",              0, 1, false, {COS,  NOP}},
    {"tan",              0, 1, false, {TAN,  NOP}},
    {"asin",             0, 1, false, {ASIN, NOP}},
    {"acos",             0, 1, false, {ACOS, NOP}},
    {"exp",              0, 1, false, {EXP,  NOP}},
    {"log",              0, 1, false, {LOG,  NOP}},
    {"sinh",             0, 1, false, {SINH, NOP}},
    {"cosh",             0, 1, false, {COSH, NOP}},
    {"tanh",             0, 1, false, {TANH, NOP}},
    {"asinh",            0, 1, false, {ASIH, NOP}},
    {"acosh",            0, 1, false, {ACOH, NOP}},
    {"atanh",            0, 1, false, {ATAH, NOP}},
    {"floor",            0, 1, false, {FLOR, NOP}},
    {"ceiling",          0, 1, false, {CEIL, NOP}},
    {"truncate",         0, 1, false, {TRUN, NOP}},
    {"round",            0, 1, false, {ROUN, NOP}},
    {"integer",          0, 1, false, {INTG, NOP}},
    {"real-part",        0, 1, false, {REPA, NOP}},
    {"imag-part",        0, 1, false, {IMPA, NOP}},
    {"magnitude",        0, 1, false, {MAGN, NOP}},
    {"angle",            0, 1, false, {ANGL, NOP}},
    {"disasm",           0, 1, false, {DASM, NOP}},
    {"open-output-file", 0, 1, false, {OOUT, NOP}},
    {"open-input-file",  0, 1, false, {OINP, NOP}},
    {"open-append-file", 0, 1, false, {OAPP, NOP}},
    {"close-input-port", 0, 1, false, {CLOS, NOP}},
    {"close-output-port",0, 1, false, {CLOS, NOP}},
    {"peek-char",        0, 1, false, {PEEK, NOP}},
    {"input-port?",      0, 1, false, {INPP, NOP}},
    {"output-port?",     0, 1, false, {OUPP, NOP}},
    {"eof-object?",      0, 1, false, {EOFO, NOP}},
    {"delete-file",      0, 1, false, {DELF, NOP}},
    {"vector?",          0, 1, false, {VECP, NOP}},
    {"vector-length",    0, 1, false, {VLEN, NOP}},
    {"bit-not",          0, 1, false, {LNOT, NOP}},

    {"cons",             0, 2, false, {CONS, NOP}},
    {"-",                0, 2, false, {SUB,  NOP}},
    {"/",                0, 2, false, {DIV,  NOP}},
    {"remainder",        0, 2, false, {REM,  NOP}},
    {"quotient",         0, 2, false, {IDIV, NOP}},
    {"eq?",              0, 2, false, {EQ,   NOP}},
    {"<=",               0, 2, false, {LEQ,  NOP}},
    {">",                0, 2, false, {NOT,  LEQ,  NOP}},
    {">=",               0, 2, true,  {LEQ,  NOP}},
    {"<",                0, 2, true,  {NOT,  LEQ,  NOP}},
    {"set-car!",         0, 2, true,  {SCAR, NOP}},
    {"set-cdr!",         0, 2, true,  {SCDR, NOP}},
    {"eqv?",             0, 2, false, {EQV,  NOP}},
    {"atan",             0, 2, false, {ATN2, NOP}},
    {"make-rectangular", 0, 2, false, {MKRA, NOP}},
    {"make-polar",       0, 2, false, {MKPO, NOP}},
    {"string-ref",       0, 2, false, {SREF, NOP}},
    {"string=?",         0, 2, false, {SEQ,  NOP}},
    {"display",          0, 2, true,  {DSPL, NOP}},
    {"write",            0, 2, true,  {WRIT, NOP}},
    {"vector-ref",       0, 2, false, {VREF, NOP}},
    {"make-vector",      0, 2, false, {VMAK, NOP}},
    {"make-string",      0, 2, false, {SMAK, NOP}},
    {"bit-shift",        0, 2, false, {LSHT, NOP}},

    {"string-set!",      0, 3, false, {SSET, NOP}},
    {"vector-set!",      0, 3, false, {VSET, NOP}},
    {"substring",        0, 3, false, {SUBS, NOP}},
    {NULL}
};
/**********************************************************************/
/* Initialize compiler tables                                         */
/**********************************************************************/
void InitCompiler(void)
{
    struct BuiltIns* bi;
    for (bi=builtIns; bi->func; bi++)
        bi->atom = findAtom(bi->func);
}
/**********************************************************************/
/* find a variable in an environment                                  */
/**********************************************************************/
LEXADR location(IPTR var, IPTR env, bool throwErr)
{
    int depth = 0, ord = 0;
    IPTR frame, name;

    for (depth = 0, frame = env; frame != NIL; ++depth, frame = cdr(frame))
        for (ord = 0, name = car(frame); name != NIL; ++ord, name = cdr(name))
            if (var == car(name)) {
                if (depth >= MAX_FRAMES || ord   >= MAX_VARS_IN_FRAME)
                    error(ERR_C17_TOO_MANY_VARS,var);
                else
                    return (depth << FRAME_SHIFT) | ord;
            }

    if (throwErr)
        error(ERR_C1_UNDEF,var);

    return VAR_NOT_FOUND;
}
/**********************************************************************/
/* Create code for pushing constant onto stack                        */
/**********************************************************************/
static void compConst(IPTR con, IPTR* code)
{
    switch (con)
    {
    case MKINT(0):  *code = cons(MKINT(LC0),  *code); break;
    case MKINT(1):  *code = cons(MKINT(LC1),  *code); break;
    case MKINT(2):  *code = cons(MKINT(LC2),  *code); break;
    case MKINT(3):  *code = cons(MKINT(LC3),  *code); break;
    case MKINT(-1): *code = cons(MKINT(LC_1), *code); break;
    case TRUE:      *code = cons(MKINT(LC_T), *code); break;
    case FALSE:     *code = cons(MKINT(LC_F), *code); break;
    case NIL:       *code = cons(MKINT(LC_N), *code); break;
    case EMPTY_STR: *code = cons(MKINT(LC_E), *code); break;
    default:        *code = cons(MKINT(LDC),cons(con,*code)); break;
    }
}
/**********************************************************************/
/* Compile an expression list using init as neutral element and       */
/* opc as combiner  always use neutral element when building a        */
/* list or string!                                                    */
/**********************************************************************/
static void comp(IPTR expr, IPTR names, IPTR* code);

static void compaccum(IPTR exlist, IPTR names, IPTR* code,
                      IPTR init,   int opc)
{
    IPTR e;
    CHECKSTACK(e);

    for (e=exlist; e != NIL; e = cdr(e))
    {
        if (!IS_PAIR(e))
            error(ERR_C6_IMPROPER_ARGS, exlist);

        if (cdr(e) != NIL  || opc == CONS
            || (opc == SAPP && cdr(exlist) == NIL))
            *code = cons(MKINT(opc),*code);

        comp(car(e), names, code);
    }
    if (exlist == NIL || opc == CONS
        || (opc == SAPP && cdr(exlist) == NIL))
        compConst(init, code);
}
/**********************************************************************/
/* compile an expression list using the new optimized LST n inst.     */
/**********************************************************************/
static void complist(IPTR exlist, IPTR names, IPTR* code)
{
    int n = 0;
    IPTR e, patch;
    CHECKSTACK(e);

    /* store length cell to patch later after counting expressions */
    *code = cons(MKINT(LST),patch=cons(BLACK_HOLE,*code));

    for (e=exlist; e != NIL; e=cdr(e))
    {
        if (!IS_PAIR(e))
            error(ERR_C6_IMPROPER_ARGS,exlist);

        ++n;
        comp(car(e), names, code);
    }
    car(patch) = MKINT(n);
}
/**********************************************************************/
/* build code for built-in function                                   */
/**********************************************************************/
static int buildIn(IPTR func, IPTR args, short argl, IPTR names, IPTR* code)
{
    int i, j;
    IPTR temp;
    struct BuiltIns* p;

    CHECKSTACK(p);
    /*------------------------------------------------------------------*/
    /* Search function name in table                                    */
    /*------------------------------------------------------------------*/
    for (p = builtIns, i = 0; p->func; ++i, ++p)
        if (func == p->atom)
        {
            /*--------------------------------------------------------------*/
            /* Found, now check for variable arity                          */
            /*--------------------------------------------------------------*/
            if (p->arity == VARIABLE)
            {
                PROTECT(temp);
                if (p->opcodes[1] == EMPTY_LIST)
                    temp = NIL;

                else if (p->opcodes[1] == EMPTY_STRING)
                    temp = cons(EMPTY_STR,(IPTR)0);

                else
                    temp = MKINT((signed char)p->opcodes[1]);

                compaccum(args, names, code, temp, p->opcodes[0]);

                UNPROTECT(temp);
                return 1;
            }
            /*--------------------------------------------------------------*/
            /* Found, now check arity                                       */
            /*--------------------------------------------------------------*/
            if (argl != p->arity)
            {
                if (p->arity == 0 && i < 4)
                    continue; /* try unary versions, too */

                if (p->arity == 1 && i < 9)
                    continue; /* try binary versions, too */

                error(ERR_C2_NUM_ARGS, func);
            }

            /*--------------------------------------------------------------*/
            /* Build opcode list from table (reversed!)                     */
            /*--------------------------------------------------------------*/
            for (j =0; p->opcodes[j] != NOP; j++)
                *code = cons(MKINT(p->opcodes[j]), *code);

            /*--------------------------------------------------------------*/
            /* Build code for subexpressions, possibly swapped              */
            /*--------------------------------------------------------------*/
            if (argl > 0)
            {
                if (p->swap)
                    comp(cadr(args), names, code);
                else
                    comp(car(args), names, code);
            }
            if (argl > 1)
            {
                if (p->swap)
                    comp(car(args), names, code);
                else
                    comp(cadr(args), names, code);
            }
            if (argl > 2)
                comp(caddr(args), names, code);

            return 1;
        }
    /*------------------------------------------------------------------*/
    /* Now check c[ad]+r functions  [hal: 15/8-00]                      */
    /*------------------------------------------------------------------*/
    if (IS_ATOM(func)) {
        a = getAtom(func);

        if (*a++ == 'c') /* atom could be c[ad]+r */
        {
            i = 1;
            /* only [ad] allowed before an r*/
            while (*a == 'a' || *a == 'd') 
                i = (i << 1) | (*a++ == 'd');

            /* atom is c[ad]+r */
            if (i > 1 && i < (1 << (PTR_BITLEN -2)) && *a == 'r') 
            {
                *code = cons(MKINT(CXR),cons(MKINT(i),*code));
                comp(car(args), names, code);
                return 1;
            }
        }
    }
    /*------------------------------------------------------------------*/
    /* No function found                                                */
    /*------------------------------------------------------------------*/
    return 0;
}
/**********************************************************************/
/* check, if name list is unique                                      */
/**********************************************************************/
static void checkUnique(IPTR vars)
{
    IPTR p1,p2;
    for (p1=vars; p1 != NIL; p1=cdr(p1))
        for (p2=cdr(p1); p2 != NIL; p2=cdr(p2))
            if (car(p1) == car(p2))
                error(ERR_C7_DUPLICATE_NAME,car(p1));
}
/**********************************************************************/
/* check let-list and  extract variable names from                    */
/* a LET or LETREC-block                                              */
/**********************************************************************/
static IPTR vars(IPTR body)
{
    IPTR e, res = NIL;

    PROTECT(res);
    for (e = body; e != NIL; e=cdr(e))
    {
        if (!IS_PAIR(e))
            error(ERR_C8_INV_LET_LIST,e);

        if (listLength(car(e)) != 2)
            error(ERR_C4_INVALID_LET,car(e));

        if (!IS_ATOM(caar(e)))
            error(ERR_C3_NOT_SYMBOL,caar(e));

        res = cons(caar(e),res);
    }
    checkUnique(res);
    UNPROTECT(res);
    return res;
}
/**********************************************************************/
/* extract expressions from a LET or LETREC-block                     */
/**********************************************************************/
static IPTR exprs(IPTR body)
{
    IPTR e, res = NIL;
    PROTECT(res);

    for (e=body; e != NIL; e=cdr(e))
        res = cons(cadar(e),res);

    UNPROTECT(res);
    return res;
}
/**********************************************************************/
/* build name list for lambda expression                              */
/**********************************************************************/
static short lambdaNames(IPTR orgArgs, IPTR* newArgs)
{
    short len  = 0;
    IPTR   oldP = NIL;

    if (IS_CONS(orgArgs) || orgArgs == NIL)
    {
        IPTR p = orgArgs;
        while (IS_CONS(p))
        {
            if (!IS_ATOM(car(p)))
                error(ERR_C5_INVALID_LAMBDA,orgArgs);
            oldP = p;
            p = cdr(p);
            ++len;
        }
        if (p==NIL)
        {
            *newArgs = orgArgs;
            checkUnique(*newArgs);
            return len;
        }
        else if (IS_ATOM(p))
        {
            cdr(oldP) = cons(p,NIL);
            *newArgs = orgArgs;
            checkUnique(*newArgs);
            return -1 -len;
        }
        else
            error(ERR_C5_INVALID_LAMBDA,orgArgs);
    }
    else if (IS_ATOM(orgArgs))
    {
        *newArgs = cons(orgArgs,NIL);
        return -1;
    }
    else
        error(ERR_C5_INVALID_LAMBDA,orgArgs);

    return 0;
}
/**********************************************************************/
/* reverse a list                                                     */
/**********************************************************************/
static IPTR reverse(IPTR list)
{
    IPTR acc = NIL;
    PROTECT(acc);

    while (list != NIL) {
        if (!IS_PAIR(list))
            error(ERR_C6_IMPROPER_ARGS,list);

        acc  = cons(car(list),acc);
        list = cdr(list);
    }
    UNPROTECT(acc);
    return acc;
}
/**********************************************************************/
/* compile an expression sequence                                     */
/**********************************************************************/
static void compseq(IPTR exlist, IPTR names, IPTR* code)
{
    IPTR e = NIL;

    CHECKSTACK(e);

    if (exlist == NIL)
        error(ERR_C10_EMPTY_SEQUENCE,NIL);

    PROTECT(e);
    for (e=reverse(exlist); e != NIL; e=cdr(e))
    {
        comp(car(e), names, code);
        if (cdr(e) != NIL) {
            *code = cons(MKINT(POP),*code);
        }
    }
    UNPROTECT(e);
}
/**********************************************************************/
/* add a single definition to a letrec binding list                   */
/**********************************************************************/
static void addDefine(IPTR *let, IPTR body, bool isMacro)
{
    if (!IS_CONS(body) || !IS_CONS(cdr(body)))
        error(ERR_C2_NUM_ARGS, DEFINE);

    if (IS_CONS(car(body)))
    {
        /*------------------------------------------------------------*/
        /* function definition:                                       */
        /* ((var . args) expr ...) -> (var (lambda args expr ...))    */
        /*------------------------------------------------------------*/
        *let = cons(cons(caar(body),
                         cons(cons(isMacro ? MLAMBDA : LAMBDA,
                                   cons(cdar(body),cdr(body))),
                              NIL)),
                    *let);
    }
    else
    {
        if (isMacro)
            error(ERR_C15_INVALID_DEFINE, body);
        /*------------------------------------------------------------*/
        /* simple definition:                                         */
        /* (var expr) -> (var expr)                                   */
        /*------------------------------------------------------------*/
        *let = cons(body,*let);
    }
}
/**********************************************************************/
/* resolve a definition (define ...) or definition list               */
/* (begin (define ...) ...) to equivalent letrec                      */
/* Same for (macro ...)                                               */
/**********************************************************************/
static int resolveDefineList(IPTR expr, IPTR* newExpr)
{
    IPTR temp, def;
    int  res = 0;

    *newExpr = expr;

    if (IS_CONS(expr))
    {
        temp = cons(LETREC, cons(NIL, cons(NOPRINT,NIL)));
        PROTECT(temp);

        if (car(expr) == DEFINE || car(expr) == MACRO)
        {
            /*--------------------------------------------------------*/
            /* single definition                                      */
            /*--------------------------------------------------------*/
            addDefine(&cadr(temp), cdr(expr), car(expr) == MACRO);
            *newExpr = temp;
            res = 1;
        }
        else if (car(expr) == BEGIN && IS_CONS(cadr(expr)) &&
                 (caadr(expr) == DEFINE || caadr(expr) == MACRO))
        {
            /*--------------------------------------------------------*/
            /* (possible) list of definitions                         */
            /*--------------------------------------------------------*/
            for (def = cdr(expr); def != NIL; def = cdr(def))
            {
                if (!IS_PAIR(def))
                    error(ERR_C15_INVALID_DEFINE, def);

                if (!IS_CONS(car(def)) 
		    || (caar(def) != DEFINE && caar(def) != MACRO))
                    error(ERR_C15_INVALID_DEFINE, car(def));

                addDefine(&cadr(temp), cdar(def), caar(def) == MACRO);
            }
            *newExpr = temp;
            res = 1;
        }
        UNPROTECT(temp);
    }
    return res;
}
/**********************************************************************/
/* resolve a inner definition ((define1 ...) (define2 ...) exp1 exp2) */
/* to equivalent letrec                                               */
/**********************************************************************/
static int resolveInnerDefine(IPTR expr, IPTR* newExpr)
{
    IPTR temp, seq, def =NIL;

    *newExpr = expr;
    if (IS_CONS(expr) && IS_CONS(car(expr)) && caar(expr) == DEFINE)
    {
        /*----------------------------------------------------------------*/
        /* OK, list starts with DEFINE, now find first non-DEFINE         */
        /*----------------------------------------------------------------*/
        for (seq = cdr(expr);
             IS_CONS(seq) && IS_CONS(car(seq)) && caar(seq) == DEFINE;
             seq = cdr(seq))
            ;

        if (seq == NIL)
            error(ERR_C10_EMPTY_SEQUENCE,NIL);

        /*----------------------------------------------------------------*/
        /* Check that no subsequent expression is a define                */
        /*----------------------------------------------------------------*/
        for (temp=seq; temp != NIL; temp = cdr(temp))
            if (IS_CONS(def) && IS_CONS(car(def)) && caar(def) == DEFINE)
                error(ERR_C9_WRONG_DEFINE, car(def));

        /*----------------------------------------------------------------*/
        /* Build LETREC with first non-DEFINE as body                     */
        /*----------------------------------------------------------------*/
        temp = cons(LETREC,cons(NIL,seq));
        PROTECT(temp);

        /*----------------------------------------------------------------*/
        /* Add each definition to binding part of LETREC                  */
        /*----------------------------------------------------------------*/
        for (def = expr; def != seq; def = cdr(def))
            addDefine(&cadr(temp), cdar(def), false);

        *newExpr = temp;
        UNPROTECT(temp);
        return 1;
    }
    else
        return 0;
}
/**
 * Dump current expression under compilation.
 */
static void cmp_dump(IPTR expr, IPTR names, FILE *port){
    fputs("#################################\n", port);
    fputs("Compiling expression ", port); 
    printSEXPfp(port, expr, PRT_ESCAPE);  
    fputc('\n', port);

#ifdef DUMP_STEPS
    fputs("with name list ", port); 
    printSEXPfp(port, names, PRT_ESCAPE); 
    fputc('\n', port);
#endif
}
/**********************************************************************/
/* Compile an expression, accumulating the resulted code              */
/**********************************************************************/
static void comp(IPTR expr, IPTR names, IPTR* code)
{
    IPTR temp, temp1, temp2, temp3, temp4;
    short opc;

    CHECKSTACK(opc);

tailCall:

#ifdef DUMP_STEPS
    fputs("call comp expr = ", logFile);
    printSEXPfp(logFile, expr, PRT_ESCAPE);
    fputc('\n',logFile);
#endif
    if (IS_INT(expr)  || IS_REAL(expr)    || IS_SPECIAL(expr) ||
        IS_CHAR(expr) || IS_STRING(expr)  || IS_PORT(expr)    ||
        (IS_CONS(expr) && IS_INTERN(car(expr))))
    {
        /*----------------------------------------------------------------*/
        /* Self-evaluating                                                */
        /*----------------------------------------------------------------*/
    selfEval:
        compConst(expr, code);
    }
    else if (IS_VEC(expr))
    {
        if (qqNest)
        {
            /*--------------------------------------------------------------*/
            /* Handle vector template for quasiquote                        */
            /*--------------------------------------------------------------*/
            *code = cons(MKINT(L2V),*code);
            expr  = vector2List(expr);
            goto tailCall;
        }
        else
            goto selfEval;
    }
    else if (IS_ATOM(expr))
    {
        if (qqNest)
            goto selfEval;

        /*----------------------------------------------------------------*/
        /* Variable access                                                */
        /*----------------------------------------------------------------*/
        *code = cons(MKINT(-location(expr, names, true)), *code);
    }
    else
    {
        IPTR   fn   = car(expr);
        IPTR   args = cdr(expr);
        short argl = listLength(args);

        if (qqNest)
        {
            /*--------------------------------------------------------------*/
            /* We're inside a quasiquote template                           */
            /*--------------------------------------------------------------*/
            if (fn == QUASIQUOTE)
            {
                if (argl != 1)
                    error(ERR_C2_NUM_ARGS,fn);

                *code = cons(MKINT(CONS),*code);
                ++qqNest;

                comp(fn, names, code);
                compaccum(args,names,code,NIL,CONS);
                --qqNest;
            }
            else if (fn == UNQUOTE)
            {
                if (argl != 1)
                    error(ERR_C2_NUM_ARGS,fn);

                if (qqNest == 1)
                {
                    --qqNest;
                    comp(car(args), names, code);
                    ++qqNest;
                }
                else
                {
                compUnquote:
                    *code = cons(MKINT(CONS),*code);
                    --qqNest;
                    comp(fn, names, code);
                    compaccum(args,names,code,NIL,CONS);
                    ++qqNest;
                }
            }
            else if (fn == UNQUOTESPLICING)
            {
                if (argl != 1)
                    error(ERR_C2_NUM_ARGS,fn);

                if (qqNest == 1)
                {
                    if (car(*code) != MKINT(CONS))
                        error(ERR_C14_SPLICE_NONLIST,NIL);

                    /*----------------------------------------------------------*/
                    /* Replace latest CONS in continuation by APND              */
                    /*----------------------------------------------------------*/
                    *code = cons(MKINT(APND), cdr(*code));
                    --qqNest;

                    comp(car(args), names, code);
                    ++qqNest;
                }
                else
                    goto compUnquote;
            }
            else
            {
                *code = cons(MKINT(CONS),*code);
                comp(fn,names,code);
                compaccum(args,names,code,NIL,CONS);
            }
        }
        else
        {
            /*--------------------------------------------------------------*/
            /* normal expression                                            */
            /*--------------------------------------------------------------*/
            if (fn == QUOTE)
            {
                /*------------------------------------------------------------*/
                /* quoted expression                                          */
                /*------------------------------------------------------------*/
                if (argl != 1)
                    error(ERR_C2_NUM_ARGS,fn);
                compConst(car(args), code);
            }
            else if (fn == QUASIQUOTE)
            {
                /*------------------------------------------------------------*/
                /* quasiquote expression                                      */
                /*------------------------------------------------------------*/
                if (argl != 1)
                    error(ERR_C2_NUM_ARGS,fn);

                ++qqNest;
                comp(car(args), names, code);
                --qqNest;
            }
            else if (fn == UNQUOTE || fn == UNQUOTESPLICING)
                error(ERR_C13_INV_UNQUOTE,fn);

            else if (buildIn(fn, args, argl, names, code))
                ;

            else if (fn == COND)
            {
                /*------------------------------------------------------------*/
                /* conditional: if cont = (RET) dont use join, but propagate  */
                /* (RET) to subcontrols and use non-pushing SEL (=SELR)       */
                /*------------------------------------------------------------*/
                temp = temp4 = cons(MKINT(CERR),NIL);
                PROTECT(temp);
                PROTECT(temp4);

                temp3 = reverse(args);
                PROTECT(temp3);

                while (temp3 != NIL)
                {
                    if (!IS_PAIR(car(temp3)))
                        error(ERR_C11_INVALID_COND,car(temp3));

                    /*----------------------------------------------------------*/
                    /* Continuation after selection                             */
                    /*----------------------------------------------------------*/
                    if (*code != rtnCont) {
                        temp1 = joinCont;
                        opc   = SEL;
                    } else {
                        temp1 = rtnCont;
                        opc   = SELR;
                    }
                    PROTECT(temp1);

                    temp2 = NIL;
                    PROTECT(temp2);

                    if (resolveInnerDefine(cdar(temp3), &temp2))
                        comp(temp2, names, &temp1);
                    else
                        compseq(temp2, names, &temp1);
                    UNPROTECT(temp2);

                    /*----------------------------------------------------------*/
                    /* Omit test, if condition is 'else'                        */
                    /*----------------------------------------------------------*/
                    if (caar(temp3) == ELSE)
                        temp = temp4 = temp1;
                    else
                    {
                        temp4 = cons(MKINT(opc),cons(temp1,cons(temp,NIL)));
                        if (opc == SEL)
                        {
                            if (cdr(temp3) != NIL)
                                cdddr(temp4) = joinCont;
                            else
                                cdddr(temp4) = *code;
                        }
                        comp(caar(temp3),names,&temp4);
                        temp = temp4;
                    }
                    UNPROTECT(temp1);
                    temp3 = cdr(temp3);
                }
                UNPROTECT(temp3);
                UNPROTECT(temp4);
                UNPROTECT(temp);
                *code = temp4;
            }
            else if (fn == CASE)
            {
                /*------------------------------------------------------------*/
                /* conditional: if cont = (RET) dont use join, but propagate  */
                /* (RET) to subcontrols and use non-pushing SEL (=SELR)       */
                /*------------------------------------------------------------*/
                if (argl < 1)
                    error(ERR_C2_NUM_ARGS,fn);

                temp = temp4 = cons(MKINT(CERR),NIL);
                PROTECT(temp);
                PROTECT(temp4);

                temp3 = reverse(cdr(args));
                PROTECT(temp3);

                while (temp3 != NIL)
                {
                    if (!IS_PAIR(car(temp3)))
                        error(ERR_C11_INVALID_COND,car(temp3));

                    /*----------------------------------------------------------*/
                    /* Continuation after selection                             */
                    /*----------------------------------------------------------*/
                    if (*code != rtnCont) {
                        temp1 = joinCont;
                        opc   = MEM;
                    } else {
                        temp1 = rtnCont;
                        opc   = MEMR;
                    }
                    PROTECT(temp1);
                    temp2 = NIL;

                    PROTECT(temp2);
                    if (resolveInnerDefine(cdar(temp3), &temp2))
                        comp(temp2, names, &temp1);
                    else
                        compseq(temp2, names, &temp1);
                    UNPROTECT(temp2);
                    /*----------------------------------------------------------*/
                    /* Omit test, if condition is 'else', but have to POP test  */
                    /*----------------------------------------------------------*/
                    if (caar(temp3) != ELSE)
                    {
                        temp4 = cons(MKINT(opc),
                                     cons(caar(temp3),
                                          cons(temp1,
                                               cons(temp,NIL))));
                        if (opc == MEM)
                        {
                            if (cdr(temp3) != NIL)
                                cdr(cdddr(temp4)) = joinCont;
                            else
                                cdr(cdddr(temp4)) = *code;
                        }
                        temp = temp4;
                    }
                    else
                        temp = temp4 = cons(MKINT(POP),temp1);

                    UNPROTECT(temp1);
                    temp3 = cdr(temp3);
                }
                UNPROTECT(temp3);
                UNPROTECT(temp4);
                UNPROTECT(temp);

                *code = temp4;
                expr  = car(args);
                goto tailCall;
            }
            else if (fn == IF)
            {
                /*------------------------------------------------------------*/
                /* [e1] SEL{R} ([e2] JOIN/RTN) ([e3] JOIN/RTN)                */
                /*------------------------------------------------------------*/
                if (argl != 3)
                    error(ERR_C2_NUM_ARGS,fn);

                if (*code != rtnCont) {
                    temp2 = temp1 = joinCont;
                    opc   = SEL;
                } else {
                    temp2 = temp1 = rtnCont;
                    opc   = SELR;
                }
                PROTECT(temp1);
                PROTECT(temp2);

                comp(cadr(args),  names, &temp1);
                comp(caddr(args), names, &temp2);

                *code = cons(MKINT(opc), 
			     cons(temp1, cons(temp2, opc != SEL ? NIL : *code)));

                UNPROTECT(temp2);
                UNPROTECT(temp1);

                expr = car(args);
                goto tailCall;
            }
            else if (fn == OR || fn == AND)
            {
                /*------------------------------------------------------------*/
                /* 0 arg: LDC #f/#t                                           */
                /* 1 arg: comp(e)                                             */
                /* n arg: comp(e1) SE(O/A){R} (comp(e2) ... JOIN/RTN)         */
                /*------------------------------------------------------------*/
                if (argl == 0) {
                    expr = fn != OR ? TRUE : FALSE;
                    goto selfEval;
                }
                temp  = reverse(args);
                temp2 = NIL;

                PROTECT(temp);
                PROTECT(temp2);

                if (*code != rtnCont)
                    opc = fn != OR ? SEA  : SEO;
                else
                    opc = fn != OR ? SEAR : SEOR;

                while (argl--)
                {
                    if (*code != rtnCont)
                        temp1 = (argl != 0) ? joinCont : *code;
                    else
                        temp1 = rtnCont;

                    if (temp2 != NIL)
                        temp2 = cons(MKINT(opc), cons(temp2, temp1));
                    else
                        temp2 = temp1;

                    comp(car(temp), names, &temp2);
                    temp = cdr(temp);
                }
                UNPROTECT(temp2);
                UNPROTECT(temp);
                *code = temp2;
            }
            else if (fn == LAMBDA)
            {
                /*------------------------------------------------------------*/
                /* lambda expression                                          */
                /*------------------------------------------------------------*/
                short len;
                if (argl < 1)
                    error(ERR_C2_NUM_ARGS,fn);

                temp = rtnCont;
                PROTECT(temp);

                temp1 = cons(NIL,names);
                PROTECT(temp1);

                len = lambdaNames(car(args), &car(temp1));

                temp2 = NIL;
                PROTECT(temp2);

                if (resolveInnerDefine(cdr(args), &temp2))
                    comp(temp2, temp1, &temp);
                else
                    compseq(temp2, temp1, &temp);

                UNPROTECT(temp2);
                UNPROTECT(temp1);

                *code = cons(MKINT(LDFC), cons(MKINT(len), cons(temp,*code)));
                UNPROTECT(temp);
            }
            else if (fn == MLAMBDA)
            {
                /*------------------------------------------------------------*/
                /* lambda expression generated by macro                       */
                /*------------------------------------------------------------*/
                short len;
                if (argl < 1)
                    error(ERR_C2_NUM_ARGS,MACRO);

                temp = rtnCont;
                PROTECT(temp);

                temp1 = cons(NIL,names);
                PROTECT(temp1);

                len = lambdaNames(car(args), &car(temp1));
                if (len != 1)
                    error(ERR_C2_NUM_ARGS,MACRO);

                temp2 = NIL;
                PROTECT(temp2);
                if (resolveInnerDefine(cdr(args), &temp2))
                    comp(temp2, temp1, &temp);
                else
                    compseq(temp2, temp1, &temp);

                UNPROTECT(temp2);
                UNPROTECT(temp1);

                *code = cons(MKINT(LDM),cons(temp,*code));
                UNPROTECT(temp);
            }
            else if (fn == LET)
            {
                /*------------------------------------------------------------*/
                /* local block                                                */
                /*------------------------------------------------------------*/
                if (argl < 2)
                    error(ERR_C2_NUM_ARGS,fn);

                temp = rtnCont;
                PROTECT(temp);

                temp1 = cons(vars(car(args)),names);
                PROTECT(temp1);

                temp2 = NIL;
                PROTECT(temp2) ;
                if (resolveInnerDefine(cdr(args), &temp2))
                    comp(temp2, temp1, &temp);
                else
                    compseq(temp2, temp1, &temp);

                UNPROTECT(temp2);
                UNPROTECT(temp1);

                if (*code == rtnCont)
                    *code = cons(MKINT(LDF),cons(temp,cons(MKINT(TAP),NIL)));
                else
                    *code = cons(MKINT(LDF),cons(temp,cons(MKINT(AP),*code)));

                temp = exprs(car(args));
                complist(temp, names, code);
                UNPROTECT(temp);
            }
            else if (fn == LETREC)
            {
                /*------------------------------------------------------------*/
                /* local recursive block                                      */
                /*------------------------------------------------------------*/
                if (argl < 2)
                    error(ERR_C2_NUM_ARGS,fn);

                temp1 = cons(vars(car(args)),names);
                PROTECT(temp1);

                temp  = rtnCont;
                PROTECT(temp);

                temp2 = NIL;
                PROTECT(temp2);

                if (resolveInnerDefine(cdr(args), &temp2))
                    comp(temp2, temp1, &temp);
                else
                    compseq(temp2, temp1, &temp);

                UNPROTECT(temp2);

                if (*code == rtnCont)
                    *code = cons(MKINT(LDF),cons(temp,cons(MKINT(RTAP),NIL)));
                else
                    *code = cons(MKINT(LDF),cons(temp,cons(MKINT(RAP),*code)));

                temp = exprs(car(args));

                complist(temp, temp1, code);
                UNPROTECT(temp);
                UNPROTECT(temp1);

                *code = cons(MKINT(DUM),*code);

                /*------------------------------------------------------------*/
                /* the last LETREC block establishes names in top environment */
                /*------------------------------------------------------------*/
                newTopLevelNames = vars(car(args));
            }
            else if (fn == LIST)
            {
                complist(args, names, code);
            }
            else if (fn == VECTOR)
            {
                *code = cons(MKINT(L2V),*code);
                complist(args, names, code);
            }
            else if (fn == BEGIN)
            {
                temp = NIL;
                PROTECT(temp);

                if (resolveInnerDefine(args, &temp))
                    comp(temp, names, code);
                else
                    compseq(temp, names, code);

                UNPROTECT(temp);
            }
            else if (fn == SET)
            {
                /*------------------------------------------------------------*/
                /* modify variable                                            */
                /*------------------------------------------------------------*/
                if (argl != 2)
                    error(ERR_C2_NUM_ARGS, fn);

                if (!IS_ATOM(car(args)))
                    error(ERR_C3_NOT_SYMBOL, car(args));

                *code = cons(MKINT(ST), 
			     cons(location(car(args), names, true), *code));
                expr  = cadr(args);
                goto tailCall;
            }
            else if (fn == DELAY)
            {
                /*------------------------------------------------------------*/
                /* build recipe                                               */
                /*------------------------------------------------------------*/
                if (argl != 1)
                    error(ERR_C2_NUM_ARGS,fn);

                temp = cons(MKINT(UPD),NIL);
                PROTECT(temp);

                comp(car(args), names, &temp);

                *code = cons(MKINT(LDE),cons(temp,*code));
                UNPROTECT(temp);
            }
            else if (fn == CALLCC)
            {
                /*------------------------------------------------------------*/
                /* call with current continuation                             */
                /*------------------------------------------------------------*/
                if (argl != 1)
                    error(ERR_C2_NUM_ARGS,fn);

                temp = *code != rtnCont ?
                    cons(MKINT(APC),*code) : cons(MKINT(TAPC),NIL);

                PROTECT(temp);
                comp(car(args), names, &temp);

                *code = cons(MKINT(LDCT),cons(*code,temp));
                UNPROTECT(temp);
            }
            else if (fn == DEFINE)
                error(ERR_C9_WRONG_DEFINE, expr);

            else if (fn == EVAL)
            {
                /*------------------------------------------------------------*/
                /* capture the name list                                      */
                /*------------------------------------------------------------*/
                if (argl != 1)
                    error(ERR_C2_NUM_ARGS,fn);

                *code = cons(MKINT(EVA), cons(names, *code));
                expr  = car(args);
                goto tailCall;
            }
            else if (fn == APPLY)
            {
                /*------------------------------------------------------------*/
                /* function application to list                               */
                /*------------------------------------------------------------*/
                if (argl != 2)
                    error(ERR_C2_NUM_ARGS, fn);

                *code = *code != rtnCont ?
                    cons(MKINT(APY), *code) : cons(MKINT(TAPY), NIL);

                comp(car(args), names, code);
                expr = cadr(args);
                goto tailCall;
            }
            else if (IS_ATOM(fn))
            {
                LEXADR la;
                if ((la = location(fn,tlNames,false)) != VAR_NOT_FOUND)
                {
                    temp4 = car(locate(tlVals,la));
                    if (IS_MACRO(temp4))
                    {
                        /*----------------------------------------------------*/
                        /* macro application                                  */
                        /*-------------------------------------------------=--*/
#ifdef DUMP_STEPS
                        fputs("Expand macro ",logFile);
                        printSEXPfp(logFile,fn,PRT_ESCAPE);
                        fputs(" using args ",logFile);
                        printSEXPfp(logFile,expr,PRT_ESCAPE);
                        fputc('\n',logFile);
#endif
                        PROTECT(temp4);
                        D = cons(S,cons(E,cons(C,D)));
                        S = cons(cdr(temp4),cons(cons(expr,NIL),NIL));
                        E = tlVals;
                        C = cons(MKINT(APC),cons(MKINT(STOP),NIL));
                        PROTECT(temp);

                        macroExpand = true;
                        temp = exec();
                        macroExpand = false;

                        S = car(D);
                        E = cadr(D);
                        C = caddr(D);
                        D = cdddr(D);
#ifdef DUMP_STEPS
                        fputs("Expanded to ",logFile);
                        printSEXPfp(logFile,temp,PRT_ESCAPE);
                        fputc('\n',logFile);
#endif
                        UNPROTECT(temp);
                        UNPROTECT(temp4);
                        expr = temp;
                        goto tailCall;
                    }
                }
                goto appl;
            }
            else
            {
                /*------------------------------------------------------------*/
                /* function application                                       */
                /*------------------------------------------------------------*/
            appl:
                if (*code == rtnCont)
                    *code = cons(MKINT(TAPC),NIL);
                else
                    *code = cons(MKINT(APC),*code);

                comp(fn,names,code);
                complist(args,names,code);
            }
        }
    }
}
/**********************************************************************/
/* Compile an SEXPR                                                   */
/**********************************************************************/
IPTR compile(IPTR expr, IPTR names)
{
    IPTR res = NIL, src = NIL;
    bool eval = names != NIL;

#ifdef DUMP_STEPS
    cmp_dump(expr, names, logFile);
#endif

    /*------------------------------------------------------------------*/
    /* Protect working registers                                        */
    /*------------------------------------------------------------------*/
    PROTECT(res);
    PROTECT(src);

    joinCont = cons(MKINT(JOIN), NIL);
    PROTECT(joinCont);

    rtnCont  = cons(MKINT(RTN), NIL);
    PROTECT(rtnCont);

    if (eval)
    {
        res = rtnCont;
        src = expr;
    }
    else
    {
        if (fileLoad)
        {
            if (!resolveDefineList(expr, &src))
                error(ERR_C16_NOT_DEFINE, NIL);
        }
        else
        {
            if (resolveDefineList(expr, &src))
                fileLoad = true;
            else
            {
                /*------------------------------------------------------------*/
                /* add (set! it) before expr. to be able to refer to last res.*/
                /*------------------------------------------------------------*/
                src = cons(SET, cons(IT, cons(expr, NIL)));
            }
        }
        res = cons(MKINT(STOP),NIL);
        names = tlNames;
    }
    qqNest = 0;
    comp(src, names, &res);

    if (fileLoad && !eval)
    {
        /*----------------------------------------------------------------*/
        /* We loaded a file, so cons the definitions from outermost       */
        /* letrec block in file to current top level environment          */
        /*----------------------------------------------------------------*/
        tlNames = cons(newTopLevelNames, tlNames);
    }
    UNPROTECT(joinCont);
    UNPROTECT(rtnCont);
    UNPROTECT(src);
    UNPROTECT(res);
    return res;
}
