/**********************************************************************/
/*                                                                    */
/* vm.c:    LISP-KIT virtual machine interpreter                      */
/*                                                                    */
/* LispMe System (c) FBI Fred Bayer Informatics                       */
/*                                                                    */
/* Distributed under the GNU General Public License;                  */
/* see the README file. This code comes with NO WARRANTY.             */
/*                                                                    */
/**********************************************************************/
#include <math.h>
#include <stdlib.h>

#include "vm.h"
#include "io.h"
#include "comp.h"
#include "error.h"
#include "numeric.h"

extern IPTR topLevelVals;
extern IPTR newTopLevelVals;
extern char fileLoad;
extern long numStep;

#ifdef DUMP_STEPS
   extern FILE* logFile;
#endif

short expectedLen;

/**********************************************************************/
/* Imported functions and data                                        */
/**********************************************************************/
extern void typeError(IPTR p, char* type);
extern void parmError(IPTR p, char* type);
extern int  broken;

/**********************************************************************/
/* Make Lisp type for a long number                                   */
/**********************************************************************/
IPTR makeNum (long n)
{
    if (MININT <= n && n <= MAXINT)
        return MKINT(n);

    return allocReal(n);
}
/**********************************************************************/
/* Integer and double arithmetic                                      */
/**********************************************************************/
static IPTR addInt (long a, long b)
{
    long res = a+b;
    if (MININT <= res && res <= MAXINT)
        return MKINT(res);

    return allocReal(res);
}

static IPTR subInt(long a, long b)
{
    long res = a-b;
    if (MININT <= res && res <= MAXINT)
        return MKINT(res);

    return allocReal(res);
}

static IPTR mulInt(long a, long b)
{
    double res = (double)a * (double)b;
    if (MININT <= res && res <= MAXINT)
        return MKINT(a*b);

    return allocReal(res);
}

static double addDbl(double a, double b) {return a + b;};
static double subDbl(double a, double b) {return a - b;};
static double mulDbl(double a, double b) {return a * b;};

/**********************************************************************/
/* generic operaton with two arbitrary numbers                        */
/**********************************************************************/
static IPTR genBinOp(IPTR a, IPTR b,
                    IPTR    (*intOp)(long,long),
                    double (*dblOp)(double,double),
                    IPTR    (*cplOp)(double,double,double,double))
{
    if (IS_INT(a))
    {
        if (IS_INT(b))
            return intOp(INTVAL(a),INTVAL(b));

        if (IS_REAL(b))
            return allocReal(dblOp(INTVAL(a),getReal(b)));

        if (IS_COMPLEX(b))
            return cplOp(INTVAL(a),0.0,getReal(cadr(b)),getReal(cddr(b)));

        typeError(b,"number");
    }
    if (IS_REAL(a))
    {
        if (IS_INT(b))
            return allocReal(dblOp(getReal(a),INTVAL(b)));

        if (IS_REAL(b))
            return allocReal(dblOp(getReal(a),getReal(b)));

        if (IS_COMPLEX(b))
            return cplOp(getReal(a),0.0,getReal(cadr(b)),getReal(cddr(b)));

        typeError(b,"number");
    }
    if (IS_COMPLEX(a))
    {
        if (IS_INT(b))
            return cplOp(getReal(cadr(a)),getReal(cddr(a)),INTVAL(b),0.0);

        if (IS_REAL(b))
            return cplOp(getReal(cadr(a)),getReal(cddr(a)),getReal(b),0.0);

        if (IS_COMPLEX(b))
            return cplOp(getReal(cadr(a)),getReal(cddr(a)),
                         getReal(cadr(b)),getReal(cddr(b)));

        typeError(b, "number");
    }
    typeError(a, "number");
    return NIL;
}
/**********************************************************************/
/* divide two arbitrary numbers                                       */
/**********************************************************************/
static IPTR divide(IPTR a, IPTR b)
{
    if (IS_INT(a))
    {
        if (IS_INT(b))
        {
            if (INTVAL(b) != 0)
            {
                long a1 = INTVAL(a);
                long b1 = INTVAL(b);

                if (a1 % b1 != 0)
                    return allocReal((double) a1 / (double)b1);

                return makeNum(a1 / b1);

            }
            error(ERR_R8_DIV_BY_ZERO,NIL);
        }
        if (IS_REAL(b))
        {
            if (getReal(b) != 0.0)
                return allocReal(INTVAL(a) / getReal(b));

            error(ERR_R8_DIV_BY_ZERO,NIL);
        }
        if (IS_COMPLEX(b))
            return divCpl(INTVAL(a), 0.0, getReal(cadr(b)), getReal(cddr(b)));

        typeError(b,"number");
    }
    if (IS_REAL(a))
    {
        if (IS_INT(b))
        {
            if (INTVAL(b) != 0)
                return allocReal(getReal(a) / INTVAL(b));

            error(ERR_R8_DIV_BY_ZERO,NIL);
        }
        if (IS_REAL(b))
        {
            if (getReal(b) != 0.0)
                return allocReal(getReal(a) / getReal(b));

            error(ERR_R8_DIV_BY_ZERO,NIL);
        }
        if (IS_COMPLEX(b))
            return divCpl(getReal(a),0.0,getReal(cadr(b)), getReal(cddr(b)));

        typeError(b,"number");
    }
    if (IS_COMPLEX(a))
    {
        if (IS_INT(b))
        {
            if (INTVAL(b) != 0)
                return divCpl(getReal(cadr(a)),getReal(cddr(a)),INTVAL(b),0.0);

            error(ERR_R8_DIV_BY_ZERO,NIL);
        }
        if (IS_REAL(b))
        {
            if (getReal(b) != 0.0)
                return divCpl(getReal(cadr(a)),getReal(cddr(a)),getReal(b),0.0);

            error(ERR_R8_DIV_BY_ZERO,NIL);
        }
        if (IS_COMPLEX(b))
            return divCpl(getReal(cadr(a)),getReal(cddr(a)),
                          getReal(cadr(b)),getReal(cddr(b)));

        typeError(b, "number");
    }
    typeError(a, "number");
    return NIL;
}
/**********************************************************************/
/* compare equivalence (works for all numbers)                        */
/**********************************************************************/
static int eqv(IPTR a, IPTR b)
{
    if (a == b)
        return 1;

    if (IS_INT(a))
    {
        if (IS_REAL(b))
            return INTVAL(a) == getReal(b);

        return 0;
    }
    if (IS_REAL(a))
    {
        if (IS_INT(b))
            return getReal(a) == INTVAL(b);

        if (IS_REAL(b))
            return getReal(a) == getReal(b);

        return 0;
    }
    if (IS_COMPLEX(a) && IS_COMPLEX(b))
        return getReal(cadr(a)) == getReal(cadr(b))
            && getReal(cddr(a)) == getReal(cddr(b));

    return 0;
}
/**********************************************************************/
/* compare two arbitrary numbers, chars or strings                    */
/**********************************************************************/
static int leq(IPTR a, IPTR b)
{
    if (IS_INT(a))
    {
        if (IS_INT(b))
            return INTVAL(a) <= INTVAL(b);

        if (IS_REAL(b))
            return INTVAL(a) <= getReal(b);

        error(ERR_R14_INVALID_COMP,NIL);
    }
    if (IS_REAL(a))
    {
        if (IS_INT(b))
            return getReal(a) <= INTVAL(b);

        if (IS_REAL(b))
            return getReal(a) <= getReal(b);

        error(ERR_R14_INVALID_COMP,NIL);
    }
    if (IS_CHAR(a))
    {
        if (IS_CHAR(b))
            return CHARVAL(a) <= CHARVAL(b);

        error(ERR_R14_INVALID_COMP,NIL);
    }
    if (IS_STRING(a))
    {
        if (IS_STRING(b))
            return strComp(a,b) <= 0;

        error(ERR_R14_INVALID_COMP, NIL);
    }
    error(ERR_R14_INVALID_COMP, NIL);
    return 0;
}
/**********************************************************************/
/* Check, if el is in list l (using eqv?)                             */
/**********************************************************************/
static bool member(IPTR el, IPTR l)
{
    IPTR p = l;
    while (IS_PAIR(p))
    {
        if (eqv(el,car(p)))
            return true;

        p = cdr(p);
    }
    if (p != NIL)
        error(ERR_C6_IMPROPER_ARGS, l);

    return false;
}
/**********************************************************************/
/* Make a real from int or real                                       */
/**********************************************************************/
static double realVal(IPTR p)
{
    if (IS_INT(p))
        return INTVAL(p);

    if (IS_REAL(p))
        return getReal(p);

    typeError(p, "real");
    return 0.;
}
/**********************************************************************/
/* Transcendental functions                                           */
/**********************************************************************/
static void transUnary(double (*fn)(double))
{
    /*----------------------------------------------------------------*/
    /* (x.s) e (FKT.c) d --> (fkt(x).s) e c d                         */
    /*----------------------------------------------------------------*/
    S = cons(allocReal(fn(realVal(car(S)))), cdr(S));
    C = cdr(C);
}
static void transCplUnary(IPTR (*fn)(double,double))
{
    /*----------------------------------------------------------------*/
    /* (x.s) e (FKT.c) d --> (fkt(x).s) e c d                         */
    /*----------------------------------------------------------------*/
    W = car(S);
    if (IS_INT(W))
        S = cons(fn(INTVAL(W),0.0), cdr(S));

    else if (IS_REAL(W))
        S = cons(fn(getReal(W),0), cdr(S));

    else if (IS_COMPLEX(W))
        S = cons(fn(getReal(cadr(W)),
                    getReal(cddr(W))),
                 cdr(S));

    else
        typeError(W,"number");

    C = cdr(C);
}

static void transBinary(double (*fn)(double,double))
{
    /*----------------------------------------------------------------*/
    /* (x y.s) e (FKT.c) d --> (fkt(x,y).s) e c d                     */
    /*----------------------------------------------------------------*/
    S = cons(allocReal(fn(realVal(car(S)),realVal(cadr(S)))),
             cddr(S));

    C = cdr(C);
}
/**********************************************************************/
/* Store a number, reducing it to real if possible                    */
/**********************************************************************/
IPTR storeNum(double re, double im)
{
    if (im == 0.0)
        return allocReal(re);
    else
    {
        W = allocReal(re);
        return cons(CPLX_TAG, cons(W,allocReal(im)));
    }
}
/**********************************************************************/
/* Make complex number from polar representation                      */
/**********************************************************************/
IPTR makePolar(double mag, double ang)
{
    return storeNum(mag*cos(ang),mag*sin(ang));
}
/**********************************************************************/
/* Locate a variable in an environment                                */
/**********************************************************************/
IPTR locate(IPTR loc, LEXADR adr)
{
    int j = adr >> FRAME_SHIFT;
    while (--j>=0)
        loc = cdr(loc);

    loc = car(loc);
    if (loc == BLACK_HOLE)
        error(ERR_R9_BLACK_HOLE, NIL);

    j = adr & SLOT_MASK;
    while (--j >= 0)
        loc = cdr(loc);

    return loc;
}
/**********************************************************************/
/* common code for APC and TAPC                                       */
/**********************************************************************/
static void application(bool copyArgs)
{
    /*----------------------------------------------------------------*/
    /* Apply closure: first check number of arguments                 */
    /*----------------------------------------------------------------*/
    short i, len = INTVAL(cadar(S));

    /*----------------------------------------------------------------*/
    /* Copy args when applying to avoid destruction of original list  */
    /*----------------------------------------------------------------*/
    if (copyArgs)
    {
        W = cadr(S);
        listCopy(&cadr(S), cadr(S), 32767);
    }
    /*----------------------------------------------------------------*/
    /* Check number of arguments                                      */
    /*----------------------------------------------------------------*/
    if (len < 0)
    {
        /*------------------------------------------------------------*/
        /* Variable number of args, modify arg list                   */
        /*------------------------------------------------------------*/
        IPTR *ptr;
        if (-len -1 > listLength(cadr(S)))
        {
            expectedLen = len +1;
            error(ERR_R3_NUM_ARGS, cadr(S), len, listLength(cadr(S)));
        }
        for(ptr = &cadr(S), i=0; i < -len -1; i++)
            ptr = &cdr(*ptr);

        *ptr = cons(*ptr, NIL);
    }
    else if (len != listLength(cadr(S)))
        error(ERR_R3_NUM_ARGS, cadr(S), len, listLength(cadr(S)));
}
/**********************************************************************/
/* Check for integer                                                  */
/**********************************************************************/
static void checkInt(IPTR p)
{
    if (!IS_INT(p))
        typeError(p,"integer");
}

static void vm_dump(FILE *port)
{
    fputs("S = ", port); printSEXPfp(port, S, PRT_ESCAPE); fputc('\n', port);
    fputs("E = ", port); printSEXPfp(port, E, PRT_ESCAPE); fputc('\n', port);
    fputs("C = ", port); printSEXPfp(port, C, PRT_ESCAPE); fputc('\n', port);
    fputs("D = ", port); printSEXPfp(port, D, PRT_ESCAPE); fputc('\n', port);
    fputs("---------------------------------\n", port);
}
/**********************************************************************/
/* Predeclare static functions                                        */
/**********************************************************************/
static void vectorAcc(IPTR *obj, IPTR vec, IPTR n, bool write);
static void stringAcc(IPTR *c,   IPTR str, IPTR n, bool write);

/**********************************************************************/
/* Virtual machine (assume code is already loaded into C register)    */
/**********************************************************************/
IPTR exec(void)
{
    static char cvBuf[1024];

    IPTR loc;
    int i, op;

#ifdef DUMP_STEPS
    fputs("**********************************************\n", logFile);
#endif
    for (;;)
    {
        if (broken)
            error(ERR_O2_INTERRUPT,NIL);

        ++numStep;

#ifdef DUMP_STEPS
        vm_dump(logFile);
#endif
        if (broken)
            error(ERR_O2_INTERRUPT, NIL);

        switch (op = INTVAL(car(C)))
        {
        case SQRT: transCplUnary(&cplSqrt);  break;
        case EXP:  transCplUnary(&cplExp);   break;
        case LOG:  transCplUnary(&cplLog);   break;
        case SIN:  transCplUnary(&cplSin);   break;
        case COS:  transCplUnary(&cplCos);   break;
        case TAN:  transCplUnary(&cplTan);   break;
        case SINH: transCplUnary(&cplSinh);  break;
        case COSH: transCplUnary(&cplCosh);  break;
        case TANH: transCplUnary(&cplTanh);  break;
        case ASIN: transCplUnary(&cplAsin);  break;
        case ACOS: transCplUnary(&cplAcos);  break;
        case ATAN: transCplUnary(&cplAtan);  break;
        case ASIH: transCplUnary(&cplAsinh); break;
        case ACOH: transCplUnary(&cplAcosh); break;
        case ATAH: transCplUnary(&cplAtanh); break;
        case ATN2: transBinary(&atan2);      break;
        case FLOR: transUnary(&floor);       break;
        case CEIL: transUnary(&ceil);        break;
        case TRUN: transUnary(&trunc);       break;
        case ROUN: transUnary(&round);       break;

        case IDIV:
            /*------------------------------------------------------------*/
            /* (a b.s) e (IDIV.c) d --> ([b/a].s) e c d                   */
            /*------------------------------------------------------------*/
            checkInt(car(S));
            checkInt(cadr(S));

            if (INTVAL(cadr(S)) == 0)
                error(ERR_R8_DIV_BY_ZERO,NIL);

            S = cons(MKINT((INTVAL(car(S))) / (INTVAL(cadr(S)))), cddr(S));
            goto skip1;

        case INTG:
            /*------------------------------------------------------------*/
            /* (x.s) e (INTG.c) d --> (int(x).s) e c d                    */
            /*------------------------------------------------------------*/
            if (IS_INT(car(S)))
                goto skip1;

            if (IS_REAL(car(S)))
            {
                double res = getReal(car(S));
                if (res < MININT || res > MAXINT)
                    typeError(car(S),"integer");

                S = cons(MKINT((long)res),cdr(S));
            }
            else
                typeError(car(S),"real");

            goto skip1;

        case CPLP:
            /*------------------------------------------------------------*/
            /* (x.s) e (CPLP.c) d --> (#t.s) e c d if x is complex        */
            /* (a.s) e (CPLP.c) d --> (#f.s) e c d                        */
            /*------------------------------------------------------------*/
            S = cons(IS_INT(car(S)) || IS_REAL(car(S)) || IS_COMPLEX(car(S))
                     ? TRUE : FALSE, cdr(S));

            goto skip1;

        case REAP:
            /*------------------------------------------------------------*/
            /* (x.s) e (REAP.c) d --> (#t.s) e c d if x is real           */
            /* (a.s) e (REAP.c) d --> (#f.s) e c d                        */
            /*------------------------------------------------------------*/
            S = cons(IS_INT(car(S)) || IS_REAL(car(S))
                     ? TRUE : FALSE, cdr(S));

            goto skip1;

        case MKRA:
            /*------------------------------------------------------------*/
            /* (a b.s) e (MKRA.c) d --> (complex(a,b).s) e c d            */
            /*------------------------------------------------------------*/
            S = cons(storeNum(realVal(car(S)),realVal(cadr(S))), cddr(S));
            goto skip1;

        case MKPO:
            /*------------------------------------------------------------*/
            /* (a b.s) e (MKPO.c) d --> (polar(a,b).s) e c d              */
            /*------------------------------------------------------------*/
            S = cons(makePolar(realVal(car(S)),realVal(cadr(S))), cddr(S));
            goto skip1;

        case REPA:
            /*------------------------------------------------------------*/
            /* (z.s) e (REPA.c) d --> (re(z).s) e c d                     */
            /*------------------------------------------------------------*/
            if (IS_INT(car(S)) || IS_REAL(car(S)))
                goto skip1;

            if (IS_COMPLEX(car(S)))
                S = cons(cadar(S),cdr(S));
            else
                typeError(car(S),"number");

            goto skip1;

        case IMPA:
            /*------------------------------------------------------------*/
            /* (z.s) e (IMPA.c) d --> (im(z).s) e c d                     */
            /*------------------------------------------------------------*/
            if (IS_COMPLEX(car(S)))
                S = cons(cddar(S),cdr(S));

            else if (IS_REAL(car(S)) || IS_INT(car(S)))
                S = cons(MKINT(0),cdr(S));

            else
                typeError(car(S),"number");

            goto skip1;

        case MAGN:
            /*------------------------------------------------------------*/
            /* (z.s) e (MAGN.c) d --> (mag(z).s) e c d                    */
            /*------------------------------------------------------------*/
            S = cons(magnitude(car(S)),cdr(S));
            goto skip1;

        case ANGL:
            /*------------------------------------------------------------*/
            /* (z.s) e (ANGL.c) d --> (angle(z).s) e c d                  */
            /*------------------------------------------------------------*/
            S = cons(angle(car(S)),cdr(S));
            goto skip1;

        case EVA:
            /*------------------------------------------------------------*/
            /* (ex.s) e (EVA na.c) d --> NIL e compile(ex,na) (s e c.d)   */
            /*------------------------------------------------------------*/
            D = cons(cdr(S),cons(E,cons(cddr(C),D)));
            C = compile(car(S),cadr(C));
            S = NIL;
            break;

        case SYMV: /* hal: 16/8-00 */
            /*------------------------------------------------------------*/
            /* (symbol . s) e (SYMV na.c) d --> locate(symbol,na.s) e c d */
            /*------------------------------------------------------------*/
            if (!IS_ATOM(car(S)))
                typeError(car(S),"symbol");

            S = cons(car(locate(E,location(car(S),cadr(C),true))),S);
            goto skip2;

        case ST:
            /*------------------------------------------------------------*/
            /* (a.s) e (ST i.c) d --> (a.s) e' c d                        */
            /* where e' = update e set locate(i) = a                      */
            /*------------------------------------------------------------*/
            loc = locate(E,cadr(C));
            car(loc) = car(S);
        skip2:
            C = cddr(C);
            break;

        case LDC:
            /*------------------------------------------------------------*/
            /* s e (LDC x.c) d --> (x.s) e c d                            */
            /*------------------------------------------------------------*/
            S = cons(cadr(C),S);
            goto skip2;

        case LDF:
            /*------------------------------------------------------------*/
            /* s e (LDF c'.c) d --> ((c'.e).s) e c d                      */
            /*------------------------------------------------------------*/
            S = cons(cons(cadr(C),E), S);
            goto skip2;

        case AP:
            /*------------------------------------------------------------*/
            /* ((c'.e') v.s) e (AP.c) d --> NIL (v.e') c' (s e c.d)       */
            /*------------------------------------------------------------*/
            D = cons(cddr(S), cons(E, cons(cdr(C), D)));
            E = cons(cadr(S), cdar(S));
            C = caar(S); S = NIL;
            break;

        case LDFC:
            /*------------------------------------------------------------*/
            /* s e (LDFC n c'.c) d --> ((#clos n c'.e).s) e c d           */
            /*------------------------------------------------------------*/
            S = cons(cons(CLOS_TAG,
                          cons(cadr(C),
                               cons(caddr(C),E))),
                     S);
            C = cdddr(C);
            break;

        case APC:
        case APY:
            /*------------------------------------------------------------*/
            /* ((#clos n c'.e') v.s) e (APC.c) d -->                      */
            /*                              NIL (v.e') c' (s e c.d)       */
            /*  (special rule for variable number of args)                */
            /* ((#cont s e c.d) (v).s') e' (APC.c') d' --> (v.s) e c d    */
            /*------------------------------------------------------------*/
            if (!IS_CONS(car(S)))
                typeError(car(S), "function");

            if (caar(S) == CLOS_TAG)
            {
                application(INTVAL(car(C)) == APY);

                D = cons(cddr(S),
                         cons(E,
                              cons(cdr(C),D)));

                E = cons(cadr(S), cdddr(car(S)));
                C = caddr(car(S));
                S = NIL;
                break;
            }
            if (caar(S) == CONT_TAG)
            {
                /*--------------------------------------------------------*/
                /* apply continuation                                     */
                /*--------------------------------------------------------*/
            applyCont:
                if (listLength(cadr(S)) != 1)
                {
                    expectedLen = 1;
                    error(ERR_R3_NUM_ARGS,cadr(S));
                }
                E = cadr(cdar(S));
                C = caddr(cdar(S));
                D = cdddr(cdar(S));
                S = cons(caadr(S), cadar(S));
                break;
            }
            typeError(car(S), "function");
            break;

        case TAPC:
        case TAPY:
            /*------------------------------------------------------------*/
            /* ((#clos n c'.e') v.s) e (TAPC) d -->  s (v.e') c' d        */
            /*  (special rule for variable number of args)                */
            /* ((#cont s e c.d) (v).s') e' (TAPC) d' --> (v.s) e c d      */
            /*------------------------------------------------------------*/
            if (!IS_CONS(car(S)))
                typeError(car(S),"function");

            if (caar(S) == CLOS_TAG)
            {
                application(INTVAL(car(C)) == TAPY);
                E = cons(cadr(S), cdddr(car(S)));
                C = caddr(car(S)); S = cddr(S);
                break;
            }
            if (caar(S) == CONT_TAG)
                goto applyCont;

            typeError(car(S), "function");
            break;

        case TAP:
            /*------------------------------------------------------------*/
            /* ((c'.e') v.s) e (TAP.c) d --> s (v.e') c' d                */
            /*------------------------------------------------------------*/
            E = cons(cadr(S), cdar(S));
            C = caar(S);
            S = cddr(S);
            break;

        case RTN:
            /*------------------------------------------------------------*/
            /* (a) e' (RTN) (s e c.d) --> (a.s) e c d                     */
            /*------------------------------------------------------------*/

            /*------------------------------------------------------------*/
            /* Store environment, if we load from file                    */
            /*------------------------------------------------------------*/
            if (fileLoad)
                newTopLevelVals = car(E);

            S = cons(car(S), car(D));
            E = cadr(D);
            C = caddr(D);
            D = cdddr(D);
            break;

        case LDM:
            /*------------------------------------------------------------*/
            /* s e (LDM c'.c) d --> ((#macro #clos 1 c'.e).s) e c d       */
            /*------------------------------------------------------------*/
            S = cons(cons(MACR_TAG,
                          cons(CLOS_TAG,
                               cons(MKINT(1),
                                    cons(cadr(C),E)))),
                     S);
            C = cddr(C);
            break;

        case DUM:
            /*------------------------------------------------------------*/
            /* s e (DUM.c) d --> s (?.e) c d                              */
            /*------------------------------------------------------------*/
            E = cons(BLACK_HOLE, E);

        skip1:
            C = cdr(C);
            break;

        case RAP:
            /*------------------------------------------------------------*/
            /* ((c'.e') v.s) (?.e) (RAP.c) d -> NIL rpl(e',v) c' (s e c.d)*/
            /*------------------------------------------------------------*/
            D = cons(cddr(S), cons(cdr(E), cons(cdr(C), D)));

            E = cdar(S);
            car(E) = cadr(S);

            C = caar(S);
            S = NIL;
            break;

        case RTAP:
            /*------------------------------------------------------------*/
            /* ((c'.e') v.s) (?.e) (RTAP.c) d --> NIL rpl(e',v) c' d      */
            /*------------------------------------------------------------*/
            E = cdar(S);
            car(E) = cadr(S);

            C = caar(S);
            S = NIL;
            break;

        case SEL:
            /*------------------------------------------------------------*/
            /* (x.s)   e (SEL ct cf.c) d --> s e ct (c.d)                 */
            /* (#f.s)  e (SEL ct cf.c) d --> s e cf (c.d)                 */
            /*------------------------------------------------------------*/
            D = cons(cdddr(C), D);
            /* fall thru! */

        case SELR:
            /*------------------------------------------------------------*/
            /* (x.s)   e (SELR ct cf.c) d --> s e ct d                    */
            /* (#f.s)  e (SELR ct cf.c) d --> s e cf d                    */
            /*------------------------------------------------------------*/
            C = (car(S) == FALSE) ? caddr(C) : cadr(C);
            S = cdr(S);
            break;

        case SEO:
        case SEA:
            /*------------------------------------------------------------*/
            /* (x.s)   e (SEO cf.c) d --> (x.s) e c d                     */
            /* (#f.s)  e (SEA ct.c) d --> (#f.s) e c d                    */
            /* (#f.s)  e (SEO cf.c) d --> s e cf (c.d)                    */
            /* (x.s)   e (SEA ct.c) d --> s e ct (c.d)                    */
            /*------------------------------------------------------------*/
            if ((INTVAL(car(C)) == SEO) == (car(S) == FALSE)) {
                D = cons(cddr(C), D);
                S = cdr(S);
                C = cadr(C);
                break;
            }
            goto skip2;

        case SEOR:
        case SEAR:
            /*------------------------------------------------------------*/
            /* (x.s)   e (SEOR cf.c) d --> (x.s) e c d                    */
            /* (#f.s)  e (SEAR ct.c) d --> (#f.s) e c d                   */
            /* (#f.s)  e (SEOR cf.c) d --> s e cf d                       */
            /* (x.s)   e (SEAR ct.c) d --> s e ct d                       */
            /*------------------------------------------------------------*/
            if ((INTVAL(car(C)) == SEOR) == (car(S) == FALSE)) {
                S = cdr(S);
                C = cadr(C);
                break;
            }
            goto skip2;

        case MEM:
            /*------------------------------------------------------------*/
            /* (x.s) e (MEM l ct cf.c) d --> s     e ct (c.d) if x in l   */
            /* (x.s) e (MEM l ct cf.c) d --> (x.s) e cf (c.d) else        */
            /*------------------------------------------------------------*/
            D = cons(cdr(cdddr(C)), D);
            /* fall thru! */

        case MEMR:
            /*------------------------------------------------------------*/
            /* (x.s) e (MEMR l ct cf.c) d --> s     e ct d if x in l      */
            /* (x.s) e (MEMR l ct cf.c) d --> (x.s) e cf d else           */
            /*------------------------------------------------------------*/
            if (member(car(S), cadr(C))) {
                S = cdr(S);
                C = caddr(C);
                break;
            }
            C = car(cdddr(C));
            break;

        case JOIN:
            /*------------------------------------------------------------*/
            /* s e (JOIN) (c.d) -> s e c d                                */
            /*------------------------------------------------------------*/
            C = car(D);
            D = cdr(D);
            break;

        case LST:
            /*------------------------------------------------------------*/
            /* (v1 ... vn.s) e (LST n.c) d --> ((v1 ... vn).s) e c d      */
            /*------------------------------------------------------------*/
            if ((i = INTVAL(cadr(C))) != 0)
            {
                loc = S;
                while (--i > 0)
                    loc = cdr(loc);

                S = cons(S,cdr(loc));
                cdr(loc) = NIL;
            }
            else
                S = cons(NIL,S);

            goto skip2;

        case CAR:
            /*------------------------------------------------------------*/
            /* ((a.b).s) e (CAR.c) d --> (a.s) e c d                      */
            /*------------------------------------------------------------*/
            if (!IS_PAIR(car(S)))
                typeError(car(S),"pair");

            S = cons(caar(S),cdr(S));
            goto skip1;

        case CDR:
            /*------------------------------------------------------------*/
            /* ((a.b).s) e (CDR.c) d --> (b.s) e c d                      */
            /*------------------------------------------------------------*/
            if (!IS_PAIR(car(S)))
                typeError(car(S),"pair");

            S = cons(cdar(S),cdr(S));
            goto skip1;

        case CXR:
            /*------------------------------------------------------------*/
            /* (x.s) e (CXR bs . c) d --> (cxxxr(x) . s) e c d            */
            /* where bs is a bitstring encoding a sequence of car/cdr     */
            /* applications [hal: 15/8-00]                                */
            /*------------------------------------------------------------*/
            i = INTVAL(cadr(C));
            W = car(S);

            while (i > 1) {
                if (!IS_PAIR(W))
                    typeError(W,"pair");

               W = (i & 1 ? cdr(W) : car(W));
                i >>= 1;
            }
            S = cons(W,cdr(S));
            goto skip2;

        case SCAR:
            /*------------------------------------------------------------*/
            /* (a' (a.b) .s) e (SCAR.c) d --> ((a'.b) .s) e c d           */
            /*------------------------------------------------------------*/
            if (!IS_PAIR(cadr(S)))
                typeError(cadr(S),"pair");

            caadr(S) = car(S);
            S = cdr(S);
            goto skip1;

        case SCDR:
            /*------------------------------------------------------------*/
            /* (b' (a.b) .s) e (SCDR.c) d --> ((a.b') .s) e c d           */
            /*------------------------------------------------------------*/
            if (!IS_PAIR(cadr(S)))
                typeError(cadr(S),"pair");

            cdadr(S) = car(S);
            S = cdr(S);
            goto skip1;

        case PAIR:
            /*------------------------------------------------------------*/
            /* ((a.b).s) e (PAIR.c) d --> (#t.s) e c d                    */
            /* (  x  .s) e (PAIR.c) d --> (#f.s) e c d                    */
            /*------------------------------------------------------------*/
            S = cons(IS_PAIR(car(S)) ? TRUE : FALSE, cdr(S));
            goto skip1;

        case NUL:
            /*------------------------------------------------------------*/
            /* (() .s) e (NUL.c) d --> (#t.s) e c d                       */
            /* (x  .s) e (NUL.c) d --> (#f.s) e c d                       */
            /*------------------------------------------------------------*/
            S = cons((car(S) == NIL) ? TRUE : FALSE, cdr(S));
            goto skip1;

        case CHR:
            /*------------------------------------------------------------*/
            /* (#\? .s) e (CHR.c) d --> (#t.s) e c d                      */
            /* (x   .s) e (CHR.c) d --> (#f.s) e c d                      */
            /*------------------------------------------------------------*/
            S = cons(IS_CHAR(car(S)) ? TRUE : FALSE, cdr(S));
            goto skip1;

        case STRG:
            /*------------------------------------------------------------*/
            /* ("..." .s) e (STRG.c) d --> (#t.s) e c d                   */
            /* (x     .s) e (STRG.c) d --> (#f.s) e c d                   */
            /*------------------------------------------------------------*/
            S = cons(IS_STRING(car(S)) ? TRUE : FALSE, cdr(S));
            goto skip1;

        case PROC:
            /*------------------------------------------------------------*/
            /* ([clos] . s) e (PROC.c) d --> (#t.s) e c d                 */
            /* (x      . s) e (PROC.c) d --> (#f.s) e c d                 */
            /*------------------------------------------------------------*/
            S = cons(IS_CONS(car(S)) &&
                     (caar(S) == CLOS_TAG ||
                      caar(S) == CONT_TAG)  ? TRUE : FALSE, cdr(S));
            goto skip1;

        case DASM:
            /*------------------------------------------------------------*/
            /* ([clos] . s) e (DASM.c) d --> (caddr(clos).s) e c d        */
            /* ([macr] . s) e (DASM.c) d --> (cadddr(macro).s) e c d      */
            /*------------------------------------------------------------*/
            if (!IS_CONS(car(S)) || caar(S) != CLOS_TAG)
            {
                if (!IS_CONS(car(S)) || caar(S) != MACR_TAG)
                    typeError(car(S),"procedure");

                S = cons(car(cdddr(car(S))), cdr(S));
            }
            else
                S = cons(caddr(car(S)), cdr(S));

            goto skip1;

        case CONT:
            /*------------------------------------------------------------*/
            /* ([cont] . s) e (PROC.c) d --> (#t.s) e c d                 */
            /* (x      . s) e (PROC.c) d --> (#f.s) e c d                 */
            /*------------------------------------------------------------*/
            S = cons(IS_CONS(car(S)) &&
                     caar(S) == CONT_TAG  ? TRUE : FALSE, cdr(S));
            goto skip1;

        case PROM:
            /*------------------------------------------------------------*/
            /* ([prom] . s) e (PROM.c) d --> (#t.s) e c d                 */
            /* (x      . s) e (PROM.c) d --> (#f.s) e c d                 */
            /*------------------------------------------------------------*/
            S = cons(IS_CONS(car(S)) &&
                     (caar(S) == RCPD_TAG ||
                      caar(S) == RCPF_TAG)  ? TRUE : FALSE, cdr(S));
            goto skip1;

        case MACP:
            /*------------------------------------------------------------*/
            /* ([macro]. s) e (MACP.c) d --> (#t.s) e c d                 */
            /* (x      . s) e (MACP.c) d --> (#f.s) e c d                 */
            /*------------------------------------------------------------*/
            S = cons(IS_MACRO(car(S)) ? TRUE : FALSE, cdr(S));
            goto skip1;

        case SYMB:
            /*------------------------------------------------------------*/
            /* (symbol . s) e (SYMB.c) d --> (#t.s) e c d                 */
            /* (x      . s) e (SYMB.c) d --> (#f.s) e c d                 */
            /*------------------------------------------------------------*/
            S = cons(IS_ATOM(car(S)) ? TRUE : FALSE, cdr(S));
            goto skip1;

        case NONE:
            /*------------------------------------------------------------*/
            /* (#n . s) e (NONE.c) d --> (#t.s) e c d                     */
            /* (x  . s) e (NONE.c) d --> (#f.s) e c d                     */
            /*------------------------------------------------------------*/
            S = cons(car(S) == NOPRINT ? TRUE : FALSE, cdr(S));
            goto skip1;

        case INTP:
            /*------------------------------------------------------------*/
            /* (n.s) e (INTP.c) d --> (#t.s) e c d                        */
            /* (a.s) e (INTP.c) d --> (#f.s) e c d                        */
            /*------------------------------------------------------------*/
            S = cons(IS_INT(car(S)) ? TRUE : FALSE, cdr(S));
            goto skip1;

        case NOT:
            /*------------------------------------------------------------*/
            /* (#f .s) e (NOT.c) d --> (#t.s) e c d                       */
            /* (x  .s) e (NOT.c) d --> (#f.s) e c d                       */
            /*------------------------------------------------------------*/
            S = cons((car(S) == FALSE) ? TRUE : FALSE,
                     cdr(S));
            goto skip1;

        case BOOL:
            /*------------------------------------------------------------*/
            /* (#f .s) e (NUL.c) d --> (#t.s) e c d                       */
            /* (#t .s) e (NUL.c) d --> (#t.s) e c d                       */
            /* (x  .s) e (NUL.c) d --> (#f.s) e c d                       */
            /*------------------------------------------------------------*/
            S = cons((car(S) == TRUE || car(S) == FALSE) ? TRUE : FALSE,
                     cdr(S));
            goto skip1;

        case INPP:
            /*------------------------------------------------------------*/
            /* (inport.s) e (INPP.c) d --> (#t.s) e c d                   */
            /* (a.s) e (INPP.c) d --> (#f.s) e c d                        */
            /*------------------------------------------------------------*/
            S = cons(IS_IPORT(car(S)) ? TRUE : FALSE,
                     cdr(S));
            goto skip1;

        case OUPP:
            /*------------------------------------------------------------*/
            /* (outport.s) e (OUPP.c) d --> (#t.s) e c d                  */
            /* (a.s) e (OUPP.c) d --> (#f.s) e c d                        */
            /*------------------------------------------------------------*/
            S = cons(IS_OPORT(car(S)) ? TRUE : FALSE,
                     cdr(S));
            goto skip1;

        case EOFO:
            /*------------------------------------------------------------*/
            /* (#eof.s) e (EOFO.c) d --> (#t.s) e c d                     */
            /* (a.s) e (EOFO.c) d --> (#f.s) e c d                        */
            /*------------------------------------------------------------*/
            S = cons((car(S) == END_OF_FILE) ? TRUE : FALSE,
                     cdr(S));
            goto skip1;

        case VECP:
            /*------------------------------------------------------------*/
            /* (#(...).s) e (VECP.c) d --> (#t.s) e c d                   */
            /* (a.s)      e (VECP.c) d --> (#f.s) e c d                   */
            /*------------------------------------------------------------*/
            S = cons(IS_VEC(car(S)) ? TRUE : FALSE,
                     cdr(S));
            goto skip1;

        case CONS:
            /*------------------------------------------------------------*/
            /* (a b.s) e (CONS.c) d --> ((a.b).s) e c d                   */
            /*------------------------------------------------------------*/
            S = cons(cons(car(S),cadr(S)),cddr(S));
            goto skip1;

        case APND:
            /*------------------------------------------------------------*/
            /* (a b.s) e (APND.c) d --> (append(a,b).s) e c d             */
            /*------------------------------------------------------------*/
            if (car(S) != NIL && !IS_PAIR(car(S)))
                typeError(car(S),"list");

            W = NIL;
            *listCopy(&W,car(S),32767) = cadr(S);

            S = cons(W, cddr(S));
            goto skip1;

        case EQ:
            /*------------------------------------------------------------*/
            /* (a a.s) e (EQ.c) d --> (#t.s) e c d                        */
            /* (a b.s) e (EQ.c) d --> (#f.s) e c d                        */
            /*------------------------------------------------------------*/
            S = cons(car(S) == cadr(S) ? TRUE : FALSE, cddr(S));
            goto skip1;

        case EQV:
            /*------------------------------------------------------------*/
            /* (a a.s) e (EQ.c) d --> (#t.s) e c d                        */
            /* (a b.s) e (EQ.c) d --> (#f.s) e c d                        */
            /* additionally compare reals                                 */
            /*------------------------------------------------------------*/
            S = cons(eqv(car(S),cadr(S)) ? TRUE : FALSE, cddr(S));
            goto skip1;

        case ADD:
            /*------------------------------------------------------------*/
            /* (a b.s) e (ADD.c) d --> (b+a.s) e c d                      */
            /*------------------------------------------------------------*/
            S = cons(genBinOp(car(S), cadr(S), &addInt, &addDbl, &addCpl),
                     cddr(S));
            goto skip1;

        case SUB:
            /*------------------------------------------------------------*/
            /* (a b.s) e (SUB.c) d --> (b-a.s) e c d                      */
            /*------------------------------------------------------------*/
            S = cons(genBinOp(car(S),cadr(S),&subInt,&subDbl,&subCpl),
                     cddr(S));
            goto skip1;

        case MUL:
            /*------------------------------------------------------------*/
            /* (a b.s) e (MUL.c) d --> (b*a.s) e c d                      */
            /*------------------------------------------------------------*/
            S = cons(genBinOp(car(S),cadr(S),&mulInt,&mulDbl,&mulCpl),
                     cddr(S));
            goto skip1;

        case DIV:
            /*------------------------------------------------------------*/
            /* (a b.s) e (DIV.c) d --> (b/a.s) e c d                      */
            /*------------------------------------------------------------*/
            S = cons(divide(car(S),cadr(S)), cddr(S));
            goto skip1;

        case REM:
            /*------------------------------------------------------------*/
            /* (a b.s) e (REM.c) d --> (b%a.s) e c d                      */
            /*------------------------------------------------------------*/
            checkInt(car(S));
            checkInt(cadr(S));

            if (INTVAL(cadr(S)) == 0)
                error(ERR_R8_DIV_BY_ZERO,NIL);

            S = cons(MKINT((INTVAL(car(S))) % (INTVAL(cadr(S)))), cddr(S));
            goto skip1;

        case LAND:
            /*------------------------------------------------------------*/
            /* (a b.s) e (LAND.c) d --> (b&a . s) e c d                   */
            /*------------------------------------------------------------*/
            checkInt(car(S));
            checkInt(cadr(S));

            /* optimization! lsb is always 1, no need to unwrap/wrap */
            S = cons(car(S) & cadr(S), cddr(S));
            goto skip1;

        case LIOR:
            /*------------------------------------------------------------*/
            /* (a b.s) e (LIOR.c) d --> (b|a . s) e c d                   */
            /*------------------------------------------------------------*/
            checkInt(car(S));
            checkInt(cadr(S));

            /* optimization! lsb is always 1, no need to unwrap/wrap */
            S = cons(car(S) | cadr(S), cddr(S));
            goto skip1;

        case LXOR:
            /*------------------------------------------------------------*/
            /* (a b.s) e (LXOR.c) d --> (b^a . s) e c d                   */
            /*------------------------------------------------------------*/
            checkInt(car(S));
            checkInt(cadr(S));

            S = cons(MKINT(INTVAL(car(S)) ^ INTVAL(cadr(S))), cddr(S));
            goto skip1;

        case LSHT:
            /*------------------------------------------------------------*/
            /* (a b.s) e (LSHT.c) d --> (b<<a . s) e c d                  */
            /*------------------------------------------------------------*/
            checkInt(car(S));
            checkInt(cadr(S));
            i = INTVAL(cadr(S));

            if (i < 0)
                S = i < 0 ?
                    cons(MKINT(INTVAL(car(S)) >> (-i)), cddr(S))
                    :
                    cons(MKINT(INTVAL(car(S)) << i), cddr(S));

            goto skip1;

        case LNOT:
            /*------------------------------------------------------------*/
            /* (a.s) e (LNOT.c) d --> (~a . s) e c d                      */
            /*------------------------------------------------------------*/
            checkInt(car(S));

            S = cons(MKINT(~INTVAL(car(S))), cdr(S));
            goto skip1;

        case LEQ:
            /*------------------------------------------------------------*/
            /* (a b.s) e (LEQ.c) d --> (T.s)   e c d  if b>=a             */
            /* (a b.s) e (LEQ.c) d --> (NIL.s) e c d  if b< a             */
            /*------------------------------------------------------------*/
            S = cons(leq(car(S),cadr(S)) ? TRUE : FALSE, cddr(S));
            goto skip1;

        case STOP:
            /*------------------------------------------------------------*/
            /* (v.s) e (STOP) d --> stop returning v                      */
            /*------------------------------------------------------------*/
            if (fileLoad && !macroExpand)
            {
                /*----------------------------------------------------------*/
                /* Extend top level environment                             */
                /*----------------------------------------------------------*/
                tlVals = cons(newTopLevelVals,tlVals);
                newTopLevelVals = NIL;
            }
            return car(S);

        case LDE:
            /*------------------------------------------------------------*/
            /* s e (LDE c.c') d --> ((#rcpd c.e).s) e c' d                */
            /*------------------------------------------------------------*/
            S = cons(cons(RCPD_TAG, cons(cadr(C), E)), S);
            goto skip2;

        case AP0:
            /*------------------------------------------------------------*/
            /* ((#rcpd c.e).s) e' (AP0.c') d -->                          */
            /*                          NIL e c (((#rcpd c.e).s) e' c'.d) */
            /* ((#rcpf.x).s) e (AP0.c) d --> (x.s) e c d                  */
            /*------------------------------------------------------------*/
            if (!IS_CONS(car(S)))
                typeError(car(S),"promise");

            if (caar(S) == RCPD_TAG)
            {
                D = cons(S, cons(E, cons(cdr(C), D)));
                C = cadar(S); E = cddar(S); S = NIL;
                break;
            }
            if (caar(S) == RCPF_TAG)
            {
                S = cons(cdar(S), cdr(S));
                goto skip1;
            }
            typeError(car(S),"promise");
            break;

        case UPD:
            /*------------------------------------------------------------*/
            /* (x) e (UPD) (((#rcpd c.e).s) e' c'.d) --> (x.s) e' c' d    */
            /* and replace (#rcpd c.e) by (#rcpf.x)                       */
            /*------------------------------------------------------------*/
            S = cons(car(S), cdar(D));
            E = cadr(D);
            C = caddr(D);

            caaar(D) = RCPF_TAG;
            cdaar(D) = car(S);
            D = cdddr(D);
            break;

        case LDCT:
            /*------------------------------------------------------------*/
            /* s e (LDCT c'.c) d --> (((#cont s e c'.d)).s) e c d         */
            /*------------------------------------------------------------*/
            S = cons(cons(cons(CONT_TAG,
                               cons(S,
                                    cons(E,
                                         cons(cadr(C),
                                              D)))),
                          NIL),
                     S);

            goto skip2;

        case UERR:
            /*------------------------------------------------------------*/
            /* (x.s) e (UERR.c) d --> abort interpreter                   */
            /*------------------------------------------------------------*/
            error(ERR_USER_ERROR, car(S));
            break;

        case CERR:
            /*------------------------------------------------------------*/
            /* Abort uncondionally                                        */
            /*------------------------------------------------------------*/
            error(ERR_R10_COND_CLAUSE,NIL);
            break;

        case POP:
            /*------------------------------------------------------------*/
            /* (x.s) e (POP.c) d --> s e c d                              */
            /*------------------------------------------------------------*/
            S = cdr(S);
            goto skip1;

        case DSPL:
            /*------------------------------------------------------------*/
            /* (p x.s) e (DSPL.c) d --> (x.s) e c d and display x on port */
            /*------------------------------------------------------------*/
            if (car(S)==MKINT(0))
                printSEXPfp(stdout,cadr(S),0);
            else
            {
                if (!IS_OPORT(car(S)))
                    typeError(car(S),"output port");

                printSEXPfp((FILE*)(cdar(S)),cadr(S), 0);
            }
            S = cdr(S);
            goto skip1;

        case WRIT:
            /*------------------------------------------------------------*/
            /* (p x.s) e (WRIT.c) d --> (x.s) e c d and write x on port   */
            /*------------------------------------------------------------*/
            if (car(S) != MKINT(0))
            {
                if (!IS_OPORT(car(S)))
                    typeError(car(S),"output port");

                printSEXPfp((FILE*)(cdar(S)),cadr(S), PRT_ESCAPE | PRT_SPACE);
            }
            else
                printSEXPfp(stdout,cadr(S), PRT_ESCAPE | PRT_SPACE);

            S = cdr(S);
            goto skip1;

        case SLEN:
            /*------------------------------------------------------------*/
            /* ((#STR . l) . s) e (SLEN.c) d --> (length(l).s) e c d      */
            /*------------------------------------------------------------*/
            if (!IS_STRING(car(S)))
                typeError(car(S),"string");

            S = cons(MKINT(LENGTH(car(S))), cdr(S));
            goto skip1;

        case S2L:
            /*------------------------------------------------------------*/
            /* ((#STR . l) . s) e (S2L.c) d --> (copy(l).s) e c d         */
            /*------------------------------------------------------------*/
            if (!IS_STRING(car(S)))
                typeError(car(S),"string");

            S = cons(string2List(car(S)),cdr(S));
            goto skip1;

        case L2S:
            /*------------------------------------------------------------*/
            /* (l . s) e (L2S.c) d --> ((#STR copy(l)) . s) e c d         */
            /*------------------------------------------------------------*/
            S = cons(makeString(listLength(car(S)),0,0,0,car(S)), cdr(S));
            goto skip1;

        case SAPP:
            /*------------------------------------------------------------*/
            /* ((#STR.l1) (#STR.l2) . s) e (SAPP.c) d -->                 */
            /*      ((#STR copy(l1) copy(l2)) . s) e c d                  */
            /*------------------------------------------------------------*/
            S = cons(appendStrings(car(S), cadr(S)), cddr(S));
            goto skip1;

        case SEQ:
            /*------------------------------------------------------------*/
            /* ((#STR.l1) (#STR.l2) . s) e (SEQ.c) d --> (l1==l2 .s) e c d*/
            /*------------------------------------------------------------*/
            if (!IS_STRING(car(S)))
                typeError(car(S),"string");

            if (!IS_STRING(cadr(S)))
                typeError(cadr(S),"string");

            S = cons(strComp(car(S), cadr(S)) == 0 ? TRUE : FALSE, cddr(S));
            goto skip1;

        case SREF:
            /*------------------------------------------------------------*/
            /* ((#STR.l) n . s) e (SREF.c) d --> (access(l,n) . s) e c d  */
            /*------------------------------------------------------------*/
            stringAcc(&W, car(S), cadr(S), false);
            S = cons(W, cddr(S));
            goto skip1;

        case SSET:
            /*------------------------------------------------------------*/
            /* ((#STR.l) n ch . s) e (SSET.c) d -->                       */
            /*      ((#STR set(l,n,ch)) . s) e c d                        */
            /*------------------------------------------------------------*/
            stringAcc(&caddr(S),car(S),cadr(S),true);
            S = cons(car(S),cdddr(S));
            goto skip1;

        case SUBS:
            /*------------------------------------------------------------*/
            /* ((#STR.l) n k . s) e (SUBS.c) d -->                        */
            /*      ((#STR substr(l,n,k)) . s) e c d                      */
            /*------------------------------------------------------------*/
            checkInt(cadr(S));
            checkInt(caddr(S));

            S = cons(substring(car(S),INTVAL(cadr(S)),INTVAL(caddr(S))),
                     cdddr(S));
            goto skip1;

        case SMAK:
            /*------------------------------------------------------------*/
            /* (n f . s) e (SMAK.c) d --> (mkString(n,f) . s) e c d       */
            /*------------------------------------------------------------*/
            checkInt(car(S));
            if (!IS_CHAR(cadr(S)))
                typeError(cadr(S),"char");

            if (INTVAL(car(S)) < 0)
                parmError(car(S),"make-string");

            S = cons(makeString(INTVAL(car(S)),0,0,(char*)1,cadr(S)), cddr(S));
            goto skip1;

        case C2I:
            /*------------------------------------------------------------*/
            /* (ch . s) e (C2I.c) d --> (asc(ch) . s) e c d               */
            /*------------------------------------------------------------*/
            if (!IS_CHAR(car(S)))
                typeError(car(S),"char");

            S = cons(MKINT(CHARVAL(car(S))), cdr(S));
            goto skip1;

        case I2C:
            /*------------------------------------------------------------*/
            /* (n . s) e (C2I.c) d --> (chr(n) . s) e c d                 */
            /*------------------------------------------------------------*/
            checkInt(car(S));
            S = cons(MKCHAR(INTVAL(car(S)) & 0xff), cdr(S));
            goto skip1;

        case O2S:
            /*------------------------------------------------------------*/
            /* (x.s) e (O2S.c) d --> (format(x).s) e c d                  */
            /*------------------------------------------------------------*/
            printSEXP(cvBuf,sizeof(cvBuf),car(S), PRT_ESCAPE);
            S = cons(str2Lisp(cvBuf), cdr(S));
            goto skip1;

        case S2O:
            /*------------------------------------------------------------*/
            /* (x.s) e (S2O.c) d --> (read(x).s) e c d                    */
            /*------------------------------------------------------------*/
            if (!IS_STRING(car(S)))
                typeError(car(S),"string");

            printSEXP(cvBuf,sizeof(cvBuf),car(S),0);
            S = cons(readSEXP(cvBuf), cdr(S));
            goto skip1;

        case RAND:
            /*------------------------------------------------------------*/
            /* (n . s) e (RAND.c) d --> (rand(n) . s) e c d               */
            /*------------------------------------------------------------*/
            checkInt(car(S));
            if (INTVAL(car(S)) == 0)
                error(ERR_R8_DIV_BY_ZERO,NIL);

            S = cons(MKINT(rand() % INTVAL(car(S))), cdr(S));
            goto skip1;

        case READ:
        case REAC:
        case REDL:
        case PEEK:
            /*------------------------------------------------------------*/
            /* (port.s) e (OPxx.c) d --> (ex.s) e c d                     */
            /* where ex is read from port in different ways               */
            /*------------------------------------------------------------*/
            S = cons(readFile(car(S), INTVAL(car(C))), cdr(S));
            goto skip1;

        case OOUT:
            /*------------------------------------------------------------*/
            /* (name.s) e (OOUT.c) d --> (port.s) e c d                   */
            /*------------------------------------------------------------*/
            S = cons(openPort(PMODE_OUTPUT,car(S)), cdr(S));
            goto skip1;

        case OINP:
            /*------------------------------------------------------------*/
            /* (name.s) e (OINP.c) d --> (port.s) e c d                   */
            /*------------------------------------------------------------*/
            S = cons(openPort(PMODE_INPUT,car(S)), cdr(S));
            goto skip1;

        case OAPP:
            /*------------------------------------------------------------*/
            /* (name.s) e (OAPP.c) d --> (port.s) e c d                   */
            /*------------------------------------------------------------*/
            S = cons(openPort(PMODE_APPEND,car(S)), cdr(S));
            goto skip1;

        case CLOS:
            /*------------------------------------------------------------*/
            /* (port.s) e (CLOS.c) d --> (closed port.s) e c d            */
            /*------------------------------------------------------------*/
            closePort(car(S));
            goto skip1;

        case DELF:
            /*------------------------------------------------------------*/
            /* (name.s) e (DELF.c) d --> (#n.s) e c d                     */
            /* and delete memo <name>                                     */
            /*------------------------------------------------------------*/
            deleteFile(car(S));
            S = cons(NOPRINT,cdr(S));
            goto skip1;

        case VMAK:
            /*------------------------------------------------------------*/
            /* (n f . s) e (VMAK.c) d --> (#(f...f).s) e c d              */
            /*------------------------------------------------------------*/
            checkInt(car(S));
            if (INTVAL(car(S)) < 0)
                parmError(car(S),"make-vector");

            S = cons(makeVector(INTVAL(car(S)), cadr(S), false), cddr(S));
            goto skip1;

        case VLEN:
            /*------------------------------------------------------------*/
            /* (vec . s) e (VLEN.c) d --> (length(vec).s) e c d           */
            /*------------------------------------------------------------*/
            if (!IS_VEC(car(S)))
                typeError(car(S),"vector");

            S = cons(MKINT(LENGTH(car(S))), cdr(S));
            goto skip1;

        case VREF:
            /*------------------------------------------------------------*/
            /* (vec n . s) e (VREF.c) d --> (access(vec,n) . s) e c d     */
            /*------------------------------------------------------------*/
            vectorAcc(&W, car(S), cadr(S), false);
            S = cons(W, cddr(S));
            goto skip1;

        case VSET:
            /*------------------------------------------------------------*/
            /* (vec n x . s) e (VSET.c) d --> (vec'. s) e c d             */
            /* where vec' is vec updated at index n with x                */
            /*------------------------------------------------------------*/
            vectorAcc(&caddr(S),car(S),cadr(S),true);
            S = cons(car(S),cdddr(S));
            goto skip1;

        case V2L:
            /*------------------------------------------------------------*/
            /* (vec . s) e (V2L.c) d --> (list(vec).s) e c d              */
            /*------------------------------------------------------------*/
            if (!IS_VEC(car(S)))
                typeError(car(S),"vector");

            S = cons(vector2List(car(S)),cdr(S));
            goto skip1;

        case L2V:
            /*------------------------------------------------------------*/
            /* (l . s) e (L2V.c) d --> (vector(l) . s) e c d              */
            /*------------------------------------------------------------*/
            S = cons(makeVector(listLength(car(S)), car(S), true), cdr(S));
            goto skip1;

        case GSYM:
            /*------------------------------------------------------------*/
            /* s e (GSYM.c) d --> (Gxxx.s) e c d                          */
            /*------------------------------------------------------------*/
            S = cons(gensym(),S);
            goto skip1;

        case DOGC:
            /*------------------------------------------------------------*/
            /* s e (DOGC.c) d --> (#n . s) e c d                          */
            /*------------------------------------------------------------*/
            gc(NIL, NIL);
            S = cons(NOPRINT,S);
            goto skip1;

            /*============================================================*/
            /* Some frequent LDC constants                                */
            /* s e (LC?? . c) d --> (??.s) e c d                          */
            /*============================================================*/
        case LC0:
            S = cons(MKINT(0),S);
            goto skip1;

        case LC1:
            S = cons(MKINT(1),S);
            goto skip1;

        case LC2:
            S = cons(MKINT(2),S);
            goto skip1;

        case LC3:
            S = cons(MKINT(3),S);
            goto skip1;

        case LC_1:
            S = cons(MKINT(-1),S);
            goto skip1;

        case LC_T:
            S = cons(TRUE,S);
            goto skip1;

        case LC_F:
            S = cons(FALSE,S);
            goto skip1;

        case LC_N:
            S = cons(NIL,S);
            goto skip1;

        case LC_E:
            S = cons(EMPTY_STR,S);
            goto skip1;

            /*============================================================*/
            /* Some frequent LD lexical addresses are hardcoded below     */
            /*============================================================*/
        case -0: /* (0 . 0) */
            if ((loc = car(E)) == BLACK_HOLE)
                error(ERR_R9_BLACK_HOLE,NIL);

            S = cons(car(loc),S);
            goto skip1;

        case -1: /* (0 . 1) */
            if ((loc = car(E)) == BLACK_HOLE)
                error(ERR_R9_BLACK_HOLE,NIL);

            S = cons(cadr(loc),S);
            goto skip1;

        case -2: /* (0 . 2) */
            if ((loc=car(E)) == BLACK_HOLE)
                error(ERR_R9_BLACK_HOLE,NIL);

            S = cons(caddr(loc),S);
            goto skip1;

        default:
            if (op < 0) {
                /*----------------------------------------------------*/
                /* negative opcodes denote LD instructions:           */
                /* s e (-n . c) d --> (locate(n,e) . s) e c d         */
                /*----------------------------------------------------*/
                loc = locate(E,-op);
                S = cons(car(loc),S);
                goto skip1;
            }
            /*--------------------------------------------------------*/
            /* illegal opcode                                         */
            /*--------------------------------------------------------*/
            error(ERR_R7_ILLEGAL_OP,NIL,INTVAL(car(C)));
            break;
        } /* switch */
    } /* for */
    return NIL;
}
/**********************************************************************/
/* Access a vector                                                    */
/**********************************************************************/
static void vectorAcc(IPTR* obj, IPTR vec, IPTR n, bool write)
{
    long i;

    if (!IS_VEC(vec))
        typeError(vec,"vector");

    if (!IS_INT(n))
        typeError(n, "integer");

    if ((i=INTVAL(n)) < 0 || i >= LENGTH(vec))
        error(ERR_R2_INVALID_INDEX,n);

    if (write)
        VECTORMEM(vec)[i] = *obj;

    else
        *obj = VECTORMEM(vec)[i];
}
/**********************************************************************/
/* Access a string                                                    */
/**********************************************************************/
static void stringAcc(IPTR* c, IPTR str, IPTR n, bool write)
{
    long i;

    if (!IS_STRING(str))
        typeError(str,"string");

    if (!IS_INT(n))
        typeError(n, "integer");

    if (write && !IS_CHAR(*c))
        typeError(*c, "char");

    if ((i = INTVAL(n)) < 0 || i >= LENGTH(str))
        error(ERR_R2_INVALID_INDEX,n);

    if (write)
        STRINGMEM(str)[i] = CHARVAL(*c);

    else
        *c = MKCHAR(STRINGMEM(str)[i]);
}
