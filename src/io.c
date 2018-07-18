/**********************************************************************/
/*                                                                    */
/* io.c:    I/O of symbolic expressions                               */
/*                                                                    */
/* LispMe System (c) FBI Fred Bayer Informatics                       */
/*                                                                    */
/* Distributed under the GNU General Public License;                  */
/* see the README file. This code comes with NO WARRANTY.             */
/*                                                                    */
/**********************************************************************/
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>

#include "io.h"
#include "vm.h"
#include "error.h"

#define MAX_TOKEN_LEN 1024

/**********************************************************************/
/* Token types                                                        */
/**********************************************************************/
#define TT_NAME     1
#define TT_INT      2
#define TT_FLOAT    3
#define TT_PUNCT    4
#define TT_CHAR     5
#define TT_STRING   6
#define TT_TRUE     7
#define TT_FALSE    8
#define TT_NOPRINT  9
#define TT_CMD     10

#define EXTENDED_ALFA  "+-*/<=>!?:$%_&~^"

/**********************************************************************/
/* Character classes for scanner                                      */
/**********************************************************************/
#define ccWS   0    /* white space: ' ', \t                           */
#define ccNL   1    /* newline \n                                     */
#define ccLXA  2    /* letters but 'e', 'E', 'i', 'I' and ext. chars  */
#define ccE    3    /* 'e', 'E'                                       */
#define ccD    4    /* decimal digit                                  */
#define ccPM   5    /* +,-                                            */
#define ccAT   6    /* @                                              */
#define ccBSL  7    /* backslash \                                    */
#define ccDOT  8    /* dot .                                          */
#define ccPUN  9    /* punctuation ( ) ' ` ,                          */
#define ccSEM  0xa  /* semicolon ;                                    */
#define ccILL  0xb  /* illegal char                                   */
#define ccEOF  0xc  /* end of file \0                                 */
#define ccHSH  0xd  /* hash sign #                                    */
#define ccDQ   0xe  /* double quote "                                 */
#define ccI    0xf  /* 'i', 'I'                                       */

/**********************************************************************/
/* Automaton states                                                   */
/**********************************************************************/
#define asSTART 0
#define asID    1
#define asCOMM  2
#define asN1    3
#define asN1D   4
#define asN2    5
#define asN3    6
#define asN4    7
#define asN5    8
#define asN6    9
#define asN7   10
#define asN8   11
#define asN9   12
#define asN10  13
#define asN11  14
#define asDOT  15
#define asHASH 16
#define asCHAR 17
#define asSTRG 18
#define asSTRE 19
#define asSTH1 20
#define asSTH2 21
#define asCHH1 22
#define asCHH2 23

#define rsREAL  0
#define rsIMAG  1
#define rsPOL   2

/**********************************************************************/
/* Add current char to token                                          */
/**********************************************************************/
#define PUSH_CHAR(c){                                   \
        if (p-token >= MAX_TOKEN_LEN)                   \
            error(ERR_S4_TOKEN_LEN, NIL, lineNr);       \
        *p++ = c;}

/**********************************************************************/
/* ASCII/EBCDIC chars classified                                      */
/**********************************************************************/
static unsigned char charClass[] = {
    /*        01    23    45    67    89    ab    cd    ef   */
    /* 0 */  0xcb, 0xbb, 0xbb, 0xbb, 0xb0, 0x1b, 0xb0, 0xbb,
    /* 1 */  0xbb, 0xbb, 0xbb, 0xbb, 0xbb, 0xbb, 0xbb, 0xbb,
    /* 2 */  0x02, 0xed, 0x22, 0x29, 0x99, 0x25, 0x95, 0x82,
    /* 3 */  0x44, 0x44, 0x44, 0x44, 0x44, 0x2a, 0x22, 0x22,
    /* 4 */  0x62, 0x22, 0x23, 0x22, 0x2f, 0x22, 0x22, 0x22,
    /* 5 */  0x22, 0x22, 0x22, 0x22, 0x22, 0x2b, 0x7b, 0x22,
    /* 6 */  0x92, 0x22, 0x23, 0x22, 0x2f, 0x22, 0x22, 0x22,
    /* 7 */  0x22, 0x22, 0x22, 0x22, 0x22, 0x2b, 0xbb, 0x2b,
    /* 8 */  0x0b, 0xbb, 0xbb, 0xbb, 0xbb, 0x2b, 0x2b, 0xbb,
    /* 9 */  0xbb, 0xbb, 0xbb, 0xbb, 0xbb, 0x2b, 0x2b, 0xb2,
    /* a */  0x0b, 0xbb, 0xbb, 0xbb, 0xbb, 0xbb, 0xbb, 0xbb,
    /* b */  0xbb, 0xbb, 0xbb, 0xbb, 0xbb, 0xbb, 0xbb, 0xbb,
    /* c */  0x22, 0x22, 0x22, 0x22, 0x22, 0x22, 0x22, 0x22,
    /* d */  0x22, 0x22, 0x22, 0x2b, 0x22, 0x22, 0x22, 0x22,
    /* e */  0x22, 0x22, 0x22, 0x22, 0x22, 0x22, 0x22, 0x22,
    /* f */  0x22, 0x22, 0x22, 0x2b, 0x22, 0x22, 0x22, 0x22
};
/**********************************************************************/
/* FP conversion constants                                            */
/**********************************************************************/
static double pow1[] =
{
    1e256, 1e128, 1e064,
    1e032, 1e016, 1e008,
    1e004, 1e002, 1e001
};

static double pow3[] =
{
    1e-256, 1e-128, 1e-064,
    1e-032, 1e-016, 1e-008,
    1e-004, 1e-002, 1e-001
};
/**********************************************************************/
/* global data                                                        */
/**********************************************************************/
char   errorChar;
short  lineNr;
char*  currPtr;
int    errorPos;
bool   onlyWS;

/**********************************************************************/
/* module data                                                        */
/**********************************************************************/
static unsigned char token[MAX_TOKEN_LEN +1];
static short         depth;
static char          prtFlags;

static short         outLen;
static char          tt;
static long          longVal;
static int           negative;
static int           nextNegative;
static long          stLen;

static double        doubleVal;
static double        doubleRe;
static int           scale;
static int           exponent;
static int           expNegative;
static int           realState;
static double        fcdRe, fcdIm;
static FILE*         outFP;
static FILE*         inFP;

/**********************************************************************/
/* output a token                                                     */
/**********************************************************************/
static void outStr(char* p)
{
    if (outFP)
        fputs(p, outFP);
    else
        while (*p)
        {
            ++outLen;
            *currPtr++ = *p++;
        }
}
/**********************************************************************/
/* internal helper function for printing                              */
/**********************************************************************/
static bool isInf(double d)
{
    return d != 0.0 && 2*d == d;
}

static bool isNan(double d)
{
    return false;
}

#define HEXDIGIT(c) ((c) < 10 ? (c)+'0' : (c)+('a'-10))

static void writeVector(IPTR p);
static void writeString(IPTR p);

static void writeSEXP(IPTR p)
{
    double r;
    unsigned char c;

    if (broken)
        error(ERR_O2_INTERRUPT, NIL);

    if (p == TRUE)
        outStr("#t");

    else if (p == FALSE)
        outStr("#f");

    else if (p == BLACK_HOLE)
        outStr("[hole]");

    else if (p == NOPRINT)
        outStr("#n");

    else if (p == NIL)
        outStr("()");

    else if (p == END_OF_FILE)
        outStr("[eof]");

    else if (p == MLAMBDA)
        outStr("[mlambda]");

    else if (IS_CHAR(p))
    {
        strcpy((char*) token, "#\\?");
        token[2] = CHARVAL(p);

        if ((prtFlags & PRT_ESCAPE) && CHARVAL(p) < ' ')
        {
            c = CHARVAL(p) >> 4;
            token[1] = '#';
            token[2] = HEXDIGIT(c);

            c = CHARVAL(p) & 0x0f;
            token[3] = HEXDIGIT(c);
            token[4] = '\0';
        }
        outStr(prtFlags & PRT_ESCAPE ? (char*) token : (char*)(token +2));
    }
    else if (IS_INT(p))
    {
        sprintf((char*)token, "%ld", (long int) INTVAL(p));
        outStr((char*)token);
    }
    else if (IS_REAL(p))
    {
        r = getReal(p);
        if (isInf(r))
            outStr(r > 0 ? "[inf]" : "[-inf]");

        else if (isNan(r))
            outStr("[nan]");

        else
        {
            sprintf((char*)token, "%.16g", r);
            outStr((char*)token);
        }
    }
    else if (IS_ATOM(p))
        outStr(getAtom(p));

    else if (IS_VEC(p))
    {
        if (depth >= LispMePrefs.printDepth)
        {
            /*--------------------------------------------------------------*/
            /* don't let recursion go to deep                               */
            /*--------------------------------------------------------------*/
            outStr("[deep]");
            return;
        }
        ++depth;
        outStr("#(");
        writeVector(p);
        outStr(")");
        --depth;
    }
    else if (IS_STRING(p))
        writeString(p);

    else if (IS_CONS(p))
    {
        /*----------------------------------------------------------------*/
        /* Special cases                                                  */
        /*----------------------------------------------------------------*/
        switch (car(p))
        {
        case CLOS_TAG:
            outStr("[clos ");
            writeSEXP(cadr(p));
            outStr("]");
            break;

        case CONT_TAG:
            outStr("[cont]");
            break;

        case RCPD_TAG:
        case RCPF_TAG:
            outStr("[prom]");
            break;

        case IPRT_TAG:
            outStr("[inport]");
            break;

        case OPRT_TAG:
            outStr("[outport]");
            break;

        case CPRT_TAG:
            outStr("[clport]");
            break;

        case MACR_TAG:
            outStr("[macro]");
            break;

        case CPLX_TAG:
            /*------------------------------------------------------------*/
            /* Output complex number: <real>(+|-)<imag>i                  */
            /*------------------------------------------------------------*/
            fcdRe = getReal(cadr(p));
            fcdIm = getReal(cddr(p));

            if (isNan(fcdRe) || isNan(fcdIm))
                outStr("[nan]");

            else if (isInf(fcdRe) || isInf(fcdIm))
                outStr("[cinf]");

            else
            {
                if (fcdRe != 0.0)
                {
                    sprintf((char*)token, "%.16g", fcdRe);
                    outStr((char*)token);
                }
                outStr(fcdIm < 0.0 ? "-" : "+");

                fcdIm = fabs(fcdIm);
                if (fcdIm != 1.0)
                {
                    sprintf((char*)token, "%.16g", fcdIm);
                    outStr((char*)token);
                }
                outStr("i");
            }
            break;

        default:
            if (LispMePrefs.printQuotes && IS_PAIR(cdr(p)) && cddr(p) == NIL)
            {
                if (car(p) == QUOTE) {
                    outStr("'");
                    writeSEXP(cadr(p));
                    return;
                }
                if (car(p) == QUASIQUOTE) {
                    outStr("`");
                    writeSEXP(cadr(p));
                    return;
                }
                if (car(p) == UNQUOTE) {
                    outStr(",");
                    writeSEXP(cadr(p));
                    return;
                }
                if (car(p) == UNQUOTESPLICING) {
                    outStr(",@");
                    writeSEXP(cadr(p));
                    return;
                }
            }
            if (depth >= LispMePrefs.printDepth)
            {
                /*----------------------------------------------------------*/
                /* don't let recursion go to deep                           */
                /*----------------------------------------------------------*/
                outStr("[deep]");
                return;
            }
            ++depth;
            outStr("(");
            while (IS_PAIR(p))
            {
                writeSEXP(car(p));

                /*----------------------------------------------------------*/
                /* last list element?                                       */
                /*----------------------------------------------------------*/
                if ((p = cdr(p)) == NIL)
                    break;

                outStr(" ");

                /*----------------------------------------------------------*/
                /* dotted pair at end of list                               */
                /*----------------------------------------------------------*/
                if (!IS_PAIR(p))
                {
                    outStr(". ");
                    writeSEXP(p);
                }
            }
            outStr(")");
            --depth;
        }
    }
    else
        error(ERR_M2_INVALID_PTR, NIL, p);
}
/**********************************************************************/
/* helper function to print a vector                                  */
/**********************************************************************/
static void writeVector(IPTR p)
{
    IPTR* pel;
    long num;

    pel = VECTORMEM(p);
    num = LENGTH(p);
    while (num--)
    {
        writeSEXP(*pel++);
        if (num)
            outStr(" ");
    }
}
/**********************************************************************/
/* helper function to print a string                                  */
/**********************************************************************/
static void writeString(IPTR p)
{
    char *pc  = STRINGMEM(p);
    long  num = LENGTH(p);

    strcpy((char*) token,     "?");
    strcpy((char*)(token +2), "#xx");

    if (prtFlags & PRT_ESCAPE)
        outStr("\"");

    while (num--)
    {
        token[0] = *pc++;
        if (prtFlags & PRT_ESCAPE)
        {
            if (token[0] < ' ')
            {
                unsigned char c = token[0] >> 4;
                token[3] = HEXDIGIT(c);

                c = token[0] & 0x0f;
                token[4] = HEXDIGIT(c);

                outStr((char*)(token +2));
            }
            else if (token[0] == '\\' || token[0] == '"' || token[0] == '#')
            {
                outStr("\\");
                outStr((char*)token);
            }
            else
                outStr((char*)token);
        }
        else
        {
            if (token[0])
                outStr((char*)token);
            else
            {
                /*------------------------------------------------------------*/
                /* '\0' byte, write ' ' instead and clear afterwards to       */
                /* advance output pointer                                     */
                /*------------------------------------------------------------*/
                outStr(" ");
                currPtr[-1] = '\x00';
            }
        }
    }
    if (prtFlags & PRT_ESCAPE)
        outStr("\"");
}
/**********************************************************************/
/* print an SEXPR using flags for destination and options             */
/**********************************************************************/
void printSEXP(char* buf, int size, IPTR p, char flags)
{
    if (p == NOPRINT && (p & PRT_NO_HASHN))
    {
        *buf = '\0';
        return;
    }
    depth    = 0;
    prtFlags = flags;
    outLen   = 0;
    outFP    = NULL;
    currPtr  = buf;

    writeSEXP(p);
    if (flags & PRT_SPACE)
        outStr(" ");

    *currPtr = '\0';
}
/**********************************************************************/
/* print an SEXPR using flags on output file                          */
/**********************************************************************/
void printSEXPfp(FILE* fp, IPTR p, char flags)
{
    if (p==NOPRINT && (p & PRT_NO_HASHN))
        return;

    outFP    = fp;
    depth    = 0;
    prtFlags = flags;

    writeSEXP(p);
    if (flags & PRT_SPACE)
        outStr(" ");

    fflush(fp);
}
/**********************************************************************/
/* Build real number from components                                  */
/**********************************************************************/
static void buildReal(void)
{
    double* pd;
    short   e1;

    if (expNegative)
        exponent = -exponent;

    exponent += scale;

    if (exponent > 308)
        doubleVal = HUGE_VAL;

    else if (exponent < -324)
        doubleVal = 0.0;

    else
        for (e1 = 256, pd = (exponent>=0 ? pow1 : pow3),
                 exponent = abs(exponent); e1; e1 >>= 1, ++pd)
            if (exponent >= e1)
            {
                exponent  -= e1;
                doubleVal *= *pd;
            }
    if (negative)
        doubleVal = -doubleVal;
}
/**********************************************************************/
/* interpret hex digit                                                */
/**********************************************************************/
static short hexVal(char c)
{
    if ('0' <= c && c<='9')
        return c - '0';

    if ('A' <= c && c <= 'F')
        return c - ('A' -10);

    if ('a'<= c && c<='f')
        return c - ('a' - 10);

    error(ERR_S8_INVALID_HASH, NIL, lineNr, c);
    return 0;
}
/**********************************************************************/
/* get a token                                                        */
/**********************************************************************/
#define GET do {                                \
        if (inFP) {                             \
            if ((c = getc(inFP)) == EOF)        \
                c = '\0';                       \
        } else                                  \
            c = *currPtr++;                     \
    } while(0)

#define UNGET do {                              \
        if (inFP)                               \
            ungetc(c, inFP);                    \
        else                                    \
            --currPtr;                          \
    } while(0)

static void scan(void)
{
    unsigned char *p = token;
    int c, state = asSTART;
    short val;
    char   cc;

    negative    = 0;
    longVal     = 0;
    realState   = rsREAL;
    doubleVal   = 0.0;
    expNegative = 0;
    exponent    = 0;
    scale       = 0;

    while (true)
    {
        GET;

        /*----------------------------------------------------------------*/
        /* Classify input char                                            */
        /*----------------------------------------------------------------*/
        cc = (c & 1) ?
            charClass[((unsigned char)c) >> 1] & 0x0f
            :
            charClass[((unsigned char)c) >> 1] >> 4;

        /*----------------------------------------------------------------*/
        /* Never read beyond EOF! Pushing back other char classes is done */
        /* in the appropriate automaton states                            */
        /*----------------------------------------------------------------*/
        if (cc == ccEOF)
            UNGET;

        else if (cc == ccNL)
            ++lineNr;

        switch (state)
        {
        case asSTART: /* non-accepting */
            /*------------------------------------------------------------*/
            /* Initial state, all is possible...                          */
            /*------------------------------------------------------------*/
            switch (cc)
            {
            case ccWS:
            case ccNL:
                break;

            case ccLXA:
            case ccE:
            case ccI:
                onlyWS = false;
                tt     = TT_NAME;

                PUSH_CHAR(c);
                state = asID;
                break;

            case ccSEM:
                state = asCOMM;
                break;

            case ccPUN:
                onlyWS = false;
                tt     = TT_PUNCT;

                if (c == ',')
                {
                    GET;
                    if (c != '@')
                    {
                        UNGET;
                        c = ',';
                    }
                }
                *p = c;
                return;

            case ccDOT:
                onlyWS = false;
                state  = asDOT;
                *p     = c;
                break;

            case ccD:
                onlyWS  = false;
                tt      = TT_INT;
                longVal = c - '0';
                state   = asN1;
                break;

            case ccPM:
                onlyWS   = false;
                negative = c == '-';

                PUSH_CHAR(c);
                state = asN2;
                break;

            case ccDQ:
                onlyWS = false;
                tt     = TT_STRING;
                state  = asSTRG;
                break;

            case ccEOF:
                /*--------------------------------------------------------*/
                /* Close potential open lists                             */
                /*--------------------------------------------------------*/
                tt = TT_PUNCT;
                *p = ')';
                return;

            case ccHSH:
                onlyWS = false;
                state  = asHASH;
                break;

            default:
                onlyWS = false;
                error(ERR_S1_INVALID_CHAR,NIL,lineNr,c);
            }
            break;

        case asID: /* accepting */
            /*------------------------------------------------------------*/
            /* Simple identifier, just append valid chars                 */
            /*------------------------------------------------------------*/
            switch (cc)
            {
            case ccLXA:
            case ccE:
            case ccI:
            case ccD:
            case ccPM:
                PUSH_CHAR(c);
                break;

            default:
                /*--------------------------------------------------------*/
                /* Push back read-ahead and fall thru                     */
                /*--------------------------------------------------------*/
                UNGET;

            case ccEOF:
                *p = '\0';
                return;
            }
            break;

        case asCOMM: /* non-accepting, but ignore */
            /*------------------------------------------------------------*/
            /* Comment: do nothing but on \n or EOF                       */
            /*------------------------------------------------------------*/
            switch (cc)
            {
            case ccNL:
            case ccEOF:
                state = asSTART;
                break;
            }
            break;

        case asN1: /* accepting, if not IMAG */
            /*------------------------------------------------------------*/
            /* Digit string representable as smallint                     */
            /*------------------------------------------------------------*/
            switch (cc)
            {
            case ccD:
                if (longVal > (MAXINT - (c - '0') + negative) / 10)
                {
                    /*------------------------------------------------------*/
                    /* Too large! Convert to double                         */
                    /*------------------------------------------------------*/
                    doubleVal = 10.0 * longVal + c - '0';
                    tt        = TT_FLOAT;
                    state     = asN1D;
                }
                else
                    longVal = 10 * longVal + c - '0';
                break;

            case ccE:
                tt        = TT_FLOAT;
                doubleVal = longVal;
                state     = asN4;
                break;

            case ccI:
            case ccPM:
            case ccAT:
                /*--------------------------------------------------------*/
                /* epsilon trans to asN1D                                 */
                /*--------------------------------------------------------*/
                tt        = TT_FLOAT;
                doubleVal = longVal;
                state     = asN1D;
                UNGET;
                break;

            case ccDOT:
                tt        = TT_FLOAT;
                doubleVal = longVal;
                state     = asN3;
                break;

            default:
                UNGET;

            case ccEOF:
                if (realState == rsIMAG)
                    error(ERR_S10_INVALID_COMP, NIL, lineNr);

                return;
            }
            break;

        case asN1D:
            /*------------------------------------------------------------*/
            /* Digit string to large for smallint                         */
            /*------------------------------------------------------------*/
            switch (cc) /* accepting */
            {
            case ccD:
                doubleVal *= 10.0;
                doubleVal += c - '0';
                break;

            case ccE:
                state = asN4;
                break;

            case ccPM:
                nextNegative = c == '-';
                state = asN8;
                break;

            case ccI:
                state = asN9;
                break;

            case ccAT:
                state = asN10;
                break;

            case ccDOT:
                state = asN3;
                break;

            default:
                UNGET;

            case ccEOF:
                if (realState == rsIMAG)
                    error(ERR_S10_INVALID_COMP,NIL,lineNr);

                buildReal();
                return;
            }
            break;

        case asN2: /* accepting */
            /*------------------------------------------------------------*/
            /* Seen + or - so far, could be number or symbol              */
            /*------------------------------------------------------------*/
            switch (cc) /* accepting */
            {
            case ccD:
                /*--------------------------------------------------------*/
                /* OK, it's a number                                      */
                /* fake epsilon trans. to avoid duplicate code            */
                /*--------------------------------------------------------*/
                tt    = TT_INT;
                state = asN1;
                UNGET;
                break;

            case ccDOT:
                /*--------------------------------------------------------*/
                /* This must be a real number                             */
                /*--------------------------------------------------------*/
                tt    = TT_FLOAT;
                state = asN5;
                break;

            case ccI:
                /*--------------------------------------------------------*/
                /* +i or -i                                               */
                /*--------------------------------------------------------*/
                doubleRe = 0.0;

            makeImagUnit:
                doubleVal   = 1.0;
                expNegative = 0;
                exponent    = 0;
                scale       = 0;

                buildReal();
                tt          = TT_FLOAT;
                realState   = rsIMAG;
                return;

            case ccE:
            case ccLXA:
            case ccPM:
                /*--------------------------------------------------------*/
                /* It's a symbol                                          */
                /*--------------------------------------------------------*/
                tt = TT_NAME;
                PUSH_CHAR(c);
                state = asID;
                break;

            case ccAT:
                error(ERR_S10_INVALID_COMP,NIL,lineNr);

            default:
                /*--------------------------------------------------------*/
                /* Single + or - , also a symbol                          */
                /*--------------------------------------------------------*/
                UNGET;

            case ccEOF:
                tt = TT_NAME;
                *p = '\0';
                return;
            }
            break;

        case asN3: /* accepting, if not IMAG */
            /*------------------------------------------------------------*/
            /* Digit string after decimal point                           */
            /*------------------------------------------------------------*/
            switch (cc)
            {
            case ccD:
                --scale;
                doubleVal *= 10.0;
                doubleVal += c-'0';
                break;

            case ccE:
                state = asN4;
                break;

            case ccPM:
                nextNegative = c == '-';
                state = asN8;
                break;

            case ccI:
                state = asN9;
                break;

            case ccAT:
                state = asN10;
                break;

            default:
                UNGET;

            case ccEOF:
                if (realState == rsIMAG)
                    error(ERR_S10_INVALID_COMP, NIL, lineNr);

                buildReal();
                return;
            }
            break;

        case asN4: /* non-accepting */
            /*------------------------------------------------------------*/
            /* Exponent part of real number, no sign or digit seen yet    */
            /*------------------------------------------------------------*/
            switch (cc)
            {
            case ccPM:
                expNegative = c == '-';
                state = asN6;
                break;

            case ccD:
                /*--------------------------------------------------------*/
                /* Epsilon trans to asN7                                  */
                /*--------------------------------------------------------*/
                UNGET;
                state = asN7;
                break;

            default:
                error(ERR_S6_INVALID_REAL,NIL,lineNr);
            }
            break;

        case asN5: /* non-accepting */
            /*------------------------------------------------------------*/
            /* Seen sign and decimal point, must get a digit              */
            /*------------------------------------------------------------*/
            switch (cc)
            {
            case ccD:
                /*--------------------------------------------------------*/
                /* Epsilon trans to asN3                                  */
                /*--------------------------------------------------------*/
                UNGET;
                state = asN3;
                break;

            default:
                error(ERR_S6_INVALID_REAL,NIL,lineNr);
            }
            break;

        case asN6: /* non-accepting */
            /*------------------------------------------------------------*/
            /* Exponent part of real number incl. sign, but no digits yet */
            /*------------------------------------------------------------*/
            switch (cc)
            {
            case ccD:
                /*--------------------------------------------------------*/
                /* Epsilon trans to asN7                                  */
                /*--------------------------------------------------------*/
                UNGET;
                state = asN7;
                break;

            default:
                error(ERR_S6_INVALID_REAL,NIL,lineNr);
            }
            break;

        case asN7: /* accepting, if not IMAG */
            /*------------------------------------------------------------*/
            /* Digit string of exponent part of real number               */
            /*------------------------------------------------------------*/
            switch (cc)
            {
            case ccD:
                exponent = 10*exponent + c - '0';
                break;

            case ccI:
                state = asN9;
                break;

            case ccPM:
                nextNegative = c == '-';
                state = asN8;
                break;

            case ccAT:
                state = asN10;
                break;

            default:
                UNGET;

            case ccEOF:
                if (realState == rsIMAG)
                    error(ERR_S10_INVALID_COMP,NIL,lineNr);

                buildReal();
                return;
            }
            break;

        case asN8: /* non-accepting */
            /*------------------------------------------------------------*/
            /* got sign after a real => begin of complex numer            */
            /* store real part and reinit floating point scanner          */
            /*------------------------------------------------------------*/
            if (realState != rsREAL)
                error(ERR_S10_INVALID_COMP,NIL,lineNr);

            realState   = rsIMAG;
            buildReal();

            doubleRe    = doubleVal;
            doubleVal   = 0.0;
            longVal     = 0;
            expNegative = 0;
            exponent    = 0;
            scale       = 0;
            negative    = nextNegative;

            switch (cc)
            {
            case ccD:
                UNGET;
                state = asN1;
                break;

            case ccDOT:
                state = asDOT;
                break;

            case ccI:
                goto makeImagUnit;

            default:
                error(ERR_S10_INVALID_COMP,NIL,lineNr);
            }
            break;

        case asN9: /* accepting */
            /*------------------------------------------------------------*/
            /* i after a real number, now check real scanner automaton    */
            /*------------------------------------------------------------*/
            if (cc != ccEOF)
                UNGET;

            switch (realState)
            {
            case rsREAL:
                /*--------------------------------------------------------*/
                /* Imaginary with no real part                            */
                /*--------------------------------------------------------*/
                buildReal();
                doubleRe  = 0.0;
                realState = rsIMAG;
                return;

            case rsIMAG:
                /*--------------------------------------------------------*/
                /* Complex number complete!                               */
                /*--------------------------------------------------------*/
                buildReal();
                realState = rsIMAG;
                return;

            case rsPOL:
                error(ERR_S10_INVALID_COMP,NIL,lineNr);
            }
            break;

        case asN10: /* non-accepting */
            /*------------------------------------------------------------*/
            /* @ after a real number, now check real scanner automaton    */
            /*------------------------------------------------------------*/
            switch (realState)
            {
            case rsREAL:
                /*--------------------------------------------------------*/
                /* Start scanning polar representation                    */
                /*--------------------------------------------------------*/
                buildReal();
                doubleRe    = doubleVal;
                realState   = rsPOL;
                doubleVal   = 0.0;
                longVal     = 0;
                expNegative = 0;
                negative    = 0;
                exponent    = 0;
                scale       = 0;

                switch (cc)
                {
                case ccD:
                    doubleVal = c-'0';
                    state = asN1D;
                    break;

                case ccDOT:
                    state = asDOT;
                    break;

                case ccPM:
                    negative = c == '-';
                    state = asN11;
                    break;

                default:
                    error(ERR_S10_INVALID_COMP,NIL,lineNr);
                }
                break;

            case rsIMAG:
            case rsPOL:
                error(ERR_S10_INVALID_COMP,NIL,lineNr);
            }
            break;

        case asN11: /* non-accepting */
            /*------------------------------------------------------------*/
            /* Seen <real>@<sign>, must get unsigned number               */
            /*------------------------------------------------------------*/
            switch (cc)
            {
            case ccD:
                doubleVal = c-'0';
                state = asN1D;
                break;

            case ccDOT:
                state = asN5;
                break;

            default:
                error(ERR_S10_INVALID_COMP,NIL,lineNr);
            }
            break;

        case asDOT: /* accepting, if not IMAG */
            /*------------------------------------------------------------*/
            /* Could be punctuation or start of number                    */
            /*------------------------------------------------------------*/
            switch (cc)
            {
            case ccD:
                tt = TT_FLOAT;
                UNGET;

                state = asN3;
                break;

            default:
                UNGET;

            case ccEOF:
                if (realState != rsREAL)
                    error(ERR_S10_INVALID_COMP,NIL,lineNr);

                tt = TT_PUNCT;
                return;
            }
            break;

        case asHASH: /* non-accepting */
            /*------------------------------------------------------------*/
            /* #t, #f, #n, #(, #\char or ##xx                             */
            /*------------------------------------------------------------*/
            switch (c)
            {
            case 't': case 'T':
                tt = TT_TRUE;
                return;

            case 'f': case 'F':
                tt = TT_FALSE;
                return;

            case 'n': case 'N':
                tt = TT_NOPRINT;
                return;

            case '(':
                tt = TT_PUNCT;
                *p = '['; /* indicates start of vector */
                return;

            case '\\':
                tt = TT_CHAR;
                state = asCHAR;
                break;

            case '#':
                tt = TT_CHAR;
                state = asCHH1;
                break;

            default:
                error(ERR_S8_INVALID_HASH,NIL,lineNr,c);
            }
            break;

        case asCHAR: /* accepting */
            /*------------------------------------------------------------*/
            /* accept any but EOF                                         */
            /*------------------------------------------------------------*/
            switch (cc)
            {
            case ccEOF:
                error(ERR_S8_INVALID_HASH,NIL,lineNr,c);

            default:
                PUSH_CHAR(c);
                return;
            }
            break;

        case asSTRG: /* non-accepting */
            /*------------------------------------------------------------*/
            /* Normal characters in string, accept all but \ " # EOF      */
            /*------------------------------------------------------------*/
            switch (cc)
            {
            case ccBSL:
                state = asSTRE;
                break;

            case ccHSH:
                state = asSTH1;
                break;

            case ccDQ:
                stLen = p-token;
                return;

            case ccEOF:
                error(ERR_S9_UNTERM_STRING, NIL, lineNr);

            default:
                PUSH_CHAR(c);
                break;
            }
            break;

        case asSTRE: /* non-accepting */
            /*------------------------------------------------------------*/
            /* Escape in string                                           */
            /*------------------------------------------------------------*/
            switch (cc)
            {
            case ccEOF:
                error(ERR_S9_UNTERM_STRING, NIL, lineNr);

            default:
                PUSH_CHAR(c);
                state = asSTRG;
                break;
            }
            break;

        case asSTH1: /* non-accepting */
            /*------------------------------------------------------------*/
            /* # in string begins hex sequence                            */
            /*------------------------------------------------------------*/
            switch (cc)
            {
            case ccEOF:
                error(ERR_S8_INVALID_HASH,NIL,lineNr,c);

            default:
                val = hexVal(c) << 4;
                PUSH_CHAR(val);
                state = asSTH2;
                break;
            }
            break;

        case asSTH2: /* non-accepting */
            /*------------------------------------------------------------*/
            /* second char of hex sequence                                */
            /*------------------------------------------------------------*/
            switch (cc)
            {
            case ccEOF:
                error(ERR_S9_UNTERM_STRING,NIL,lineNr);

            default:
                p[-1] |= hexVal(c);
                state = asSTRG;
                break;
            }
            break;

        case asCHH1: /* non-accepting */
            /*------------------------------------------------------------*/
            /* ## starts hex sequence for single characters               */
            /*------------------------------------------------------------*/
            switch (cc)
            {
            case ccEOF:
                error(ERR_S8_INVALID_HASH,NIL,lineNr,c);

            default:
                val = hexVal(c) << 4;
                PUSH_CHAR(val);
                state = asCHH2;
                break;
            }
            break;

        case asCHH2: /* accepting */
            /*------------------------------------------------------------*/
            /* second char of hex sequence in character                   */
            /*------------------------------------------------------------*/
            switch (cc)
            {
            case ccEOF:
                error(ERR_S8_INVALID_HASH,NIL,lineNr,c);

            default:
                p[-1] |= hexVal(c);
                return;
            }
            break;
        } /* switch state */
    } /* char loop */
}
/**********************************************************************/
/* internal helper functions                                          */
/**********************************************************************/
static void getSEXPList(IPTR *e);

static IPTR getSEXP (void)
{
    static const char *const macroChars = "'`,@";
    static char *ptrPos;

    IPTR  res;
    bool isVec;
    int  macroNr;

    CHECKSTACK(macroNr);

    if (tt == TT_TRUE)
        return TRUE;

    if (tt == TT_FALSE)
        return FALSE;

    if (tt == TT_NOPRINT)
        return NOPRINT;

    if (tt == TT_CHAR)
        return MKCHAR(token[0]);

    if (tt == TT_STRING)
        return makeString(stLen, (char*)token, 0, 0, NIL);

    if (tt == TT_INT)
        return MKINT(negative ? -longVal : longVal);

    if (tt == TT_FLOAT)
    {
        switch (realState)
        {
        case rsREAL:
            return allocReal(doubleVal);

        case rsIMAG:
            return storeNum(doubleRe, doubleVal);

        case rsPOL:
            return makePolar(doubleRe, doubleVal);
        }
    }
    if (tt == TT_NAME)
    {
        strlwr((char*)token);
        return findAtom((char*)token);
    }
    if (*token == '(' || *token == '[')
    {
        /*----------------------------------------------------------------*/
        /* Start of pair, try reading list or vector                      */
        /*----------------------------------------------------------------*/
        isVec = *token == '[';
        res   = NIL;

        PROTECT(res);
        getSEXPList(&res);
        UNPROTECT(res);

        return isVec ? makeVector(listLength(res), res, true) : res;
    }
    if (tt == TT_PUNCT && (ptrPos = strchr(macroChars,*token)))
    {
        /*----------------------------------------------------------------*/
        /* quote, quasiquote, unquote or unquote-splicing,                */
        /* build 2-list (macro-expansion expr)                            */
        /*----------------------------------------------------------------*/
        macroNr = ptrPos - macroChars;
        scan();

        return cons(keyWords[macroNr], cons(getSEXP(), NIL));
    }
    error(ERR_S2_INVALID_SEXP, NIL, lineNr);
    return NIL;
}

static void getSEXPList(IPTR *e)
{
    scan();

    if (*token == ')' && tt == TT_PUNCT)
        return;

loop:
    *e = cons(getSEXP(), NIL);
    scan();

    if (*token == '.' && tt == TT_PUNCT)
    {
        /*--------------------------------------------------------------*/
        /* Dotted pair, read cdr as single SEXPR, assure a closing      */
        /* parenthesis follows                                          */
        /*--------------------------------------------------------------*/
        scan();
        cdr(*e) = getSEXP();
        scan();

        if (*token != ')' || tt != TT_PUNCT)
            error(ERR_S3_MULTI_DOT,NIL,lineNr);

        return;
    }
    if (*token == ')' && tt == TT_PUNCT)
        return;

    /*--------------------------------------------------------------*/
    /* List notation                                                */
    /*--------------------------------------------------------------*/
    e = &cdr(*e);
    goto loop;
}
/**********************************************************************/
/* read an SEXPR                                                      */
/**********************************************************************/
IPTR readSEXP(char *src)
{
    inFP    = NULL;
    lineNr  = 1;
    onlyWS  = true;
    currPtr = src;

    scan();

    W = getSEXP();
    return W;
}

IPTR readSEXPfp(FILE *fp)
{
    inFP   = fp;
    lineNr = 1;
    onlyWS = true;

    scan();

    W = getSEXP();
    return W;
}
/**********************************************************************/
/* load a file                                                        */
/**********************************************************************/
IPTR loadFile(FILE *fp)
{
    inFP    = fp;
    lineNr  = 1;

    IPTR res = cons(BEGIN, NIL);
    PROTECT(res);

    getSEXPList(&cdr(res));

    UNPROTECT(res);
    return res;
}
/**********************************************************************/
/* Read from a file                                                   */
/**********************************************************************/
IPTR readFile(IPTR port, int opcode)
{
    static char buf[4096];

    FILE* fp = stdin;
    char* cp;
    int   c;

    if (port != MKINT(0))
    {
        if (!IS_IPORT(port))
            typeError(port, "input port");

        fp = (FILE*)(cdr(port));
    }

    switch (opcode)
    {
    case PEEK:
        if ((c = fgetc(fp)) != EOF)
        {
            ungetc(c,fp);
            return MKCHAR(c);
        }
        return END_OF_FILE;

    case REAC:
        if ((c = fgetc(fp)) != EOF)
            return MKCHAR(c);

        return END_OF_FILE;

    case REDL:
        if (fgets(buf, sizeof(buf), fp))
        {
            if ((cp = strchr(buf,'\n')))
                *cp = '\0';

            return str2Lisp(buf);
        }
        else if (feof(fp))
            return END_OF_FILE;

    case READ:
        return readSEXPfp(fp);
        break;
    }
    return NIL;
}
/**********************************************************************/
/* Delete an existing file                                            */
/**********************************************************************/
void deleteFile(IPTR fileName)
{
    static char buf[80];

    if (!IS_STRING(fileName))
        typeError(fileName,"string");

    memcpy(buf,STRINGMEM(fileName),LENGTH(fileName));
    buf[LENGTH(fileName)] = '\0';

    if (remove(buf))
        error(ERR_R17_OPEN_FILE, fileName);
}
