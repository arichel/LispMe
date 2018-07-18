/**********************************************************************/
/*                                                                    */
/* store.c: Storage management functions                              */
/*                                                                    */
/* LispMe System (c) FBI Fred Bayer Informatics                       */
/*                                                                    */
/* Distributed under the GNU General Public License;                  */
/* see the README file. This code comes with NO WARRANTY.             */
/*                                                                    */
/**********************************************************************/
#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>

#include "store.h"
#include "error.h"
#include "vm.h"

#ifdef DUMP_STEPS
#  include "io.h"
   extern FILE* logFile;
#endif

/**********************************************************************/
/* module data                                                        */
/**********************************************************************/
static char*          strStore;
static char*          reals;
static IPTR           firstFree;
static IPTR           firstFreeReal;
static unsigned char* markBit;
static unsigned char* markRealBit;
static int            symCnt;

/**********************************************************************/
/* global data                                                        */
/**********************************************************************/
long  heapSize = DEFAULT_HEAP_SIZE;
long  atomSize = DEFAULT_STRING_STORE_SIZE;
long  realSize = DEFAULT_REAL_STORE_SIZE;

IPTR  keyWords[LAST_KEYWORD];
IPTR* protPtr[MAX_PROTECT];
int   protIdx;
char* heap;

struct LispMePrefs LispMePrefs = {
    12, true
};

IPTR S, E, C, D, W;
IPTR tlNames;
IPTR tlVals;
IPTR newTopLevelNames;
IPTR newTopLevelVals;
bool fileLoad;
int  broken;

extern long numGC;

/**********************************************************************/
/* MARK macros for garbage collection                                 */
/**********************************************************************/
#define MARK(n)         markBit[n>>6]     |= (1 << ((n >> 3) & 0x07))
#define MARKED(n)      (markBit[n>>6]     &  (1 << ((n >> 3) & 0x07)))
#define MARK_REAL(n)    markRealBit[n>>6] |= (1 << ((n >> 3) & 0x07))
#define MARKED_REAL(n) (markRealBit[n>>6] &  (1 << ((n >> 3) & 0x07)))

/**********************************************************************/
/* Make a string lower case                                           */
/**********************************************************************/
#ifndef _WIN32
char* strlwr(char* s)
{
    char* r = s;
    while (*s)
    {
        *s = tolower(*s);
        ++s;
    }
    return r;      
}
#endif /*_WIN32*/

/**********************************************************************/
/* init memory for atoms                                              */
/**********************************************************************/
static void initAtoms(void)
{
    memset(strStore,'\x01',atomSize);
    strStore[0] = '\0'; 

    QUOTE           = findAtom("quote");
    DEFINE          = findAtom("define");
    LET             = findAtom("let");
    LETREC          = findAtom("letrec");
    LAMBDA          = findAtom("lambda");
    BEGIN           = findAtom("begin");
    ELSE            = findAtom("else");
    COND            = findAtom("cond");
    IF              = findAtom("if");
    AND             = findAtom("and");
    OR              = findAtom("or");
    LIST            = findAtom("list");
    VECTOR          = findAtom("vector");
    SET             = findAtom("set!");
    DELAY           = findAtom("delay");
    CALLCC          = findAtom("call/cc");
    APPLY           = findAtom("apply");
    IT              = findAtom("it");
    CASE            = findAtom("case");
    QUASIQUOTE      = findAtom("quasiquote");
    UNQUOTE         = findAtom("unquote");
    UNQUOTESPLICING = findAtom("unquote-splicing");
    EVAL            = findAtom("eval");
    MACRO           = findAtom("define-macro");
}
/**********************************************************************/
/* find atom in store or add it if not found                          */
/**********************************************************************/
IPTR findAtom(char* str)
{
    char *store = strStore, *newstr =str;
    int len;
 
    for(;;)
    {
        if (*store != *newstr)
        {
            /* advance to end of string in store */
            while (*store++)
                ;

            if (*store == '\x01')
            {
                /* end of store, check for space and insert string */
                len = strlen(str);
                if (store - strStore +len +1 >= atomSize)
                    error(ERR_M1_STRING_FULL,NIL);
 
                memcpy(store,str,len+1);
                return MKATOM(store-strStore);
            }
            /* continue comparison at start of search string */
            newstr = str;
        }
        else
        {
            if (*store == '\0')
                /* string found */
                return MKATOM(store-strStore-(newstr-str));
            
            /* compare next chars */
            ++store;
            ++newstr;
        }
    }
}
/**********************************************************************/
/* get string for atom from memory                                    */
/**********************************************************************/
char* getAtom(IPTR p)
{
    if (IS_ATOM(p))
        return strStore + ATOMVAL(p);
    else
        error(ERR_M2_INVALID_PTR, NIL, p);
    return 0; /* never reached */
}
/**********************************************************************/
/* init real memory                                                   */
/**********************************************************************/
static void initReals(void)
{
    IPTR p;
    firstFreeReal = NIL;

    for (p=0; p<realSize; p+=sizeof(double))
    {
        *((IPTR*)(reals+p)) = firstFreeReal;
        firstFreeReal = p;
    }
}
/**********************************************************************/
/* allocate a real in memory                                          */
/**********************************************************************/
IPTR allocReal(double d)
{
    IPTR res;
    if (firstFreeReal == NIL)
    {
        gc(NIL,NIL);
        if (firstFreeReal == NIL)
            error(ERR_M9_NO_REALS,NIL);
    }
    res = firstFreeReal;
    firstFreeReal = *((IPTR*)(reals+firstFreeReal));
    *((double*)(reals+res)) = d;

    return MKREAL(res);
}
/**********************************************************************/
/* get a real from memory                                             */
/**********************************************************************/
double getReal(IPTR p)
{
    if (!IS_REAL(p))
        error(ERR_M2_INVALID_PTR, NIL, p);

    return *((double*) (reals + REALVAL(p)));
}
/**********************************************************************/
/* init heap cells                                                    */
/**********************************************************************/
static void initHeap(void)
{
    IPTR p;

    firstFree = NIL;
    S = E = C = D = W = NIL;
    tlNames = tlVals = NIL;

    for (p=0; p<heapSize; p+=2*sizeof(IPTR))
    {
        car(p) = firstFree;
        firstFree = p;
    }
}
/**********************************************************************/
/* allocate a heap cell                                               */
/**********************************************************************/
IPTR cons(IPTR a, IPTR b)
{
    IPTR res;

    if (firstFree == NIL)
    {
        gc(a,b);
        if (firstFree == NIL)
            error(ERR_M4_NO_HEAP, NIL);
    }
    res       = firstFree;
    firstFree = car(firstFree);
    car(res)  = a; 
    cdr(res)  = b;
    return res;
}
/**********************************************************************/
/* mark phase of garbage collection                                   */
/**********************************************************************/
static void markVector(IPTR p);
static void mark(IPTR p)
{
    CHECKSTACK(p)

        loop:
    if (IS_CONS(p))
    {
        if (!MARKED(p))
        {
            MARK(p);
            if (IS_VECFAST(p))
                markVector(p);

            else if (HAS_PTRCDR(p))
                return;

            else
            {
                mark(car(p));
                p = cdr(p);
                goto loop;
            }
        }
    }
    else if (IS_REAL(p))
        MARK_REAL(p);
}
/**********************************************************************/
/* mark a vector                                                      */
/**********************************************************************/
static void markVector(IPTR p)
{
    IPTR* pel;
    long num;

    pel = VECTORMEM(p);
    num = LENGTH(p);
    while (num--)
        mark(*pel++);
}
/**********************************************************************/
/* collect unused cells                                               */
/**********************************************************************/
static void collect(void)
{
    IPTR  p;
    for (p=0; p < heapSize; p += 2 * sizeof(IPTR))
        if (!MARKED(p))
        {
            switch (car(p))
            {
            case IPRT_TAG:
            case OPRT_TAG:
                fclose(FILEPTR(p));
                break;

            case EMPTY_STR:
            case EMPTY_VEC:
                break;

            default:
                if (IS_STRFAST(p))
                    free(STRINGMEM(p));

                else if (IS_VECFAST(p))
                    free(VECTORMEM(p));
            }
            car(p) = firstFree;
            firstFree = p;
        }

    for (p=0; p<realSize; p+=sizeof(double))
        if (!MARKED_REAL(p))
        {
            *((IPTR*)(reals+p)) = firstFreeReal;
            firstFreeReal = p;
        }
}
/**********************************************************************/
/* collect garbage                                                    */
/**********************************************************************/
void gc(IPTR a, IPTR b)
{
    int i;

#ifdef DUMP_STEPS
    fputs("### GC invoked ###\n",logFile);
#endif

    memset(markBit,     0, heapSize >> 6);
    memset(markRealBit, 0, realSize >> 6);

    /*------------------------------------------------------------------*/
    /* mark top level REP values                                        */
    /*------------------------------------------------------------------*/
    mark(newTopLevelVals);
    mark(tlNames); 
    mark(tlVals);
    mark(a); 
    mark(b);
    mark(S); 
    mark(E); 
    mark(C); 
    mark(D); 
    mark(W);

    for (i=0; i < protIdx; ++i)
    {
#ifdef DUMP_STEPS
        fprintf(logFile,"Protected[%d] = ",i); 
        printSEXPfp(logFile, *(protPtr[i]), PRT_ESCAPE); 
        fputc('\n',logFile);
#endif
        mark(*(protPtr[i]));
    }
    firstFree     = NIL;
    firstFreeReal = NIL;
    collect();
    ++numGC;
}
/**********************************************************************/
/* memory statistics                                                  */
/**********************************************************************/
void memStat(void)
{
    int   i;
    char* p;
    long int heapUse    = 0;
    long int realUse    = 0;
    long int atomUse    = 0;
    long int numStrings = 0;
    long int numVectors = 0;
    long int numFiles   = 0;
    long int stringMem  = 0;
    long int vectorMem  = 0;

    S = E = D = W = C = NIL;
    protIdx = 0;
    gc(NIL,NIL);

    for (i=0; i < heapSize; i+=2*sizeof(IPTR))
    {
        if (MARKED(i))
            ++heapUse;
        if (IS_STRING(i))
        {
            ++numStrings;
            stringMem += LENGTH(i);
        }
        else if (IS_VEC(i))
        {
            ++numVectors;
            vectorMem += LENGTH(i);
        }
        else if (IS_PORT(i))
            ++numFiles;
    }

    for (i=0; i < realSize; i+=sizeof(double))
        if (MARKED_REAL(i))
            ++realUse;

    for (p=strStore; *p != '\x01'; ++p)
        ;
    atomUse = p-strStore;

    fprintf(stderr,"========= Memory Usage =========\n"
            "Atoms:   %ld/%ld bytes\n"
            "Reals:   %ld/%ld cells\n"
            "Heap:    %ld/%ld cells\n"
            "Strings: %ld=%ld bytes\n"
            "Vectors: %ld=%ld cells\n"
            "Ports:   %ld\n",
            atomUse, atomSize,
            realUse, realSize / sizeof(double),
            heapUse, heapSize / (2*sizeof(IPTR)),
            numStrings, stringMem,
            numVectors, vectorMem,
            numFiles);
}
/**********************************************************************/
/* init all memory                                                    */
/**********************************************************************/
void initMemory(void)
{
    if ((strStore = malloc(atomSize)) == NULL)
        fatal("Can't allocate atoms");

    if ((heap = malloc(heapSize)) == NULL)
        fatal("Can't allocate heap");

    if ((markBit = malloc(heapSize>>6)) == NULL)
        fatal("Can't allocate mark bits");

    if ((reals = malloc(realSize)) == NULL)
        fatal("Can't allocate FP");

    if ((markRealBit = malloc(realSize>>6)) == NULL)
        fatal("Can't allocate FP mark bits");

    initAtoms();
    initHeap();
    initReals();

    tlNames          = cons(cons(IT,NIL),NIL);
    tlVals           = cons(cons(NOPRINT,NIL),NIL);
    newTopLevelNames = NIL;
    newTopLevelVals  = NIL;
}
/**********************************************************************/
/* length of a list                                                   */
/**********************************************************************/
long listLength(IPTR l)
{
    long len = 0;
    IPTR   p = l;
    while (IS_PAIR(p))
    {
        p = cdr(p);
        ++len;
    }
    if (p!=NIL)
        error(ERR_C6_IMPROPER_ARGS, l);
    return len;
}
/**********************************************************************/
/* Create a vector (either filled with constant or init'ed by a list) */
/**********************************************************************/
IPTR makeVector(long len, IPTR fill, bool advance)
{
    int  j;
    IPTR  res;
    IPTR* mem;

    if (len == 0)
        return cons(EMPTY_VEC,(IPTR)0);

    if (len > MAXLEN)
        error(ERR_R15_INVALID_PARM, MKINT(len), "make-vector");

    if ((mem =malloc(sizeof(IPTR) *len)) == NULL)
        error(ERR_M7_NO_VECTOR_MEM, NIL);

    res = cons(MKVECTOR(len),(IPTR)mem);
    for (j=0; j<len; ++j)
        if (advance)
        {
            mem[j] = car(fill);
            fill   = cdr(fill);
        }
        else
            mem[j] = fill;
    return res;
}
/**********************************************************************/
/* Create a string                                                    */
/**********************************************************************/
IPTR makeString(long len, char* buf, long len2, char* buf2, IPTR l)
{
    IPTR   res, p = l;
    char* mem, *cp;

    if (len +len2 == 0 || (buf == 0 && p == NIL))
        return cons(EMPTY_STR,(IPTR)0);

    if (len+len2 > MAXLEN)
        error(ERR_R15_INVALID_PARM,MKINT(len+len2),"makeString");

    if ((cp = mem = malloc(len+len2)) == NULL)
        error(ERR_M8_NO_STRING_MEM,NIL);

    res = cons(MKSTRING(len+len2),(IPTR)mem);
    if (buf==0)
    {
        if (buf2==0)
        {
            /*--------------------------------------------------------------*/
            /* Fill by copying characters from list                         */
            /*--------------------------------------------------------------*/
            while (IS_PAIR(p))
            {
                if (!IS_CHAR(car(p)))
                    typeError(car(p),"char");

                *cp++ = CHARVAL(car(p));
                p = cdr(p);
            }
            if (p!=NIL)
                typeError(l,"list");
        } 
        else
        {
            /*--------------------------------------------------------------*/
            /* Fill with copies of single character                         */
            /*--------------------------------------------------------------*/
            memset (cp, CHARVAL(l), len);
        }  
    }
    else
    {
        memcpy (cp,buf,len);
        if (buf2)
            memcpy(cp+len,buf2,len2);
    }
    return res;
}
/**********************************************************************/
/* Append 2 strings                                                   */
/**********************************************************************/
IPTR appendStrings (IPTR a, IPTR b)
{
    IPTR t;

    if (!IS_STRING(a))
        typeError(a,"string");

    if (!IS_STRING(b))
        typeError(b,"string");

    if (LENGTH(a) == 0) {
        t = b; 
        b = a; 
        a = t;
    }
    return makeString (LENGTH(a), STRINGMEM(a),
                       LENGTH(b), STRINGMEM(b), NIL);
}
/**********************************************************************/
/* Extract substring                                                  */
/**********************************************************************/
IPTR substring (IPTR str, int start, int end)
{
    int len;

    if (!IS_STRING(str))
        typeError(str,"string");

    len = LENGTH(str);
    if (start < 0 || start > len)
        error(ERR_R2_INVALID_INDEX, MKINT(start));

    if (end > len)
        end = len;

    return makeString(max(0,end-start), STRINGMEM(str)+start, 0, 0, NIL);
}
/**********************************************************************/
/* compare strings                                                    */
/**********************************************************************/
int strComp (IPTR a, IPTR b)
{
    int      i,d;
    char*    p1;
    char*    p2;

    p1 = STRINGMEM(a);
    p2 = STRINGMEM(b);
    for (i=0; i < min(LENGTH(a),LENGTH(b)); ++i)
    {
        d = *p1++ - *p2++;
        if (d)
            return d;
    }
    return i==LENGTH(a) ? (i==LENGTH(b) ? 0 : -1) : 1;
}
/**********************************************************************/
/* copy a list (one level) into destination, return last pair         */
/**********************************************************************/
IPTR* listCopy (IPTR *d, IPTR l, short nc)
{
    while (IS_PAIR(l) && --nc >= 0)
    {
        *d = cons(car(l), NIL);
        d = &cdr(*d);
        l = cdr(l);
    }
    return d;
}
/**********************************************************************/
/* create a list from a vector                                        */
/**********************************************************************/
IPTR vector2List (IPTR v)
{
    short    i,num;
    IPTR*     pel;
    IPTR      res = NIL;
    IPTR*     d = &res;

    PROTECT(res);
    num = LENGTH(v);
    pel = VECTORMEM(v);

    for (i=0;i<num;++i)
    {
        *d = cons(pel[i],NIL);
        d = &cdr(*d);
    }
    UNPROTECT(res);
    return res;
}
/**********************************************************************/
/* create a list from a string                                        */
/**********************************************************************/
IPTR string2List (IPTR str)
{
    long i,num;
    char* pc;
    IPTR   res = NIL;
    IPTR*  d = &res;

    PROTECT(res);
    num = LENGTH(str);
    pc = STRINGMEM(str);
    for (i=0;i<num;++i)
    {
        *d = cons(MKCHAR(*pc++),NIL);
        d  = &cdr(*d);
    }
    UNPROTECT(res);
    return res;
}
/**********************************************************************/
/* create a new unique symbol                                         */
/**********************************************************************/
IPTR gensym (void)
{
    static char buf[8];
    sprintf(buf,"G%03d", symCnt++);
    return findAtom(buf);
}
/**********************************************************************/
/* Convert a C string into LispMe string                              */
/**********************************************************************/
IPTR str2Lisp (char* cp)
{
    return makeString(strlen(cp), cp, 0, 0, NIL);
}
/**********************************************************************/
/* Open a port                                                        */
/**********************************************************************/
IPTR openPort (int mode, IPTR name)
{
    static char buf[80];

    FILE* fp = NULL;
    IPTR tag  = NIL, res = cons(NIL,NIL);

    if (!IS_STRING(name))
        typeError(name,"string");

    memcpy(buf, STRINGMEM(name), LENGTH(name));
    buf[LENGTH(name)] = '\0';

    switch (mode)
    {
    case PMODE_INPUT:
        if ((fp = fopen(buf,"r")) == NULL)
            error(ERR_R17_OPEN_FILE,name);
        
        tag = IPRT_TAG;
        break;

    case PMODE_OUTPUT:
        if ((fp = fopen(buf,"w")) == NULL)
            error(ERR_R16_CREATE_FILE, name);

        tag = OPRT_TAG;
        break;

    case PMODE_APPEND:
        if ((fp = fopen(buf,"a")) == NULL)
            error(ERR_R17_OPEN_FILE, name);
  
        tag = OPRT_TAG;
        break;
    }
    car(res) = tag; 
    cdr(res) = (IPTR)fp;
    return res;
}
/**********************************************************************/
/* Close a port                                                       */
/**********************************************************************/
void closePort(IPTR p)
{
    if (!IS_CONS(p))
        typeError(p, "port");

    switch (car(p))
    {
    case IPRT_TAG:
    case OPRT_TAG:
        fclose(FILEPTR(p));
        car(p) = CPRT_TAG;
        return;

    case CPRT_TAG:
        return;

    default:
        typeError(p,"port");
    }
}
