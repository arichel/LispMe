/**********************************************************************/
/*                                                                    */
/* store.h: Structures for storage management                         */
/*                                                                    */
/* LispMe System (c) FBI Fred Bayer Informatics                       */
/*                                                                    */
/* Distributed under the GNU General Public License;                  */
/* see the README file. This code comes with NO WARRANTY.             */
/*                                                                    */
/**********************************************************************/
#ifndef INC_STORE_H
#define INC_STORE_H

/**********************************************************************/
/* Memory organization of LISP-KIT:                                   */
/*                                                                    */
/* 1. String store:                                                   */
/*    All atoms are stored contiguously, separated by 0x00 bytes, so  */
/*    they can directly accessed as C strings. After last used atom   */
/*    all storage is initialized with 0x01                            */
/*                                                                    */
/* 2. Cons storage:                                                   */
/*    uniform 64 bit cells, two PTRs each                             */
/*                                                                    */
/* 3. PTR format:                                                     */
/*    32 bit pointers, lower 3 bits determine type                    */
/*                                                                    */
/* sxxx xxxx xxxx xxxx xxxx xxxx xxxx xxx1 31 bit signed integer      */
/*                                                                    */
/* 0xxx xxxx xxxx xxxx xxxx xxxx xxxx x000 31 bit signed offset into  */
/*                                         heap, no scale             */
/*                                                                    */
/* 0xxx xxxx xxxx xxxx xxxx xxxx xxxx x010 28 bit index into atom     */
/*                                         table (char*)              */
/*                                                                    */
/* 0xxx xxxx xxxx xxxx xxxx xxxx xxxx x110 28 bit index into double   */
/*                                         table (double*)            */
/*                                         unmask 3 low bits, no scale*/
/*                                                                    */
/* llll llll llll llll llll llll 0000 1100 vector, 24 bit length      */
/*                                                                    */
/* llll llll llll llll llll llll 0001 1100 string, 24 bit length      */
/*                                                                    */
/* llll llll llll llll llll llll 0010 1100 realvector, 24 bit length  */
/*                                                                    */
/* llll llll llll llll llll llll 0011 1100 bigint, 24 bit length      */
/*                                                                    */
/* 0000 0000 0000 0000 0000 0000 0100 1100 input port                 */
/*                                                                    */
/* 0000 0000 0000 0000 0000 0000 0101 1100 output port                */
/*                                                                    */
/* 0000 0000 0000 0000 0000 0000 0110 1100 closed port                */
/*                                                                    */
/* 0000 0000 0000 0000 0000 0000 1100 0100 lambda for macro           */
/*                                                                    */
/* ???? ???? ???? ???? xxxx xxxx 1001 1100 special values in car      */
/*                                                                    */
/* ???? ???? ???? ???? aaaa aaaa 0000 0100 8 bit ASCII/EBCDIC char    */
/*                                                                    */
/* ???? ???? ???? ???? xxxx xxxx 1001 0100 reserved for special values*/
/*                                                                    */
/* 4. Internal compound data:                                         */
/*    Complex:       (CPLX_TAG real . imag)                           */
/*    Closures:      (CLOS_TAG arity code . env)                      */
/*    Macros:        (MACR_TAG . expander)                            */
/*    Continuations: (CONT_TAG stack env code . dump)                 */
/*    Promises:      (RCPD_TAG code . env)                            */
/*                   (RCPF_TAG . value)                               */
/*    Ports:         (PORT_TAG . fileptr)                             */
/*    Strings:       (STRG_TAG . cptr)                                */
/*    Vectors:       (VECT_TAG . cptr)                                */
/*                                                                    */
/**********************************************************************/
#include <stdio.h>
#include <stdlib.h>

#define DEFAULT_HEAP_SIZE         (2  *1024 *1024)
#define DEFAULT_REAL_STORE_SIZE   (32 *1024)
#define DEFAULT_STRING_STORE_SIZE (16 *1024)

#define MAX_PROTECT 1000
#define PTR_BITLEN    32

/**********************************************************************/
/* typedefs                                                           */
/**********************************************************************/
typedef long IPTR;
typedef char bool;

#define false 0
#define true  1

#define MAXINT   1073741823
#define MININT (-1073741824)
#define MAXLEN   16777215

struct LispMePrefs {
    char printDepth;
    bool printQuotes;
};
/**********************************************************************/
/* macros                                                             */
/**********************************************************************/
#define IS_INT(ptr)     ((ptr)  & 1)
#define IS_REAL(ptr)    (((ptr) & 7) == 6)
#define IS_ATOM(ptr)    (((ptr) & 7) == 2)
#define IS_CONS(ptr)    (((ptr) & 7) == 0)
#define IS_SPECIAL(ptr) (((ptr) & 0xff) == 0x94)
#define IS_INTERN(ptr)  (((ptr) & 0xf7) == 0x94)
#define IS_CHAR(ptr)    (((ptr) & 0xff) == 0x04)
#define IS_IPORT(ptr)   (IS_CONS(ptr) && car(ptr) == IPRT_TAG)
#define IS_OPORT(ptr)   (IS_CONS(ptr) && car(ptr) == OPRT_TAG)
#define IS_CPORT(ptr)   (IS_CONS(ptr) && car(ptr) == CPRT_TAG)
#define IS_MACRO(ptr)   (IS_CONS(ptr) && car(ptr) == MACR_TAG)
#define IS_PORT(ptr)    (IS_CONS(ptr) && (car(ptr) & 0x4f) == 0x4c)
#define IS_VEC(ptr)     (IS_CONS(ptr) && (car(ptr) & 0xff) == 0x0c)
#define IS_STRING(ptr)  (IS_CONS(ptr) && (car(ptr) & 0xff) == 0x1c)
#define IS_RVEC(ptr)    (IS_CONS(ptr) && (car(ptr) & 0xff) == 0x2c)
#define IS_BIGINT(ptr)  (IS_CONS(ptr) && (car(ptr) & 0xff) == 0x3c)
#define HAS_PTRCDR(ptr) ((car(ptr) & 0x8f) == 0x0c)
#define IS_VECFAST(ptr) ((car(ptr) & 0xff) == 0x0c)
#define IS_STRFAST(ptr) ((car(ptr) & 0xff) == 0x1c)
#define IS_COMPLEX(ptr) (IS_CONS(ptr) && car(ptr) == CPLX_TAG)
#define IS_PAIR(ptr)    (IS_CONS(ptr) && (((car(ptr)) & 0x0f) != 0x0c))

#define EMPTY_STR       ((IPTR)0x001c)
#define EMPTY_VEC       ((IPTR)0x000c)

#define BLACK_HOLE      ((IPTR)0x0294)
#define NIL             ((IPTR)0x0394)
#define FALSE           ((IPTR)0x0494)
#define TRUE            ((IPTR)0x0594)
#define NOPRINT         ((IPTR)0x0694)
#define END_OF_FILE     ((IPTR)0x0794)

#define CLOS_TAG        ((IPTR)0x019c)
#define RCPD_TAG        ((IPTR)0x029c)
#define RCPF_TAG        ((IPTR)0x039c)
#define CONT_TAG        ((IPTR)0x049c)
#define CPLX_TAG        ((IPTR)0x059c)
#define MACR_TAG        ((IPTR)0x069c)

#define IPRT_TAG        ((IPTR)0x004c)
#define OPRT_TAG        ((IPTR)0x005c)
#define CPRT_TAG        ((IPTR)0x006c)

#define MLAMBDA         ((IPTR)0x00c4)

#define MKATOM(off)     (((off)<< 3) | 2)
#define ATOMVAL(ptr)    ((ptr) >> 3)
#define INTVAL(ptr)     ((ptr) >> 1)
#define MKINT(i)        ((IPTR)((i) << 1) | 1)
#define REALVAL(ptr)    ((ptr) & 0xfffffff8)
#define MKREAL(off)     ((IPTR)((off) | 6))

#define CHARVAL(ptr)    (((ptr) >> 8) & 0xff)
#define MKCHAR(c)       ((IPTR)(((unsigned char)(c)) << 8) | 0x04)
#define LENGTH(ptr)     (((unsigned long)(car(ptr))) >> 8)
#define STRINGMEM(p)    ((char*)cdr(p))
#define VECTORMEM(p)    ((IPTR*)cdr(p))
#define FILEPTR(p)      ((FILE*)cdr(p))

#define MKVECTOR(l)     ((((unsigned long)(l))<<8) | 0x0c)
#define MKSTRING(l)     ((((unsigned long)(l))<<8) | 0x1c)

#define PMODE_INPUT   1
#define PMODE_OUTPUT  2
#define PMODE_APPEND  3

#define car(ptr)    (*((IPTR*)(heap+ptr)))
#define cdr(ptr)    (*((IPTR*)(heap+ptr+sizeof(IPTR))))
#define caar(ptr)   car(car(ptr))
#define cadr(ptr)   car(cdr(ptr))
#define cdar(ptr)   cdr(car(ptr))
#define cddr(ptr)   cdr(cdr(ptr))
#define caaar(ptr)  car(car(car(ptr)))
#define caadr(ptr)  car(car(cdr(ptr)))
#define cadar(ptr)  car(cdr(car(ptr)))
#define caddr(ptr)  car(cdr(cdr(ptr)))
#define cdaar(ptr)  cdr(car(car(ptr)))
#define cdadr(ptr)  cdr(car(cdr(ptr)))
#define cddar(ptr)  cdr(cdr(car(ptr)))
#define cdddr(ptr)  cdr(cdr(cdr(ptr)))

/**********************************************************************/
/* Encoding of (frame . slot) in an integer                           */
/**********************************************************************/
typedef long LEXADR;
#define FRAME_SHIFT                 16
#define SLOT_MASK               0xffff
#define MAX_FRAMES                4096
#define MAX_VARS_IN_FRAME        65636
#define VAR_NOT_FOUND      (0xffffffff)

/**********************************************************************/
/* Aliases for keyword IPTRs                                           */
/**********************************************************************/
#define QUOTE           keyWords[0]
#define QUASIQUOTE      keyWords[1]
#define UNQUOTE         keyWords[2]
#define UNQUOTESPLICING keyWords[3]
#define DEFINE          keyWords[4]
#define LET             keyWords[5]
#define LETREC          keyWords[6]
#define LAMBDA          keyWords[7]
#define BEGIN           keyWords[8]
#define ELSE            keyWords[9]
#define COND            keyWords[10]
#define IF              keyWords[11]
#define AND             keyWords[12]
#define OR              keyWords[13]
#define LIST            keyWords[14]
#define VECTOR          keyWords[15]
#define SET             keyWords[16]
#define DELAY           keyWords[17]
#define CALLCC          keyWords[18]
#define APPLY           keyWords[19]
#define IT              keyWords[20]
#define CASE            keyWords[21]
#define EVAL            keyWords[22]
#define SYMVAL          keyWords[23]
#define MACRO           keyWords[24]
/* keep this line last and uptodate ! */
#define LAST_KEYWORD             25

/**********************************************************************/
/* Protect for GC                                                     */
/**********************************************************************/
#define PROTECT(p)   {protPtr[protIdx++] = &p;}
#define UNPROTECT(p) {--protIdx;}
#define CHECKSTACK(p)

/**********************************************************************/
/* exported data                                                      */
/**********************************************************************/
extern long   heapSize;
extern long   atomSize;
extern long   realSize;

extern char*  heap;
extern IPTR   S, E, C, D, W;
extern IPTR   keyWords[];
extern IPTR*  protPtr[MAX_PROTECT];
extern int    protIdx;
extern char*  stackLimit;
extern IPTR   tlNames;
extern IPTR   tlVals;
extern IPTR   newTopLevelNames;
extern IPTR   newTopLevelVals;
extern bool   fileLoad;

extern struct LispMePrefs LispMePrefs;
extern int    broken;

/**********************************************************************/
/* Some macros                                                        */
/**********************************************************************/
#ifndef _WIN32
#  define max(a,b) ((a)>(b)?(a):(b))
#  define min(a,b) ((a)<(b)?(a):(b))
   char* strlwr(char* s);
#endif /* _WIN32 */

/**********************************************************************/
/* prototypes                                                         */
/**********************************************************************/
void   initMemory    (void);

IPTR   findAtom      (char* str);
char*  getAtom       (IPTR p);

IPTR   allocReal     (double d);
double getReal       (IPTR p);

IPTR   cons          (IPTR a, IPTR b);
void   gc            (IPTR a, IPTR b);

long   listLength    (IPTR l);
IPTR*  listCopy      (IPTR* d, IPTR l, short nc);

IPTR   makeString    (long len, char* buf, long len2, char* buf2, IPTR l);
IPTR   appendStrings (IPTR a, IPTR b);
IPTR   substring     (IPTR str, int start, int end);
IPTR   string2List   (IPTR str);
IPTR   str2Lisp      (char* cp);
int    strComp       (IPTR a, IPTR b);

IPTR   makeVector    (long len, IPTR fill, bool advance);
IPTR   vector2List   (IPTR v);

IPTR   openPort      (int mode, IPTR name);
void   closePort     (IPTR p);

IPTR   gensym        (void);
void   memStat       (void);

#endif
