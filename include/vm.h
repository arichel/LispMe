/**********************************************************************/
/*                                                                    */
/* vm.h:    Interface for virtual LISPKIT machine                     */
/*                                                                    */
/* LispMe System (c) FBI Fred Bayer Informatics                       */
/*                                                                    */
/* Distributed under the GNU General Public License;                  */
/* see the README file. This code comes with NO WARRANTY.             */
/*                                                                    */
/**********************************************************************/
#ifndef INC_VM_H
#define INC_VM_H

#include "store.h"

/**********************************************************************/
/* VM opcodes                                                         */
/**********************************************************************/
#define LDC    2 /* load constant                                     */
#define LDF    3 /* load function                                     */
#define AP     4 /* apply function                                    */
#define RTN    5 /* return from function                              */
#define DUM    6 /* create dummy environment                          */
#define RAP    7 /* recursively apply (letrec-block)                  */
#define SEL    8 /* select subcontrol list                            */
#define JOIN   9 /* join to main control list                         */
#define CAR   10 /* CAR of TOS object                                 */
#define CDR   11 /* CDR of TOS object                                 */
#define PAIR  12 /* PAIR? of TOS object                               */
#define CONS  13 /* CONS of 2 TOS objects                             */
#define EQ    14 /* EQ of 2 TOS objects                               */
#define ADD   15 /* add 2 TOS objects                                 */
#define SUB   16 /* subtract 2 TOS objects                            */
#define MUL   17 /* multiply 2 TOS objects                            */
#define DIV   18 /* divide 2 TOS objects                              */
#define REM   19 /* remainder of 2 TOS objects                        */
#define LEQ   20 /* <= of 2 TOS objects                               */
#define STOP  21 /* stop execution of VM                              */
#define NUL   22 /* NULL predicate of TOS object                      */
#define ST    23 /* store TOS to variable                             */
#define LDE   24 /* load expression (build recipe)                    */
#define AP0   25 /* apply parameterless function                      */
#define UPD   26 /* update recipe and return                          */
#define LDFC  27 /* load closure including number of args             */
#define APC   28 /* apply closure and check for number of args        */
#define LDCT  29 /* load continuation                                 */
#define UERR  30 /* report user error                                 */
#define INTP  31 /* is TOS an integer                                 */
#define SELR  32 /* select subcontrol without pushing rest            */
#define TAPC  33 /* tail-apply closure and check for number of args   */
#define POP   34 /* pop topmost stack object                          */
#define DSPL  35 /* display topmost stack object                      */
#define CERR  36 /* cond error, no true clause found                  */
#define SCAR  37 /* set CAR of TOS object                             */
#define SCDR  38 /* set CDR of TOS object                             */
#define EQV   39 /* EQV of 2 TOS objects                              */
#define SQRT  40 /* sqrt of TOS object                                */
#define SIN   41 /* sin  of TOS object                                */
#define COS   42 /* cos  of TOS object                                */
#define TAN   43 /* tan  of TOS object                                */
#define ASIN  44 /* asin of TOS object                                */
#define ACOS  45 /* acos of TOS object                                */
#define ATAN  46 /* atan of TOS object                                */
#define EXP   47 /* exp  of TOS object                                */
#define LOG   48 /* log  of TOS object                                */
#define MAGN  49 /* magnitude                                         */
#define ATN2  50 /* atan2 of 2 TOS objects                            */
#define SINH  51 /* sinh  of TOS object                               */
#define COSH  52 /* cosh  of TOS object                               */
#define TANH  53 /* tanh  of TOS object                               */
#define ASIH  54 /* asinh of TOS object                               */
#define ACOH  55 /* acosh of TOS object                               */
#define ATAH  56 /* atanh of TOS object                               */
#define REAP  57 /* is TOS a real                                     */
#define FLOR  58 /* floor of TOS object                               */
#define CEIL  59 /* ceil of TOS object                                */
#define TRUN  60 /* trunc of TOS object                               */
#define ROUN  61 /* round of TOS object                               */
#define INTG  62 /* convert TOS to integer                            */
#define IDIV  63 /* integer division of 2 TOS objects                 */
#define ANGL  64 /* angle                                             */
#define BOOL  65 /* BOOLEAN? of TOS object                            */
#define NOT   66 /* NOT of TOS object                                 */
#define RECT  67 /* Draw a rectangle (filled)                         */
#define DRAW  68 /* Draw a line                                       */
#define CHR   69 /* CHAR? of TOS object                               */
#define STRG  70 /* STRING? of TOS object                             */
#define WRIT  71 /* write topmost stack object                        */
#define REAC  72 /* read-char                                         */
#define SLEN  73 /* string-length                                     */
#define S2L   74 /* string->list                                      */
#define L2S   75 /* list->string                                      */
#define SREF  76 /* string-ref                                        */
#define SAPP  77 /* string-append                                     */
#define SSET  78 /* string-set!                                       */
#define SUBS  79 /* substring                                         */
#define SEQ   80 /* string=?                                          */
#define C2I   81 /* char->integer                                     */
#define I2C   82 /* integer->char                                     */
#define PROC  83 /* procedure?                                        */
#define CONT  84 /* continuation?                                     */
#define PROM  85 /* promise?                                          */
#define SYMB  86 /* symbol?                                           */
#define O2S   87 /* object->string                                    */
#define S2O   89 /* string->object                                    */
#define RAND  90 /* random                                            */
#define DASM  91 /* disassemble procedure                             */
#define OOUT  93 /* open-output-file                                  */
#define OINP  94 /* open-input-file                                   */
#define PEEK  95 /* peek-char                                         */
#define DELF  96 /* delete-file                                       */
#define INPP  97 /* input-port?                                       */
#define OUPP  98 /* output-port?                                      */
#define EOFO  99 /* eof-object?                                       */
#define READ 100 /* read                                              */
#define REDL 101 /* read-line                                         */
#define OAPP 102 /* open-append-file                                  */
#define VECP 103 /* vector?                                           */
#define VSET 104 /* vector-set!                                       */
#define VREF 105 /* vector-ref                                        */
#define VMAK 106 /* make-vector                                       */
#define VLEN 107 /* vector-length                                     */
#define V2L  108 /* vector->list                                      */
#define L2V  109 /* list->vector                                      */
#define SEO  110 /* select subcontrol, don't pop if #t                */
#define SEOR 111 /* select subcontrol, don't pop if #t, don't push cdr*/
#define SEA  112 /* select subcontrol, don't pop if #f                */
#define SEAR 113 /* select subcontrol, don't pop if #f, don't push cdr*/
#define MEM  114 /* select subcontrol if TOS is in set                */
#define MEMR 115 /* select subcontrol if TOS is in set, don't push cdr*/
#define MKRA 116 /* make-rectangular                                  */
#define MKPO 117 /* make-polar                                        */
#define CPLP 118 /* is TOS a complex                                  */
#define REPA 119 /* real-part                                         */
#define IMPA 120 /* imag-part                                         */
#define APY  121 /* apply closure and check for number of args,copy   */
#define TAPY 122 /* tail apply clos. and check for number of args,copy*/
#define APND 123 /* append 2 TOS items                                */
#define EVA  124 /* eval                                              */
#define GSYM 125 /* gensym                                            */
#define CLOS 126 /* close-[input/output]-port                         */
#define NONE 127 /* none?                                             */
#define TAP  128 /* tail-apply let closure                            */
#define RTAP 129 /* tail-apply letrec closure                         */
#define SMAK 130 /* make-string                                       */
#define DOGC 131 /* gc                                                */
#define LDM  132 /* load macro                                        */
#define MACP 133 /* macro?                                            */
#define LAND 134 /* bit-and                                           */
#define LIOR 135 /* bit-or                                            */
#define LXOR 136 /* bit-xor                                           */
#define LNOT 137 /* bit-not                                           */
#define LSHT 138 /* bit-shift                                         */
#define LST  139 /* make a list out of n TOS values                   */
#define CXR  140 /* generalized c[ad]+r                               */
#define SYMV 141 /* symval                                            */
#define LC0  142 /* LDC 0                                             */
#define LC1  143 /* LDC 1                                             */
#define LC2  144 /* LDC 2                                             */
#define LC3  145 /* LDC 3                                             */
#define LC_1 146 /* LDC -1                                            */
#define LC_T 147 /* LDC #t                                            */
#define LC_F 148 /* LDC #f                                            */
#define LC_N 149 /* LDC NIL                                           */
#define LC_E 150 /* LDC ""                                            */

#define NOP 0xff /* marks end of code list in compiler tables         */

#define PI 3.141592653589793

extern short expectedLen;

IPTR exec      (void);
IPTR makeNum   (long n);
IPTR storeNum  (double re, double im);
IPTR makePolar (double mag, double ang);
IPTR exec      (void);
IPTR locate    (IPTR loc, LEXADR adr);

#endif
