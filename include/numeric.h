/**********************************************************************/
/*                                                                    */
/* numeric.h:  LISPME complex arithmetic                              */
/*                                                                    */
/* LispMe System (c) FBI Fred Bayer Informatics                       */
/*                                                                    */
/* Distributed under the GNU General Public License;                  */
/* see the README file. This code comes with NO WARRANTY.             */
/*                                                                    */
/**********************************************************************/
#ifndef INC_CPLX_H
#define INC_CPLX_H

#include "store.h"

double trunc     (double x);
double round     (double x);
double asinh     (double x);
double acosh     (double x);
double atanh     (double x);

IPTR   addCpl    (double ar, double ai, double br, double bi);
IPTR   subCpl    (double ar, double ai, double br, double bi);
IPTR   mulCpl    (double ar, double ai, double br, double bi);
IPTR   divCpl    (double ar, double ai, double br, double bi);

IPTR   magnitude (IPTR p);
IPTR   angle     (IPTR p);

IPTR   cplExp    (double re, double im);
IPTR   cplLog    (double re, double im);
IPTR   cplSqrt   (double re, double im);

IPTR   cplSin    (double re, double im);
IPTR   cplCos    (double re, double im);
IPTR   cplTan    (double re, double im);

IPTR   cplSinh   (double re, double im);
IPTR   cplCosh   (double re, double im);
IPTR   cplTanh   (double re, double im);

IPTR   cplAsin   (double re, double im);
IPTR   cplAcos   (double re, double im);
IPTR   cplAtan   (double re, double im);

IPTR   cplAsinh  (double re, double im);
IPTR   cplAcosh  (double re, double im);
IPTR   cplAtanh  (double re, double im);

#endif
