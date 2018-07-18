/**********************************************************************/
/*                                                                    */
/* cplx.c:  LISPME complex arithmetic                                 */
/*                                                                    */
/* LispMe System (c) FBI Fred Bayer Informatics                       */
/*                                                                    */
/* Distributed under the GNU General Public License;                  */
/* see the README file. This code comes with NO WARRANTY.             */
/*                                                                    */
/**********************************************************************/
#include <math.h>

#include "numeric.h"
#include "store.h"
#include "vm.h"
#include "error.h"

/**********************************************************************/
/* Helper functions                                                   */
/**********************************************************************/
double trunc(double x)
{
  return x>0.0 ? floor(x) : ceil(x);
}

double round(double x)
{
  return x>0.0 ? floor(x+0.5) : ceil(x-0.5);
}

double asinh(double x)
{
  return log(x+sqrt(x*x+1.0));
}

double acosh(double x)
{
  return log(x+sqrt(x*x-1.0));
}

double atanh(double x)
{
  return 0.5*log((1.0+x)/(1.0-x));
}

static double mag(double re, double im)
{
    return sqrt(re*re + im*im);
}

static void divHlp(double ar, double ai, double br, double bi,
                   double* cr, double* ci)
{
    double z = br*br + bi*bi;
    *cr = (ar*br + ai*bi) / z;
    *ci = (ai*br - ar*bi) / z;
}

static void logHlp(double ar, double ai, double* br, double* bi)
{
    double res = mag(ar,ai);
    *br = log(res);
    *bi = atan2(ai,ar);
}

static void sqrtHlp(double ar, double ai, double* br, double* bi)
{
    double r1,r2,s,d;
    if (ar==0.0 && ai==0.0)
        *br = *bi = ar;
    else
    {
        r1 = ar;
        r1 = fabs(r1);
        r2 = 0.5*(mag(ar,ai)+r1);
        s  = sqrt(r2);
        d  = 0.5*ai/s;

        if (ar>0.0)
        {
            *br = s; *bi = d;
        }
        else
        {
            if (0.0>ai)
            {
                d = -d;
                s = -s;
            }
            *br = d; *bi = s;
        }
    }
}
/**********************************************************************/
/* Basic operations                                                   */
/**********************************************************************/
IPTR addCpl(double ar, double ai, double br, double bi)
{
    return storeNum(ar+br,ai+bi);
}

IPTR subCpl(double ar, double ai, double br, double bi)
{
    return storeNum(ar-br,ai-bi);
}

IPTR mulCpl(double ar, double ai, double br, double bi)
{
    return storeNum(ar*br - ai*bi, ar*bi + ai*br);
}

IPTR divCpl(double ar, double ai, double br, double bi)
{
    double cr,ci;
    divHlp(ar,ai,br,bi,&cr,&ci);
    return storeNum(cr,ci);
}
/**********************************************************************/
/* Magnitude and angle                                                */
/**********************************************************************/
IPTR magnitude(IPTR p)
{
    if (IS_INT(p))
    {
        if (INTVAL(p) < 0)
            return makeNum(-INTVAL(p));

        return p;
    }
    else if (IS_REAL(p))
    {
        if (getReal(p) < 0.0)
            return allocReal(-getReal(p));

        return p;
    }
    else if (IS_COMPLEX(p))
        return allocReal(mag(getReal(cadr(p)),getReal(cddr(p))));

    typeError(p,"number");
    return NIL;
}

IPTR angle(IPTR p)
{
    if (IS_INT(p))
    {
        if (INTVAL(p) < 0)
            return allocReal(PI);

        return MKINT(0);
    }
    else if (IS_REAL(p))
    {
        if (getReal(p) < 0.0)
            return allocReal(PI);

        return MKINT(0);
    }
    else if (IS_COMPLEX(p))
        return allocReal(atan2(getReal(cddr(p)),getReal(cadr(p))));

    typeError(p,"number");
    return NIL;
}
/**********************************************************************/
/* exp, log and sqrt                                                  */
/**********************************************************************/
IPTR cplExp(double re, double im)
{
    double res = exp(re);

    if (im != 0.0)
        return makePolar(res, im);

    return allocReal(res);
}

IPTR cplLog(double re, double im)
{
    double res,res1;

    if (im != 0.0)
    {
        logHlp(re,im,&res1,&res);
        return storeNum(res1,res);
    }
    if (re < 0.0)
        return storeNum(log(-re),PI);

    return allocReal(log(re));
}

IPTR cplSqrt(double re, double im)
{
    double res,res1;
    if (im == 0.0)
    {
        if (re < 0.0)
            return storeNum(0.0,sqrt(-re));
        else
            return allocReal(sqrt(re));
    }
    else
    {
        sqrtHlp(re,im,&res1,&res);
        return storeNum(res1,res);
    }
}
/**********************************************************************/
/* sin, cos, sinh, cosh                                               */
/**********************************************************************/
IPTR cplSin(double re, double im)
{
    if (im == 0.0)
        return allocReal(sin(re));
    else
        return storeNum(sin(re)*cosh(im),cos(re)*sinh(im));
}

IPTR cplCos(double re, double im)
{
    if (im == 0.0)
        return allocReal(cos(re));
    else
        return storeNum(cos(re)*cosh(im),sin(re) * -sinh(im));
}

IPTR cplSinh(double re, double im)
{
    if (im == 0.0)
        return allocReal(sinh(re));
    else
        return storeNum(sinh(re)*cos(im),cosh(re)*sin(im));
}

IPTR cplCosh(double re, double im)
{
    if (im == 0.0)
        return allocReal(cosh(re));
    else
        return storeNum(cosh(re)*cos(im),sinh(re)*sin(im));
}
/**********************************************************************/
/* asin, asinh, acos, acosh                                           */
/**********************************************************************/
IPTR cplAsin(double re, double im)
{
    double r1, r2, r3, r4;
    bool negate = false;

    if (im==0.0 && -1.0<=re && re<=1.0)
        return allocReal(asin(re));

    /*----------------------------------------------------------------*/
    /* Check dangerous case: z=iy for large y, avoid cancellation by  */
    /* symmetry asin(z) = -asin(-z) (stay out off Q1 and Q2           */
    /*----------------------------------------------------------------*/
    if (im > 0.0 || (im == 0.0 && re < 0.0))
    {
        re = -re;
        im = -im;
        negate = true;
    }
    r1 = re * re;
    r2 = im * im;

    sqrtHlp(1.0-(r1-r2), -2.0*re*im, &r3, &r4);
    logHlp(r3-im, re+r4, &r1 ,&r2);

    if (negate)
        r2 = -r2;
    else
        r1 = -r1;

    return storeNum(r2, r1);
}

IPTR cplAsinh(double re, double im)
{
    double r1, r2, r3, r4;
    bool negate = false;

    if (im == 0.0)
        return allocReal(asinh(re));

    /*----------------------------------------------------------------*/
    /* Check dangerous case: z=iy for large y, avoid cancellation by  */
    /* symmetry asinh(z) = -asinh(-z) (stay out of Q1 and Q4)         */
    /*----------------------------------------------------------------*/
    if (re > 0.0 || (re == 0.0 && im >= 0.0))
    {
        re = -re;
        im = -im;
        negate = true;
    }
    r1 = re*re;
    r2 = im*im;

    sqrtHlp(r1 -r2 +1.0, 2.0 *re *im, &r3, &r4);
    logHlp(r3-re, r4-im, &r1, &r2);

    if (!negate)
    {
        r1 = -r1;
        r2 = -r2;
    }
    return storeNum(r1,r2);
}

IPTR cplAcos(double re, double im)
{
    double r1, r2, r3, r4;
    bool negate = false, dropReal = false;

    if (im == 0.0 && -1.0 <= re && re <= 1.0)
        return allocReal(acos(re));

    if (im == 0.0 && re >= 0.0)
    {
        re = -re;
        negate = dropReal = true;
    }
    else if (im <= 0.0)
    {
        im = -im;
        negate = true;
    }
    r1 = re*re;
    r2 = im*im;

    sqrtHlp(1.0 -(r1 -r2), -2.0 *re *im, &r3, &r4);
    logHlp(re-r4, im+r3, &r1, &r2);

    if (!negate)
        r1 = -r1;

    return storeNum(dropReal ? 0.0 : r2, r1);
}

IPTR cplAcosh(double re, double im)
{
    double r1, r2, r3, r4;
    bool negate = false;

    if (im == 0.0 && 1.0 <= re)
        return allocReal(acosh(re));

    if (im < 0.0)
    {
        im = -im;
        negate = true;
    }
    r1 = re*re;
    r2 = im*im;

    sqrtHlp(1.0 -(r1-r2), -2.0 *re *im, &r3, &r4);
    logHlp(re-r4, im+r3, &r1, &r2);

    if (negate)
        r1 = -r1;

    return storeNum(r1,r2);
}

/**********************************************************************/
/* tan, tanh, atan, atanh                                             */
/**********************************************************************/
IPTR cplTan(double re, double im)
{
    double res;
    if (im == 0.0)
        return allocReal(tan(re));

    re *= 2.0;
    im *= 2.0;

    res =  cos(re) + cosh(im);
    return storeNum(sin(re)/res, sinh(im)/res);
}

IPTR cplTanh(double re, double im)
{
    double res;
    if (im == 0.0)
        return allocReal(tanh(re));

    re *= 2.0;
    im *= 2.0;
    res = cosh(re) + cos(im);

    return storeNum(sinh(re)/res, sin(im)/res);
}

IPTR cplAtan(double re, double im)
{
    double res, res1, res2, res3;

    if (im == 0.0)
        return allocReal(atan(re));

    divHlp(re, 1.0+im, -re, 1.0-im, &res, &res1);
    logHlp(res, res1, &res2, &res3);

    return storeNum(-0.5*res3, 0.5*res2);
}

IPTR cplAtanh(double re, double im)
{
    double res, res1, res2, res3;

    if (im == 0.0 && -1.0 <= re && re<=1.0)
        return allocReal(atanh(re));

    divHlp(1.0+re, im, 1.0-re, -im, &res, &res1);
    logHlp(res, res1, &res2, &res3);

    return storeNum(0.5*res2, 0.5*res3);
}
