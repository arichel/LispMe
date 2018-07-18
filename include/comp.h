/**********************************************************************/
/*                                                                    */
/* comp.h:  LISP to SECD compiler                                     */
/*                                                                    */
/* LispMe System (c) FBI Fred Bayer Informatics                       */
/*                                                                    */
/* Distributed under the GNU General Public License;                  */
/* see the README file. This code comes with NO WARRANTY.             */
/*                                                                    */
/**********************************************************************/
#ifndef INC_COMP_H
#define INC_COMP_H

#include "store.h"

extern bool macroExpand;

void   InitCompiler (void);
LEXADR location     (IPTR var, IPTR env, bool throwErr);
IPTR   compile      (IPTR expr, IPTR names);

#endif
