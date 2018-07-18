/**********************************************************************/
/*                                                                    */
/* io.h:    Definitions for I/O of SEXPRs                             */
/*                                                                    */
/* LispMe System (c) FBI Fred Bayer Informatics                       */
/*                                                                    */
/* Distributed under the GNU General Public License;                  */
/* see the README file. This code comes with NO WARRANTY.             */
/*                                                                    */
/**********************************************************************/
#ifndef INC_IO_H
#define INC_IO_H

#include <stdio.h>
#include "store.h"

/**********************************************************************/
/* Printing flags                                                     */
/**********************************************************************/
#define PRT_ESCAPE   0x01
#define PRT_AUTOLF   0x02
#define PRT_SPACE    0x04
#define PRT_NO_HASHN 0x08

/**********************************************************************/
/* prototypes                                                         */
/**********************************************************************/
#define PRNT(p)  (printSEXPfp(stderr, (p), PRT_ESCAPE | PRT_NO_HASHN | PRT_SPACE))

void printSEXP   (char* buf, int size, IPTR p, char flags);
void printSEXPfp (FILE* fp, IPTR p, char flags);

IPTR readSEXP    (char* src);
IPTR readSEXPfp  (FILE* fp);

IPTR loadFile    (FILE* fp);
IPTR readFile    (IPTR port, int opcode);
void deleteFile  (IPTR fileName);

#endif
