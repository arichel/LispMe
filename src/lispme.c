/**********************************************************************/
/*                                                                    */
/* lispme.c: Read-Eval-Print-Loop and Test-Driver                     */
/*                                                                    */
/* LispMe System (c) FBI Fred Bayer Informatics                       */
/*                                                                    */
/* Distributed under the GNU General Public License;                  */
/* see the README file. This code comes with NO WARRANTY.             */
/*                                                                    */
/**********************************************************************/
#include <stdlib.h>
#include <signal.h>
#include <string.h>
#include <setjmp.h>
#include <stdio.h>
#include <ctype.h>
#include <time.h>

#include "lispme.h"
#include "store.h"
#include "io.h"
#include "vm.h"
#include "comp.h"
#include "error.h"

jmp_buf  catcher;
long int numStep;
long int numGC;

#ifdef DUMP_STEPS
   FILE* logFile;
#endif

static FILE* volatile in = NULL;

/**
 * @brief signal handler
 */
static void sig_handler(int sig)
{
    broken = 1;
    signal(SIGINT, &sig_handler);
}
/**
 * @brief Print usage info message to standard error port.
 */
static void usage(char* pname)
{
    fprintf(stderr, "Usage: %s <options> <files>\n"  \
            "  -h, -?    display help\n"             \
            "  -a <size> set atom size\n"            \
            "  -m <size> set heap size\n"            \
            "  -r <size> set FP size\n"
            "all sizes in bytes, may be followed by k (kilo) or M (mega)" \
            "<files> are loaded in order specified", pname);

    exit(EXIT_FAILURE);
}
/**
 * @brief Scan string giving size and round up.
 */
static long mem_size(char* s, long minSize, int multiple)
{
    long res = atol(s);
    switch(tolower(s[strlen(s)-1]))
    {
    case 'm':
        res *= 1024;

    case 'k':
        res *= 1024;
        /* all fall-thrus intentional, a very rare case :-) */
    }
    if (res % multiple)
        res = (res/multiple+1) * multiple;

    return max(minSize, res);
}
/**
 * @brief Load a source file.
 */
static void src_load(char* name)
{
    IPTR sexp;
    if ((in = fopen(name,"r")) != NULL)
    {
        fileLoad = 1;
	sexp = loadFile(in);

        S = D = NIL;
        C = compile(sexp, NIL);
        E = tlVals;

        exec();

        fclose(in);
        in = NULL;
    }
    else
        error(ERR_R17_OPEN_FILE, str2Lisp(name));
}
/**
 * @brief Interpreter startup options.
 */
static int cmd_option (int argn, char *argv[])
{
    int i, src_start =1;

    if(!argv)
        return 0;

    for(i = 1; i < argn; ++i)
        if (argv[i][0] != '-'){
            src_start = i;
            break;
        }
        else
        {
            switch (tolower(argv[i][1]))
            {
            case 'h':
            case '?':
            default:
                usage(argv[0]);

            case 'a':
                atomSize = mem_size(argv[++i], 1400, 1);
                break;

            case 'r':
                realSize = mem_size(argv[++i], 64, 64);
                break;

            case 'm':
                heapSize = mem_size(argv[++i], 1024, 64);
                break;
            }
        }

    return src_start;
}
/**
 * @brief LispMe interactive Read-Eval-Print-Loop.
 */
int lispme (int argn, char *argv[])
{
    static char fileName[256], *se, *env;
    clock_t start, stop;

    IPTR res;
    bool first = true;
    int  i, c, isrc;

#ifdef DUMP_STEPS
    logFile = fopen("lispme.log", "w");
#endif

    /* Interpreter options: */
    isrc = cmd_option(argn, argv);

    /* Init all components: */
init:
    initMemory();
    InitCompiler();
    first = true;

    /* Error caught, cleanup for new input: */
    if(setjmp(catcher))
    {
        if (in)
        {
            fclose(in);
            in = NULL;
        }
        putc('\n',stderr);
        fflush(stderr);
        fflush(stdin);

        if (listLength(tlNames) > listLength(tlVals))
            tlNames = cdr(tlNames);

        broken = 0;
        first  = false;
    }

    /* Toplevel REPLoop: */
    for (;;)
    {
        protIdx = 0;

        /* Read startup files: */
        if (first) {
            first = false;

            *fileName = '\0';
            if((env = getenv("LISPME_INIT")) && strlen(env) > 0)
                src_load(env);
            else
                src_load("lispmerc.scm");

            for(i = isrc; i < argn; ++i)
                src_load(argv[i]);
        }
        fputs("> ", stderr);
        fflush(stderr);
        fileLoad = 0;

        while (isspace(c = getc(stdin)))
            ;

        /* Interpret command: */
        if (c == ':')
        {
            switch (getc(stdin))
            {
            case 'q':                 /* Quit interpreter     */
                goto cleanup;

            case 'r':                 /* Reset interpreter    */
                first = true;
                goto init;

            case 'm':                 /* Memory statistics    */
                memStat();
                break;

            case 'n':                 /* List all bound names */
                fputs("Names: ",stdout);
                printSEXPfp(stdout, tlNames, PRT_ESCAPE);
                fputs("\n",stdout);
                break;

            case 'p':                 /* Pop last loaded file */
                if (IS_CONS(tlNames) && IS_CONS(cdr(tlNames)))
                {
                    tlNames = cdr(tlNames);
                    tlVals  = cdr(tlVals);
                }
                else
                    error(ERR_O1_STACK_EMPTY, NIL);

                break;

            case 'l':                 /* Load a file          */
                fgets(fileName, 256, stdin);

                if ((se = strrchr(fileName, '\n')))
                    *se = '\0';

                src_load(fileName);
                break;

            default:
                error(ERR_O9_INVALID_CMD, NIL);
            }
            continue;
        }
        /* Evaluate expression entered: */
        ungetc(c, stdin);
        macroExpand = false;

        S = D = NIL;
        signal(SIGINT, &sig_handler);

        C = compile(readSEXPfp(stdin), NIL);
        E = tlVals;
        numStep = numGC = 0;

        start = clock();
        getc(stdin);                       /* skip newline from input */

        res   = exec();
        stop  = clock();

#ifdef DUMP_STEPS
        fflush(logFile);
#endif
        printSEXPfp(stdout, res, PRT_ESCAPE | PRT_NO_HASHN | PRT_SPACE);
        fputs("\n",stdout);

        fprintf(stderr, "[%.2f seconds, %ld steps, %ld GCs]\n",
                (double)(stop -start) / CLOCKS_PER_SEC,
                numStep, numGC);

    }
cleanup:

#ifdef DUMP_STEPS
    fclose(logFile);
#endif
    return EXIT_SUCCESS;
}

#ifdef MAIN_REPL

int main(int argn, char *argv[]){
    int retc;

    retc = lispme(argn, argv);

    return retc;
}

#endif /*MAIN_REPL*/ 
