#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP _BigVAR_ARFitVARXR(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _BigVAR_Eigencomp(SEXP, SEXP, SEXP, SEXP);
extern SEXP _BigVAR_EigencompOO(SEXP, SEXP, SEXP, SEXP);
extern SEXP _BigVAR_FistaElem(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _BigVAR_Fistapar(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _BigVAR_gamloopElem(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _BigVAR_gamloopFista(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _BigVAR_GamLoopGL2(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _BigVAR_GamLoopGLOO(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _BigVAR_gamloopHVAR(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _BigVAR_gamloopOO(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _BigVAR_GamLoopSGL(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _BigVAR_GamLoopSGLDP(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _BigVAR_GamLoopSGLOO(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _BigVAR_GamLoopSGLOODP(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _BigVAR_GamLoopSGLX(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _BigVAR_GamLoopSGLXDP(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _BigVAR_ICX(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _BigVAR_norm2(SEXP);
extern SEXP _BigVAR_powermethod(SEXP, SEXP);
extern SEXP _BigVAR_proxvx2(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _BigVAR_RelaxedLS(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _BigVAR_VARXCons(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_BigVAR_ARFitVARXR",     (DL_FUNC) &_BigVAR_ARFitVARXR,      5},
    {"_BigVAR_Eigencomp",      (DL_FUNC) &_BigVAR_Eigencomp,       4},
    {"_BigVAR_EigencompOO",    (DL_FUNC) &_BigVAR_EigencompOO,     4},
    {"_BigVAR_FistaElem",      (DL_FUNC) &_BigVAR_FistaElem,       8},
    {"_BigVAR_Fistapar",       (DL_FUNC) &_BigVAR_Fistapar,        8},
    {"_BigVAR_gamloopElem",    (DL_FUNC) &_BigVAR_gamloopElem,    10},
    {"_BigVAR_gamloopFista",   (DL_FUNC) &_BigVAR_gamloopFista,   13},
    {"_BigVAR_GamLoopGL2",     (DL_FUNC) &_BigVAR_GamLoopGL2,     16},
    {"_BigVAR_GamLoopGLOO",    (DL_FUNC) &_BigVAR_GamLoopGLOO,    17},
    {"_BigVAR_gamloopHVAR",    (DL_FUNC) &_BigVAR_gamloopHVAR,    10},
    {"_BigVAR_gamloopOO",      (DL_FUNC) &_BigVAR_gamloopOO,      12},
    {"_BigVAR_GamLoopSGL",     (DL_FUNC) &_BigVAR_GamLoopSGL,     17},
    {"_BigVAR_GamLoopSGLDP",   (DL_FUNC) &_BigVAR_GamLoopSGLDP,   17},
    {"_BigVAR_GamLoopSGLOO",   (DL_FUNC) &_BigVAR_GamLoopSGLOO,   17},
    {"_BigVAR_GamLoopSGLOODP", (DL_FUNC) &_BigVAR_GamLoopSGLOODP, 17},
    {"_BigVAR_GamLoopSGLX",    (DL_FUNC) &_BigVAR_GamLoopSGLX,    17},
    {"_BigVAR_GamLoopSGLXDP",  (DL_FUNC) &_BigVAR_GamLoopSGLXDP,  17},
    {"_BigVAR_ICX",            (DL_FUNC) &_BigVAR_ICX,             8},
    {"_BigVAR_norm2",          (DL_FUNC) &_BigVAR_norm2,           1},
    {"_BigVAR_powermethod",    (DL_FUNC) &_BigVAR_powermethod,     2},
    {"_BigVAR_proxvx2",        (DL_FUNC) &_BigVAR_proxvx2,         6},
    {"_BigVAR_RelaxedLS",      (DL_FUNC) &_BigVAR_RelaxedLS,       6},
    {"_BigVAR_VARXCons",       (DL_FUNC) &_BigVAR_VARXCons,        8},
    {NULL, NULL, 0}
};

void R_init_BigVAR(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
