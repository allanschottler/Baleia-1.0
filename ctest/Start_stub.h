#include "HsFFI.h"
#if defined(__cplusplus)
extern "C" {
#endif
void HsStart();
void HsEnd();
extern HsPtr controlInMeta(void);
extern HsStablePtr startCtx(void);
extern void freeCtx(HsStablePtr a1);
extern HsStablePtr main2(HsStablePtr a1, HsInt a2, HsInt a3, HsPtr a4, HsPtr a5, HsPtr a6, HsPtr a7, HsPtr a8, HsPtr a9);
#if defined(__cplusplus)
}
#endif