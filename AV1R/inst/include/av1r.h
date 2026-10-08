#pragma once

#ifdef __cplusplus
extern "C" {
#endif

/* AV1R public C API */

typedef enum {
    AV1R_BACKEND_AUTO   = 0,  /* auto-detect: GPU if available, else CPU */
    AV1R_BACKEND_CPU    = 1,  /* FFmpeg + SVT-AV1 */
    AV1R_BACKEND_VULKAN = 2   /* VK_KHR_VIDEO_ENCODE_AV1 */
} av1r_backend_t;

typedef struct {
    int   crf;         /* 0-63, default 28 */
    int   preset;      /* 0 (slowest/best) to 13 (fastest), default 8 */
    int   threads;     /* 0 = auto */
    av1r_backend_t backend;
} av1r_options_t;

/* Default options */
static inline av1r_options_t av1r_default_options(void) {
    av1r_options_t o;
    o.crf     = 28;
    o.preset  = 8;
    o.threads = 0;
    o.backend = AV1R_BACKEND_AUTO;
    return o;
}

#ifdef __cplusplus
}
#endif
