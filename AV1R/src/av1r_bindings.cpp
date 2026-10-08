// R ↔ C++ bindings for AV1R
// CPU encoding: ffmpeg вызывается через system() в R-коде (нет линковки с libavcodec)
// GPU encoding: Vulkan через этот файл

#include <cstring>
#include <cstdio>
#include <stdexcept>
#include <string>
#include <vector>

// R headers must come after C++ STL to avoid macro conflicts.
// Rinternals.h defines length(x) as Rf_length(x) which clashes with
// std::locale::length() on macOS/clang.
#include <R.h>
#include <Rinternals.h>
#ifdef length
#  undef length
#endif

#include "../inst/include/av1r.h"

#ifdef AV1R_USE_VULKAN
#include "av1r_vulkan_ctx.h"

// Forward declarations
VkInstance       av1r_create_instance();
void             av1r_destroy_instance(VkInstance);
int              av1r_device_count(VkInstance);
VkPhysicalDevice av1r_select_device(VkInstance, int);
bool             av1r_device_supports_av1_encode(VkPhysicalDevice);
void             av1r_device_name(VkPhysicalDevice, char*, int);
VkDevice         av1r_create_logical_device(VkPhysicalDevice, uint32_t*, uint32_t*);
void             av1r_destroy_logical_device(VkDevice);
VkFence          av1r_create_fence(VkDevice);
VkCommandPool    av1r_create_command_pool(VkDevice, uint32_t);
#endif

// ============================================================================
// R_av1r_vulkan_available  →  logical(1)
// ============================================================================
extern "C" SEXP R_av1r_vulkan_available(void) {
#ifdef AV1R_VULKAN_VIDEO_AV1
    return Rf_ScalarLogical(TRUE);
#else
    return Rf_ScalarLogical(FALSE);
#endif
}

// ============================================================================
// R_av1r_vulkan_devices  →  character vector
// ============================================================================
extern "C" SEXP R_av1r_vulkan_devices(void) {
#ifdef AV1R_USE_VULKAN
    try {
        VkInstance inst = av1r_create_instance();
        int n = av1r_device_count(inst);
        SEXP result = PROTECT(Rf_allocVector(STRSXP, n));
        for (int i = 0; i < n; i++) {
            VkPhysicalDevice dev = av1r_select_device(inst, i);
            char name[256];
            av1r_device_name(dev, name, sizeof(name));
            bool av1_ok = av1r_device_supports_av1_encode(dev);
            std::string label = std::string(name) + (av1_ok ? " [AV1]" : "");
            SET_STRING_ELT(result, i, Rf_mkChar(label.c_str()));
        }
        UNPROTECT(1);
        av1r_destroy_instance(inst);
        return result;
    } catch (const std::exception& e) {
        Rf_warning("vulkan_devices: %s", e.what());
        return Rf_allocVector(STRSXP, 0);
    }
#else
    return Rf_allocVector(STRSXP, 0);
#endif
}

// ============================================================================
// R_av1r_detect_backend(prefer)  →  character(1): "vulkan" | "cpu"
// ============================================================================
extern "C" SEXP R_av1r_detect_backend(SEXP prefer) {
    const char* pref = CHAR(STRING_ELT(prefer, 0));
    if (strcmp(pref, "cpu") == 0) {
        return Rf_mkString("cpu");
    }
#ifdef AV1R_VULKAN_VIDEO_AV1
    try {
        VkInstance inst = av1r_create_instance();
        int n = av1r_device_count(inst);
        for (int i = 0; i < n; i++) {
            VkPhysicalDevice dev = av1r_select_device(inst, i);
            if (av1r_device_supports_av1_encode(dev)) {
                av1r_destroy_instance(inst);
                return Rf_mkString("vulkan");
            }
        }
        av1r_destroy_instance(inst);
    } catch (...) {}
#endif
    return Rf_mkString("cpu");
}

// ============================================================================
// R_av1r_vulkan_encode(input, output, width, height, fps, crf)
// ffmpeg декодирует input в NV12 через pipe → C++ encode → IVF файл
// ============================================================================
#ifdef AV1R_VULKAN_VIDEO_AV1

#include "av1r_stream_encoder.h"

// Query minimum encode resolution (from av1r_encode_vulkan.cpp)
void av1r_vulkan_query_min_extent(VkInstance, VkPhysicalDevice, uint32_t*, uint32_t*);

// Minimal IVF muxer (AV1 raw bitstream → IVF container readable by ffmpeg)
static void write_ivf_header(FILE* f, int width, int height, int fps, int n_frames) {
    uint8_t hdr[32] = {};
    hdr[0]='D'; hdr[1]='K'; hdr[2]='I'; hdr[3]='F';
    // version=0, header_size=32
    hdr[4]=0; hdr[5]=0; hdr[6]=32; hdr[7]=0;
    // fourcc AV01
    hdr[8]='A'; hdr[9]='V'; hdr[10]='0'; hdr[11]='1';
    hdr[12]= width       & 0xFF; hdr[13]= (width  >>8)& 0xFF;
    hdr[14]= height      & 0xFF; hdr[15]= (height >>8)& 0xFF;
    hdr[16]= fps         & 0xFF; hdr[17]= (fps    >>8)& 0xFF; hdr[18]=0; hdr[19]=0;
    hdr[20]=1; hdr[21]=0; hdr[22]=0; hdr[23]=0; // timescale denominator
    hdr[24]= n_frames    & 0xFF; hdr[25]=(n_frames>>8)&0xFF;
    hdr[26]=(n_frames>>16)&0xFF; hdr[27]=(n_frames>>24)&0xFF;
    fwrite(hdr, 1, 32, f);
}

static void write_ivf_frame(FILE* f, const uint8_t* data, size_t size, uint64_t pts) {
    uint8_t fhdr[12] = {};
    fhdr[0]= size     & 0xFF; fhdr[1]=(size>> 8)&0xFF;
    fhdr[2]=(size>>16)&0xFF;  fhdr[3]=(size>>24)&0xFF;
    for (int i = 0; i < 8; i++) fhdr[4+i] = (pts >> (8*i)) & 0xFF;
    fwrite(fhdr, 1, 12, f);
    fwrite(data, 1, size, f);
}

extern "C" SEXP R_av1r_vulkan_encode(SEXP r_input, SEXP r_output,
                                      SEXP r_width, SEXP r_height,
                                      SEXP r_fps,   SEXP r_crf) {
    const char* input  = CHAR(STRING_ELT(r_input,  0));
    const char* output = CHAR(STRING_ELT(r_output, 0));
    int width  = INTEGER(r_width)[0];
    int height = INTEGER(r_height)[0];
    int fps    = INTEGER(r_fps)[0];
    int crf    = INTEGER(r_crf)[0];

    // Align to even (NV12 requirement)
    width  = width  & ~1;
    height = height & ~1;

    // Init Vulkan first — we need physDevice to query min encode extent
    Av1rVulkanCtx ctx{};
    try {
        ctx.instance   = av1r_create_instance();
        ctx.physDevice = av1r_select_device(ctx.instance, 0);
        uint32_t encQfam  = UINT32_MAX;
        uint32_t xferQfam = UINT32_MAX;
        ctx.device     = av1r_create_logical_device(ctx.physDevice, &encQfam, &xferQfam);
        ctx.encodeQueue.queue_family_index = encQfam;
        vkGetDeviceQueue(ctx.device, encQfam, 0, &ctx.encodeQueue.queue);
        ctx.transferQueue.queue_family_index = xferQfam;
        vkGetDeviceQueue(ctx.device, xferQfam, 0, &ctx.transferQueue.queue);
        ctx.initialized = true;
    } catch (const std::exception& e) {
        Rf_error("Vulkan init failed: %s", e.what());
    }

    // Query minimum encode resolution and scale up if needed
    uint32_t minW = 0, minH = 0;
    av1r_vulkan_query_min_extent(ctx.instance, ctx.physDevice, &minW, &minH);
    if ((uint32_t)width  < minW) width  = (int)minW;
    if ((uint32_t)height < minH) height = (int)minH;
    // Re-align after possible adjustment
    width  = width  & ~1;
    height = height & ~1;

    size_t frame_bytes = static_cast<size_t>(width * height * 3 / 2);

    // ffmpeg pipe: decode to raw NV12
    // Image sequences (printf pattern with %) and TIFF need -framerate before -i
    std::string inp(input);
    bool is_image_seq = (inp.find('%') != std::string::npos) ||
        (inp.size() >= 4 && (inp.compare(inp.size()-4, 4, ".tif") == 0 ||
                              inp.compare(inp.size()-5, 5, ".tiff") == 0));

    std::string cmd = "ffmpeg";
    if (is_image_seq) cmd += " -framerate " + std::to_string(fps);
    cmd += " -i \"" + inp + "\""
           " -f rawvideo -pix_fmt nv12"
           " -vf scale=" + std::to_string(width) + ":" + std::to_string(height) +
           " -an - 2>/dev/null";

    FILE* pipe = popen(cmd.c_str(), "r");
    if (!pipe) {
        av1r_destroy_logical_device(ctx.device);
        av1r_destroy_instance(ctx.instance);
        Rf_error("Failed to open ffmpeg pipe");
    }

    // Init streaming encoder
    Av1rStreamEncoder* se = av1r_vulkan_stream_new();
    try {
        av1r_vulkan_stream_init(ctx, se, width, height, fps, crf);
    } catch (const std::exception& e) {
        av1r_vulkan_stream_delete(se);
        pclose(pipe);
        av1r_destroy_logical_device(ctx.device);
        av1r_destroy_instance(ctx.instance);
        Rf_error("Vulkan encoder init failed: %s", e.what());
    }

    // Open IVF output (write header with 0 frames, update later)
    std::string ivf_tmp = std::string(output) + ".ivf";
    FILE* fout = fopen(ivf_tmp.c_str(), "wb");
    if (!fout) {
        av1r_vulkan_stream_finish(se);
        av1r_vulkan_stream_delete(se);
        pclose(pipe);
        av1r_destroy_logical_device(ctx.device);
        av1r_destroy_instance(ctx.instance);
        Rf_error("Cannot write output IVF: %s", ivf_tmp.c_str());
    }
    write_ivf_header(fout, width, height, fps, 0);

    // Stream: read one frame → encode → write IVF packet → repeat
    std::vector<uint8_t> frame_buf(frame_bytes);
    std::vector<uint8_t> packet;
    int n_frames = 0;
    bool encode_error = false;
    std::string error_msg;

    while (true) {
        size_t got = fread(frame_buf.data(), 1, frame_bytes, pipe);
        if (got != frame_bytes) break;

        try {
            av1r_vulkan_stream_encode(se, frame_buf.data(), n_frames, packet);
        } catch (const std::exception& e) {
            encode_error = true;
            error_msg = e.what();
            break;
        }

        write_ivf_frame(fout, packet.data(), packet.size(),
                        static_cast<uint64_t>(n_frames));
        n_frames++;

        if (n_frames % 100 == 0)
            REprintf("\r  [vulkan] %d frames encoded", n_frames);
    }
    if (n_frames > 0) REprintf("\r  [vulkan] %d frames encoded\n", n_frames);

    pclose(pipe);
    av1r_vulkan_stream_finish(se);
    av1r_vulkan_stream_delete(se);

    // Update IVF header with actual frame count
    fseek(fout, 24, SEEK_SET);
    uint8_t fc[4] = {
        static_cast<uint8_t>(n_frames & 0xFF),
        static_cast<uint8_t>((n_frames >> 8) & 0xFF),
        static_cast<uint8_t>((n_frames >> 16) & 0xFF),
        static_cast<uint8_t>((n_frames >> 24) & 0xFF)
    };
    fwrite(fc, 1, 4, fout);
    fclose(fout);

    vkDeviceWaitIdle(ctx.device);
    av1r_destroy_logical_device(ctx.device);
    av1r_destroy_instance(ctx.instance);

    if (encode_error) {
        remove(ivf_tmp.c_str());
        Rf_error("Vulkan encode failed: %s", error_msg.c_str());
    }

    if (n_frames == 0) {
        remove(ivf_tmp.c_str());
        Rf_error("No frames decoded from input");
    }

    // Wrap IVF → MP4 via ffmpeg
    std::string wrap_cmd = std::string("ffmpeg -y -i \"") + ivf_tmp +
        "\" -i \"" + input + "\" -map 0:v -map 1:a? -c:v copy -c:a copy"
        " -movflags +faststart \"" +
        output + "\" 2>/dev/null";
    int ret = system(wrap_cmd.c_str());
    remove(ivf_tmp.c_str());

    if (ret != 0)
        Rf_error("ffmpeg mux failed (exit %d)", ret);

    return Rf_ScalarInteger(0);
}
#endif // AV1R_VULKAN_VIDEO_AV1

// ============================================================================
// Registration table
// ============================================================================
static const R_CallMethodDef CallEntries[] = {
    { "R_av1r_vulkan_available", (DL_FUNC) &R_av1r_vulkan_available, 0 },
    { "R_av1r_vulkan_devices",   (DL_FUNC) &R_av1r_vulkan_devices,   0 },
    { "R_av1r_detect_backend",   (DL_FUNC) &R_av1r_detect_backend,   1 },
#ifdef AV1R_VULKAN_VIDEO_AV1
    { "R_av1r_vulkan_encode",    (DL_FUNC) &R_av1r_vulkan_encode,    6 },
#endif
    { nullptr, nullptr, 0 }
};

extern "C" void R_init_AV1R(DllInfo* dll) {
    R_registerRoutines(dll, nullptr, CallEntries, nullptr, nullptr);
    R_useDynamicSymbols(dll, FALSE);
}
