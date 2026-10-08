// Opaque streaming encoder handle for frame-by-frame Vulkan AV1 encoding.
// Full definition lives in av1r_encode_vulkan.cpp; bindings use only pointers.

#ifndef AV1R_STREAM_ENCODER_H
#define AV1R_STREAM_ENCODER_H

#ifdef AV1R_VULKAN_VIDEO_AV1

#include <vector>
#include <cstdint>

struct Av1rVulkanCtx;
struct Av1rStreamEncoder;

Av1rStreamEncoder* av1r_vulkan_stream_new();
void av1r_vulkan_stream_init(Av1rVulkanCtx& ctx, Av1rStreamEncoder* se,
                              int width, int height, int fps, int crf);
void av1r_vulkan_stream_encode(Av1rStreamEncoder* se, const uint8_t* frame_nv12,
                                int frame_index, std::vector<uint8_t>& out_packet);
void av1r_vulkan_stream_finish(Av1rStreamEncoder* se);
void av1r_vulkan_stream_delete(Av1rStreamEncoder* se);

#endif
#endif
