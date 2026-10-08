#pragma once

#ifdef AV1R_USE_VULKAN

#include <vulkan/vulkan.h>
#include <cstdint>
#include <cstddef>
#include <vector>

// ============================================================================
// Структуры данных
// Адаптировано из ggmlR/src/ggml-vulkan/ggml-vulkan.cpp (строки 855-863, 895-898)
// ============================================================================

struct Av1rBuffer {
    VkBuffer            buffer         = VK_NULL_HANDLE;
    VkDeviceMemory      device_memory  = VK_NULL_HANDLE;
    VkMemoryPropertyFlags memory_flags = 0;
    void*               ptr            = nullptr;  // mapped address (host-visible only)
    size_t              size           = 0;
    VkDevice            device         = VK_NULL_HANDLE;
};

struct Av1rSemaphore {
    VkSemaphore s     = VK_NULL_HANDLE;
    uint64_t    value = 0;  // timeline semaphore counter
};

struct Av1rQueue {
    VkQueue      queue              = VK_NULL_HANDLE;
    uint32_t     queue_family_index = UINT32_MAX;
    VkCommandPool cmd_pool          = VK_NULL_HANDLE;
};

// Основной контекст Vulkan для AV1R
struct Av1rVulkanCtx {
    VkInstance       instance    = VK_NULL_HANDLE;
    VkPhysicalDevice physDevice  = VK_NULL_HANDLE;
    VkDevice         device      = VK_NULL_HANDLE;
    Av1rQueue        encodeQueue;
    Av1rQueue        transferQueue;  // for vkCmdCopyBufferToImage (needs TRANSFER bit)
    VkFence          fence       = VK_NULL_HANDLE;
    bool             initialized = false;

    // Garbage collection — семафоры и fence для cleanup
    std::vector<Av1rSemaphore> semaphores;     // binary
    std::vector<Av1rSemaphore> tl_semaphores;  // timeline
};

// ============================================================================
// av1r_init.cpp
// ============================================================================
VkInstance av1r_create_instance();
void       av1r_destroy_instance(VkInstance instance);

// ============================================================================
// av1r_device.cpp
// ============================================================================
int              av1r_device_count(VkInstance instance);
VkPhysicalDevice av1r_select_device(VkInstance instance, int device_index);
bool             av1r_device_supports_av1_encode(VkPhysicalDevice dev);
void             av1r_device_name(VkPhysicalDevice dev, char* buf, int buf_len);

// ============================================================================
// av1r_memory.cpp
// ============================================================================
VkDevice av1r_create_logical_device(VkPhysicalDevice phys,
                                    uint32_t* encode_qfamily_out,
                                    uint32_t* transfer_qfamily_out = nullptr);
void     av1r_destroy_logical_device(VkDevice device);

// Buffer management (адаптировано из ggmlR строки 2402-2503)
Av1rBuffer av1r_buffer_create(VkPhysicalDevice phys, VkDevice device,
                               size_t size, VkBufferUsageFlags usage,
                               VkMemoryPropertyFlags req_flags,
                               VkMemoryPropertyFlags fallback_flags = 0);
void       av1r_buffer_destroy(VkDevice device, Av1rBuffer& buf);

// Staging transfer CPU → GPU  (адаптировано из ggmlR строки 6129-6156)
void av1r_buffer_upload(VkDevice device, VkCommandBuffer cmd,
                         Av1rBuffer& staging, Av1rBuffer& dst,
                         const void* src, size_t size);

// Staging transfer GPU → CPU
void av1r_buffer_download(VkDevice device, VkCommandBuffer cmd,
                           Av1rBuffer& src, Av1rBuffer& staging,
                           size_t size);

// ============================================================================
// av1r_commands.cpp
// ============================================================================
VkCommandPool   av1r_create_command_pool(VkDevice device, uint32_t qfamily);
VkCommandBuffer av1r_alloc_command_buffer(VkDevice device, VkCommandPool pool);
void            av1r_begin_command_buffer(VkCommandBuffer cmd);
void            av1r_end_command_buffer(VkCommandBuffer cmd);

// vkQueueSubmit wrapper (адаптировано из ggmlR строки 2183-2260)
void av1r_queue_submit(VkQueue queue, VkCommandBuffer cmd,
                        VkFence fence,
                        VkSemaphore wait_sem   = VK_NULL_HANDLE,
                        uint64_t    wait_val   = 0,
                        VkSemaphore signal_sem = VK_NULL_HANDLE,
                        uint64_t    signal_val = 0);

// Синхронизация (адаптировано из ggmlR строки 1959-1966, 5029, 2337-2356)
VkFence     av1r_create_fence(VkDevice device);
void        av1r_wait_fence(VkDevice device, VkFence fence);
void        av1r_reset_fence(VkDevice device, VkFence fence);
VkSemaphore av1r_create_semaphore_binary(VkDevice device);
VkSemaphore av1r_create_semaphore_timeline(VkDevice device);

#endif // AV1R_USE_VULKAN
