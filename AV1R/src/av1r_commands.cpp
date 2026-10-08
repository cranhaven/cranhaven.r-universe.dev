// Vulkan command execution and synchronisation for AV1R
// Адаптировано из ggmlR/src/ggml-vulkan/ggml-vulkan.cpp:
//   - CommandPool:   строки 841-847 (vk_command_pool::init)
//   - CommandBuffer: строки 2162-2181 (ggml_vk_create_cmd_buffer)
//   - QueueSubmit:   строки 2183-2260 (ggml_vk_submit)
//   - Fence:         строки 5029, 1959-1966
//   - Semaphore:     строки 2337-2356
// Compiled only when AV1R_USE_VULKAN is defined

#ifdef AV1R_USE_VULKAN

#include <vulkan/vulkan.h>
#include <stdexcept>
#include <cstdint>
#include "av1r_vulkan_ctx.h"

// ============================================================================
// CommandPool
// Адаптировано из ggmlR строки 841-847 (vk_command_pool::init)
// Флаг TRANSIENT: буферы короткоживущие (сбрасываются после каждого кадра)
// ============================================================================
VkCommandPool av1r_create_command_pool(VkDevice device, uint32_t qfamily)
{
    VkCommandPoolCreateInfo ci{};
    ci.sType            = VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO;
    ci.flags            = VK_COMMAND_POOL_CREATE_TRANSIENT_BIT |
                          VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT;
    ci.queueFamilyIndex = qfamily;

    VkCommandPool pool = VK_NULL_HANDLE;
    VkResult res = vkCreateCommandPool(device, &ci, nullptr, &pool);
    if (res != VK_SUCCESS) {
        throw std::runtime_error("vkCreateCommandPool failed: " + std::to_string(res));
    }
    return pool;
}

// ============================================================================
// CommandBuffer allocation
// Адаптировано из ggmlR строки 2162-2181 (ggml_vk_create_cmd_buffer)
// ============================================================================
VkCommandBuffer av1r_alloc_command_buffer(VkDevice device, VkCommandPool pool)
{
    VkCommandBufferAllocateInfo ai{};
    ai.sType              = VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO;
    ai.commandPool        = pool;
    ai.level              = VK_COMMAND_BUFFER_LEVEL_PRIMARY;
    ai.commandBufferCount = 1;

    VkCommandBuffer cmd = VK_NULL_HANDLE;
    VkResult res = vkAllocateCommandBuffers(device, &ai, &cmd);
    if (res != VK_SUCCESS) {
        throw std::runtime_error("vkAllocateCommandBuffers failed: " + std::to_string(res));
    }
    return cmd;
}

void av1r_begin_command_buffer(VkCommandBuffer cmd)
{
    VkCommandBufferBeginInfo bi{};
    bi.sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO;
    bi.flags = VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT;
    VkResult res = vkBeginCommandBuffer(cmd, &bi);
    if (res != VK_SUCCESS) {
        throw std::runtime_error("vkBeginCommandBuffer failed: " + std::to_string(res));
    }
}

void av1r_end_command_buffer(VkCommandBuffer cmd)
{
    VkResult res = vkEndCommandBuffer(cmd);
    if (res != VK_SUCCESS) {
        throw std::runtime_error("vkEndCommandBuffer failed: " + std::to_string(res));
    }
}

// ============================================================================
// vkQueueSubmit wrapper
// Адаптировано из ggmlR строки 2183-2260 (ggml_vk_submit)
// Поддерживает timeline semaphore wait/signal для синхронизации между
// операциями (decode frame → encode frame → output).
// ============================================================================
void av1r_queue_submit(VkQueue         queue,
                        VkCommandBuffer cmd,
                        VkFence         fence,
                        VkSemaphore     wait_sem,
                        uint64_t        wait_val,
                        VkSemaphore     signal_sem,
                        uint64_t        signal_val)
{
    // Timeline semaphore submit info (ggmlR строки 2234-2241)
    VkTimelineSemaphoreSubmitInfo tl_info{};
    tl_info.sType = VK_STRUCTURE_TYPE_TIMELINE_SEMAPHORE_SUBMIT_INFO;

    VkPipelineStageFlags wait_stage = VK_PIPELINE_STAGE_ALL_COMMANDS_BIT;

    VkSubmitInfo si{};
    si.sType                = VK_STRUCTURE_TYPE_SUBMIT_INFO;
    si.commandBufferCount   = 1;
    si.pCommandBuffers      = &cmd;

    if (wait_sem != VK_NULL_HANDLE) {
        tl_info.waitSemaphoreValueCount   = 1;
        tl_info.pWaitSemaphoreValues      = &wait_val;
        si.waitSemaphoreCount             = 1;
        si.pWaitSemaphores                = &wait_sem;
        si.pWaitDstStageMask              = &wait_stage;
    }
    if (signal_sem != VK_NULL_HANDLE) {
        tl_info.signalSemaphoreValueCount = 1;
        tl_info.pSignalSemaphoreValues    = &signal_val;
        si.signalSemaphoreCount           = 1;
        si.pSignalSemaphores              = &signal_sem;
    }

    if (wait_sem != VK_NULL_HANDLE || signal_sem != VK_NULL_HANDLE) {
        si.pNext = &tl_info;
    }

    // ggmlR строка 2257: queue.submit(submit_infos, fence)
    VkResult res = vkQueueSubmit(queue, 1, &si, fence);
    if (res != VK_SUCCESS) {
        throw std::runtime_error("vkQueueSubmit failed: " + std::to_string(res));
    }
}

// ============================================================================
// Fence
// Адаптировано из ggmlR строки 5029, 1959-1966
// ============================================================================
VkFence av1r_create_fence(VkDevice device)
{
    VkFenceCreateInfo ci{};
    ci.sType = VK_STRUCTURE_TYPE_FENCE_CREATE_INFO;
    // Не SIGNALED — ждём явного сигнала от vkQueueSubmit

    VkFence fence = VK_NULL_HANDLE;
    VkResult res = vkCreateFence(device, &ci, nullptr, &fence);
    if (res != VK_SUCCESS) {
        throw std::runtime_error("vkCreateFence failed: " + std::to_string(res));
    }
    return fence;
}

// Блокирующее ожидание (ggmlR строки 1959-1960: waitForFences + resetFences)
void av1r_wait_fence(VkDevice device, VkFence fence)
{
    VkResult res = vkWaitForFences(device, 1, &fence, VK_TRUE, UINT64_MAX);
    if (res != VK_SUCCESS) {
        throw std::runtime_error("vkWaitForFences failed: " + std::to_string(res));
    }
}

void av1r_reset_fence(VkDevice device, VkFence fence)
{
    vkResetFences(device, 1, &fence);
}

// ============================================================================
// Semaphore
// Адаптировано из ggmlR строки 2337-2356
//   - Binary:   ggml_vk_create_binary_semaphore
//   - Timeline: ggml_vk_create_timeline_semaphore
// ============================================================================
VkSemaphore av1r_create_semaphore_binary(VkDevice device)
{
    // ggmlR строки 2339-2342
    VkSemaphoreTypeCreateInfo tci{};
    tci.sType         = VK_STRUCTURE_TYPE_SEMAPHORE_TYPE_CREATE_INFO;
    tci.semaphoreType = VK_SEMAPHORE_TYPE_BINARY;
    tci.initialValue  = 0;

    VkSemaphoreCreateInfo ci{};
    ci.sType = VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO;
    ci.pNext = &tci;

    VkSemaphore sem = VK_NULL_HANDLE;
    VkResult res = vkCreateSemaphore(device, &ci, nullptr, &sem);
    if (res != VK_SUCCESS) {
        throw std::runtime_error("vkCreateSemaphore (binary) failed: " + std::to_string(res));
    }
    return sem;
}

VkSemaphore av1r_create_semaphore_timeline(VkDevice device)
{
    // ggmlR строки 2350-2353
    VkSemaphoreTypeCreateInfo tci{};
    tci.sType         = VK_STRUCTURE_TYPE_SEMAPHORE_TYPE_CREATE_INFO;
    tci.semaphoreType = VK_SEMAPHORE_TYPE_TIMELINE;
    tci.initialValue  = 0;

    VkSemaphoreCreateInfo ci{};
    ci.sType = VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO;
    ci.pNext = &tci;

    VkSemaphore sem = VK_NULL_HANDLE;
    VkResult res = vkCreateSemaphore(device, &ci, nullptr, &sem);
    if (res != VK_SUCCESS) {
        throw std::runtime_error("vkCreateSemaphore (timeline) failed: " + std::to_string(res));
    }
    return sem;
}

#endif // AV1R_USE_VULKAN
