// Vulkan memory and buffer management for AV1R
// Адаптировано из ggmlR/src/ggml-vulkan/ggml-vulkan.cpp:
//   - find_memory_properties: строки 2388-2400
//   - create_buffer:          строки 2402-2503
//   - create_logical_device:  строки 4573-4960
// Compiled only when AV1R_USE_VULKAN is defined

#ifdef AV1R_USE_VULKAN

#include <vulkan/vulkan.h>
#include "vk_video/vulkan_video_encode_av1_khr.h"
#include <vector>
#include <stdexcept>
#include <cstring>
#include <cstdio>
#include "av1r_vulkan_ctx.h"

// ============================================================================
// Поиск типа памяти GPU
// Адаптировано из ggmlR строки 2388-2400 (ggml_vk_find_memory_properties)
// ============================================================================
static uint32_t find_memory_type(VkPhysicalDevice phys,
                                  uint32_t         type_bits,
                                  VkMemoryPropertyFlags req_flags)
{
    VkPhysicalDeviceMemoryProperties mem_props{};
    vkGetPhysicalDeviceMemoryProperties(phys, &mem_props);

    for (uint32_t i = 0; i < mem_props.memoryTypeCount; i++) {
        if ((type_bits & (1u << i)) &&
            (mem_props.memoryTypes[i].propertyFlags & req_flags) == req_flags) {
            return i;
        }
    }
    return UINT32_MAX;
}

// ============================================================================
// Поиск encode queue family
// ============================================================================
static uint32_t find_encode_queue_family(VkPhysicalDevice phys)
{
    uint32_t count = 0;
    vkGetPhysicalDeviceQueueFamilyProperties(phys, &count, nullptr);
    std::vector<VkQueueFamilyProperties> props(count);
    vkGetPhysicalDeviceQueueFamilyProperties(phys, &count, props.data());

    for (uint32_t i = 0; i < count; i++) {
        if (props[i].queueFlags & VK_QUEUE_VIDEO_ENCODE_BIT_KHR) {
            return i;
        }
    }
    return UINT32_MAX;
}

// ============================================================================
// Logical device + encode queue
// Адаптировано из ggmlR строки 4573-4960 (ggml_vk_get_device)
// ============================================================================
// Find a queue family with TRANSFER support (for vkCmdCopyBufferToImage)
static uint32_t find_transfer_queue_family(VkPhysicalDevice phys)
{
    uint32_t count = 0;
    vkGetPhysicalDeviceQueueFamilyProperties(phys, &count, nullptr);
    std::vector<VkQueueFamilyProperties> props(count);
    vkGetPhysicalDeviceQueueFamilyProperties(phys, &count, props.data());

    // Prefer a queue with GRAPHICS|TRANSFER (family 0 typically)
    for (uint32_t i = 0; i < count; i++) {
        if ((props[i].queueFlags & VK_QUEUE_TRANSFER_BIT) &&
            (props[i].queueFlags & VK_QUEUE_GRAPHICS_BIT)) {
            return i;
        }
    }
    // Fallback: any queue with TRANSFER
    for (uint32_t i = 0; i < count; i++) {
        if (props[i].queueFlags & VK_QUEUE_TRANSFER_BIT) {
            return i;
        }
    }
    return UINT32_MAX;
}

VkDevice av1r_create_logical_device(VkPhysicalDevice phys,
                                     uint32_t* encode_qfamily_out,
                                     uint32_t* transfer_qfamily_out)
{
    uint32_t encFamily = find_encode_queue_family(phys);
    if (encFamily == UINT32_MAX) {
        throw std::runtime_error("No VIDEO_ENCODE queue family on this GPU");
    }
    if (encode_qfamily_out) *encode_qfamily_out = encFamily;

    uint32_t xferFamily = find_transfer_queue_family(phys);
    if (xferFamily == UINT32_MAX) {
        throw std::runtime_error("No TRANSFER queue family on this GPU");
    }
    if (transfer_qfamily_out) *transfer_qfamily_out = xferFamily;

    float priority = 1.0f;
    std::vector<VkDeviceQueueCreateInfo> qcis;

    VkDeviceQueueCreateInfo encQci{};
    encQci.sType            = VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO;
    encQci.queueFamilyIndex = encFamily;
    encQci.queueCount       = 1;
    encQci.pQueuePriorities = &priority;
    qcis.push_back(encQci);

    if (xferFamily != encFamily) {
        VkDeviceQueueCreateInfo xferQci{};
        xferQci.sType            = VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO;
        xferQci.queueFamilyIndex = xferFamily;
        xferQci.queueCount       = 1;
        xferQci.pQueuePriorities = &priority;
        qcis.push_back(xferQci);
    }

    // Расширения для AV1 video encode
    std::vector<const char*> dev_exts = {
        VK_KHR_VIDEO_QUEUE_EXTENSION_NAME,
        VK_KHR_VIDEO_ENCODE_QUEUE_EXTENSION_NAME,
#ifdef AV1R_VULKAN_VIDEO_AV1
        VK_KHR_VIDEO_ENCODE_AV1_EXTENSION_NAME,
#endif
    };

    VkDeviceCreateInfo dci{};
    dci.sType                   = VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO;
    dci.queueCreateInfoCount    = static_cast<uint32_t>(qcis.size());
    dci.pQueueCreateInfos       = qcis.data();
    dci.enabledExtensionCount   = static_cast<uint32_t>(dev_exts.size());
    dci.ppEnabledExtensionNames = dev_exts.data();

    VkDevice device = VK_NULL_HANDLE;
    VkResult res = vkCreateDevice(phys, &dci, nullptr, &device);
    if (res != VK_SUCCESS) {
        throw std::runtime_error("vkCreateDevice failed: " + std::to_string(res));
    }
    return device;
}

void av1r_destroy_logical_device(VkDevice device)
{
    if (device != VK_NULL_HANDLE) {
        vkDestroyDevice(device, nullptr);
    }
}

// ============================================================================
// Buffer creation
// Адаптировано из ggmlR строки 2402-2503 (ggml_vk_create_buffer)
// Логика: создать VkBuffer → получить требования памяти → найти тип →
//         allocateMemory → bindBufferMemory → mapMemory если host-visible
// ============================================================================
Av1rBuffer av1r_buffer_create(VkPhysicalDevice      phys,
                               VkDevice              device,
                               size_t                size,
                               VkBufferUsageFlags    usage,
                               VkMemoryPropertyFlags req_flags,
                               VkMemoryPropertyFlags fallback_flags)
{
    Av1rBuffer buf{};
    buf.device = device;
    buf.size   = size;

    VkBufferCreateInfo bci{};
    bci.sType       = VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO;
    bci.size        = size;
    bci.usage       = usage;
    bci.sharingMode = VK_SHARING_MODE_EXCLUSIVE;

    VkResult res = vkCreateBuffer(device, &bci, nullptr, &buf.buffer);
    if (res != VK_SUCCESS) {
        throw std::runtime_error("vkCreateBuffer failed: " + std::to_string(res));
    }

    VkMemoryRequirements mem_req{};
    vkGetBufferMemoryRequirements(device, buf.buffer, &mem_req);

    // Сначала пробуем req_flags, потом fallback (как в ggmlR строка 2445-2475)
    uint32_t mem_type = find_memory_type(phys, mem_req.memoryTypeBits, req_flags);
    if (mem_type == UINT32_MAX && fallback_flags != 0) {
        mem_type = find_memory_type(phys, mem_req.memoryTypeBits, fallback_flags);
        if (mem_type != UINT32_MAX) {
            buf.memory_flags = fallback_flags;
        }
    } else {
        buf.memory_flags = req_flags;
    }

    if (mem_type == UINT32_MAX) {
        vkDestroyBuffer(device, buf.buffer, nullptr);
        throw std::runtime_error("No suitable memory type for buffer");
    }

    VkMemoryAllocateInfo alloc_info{};
    alloc_info.sType           = VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO;
    alloc_info.allocationSize  = mem_req.size;
    alloc_info.memoryTypeIndex = mem_type;

    res = vkAllocateMemory(device, &alloc_info, nullptr, &buf.device_memory);
    if (res != VK_SUCCESS) {
        vkDestroyBuffer(device, buf.buffer, nullptr);
        throw std::runtime_error("vkAllocateMemory failed: " + std::to_string(res));
    }

    vkBindBufferMemory(device, buf.buffer, buf.device_memory, 0);

    // Map if host-visible (ggmlR строка 2484-2486)
    if (buf.memory_flags & VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT) {
        vkMapMemory(device, buf.device_memory, 0, VK_WHOLE_SIZE, 0, &buf.ptr);
    }

    return buf;
}

void av1r_buffer_destroy(VkDevice device, Av1rBuffer& buf)
{
    if (buf.ptr != nullptr) {
        vkUnmapMemory(device, buf.device_memory);
        buf.ptr = nullptr;
    }
    if (buf.device_memory != VK_NULL_HANDLE) {
        vkFreeMemory(device, buf.device_memory, nullptr);
        buf.device_memory = VK_NULL_HANDLE;
    }
    if (buf.buffer != VK_NULL_HANDLE) {
        vkDestroyBuffer(device, buf.buffer, nullptr);
        buf.buffer = VK_NULL_HANDLE;
    }
}

// ============================================================================
// Staging transfer: CPU → GPU
// Адаптировано из ggmlR строки 6129-6135 (sync staging buffer copy)
// Паттерн: данные → host-visible staging → vkCmdCopyBuffer → device-local dst
// ============================================================================
void av1r_buffer_upload(VkDevice       device,
                         VkCommandBuffer cmd,
                         Av1rBuffer&    staging,
                         Av1rBuffer&    dst,
                         const void*    src,
                         size_t         size)
{
    if (staging.ptr == nullptr) {
        throw std::runtime_error("Staging buffer is not host-visible");
    }
    memcpy(staging.ptr, src, size);

    // Flush host writes (если не coherent)
    if (!(staging.memory_flags & VK_MEMORY_PROPERTY_HOST_COHERENT_BIT)) {
        VkMappedMemoryRange range{};
        range.sType  = VK_STRUCTURE_TYPE_MAPPED_MEMORY_RANGE;
        range.memory = staging.device_memory;
        range.offset = 0;
        range.size   = VK_WHOLE_SIZE;
        vkFlushMappedMemoryRanges(device, 1, &range);
    }

    // vkCmdCopyBuffer (ggmlR строка 6135)
    VkBufferCopy copy{ 0, 0, size };
    vkCmdCopyBuffer(cmd, staging.buffer, dst.buffer, 1, &copy);
}

// ============================================================================
// Staging transfer: GPU → CPU
// ============================================================================
void av1r_buffer_download(VkDevice        device,
                           VkCommandBuffer cmd,
                           Av1rBuffer&     src,
                           Av1rBuffer&     staging,
                           size_t          size)
{
    VkBufferCopy copy{ 0, 0, size };
    vkCmdCopyBuffer(cmd, src.buffer, staging.buffer, 1, &copy);

    // Invalidate после завершения (вызывается после fence wait)
    if (!(staging.memory_flags & VK_MEMORY_PROPERTY_HOST_COHERENT_BIT)) {
        VkMappedMemoryRange range{};
        range.sType  = VK_STRUCTURE_TYPE_MAPPED_MEMORY_RANGE;
        range.memory = staging.device_memory;
        range.offset = 0;
        range.size   = VK_WHOLE_SIZE;
        vkInvalidateMappedMemoryRanges(device, 1, &range);
    }
    (void)device;
}

#endif // AV1R_USE_VULKAN
