// GPU selection and capability check for AV1R
// Адаптировано из ggmlR/src/ggml-vulkan/ggml-vulkan.cpp:
//   - enumeratePhysicalDevices: строки 4353-4360
//   - discrete GPU приоритет:   строка 4532 (device->uma = eIntegratedGpu)
//   - vendor_id AMD/Nvidia:      строка 4492-4493
// Compiled only when AV1R_USE_VULKAN is defined

#ifdef AV1R_USE_VULKAN

#include <vulkan/vulkan.h>
#include "vk_video/vulkan_video_encode_av1_khr.h"
#include "av1r_stderr_suppress.h"
#include <cstring>
#include <vector>
#include <stdexcept>
#include "av1r_vulkan_ctx.h"

int av1r_device_count(VkInstance instance)
{
    uint32_t count = 0;
    StderrSuppressor quiet;
    vkEnumeratePhysicalDevices(instance, &count, nullptr);
    return static_cast<int>(count);
}

// ============================================================================
// Device selection с приоритетом discrete GPU
// Адаптировано из ggmlR строк 4353-4360, 4532
// Если device_index == -1: автоматически выбирает лучший GPU
//   приоритет: discrete > integrated > virtual/cpu
// Если device_index >= 0: выбирает по явному индексу
// ============================================================================
VkPhysicalDevice av1r_select_device(VkInstance instance, int device_index)
{
    uint32_t count = 0;
    {
        StderrSuppressor quiet;
        vkEnumeratePhysicalDevices(instance, &count, nullptr);
    }
    if (count == 0) {
        throw std::runtime_error("No Vulkan-capable GPUs found");
    }

    std::vector<VkPhysicalDevice> devs(count);
    {
        StderrSuppressor quiet;
        vkEnumeratePhysicalDevices(instance, &count, devs.data());
    }

    if (device_index >= 0) {
        // Явный выбор по индексу (ggmlR строки 4355-4358)
        if (device_index >= static_cast<int>(count)) {
            throw std::runtime_error("Device index " + std::to_string(device_index) +
                                     " does not exist (count=" + std::to_string(count) + ")");
        }
        return devs[device_index];
    }

    // Авто-выбор: приоритет discrete → integrated → остальные
    // Адаптировано из ggmlR строки 4532: device->uma = eIntegratedGpu
    // и логики auto-select из vulkan backend init
    VkPhysicalDevice best = VK_NULL_HANDLE;
    int best_score = -1;

    for (auto& dev : devs) {
        VkPhysicalDeviceProperties props{};
        vkGetPhysicalDeviceProperties(dev, &props);

        int score = 0;
        switch (props.deviceType) {
            case VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU:   score = 3; break; // AMD/Nvidia dedicated
            case VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU: score = 2; break; // Intel/AMD iGPU
            case VK_PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU:    score = 1; break;
            default:                                      score = 0; break;
        }

        // Бонус за поддержку AV1 encode (ggmlR паттерн: проверка расширений)
        if (av1r_device_supports_av1_encode(dev)) {
            score += 10;
        }

        if (score > best_score) {
            best_score = score;
            best = dev;
        }
    }

    return best;
}

// ============================================================================
// Проверка поддержки VK_KHR_VIDEO_ENCODE_AV1
// Адаптировано из ggmlR строки 4389-4411 (цикл по ext_props)
// ============================================================================
bool av1r_device_supports_av1_encode(VkPhysicalDevice dev)
{
#ifdef AV1R_VULKAN_VIDEO_AV1
    uint32_t ext_count = 0;
    vkEnumerateDeviceExtensionProperties(dev, nullptr, &ext_count, nullptr);
    std::vector<VkExtensionProperties> exts(ext_count);
    vkEnumerateDeviceExtensionProperties(dev, nullptr, &ext_count, exts.data());

    bool has_video_queue  = false;
    bool has_encode_queue = false;
    bool has_encode_av1   = false;

    for (const auto& e : exts) {
        if (strcmp(e.extensionName, VK_KHR_VIDEO_QUEUE_EXTENSION_NAME)        == 0) has_video_queue  = true;
        if (strcmp(e.extensionName, VK_KHR_VIDEO_ENCODE_QUEUE_EXTENSION_NAME) == 0) has_encode_queue = true;
        if (strcmp(e.extensionName, VK_KHR_VIDEO_ENCODE_AV1_EXTENSION_NAME)   == 0) has_encode_av1   = true;
    }

    return has_video_queue && has_encode_queue && has_encode_av1;
#else
    (void)dev;
    return false;
#endif
}

void av1r_device_name(VkPhysicalDevice dev, char* buf, int buf_len)
{
    VkPhysicalDeviceProperties props{};
    vkGetPhysicalDeviceProperties(dev, &props);
    strncpy(buf, props.deviceName, static_cast<size_t>(buf_len - 1));
    buf[buf_len - 1] = '\0';
}

#endif // AV1R_USE_VULKAN
