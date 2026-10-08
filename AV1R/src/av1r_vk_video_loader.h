// Dynamic loader for Vulkan Video KHR extension functions.
// These functions are not exported by libvulkan.so in SDK < 1.3.290,
// so we load them at runtime via vkGetInstanceProcAddr / vkGetDeviceProcAddr.

#ifndef AV1R_VK_VIDEO_LOADER_H
#define AV1R_VK_VIDEO_LOADER_H

#include <vulkan/vulkan.h>
#include "vk_video/vulkan_video_encode_av1_khr.h"
#include <stdexcept>
#include <string>

// Function pointer types (PFN_vk*)
using PFN_vkGetPhysicalDeviceVideoCapabilitiesKHR =
    VkResult (VKAPI_PTR *)(VkPhysicalDevice, const VkVideoProfileInfoKHR*, VkVideoCapabilitiesKHR*);
using PFN_vkGetPhysicalDeviceVideoFormatPropertiesKHR =
    VkResult (VKAPI_PTR *)(VkPhysicalDevice, const VkPhysicalDeviceVideoFormatInfoKHR*, uint32_t*, VkVideoFormatPropertiesKHR*);
using PFN_vkCreateVideoSessionKHR =
    VkResult (VKAPI_PTR *)(VkDevice, const VkVideoSessionCreateInfoKHR*, const VkAllocationCallbacks*, VkVideoSessionKHR*);
using PFN_vkDestroyVideoSessionKHR =
    void (VKAPI_PTR *)(VkDevice, VkVideoSessionKHR, const VkAllocationCallbacks*);
using PFN_vkGetVideoSessionMemoryRequirementsKHR =
    VkResult (VKAPI_PTR *)(VkDevice, VkVideoSessionKHR, uint32_t*, VkVideoSessionMemoryRequirementsKHR*);
using PFN_vkBindVideoSessionMemoryKHR =
    VkResult (VKAPI_PTR *)(VkDevice, VkVideoSessionKHR, uint32_t, const VkBindVideoSessionMemoryInfoKHR*);
using PFN_vkCreateVideoSessionParametersKHR =
    VkResult (VKAPI_PTR *)(VkDevice, const VkVideoSessionParametersCreateInfoKHR*, const VkAllocationCallbacks*, VkVideoSessionParametersKHR*);
using PFN_vkDestroyVideoSessionParametersKHR =
    void (VKAPI_PTR *)(VkDevice, VkVideoSessionParametersKHR, const VkAllocationCallbacks*);
using PFN_vkCmdBeginVideoCodingKHR =
    void (VKAPI_PTR *)(VkCommandBuffer, const VkVideoBeginCodingInfoKHR*);
using PFN_vkCmdEndVideoCodingKHR =
    void (VKAPI_PTR *)(VkCommandBuffer, const VkVideoEndCodingInfoKHR*);
using PFN_vkCmdControlVideoCodingKHR =
    void (VKAPI_PTR *)(VkCommandBuffer, const VkVideoCodingControlInfoKHR*);
using PFN_vkCmdEncodeVideoKHR =
    void (VKAPI_PTR *)(VkCommandBuffer, const VkVideoEncodeInfoKHR*);
using PFN_vkGetEncodedVideoSessionParametersKHR =
    VkResult (VKAPI_PTR *)(VkDevice, const VkVideoEncodeSessionParametersGetInfoKHR*,
                            VkVideoEncodeSessionParametersFeedbackInfoKHR*, size_t*, void*);

// Global function pointers
struct Av1rVkVideoFuncs {
    PFN_vkGetPhysicalDeviceVideoCapabilitiesKHR      GetPhysDevVideoCapabilities;
    PFN_vkGetPhysicalDeviceVideoFormatPropertiesKHR  GetPhysDevVideoFormatProperties;
    PFN_vkCreateVideoSessionKHR                      CreateVideoSession;
    PFN_vkDestroyVideoSessionKHR                     DestroyVideoSession;
    PFN_vkGetVideoSessionMemoryRequirementsKHR       GetVideoSessionMemoryRequirements;
    PFN_vkBindVideoSessionMemoryKHR                  BindVideoSessionMemory;
    PFN_vkCreateVideoSessionParametersKHR            CreateVideoSessionParameters;
    PFN_vkDestroyVideoSessionParametersKHR           DestroyVideoSessionParameters;
    PFN_vkCmdBeginVideoCodingKHR                     CmdBeginVideoCoding;
    PFN_vkCmdEndVideoCodingKHR                       CmdEndVideoCoding;
    PFN_vkCmdControlVideoCodingKHR                   CmdControlVideoCoding;
    PFN_vkCmdEncodeVideoKHR                          CmdEncodeVideo;
    PFN_vkGetEncodedVideoSessionParametersKHR        GetEncodedSessionParams;
    bool loaded = false;
};

inline Av1rVkVideoFuncs& av1r_vk_video_funcs() {
    static Av1rVkVideoFuncs f{};
    return f;
}

inline void av1r_load_vk_video_funcs(VkInstance instance, VkDevice device) {
    auto& f = av1r_vk_video_funcs();

    auto getI = [&](const char* name) -> PFN_vkVoidFunction {
        auto p = vkGetInstanceProcAddr(instance, name);
        if (!p) throw std::runtime_error(std::string("Failed to load ") + name);
        return p;
    };
    auto getD = [&](const char* name) -> PFN_vkVoidFunction {
        auto p = vkGetDeviceProcAddr(device, name);
        if (!p) throw std::runtime_error(std::string("Failed to load ") + name);
        return p;
    };

    // Instance-level
    f.GetPhysDevVideoCapabilities     = (PFN_vkGetPhysicalDeviceVideoCapabilitiesKHR)     getI("vkGetPhysicalDeviceVideoCapabilitiesKHR");
    f.GetPhysDevVideoFormatProperties = (PFN_vkGetPhysicalDeviceVideoFormatPropertiesKHR) getI("vkGetPhysicalDeviceVideoFormatPropertiesKHR");

    // Device-level
    f.CreateVideoSession              = (PFN_vkCreateVideoSessionKHR)              getD("vkCreateVideoSessionKHR");
    f.DestroyVideoSession             = (PFN_vkDestroyVideoSessionKHR)             getD("vkDestroyVideoSessionKHR");
    f.GetVideoSessionMemoryRequirements = (PFN_vkGetVideoSessionMemoryRequirementsKHR) getD("vkGetVideoSessionMemoryRequirementsKHR");
    f.BindVideoSessionMemory          = (PFN_vkBindVideoSessionMemoryKHR)          getD("vkBindVideoSessionMemoryKHR");
    f.CreateVideoSessionParameters    = (PFN_vkCreateVideoSessionParametersKHR)    getD("vkCreateVideoSessionParametersKHR");
    f.DestroyVideoSessionParameters   = (PFN_vkDestroyVideoSessionParametersKHR)   getD("vkDestroyVideoSessionParametersKHR");
    f.CmdBeginVideoCoding             = (PFN_vkCmdBeginVideoCodingKHR)             getD("vkCmdBeginVideoCodingKHR");
    f.CmdEndVideoCoding               = (PFN_vkCmdEndVideoCodingKHR)               getD("vkCmdEndVideoCodingKHR");
    f.CmdControlVideoCoding           = (PFN_vkCmdControlVideoCodingKHR)           getD("vkCmdControlVideoCodingKHR");
    f.CmdEncodeVideo                  = (PFN_vkCmdEncodeVideoKHR)                  getD("vkCmdEncodeVideoKHR");
    f.GetEncodedSessionParams         = (PFN_vkGetEncodedVideoSessionParametersKHR) getD("vkGetEncodedVideoSessionParametersKHR");

    f.loaded = true;
}

#endif // AV1R_VK_VIDEO_LOADER_H
