// Vulkan AV1 video encoding for AV1R
// Структура адаптирована из vulkan-video-encode-simple-main/videoencoder.cpp (H.264 → AV1)
// Compiled only when AV1R_VULKAN_VIDEO_AV1 is defined

#ifdef AV1R_VULKAN_VIDEO_AV1

#include <vulkan/vulkan.h>
#include "vk_video/vulkan_video_encode_av1_khr.h"
#include "av1r_vk_video_loader.h"
#include <stdexcept>
#include <vector>
#include <cstdint>
#include <cstring>
#include <cstdio>
#include <limits>
#include "av1r_vulkan_ctx.h"

// av1r_commands.cpp
VkCommandPool   av1r_create_command_pool(VkDevice, uint32_t);
VkCommandBuffer av1r_alloc_command_buffer(VkDevice, VkCommandPool);
void            av1r_begin_command_buffer(VkCommandBuffer);
void            av1r_end_command_buffer(VkCommandBuffer);
VkFence         av1r_create_fence(VkDevice);
void            av1r_wait_fence(VkDevice, VkFence);
void            av1r_reset_fence(VkDevice, VkFence);

// av1r_memory.cpp
Av1rBuffer av1r_buffer_create(VkPhysicalDevice, VkDevice, size_t,
                               VkBufferUsageFlags, VkMemoryPropertyFlags,
                               VkMemoryPropertyFlags);
void       av1r_buffer_destroy(VkDevice, Av1rBuffer&);

// ============================================================================
// Вспомогательные функции
// ============================================================================

// CRF (0-63) → AV1 qIndex (0-255) nonlinear mapping.
// AV1 qIndex 0 = lossless, 255 = worst quality.
// Linear crf*4 gives qIndex 112 at CRF 28 which is too low for real compression.
// This LUT approximates aomenc --cq-level behaviour.
static uint32_t crf_to_qindex(uint32_t crf)
{
    struct Point { uint32_t crf; uint32_t qi; };
    static const Point lut[] = {
        { 0,   0}, {10,  60}, {20, 120}, {28, 175},
        {35, 210}, {50, 240}, {63, 255}
    };
    static const int N = sizeof(lut) / sizeof(lut[0]);

    if (crf == 0)   return 0;
    if (crf >= 63)  return 255;

    for (int i = 1; i < N; i++) {
        if (crf <= lut[i].crf) {
            uint32_t dc = crf - lut[i-1].crf;
            uint32_t dr = lut[i].crf - lut[i-1].crf;
            uint32_t dq = lut[i].qi  - lut[i-1].qi;
            return lut[i-1].qi + dc * dq / dr;
        }
    }
    return 255;
}

// Найти тип памяти (из av1r_memory.cpp паттерна)
static uint32_t find_mem_type(VkPhysicalDevice phys,
                               uint32_t type_bits,
                               VkMemoryPropertyFlags flags)
{
    VkPhysicalDeviceMemoryProperties props{};
    vkGetPhysicalDeviceMemoryProperties(phys, &props);
    for (uint32_t i = 0; i < props.memoryTypeCount; i++) {
        if ((type_bits & (1u << i)) &&
            (props.memoryTypes[i].propertyFlags & flags) == flags)
            return i;
    }
    return UINT32_MAX;
}

// ============================================================================
// Av1rEncoder — инкапсулирует состояние Vulkan AV1 сессии
// Аналог класса VideoEncoder из vulkan-video-encode-simple-main
// ============================================================================
struct Av1rEncoder {
    // Vulkan объекты — не владеем, берём из Av1rVulkanCtx
    VkPhysicalDevice physDevice  = VK_NULL_HANDLE;
    VkDevice         device      = VK_NULL_HANDLE;
    VkQueue          encodeQueue = VK_NULL_HANDLE;
    uint32_t         encodeQFam  = UINT32_MAX;

    // Video session
    VkVideoSessionKHR           videoSession           = VK_NULL_HANDLE;
    VkVideoSessionParametersKHR videoSessionParameters = VK_NULL_HANDLE;
    std::vector<VkDeviceMemory> sessionMemory;

    // DPB (Decoded Picture Buffer) — ping-pong, как в примере строки 738-744
    static constexpr uint32_t DPB_COUNT = 2;
    VkImage     dpbImages[DPB_COUNT]     = {};
    VkImageView dpbImageViews[DPB_COUNT] = {};
    VkDeviceMemory dpbMemory[DPB_COUNT]  = {};

    // Промежуточный NV12 образ — вход для encode
    VkImage        srcImage     = VK_NULL_HANDLE;
    VkImageView    srcImageView = VK_NULL_HANDLE;
    VkDeviceMemory srcMemory    = VK_NULL_HANDLE;

    // Bitstream output buffer (GPU→CPU)
    VkBuffer       bitstreamBuf    = VK_NULL_HANDLE;
    VkDeviceMemory bitstreamMemory = VK_NULL_HANDLE;
    void*          bitstreamPtr    = nullptr;
    static constexpr VkDeviceSize BITSTREAM_BUF_SIZE = 8 * 1024 * 1024; // 8 MB per frame

    // Query pool для получения размера bitstream (строки 464-476 примера)
    VkQueryPool queryPool = VK_NULL_HANDLE;

    // Command pools + fence
    VkCommandPool encodeCommandPool   = VK_NULL_HANDLE;
    VkCommandPool transferCommandPool = VK_NULL_HANDLE;
    VkFence       encodeFence         = VK_NULL_HANDLE;
    VkFence       transferFence       = VK_NULL_HANDLE;

    // Transfer queue (for vkCmdCopyBufferToImage — encode queue has no TRANSFER)
    VkQueue       transferQueue = VK_NULL_HANDLE;
    uint32_t      transferQFam  = UINT32_MAX;

    // Семафор между очередями (transfer → encode)
    VkSemaphore interQueueSemaphore = VK_NULL_HANDLE;

    // Параметры
    uint32_t width  = 0;
    uint32_t height = 0;
    uint32_t fps    = 0;
    uint32_t crf    = 28;
    uint32_t frameCount = 0;

    // AV1 codec state (аналог m_sps/m_pps из примера)
    StdVideoAV1SequenceHeader seqHeader{};

    // Encoded sequence header OBU (prepended to first frame)
    std::vector<uint8_t> seqHeaderData;
    bool seqHeaderPending = true;

    // Rate control (из initRateControl примера)
    VkVideoEncodeRateControlInfoKHR     rateControlInfo{};
    VkVideoEncodeRateControlLayerInfoKHR rateControlLayer{};
    VkVideoEncodeAV1RateControlInfoKHR  av1RateControlInfo{};
    VkVideoEncodeAV1RateControlLayerInfoKHR av1RateControlLayer{};
    VkVideoEncodeRateControlModeFlagBitsKHR chosenRateControlMode =
        VK_VIDEO_ENCODE_RATE_CONTROL_MODE_DEFAULT_KHR;

    // Выбранный формат (определяется при createVideoSession)
    VkFormat srcFormat = VK_FORMAT_G8_B8R8_2PLANE_420_UNORM; // NV12
    VkFormat dpbFormat = VK_FORMAT_G8_B8R8_2PLANE_420_UNORM;

    VkVideoProfileInfoKHR     videoProfile{};
    VkVideoProfileListInfoKHR videoProfileList{};
    VkVideoEncodeAV1ProfileInfoKHR av1ProfileInfo{};

    bool initialized = false;
};

// ============================================================================
// Query minimum coded extent for AV1 encode (called before ffmpeg pipe)
// ============================================================================
void av1r_vulkan_query_min_extent(VkInstance instance, VkPhysicalDevice physDevice,
                                   uint32_t* out_min_w, uint32_t* out_min_h)
{
    *out_min_w = 0;
    *out_min_h = 0;

    // Load instance-level function directly (device funcs may not be loaded yet)
    auto pfn = (PFN_vkGetPhysicalDeviceVideoCapabilitiesKHR)
        vkGetInstanceProcAddr(instance, "vkGetPhysicalDeviceVideoCapabilitiesKHR");
    if (!pfn) return;

    VkVideoEncodeAV1ProfileInfoKHR av1Prof{};
    av1Prof.sType      = VK_STRUCTURE_TYPE_VIDEO_ENCODE_AV1_PROFILE_INFO_KHR;
    av1Prof.stdProfile = STD_VIDEO_AV1_PROFILE_MAIN;

    VkVideoProfileInfoKHR profile{};
    profile.sType               = VK_STRUCTURE_TYPE_VIDEO_PROFILE_INFO_KHR;
    profile.pNext               = &av1Prof;
    profile.videoCodecOperation = VK_VIDEO_CODEC_OPERATION_ENCODE_AV1_BIT_KHR;
    profile.chromaSubsampling   = VK_VIDEO_CHROMA_SUBSAMPLING_420_BIT_KHR;
    profile.lumaBitDepth        = VK_VIDEO_COMPONENT_BIT_DEPTH_8_BIT_KHR;
    profile.chromaBitDepth      = VK_VIDEO_COMPONENT_BIT_DEPTH_8_BIT_KHR;

    VkVideoEncodeAV1CapabilitiesKHR av1Caps{};
    av1Caps.sType = VK_STRUCTURE_TYPE_VIDEO_ENCODE_AV1_CAPABILITIES_KHR;

    VkVideoEncodeCapabilitiesKHR encodeCaps{};
    encodeCaps.sType = VK_STRUCTURE_TYPE_VIDEO_ENCODE_CAPABILITIES_KHR;
    encodeCaps.pNext = &av1Caps;

    VkVideoCapabilitiesKHR caps{};
    caps.sType = VK_STRUCTURE_TYPE_VIDEO_CAPABILITIES_KHR;
    caps.pNext = &encodeCaps;

    VkResult res = pfn(physDevice, &profile, &caps);
    if (res != VK_SUCCESS) return;

    *out_min_w = caps.minCodedExtent.width;
    *out_min_h = caps.minCodedExtent.height;
}

// ============================================================================
// createVideoSession
// Адаптировано из VideoEncoder::createVideoSession() строки 139-250
// H.264 profile/capabilities → AV1 profile/capabilities
// ============================================================================
static void createVideoSession(Av1rEncoder& enc, int crf)
{
    // AV1 profile (аналог строки 140-148 примера)
    enc.av1ProfileInfo.sType      = VK_STRUCTURE_TYPE_VIDEO_ENCODE_AV1_PROFILE_INFO_KHR;
    enc.av1ProfileInfo.stdProfile = STD_VIDEO_AV1_PROFILE_MAIN;

    enc.videoProfile.sType               = VK_STRUCTURE_TYPE_VIDEO_PROFILE_INFO_KHR;
    enc.videoProfile.pNext               = &enc.av1ProfileInfo;
    enc.videoProfile.videoCodecOperation = VK_VIDEO_CODEC_OPERATION_ENCODE_AV1_BIT_KHR;
    enc.videoProfile.chromaSubsampling   = VK_VIDEO_CHROMA_SUBSAMPLING_420_BIT_KHR;
    enc.videoProfile.lumaBitDepth        = VK_VIDEO_COMPONENT_BIT_DEPTH_8_BIT_KHR;
    enc.videoProfile.chromaBitDepth      = VK_VIDEO_COMPONENT_BIT_DEPTH_8_BIT_KHR;

    enc.videoProfileList.sType        = VK_STRUCTURE_TYPE_VIDEO_PROFILE_LIST_INFO_KHR;
    enc.videoProfileList.profileCount = 1;
    enc.videoProfileList.pProfiles    = &enc.videoProfile;

    // Запрос capabilities (строки 154-174 примера, AV1 структуры)
    VkVideoEncodeAV1CapabilitiesKHR av1Caps{};
    av1Caps.sType = VK_STRUCTURE_TYPE_VIDEO_ENCODE_AV1_CAPABILITIES_KHR;

    VkVideoEncodeCapabilitiesKHR encodeCaps{};
    encodeCaps.sType = VK_STRUCTURE_TYPE_VIDEO_ENCODE_CAPABILITIES_KHR;
    encodeCaps.pNext = &av1Caps;

    VkVideoCapabilitiesKHR caps{};
    caps.sType = VK_STRUCTURE_TYPE_VIDEO_CAPABILITIES_KHR;
    caps.pNext = &encodeCaps;

    VkResult res = av1r_vk_video_funcs().GetPhysDevVideoCapabilities(enc.physDevice,
                                                           &enc.videoProfile, &caps);
    if (res != VK_SUCCESS)
        throw std::runtime_error("vkGetPhysicalDeviceVideoCapabilitiesKHR failed: " +
                                 std::to_string(res));

    // DISABLED mode (CQP): quality via constantQIndex per frame
    // NOTE: on RADV, VBR is reported but broken (produces huge files).
    //       DISABLED + constantQIndex is the only working mode.
    if (encodeCaps.rateControlModes & VK_VIDEO_ENCODE_RATE_CONTROL_MODE_DISABLED_BIT_KHR)
        enc.chosenRateControlMode = VK_VIDEO_ENCODE_RATE_CONTROL_MODE_DISABLED_BIT_KHR;
    else
        enc.chosenRateControlMode = VK_VIDEO_ENCODE_RATE_CONTROL_MODE_DEFAULT_KHR;

    // Определение поддерживаемых форматов (строки 190-235 примера)
    VkPhysicalDeviceVideoFormatInfoKHR fmtInfo{};
    fmtInfo.sType       = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VIDEO_FORMAT_INFO_KHR;
    fmtInfo.pNext       = &enc.videoProfileList;
    fmtInfo.imageUsage  = VK_IMAGE_USAGE_VIDEO_ENCODE_SRC_BIT_KHR;

    uint32_t fmtCount = 0;
    av1r_vk_video_funcs().GetPhysDevVideoFormatProperties(enc.physDevice, &fmtInfo, &fmtCount, nullptr);
    std::vector<VkVideoFormatPropertiesKHR> fmtProps(fmtCount);
    for (auto& f : fmtProps) f.sType = VK_STRUCTURE_TYPE_VIDEO_FORMAT_PROPERTIES_KHR;
    av1r_vk_video_funcs().GetPhysDevVideoFormatProperties(enc.physDevice, &fmtInfo,
                                                &fmtCount, fmtProps.data());

    enc.srcFormat = VK_FORMAT_UNDEFINED;
    for (const auto& f : fmtProps) {
        if (f.format == VK_FORMAT_G8_B8R8_2PLANE_420_UNORM ||
            f.format == VK_FORMAT_G8_B8_R8_3PLANE_420_UNORM) {
            enc.srcFormat = f.format;
            break;
        }
    }
    if (enc.srcFormat == VK_FORMAT_UNDEFINED)
        throw std::runtime_error("No supported NV12/YUV420 format for AV1 encode src");

    // DPB формат (строки 223-235 примера)
    fmtInfo.imageUsage = VK_IMAGE_USAGE_VIDEO_ENCODE_DPB_BIT_KHR;
    av1r_vk_video_funcs().GetPhysDevVideoFormatProperties(enc.physDevice, &fmtInfo, &fmtCount, nullptr);
    std::vector<VkVideoFormatPropertiesKHR> dpbFmtProps(fmtCount);
    for (auto& f : dpbFmtProps) f.sType = VK_STRUCTURE_TYPE_VIDEO_FORMAT_PROPERTIES_KHR;
    av1r_vk_video_funcs().GetPhysDevVideoFormatProperties(enc.physDevice, &fmtInfo,
                                                &fmtCount, dpbFmtProps.data());
    if (dpbFmtProps.empty())
        throw std::runtime_error("No supported DPB format for AV1 encode");
    enc.dpbFormat = dpbFmtProps[0].format;

    // AV1 sequence header (аналог SPS/PPS, строки 237-249 примера)
    static const VkExtensionProperties av1StdExt = {
        VK_STD_VULKAN_VIDEO_CODEC_AV1_ENCODE_EXTENSION_NAME,
        VK_STD_VULKAN_VIDEO_CODEC_AV1_ENCODE_SPEC_VERSION
    };

    VkVideoSessionCreateInfoKHR sessionCI{};
    sessionCI.sType                       = VK_STRUCTURE_TYPE_VIDEO_SESSION_CREATE_INFO_KHR;
    sessionCI.pVideoProfile               = &enc.videoProfile;
    sessionCI.queueFamilyIndex            = enc.encodeQFam;
    sessionCI.pictureFormat               = enc.srcFormat;
    sessionCI.maxCodedExtent              = {enc.width, enc.height};
    sessionCI.maxDpbSlots                 = 2;
    sessionCI.maxActiveReferencePictures  = 1;
    sessionCI.referencePictureFormat      = enc.dpbFormat;
    sessionCI.pStdHeaderVersion           = &av1StdExt;

    res = av1r_vk_video_funcs().CreateVideoSession(enc.device, &sessionCI, nullptr, &enc.videoSession);
    if (res != VK_SUCCESS)
        throw std::runtime_error("vkCreateVideoSessionKHR failed: " + std::to_string(res));
}

// ============================================================================
// allocateVideoSessionMemory
// Прямой перенос из VideoEncoder::allocateVideoSessionMemory() строки 252-288
// ============================================================================
static void allocateVideoSessionMemory(Av1rEncoder& enc)
{
    uint32_t count = 0;
    av1r_vk_video_funcs().GetVideoSessionMemoryRequirements(enc.device, enc.videoSession, &count, nullptr);

    std::vector<VkVideoSessionMemoryRequirementsKHR> reqs(count);
    for (auto& r : reqs) r.sType = VK_STRUCTURE_TYPE_VIDEO_SESSION_MEMORY_REQUIREMENTS_KHR;
    av1r_vk_video_funcs().GetVideoSessionMemoryRequirements(enc.device, enc.videoSession, &count, reqs.data());

    enc.sessionMemory.resize(count, VK_NULL_HANDLE);
    std::vector<VkBindVideoSessionMemoryInfoKHR> binds(count);

    for (uint32_t i = 0; i < count; i++) {
        const auto& mr = reqs[i].memoryRequirements;
        uint32_t mt = find_mem_type(enc.physDevice, mr.memoryTypeBits,
                                    VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT);
        if (mt == UINT32_MAX)
            throw std::runtime_error("No device-local memory for video session");

        VkMemoryAllocateInfo ai{};
        ai.sType           = VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO;
        ai.allocationSize  = mr.size;
        ai.memoryTypeIndex = mt;
        vkAllocateMemory(enc.device, &ai, nullptr, &enc.sessionMemory[i]);

        binds[i].sType            = VK_STRUCTURE_TYPE_BIND_VIDEO_SESSION_MEMORY_INFO_KHR;
        binds[i].memoryBindIndex  = reqs[i].memoryBindIndex;
        binds[i].memory           = enc.sessionMemory[i];
        binds[i].memoryOffset     = 0;
        binds[i].memorySize       = mr.size;
    }
    av1r_vk_video_funcs().BindVideoSessionMemory(enc.device, enc.videoSession, count, binds.data());
}

// ============================================================================
// createVideoSessionParameters (AV1 sequence header вместо SPS/PPS)
// Аналог строк 290-318 примера
// ============================================================================
static void createVideoSessionParameters(Av1rEncoder& enc)
{
    // AV1 sequence header — аналог SPS в H.264
    memset(&enc.seqHeader, 0, sizeof(enc.seqHeader));
    enc.seqHeader.flags.film_grain_params_present = 0;
    enc.seqHeader.flags.frame_id_numbers_present_flag = 0;
    enc.seqHeader.flags.enable_order_hint  = 1;  // required for inter prediction
    enc.seqHeader.flags.enable_cdef        = 1;
    enc.seqHeader.seq_profile = STD_VIDEO_AV1_PROFILE_MAIN;
    enc.seqHeader.frame_width_bits_minus_1  = 15; // 16 бит max для размера кадра
    enc.seqHeader.frame_height_bits_minus_1 = 15;
    enc.seqHeader.max_frame_width_minus_1   = enc.width  - 1;
    enc.seqHeader.max_frame_height_minus_1  = enc.height - 1;
    enc.seqHeader.order_hint_bits_minus_1   = 7;  // 8 bits for order_hint

    // Color config — required by driver (NV12 = 4:2:0, 8-bit)
    static StdVideoAV1ColorConfig colorConfig{};
    memset(&colorConfig, 0, sizeof(colorConfig));
    colorConfig.BitDepth       = 8;
    colorConfig.subsampling_x  = 1;  // 4:2:0
    colorConfig.subsampling_y  = 1;
    colorConfig.color_primaries          = STD_VIDEO_AV1_COLOR_PRIMARIES_BT_709;
    colorConfig.transfer_characteristics = STD_VIDEO_AV1_TRANSFER_CHARACTERISTICS_BT_709;
    colorConfig.matrix_coefficients      = STD_VIDEO_AV1_MATRIX_COEFFICIENTS_BT_709;
    colorConfig.chroma_sample_position   = STD_VIDEO_AV1_CHROMA_SAMPLE_POSITION_UNKNOWN;
    enc.seqHeader.pColorConfig = &colorConfig;

    VkVideoEncodeAV1SessionParametersCreateInfoKHR av1ParamsCI{};
    av1ParamsCI.sType             = VK_STRUCTURE_TYPE_VIDEO_ENCODE_AV1_SESSION_PARAMETERS_CREATE_INFO_KHR;
    av1ParamsCI.pStdSequenceHeader = &enc.seqHeader;

    VkVideoSessionParametersCreateInfoKHR paramsCI{};
    paramsCI.sType                       = VK_STRUCTURE_TYPE_VIDEO_SESSION_PARAMETERS_CREATE_INFO_KHR;
    paramsCI.pNext                       = &av1ParamsCI;
    paramsCI.videoSessionParametersTemplate = VK_NULL_HANDLE;
    paramsCI.videoSession                = enc.videoSession;

    VkResult res = av1r_vk_video_funcs().CreateVideoSessionParameters(enc.device, &paramsCI,
                                                     nullptr, &enc.videoSessionParameters);
    if (res != VK_SUCCESS)
        throw std::runtime_error("vkCreateVideoSessionParametersKHR failed: " +
                                 std::to_string(res));
}

// ============================================================================
// getSequenceHeader: retrieve AV1 sequence header OBU from session parameters
// Analogous to H.264 SPS/PPS retrieval in reference example (lines 318-343)
// ============================================================================
static void getSequenceHeader(Av1rEncoder& enc)
{
    VkVideoEncodeSessionParametersGetInfoKHR getInfo{};
    getInfo.sType = VK_STRUCTURE_TYPE_VIDEO_ENCODE_SESSION_PARAMETERS_GET_INFO_KHR;
    getInfo.videoSessionParameters = enc.videoSessionParameters;

    VkVideoEncodeSessionParametersFeedbackInfoKHR feedback{};
    feedback.sType = VK_STRUCTURE_TYPE_VIDEO_ENCODE_SESSION_PARAMETERS_FEEDBACK_INFO_KHR;

    size_t dataLen = 0;
    VkResult res = av1r_vk_video_funcs().GetEncodedSessionParams(
        enc.device, &getInfo, &feedback, &dataLen, nullptr);
    if (res != VK_SUCCESS)
        throw std::runtime_error("vkGetEncodedVideoSessionParametersKHR (size) failed: " +
                                 std::to_string(res));

    enc.seqHeaderData.resize(dataLen);
    res = av1r_vk_video_funcs().GetEncodedSessionParams(
        enc.device, &getInfo, &feedback, &dataLen, enc.seqHeaderData.data());
    if (res != VK_SUCCESS)
        throw std::runtime_error("vkGetEncodedVideoSessionParametersKHR (data) failed: " +
                                 std::to_string(res));
    enc.seqHeaderData.resize(dataLen);
    enc.seqHeaderPending = true;

    // sequence header retrieved
}

// ============================================================================
// Выделение VkImage с памятью (DPB и src образы)
// ============================================================================
static void createImage(VkPhysicalDevice phys, VkDevice device,
                        uint32_t width, uint32_t height,
                        VkFormat format,
                        VkImageUsageFlags usage,
                        const void* pNext,
                        VkImage& outImage, VkDeviceMemory& outMemory,
                        const uint32_t* queueFamilies = nullptr,
                        uint32_t queueFamilyCount = 0)
{
    VkImageCreateInfo ici{};
    ici.sType         = VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO;
    ici.pNext         = pNext;
    ici.imageType     = VK_IMAGE_TYPE_2D;
    ici.format        = format;
    ici.extent        = {width, height, 1};
    ici.mipLevels     = 1;
    ici.arrayLayers   = 1;
    ici.samples       = VK_SAMPLE_COUNT_1_BIT;
    ici.tiling        = VK_IMAGE_TILING_OPTIMAL;
    ici.usage         = usage;
    ici.initialLayout = VK_IMAGE_LAYOUT_UNDEFINED;

    if (queueFamilies && queueFamilyCount > 1) {
        ici.sharingMode            = VK_SHARING_MODE_CONCURRENT;
        ici.queueFamilyIndexCount  = queueFamilyCount;
        ici.pQueueFamilyIndices    = queueFamilies;
    } else {
        ici.sharingMode = VK_SHARING_MODE_EXCLUSIVE;
    }

    vkCreateImage(device, &ici, nullptr, &outImage);

    VkMemoryRequirements mr{};
    vkGetImageMemoryRequirements(device, outImage, &mr);

    uint32_t mt = UINT32_MAX;
    VkPhysicalDeviceMemoryProperties mp{};
    vkGetPhysicalDeviceMemoryProperties(phys, &mp);
    for (uint32_t i = 0; i < mp.memoryTypeCount; i++) {
        if ((mr.memoryTypeBits & (1u << i)) &&
            (mp.memoryTypes[i].propertyFlags & VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT)) {
            mt = i; break;
        }
    }

    VkMemoryAllocateInfo ai{};
    ai.sType           = VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO;
    ai.allocationSize  = mr.size;
    ai.memoryTypeIndex = mt;
    vkAllocateMemory(device, &ai, nullptr, &outMemory);
    vkBindImageMemory(device, outImage, outMemory, 0);
}

// ============================================================================
// allocateImages: DPB + src NV12
// Адаптировано из allocateReferenceImages() строки 360-396 и
// allocateIntermediateImage() строк 399-461
// ============================================================================
static void allocateImages(Av1rEncoder& enc)
{
    // DPB образы (строки 360-396 примера)
    for (uint32_t i = 0; i < Av1rEncoder::DPB_COUNT; i++) {
        createImage(enc.physDevice, enc.device,
                    enc.width, enc.height,
                    enc.dpbFormat,
                    VK_IMAGE_USAGE_VIDEO_ENCODE_DPB_BIT_KHR,
                    &enc.videoProfileList,
                    enc.dpbImages[i], enc.dpbMemory[i]);

        VkImageViewCreateInfo vci{};
        vci.sType    = VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO;
        vci.image    = enc.dpbImages[i];
        vci.viewType = VK_IMAGE_VIEW_TYPE_2D;
        vci.format   = enc.dpbFormat;
        vci.subresourceRange = {VK_IMAGE_ASPECT_COLOR_BIT, 0, 1, 0, 1};
        vkCreateImageView(enc.device, &vci, nullptr, &enc.dpbImageViews[i]);
    }

    // Src образ NV12 (строки 399-461 примера, без compute шейдера)
    // В AV1R данные NV12 приходят от ffmpeg, загружаем напрямую через staging
    // Concurrent sharing between transfer queue (upload) and encode queue (read)
    uint32_t srcQueueFamilies[2] = { enc.transferQFam, enc.encodeQFam };
    uint32_t srcQfCount = (enc.transferQFam != enc.encodeQFam) ? 2u : 1u;
    createImage(enc.physDevice, enc.device,
                enc.width, enc.height,
                enc.srcFormat,
                VK_IMAGE_USAGE_VIDEO_ENCODE_SRC_BIT_KHR | VK_IMAGE_USAGE_TRANSFER_DST_BIT,
                &enc.videoProfileList,
                enc.srcImage, enc.srcMemory,
                srcQueueFamilies, srcQfCount);

    VkImageViewCreateInfo vci{};
    vci.sType    = VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO;
    vci.image    = enc.srcImage;
    vci.viewType = VK_IMAGE_VIEW_TYPE_2D;
    vci.format   = enc.srcFormat;
    vci.subresourceRange = {VK_IMAGE_ASPECT_COLOR_BIT, 0, 1, 0, 1};
    vkCreateImageView(enc.device, &vci, nullptr, &enc.srcImageView);
}

// ============================================================================
// allocateBitstreamBuffer (строки 345-358 примера)
// ============================================================================
static void allocateBitstreamBuffer(Av1rEncoder& enc)
{
    VkBufferCreateInfo bci{};
    bci.sType       = VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO;
    bci.pNext       = &enc.videoProfileList;
    bci.size        = Av1rEncoder::BITSTREAM_BUF_SIZE;
    bci.usage       = VK_BUFFER_USAGE_VIDEO_ENCODE_DST_BIT_KHR;
    bci.sharingMode = VK_SHARING_MODE_EXCLUSIVE;
    vkCreateBuffer(enc.device, &bci, nullptr, &enc.bitstreamBuf);

    VkMemoryRequirements mr{};
    vkGetBufferMemoryRequirements(enc.device, enc.bitstreamBuf, &mr);
    uint32_t mt = find_mem_type(enc.physDevice, mr.memoryTypeBits,
                                VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT |
                                VK_MEMORY_PROPERTY_HOST_COHERENT_BIT);

    VkMemoryAllocateInfo ai{};
    ai.sType           = VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO;
    ai.allocationSize  = mr.size;
    ai.memoryTypeIndex = mt;
    vkAllocateMemory(enc.device, &ai, nullptr, &enc.bitstreamMemory);
    vkBindBufferMemory(enc.device, enc.bitstreamBuf, enc.bitstreamMemory, 0);
    vkMapMemory(enc.device, enc.bitstreamMemory, 0, VK_WHOLE_SIZE, 0, &enc.bitstreamPtr);
}

// ============================================================================
// createQueryPool (строки 464-476 примера)
// ============================================================================
static void createQueryPool(Av1rEncoder& enc)
{
    VkQueryPoolVideoEncodeFeedbackCreateInfoKHR feedbackCI{};
    feedbackCI.sType = VK_STRUCTURE_TYPE_QUERY_POOL_VIDEO_ENCODE_FEEDBACK_CREATE_INFO_KHR;
    feedbackCI.pNext = &enc.videoProfile;
    feedbackCI.encodeFeedbackFlags =
        VK_VIDEO_ENCODE_FEEDBACK_BITSTREAM_BUFFER_OFFSET_BIT_KHR |
        VK_VIDEO_ENCODE_FEEDBACK_BITSTREAM_BYTES_WRITTEN_BIT_KHR;

    VkQueryPoolCreateInfo qpci{};
    qpci.sType      = VK_STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO;
    qpci.pNext      = &feedbackCI;
    qpci.queryType  = VK_QUERY_TYPE_VIDEO_ENCODE_FEEDBACK_KHR;
    qpci.queryCount = 1;
    vkCreateQueryPool(enc.device, &qpci, nullptr, &enc.queryPool);
}

// ============================================================================
// initRateControl
// Адаптировано из VideoEncoder::initRateControl() строки 575-619
// H.264 rate control → AV1 rate control структуры
// ============================================================================
static void initRateControl(Av1rEncoder& enc, VkCommandBuffer cmd)
{
    VkVideoBeginCodingInfoKHR beginInfo{};
    beginInfo.sType                  = VK_STRUCTURE_TYPE_VIDEO_BEGIN_CODING_INFO_KHR;
    beginInfo.videoSession           = enc.videoSession;
    beginInfo.videoSessionParameters = enc.videoSessionParameters;

    // AV1 rate control layer (аналог VkVideoEncodeH264RateControlLayerInfoKHR)
    enc.av1RateControlLayer.sType = VK_STRUCTURE_TYPE_VIDEO_ENCODE_AV1_RATE_CONTROL_LAYER_INFO_KHR;

    enc.rateControlLayer.pNext               = &enc.av1RateControlLayer;
    enc.rateControlLayer.sType               = VK_STRUCTURE_TYPE_VIDEO_ENCODE_RATE_CONTROL_LAYER_INFO_KHR;
    enc.rateControlLayer.frameRateNumerator  = enc.fps;
    enc.rateControlLayer.frameRateDenominator = 1;
    enc.rateControlLayer.averageBitrate      = 5000000;  // 5 Mbps
    enc.rateControlLayer.maxBitrate          = 20000000; // 20 Mbps

    // AV1 rate control info (аналог VkVideoEncodeH264RateControlInfoKHR)
    enc.av1RateControlInfo.sType          = VK_STRUCTURE_TYPE_VIDEO_ENCODE_AV1_RATE_CONTROL_INFO_KHR;
    enc.av1RateControlInfo.flags          = VK_VIDEO_ENCODE_AV1_RATE_CONTROL_REGULAR_GOP_BIT_KHR;
    enc.av1RateControlInfo.gopFrameCount  = 16;
    enc.av1RateControlInfo.keyFramePeriod = 16;
    enc.av1RateControlInfo.temporalLayerCount = 1;

    enc.rateControlInfo.sType               = VK_STRUCTURE_TYPE_VIDEO_ENCODE_RATE_CONTROL_INFO_KHR;
    enc.rateControlInfo.pNext               = &enc.av1RateControlInfo;
    enc.rateControlInfo.rateControlMode     = enc.chosenRateControlMode;
    enc.rateControlInfo.layerCount          = 1;
    enc.rateControlInfo.pLayers             = &enc.rateControlLayer;
    enc.rateControlInfo.initialVirtualBufferSizeInMs = 100;
    enc.rateControlInfo.virtualBufferSizeInMs        = 200;

    if (enc.chosenRateControlMode & VK_VIDEO_ENCODE_RATE_CONTROL_MODE_CBR_BIT_KHR)
        enc.rateControlLayer.averageBitrate = enc.rateControlLayer.maxBitrate;

    if (enc.chosenRateControlMode & VK_VIDEO_ENCODE_RATE_CONTROL_MODE_DISABLED_BIT_KHR ||
        enc.chosenRateControlMode == VK_VIDEO_ENCODE_RATE_CONTROL_MODE_DEFAULT_KHR) {
        enc.av1RateControlInfo.temporalLayerCount = 0;
        enc.rateControlInfo.layerCount = 0;
    }

    VkVideoCodingControlInfoKHR controlInfo{};
    controlInfo.sType = VK_STRUCTURE_TYPE_VIDEO_CODING_CONTROL_INFO_KHR;
    controlInfo.pNext = &enc.rateControlInfo;
    controlInfo.flags = VK_VIDEO_CODING_CONTROL_RESET_BIT_KHR |
                        VK_VIDEO_CODING_CONTROL_ENCODE_RATE_CONTROL_BIT_KHR;

    VkVideoEndCodingInfoKHR endInfo{};
    endInfo.sType = VK_STRUCTURE_TYPE_VIDEO_END_CODING_INFO_KHR;

    av1r_vk_video_funcs().CmdBeginVideoCoding(cmd, &beginInfo);
    av1r_vk_video_funcs().CmdControlVideoCoding(cmd, &controlInfo);
    av1r_vk_video_funcs().CmdEndVideoCoding(cmd, &endInfo);
}

// ============================================================================
// transitionDpbImagesInitial
// Адаптировано из transitionImagesInitial() строки 622-644
// ============================================================================
static void transitionDpbImagesInitial(Av1rEncoder& enc, VkCommandBuffer cmd)
{
    std::vector<VkImageMemoryBarrier2> barriers;
    for (uint32_t i = 0; i < Av1rEncoder::DPB_COUNT; i++) {
        VkImageMemoryBarrier2 b{};
        b.sType            = VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER_2;
        b.srcStageMask     = VK_PIPELINE_STAGE_2_NONE;
        b.dstStageMask     = VK_PIPELINE_STAGE_2_VIDEO_ENCODE_BIT_KHR;
        b.oldLayout        = VK_IMAGE_LAYOUT_UNDEFINED;
        b.newLayout        = VK_IMAGE_LAYOUT_VIDEO_ENCODE_DPB_KHR;
        b.image            = enc.dpbImages[i];
        b.subresourceRange = {VK_IMAGE_ASPECT_COLOR_BIT, 0, 1, 0, 1};
        barriers.push_back(b);
    }
    VkDependencyInfoKHR dep{};
    dep.sType                    = VK_STRUCTURE_TYPE_DEPENDENCY_INFO_KHR;
    dep.imageMemoryBarrierCount  = static_cast<uint32_t>(barriers.size());
    dep.pImageMemoryBarriers     = barriers.data();
    vkCmdPipelineBarrier2(cmd, &dep);
}

// ============================================================================
// uploadNV12Frame: CPU NV12 → GPU src image через staging buffer
// Заменяет RGB→YCbCr compute shader из примера (у нас данные уже NV12 от ffmpeg)
// ============================================================================
static void uploadNV12Frame(Av1rEncoder& enc,
                             VkCommandBuffer cmd,
                             const uint8_t* nv12,
                             VkBuffer stagingBuf, void* stagingPtr)
{
    size_t frameBytes = enc.width * enc.height * 3 / 2;
    memcpy(stagingPtr, nv12, frameBytes);

    // Transition src image UNDEFINED → TRANSFER_DST
    VkImageMemoryBarrier2 toTransfer{};
    toTransfer.sType         = VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER_2;
    toTransfer.srcStageMask  = VK_PIPELINE_STAGE_2_NONE;
    toTransfer.dstStageMask  = VK_PIPELINE_STAGE_2_COPY_BIT;
    toTransfer.oldLayout     = VK_IMAGE_LAYOUT_UNDEFINED;
    toTransfer.newLayout     = VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL;
    toTransfer.image         = enc.srcImage;
    toTransfer.subresourceRange = {VK_IMAGE_ASPECT_COLOR_BIT, 0, 1, 0, 1};

    VkDependencyInfoKHR dep{};
    dep.sType                   = VK_STRUCTURE_TYPE_DEPENDENCY_INFO_KHR;
    dep.imageMemoryBarrierCount = 1;
    dep.pImageMemoryBarriers    = &toTransfer;
    vkCmdPipelineBarrier2(cmd, &dep);

    // Copy Y plane
    VkBufferImageCopy yRegion{};
    yRegion.bufferOffset      = 0;
    yRegion.bufferRowLength   = enc.width;
    yRegion.imageSubresource  = {VK_IMAGE_ASPECT_PLANE_0_BIT, 0, 0, 1};
    yRegion.imageExtent       = {enc.width, enc.height, 1};
    vkCmdCopyBufferToImage(cmd, stagingBuf, enc.srcImage,
                           VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL, 1, &yRegion);

    // Copy UV plane
    VkBufferImageCopy uvRegion{};
    uvRegion.bufferOffset     = enc.width * enc.height;
    uvRegion.bufferRowLength  = enc.width / 2;
    uvRegion.imageSubresource = {VK_IMAGE_ASPECT_PLANE_1_BIT, 0, 0, 1};
    uvRegion.imageExtent      = {enc.width / 2, enc.height / 2, 1};
    vkCmdCopyBufferToImage(cmd, stagingBuf, enc.srcImage,
                           VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL, 1, &uvRegion);

    // Transition src image TRANSFER_DST → VIDEO_ENCODE_SRC (строки 786-806 примера)
    VkImageMemoryBarrier2 toEncode{};
    toEncode.sType         = VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER_2;
    toEncode.srcStageMask  = VK_PIPELINE_STAGE_2_COPY_BIT;
    toEncode.srcAccessMask = VK_ACCESS_2_TRANSFER_WRITE_BIT;
    toEncode.dstStageMask  = VK_PIPELINE_STAGE_2_VIDEO_ENCODE_BIT_KHR;
    toEncode.dstAccessMask = VK_ACCESS_2_VIDEO_ENCODE_READ_BIT_KHR;
    toEncode.oldLayout     = VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL;
    toEncode.newLayout     = VK_IMAGE_LAYOUT_VIDEO_ENCODE_SRC_KHR;
    toEncode.image         = enc.srcImage;
    toEncode.subresourceRange = {VK_IMAGE_ASPECT_COLOR_BIT, 0, 1, 0, 1};

    dep.pImageMemoryBarriers = &toEncode;
    vkCmdPipelineBarrier2(cmd, &dep);
}

// ============================================================================
// encodeOneFrame
// Прямой перенос структуры из VideoEncoder::encodeVideoFrame() строки 718-857
// H.264 picture info → AV1 picture info
// ============================================================================
static void encodeOneFrame(Av1rEncoder& enc, VkCommandBuffer cmd)
{
    const uint32_t GOP_LENGTH   = 16;
    const uint32_t gopIdx       = enc.frameCount % GOP_LENGTH;
    const bool     isKeyFrame   = (gopIdx == 0);
    const uint32_t querySlotId  = 0;

    vkCmdResetQueryPool(cmd, enc.queryPool, querySlotId, 1);

    // Ping-pong DPB: текущий кадр пишется в curSlot, reference читается из refSlot
    // Разные images — нет конфликта read/write
    uint32_t curSlot, refSlot;
    if (isKeyFrame) {
        // Keyframes always use slot 0 — no reference needed
        curSlot = 0;
        refSlot = 0;
    } else {
        curSlot = enc.frameCount & 1u;     // 0, 1, 0, 1, ...
        refSlot = curSlot ^ 1u;            // 1, 0, 1, 0, ...
    }

    // Picture resources для DPB images
    VkVideoPictureResourceInfoKHR curPicRes{};
    curPicRes.sType            = VK_STRUCTURE_TYPE_VIDEO_PICTURE_RESOURCE_INFO_KHR;
    curPicRes.imageViewBinding = enc.dpbImageViews[curSlot];
    curPicRes.codedOffset      = {0, 0};
    curPicRes.codedExtent      = {enc.width, enc.height};

    VkVideoPictureResourceInfoKHR refPicRes{};
    refPicRes.sType            = VK_STRUCTURE_TYPE_VIDEO_PICTURE_RESOURCE_INFO_KHR;
    refPicRes.imageViewBinding = enc.dpbImageViews[refSlot];
    refPicRes.codedOffset      = {0, 0};
    refPicRes.codedExtent      = {enc.width, enc.height};

    // Setup reference info — текущий кадр записывается в DPB curSlot
    StdVideoEncodeAV1ReferenceInfo stdSetupRef{};
    memset(&stdSetupRef, 0, sizeof(stdSetupRef));
    stdSetupRef.frame_type = isKeyFrame ? STD_VIDEO_AV1_FRAME_TYPE_KEY
                                        : STD_VIDEO_AV1_FRAME_TYPE_INTER;
    stdSetupRef.OrderHint  = static_cast<uint8_t>(enc.frameCount & 0xFF);

    VkVideoEncodeAV1DpbSlotInfoKHR setupDpbInfo{};
    setupDpbInfo.sType             = VK_STRUCTURE_TYPE_VIDEO_ENCODE_AV1_DPB_SLOT_INFO_KHR;
    setupDpbInfo.pStdReferenceInfo = &stdSetupRef;

    VkVideoReferenceSlotInfoKHR setupSlot{};
    setupSlot.sType            = VK_STRUCTURE_TYPE_VIDEO_REFERENCE_SLOT_INFO_KHR;
    setupSlot.pNext            = &setupDpbInfo;
    setupSlot.slotIndex        = static_cast<int32_t>(curSlot);
    setupSlot.pPictureResource = &curPicRes;

    // Reference info — предыдущий кадр из DPB refSlot (inter only)
    // frame_type must match what was actually stored: frame 0 = KEY, rest = INTER
    const uint32_t prevGopIdx = (enc.frameCount - 1) % 16;
    StdVideoEncodeAV1ReferenceInfo stdRefInfo{};
    memset(&stdRefInfo, 0, sizeof(stdRefInfo));
    stdRefInfo.frame_type = (prevGopIdx == 0) ? STD_VIDEO_AV1_FRAME_TYPE_KEY
                                               : STD_VIDEO_AV1_FRAME_TYPE_INTER;
    stdRefInfo.OrderHint  = static_cast<uint8_t>((enc.frameCount - 1) & 0xFF);

    VkVideoEncodeAV1DpbSlotInfoKHR refDpbInfo{};
    refDpbInfo.sType             = VK_STRUCTURE_TYPE_VIDEO_ENCODE_AV1_DPB_SLOT_INFO_KHR;
    refDpbInfo.pStdReferenceInfo = &stdRefInfo;

    VkVideoReferenceSlotInfoKHR refSlotInfo{};
    refSlotInfo.sType            = VK_STRUCTURE_TYPE_VIDEO_REFERENCE_SLOT_INFO_KHR;
    refSlotInfo.pNext            = &refDpbInfo;
    refSlotInfo.slotIndex        = static_cast<int32_t>(refSlot);
    refSlotInfo.pPictureResource = &refPicRes;

    // DPB barrier: ensure previous write to refSlot is visible as read
    if (!isKeyFrame) {
        VkImageMemoryBarrier2 dpbBarrier{};
        dpbBarrier.sType            = VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER_2;
        dpbBarrier.srcStageMask     = VK_PIPELINE_STAGE_2_VIDEO_ENCODE_BIT_KHR;
        dpbBarrier.srcAccessMask    = VK_ACCESS_2_VIDEO_ENCODE_WRITE_BIT_KHR;
        dpbBarrier.dstStageMask     = VK_PIPELINE_STAGE_2_VIDEO_ENCODE_BIT_KHR;
        dpbBarrier.dstAccessMask    = VK_ACCESS_2_VIDEO_ENCODE_READ_BIT_KHR;
        dpbBarrier.oldLayout        = VK_IMAGE_LAYOUT_VIDEO_ENCODE_DPB_KHR;
        dpbBarrier.newLayout        = VK_IMAGE_LAYOUT_VIDEO_ENCODE_DPB_KHR;
        dpbBarrier.image            = enc.dpbImages[refSlot];
        dpbBarrier.subresourceRange = {VK_IMAGE_ASPECT_COLOR_BIT, 0, 1, 0, 1};

        VkDependencyInfoKHR dpbDep{};
        dpbDep.sType                    = VK_STRUCTURE_TYPE_DEPENDENCY_INFO_KHR;
        dpbDep.imageMemoryBarrierCount  = 1;
        dpbDep.pImageMemoryBarriers     = &dpbBarrier;
        vkCmdPipelineBarrier2(cmd, &dpbDep);
    }

    // Begin video coding — перечисляем ВСЕ активные DPB slots
    // Per reference example: setupSlot uses slotIndex = -1 in beginCodingInfo,
    // then gets real slotIndex before being used as pSetupReferenceSlot
    VkVideoReferenceSlotInfoKHR beginSlots[2];
    uint32_t beginSlotCount = 0;

    // Copy setupSlot but with slotIndex = -1 for beginCodingInfo
    beginSlots[0] = setupSlot;
    beginSlots[0].slotIndex = -1;

    if (isKeyFrame) {
        beginSlotCount = 1;
    } else {
        beginSlots[1] = refSlotInfo;
        beginSlotCount = 2;
    }

    VkVideoBeginCodingInfoKHR beginCodingInfo{};
    beginCodingInfo.sType                  = VK_STRUCTURE_TYPE_VIDEO_BEGIN_CODING_INFO_KHR;
    beginCodingInfo.pNext                  = &enc.rateControlInfo;
    beginCodingInfo.videoSession           = enc.videoSession;
    beginCodingInfo.videoSessionParameters = enc.videoSessionParameters;
    beginCodingInfo.referenceSlotCount     = beginSlotCount;
    beginCodingInfo.pReferenceSlots        = beginSlots;
    av1r_vk_video_funcs().CmdBeginVideoCoding(cmd, &beginCodingInfo);

    // --- Sub-structures required by StdVideoEncodeAV1PictureInfo ---

    // Tile info: single tile covering the whole frame
    StdVideoAV1TileInfo tileInfo{};
    memset(&tileInfo, 0, sizeof(tileInfo));
    tileInfo.flags.uniform_tile_spacing_flag = 1;
    tileInfo.TileCols = 1;
    tileInfo.TileRows = 1;
    // pMiColStarts/pMiRowStarts not needed with uniform spacing and 1 tile

    // Quantization
    StdVideoAV1Quantization quantization{};
    memset(&quantization, 0, sizeof(quantization));
    uint32_t qIdx = crf_to_qindex(enc.crf);
    quantization.base_q_idx = static_cast<uint8_t>(qIdx);

    // Loop filter
    StdVideoAV1LoopFilter loopFilter{};
    memset(&loopFilter, 0, sizeof(loopFilter));

    // CDEF
    StdVideoAV1CDEF cdef{};
    memset(&cdef, 0, sizeof(cdef));
    cdef.cdef_damping_minus_3 = 0;  // damping = 3
    cdef.cdef_bits = 0;             // 1 CDEF filter

    // Loop restoration — disabled
    StdVideoAV1LoopRestoration loopRestoration{};
    memset(&loopRestoration, 0, sizeof(loopRestoration));
    loopRestoration.FrameRestorationType[0] = STD_VIDEO_AV1_FRAME_RESTORATION_TYPE_NONE;
    loopRestoration.FrameRestorationType[1] = STD_VIDEO_AV1_FRAME_RESTORATION_TYPE_NONE;
    loopRestoration.FrameRestorationType[2] = STD_VIDEO_AV1_FRAME_RESTORATION_TYPE_NONE;
    loopRestoration.LoopRestorationSize[0] = 256;
    loopRestoration.LoopRestorationSize[1] = 256;
    loopRestoration.LoopRestorationSize[2] = 256;

    // Global motion — identity for all refs
    StdVideoAV1GlobalMotion globalMotion{};
    memset(&globalMotion, 0, sizeof(globalMotion));

    // AV1 picture info
    StdVideoEncodeAV1PictureInfo stdPicInfo{};
    memset(&stdPicInfo, 0, sizeof(stdPicInfo));
    stdPicInfo.flags.show_frame     = 1;
    stdPicInfo.flags.showable_frame = 1;
    stdPicInfo.frame_type = isKeyFrame ? STD_VIDEO_AV1_FRAME_TYPE_KEY
                                       : STD_VIDEO_AV1_FRAME_TYPE_INTER;
    stdPicInfo.current_frame_id    = 0;
    stdPicInfo.order_hint          = static_cast<uint8_t>(enc.frameCount & 0xFF);
    stdPicInfo.primary_ref_frame   = isKeyFrame ? STD_VIDEO_AV1_PRIMARY_REF_NONE : 0;
    // refresh_frame_flags: keyframe updates ALL slots (AV1 spec requirement),
    // inter updates only curSlot
    stdPicInfo.refresh_frame_flags = isKeyFrame ? 0xFF : (1u << curSlot);
    stdPicInfo.TxMode              = STD_VIDEO_AV1_TX_MODE_LARGEST;
    stdPicInfo.interpolation_filter = STD_VIDEO_AV1_INTERPOLATION_FILTER_EIGHTTAP;

    // ref_frame_idx: map all 7 AV1 reference types to the physical refSlot
    for (uint32_t i = 0; i < STD_VIDEO_AV1_REFS_PER_FRAME; i++) {
        stdPicInfo.ref_frame_idx[i] = isKeyFrame ? 0 : static_cast<int8_t>(refSlot);
    }

    // ref_order_hint: order_hint stored in each of the 8 virtual slots
    // After keyframe at slot curSlot: that slot has current order_hint
    // After inter at slot curSlot: that slot has current order_hint
    // The refSlot has the previous frame's order_hint
    if (!isKeyFrame) {
        stdPicInfo.ref_order_hint[refSlot] = static_cast<uint8_t>((enc.frameCount - 1) & 0xFF);
        stdPicInfo.ref_order_hint[curSlot] = 0;  // not yet written
    }

    // Wire up sub-structure pointers
    stdPicInfo.pTileInfo        = &tileInfo;
    stdPicInfo.pQuantization    = &quantization;
    stdPicInfo.pLoopFilter      = &loopFilter;
    stdPicInfo.pCDEF            = &cdef;
    stdPicInfo.pLoopRestoration = &loopRestoration;
    stdPicInfo.pGlobalMotion    = &globalMotion;

    VkVideoEncodeAV1PictureInfoKHR av1PicInfo{};
    av1PicInfo.sType              = VK_STRUCTURE_TYPE_VIDEO_ENCODE_AV1_PICTURE_INFO_KHR;
    av1PicInfo.predictionMode     = isKeyFrame
        ? VK_VIDEO_ENCODE_AV1_PREDICTION_MODE_INTRA_ONLY_KHR
        : VK_VIDEO_ENCODE_AV1_PREDICTION_MODE_SINGLE_REFERENCE_KHR;
    av1PicInfo.rateControlGroup   = isKeyFrame
        ? VK_VIDEO_ENCODE_AV1_RATE_CONTROL_GROUP_INTRA_KHR
        : VK_VIDEO_ENCODE_AV1_RATE_CONTROL_GROUP_PREDICTIVE_KHR;
    uint32_t qIndex = crf_to_qindex(enc.crf);
    av1PicInfo.constantQIndex     = qIndex;
    av1PicInfo.pStdPictureInfo    = &stdPicInfo;

    // Reference name slot indices: для inter все 7 ref names → refSlot
    for (uint32_t r = 0; r < VK_MAX_VIDEO_AV1_REFERENCES_PER_FRAME_KHR; r++) {
        av1PicInfo.referenceNameSlotIndices[r] = isKeyFrame
            ? -1
            : static_cast<int32_t>(refSlot);
    }

    // src picture resource
    VkVideoPictureResourceInfoKHR inputPicRes{};
    inputPicRes.sType            = VK_STRUCTURE_TYPE_VIDEO_PICTURE_RESOURCE_INFO_KHR;
    inputPicRes.imageViewBinding = enc.srcImageView;
    inputPicRes.codedOffset      = {0, 0};
    inputPicRes.codedExtent      = {enc.width, enc.height};

    // VkVideoEncodeInfoKHR
    VkVideoEncodeInfoKHR encodeInfo{};
    encodeInfo.sType               = VK_STRUCTURE_TYPE_VIDEO_ENCODE_INFO_KHR;
    encodeInfo.pNext               = &av1PicInfo;
    encodeInfo.dstBuffer           = enc.bitstreamBuf;
    encodeInfo.dstBufferOffset     = 0;
    encodeInfo.dstBufferRange      = Av1rEncoder::BITSTREAM_BUF_SIZE;
    encodeInfo.srcPictureResource  = inputPicRes;
    encodeInfo.pSetupReferenceSlot = &setupSlot;
    if (!isKeyFrame) {
        encodeInfo.referenceSlotCount = 1;
        encodeInfo.pReferenceSlots    = &refSlotInfo;
    }

    // Query + encode + end
    vkCmdBeginQuery(cmd, enc.queryPool, querySlotId, 0);
    av1r_vk_video_funcs().CmdEncodeVideo(cmd, &encodeInfo);
    vkCmdEndQuery(cmd, enc.queryPool, querySlotId);

    VkVideoEndCodingInfoKHR endInfo{};
    endInfo.sType = VK_STRUCTURE_TYPE_VIDEO_END_CODING_INFO_KHR;
    av1r_vk_video_funcs().CmdEndVideoCoding(cmd, &endInfo);
}

// ============================================================================
// getOutputPacket: читаем bitstream после encode fence
// Прямой перенос из getOutputVideoPacket() строки 860-883
// ============================================================================
static size_t getOutputPacket(Av1rEncoder& enc, std::vector<uint8_t>& out)
{
    struct EncodeStatus {
        uint32_t bitstreamOffset;
        uint32_t bitstreamSize;
        VkQueryResultStatusKHR status;
    } result{};

    vkGetQueryPoolResults(enc.device, enc.queryPool, 0, 1,
                          sizeof(result), &result, sizeof(result),
                          VK_QUERY_RESULT_WITH_STATUS_BIT_KHR | VK_QUERY_RESULT_WAIT_BIT);

    if (result.status != VK_QUERY_RESULT_STATUS_COMPLETE_KHR || result.bitstreamSize == 0)
        return 0;

    const uint8_t* src = static_cast<const uint8_t*>(enc.bitstreamPtr)
                         + result.bitstreamOffset;
    out.insert(out.end(), src, src + result.bitstreamSize);
    return result.bitstreamSize;
}

// ============================================================================
// Cleanup
// ============================================================================
static void destroyEncoder(Av1rEncoder& enc)
{
    vkDeviceWaitIdle(enc.device);

    if (enc.bitstreamPtr)
        vkUnmapMemory(enc.device, enc.bitstreamMemory);
    if (enc.bitstreamBuf != VK_NULL_HANDLE)
        vkDestroyBuffer(enc.device, enc.bitstreamBuf, nullptr);
    if (enc.bitstreamMemory != VK_NULL_HANDLE)
        vkFreeMemory(enc.device, enc.bitstreamMemory, nullptr);

    if (enc.queryPool != VK_NULL_HANDLE)
        vkDestroyQueryPool(enc.device, enc.queryPool, nullptr);

    vkDestroyImageView(enc.device, enc.srcImageView, nullptr);
    vkDestroyImage(enc.device, enc.srcImage, nullptr);
    vkFreeMemory(enc.device, enc.srcMemory, nullptr);

    for (uint32_t i = 0; i < Av1rEncoder::DPB_COUNT; i++) {
        vkDestroyImageView(enc.device, enc.dpbImageViews[i], nullptr);
        vkDestroyImage(enc.device, enc.dpbImages[i], nullptr);
        vkFreeMemory(enc.device, enc.dpbMemory[i], nullptr);
    }

    if (enc.videoSessionParameters != VK_NULL_HANDLE)
        av1r_vk_video_funcs().DestroyVideoSessionParameters(enc.device, enc.videoSessionParameters, nullptr);
    if (enc.videoSession != VK_NULL_HANDLE)
        av1r_vk_video_funcs().DestroyVideoSession(enc.device, enc.videoSession, nullptr);
    for (auto& m : enc.sessionMemory)
        vkFreeMemory(enc.device, m, nullptr);

    if (enc.interQueueSemaphore != VK_NULL_HANDLE)
        vkDestroySemaphore(enc.device, enc.interQueueSemaphore, nullptr);
    if (enc.encodeFence != VK_NULL_HANDLE)
        vkDestroyFence(enc.device, enc.encodeFence, nullptr);
    if (enc.transferFence != VK_NULL_HANDLE)
        vkDestroyFence(enc.device, enc.transferFence, nullptr);
    if (enc.encodeCommandPool != VK_NULL_HANDLE)
        vkDestroyCommandPool(enc.device, enc.encodeCommandPool, nullptr);
    if (enc.transferCommandPool != VK_NULL_HANDLE)
        vkDestroyCommandPool(enc.device, enc.transferCommandPool, nullptr);
}

// ============================================================================
// Публичный API: av1r_vulkan_encode
// ============================================================================
// Streaming encoder context — allows frame-by-frame encoding
struct Av1rStreamEncoder {
    Av1rEncoder enc{};
    Av1rBuffer  staging{};
    size_t      frameBytes = 0;
    bool        ready = false;
};

// Initialize streaming encoder (call once before encoding frames)
void av1r_vulkan_encode_init(
    Av1rVulkanCtx&       ctx,
    Av1rStreamEncoder&   se,
    int width, int height, int fps, int crf)
{
    if (!ctx.initialized)
        throw std::runtime_error("Vulkan context not initialized");

    av1r_load_vk_video_funcs(ctx.instance, ctx.device);

    se.enc.physDevice     = ctx.physDevice;
    se.enc.device         = ctx.device;
    se.enc.encodeQueue    = ctx.encodeQueue.queue;
    se.enc.encodeQFam     = ctx.encodeQueue.queue_family_index;
    se.enc.transferQueue  = ctx.transferQueue.queue;
    se.enc.transferQFam   = ctx.transferQueue.queue_family_index;
    se.enc.width          = static_cast<uint32_t>(width  & ~1);
    se.enc.height         = static_cast<uint32_t>(height & ~1);
    se.enc.fps            = static_cast<uint32_t>(fps);
    se.enc.crf            = static_cast<uint32_t>(crf);

    createVideoSession(se.enc, crf);
    allocateVideoSessionMemory(se.enc);
    createVideoSessionParameters(se.enc);
    getSequenceHeader(se.enc);
    allocateImages(se.enc);
    allocateBitstreamBuffer(se.enc);
    createQueryPool(se.enc);

    se.enc.encodeCommandPool   = av1r_create_command_pool(se.enc.device, se.enc.encodeQFam);
    se.enc.transferCommandPool = av1r_create_command_pool(se.enc.device, se.enc.transferQFam);
    se.enc.encodeFence         = av1r_create_fence(se.enc.device);
    se.enc.transferFence       = av1r_create_fence(se.enc.device);
    se.enc.interQueueSemaphore = av1r_create_semaphore_binary(se.enc.device);

    se.frameBytes = static_cast<size_t>(width * height * 3 / 2);
    se.staging = av1r_buffer_create(
        se.enc.physDevice, se.enc.device, se.frameBytes,
        VK_BUFFER_USAGE_TRANSFER_SRC_BIT,
        VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT);

    // Rate control + DPB layout init
    {
        VkCommandBuffer initCmd = av1r_alloc_command_buffer(se.enc.device, se.enc.encodeCommandPool);
        av1r_begin_command_buffer(initCmd);
        initRateControl(se.enc, initCmd);
        transitionDpbImagesInitial(se.enc, initCmd);
        av1r_end_command_buffer(initCmd);

        VkSubmitInfo si{};
        si.sType              = VK_STRUCTURE_TYPE_SUBMIT_INFO;
        si.commandBufferCount = 1;
        si.pCommandBuffers    = &initCmd;
        av1r_reset_fence(se.enc.device, se.enc.encodeFence);
        VkResult initRes = vkQueueSubmit(se.enc.encodeQueue, 1, &si, se.enc.encodeFence);
        if (initRes != VK_SUCCESS)
            throw std::runtime_error("vkQueueSubmit (init) failed: " + std::to_string(initRes));
        av1r_wait_fence(se.enc.device, se.enc.encodeFence);
        vkFreeCommandBuffers(se.enc.device, se.enc.encodeCommandPool, 1, &initCmd);
    }

    se.ready = true;
}

// Encode one NV12 frame, append encoded packet to out_packet
void av1r_vulkan_encode_frame(
    Av1rStreamEncoder&    se,
    const uint8_t*        frame_nv12,
    int                   frame_index,
    std::vector<uint8_t>& out_packet)
{
    se.enc.frameCount = static_cast<uint32_t>(frame_index);

    // --- Step 1: Upload NV12 on transfer queue ---
    VkCommandBuffer xferCmd = av1r_alloc_command_buffer(se.enc.device, se.enc.transferCommandPool);
    av1r_begin_command_buffer(xferCmd);
    uploadNV12Frame(se.enc, xferCmd, frame_nv12, se.staging.buffer, se.staging.ptr);
    av1r_end_command_buffer(xferCmd);

    // Submit transfer, signal semaphore when done
    VkSubmitInfo xferSi{};
    xferSi.sType              = VK_STRUCTURE_TYPE_SUBMIT_INFO;
    xferSi.commandBufferCount = 1;
    xferSi.pCommandBuffers    = &xferCmd;
    xferSi.signalSemaphoreCount = 1;
    xferSi.pSignalSemaphores    = &se.enc.interQueueSemaphore;
    av1r_reset_fence(se.enc.device, se.enc.transferFence);
    VkResult xferRes = vkQueueSubmit(se.enc.transferQueue, 1, &xferSi, se.enc.transferFence);
    if (xferRes != VK_SUCCESS)
        throw std::runtime_error("vkQueueSubmit (transfer) failed: " + std::to_string(xferRes));

    // --- Step 2: Encode on encode queue, wait for transfer semaphore ---
    VkCommandBuffer encCmd = av1r_alloc_command_buffer(se.enc.device, se.enc.encodeCommandPool);
    av1r_begin_command_buffer(encCmd);
    encodeOneFrame(se.enc, encCmd);
    av1r_end_command_buffer(encCmd);

    VkPipelineStageFlags waitStage = VK_PIPELINE_STAGE_ALL_COMMANDS_BIT;
    VkSubmitInfo encSi{};
    encSi.sType                = VK_STRUCTURE_TYPE_SUBMIT_INFO;
    encSi.waitSemaphoreCount   = 1;
    encSi.pWaitSemaphores      = &se.enc.interQueueSemaphore;
    encSi.pWaitDstStageMask    = &waitStage;
    encSi.commandBufferCount   = 1;
    encSi.pCommandBuffers      = &encCmd;
    av1r_reset_fence(se.enc.device, se.enc.encodeFence);
    VkResult encRes = vkQueueSubmit(se.enc.encodeQueue, 1, &encSi, se.enc.encodeFence);
    if (encRes != VK_SUCCESS)
        throw std::runtime_error("vkQueueSubmit (encode) failed: " + std::to_string(encRes));

    // Wait for both to finish
    av1r_wait_fence(se.enc.device, se.enc.transferFence);
    av1r_wait_fence(se.enc.device, se.enc.encodeFence);

    out_packet.clear();
    // Prepend sequence header OBU before every keyframe (not just the first).
    // AV1 decoders need the sequence header to start decoding at any keyframe,
    // which is required for seek / random access to work.
    const uint32_t GOP_LEN = 16;
    bool isKey = (se.enc.frameCount % GOP_LEN == 0);
    if (isKey && !se.enc.seqHeaderData.empty()) {
        out_packet.insert(out_packet.end(),
                          se.enc.seqHeaderData.begin(),
                          se.enc.seqHeaderData.end());
    }
    getOutputPacket(se.enc, out_packet);


    vkFreeCommandBuffers(se.enc.device, se.enc.transferCommandPool, 1, &xferCmd);
    vkFreeCommandBuffers(se.enc.device, se.enc.encodeCommandPool, 1, &encCmd);
}

// Cleanup streaming encoder
void av1r_vulkan_encode_finish(Av1rStreamEncoder& se) {
    av1r_buffer_destroy(se.enc.device, se.staging);
    destroyEncoder(se.enc);
    se.ready = false;
}

// Opaque API (used from av1r_bindings.cpp via av1r_stream_encoder.h)
Av1rStreamEncoder* av1r_vulkan_stream_new() { return new Av1rStreamEncoder{}; }

void av1r_vulkan_stream_init(Av1rVulkanCtx& ctx, Av1rStreamEncoder* se,
                              int w, int h, int fps, int crf) {
    av1r_vulkan_encode_init(ctx, *se, w, h, fps, crf);
}
void av1r_vulkan_stream_encode(Av1rStreamEncoder* se, const uint8_t* frame,
                                int idx, std::vector<uint8_t>& pkt) {
    av1r_vulkan_encode_frame(*se, frame, idx, pkt);
}
void av1r_vulkan_stream_finish(Av1rStreamEncoder* se) {
    av1r_vulkan_encode_finish(*se);
}
void av1r_vulkan_stream_delete(Av1rStreamEncoder* se) { delete se; }

#endif // AV1R_VULKAN_VIDEO_AV1
