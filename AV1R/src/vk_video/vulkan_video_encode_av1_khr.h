// Polyfill for VK_KHR_video_encode_av1 Vulkan API types
// Required when building against Vulkan SDK < 1.3.290 which lacks this extension.
// Definitions extracted from Khronos vulkan_core.h (VK_KHR_video_encode_av1 section).
// The codec-level types (StdVideoAV1*, StdVideoEncodeAV1*) come from the bundled
// vk_video/vulkan_video_codec_av1std.h and vulkan_video_codec_av1std_encode.h headers.

#ifndef VULKAN_VIDEO_ENCODE_AV1_KHR_H_
#define VULKAN_VIDEO_ENCODE_AV1_KHR_H_

#include <vulkan/vulkan.h>
#include "vulkan_video_codec_av1std.h"
#include "vulkan_video_codec_av1std_encode.h"

// Only define if the system SDK does not already provide these
#ifndef VK_KHR_video_encode_av1

#define VK_KHR_video_encode_av1 1
#define VK_KHR_VIDEO_ENCODE_AV1_SPEC_VERSION 1
#define VK_KHR_VIDEO_ENCODE_AV1_EXTENSION_NAME "VK_KHR_video_encode_av1"
#define VK_MAX_VIDEO_AV1_REFERENCES_PER_FRAME_KHR 7U

// VkStructureType values
static const VkStructureType VK_STRUCTURE_TYPE_VIDEO_ENCODE_AV1_CAPABILITIES_KHR                   = (VkStructureType)1000513000;
static const VkStructureType VK_STRUCTURE_TYPE_VIDEO_ENCODE_AV1_SESSION_PARAMETERS_CREATE_INFO_KHR  = (VkStructureType)1000513001;
static const VkStructureType VK_STRUCTURE_TYPE_VIDEO_ENCODE_AV1_PICTURE_INFO_KHR                    = (VkStructureType)1000513002;
static const VkStructureType VK_STRUCTURE_TYPE_VIDEO_ENCODE_AV1_DPB_SLOT_INFO_KHR                   = (VkStructureType)1000513003;
static const VkStructureType VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VIDEO_ENCODE_AV1_FEATURES_KHR        = (VkStructureType)1000513004;
static const VkStructureType VK_STRUCTURE_TYPE_VIDEO_ENCODE_AV1_PROFILE_INFO_KHR                    = (VkStructureType)1000513005;
static const VkStructureType VK_STRUCTURE_TYPE_VIDEO_ENCODE_AV1_RATE_CONTROL_INFO_KHR               = (VkStructureType)1000513006;
static const VkStructureType VK_STRUCTURE_TYPE_VIDEO_ENCODE_AV1_RATE_CONTROL_LAYER_INFO_KHR         = (VkStructureType)1000513007;
static const VkStructureType VK_STRUCTURE_TYPE_VIDEO_ENCODE_AV1_QUALITY_LEVEL_PROPERTIES_KHR        = (VkStructureType)1000513008;
static const VkStructureType VK_STRUCTURE_TYPE_VIDEO_ENCODE_AV1_SESSION_CREATE_INFO_KHR             = (VkStructureType)1000513009;
static const VkStructureType VK_STRUCTURE_TYPE_VIDEO_ENCODE_AV1_GOP_REMAINING_FRAME_INFO_KHR        = (VkStructureType)1000513010;

// VkVideoCodecOperationFlagBitsKHR
static const VkVideoCodecOperationFlagBitsKHR VK_VIDEO_CODEC_OPERATION_ENCODE_AV1_BIT_KHR = (VkVideoCodecOperationFlagBitsKHR)0x00040000;

// ---------------------------------------------------------------------------
// Enums
// ---------------------------------------------------------------------------
typedef enum VkVideoEncodeAV1PredictionModeKHR {
    VK_VIDEO_ENCODE_AV1_PREDICTION_MODE_INTRA_ONLY_KHR              = 0,
    VK_VIDEO_ENCODE_AV1_PREDICTION_MODE_SINGLE_REFERENCE_KHR        = 1,
    VK_VIDEO_ENCODE_AV1_PREDICTION_MODE_UNIDIRECTIONAL_COMPOUND_KHR = 2,
    VK_VIDEO_ENCODE_AV1_PREDICTION_MODE_BIDIRECTIONAL_COMPOUND_KHR  = 3,
    VK_VIDEO_ENCODE_AV1_PREDICTION_MODE_MAX_ENUM_KHR                = 0x7FFFFFFF
} VkVideoEncodeAV1PredictionModeKHR;

typedef enum VkVideoEncodeAV1RateControlGroupKHR {
    VK_VIDEO_ENCODE_AV1_RATE_CONTROL_GROUP_INTRA_KHR          = 0,
    VK_VIDEO_ENCODE_AV1_RATE_CONTROL_GROUP_PREDICTIVE_KHR     = 1,
    VK_VIDEO_ENCODE_AV1_RATE_CONTROL_GROUP_BIPREDICTIVE_KHR   = 2,
    VK_VIDEO_ENCODE_AV1_RATE_CONTROL_GROUP_MAX_ENUM_KHR       = 0x7FFFFFFF
} VkVideoEncodeAV1RateControlGroupKHR;

typedef enum VkVideoEncodeAV1CapabilityFlagBitsKHR {
    VK_VIDEO_ENCODE_AV1_CAPABILITY_PER_RATE_CONTROL_GROUP_MIN_MAX_Q_INDEX_BIT_KHR = 0x00000001,
    VK_VIDEO_ENCODE_AV1_CAPABILITY_GENERATE_OBU_EXTENSION_HEADER_BIT_KHR         = 0x00000002,
    VK_VIDEO_ENCODE_AV1_CAPABILITY_PRIMARY_REFERENCE_CDF_ONLY_BIT_KHR            = 0x00000004,
    VK_VIDEO_ENCODE_AV1_CAPABILITY_FRAME_SIZE_OVERRIDE_BIT_KHR                   = 0x00000008,
    VK_VIDEO_ENCODE_AV1_CAPABILITY_MOTION_VECTOR_SCALING_BIT_KHR                 = 0x00000010,
    VK_VIDEO_ENCODE_AV1_CAPABILITY_COMPOUND_PREDICTION_INTRA_REFRESH_BIT_KHR     = 0x00000020,
    VK_VIDEO_ENCODE_AV1_CAPABILITY_FLAG_BITS_MAX_ENUM_KHR                        = 0x7FFFFFFF
} VkVideoEncodeAV1CapabilityFlagBitsKHR;
typedef VkFlags VkVideoEncodeAV1CapabilityFlagsKHR;

typedef enum VkVideoEncodeAV1StdFlagBitsKHR {
    VK_VIDEO_ENCODE_AV1_STD_UNIFORM_TILE_SPACING_FLAG_SET_BIT_KHR = 0x00000001,
    VK_VIDEO_ENCODE_AV1_STD_SKIP_MODE_PRESENT_UNSET_BIT_KHR      = 0x00000002,
    VK_VIDEO_ENCODE_AV1_STD_PRIMARY_REF_FRAME_BIT_KHR            = 0x00000004,
    VK_VIDEO_ENCODE_AV1_STD_DELTA_Q_BIT_KHR                      = 0x00000008,
    VK_VIDEO_ENCODE_AV1_STD_FLAG_BITS_MAX_ENUM_KHR               = 0x7FFFFFFF
} VkVideoEncodeAV1StdFlagBitsKHR;
typedef VkFlags VkVideoEncodeAV1StdFlagsKHR;

typedef enum VkVideoEncodeAV1SuperblockSizeFlagBitsKHR {
    VK_VIDEO_ENCODE_AV1_SUPERBLOCK_SIZE_64_BIT_KHR              = 0x00000001,
    VK_VIDEO_ENCODE_AV1_SUPERBLOCK_SIZE_128_BIT_KHR             = 0x00000002,
    VK_VIDEO_ENCODE_AV1_SUPERBLOCK_SIZE_FLAG_BITS_MAX_ENUM_KHR  = 0x7FFFFFFF
} VkVideoEncodeAV1SuperblockSizeFlagBitsKHR;
typedef VkFlags VkVideoEncodeAV1SuperblockSizeFlagsKHR;

typedef enum VkVideoEncodeAV1RateControlFlagBitsKHR {
    VK_VIDEO_ENCODE_AV1_RATE_CONTROL_REGULAR_GOP_BIT_KHR                     = 0x00000001,
    VK_VIDEO_ENCODE_AV1_RATE_CONTROL_TEMPORAL_LAYER_PATTERN_DYADIC_BIT_KHR   = 0x00000002,
    VK_VIDEO_ENCODE_AV1_RATE_CONTROL_REFERENCE_PATTERN_FLAT_BIT_KHR          = 0x00000004,
    VK_VIDEO_ENCODE_AV1_RATE_CONTROL_REFERENCE_PATTERN_DYADIC_BIT_KHR        = 0x00000008,
    VK_VIDEO_ENCODE_AV1_RATE_CONTROL_FLAG_BITS_MAX_ENUM_KHR                  = 0x7FFFFFFF
} VkVideoEncodeAV1RateControlFlagBitsKHR;
typedef VkFlags VkVideoEncodeAV1RateControlFlagsKHR;

// ---------------------------------------------------------------------------
// Structs
// ---------------------------------------------------------------------------
typedef struct VkVideoEncodeAV1ProfileInfoKHR {
    VkStructureType       sType;
    const void*           pNext;
    StdVideoAV1Profile    stdProfile;
} VkVideoEncodeAV1ProfileInfoKHR;

typedef struct VkVideoEncodeAV1QIndexKHR {
    uint32_t    intraQIndex;
    uint32_t    predictiveQIndex;
    uint32_t    bipredictiveQIndex;
} VkVideoEncodeAV1QIndexKHR;

typedef struct VkVideoEncodeAV1CapabilitiesKHR {
    VkStructureType                           sType;
    void*                                     pNext;
    VkVideoEncodeAV1CapabilityFlagsKHR        flags;
    StdVideoAV1Level                          maxLevel;
    VkExtent2D                                codedPictureAlignment;
    VkExtent2D                                maxTiles;
    VkExtent2D                                minTileSize;
    VkExtent2D                                maxTileSize;
    VkVideoEncodeAV1SuperblockSizeFlagsKHR    superblockSizes;
    uint32_t                                  maxSingleReferenceCount;
    uint32_t                                  singleReferenceNameMask;
    uint32_t                                  maxUnidirectionalCompoundReferenceCount;
    uint32_t                                  maxUnidirectionalCompoundGroup1ReferenceCount;
    uint32_t                                  unidirectionalCompoundReferenceNameMask;
    uint32_t                                  maxBidirectionalCompoundReferenceCount;
    uint32_t                                  maxBidirectionalCompoundGroup1ReferenceCount;
    uint32_t                                  maxBidirectionalCompoundGroup2ReferenceCount;
    uint32_t                                  bidirectionalCompoundReferenceNameMask;
    uint32_t                                  maxTemporalLayerCount;
    uint32_t                                  maxSpatialLayerCount;
    uint32_t                                  maxOperatingPoints;
    uint32_t                                  minQIndex;
    uint32_t                                  maxQIndex;
    VkBool32                                  prefersGopRemainingFrames;
    VkBool32                                  requiresGopRemainingFrames;
    VkVideoEncodeAV1StdFlagsKHR               stdSyntaxFlags;
} VkVideoEncodeAV1CapabilitiesKHR;

typedef struct VkVideoEncodeAV1SessionParametersCreateInfoKHR {
    VkStructureType                               sType;
    const void*                                   pNext;
    const StdVideoAV1SequenceHeader*              pStdSequenceHeader;
    const StdVideoEncodeAV1DecoderModelInfo*      pStdDecoderModelInfo;
    uint32_t                                      stdOperatingPointCount;
    const StdVideoEncodeAV1OperatingPointInfo*    pStdOperatingPoints;
} VkVideoEncodeAV1SessionParametersCreateInfoKHR;

typedef struct VkVideoEncodeAV1PictureInfoKHR {
    VkStructureType                        sType;
    const void*                            pNext;
    VkVideoEncodeAV1PredictionModeKHR      predictionMode;
    VkVideoEncodeAV1RateControlGroupKHR    rateControlGroup;
    uint32_t                               constantQIndex;
    const StdVideoEncodeAV1PictureInfo*    pStdPictureInfo;
    int32_t                                referenceNameSlotIndices[VK_MAX_VIDEO_AV1_REFERENCES_PER_FRAME_KHR];
    VkBool32                               primaryReferenceCdfOnly;
    VkBool32                               generateObuExtensionHeader;
} VkVideoEncodeAV1PictureInfoKHR;

typedef struct VkVideoEncodeAV1DpbSlotInfoKHR {
    VkStructureType                          sType;
    const void*                              pNext;
    const StdVideoEncodeAV1ReferenceInfo*    pStdReferenceInfo;
} VkVideoEncodeAV1DpbSlotInfoKHR;

typedef struct VkVideoEncodeAV1FrameSizeKHR {
    uint32_t    intraFrameSize;
    uint32_t    predictiveFrameSize;
    uint32_t    bipredictiveFrameSize;
} VkVideoEncodeAV1FrameSizeKHR;

typedef struct VkVideoEncodeAV1RateControlInfoKHR {
    VkStructureType                        sType;
    const void*                            pNext;
    VkVideoEncodeAV1RateControlFlagsKHR    flags;
    uint32_t                               gopFrameCount;
    uint32_t                               keyFramePeriod;
    uint32_t                               consecutiveBipredictiveFrameCount;
    uint32_t                               temporalLayerCount;
} VkVideoEncodeAV1RateControlInfoKHR;

typedef struct VkVideoEncodeAV1RateControlLayerInfoKHR {
    VkStructureType                 sType;
    const void*                     pNext;
    VkBool32                        useMinQIndex;
    VkVideoEncodeAV1QIndexKHR       minQIndex;
    VkBool32                        useMaxQIndex;
    VkVideoEncodeAV1QIndexKHR       maxQIndex;
    VkBool32                        useMaxFrameSize;
    VkVideoEncodeAV1FrameSizeKHR    maxFrameSize;
} VkVideoEncodeAV1RateControlLayerInfoKHR;

typedef struct VkVideoEncodeAV1GopRemainingFrameInfoKHR {
    VkStructureType    sType;
    const void*        pNext;
    VkBool32           useGopRemainingFrames;
    uint32_t           gopRemainingIntra;
    uint32_t           gopRemainingPredictive;
    uint32_t           gopRemainingBipredictive;
} VkVideoEncodeAV1GopRemainingFrameInfoKHR;

typedef struct VkPhysicalDeviceVideoEncodeAV1FeaturesKHR {
    VkStructureType    sType;
    void*              pNext;
    VkBool32           videoEncodeAV1;
} VkPhysicalDeviceVideoEncodeAV1FeaturesKHR;

#endif // VK_KHR_video_encode_av1
#endif // VULKAN_VIDEO_ENCODE_AV1_KHR_H_
