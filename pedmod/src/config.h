#ifndef PEDMOD_CONFIG_H
#define PEDMOD_CONFIG_H

#ifdef DO_CHECKS
#define PEDMOD_NOEXCEPT
#else
#define PEDMOD_NOEXCEPT noexcept
#endif

#if defined(__SUNPRO_C) || defined(__SUNPRO_CC)
#define PEDMOD_RESTRICT
#else
#define PEDMOD_RESTRICT __restrict__
#endif

namespace pedmod {

inline constexpr size_t cacheline_size(){
  return 128L;
}

} // namespace pedmod

#endif
