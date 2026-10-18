#pragma once

#include <RcppArmadillo.h>
#include <string>
#include <unordered_map>
#include <mutex>
#include <stdexcept>
#include <vector>

/*
 * This logic handles symbol visibility.
 * - When your package (iglm) is compiling, it must define
 * IGLM_COMPILING_IGLM. This tells the compiler to EXPORT symbols.
 * - When another package (OtherPackage) includes this header, this
 * define will be absent, and the compiler will IMPORT symbols.
 *
 * You set IGLM_COMPILING_IGLM in your src/Makevars file.
 */
#ifndef IGLM_API
#if defined(_WIN32)
#ifdef IGLM_COMPILING_IGLM
#define IGLM_API __declspec(dllexport)
#else
#define IGLM_API __declspec(dllimport)
#endif
#else
// On Linux/macOS, we need to ensure symbols are visible even if loaded with R_LD_LOCAL
#define IGLM_API __attribute__ ((visibility ("default")))
#endif
#endif
class XYZ_class; 

#include "iglm/xyz_class.h"

namespace iglm {

// --- Function type (Unchanged) ---
using ExtFn = double(*)(const ::XYZ_class&,const int&,const int&,const arma::mat&,
                     const double&,const std::string&,const bool&);


struct FUN {
  ExtFn fn;
  std::string short_name;
  double value;
};


class IGLM_API Registry {
public:
  
  static Registry& instance();
  bool add(const std::string& name,
           ExtFn fn,
           const std::string& short_name,
           double value);
  
  bool has(const std::string& name) const;
  
  ExtFn get(const std::string& name) const;
  
  FUN info(const std::string& name) const;
  
  std::vector<std::string> names() const;
  
  std::vector<FUN> all_meta() const;
  
private:
  Registry() = default;
  Registry(const Registry&) = delete;
  Registry& operator=(const Registry&) = delete;
  
  mutable std::mutex mu_;
  std::unordered_map<std::string, FUN> map_;
}; 

struct Registrar {
  Registrar(const std::string& name,
            ExtFn fn,
            const std::string& short_name,
            double value)
  {
#ifdef IGLM_COMPILING_IGLM
    // When compiling iglm itself, call the registry directly.
    if (!Registry::instance().add(name, fn, short_name, value)) {
      Rcpp::Rcerr << "Duplicate extension name '" << name << "' ignored.\n";
    }
#else
    // When compiling an extension package, call through R_GetCCallable so that
    // the registration always lands in iglm's singleton — not a duplicate one
    // created by the extension's DLL.
    typedef void (*reg_fn_t)(const char*, void*, const char*, double);
    reg_fn_t reg = (reg_fn_t)R_GetCCallable("iglm", "iglm_register_term_C");
    if (reg) {
      reg(name.c_str(), (void*)fn, short_name.c_str(), value);
    }
#endif
  }
};

#define iglm_JOIN_IMPL(a,b) a##b
#define iglm_JOIN(a,b)      iglm_JOIN_IMPL(a,b)

#if defined(__COUNTER__)  // works on GCC/Clang/MSVC
#define iglm_UNIQ(prefix) iglm_JOIN(prefix, __COUNTER__)
#else                     // portable fallback
#define iglm_UNIQ(prefix) iglm_JOIN(prefix, __LINE__)
#endif

#define EFFECT_REGISTER(NAME, FN, SHORT, VAL) \
static ::iglm::Registrar iglm_UNIQ(_iglm_registrar_){ (NAME), (FN), (SHORT), (VAL) }

} // namespace iglm
