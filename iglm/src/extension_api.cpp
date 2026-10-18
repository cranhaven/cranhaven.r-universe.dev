

#include "iglm/extension_api.hpp"

namespace iglm {
class XYZ_class; 

Registry& Registry::instance() {
  static Registry inst;
  return inst;
}

bool Registry::add(const std::string& name,
                   ExtFn fn,
                   const std::string& short_name,
                   double value)
{
  std::lock_guard<std::mutex> lock(mu_);
  auto result = map_.emplace(name, FUN{fn, short_name, value});
  return result.second; 
}

bool Registry::has(const std::string& name) const {
  std::lock_guard<std::mutex> lock(mu_);
  return map_.count(name); 
}

ExtFn Registry::get(const std::string& name) const {
  std::lock_guard<std::mutex> lock(mu_);
  auto it = map_.find(name);
  if (it == map_.end())
    throw std::out_of_range("No extension named '" + name + "'");
  return it->second.fn; 
}

FUN Registry::info(const std::string& name) const {
  std::lock_guard<std::mutex> lock(mu_);
  auto it = map_.find(name);
  if (it == map_.end())
    throw std::out_of_range("No extension named '" + name + "'");
  return it->second; 
}

std::vector<std::string> Registry::names() const {
  std::lock_guard<std::mutex> lock(mu_);
  std::vector<std::string> out;
  out.reserve(map_.size());
  for (auto& kv : map_) {
    out.push_back(kv.first);
  }
  return out; 
}

std::vector<FUN> Registry::all_meta() const {
  std::lock_guard<std::mutex> lock(mu_);
  std::vector<FUN> out;
  out.reserve(map_.size());
  for (auto& kv : map_) {
    out.push_back(kv.second);
  }
  return out; 
}

} // namespace iglm

extern "C" void iglm_register_term_C(const char* name, void* fn_ptr, const char* short_name, double value) {
    if (!fn_ptr) {
        Rcpp::stop("Invalid function pointer passed to iglm_register_term_C");
    }
    std::string n(name);
    std::string sn(short_name);
    iglm::Registry::instance().add(n, (iglm::ExtFn)fn_ptr, sn, value);
}

// [[Rcpp::init]]
void iglm_init_callable(DllInfo *dll) {
    R_RegisterCCallable("iglm", "iglm_register_term_C", (DL_FUNC)iglm_register_term_C);
}
