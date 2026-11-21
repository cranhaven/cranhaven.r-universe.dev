#include <stdexcept>

namespace debug{

#define ASSERT(bool_expr, msg) if(!static_cast<bool>(bool_expr)) { throw std::runtime_error(msg); }

}
