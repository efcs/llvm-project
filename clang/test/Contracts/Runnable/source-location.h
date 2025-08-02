#ifndef CONTRACTS_SOURCE_LOCATION_H
#define CONTRACTS_SOURCE_LOCATION_H

namespace std {

class source_location {
  // The names source_location::__impl, _M_file_name, _M_function_name, _M_line, and _M_column
  // are hard-coded in the compiler and must not be changed here.
  struct __impl {
    const char* _M_file_name;
    const char* _M_function_name;
    unsigned _M_line;
    unsigned _M_column;
  };

  // This is a terrible hack to allow contract_violation to construct a source_location
  // from a different typed-ed struct with the same layout and members (so it should be well defined)
  const __impl* __ptr_ = nullptr;
  using __bsl_ty = decltype(__builtin_source_location());

public:
  static std::source_location __create_from_pointer(const void* __ptr) {
    std::source_location __loc;
    __loc.__ptr_ = static_cast<const __impl*>(__ptr);
    return __loc;
  }

public:
  // The defaulted __ptr argument is necessary so that the builtin is evaluated
  // in the context of the caller. An explicit value should never be provided.
  static consteval source_location current(__bsl_ty __ptr = __builtin_source_location()) noexcept {
    source_location __sl;
    __sl.__ptr_ = static_cast<const __impl*>(__ptr);
    return __sl;
  }
  constexpr source_location() noexcept = default;

  constexpr unsigned line() const noexcept {
    return __ptr_ != nullptr ? __ptr_->_M_line : 0;
  }
  constexpr unsigned column() const noexcept {
    return __ptr_ != nullptr ? __ptr_->_M_column : 0;
  }
  constexpr const char* file_name() const noexcept {
    return __ptr_ != nullptr ? __ptr_->_M_file_name : "";
  }
  constexpr const char* function_name() const noexcept {
    return __ptr_ != nullptr ? __ptr_->_M_function_name : "";
  }
};
} // namespace std

#endif 
