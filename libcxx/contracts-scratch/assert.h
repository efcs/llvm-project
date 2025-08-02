

#include_next <assert.h>

#if ! __is_identifier(contract_assert)
#undef assert

#ifdef NDEBUG
#define assert(...) ({contract_assert [[clang::contract_semantic("ignore")]] (__VA_ARGS__); static_cast<void>(0); })
#else
#define assert(...) contract_assert [[clang::contract_semantic("enforce")]]  (__VA_ARGS__)
#endif
#endif
