#ifndef EVENT_SEQUENCE_H
#define EVENT_SEQUENCE_H

#include <vector>
#include <string>
#include <iostream>
#ifdef NDEBUG
#undef NDEBUG
#endif
#include <cassert>

namespace impl {
template <class T>
std::string to_string(std::vector<T> const& vec) {
  std::string result = "[ ";
  for (auto event : vec) {
    if constexpr (std::is_same_v<T, std::string>) {
      result += "\"" + event + "\", ";
    } else {
      result += event + ", ";
    }
  }
  result = result.erase(result.size() - 2, 2);
  result += " ]";
  return result;
}
}

struct EventSequence {
  std::vector<std::string> Events;


  bool operator()(const char* event) {
    Events.push_back(event);
    return true;
  }

  template <class ReturnT>
  ReturnT operator()(const char* event, ReturnT ret) {
    Events.push_back(event);
    return ret;
  }



  std::string to_string() const {
    return impl::to_string(Events);
  }

};

#endif // EVENT_SEQUENCE_H