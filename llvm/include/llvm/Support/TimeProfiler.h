//===- llvm/Support/TimeProfiler.h - Hierarchical Time Profiler -*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_SUPPORT_TIME_PROFILER_H
#define LLVM_SUPPORT_TIME_PROFILER_H

#include "llvm/Support/raw_ostream.h"

namespace llvm {

struct TracingProfiler;
extern TracingProfiler *TracingProfilerInstance;

/// Initialize the time trace profiler.
/// This sets up the global \p TracingProfilerInstance
/// variable to be the profiler instance.
void tracingProfilerInitialize(unsigned TimeTraceGranularity);

/// Cleanup the time trace profiler, if it was initialized.
void tracingProfilerCleanup();

/// Is the time trace profiler enabled, i.e. initialized?
inline bool tracingProfilerEnabled() {
  return TracingProfilerInstance != nullptr;
}

/// Write profiling data to output file.
/// Data produced is JSON, in Chrome "Trace Event" format, see
/// https://docs.google.com/document/d/1CvAClvFfyA5R-PhYUmn5OOQtYMH4h6I0nSsKchNAySU/preview
void tracingProfilerWrite(raw_pwrite_stream &OS);

/// Manually begin a time section, with the given \p Name and \p Detail.
/// Profiler copies the string data, so the pointers can be given into
/// temporaries. Time sections can be hierarchical; every Begin must have a
/// matching End pair but they can nest.
void tracingProfilerBeginTimeEvent(StringRef Name, StringRef Detail);
void tracingProfilerBeginTimeEvent(StringRef Name,
                            llvm::function_ref<std::string()> Detail);

void tracingProfilerCounterEvent(StringRef Name,  StringRef ArgName, int Count);

/// Manually end the last time section.
void tracingProfilerEndTimeEvent();

/// The TimeTraceScope is a helper class to call the begin and end functions
/// of the time trace profiler.  When the object is constructed, it begins
/// the section; and when it is destroyed, it stops it. If the time profiler
/// is not initialized, the overhead is a single branch.
struct TimeTraceScope {

  TimeTraceScope() = delete;
  TimeTraceScope(const TimeTraceScope &) = delete;
  TimeTraceScope &operator=(const TimeTraceScope &) = delete;
  TimeTraceScope(TimeTraceScope &&) = delete;
  TimeTraceScope &operator=(TimeTraceScope &&) = delete;

  TimeTraceScope(StringRef Name, StringRef Detail) {
    if (TracingProfilerInstance != nullptr)
      tracingProfilerBeginTimeEvent(Name, Detail);
  }
  TimeTraceScope(StringRef Name, llvm::function_ref<std::string()> Detail) {
    if (TracingProfilerInstance != nullptr)
      tracingProfilerBeginTimeEvent(Name, Detail);
  }
  ~TimeTraceScope() {
    if (TracingProfilerInstance != nullptr)
      tracingProfilerEndTimeEvent();
  }
};

} // end namespace llvm

#endif
