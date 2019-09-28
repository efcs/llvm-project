//===-- TimeProfiler.cpp - Hierarchical Time Profiler ---------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file implements hierarchical time profiler.
//
//===----------------------------------------------------------------------===//

#include "llvm/Support/TimeProfiler.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/JSON.h"
#include <cassert>
#include <chrono>
#include <string>
#include <vector>

using namespace std::chrono;

namespace llvm {

TracingProfiler *TracingProfilerInstance = nullptr;

typedef duration<steady_clock::rep, steady_clock::period> DurationType;
typedef time_point<steady_clock> TimePointType;
typedef std::pair<size_t, DurationType> CountAndDurationType;
typedef std::pair<std::string, CountAndDurationType>
    NameAndCountAndDurationType;

struct TimeTraceEntry {
  TimePointType Start;
  TimePointType End;
  std::string Name;
  std::string Detail;

  TimeTraceEntry(TimePointType &&S, TimePointType &&E, std::string &&N, std::string &&Dt)
      : Start(std::move(S)), End(std::move(E)), Name(std::move(N)),
        Detail(std::move(Dt)){};

  // Calculate timings for FlameGraph. Cast time points to microsecond precision
  // rather than casting duration. This avoid truncation issues causing inner
  // scopes overruning outer scopes.
  steady_clock::rep getFlameGraphStartUs(TimePointType StartTime) const {
    return (time_point_cast<microseconds>(Start) -
            time_point_cast<microseconds>(StartTime))
        .count();
  }

  steady_clock::rep getFlameGraphDurUs() const {
    return (time_point_cast<microseconds>(End) -
            time_point_cast<microseconds>(Start))
        .count();
  }
};

struct CounterEntry {
  std::string Name;
  TimePointType Start;
  std::string CounterName;
  int Value;

  steady_clock::rep getFlameGraphStartUs(TimePointType StartTime) const {
    return (time_point_cast<microseconds>(Start) -
            time_point_cast<microseconds>(StartTime))
        .count();
  }

  CounterEntry(StringRef Name, TimePointType Start, StringRef CounterName, int Value)
    : Name(Name), Start(Start), CounterName(CounterName), Value(Value) {}
};

struct TracingProfiler {
  TracingProfiler() {
    StartTime = steady_clock::now();
  }


  void Write(raw_pwrite_stream &OS) {
    assert(TimeEvents.Stack.empty() &&
           "All profiler sections should be ended when calling Write");
    json::OStream J(OS);
    J.objectBegin();
    J.attributeBegin("traceEvents");
    J.arrayBegin();

    TimeEvents.Write(*this, J);
    CounterEvents.Write(*this, J);

    // Emit metadata event with process name.
    J.object([&] {
      J.attribute("cat", "");
      J.attribute("pid", 1);
      J.attribute("tid", 0);
      J.attribute("ts", 0);
      J.attribute("ph", "M");
      J.attribute("name", "process_name");
      J.attributeObject("args", [&] { J.attribute("name", "clang"); });
    });

    J.arrayEnd();
    J.attributeEnd();
    J.objectEnd();
  }

  TimePointType StartTime;

  struct TimeEventsData {
    SmallVector<TimeTraceEntry, 16> Stack;
    SmallVector<TimeTraceEntry, 128> Entries;
    StringMap<CountAndDurationType> CountAndTotalPerName;
    // Minimum time granularity (in microseconds)
    unsigned TimeTraceGranularity;

    void Write(TracingProfiler&, json::OStream& J);
    void begin(std::string Name, llvm::function_ref<std::string()> Detail);
    void end(TracingProfiler&);
  } TimeEvents;

  struct CounterEventsData {
    SmallVector<CounterEntry, 128> Entries;

    void add(StringRef Name, StringRef ArgName, int Value) {
      Entries.emplace_back(Name, steady_clock::now(), ArgName, Value);
    }

    void Write(TracingProfiler&, json::OStream& J);
  } CounterEvents;

};


void TracingProfiler::CounterEventsData::Write(TracingProfiler& TP, json::OStream& J) {
  // Emit all events for the main flame graph.
    for (const auto &E : Entries) {
      J.object([&]{
        J.attribute("pid", 1);
        J.attribute("tid", 0);
        J.attribute("ph", "C");
        J.attribute("ts", E.getFlameGraphStartUs(TP.StartTime));
        J.attribute("name", E.Name);
        J.attributeObject("args", [&] {
          J.attribute(E.CounterName, E.Value);
        });
      });
    }
}

void TracingProfiler::TimeEventsData::Write(TracingProfiler& TP, json::OStream& J) {
  // Emit all events for the main flame graph.
    for (const auto &E : Entries) {
      auto StartUs = E.getFlameGraphStartUs(TP.StartTime);
      auto DurUs = E.getFlameGraphDurUs();

      J.object([&]{
        J.attribute("pid", 1);
        J.attribute("tid", 0);
        J.attribute("ph", "X");
        J.attribute("ts", StartUs);
        J.attribute("dur", DurUs);
        J.attribute("name", E.Name);
        J.attributeObject("args", [&] { J.attribute("detail", E.Detail); });
      });
    }

    // Emit totals by section name as additional "thread" events, sorted from
    // longest one.
    int Tid = 1;
    std::vector<NameAndCountAndDurationType> SortedTotals;
    SortedTotals.reserve(CountAndTotalPerName.size());
    for (const auto &E : CountAndTotalPerName)
      SortedTotals.emplace_back(E.getKey(), E.getValue());

    llvm::sort(SortedTotals.begin(), SortedTotals.end(),
               [](const NameAndCountAndDurationType &A,
                  const NameAndCountAndDurationType &B) {
                 return A.second.second > B.second.second;
               });
    for (const auto &E : SortedTotals) {
      auto DurUs = duration_cast<microseconds>(E.second.second).count();
      auto Count = CountAndTotalPerName[E.first].first;

      J.object([&]{
        J.attribute("pid", 1);
        J.attribute("tid", Tid);
        J.attribute("ph", "X");
        J.attribute("ts", 0);
        J.attribute("dur", DurUs);
        J.attribute("name", "Total " + E.first);
        J.attributeObject("args", [&] {
          J.attribute("count", int64_t(Count));
          J.attribute("avg ms", int64_t(DurUs / Count / 1000));
        });
      });

      ++Tid;
    }
}

void TracingProfiler::TimeEventsData::begin(std::string Name, llvm::function_ref<std::string()> Detail) {
  Stack.emplace_back(steady_clock::now(), TimePointType(), std::move(Name),
                       Detail());
}

  void TracingProfiler::TimeEventsData::end(TracingProfiler& TP) {
    assert(!Stack.empty() && "Must call begin() first");
    auto &E = Stack.back();
    E.End = steady_clock::now();

    // Check that end times monotonically increase.
    assert((Entries.empty() ||
            (E.getFlameGraphStartUs(TP.StartTime) + E.getFlameGraphDurUs() >=
             Entries.back().getFlameGraphStartUs(TP.StartTime) +
                 Entries.back().getFlameGraphDurUs())) &&
           "TimeProfiler scope ended earlier than previous scope");

    // Calculate duration at full precision for overall counts.
    DurationType Duration = E.End - E.Start;

    // Only include sections longer or equal to TimeTraceGranularity msec.
    if (duration_cast<microseconds>(Duration).count() >= TimeTraceGranularity)
      Entries.emplace_back(E);

    // Track total time taken by each "name", but only the topmost levels of
    // them; e.g. if there's a template instantiation that instantiates other
    // templates from within, we only want to add the topmost one. "topmost"
    // happens to be the ones that don't have any currently open entries above
    // itself.
    if (std::find_if(++Stack.rbegin(), Stack.rend(), [&](const TimeTraceEntry &Val) {
          return Val.Name == E.Name;
        }) == Stack.rend()) {
      auto &CountAndTotal = CountAndTotalPerName[E.Name];
      CountAndTotal.first++;
      CountAndTotal.second += Duration;
    }

    Stack.pop_back();
  }

void tracingProfilerInitialize(unsigned TimeTraceGranularity) {
  assert(TracingProfilerInstance == nullptr &&
         "Profiler should not be initialized");
  TracingProfilerInstance = new TracingProfiler();
  TracingProfilerInstance->TimeEvents.TimeTraceGranularity = TimeTraceGranularity;
}

void tracingProfilerCleanup() {
  delete TracingProfilerInstance;
  TracingProfilerInstance = nullptr;
}

void tracingProfilerWrite(raw_pwrite_stream &OS) {
  assert(TracingProfilerInstance != nullptr &&
         "Profiler object can't be null");
  TracingProfilerInstance->Write(OS);
}

void tracingProfilerBeginTimeEvent(StringRef Name, StringRef Detail) {
  if (TracingProfilerInstance != nullptr)
    TracingProfilerInstance->TimeEvents.begin(Name, [&]() { return Detail; });
}

void tracingProfilerBeginTimeEvent(StringRef Name,
                          llvm::function_ref<std::string()> Detail) {
  if (TracingProfilerInstance != nullptr)
    TracingProfilerInstance->TimeEvents.begin(Name, Detail);
}

void tracingProfilerEndTimeEvent() {
  if (TracingProfilerInstance != nullptr)
    TracingProfilerInstance->TimeEvents.end(*TracingProfilerInstance);
}

void tracingProfilerCounterEvent(StringRef Name, StringRef ArgName, int Count) {
  if (TracingProfilerInstance != nullptr)
    TracingProfilerInstance->CounterEvents.add(Name, ArgName, Count);
}

} // namespace llvm
