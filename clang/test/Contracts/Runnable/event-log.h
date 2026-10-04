#ifndef CONTRACTS_EVENT_LOG_H
#define CONTRACTS_EVENT_LOG_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Records the order in which things happen as a string of ';'-terminated
// events, so that tests can check both the order of the events and that each
// one happened exactly once.
inline char EventLog[4096];

inline bool ev(const char *Event) {
  if (strlen(EventLog) + strlen(Event) + 2 > sizeof(EventLog)) {
    fprintf(stderr, "event log overflow\n");
    abort();
  }
  strcat(EventLog, Event);
  strcat(EventLog, ";");
  return true;
}

inline void expect_events_impl(const char *File, int Line,
                               const char *Expected) {
  if (strcmp(EventLog, Expected) != 0) {
    fprintf(stderr,
            "%s:%d: unexpected sequence of events\n"
            "  expected: %s\n"
            "  actual:   %s\n",
            File, Line, Expected, EventLog);
    abort();
  }
  EventLog[0] = '\0';
}

// Check that exactly the given events were recorded since the last check.
#define EXPECT_EVENTS(...) expect_events_impl(__FILE__, __LINE__, __VA_ARGS__)

// The events recorded by contract predicates: when the contracts are ignored
// (the test is compiled with -DIGNORED), the predicates are not evaluated.
#ifdef IGNORED
#define C(X) ""
#else
#define C(X) X
#endif

#endif // CONTRACTS_EVENT_LOG_H
