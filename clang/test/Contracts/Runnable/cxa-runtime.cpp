union alignas(8) guard_t {
  long int state;
  long int aligner;
};

extern "C" int __cxa_guard_acquire(guard_t *guard_object) {
  return __sync_bool_compare_and_swap(&guard_object->state, 0, 1);
}

extern "C" void __cxa_guard_release(guard_t *guard_object) {
  __sync_synchronize();
  guard_object->state = 2;
}

extern "C" void __cxa_guard_abort(guard_t *guard_object) {
  guard_object->state = 0;
}