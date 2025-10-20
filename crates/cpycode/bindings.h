#include <cstdarg>
#include <cstdint>
#include <cstdlib>
#include <ostream>
#include <new>

extern "C" {

int64_t cpycode_init();

void cpycode_close();

void *cpycode_phase_open(const char *filename);

void cpycode_phase_close(const void *phase);

const char *cpycode_phase_print(const void *phase);

float cpycode_phase_sampling_frequency(const void *phase);

uintptr_t cpycode_phase_datalen(const void *phase);

uintptr_t cpycode_phase_n_channels(const void *phase);

const char *cpycode_phase_channel_label(const void *phase, uintptr_t index);

uintptr_t cpycode_phase_channel_group(const void *phase, uintptr_t index);

uintptr_t cpycode_phase_raw_data(const void *phase,
                                 uintptr_t group,
                                 const char *label,
                                 const float **data,
                                 uintptr_t start,
                                 uintptr_t end);

}  // extern "C"
