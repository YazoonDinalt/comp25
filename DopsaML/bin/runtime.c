/* Copyright 2025-2026, Vitaliy Dyachkov, Ruslan Nafikov, Vladislav Shalnev */
/* SPDX-License-Identifier: LGPL-3.0-or-later */

#include <stdint.h>
#include <stdio.h>

int64_t print_int(int64_t n) {
    printf("%lld\n", (long long)n);
    fflush(stdout);
    return 0;
}
