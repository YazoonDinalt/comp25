/* Copyright 2025-2026, Vitaliy Dyachkov, Ruslan Nafikov, Vladislav Shalnev */
/* SPDX-License-Identifier: LGPL-3.0-or-later */

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


int64_t print_int(int64_t tagged) {
    printf("%lld\n", (long long)(tagged >> 1));
    fflush(stdout);
    return 1;
}

typedef int64_t (*func_t)();

typedef struct {
    func_t func;
    int32_t arity;
    int32_t n_applied;
    int64_t args[];
} closure_t;

static int64_t call_closure(closure_t *c) {
    switch (c->arity) {
    case 1:
        return ((int64_t (*)(int64_t))c->func)(c->args[0]);
    case 2:
        return ((int64_t (*)(int64_t, int64_t))c->func)(c->args[0], c->args[1]);
    case 3:
        return ((int64_t (*)(int64_t, int64_t, int64_t))c->func)(
            c->args[0], c->args[1], c->args[2]);
    case 4:
        return ((int64_t (*)(int64_t, int64_t, int64_t, int64_t))c->func)(
            c->args[0], c->args[1], c->args[2], c->args[3]);
    case 5:
        return ((int64_t (*)(int64_t, int64_t, int64_t, int64_t, int64_t))c->func)(
            c->args[0], c->args[1], c->args[2], c->args[3], c->args[4]);
    default:
        abort();
    }
}

int64_t make_closure(int64_t fptr, int32_t arity) {
    closure_t *c = malloc(sizeof(closure_t));
    c->func = (func_t)(uintptr_t)fptr;
    c->arity = arity;
    c->n_applied = 0;
    return (int64_t)(uintptr_t)c;
}

int64_t apply(int64_t closure_val, int64_t arg) {
    closure_t *old_c = (closure_t *)(uintptr_t)closure_val;
    int32_t new_n = old_c->n_applied + 1;
    closure_t *new_c = malloc(sizeof(closure_t) + new_n * sizeof(int64_t));
    new_c->func = old_c->func;
    new_c->arity = old_c->arity;
    new_c->n_applied = new_n;
    if (old_c->n_applied > 0)
        memcpy(new_c->args, old_c->args, old_c->n_applied * sizeof(int64_t));
    new_c->args[old_c->n_applied] = arg;
    if (new_n == old_c->arity) {
        int64_t result = call_closure(new_c);
        free(new_c);
        return result;
    }
    return (int64_t)(uintptr_t)new_c;
}
