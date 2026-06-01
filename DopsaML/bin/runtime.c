/* Copyright 2025-2026, Vitaliy Dyachkov, Ruslan Nafikov, Vladislav Shalnev */
/* SPDX-License-Identifier: LGPL-3.0-or-later */

#include <setjmp.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* ints are odd (2n+1), pointers even */
static inline int is_int(int64_t v) { return v & 1; }

int64_t print_int(int64_t tagged) {
    printf("%lld\n", (long long)(tagged >> 1));
    fflush(stdout);
    return 1;
}

typedef int64_t (*func_t)();

/* header of a live object is its tag; after a copy it holds the new address */
#define CLOSURE 1
#define TUPLE 2

/* closure layout is fixed, the asm trampoline below depends on the offsets */
typedef struct {
    int64_t header;
    func_t func;
    int64_t arity;
    int64_t n_applied;
    int64_t args[];
} closure_t;

typedef struct {
    int64_t header;
    int64_t size;
    int64_t fields[];
} tuple_t;

static int64_t obj_bytes(int64_t *obj) {
    if (*obj == CLOSURE)
        return (int64_t)sizeof(closure_t) + ((closure_t *)obj)->n_applied * 8;
    return (int64_t)sizeof(tuple_t) + ((tuple_t *)obj)->size * 8;
}

/* copying gc: two banks, bump allocation, Cheney */

#define BANK_SIZE (1 << 20) /* 1 MiB */

static struct {
    uint8_t *from_start; /* active bank */
    uint8_t *to_start; /* the other one */
    uint8_t *free; /* bump pointer */
    int64_t bank_idx;
    int64_t total_allocated;
    int64_t collections;
} gc;

static int64_t *stack_bottom;

void gc_init(void) {
    gc.from_start = malloc(BANK_SIZE);
    gc.to_start = malloc(BANK_SIZE);
    gc.free = gc.from_start;
    gc.bank_idx = 0;
    gc.total_allocated = 0;
    gc.collections = 0;
    /* main's frame, top of the stack for the root scan */
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wframe-address"
    stack_bottom = (int64_t *)__builtin_frame_address(1);
#pragma GCC diagnostic pop
}

static void *bump(int64_t bytes) {
    if (gc.free + bytes > gc.from_start + BANK_SIZE) return NULL;
    void *p = gc.free;
    gc.free += bytes;
    gc.total_allocated += bytes;
    return p;
}

void gc_collect(void);

static void *gc_alloc(int64_t bytes) {
    void *p = bump(bytes);
    if (p == NULL) {
        gc_collect();
        p = bump(bytes);
        if (p == NULL) {
            fprintf(stderr, "GC: out of memory\n");
            exit(1);
        }
    }
    return p;
}

/* move obj to the new bank (if not yet), leave a forwarding pointer, return
   its new address. children are handled by the scan loop below */
static int64_t forward(int64_t ptr) {
    int64_t *obj = (int64_t *)(uintptr_t)ptr;
    if (*obj != CLOSURE && *obj != TUPLE) return *obj; /* already copied */
    int64_t bytes = obj_bytes(obj);
    int64_t *dst = (int64_t *)gc.free;
    gc.free += bytes;
    gc.total_allocated += bytes;
    memcpy(dst, obj, bytes); /* tag is copied along */
    *obj = (int64_t)(uintptr_t)dst;
    return (int64_t)(uintptr_t)dst;
}

/* forward an arg/field if it points into the old bank */
static int64_t forward_child(int64_t v, uint8_t *old_start, uint8_t *old_free) {
    if (!is_int(v) && (uint8_t *)v >= old_start && (uint8_t *)v < old_free)
        return forward(v);
    return v;
}

void gc_collect(void) {
    /* setjmp dumps the callee-saved regs onto the stack, otherwise a root
       sitting only in a register would be missed by the scan */
    jmp_buf regs;
    (void)setjmp(regs);
    int64_t *sp = (int64_t *)&regs;
    uint8_t *old_start = gc.from_start;
    uint8_t *old_free = gc.free;

    /* flip banks */
    uint8_t *tmp = gc.from_start;
    gc.from_start = gc.to_start;
    gc.to_start = tmp;
    gc.free = gc.from_start;
    gc.bank_idx = 1 - gc.bank_idx;

    /* roots: walk the stack, forward anything that looks like a pointer into
       the old bank and fix up the slot */
    for (int64_t *slot = stack_bottom; slot >= sp; slot--) {
        int64_t w = *slot;
        if (!is_int(w) && (w & 7) == 0 && (uint8_t *)w >= old_start
            && (uint8_t *)w < old_free) {
            int64_t h = *(int64_t *)(uintptr_t)w;
            if (h == CLOSURE || h == TUPLE
                || ((uint8_t *)h >= gc.from_start
                    && (uint8_t *)h < gc.from_start + BANK_SIZE))
                *slot = forward(w);
        }
    }

    /* now walk the copied objects and forward what they point to */
    uint8_t *scan = gc.from_start;
    while (scan < gc.free) {
        int64_t *obj = (int64_t *)scan;
        if (*obj == CLOSURE) {
            closure_t *c = (closure_t *)obj;
            for (int64_t i = 0; i < c->n_applied; i++)
                c->args[i] = forward_child(c->args[i], old_start, old_free);
        } else {
            tuple_t *t = (tuple_t *)obj;
            for (int64_t i = 0; i < t->size; i++)
                t->fields[i] = forward_child(t->fields[i], old_start, old_free);
        }
        scan += obj_bytes(obj);
    }
    gc.collections++;
}

int64_t make_closure(int64_t fptr, int32_t arity) {
    closure_t *c = gc_alloc(sizeof(closure_t));
    c->header = CLOSURE;
    c->func = (func_t)(uintptr_t)fptr;
    c->arity = arity;
    c->n_applied = 0;
    return (int64_t)(uintptr_t)c;
}

int64_t call_closure(closure_t *c);

int64_t apply(int64_t closure_val, int64_t arg) {
    closure_t *old_c = (closure_t *)(uintptr_t)closure_val;
    int64_t new_n = old_c->n_applied + 1;
    /* gc_alloc might collect here; old_c and arg are on the stack so the scan
       fixes them up, safe to use after */
    closure_t *new_c = gc_alloc(sizeof(closure_t) + new_n * 8);
    new_c->header = CLOSURE;
    new_c->func = old_c->func;
    new_c->arity = old_c->arity;
    new_c->n_applied = new_n;
    memcpy(new_c->args, old_c->args, old_c->n_applied * 8);
    new_c->args[old_c->n_applied] = arg;
    if (new_n == new_c->arity) return call_closure(new_c);
    return (int64_t)(uintptr_t)new_c;
}

int64_t create_tuple(int64_t size, int64_t *init) {
    /* init points to a stack array of the fields; safe across a collection
       (it is a root) */
    tuple_t *t = gc_alloc(sizeof(tuple_t) + size * 8);
    t->header = TUPLE;
    t->size = size;
    memcpy(t->fields, init, size * 8);
    return (int64_t)(uintptr_t)t;
}

int64_t field(int64_t tuple, int64_t i) {
    return ((tuple_t *)(uintptr_t)tuple)->fields[i >> 1];
}

/* gc functions callable from the language, the unit arg is ignored */

int64_t collect(int64_t unit) {
    (void)unit;
    gc_collect();
    return 1;
}

int64_t get_heap_start(int64_t unit) {
    (void)unit;
    return ((int64_t)(uintptr_t)gc.from_start << 1) | 1;
}

int64_t get_heap_fin(int64_t unit) {
    (void)unit;
    return ((int64_t)(uintptr_t)(gc.from_start + BANK_SIZE) << 1) | 1;
}

int64_t print_gc_status(int64_t unit) {
    (void)unit;
    printf("GC status:\n");
    printf("  bank: %lld\n", (long long)gc.bank_idx);
    printf("  allocated: %lld\n", (long long)(gc.free - gc.from_start));
    printf("  total allocated: %lld\n", (long long)gc.total_allocated);
    printf("  collections: %lld\n", (long long)gc.collections);
    fflush(stdout);
    return 1;
}

/* call the closure with all its args: first 8 in a0-a7, rest on the stack.
   reads func at 8, arity at 16, args at 32. riscv only (host compiles too). */
#if defined(__riscv) && __riscv_xlen == 64
__asm__(
    "    .text\n"
    "    .globl call_closure\n"
    "call_closure:\n"
    "    addi sp, sp, -48\n"
    "    sd   ra, 40(sp)\n"
    "    sd   s0, 32(sp)\n"
    "    sd   s1, 24(sp)\n"
    "    sd   s2, 16(sp)\n"
    "    sd   s3, 8(sp)\n"
    "    addi s0, sp, 48\n"
    "    ld   s1, 8(a0)\n"             /* s1 = func */
    "    ld   s2, 16(a0)\n"            /* s2 = arity */
    "    addi s3, a0, 32\n"           /* s3 = &args[0] */
    "    li   t0, 8\n"
    "    ble  s2, t0, 2f\n"
    "    sub  t1, s2, t0\n"
    "    slli t2, t1, 3\n"
    "    addi t2, t2, 15\n"
    "    andi t2, t2, -16\n"
    "    sub  sp, sp, t2\n"
    "    addi t3, s3, 64\n"
    "    mv   t4, sp\n"
    "1:\n"
    "    ld   t5, 0(t3)\n"
    "    sd   t5, 0(t4)\n"
    "    addi t3, t3, 8\n"
    "    addi t4, t4, 8\n"
    "    addi t1, t1, -1\n"
    "    bnez t1, 1b\n"
    "2:\n"
    "    li   t0, 1\n"
    "    blt  s2, t0, 3f\n"
    "    ld   a0, 0(s3)\n"
    "    li   t0, 2\n"
    "    blt  s2, t0, 3f\n"
    "    ld   a1, 8(s3)\n"
    "    li   t0, 3\n"
    "    blt  s2, t0, 3f\n"
    "    ld   a2, 16(s3)\n"
    "    li   t0, 4\n"
    "    blt  s2, t0, 3f\n"
    "    ld   a3, 24(s3)\n"
    "    li   t0, 5\n"
    "    blt  s2, t0, 3f\n"
    "    ld   a4, 32(s3)\n"
    "    li   t0, 6\n"
    "    blt  s2, t0, 3f\n"
    "    ld   a5, 40(s3)\n"
    "    li   t0, 7\n"
    "    blt  s2, t0, 3f\n"
    "    ld   a6, 48(s3)\n"
    "    li   t0, 8\n"
    "    blt  s2, t0, 3f\n"
    "    ld   a7, 56(s3)\n"
    "3:\n"
    "    jalr s1\n"
    "    ld   ra, -8(s0)\n"
    "    ld   s1, -24(s0)\n"
    "    ld   s2, -32(s0)\n"
    "    ld   s3, -40(s0)\n"
    "    ld   t0, -16(s0)\n"
    "    mv   sp, s0\n"
    "    mv   s0, t0\n"
    "    ret\n");
#else
int64_t call_closure(closure_t *c) {
    (void)c;
    abort();
}
#endif
