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

int64_t call_closure(closure_t *c);

/* runtime.c is also compiled for the host (dune foreign_stubs), so the RISC-V
   trampoline must be guarded out there. */
#if defined(__riscv) && __riscv_xlen == 64
__asm__(
    "    .text\n"
    "    .globl call_closure\n"
    "call_closure:\n"
    "    addi sp, sp, -48\n"          /* save callee-saved regs + ra */
    "    sd   ra, 40(sp)\n"
    "    sd   s0, 32(sp)\n"
    "    sd   s1, 24(sp)\n"
    "    sd   s2, 16(sp)\n"
    "    sd   s3, 8(sp)\n"
    "    addi s0, sp, 48\n"           /* s0 = sp on entry (frame anchor) */
    "    ld   s1, 0(a0)\n"            /* s1 = func */
    "    lw   s2, 8(a0)\n"            /* s2 = arity */
    "    addi s3, a0, 16\n"           /* s3 = &args[0] */
    /* push args[8..] onto the stack when arity > 8 */
    "    li   t0, 8\n"
    "    ble  s2, t0, 2f\n"
    "    sub  t1, s2, t0\n"           /* t1 = number of stack args */
    "    slli t2, t1, 3\n"            /* bytes = stack args * 8 */
    "    addi t2, t2, 15\n"           /* round up to a multiple of 16 */
    "    andi t2, t2, -16\n"
    "    sub  sp, sp, t2\n"
    "    addi t3, s3, 64\n"           /* t3 = &args[8] */
    "    mv   t4, sp\n"
    "1:\n"
    "    ld   t5, 0(t3)\n"
    "    sd   t5, 0(t4)\n"
    "    addi t3, t3, 8\n"
    "    addi t4, t4, 8\n"
    "    addi t1, t1, -1\n"
    "    bnez t1, 1b\n"
    "2:\n"
    /* load the first 8 args into a0-a7, guarded by arity */
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
    "    jalr s1\n"                   /* call; result lands in a0 */
    "    ld   ra, -8(s0)\n"           /* restore saved regs via the anchor */
    "    ld   s1, -24(s0)\n"
    "    ld   s2, -32(s0)\n"
    "    ld   s3, -40(s0)\n"
    "    ld   t0, -16(s0)\n"          /* caller's s0 */
    "    mv   sp, s0\n"               /* drop frame + any stack args */
    "    mv   s0, t0\n"
    "    ret\n");
#else
int64_t call_closure(closure_t *c) {
    (void)c;
    abort();
}
#endif

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
