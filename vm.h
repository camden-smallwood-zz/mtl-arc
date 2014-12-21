//==============================================================
// MTL-Arc
//   A new implementation of the Arc language
// Copyright (C) 2014 Camden Smallwood
//==============================================================
// vm.h
//   Virtual Machine header
//==============================================================

#ifndef MTL_ARC_VM_H
#define MTL_ARC_VM_H

#include "port.h"

typedef struct vm vm_t;

int vm_printf(vm_t *, const char *fmt, ...);
int vm_errorf(vm_t *, const char *fmt, ...);

typedef enum {
	VM_IN_PORT,
	VM_OUT_PORT,
	VM_ERR_PORT,
	VM_DBG_PORT
} vm_port_t;

struct vm {
	cell_t *begin;
	cell_t *stack;
	cell_t *env;
	cell_t *control;
	cell_t *dump;
	cell_t *free;
	cell_t *global_env;
	cell_t *sym_store;
	cell_t *end;
	cell_t *input_port;
	cell_t *output_port;
	cell_t *error_port;
	cell_t *debug_port;
	unsigned long tick;
};

#endif /* MTL_ARC_VM_H */