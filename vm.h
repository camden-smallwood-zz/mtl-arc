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

#include <stdarg.h>

typedef struct vm vm_t;
typedef struct cell cell_t;

//==============================================================
// Port System
//==============================================================

typedef enum {
	PORT_STREAM,
	PORT_STRING,
	PORT_SOCKET,
	NUM_PORT_TYPES
} port_type_t;

typedef enum {
	VM_IN_PORT,
	VM_OUT_PORT,
	VM_ERR_PORT,
	VM_DBG_PORT
} vm_port_t;

typedef const char * (*port_info_func_t)(vm_t *, cell_t *, cell_t **);
typedef int (*port_open_func_t)(vm_t *, cell_t *, const char *mode, cell_t *params);
typedef int (*port_close_func_t)(vm_t *, cell_t *);
typedef int (*port_getc_func_t)(vm_t *, cell_t *);
typedef long (*port_size_func_t)(vm_t *, cell_t *);
typedef size_t (*port_read_func_t)(vm_t *, cell_t *, size_t, char *);
typedef int (*port_vprintf_func_t)(vm_t *, cell_t *, const char *fmt, va_list);
typedef cell_t *(*port_owns_func_t)(secd_t*, cell_t *, cell_t **, cell_t **, cell_t **);
typedef cell_t *(*vm_port_func_t)(secd_t*, vm_port_t);

typedef struct {
	port_info_func_t info;
	port_open_func_t open;
	port_close_func_t close;
	port_getc_func_t getc;
	port_size_func_t size;
	port_read_func_t read;
	port_vprintf_func_t vprintf;
	port_owns_func_t owns;
	vm_port_func_t vm_port;
} port_ops_t;

typedef struct {
	port_type_t type;
	int input  : 1; // allow input?
	int output : 1; // allow output?
	long data[2];
} port_t;

cell_t *vm_stdin(vm_t *);
cell_t *vm_stdout(vm_t *);
cell_t *vm_stderr(vm_t *);
cell_t *vm_stddbg(vm_t *);
cell_t *vm_set_port(vm_t *, std_port_t, cell_t *);

int vm_open_port(vm_t *, cell_t *, const char *mode, cell_t *info);
long vm_port_size(vm_t *, cell_t *);
int vm_close_port(vm_t *, cell_t *);

int vm_getc(vm_t *, cell_t *);
size_t vm_read(vm_t *, cell_t *, size_t, char *);

//==============================================================
// Virtual Machine
//==============================================================

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
	port_ops_t port_ops[NUM_PORT_TYPES];
};

#endif /* MTL_ARC_VM_H */
