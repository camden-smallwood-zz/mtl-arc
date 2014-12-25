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

typedef struct atom atom_t;

typedef enum {
	ATOM_INTEGER,    // solid numbers (int)
	ATOM_DECIMAL,    // decimal-point numbers (num)
	ATOM_CHARACTER,  // ascii characters (char)
	ATOM_STRING,     // ascii strings (string)
	ATOM_SYMBOL,     // unique symbols (sym)
	ATOM_CONS,       // atom pairs (cons)
	ATOM_LAMBDA,     // lambda functions (fn)
	ATOM_MACRO,      // macro functions (mac)
	ATOM_TAGGED,     // type annotation: (type (tag 'x 'spcl)) => spcl
	ATOM_TABLE,      // hashtables (table)
	ATOM_INPUT,      // input ports (input)
	ATOM_OUTPUT,     // output ports (output)
	ATOM_SOCKET,     // socket ports (socket)
	ATOM_BUILTIN,    // builtin funtions (builtin)
	ATOM_EXCEPTION,  // exceptions (err)
	ATOM_VM_FRAME,   // encapsulation (frame)
	ATOM_VM_OP,      // virtual machine operations (op)
	ATOM_VM_DUMP     // continuations (cont)
} atom_type_t;

typedef struct {
	char *data;
} string_t;

typedef struct {
	atom_t *car, *cdr;
} cons_t;

typedef struct {
	char *data;
} symbol_t;

typedef struct {
	atom_t *prev, *next;
	int free : 1;  // is the area free?
	int atoms : 1; // are there atoms in the area?
} gc_area_t;

typedef struct {
	atom_t *context,
	       *message,
	       *dump;
} exception_t;

typedef struct {
	atom_t *bindings, // current environment
	       *in_port,  // current stdin port
	       *out_port, // current stdout port
	       *err_port; // current stderr port
} vm_env_t;

typedef struct {
	atom_t *stack,
	       *env,
	       *control;
} vm_dump_t;

typedef struct {
	atom_t *stack,   // list of atom_t
	       *env,     // list of ATOM_VM_ENV
	       *control, // list of ATOM_VM_OP
	       *dump;    // list of ATOM_VM_DUMP
} vm_t;

#endif /* MTL_ARC_VM_H */