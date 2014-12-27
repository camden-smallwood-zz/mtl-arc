//==============================================================
// MTL-Arc
//   A new implementation of the Arc language
// Copyright (C) 2014 Camden Smallwood
//==============================================================
// atom.c
//   Main data structure
//==============================================================

#include <stdlib.h>
#include "atom.h"

atom_t *nil;

atom_t *new_atom(const atom_type_t type) {
	atom_t *result = malloc(sizeof(atom_t));
	result->type = type;
	return result;
}

atom_t *new_integer(const long long value) {
	atom_t *result = new_atom(ATOM_INTEGER);
	integer(result) = value;
	return result;
}

atom_t *new_decimal(const double value) {
	atom_t *result = new_atom(ATOM_DECIMAL);
	decimal(result) = value;
	return result;
}

atom_t *new_character(const char value) {
	atom_t *result = new_atom(ATOM_CHARACTER);
	character(result) = value;
	return result;
}

atom_t *new_string(const char *value) {
	atom_t *result = new_atom(ATOM_STRING);
	string(result) = strdup(value);
	return result;
}

atom_t *new_symbol(const char *value) {
	atom_t *result = new_atom(ATOM_SYMBOL);
	symbol(result) = strdup(value);
	return result;
}

atom_t *new_cons(atom_t *car, atom_t *cdr) {
	atom_t *result = new_atom(ATOM_CONS);
	car(result) = car;
	cdr(result) = cdr;
	return result;
}

atom_t *new_lambda(atom_t *args, atom_t *body, atom_t *env) {
	atom_t *result = new_atom(ATOM_LAMBDA);
	args(result) = args;
	body(result) = body;
	env(result) = env;
	return result;
}

atom_t *new_macro(atom_t *args, atom_t *body, atom_t *env) {
	atom_t *result = new_atom(ATOM_MACRO);
	args(result) = args;
	body(result) = body;
	env(result) = env;
	return result;
}

atom_t *new_tagged(atom_t *tag, atom_t *rep) {
	atom_t *result = new_atom(ATOM_TAGGED);
	tag(result) = tag;
	rep(result) = rep;
	return result;
}

atom_t *new_table(atom_t *entries) {
	atom_t *result = new_atom(ATOM_TABLE);
	table(result) = entries;
	return result;
}

atom_t *new_input(port_t *port) {
	atom_t *result = new_atom(ATOM_INPUT);
	port(result) = port;
	return result;
}

atom_t *new_output(port_t *port) {
	atom_t *result = new_atom(ATOM_OUTPUT);
	port(result) = port;
	return result;
}

atom_t *new_socket(port_t *port) {
	atom_t *result = new_atom(ATOM_SOCKET);
	port(result) = port;
	return result;
}

atom_t *new_builtin(const char *help, atom_t *(*func)(atom_t *)) {
	atom_t *result = new_atom(ATOM_BUILTIN);
	help(result) = new_string(help);
	func(result) = func;
	return result;
}

atom_t *new_exception(atom_t *context, const char *message, atom_t *continuation) {
	atom_t *result = new_atom(ATOM_EXCEPTION);
	context(result) = context;
	message(result) = new_string(message);
	continuation(result) = continuation;
	return result;
}

atom_t *symbol_table;

atom_t *intern(const char *symbol) {
	atom_t *i;
	for (i = symbol_table; !no(i); i = cdr(i))
		if (!strcmp(symbol, symbol(car(i))))
			return car(i);
	return car(symbol_table = cons(new_symbol(symbol), symbol_table));
}

atom_t *coerce_integer(atom_t *value) {
	switch (value->type) {
		case ATOM_INTEGER:
			return value;
		case ATOM_DECIMAL:
			return new_integer((long long)decimal(value));
		case ATOM_CHARACTER:
			return new_integer((long long)character(value));
		case ATOM_STRING:
			return new_integer(atol(string(value)));
		default:
			return nil; // TODO: exception
	}
}

atom_t *coerce_decimal(atom_t *value) {
	switch (value->type) {
		case ATOM_INTEGER:
			return new_decimal((double)integer(value));
		case ATOM_DOUBLE:
			return value;
		case ATOM_CHARACTER:
			return new_decimal((double)character(value));
		case ATOM_STRING:
			return new_decimal(atof(string(value)));
		default:
			return nil; // TODO: exception
}

atom_t *coerce_character(atom_t *value) {
	
}

atom_t *coerce_string(atom_t *value) {
	
}

atom_t *coerce_symbol(atom_t *value) {
	
}

atom_t *coerce_cons(atom_t *value) {
	
}

atom_t *coerce_table(atom_t *value) {
	
}
