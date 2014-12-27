
#ifndef MTL_ARC_ATOM_H
#define MTL_ARC_ATOM_H

#include "port.h"

typedef struct atom atom_t;

typedef enum {
	ATOM_INTEGER,
	ATOM_DECIMAL,
	ATOM_CHARACTER,
	ATOM_STRING,
	ATOM_SYMBOL,
	ATOM_CONS,
	ATOM_LAMBDA,
	ATOM_MACRO,
	ATOM_TAGGED,
	ATOM_TABLE,
	ATOM_INPUT,
	ATOM_OUTPUT,
	ATOM_SOCKET,
	ATOM_BUILTIN,
	ATOM_EXCEPTION
} atom_type_t;

struct atom {
	atom_type_t type;
	union {
		long long integer;
		double decimal;
		char character;
		char *string;
		char *symbol;
		struct { // cons
			atom_t *car, *cdr;
		};
		struct { // lambda & macro
			atom_t *args, *body, *env;
		};
		struct { // tagged
			atom_t *tag, *rep;
		};
		atom_t *table;
		port_t *port; // input, output & socket
		struct { // builtin
			atom_t *help, *(*func)(atom_t *);
		};
		struct { // exception
			atom_t *context, *message, *continuation;
		};
	};
};

atom_t *new_atom(atom_type_t type);
atom_t *new_integer(const long long value);
atom_t *new_decimal(const double value);
atom_t *new_character(const char value);
atom_t *new_string(const char *value);
atom_t *new_symbol(const char *value);
atom_t *new_cons(atom_t *car, atom_t *cdr);
atom_t *new_lambda(atom_t *args, atom_t *body, atom_t *env);
atom_t *new_macro(atom_t *args, atom_t *body, atom_t *env);
atom_t *new_tagged(atom_t *tag, atom_t *rep);
atom_t *new_table(atom_t *entries);
atom_t *new_input(port_t *port);
atom_t *new_output(port_t *port);
atom_t *new_socket(port_t *port);
atom_t *new_builtin(const char *help, atom_t *(*func)(atom_t *));
atom_t *new_exception(atom_t *context, const char *message, atom_t *continuation);

#endif /* MTL_ARC_ATOM_H */