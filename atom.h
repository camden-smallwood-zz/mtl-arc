
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

#define integer(atom) ((atom)->integer)
#define decimal(atom) ((atom)->decimal)
#define character(atom) ((atom)->character)
#define string(atom) ((atom)->string)
#define symbol(atom) ((atom)->symbol)
#define cons(car, cdr) new_cons((car), (cdr))
#define car(atom) ((atom)->car)
#define cdr(atom) ((atom)->cdr)
#define args(atom) ((atom)->args)
#define body(atom) ((atom)->body)
#define env(atom) ((atom)->env)
#define tag(atom) ((atom)->tag)
#define rep(atom) ((atom)->rep)
#define table(atom) ((atom)->table)
#define port(atom) ((atom)->port)
#define help(atom) ((atom)->help)
#define func(atom) ((atom)->func)
#define context(atom) ((atom)->context)
#define message(atom) ((atom)->message)
#define continuation(atom) ((atom)->continuation)

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

atom_t *intern(const char *symbol);

atom_t *coerce_integer(atom_t *value);
atom_t *coerce_decimal(atom_t *value);
atom_t *coerce_character(atom_t *value);
atom_t *coerce_string(atom_t *value);
atom_t *coerce_symbol(atom_t *value);
atom_t *coerce_cons(atom_t *value);
atom_t *coerce_table(atom_t *value);

#endif /* MTL_ARC_ATOM_H */