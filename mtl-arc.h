// mtl-arc
//  A new implementation of the Arc language
// Copyright (C) 2014 Camden Smallwood

#ifndef mtl_arc_header
#define mtl_arc_header

#include <ctype.h>
#include <math.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef enum {
	type_num, type_sym, type_string, type_char, type_cons,
	type_fn, type_mac, type_table, type_exception,
	type_builtin, type_input, type_output
} atom_type;

typedef struct atom *atom;
typedef atom (*builtin)(atom);

struct atom {
	atom_type type;
	union {
		double num;
		char *sym;
		struct { atom car, cdr; };
		struct { char *help; builtin prim; };
		struct { FILE *stream; int outsize; char *outbuf; };
	};
};

#define no(atom) ((atom) == nil)
#define type(atom) ((atom)->type)
#define isa(a, b) (type(a) == b)
#define anum(atom) (isa(atom, type_num))
#define numval(atom) ((atom)->num)
#define asym(atom) (isa(atom, type_sym))
#define symname(atom) ((atom)->sym)
#define astring(atom) (isa(atom, type_string))
#define stringval(atom) ((atom)->sym)
#define achar(atom) (isa(atom, type_char))
#define charval(atom) ((atom)->sym[0])
#define acons(atom) (isa(atom, type_cons))
#define car(atom) ((atom)->car)
#define cdr(atom) ((atom)->cdr)
#define caar(atom) (car(car(atom)))
#define cadr(atom) (car(cdr(atom)))
#define cdar(atom) (cdr(car(atom)))
#define cddr(atom) (cdr(cdr(atom)))
#define afn(atom) (isa(atom, type_fn))
#define amac(atom) (isa(atom, type_mac))
#define atable(atom) (isa(atom, type_table))
#define abuiltin(atom) (isa(atom, type_builtin))
#define help(atom) ((atom)->help)
#define prim(atom) ((atom)->prim)
#define isinput(atom) (isa(atom, type_input))
#define isoutput(atom) (isa(atom, type_output))
#define stream(atom) ((atom)->stream)
#define outsize(atom) ((atom)->outsize)
#define outbuf(atom) ((atom)->outbuf)
#define iserr(atom) (isa(atom, type_exception))
#define exctx(atom) (car(atom))
#define exmsg(atom) (cdr(atom))

extern atom nil, t, syms, root,
            sym_num, sym_sym, sym_char, sym_string, sym_cons, sym_fn, sym_mac, sym_table, sym_list,
            sym_exception, sym_builtin, sym_input, sym_output, sym_quote, sym_if, sym_is, sym_car,
            sym_cdr, sym_while, sym_assign, sym_bound, sym_optional, sym_wildcard, sym_quasiquote,
            sym_unquote, sym_unquote_expand, sym_compose, sym_complement, sym_minus, sym_divide;

extern atom stack;
extern long long stack_size, stack_capacity;
void stack_add(atom a);
void stack_restore(long long saved_size);

#define gc_tolerance 10000

void gc_consider();
void gc_mark(atom a);
void gc_sweep();

atom make(atom_type type);
atom cons(atom car, atom cdr);
atom err(const char *message, atom context);
atom new_num(const double value);
atom new_sym(const char *value);
atom sym(const char *sym);
atom new_string(const char *value);
atom new_char(const char value);
atom new_closure(atom args, atom body, atom env);
atom new_fn(atom args, atom body, atom env);
atom new_mac(atom args, atom body, atom env);
atom tget(atom table, atom key);
atom tset(atom table, atom key, atom value);
atom table(atom args);
atom new_builtin(builtin prim, const char *help);
atom new_input(FILE *value);
atom new_output(FILE *value);

atom env_create(atom parent);
atom env_get(atom env, atom key);
atom env_assign(atom env, atom key, atom value);
atom env_assign_eq(atom env, atom key, atom value);

char *char_to_token(const char value);
char token_to_char(const char *value);
char **split_string(char *string, const char delim);

atom ssexpand(char *token);
atom infix_expand(atom args);
atom read_expr(FILE *stream);
atom read_list(FILE *stream);
atom read_bracket(FILE *stream);
atom read_brace(FILE *stream);
atom read_string(FILE *stream);

void write_expr(FILE *stream, atom expr);

int alist(atom expr);
atom copy_list(atom list);

atom apply(atom fn, atom args);
atom eval(atom expr, atom env);

atom coerce_num(atom value);
atom coerce_sym(atom value);
atom coerce_char(atom value);
atom coerce_string(atom value);
atom coerce_cons(atom value);
atom coerce_table(atom value);

atom builtin_type(atom args);
atom builtin_err(atom args);
atom builtin_help(atom args);
atom builtin_cons(atom args);
atom builtin_car(atom args);
atom builtin_cdr(atom args);
atom builtin_len(atom args);
atom builtin_add(atom args);
atom builtin_sub(atom args);
atom builtin_mul(atom args);
atom builtin_div(atom args);
atom builtin_lt(atom args);
atom builtin_gt(atom args);
atom builtin_cos(atom args);
atom builtin_expt(atom args);
atom builtin_log(atom args);
atom builtin_mod(atom args);
atom builtin_rand(atom args);
atom builtin_sin(atom args);
atom builtin_sqrt(atom args);
atom builtin_tan(atom args);
atom builtin_trunc(atom args);
atom builtin_shl(atom args);
atom builtin_newstring(atom args);
atom builtin_coerce(atom args);
atom builtin_copy(atom args);
atom builtin_apply(atom args);
atom builtin_eval(atom args);
atom builtin_stdin(atom args);
atom builtin_stdout(atom args);
atom builtin_stderr(atom args);
atom builtin_readb(atom args);
atom builtin_readc(atom args);
atom builtin_peekc(atom args);
atom builtin_readline(atom args);
atom builtin_sread(atom args);
atom builtin_load(atom args);
atom builtin_disp(atom args);
atom builtin_write(atom args);
atom builtin_writeb(atom args);
atom builtin_writec(atom args);
atom builtin_infile(atom args);
atom builtin_outfile(atom args);
atom builtin_close(atom args);

atom arc_load_file(const char *path);
void arc_init();

#endif
