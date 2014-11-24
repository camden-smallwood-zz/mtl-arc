// mtl-arc
//  A new implementation of the Arc language
// Copyright (C) 2014 Camden Smallwood

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef enum {
	type_num, type_sym, type_str, type_char,
	type_cons, type_fn, type_mac, type_table,
	type_builtin, type_stream, type_exception
} atom_type;

typedef struct atom *atom;
typedef atom (*builtin)(atom);

struct atom {
	atom_type type;
	union {
		double num;
		char *sym;
		struct { atom car, cdr; };
		struct { char *doc; builtin prim; };
		FILE *stream;
	};
};

atom nil, t, syms, root,
     sym_quote, sym_quasiquote, sym_unquote, sym_unquote_expand,
     sym_if, sym_is, sym_while, sym_assign, sym_fn, sym_mac;

#define no(atom) ((atom) == nil)
#define type(atom) ((atom)->type)
#define isa(a, b) (type(a) == b)
#define anum(atom) (isa(atom, type_num))
#define numval(atom) ((atom)->num)
#define asym(atom) (isa(atom, type_sym))
#define symname(atom) ((atom)->sym)
#define astr(atom) (isa(atom, type_str))
#define strval(atom) ((atom)->sym)
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
#define primdoc(atom) ((atom)->doc)
#define primval(atom) ((atom)->prim)
#define astream(atom) (isa(atom, type_stream))
#define streamval(atom) ((atom)->stream)
#define iserr(atom) (isa(atom, type_exception))
#define exctx(atom) (car(atom))
#define exmsg(atom) (cdr(atom))

atom make(atom_type type) {
	atom result = malloc(sizeof(struct atom));
	type(result) = type;
	return result;
}

atom new_num(const double num) {
	atom result = make(type_num);
	numval(result) = num;
	return result;
}

atom new_sym(const char *sym) {
	atom result = make(type_sym);
	symname(result) = strdup(sym);
	return result;
}

atom new_str(const char *str) {
	atom result = make(type_str);
	strval(result) = strdup(str);
	return result;
}

atom new_char(const char c) {
	atom result = make(type_char);
	result->sym = malloc(1);
	charval(result) = c;
	return result;
}

char *charstr(const char value) {
	if (value == '\n') return "#\\newline";
	else if (value == '\t') return "#\\tab";
	else if (value == ' ') return "#\\space";
	char cbuf[] = { 0, 0, 0 };
	sprintf(cbuf, "#\\%c", value);
	return strdup(cbuf);
}

char strchar(const char *str) {
	if (!strcmp(str, "#\\newline")) return '\n';
	if (!strcmp(str, "#\\tab")) return '\t';
	if (!strcmp(str, "#\\space")) return ' ';
	return str[2];
}

atom cons(atom car, atom cdr) {
	atom result = make(type_cons);
	car(result) = car;
	cdr(result) = cdr;
	return result;
}

atom intern(const char *sym) {
	for (atom p = syms; !no(p); p = cdr(p))
		if (!strcmp(sym, symname(car(p))))
			return car(p);
	return car(syms = cons(new_sym(sym), syms));
}

atom new_closure(atom args, atom body, atom env) {
	return cons(env, cons(args, body));
}

atom new_fn(atom args, atom body, atom env) {
	atom result = new_closure(args, body, env);
	type(result) = type_fn;
	return result;
}

atom new_mac(atom args, atom body, atom env) {
	atom result = new_closure(args, body, env);
	type(result) = type_mac;
	return result;
}

atom tget(atom table, atom key) {
	for (; !no(table); table = cdr(table))
		if (caar(table) == key)
			return cdar(table);
	return nil;
}

atom tset(atom table, atom key, atom value) {
	for (atom p = table; !no(p); p = cdr(p))
		if (caar(p) == key)
			return cdar(p) = value;
	if (car(table) != nil)
		cdr(table) = cons(car(table), cdr(table));
	car(table) = cons(key, value);
	return value;
}

atom table(atom entries) {
	atom result = make(type_table);
	car(result) = cdr(result) = nil;
	for (; !no(entries); entries = cddr(entries)) {
		if (no(cdr(entries))) {
			tset(result, car(entries), nil);
			break;
		}
		tset(result, car(entries), cadr(entries));
	}
	return result;
}

atom new_builtin(builtin prim, const char *doc) {
	atom result = make(type_builtin);
	primdoc(result) = strdup(doc);
	primval(result) = prim;
	return result;
}

atom new_stream(FILE *stream) {
	atom result = make(type_stream);
	streamval(result) = stream;
	return result;
}

atom new_exception(const char *message, atom context) {
	atom result = make(type_exception);
	exctx(result) = context;
	exmsg(result) = new_str(message);
	return result;
}

#define err new_exception

atom env_create(atom parent) {
	return cons(parent, nil);
}

atom env_get(atom env, atom sym) {
	for (atom bs = cdr(env); !no(bs); bs = cdr(bs))
		if (caar(bs) == sym)
			return cdar(bs);
	if (no(car(env)))
		return err("unbound symbol", sym);
	return env_get(car(env), sym);
}

atom env_assign(atom env, atom sym, atom val) {
	for (atom bs = cdr(env); !no(bs); bs = cdr(bs))
		if (caar(bs) == sym)
			return cdar(bs) = val;
	return cdar(cdr(env) = cons(cons(sym, val), cdr(env)));
}

atom env_assign_eq(atom env, atom sym, atom val) {
	for (atom e = env; !no(e); e = car(e))
		for (atom bs = cdr(e); !no(bs); bs = cdr(bs))
			if (caar(bs) == sym)
				return cdar(bs) = val;
	return env_assign(env, sym, val);
}

int buf_size = 0, last_valid = 0;
#define BUF_MAX 1024
char buf[BUF_MAX], *last_token;

void add_to_buf(const char c) {
	if (buf_size < BUF_MAX - 1)
		buf[buf_size++] = c;
}

char *buf_to_string() {
	buf[buf_size++] = '\0';
	return strdup(buf);
}

void unget_token(char *token) {
	last_token = token;
	last_valid = 1;
}

char *get_token(FILE *stream) {
	int ch;
	buf_size = 0;
	if(last_valid) { last_valid = 0; return last_token; }
	do { if((ch = getc(stream)) == EOF) return NULL; } while(isspace(ch));
	add_to_buf(ch);
	if(strchr("()\"`',;", ch)) return buf_to_string();
	for(;;) {
		if((ch = getc(stream)) == EOF) exit(0);
		if(strchr("()\"`',;", ch) || isspace(ch)) {
			ungetc(ch, stream);
			return buf_to_string();
		}
		add_to_buf(ch);
	}
}


atom read_list(FILE *stream);

atom read_expr(FILE *stream) {
	char *token = get_token(stream);
	if (token == NULL) {
		return nil;
	} else if (!strcmp(token, "(")) {
		return read_list(stream);
	} else if (!strcmp(token, "'")) {
		return cons(sym_quote, cons(read_expr(stream), nil));
	} else if (!strcmp(token, "`")) {
		return cons(sym_quasiquote, cons(read_expr(stream), nil));
	} else if (!strcmp(token, ",")) {
		char c = getc(stream);
		if (c == '@')
			return cons(sym_unquote_expand, cons(read_expr(stream), nil));
		ungetc(c, stream);
		return cons(sym_unquote, cons(read_expr(stream), nil));
	} else if (token[strspn(token, "-.0123456789")] == '\0') {
		if (!strcmp(token, "-") || !strcmp(token, "."))
			return intern(token);
		return new_num(atof(token));
	} else if (strlen(token) > 2) { // possible reader macro
		if (token[0] == '#' && token[1] == '\\') // char
			return new_char(strchar(token));
	}
	return intern(token);
}

atom read_list(FILE *stream) {
	char *token = get_token(stream);
	if (!strcmp(token, ")")) {
		return nil;
	} else if (!strcmp(token, ".")) {
		atom result = read_expr(stream);
		if (strcmp(get_token(stream), ")"))
			return err("invalid syntax while reading list", nil);
		return result;
	}
	unget_token(token);
	return cons(read_expr(stream), read_list(stream));
}

void write_expr(FILE *stream, atom expr) {
	if (anum(expr)) {
		fprintf(stream, "%g", numval(expr));
	} else if (asym(expr)) {
		fprintf(stream, "%s", symname(expr));
	} else if (astr(expr)) {
		fprintf(stream, "\"%s\"", strval(expr));
	} else if (achar(expr)) {
		fprintf(stream, "%s", charstr(charval(expr)));
	} else if (acons(expr)) {
		fprintf(stream, "(");
		for (;;) {
			write_expr(stream, car(expr));
			if (no(cdr(expr))) {
				fprintf(stream, ")");
				break;
			}
			expr = cdr(expr);
			if (!acons(expr)) {
				fprintf(stream, " . ");
				write_expr(stream, expr);
				fprintf(stream, ")");
				break;
			}
			fprintf(stream, " ");
		}
	} else if (afn(expr)) {
		fprintf(stream, "#<fn ");
		write_expr(stream, cadr(expr));
		fprintf(stream, ": ");
		write_expr(stream, cddr(expr));
		fprintf(stream, ">");
	} else if (amac(expr)) {
		fprintf(stream, "#<mac ");
		write_expr(stream, cadr(expr));
		fprintf(stream, ": ");
		write_expr(stream, cddr(expr));
		fprintf(stream, ">");
	} else if (atable(expr)) {
		fprintf(stream, "#table");
		type(expr) = type_cons;
		write_expr(stream, expr);
		type(expr) = type_table;
	} else if (abuiltin(expr)) {
		fprintf(stream, "#<builtin %p>", primval(expr));
	} else if (astream(expr)) {
		fprintf(stream, "#<stream %p>", streamval(expr));
	} else if (iserr(expr)) {
		fprintf(stream, "exception:\n==> %s", strval(exmsg(expr)));
		if (!no(exctx(expr))) {
			fprintf(stream, ": ");
			write_expr(stream, exctx(expr));
		}
	}
}

int alist(atom expr) {
	for (; !no(expr); expr = cdr(expr))
		if (!acons(expr))
			return 0;
	return 1;
}

atom copy_list(atom list) {
	if (no(list))
		return nil;
	atom a = cons(car(list), nil), p = a;
	list = cdr(list);
	while (!no(list)) {
		cdr(p) = cons(car(list), nil);
		p = cdr(p);
		list = cdr(list);
		if (!acons(list)) {
			p = list;
			break;
		}
	}
	return a;
}

atom eval(atom expr, atom env);

atom apply(atom fn, atom args, atom env) {
	if (abuiltin(fn)) {
		return (*primval(fn))(args);
	} else if (afn(fn)) {
		atom env = env_create(car(fn));
		for (atom names = cadr(fn); !no(names); names = cdr(names)) {
			if (asym(names)) {
				env_assign(env, names, args);
				args = nil;
				break;
			} else if (asym(car(names))) {
				if (no(args))
					return err("invalid arguments supplied to fn", nil);
				env_assign(env, car(names), car(args));
				args = cdr(args);
			} else {
				atom val;
				if (no(args)) {
					if (no(cddr(car(names))))
						val = nil;
					else if (iserr(val = eval(car(cddr(car(names))), env)))
						return val;
				} else {
					val = car(args);
					args = cdr(args);
				}
				env_assign(env, cadr(car(names)), val);
			}
		}
		if (!no(args))
			return err("invalid arguments supplied to fn", nil);
		atom result = nil;
		for (atom body = cddr(fn); !no(body); body = cdr(body))
			if (iserr(result = eval(car(body), env)))
				return result;
		return result;
	} else if (astr(fn)) {
		return new_char(strval(fn)[(int)numval(car(args))]);
	} else if (atable(fn)) {
		return tget(fn, car(args));
	} else {
		return err("invalid operator supplied to apply", fn);
	}
}

atom eval(atom expr, atom env) {
	if (expr == nil) {
		return nil;
	} else if (asym(expr)) {
		return env_get(env, expr);
	} else if (!acons(expr)) {
		return expr;
	} else {
		atom op = car(expr), args = cdr(expr);
		if (op == sym_quote) {
			return car(args);
		} else if (op == sym_if) {
			atom cond;
			while (!no(args)) {
				if (iserr(cond = eval(car(args), env)))
					return cond;
				if (no(cdr(args)))
					return cond;
				if (!no(cond))
					return eval(cadr(args), env);
				args = cddr(args);
			}
			return nil;
		} else if (op == sym_is) {
			atom a = eval(car(args), env);
			if (iserr(a))
				return a;
			atom b = eval(cadr(args), env);
			if (iserr(b))
				return b;
			if (type(a) == type(b)) {
				
			}
			return nil;
		} else if (op == sym_while) {
			if (no(args))
				return err("invalid arguments supplied to while", args);
			atom result, pred = car(args);
			while (!iserr(result = eval(pred, env)) && !no(result)) {
				if (iserr(result))
					return result;
				for (atom e = cdr(args); !no(e); e = cdr(e))
					if (iserr(result = eval(car(e), env)))
						return result;
			}
			return result;
		} else if (op == sym_assign) {
			if (asym(car(args))) {
				atom val = eval(cadr(args), env);
				if (iserr(val))
					return val;
				return env_assign_eq(env, car(args), val);
			} else if (acons(car(args))) {
				if (caar(args) == intern("car")) {
					atom place = eval(car(cdar(args)), env);
					if (iserr(place))
						return place;
					atom val = eval(cadr(args), env);
					if (iserr(val))
						return val;
					return car(place) = val;
				} else if (caar(args) == intern("cdr")) {
					atom place = eval(car(cdar(args)), env);
					if (iserr(place))
						return place;
					atom val = eval(cadr(args), env);
					if (iserr(val))
						return val;
					return cdr(place) = val;
				} else {
					atom iop = eval(caar(args), env);
					if (iserr(iop)) {
						return iop;
					} else if (atable(iop)) {
						atom key = eval(car(cdar(args)), env);
						if (iserr(key))
							return key;
						return tset(iop, key, eval(cadr(args), env));
					}
				}
			}
			return err("cannot assign value to place", car(args));
		} else if (op == sym_fn) {
			return new_fn(car(args), cdr(args), env);
		} else if (op == sym_mac) {
			if (no(args) || no(cdr(args)) || no(cdr(cdr(args))))
				return err("invalid arguments supplied to mac", args);
			atom name = car(args);
			if (!asym(name))
				return err("mac name must be a sym", name);
			atom macro = new_mac(cadr(args), cddr(args), env);
			if (iserr(env_assign(env, name, macro)))
				return err("could not assign mac", macro);
			return name;
		}
		if (iserr(op = eval(op, env)))
			return op;
		if (amac(op)) {
			atom ex;
			type(op) = type_fn;
			if (iserr(ex = apply(op, args, env)))
				return ex;
			return eval(ex, env);
		}
		args = copy_list(args);
		for (atom p = args; !no(p); p = cdr(p))
			if (iserr(car(p) = eval(car(p), env)))
				return car(p);
		return apply(op, args, env);
	}
}

atom macex(atom expr) {
	if (asym(expr)) {
		return expr;
	} else if (!acons(expr)) {
		return expr;
	} else if (!alist(expr)) {
		return expr;
	} else {
		atom op = car(expr), args = cdr(expr);
		if (asym(op)) {
			if (op == sym_quote) {
				if (no(args) || !no(cdr(args)))
					return err("invalid arguments supplied to quote", args);
				return expr;
			} else if (op == sym_mac) {
				if (no(args) || no(cdr(args)) || no(cddr(args)))
					return err("invalid arguments supplied to mac", args);
				atom name = car(args);
				if (!asym(name))
					return err("mac name must be a sym", name);
				atom macro = new_mac(cadr(args), cddr(args), root);
				if (iserr(macro))
					return macro;
				env_assign(root, name, macro);
				return cons(sym_quote, cons(name, nil));
			}
		}
		
		atom result;
		if (asym(op) && !iserr(result = env_get(root, op)) && amac(result)) {
			if (iserr(op = eval(op, root)))
				return op;
			type(op) = type_fn;
			atom result2 = apply(op, args, root);
			if (iserr(result2))
				return result2;
			return macex(result2);
		} else {
			atom expr2 = copy_list(expr);
			for (atom p = expr2; !no(p); p = cdr(p))
				if (iserr(car(p) = macex(car(p))))
					return car(p);
			return expr2;
		}
	}
}

atom prim_type(atom args) {
	if (no(args) || !no(cdr(args)))
		return err("invalid arguments supplied to 'type'", args);
	switch (type(car(args))) {
		case type_num: return intern("num");
		case type_sym: return intern("sym");
		case type_str: return intern("str");
		case type_char: return intern("char");
		case type_cons: return intern("cons");
		case type_fn: return intern("fn");
		case type_mac: return intern("mac");
		case type_table: return intern("table");
		case type_builtin: return intern("builtin");
		case type_stream: return intern("stream");
		case type_exception: return intern("exception");
	}
	return err("unknown type of atom", car(args));
}

atom prim_err(atom args) {
	if (no(args))
		args = cons(new_str("unspecified"), nil);
	return err(strval(car(args)), no(cdr(args)) ? nil : cadr(args));
}

atom prim_add(atom args) {
	if (!no(args))
		for (atom p = args; !no(p); p = cdr(p))
			if (!anum(car(p)))
				return err("invalid arguments supplied to +", args);
	double sum;
	for(sum = 0; !no(args); sum += numval(car(args)), args = cdr(args));
	return new_num(sum);
}

atom prim_sub(atom args) {
	if (!no(args))
		for (atom p = args; !no(p); p = cdr(p))
			if (!anum(car(p)))
				return err("invalid arguments supplied to -", args);
	double sum = numval(car(args));
	for(args = cdr(args); !no(args); sum -= numval(car(args)), args = cdr(args));
	return new_num(sum);
}

atom prim_mul(atom args) {
	if (!no(args))
		for (atom p = args; !no(p); p = cdr(p))
			if (!anum(car(p)))
				return err("invalid arguments supplied to *", args);
	double prod;
	for(prod = 1; !no(args); prod *= numval(car(args)), args = cdr(args));
	return new_num(prod);
}

atom prim_div(atom args) {
	if (!no(args))
		for (atom p = args; !no(p); p = cdr(p))
			if (!anum(car(p)))
				return err("invalid arguments supplied to /", args);
	double rem = numval(car(args));
	for (args = cdr(args); !no(args); rem /= numval(car(args)), args = cdr(args));
	return new_num(rem);
}

atom prim_lt(atom args) {
	if ((no(args) || no(cdr(args)) || !no(cddr(args))) ||
	    !anum(car(args)) || !anum(cadr(args)))
		return err("invalid arguments supplied to <", args);
	atom a, b;
	if (anum(a = car(args)) && anum(b = cadr(args)))
		return numval(a) < numval(b) ? t : nil;
	return nil;
}

atom prim_pr(atom args) {
	for (; !no(args); args = cdr(args)) {
		if (astr(car(args))) {
			printf("%s", strval(car(args)));
		} else if (achar(car(args))) {
			printf("%c", charval(car(args)));
		} else {
			write_expr(stdout, car(args));
		}
	}
	return nil;
}

atom prim_cons(atom args) {
	if (no(args) || no(cdr(args)) || !no(cddr(args)))
		return err("invalid arguments supplied to cons", args);
	return cons(car(args), cadr(args));
}

atom prim_car(atom args) {
	if (no(args) || !no(cdr(args)))
		return err("invalid arguments supplied to car", args);
	if (car(args) == nil)
		return nil;
	return caar(args);
}

atom prim_cdr(atom args) {
	if (no(args) || !no(cdr(args)))
		return err("invalid arguments supplied to cdr", args);
	if (car(args) == nil)
		return nil;
	return cdar(args);
}

atom arc_load_file(const char *path) {
	printf("loading \"%s\"...\n", path);
	FILE *stream = fopen(path, "r+");
	atom prev, expr = nil;
	while (prev = expr, (expr = macex(eval(read_expr(stream), root))) != nil)
		if (iserr(expr)) { printf("=> "); write_expr(stdout, expr); putchar('\n'); }
	fclose(stream);
	return prev;
}

void arc_init() {
	nil = new_sym("nil");
	syms = cons(nil, nil);
	root = cons(nil, nil);
	sym_quote = intern("quote");
	sym_quasiquote = intern("quasiquote");
	sym_unquote = intern("unquote");
	sym_unquote_expand = intern("unquote-expand");
	sym_if = intern("if");
	sym_is = intern("is");
	sym_while = intern("while");
	sym_assign = intern("assign");
	sym_fn = intern("fn");
	sym_mac = intern("mac");
	env_assign(root, t = intern("t"), t);
	env_assign(root, intern("type"), new_builtin(prim_type, ""));
	env_assign(root, intern("err"), new_builtin(prim_err, ""));
	env_assign(root, intern("+"), new_builtin(prim_add, ""));
	env_assign(root, intern("-"), new_builtin(prim_sub, ""));
	env_assign(root, intern("*"), new_builtin(prim_mul, ""));
	env_assign(root, intern("/"), new_builtin(prim_div, ""));
	env_assign(root, intern("<"), new_builtin(prim_lt, ""));
	env_assign(root, intern("pr"), new_builtin(prim_pr, ""));
	env_assign(root, intern("cons"), new_builtin(prim_cons, ""));
	env_assign(root, intern("car"), new_builtin(prim_car, ""));
	env_assign(root, intern("cdr"), new_builtin(prim_cdr, ""));
	env_assign(root, intern("table"), new_builtin(table, ""));
	env_assign(root, intern("stdin"), new_stream(stdin));
	env_assign(root, intern("stdout"), new_stream(stdout));
	env_assign(root, intern("stderr"), new_stream(stderr));
	arc_load_file("arc.arc");
}

int main(int argc, char **argv) {
	puts("  mtl-arc v0.2\n================");
	arc_init();
	for(;;) {
		printf("%s", "> ");
		atom result = macex(eval(read_expr(stdin), root));
		printf("%s", "=> ");
		write_expr(stdout, result);
		printf("\n");
	}
	return 0;
}
