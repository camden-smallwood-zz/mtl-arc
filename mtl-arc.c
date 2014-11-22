// mtl-arc
//  A new implementation of the Arc language
// Copyright (C) 2014 Camden Smallwood

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

typedef enum {
	type_num, type_sym, type_str, type_char,
	type_cons, type_fn, type_table, type_tagged, type_exception,
	type_builtin, type_stream
} atomtype;

typedef struct atom {
	atomtype type;
	union {
		double num;
		char *sym;
		struct { struct atom *car, *cdr; };
		struct atom *(*builtin)(struct atom *);
		FILE *stream;
	};
} atom;

atom *nil, *t,
     *symbol_root, *env_root,
     *sym_eval, *sym_apply,
     *sym_quote, *sym_quasiquote, *sym_unquote, *sym_unquote_expand,
     *sym_if, *sym_while, *sym_fn, *sym_assign;

#define type(atom) ((atom)->type)
#define numval(atom) ((atom)->num)
#define symval(atom) ((atom)->sym)
#define charval(atom) ((atom)->sym[0])
#define car(atom) ((atom)->car)
#define cdr(atom) ((atom)->cdr)
#define tag(atom) (car(atom))
#define rep(atom) (cdr(atom))
#define builtin(atom) ((atom)->builtin)
#define stream(atom) ((atom)->stream)
#define exctx(atom) (car(atom))
#define exmsg(atom) (cdr(atom))
#define iserr(atom) (type(atom) == type_exception)
#define no(atom) (((atom) == nil) || (type(atom) == type_exception))

atom *new_atom(atomtype type) {
	atom *result = (atom *)malloc(sizeof(atom));
	type(result) = type;
	return result;
}

atom *new_num(const double value) {
	atom *result = new_atom(type_num);
	numval(result) = value;
	return result;
}

atom *new_sym(const char *value) {
	atom *result = new_atom(type_sym);
	symval(result) = strdup(value);
	return result;
}

atom *new_str(const char *value) {
	atom *result = new_atom(type_str);
	symval(result) = strdup(value);
	return result;
}

atom *new_char(const char value) {
	atom *result = new_atom(type_char);
	result->sym = (char *)malloc(sizeof(char));
	charval(result) = value;
	return result;
}

atom *cons(atom *car, atom *cdr) {
	atom *result = new_atom(type_cons);
	car(result) = car;
	cdr(result) = cdr;
	return result;
}

atom *new_fn(atom *args, atom *body, atom *env) {
	atom *result = new_atom(type_fn);
	car(result) = args;
	cdr(result) = cons(body, env);
	return result;
}

atom *error(const char *message, atom *context) {
	atom *result = new_atom(type_exception);
	exctx(result) = context;
	exmsg(result) = new_str(strdup(message));
	return result;
}

int is(atom *a, atom *b) {
	if (type(a) == type(b))
		switch (type(a)) {
			case type_num: return numval(a) == numval(b);
			case type_sym: return a == b;
			case type_str: return !strcmp(symval(a), symval(b));
			case type_char: return charval(a) == charval(b);
			case type_builtin: return builtin(a) == builtin(b);
			case type_stream: return stream(a) == stream(b);
			case type_cons:
			case type_fn:
			case type_table:
			case type_tagged:
			case type_exception:
				return is(car(a), car(b)) && is(cdr(a), cdr(b));
		}
	return 0;
}

atom *findsym(const char *name) {
	for(atom *symlist = symbol_root; !no(symlist); symlist = cdr(symlist))
		if(!strcmp(name, symval(car(symlist)))) return symlist;
	return nil;
}

atom *intern(const char *name) {
	atom *p = findsym(name);
	if(!no(p)) return car(p);
	p = new_sym(name);
	symbol_root = cons(p, symbol_root);
	return p;
}

atom *tget(atom *table, atom *key) {
	while (!no(table)) {
		if (car(car(table)) == key) return cdr(car(table));
		table = cdr(table);
	}
	return nil;
}

atom *tset(atom *table, atom *key, atom *value) {
	atom *tp = table;
	while (!no(tp)) {
		if (car(car(tp)) == key) return cdr(car(tp)) = value;
		tp = cdr(tp);
	}
	if (car(table) != nil)
		cdr(table) = cons(car(table), cdr(table));
	car(table) = cons(key, value);
	return value;
}

atom *table(atom *entries) {
	atom *tp, *result = new_atom(type_table);
	car(result) = cdr(result) = nil;
	while (!no(entries)) {
	loop_entries:
		tp = result;
		while (!no(tp)) {
			if (car(car(tp)) == car(entries)) {
				cdr(car(tp)) = car(cdr(entries));
				entries = cdr(cdr(entries));
				if (no(entries)) return result;
				else goto loop_entries;
			}
			tp = cdr(tp);
		}
		if (car(result) != nil)
			cdr(result) = cons(car(result), cdr(result));
		car(result) = cons(car(entries), car(cdr(entries)));
		entries = cdr(cdr(entries));
	}
	return result;
}

atom *annotate(atom *args) {
	atom *result = new_atom(type_tagged);
	tag(result) = car(args);
	rep(result) = car(cdr(args));
	return result;
}

atom *new_builtin(atom *(*function)(atom *)) {
	atom *result = new_atom(type_builtin);
	builtin(result) = function;
	return result;
}

atom *new_stream(FILE *stream) {
	atom *result = new_atom(type_stream);
	stream(result) = stream;
	return result;
}

atom *env_create(atom *parent) { return cons(parent, nil); }

atom *env_get(atom *env, atom *sym) {
	atom *parent = car(env), *bs = cdr(env);
	while (!no(bs)) {
		if (car(car(bs)) == sym) return cdr(car(bs));
		bs = cdr(bs);
	}
	if (no(parent)) return error("unbound symbol", sym);
	return env_get(parent, sym);
}

atom *env_assign(atom *env, atom *sym, atom *val) {
	atom *bs = cdr(env), *b = nil;
	while (!no(bs)) {
		if (car(car(bs)) == sym) return cdr(car(bs)) = val;
		bs = cdr(bs);
	}
	cdr(env) = cons(cons(sym, val), cdr(env));
	return val;
}

atom *env_assign_eq(atom *env, atom *sym, atom *val) {
	atom *env_origin = env;
	while (!no(env)) {
		atom *bs = cdr(env);
		while (!no(bs)) {
			if (car(car(bs)) == sym) return cdr(car(bs)) = val;
			bs = cdr(bs);
		}
		env = car(env);
	}
	return env_assign(env_origin, sym, val);
}

atom *env_assign_multiple(atom *env, atom *syms, atom *vals) {
	if (no(syms)) return env;
	return env_assign_multiple(env_assign_eq(env, car(syms), car(vals)), cdr(syms), cdr(vals));
}

atom *assoc(atom *key, atom *alist) {
	if(no(alist)) return nil;
	if(car(car(alist)) == key) return car(alist);
	return assoc(key, cdr(alist));
}

FILE *ifp;
char *token_la;
int la_valid = 0;
#define MAXLEN 100
char buf[MAXLEN];
int bufused;

void add_to_buf(char ch) { if(bufused < MAXLEN - 1) buf[bufused++] = ch; }
char *buf_str() { buf[bufused++] = '\0'; return strdup(buf); }
void set_input(FILE *fp) { ifp = fp; }
void putback_token(char *token) { token_la = token; la_valid = 1; }

char *read_token() {
	int ch;
	bufused = 0;
	if(la_valid) { la_valid = 0; return token_la; }
	do { if((ch = getc(ifp)) == EOF) return NULL; } while(isspace(ch));
	add_to_buf(ch);
	if(strchr("()\"`',@;", ch)) return buf_str();
	for(;;) {
		if((ch = getc(ifp)) == EOF) exit(0);
		if(strchr("()\"`',@;", ch) || isspace(ch)) {
			ungetc(ch, ifp);
			return buf_str();
		}
		add_to_buf(ch);
	}
}

char strchar(const char *str) {
	if (!strcmp(str, "#\\newline")) return '\n';
	if (!strcmp(str, "#\\tab")) return '\t';
	if (!strcmp(str, "#\\space")) return ' ';
	return str[2];
}

atom *read_list();
atom *read_str();
void read_eol_comment();
atom *read_expr() {
	char *token = read_token();
	if (token == NULL) return nil;
	if (!strcmp(token, "(")) return read_list();
	if (!strcmp(token, "\"")) return read_str();
	if (!strcmp(token, "'")) return cons(sym_quote, cons(read_expr(), nil));
	if (!strcmp(token, ";")) { read_eol_comment(); return read_expr(); }
	if (!strcmp(token, "`")) return cons(sym_quasiquote, cons(read_expr(), nil));
	if (!strcmp(token, ",")) {
		char c = getc(ifp);
		if (c == '@') return cons(sym_unquote_expand, cons(read_expr(), nil));
		ungetc(c, ifp);
		return cons(sym_unquote, cons(read_expr(), nil));
	} else if (token[strspn(token, "0123456789.-")] == '\0') {
		if (!strcmp(token, ".") || !strcmp(token, "-")) return intern(token);
		return new_num((double)atof(token));
	} else if (strlen(token) > 2) { // possible reader macro
		if (token[0] == '#' && token[1] == '\\') // char
			return new_char(strchar(token));
	}
	return intern(token);
}

atom *read_list() {
	char *token = read_token();
	atom *tmp;
	if(!strcmp(token, ")")) return nil;
	if(!strcmp(token, ".")) {
		tmp = read_expr();
		if(strcmp(read_token(), ")")) return error("invalid syntax", tmp);
		return tmp;
	}
	putback_token(token);
	tmp = read_expr();
	return cons(tmp, read_list());
}

atom *read_str() {
	char c, cbuf[1024];
	int n = 0;
	memset(cbuf, 0, 1024);
	while ((c = fgetc(ifp)) != '\"') cbuf[n++] = c;
	return new_str(cbuf);
}

void read_eol_comment() {
	while (fgetc(ifp) != '\n');
}

char *charstr(const char value) {
	if (value == '\n') return "#\\newline";
	else if (value == '\t') return "#\\tab";
	else if (value == ' ') return "#\\space";
	char cbuf[] = { 0, 0, 0 };
	sprintf(cbuf, "#\\%c", value);
	return strdup(cbuf);
}

atom *eval(atom *exp, atom *env);

void write_expr(FILE *stream, atom *expr) {
	switch(type(expr)) {
		case type_num: fprintf(stream, "%g", numval(expr)); break;
		case type_sym: fprintf(stream, "%s", symval(expr)); break;
		case type_str: fprintf(stream, "\"%s\"", symval(expr)); break;
		case type_char: fprintf(stream, "%s", charstr(charval(expr))); break;
		case type_builtin: fprintf(stream, "#<builtin:%p>", builtin(expr)); break;
		case type_stream: fprintf(stream, "#<stream:%p>", stream(expr)); break;
		case type_fn:
			fprintf(stream, "#<fn ");
			write_expr(stream, car(expr));
			fprintf(stream, ": ");
			write_expr(stream, car(cdr(expr)));
			fprintf(stream, ">");
			break;
		case type_tagged:
			fprintf(stream, "#<tagged %s ", symval(tag(expr)));
			write_expr(stream, rep(expr));
			putchar('>');
			break;
		case type_table:
			fprintf(stream, "#table");
			expr->type = type_cons;
			write_expr(stream, expr);
			expr->type = type_table;
			break;
		case type_exception:
			fprintf(stream, "exception:\n==> %s", symval(exmsg(expr)));
			if (!no(exctx(expr))) {
				fprintf(stream, "\n===> ");
				write_expr(stream, exctx(expr));
			}
			break;
		case type_cons: 
			fprintf(stream, "(");
			for(;;) {
				write_expr(stream, car(expr));
				if (no(cdr(expr))) { fprintf(stream, ")"); break; }
				expr = cdr(expr);
				if (type(expr) != type_cons) {
					fprintf(stream, " . ");
					write_expr(stream, expr);
					fprintf(stream, ")");
					break;
				}
				fprintf(stream, " ");
			}
			break;
		default: eval(error("write_expr not implemented for this type", nil), env_root);
	}
}

atom *evlis(atom *exprs, atom *env);
atom *apply(atom *fn, atom *args, atom *env);
atom *eval(atom *exp, atom *env) {
	atom *tmp;
	if(exp == nil) return nil;
	switch(exp->type) {
		case type_num:
		case type_str:
		case type_char:
		case type_fn:
		case type_table:
		case type_tagged:
		case type_exception:
		case type_builtin:
		case type_stream: return exp;
		case type_sym:
			tmp = assoc(exp, env);
			if(tmp == nil) return error("unbound symbol", exp);
			return cdr(tmp);
		case type_cons: {
			atom *op = car(exp), *args = cdr(exp);
			if (op == sym_eval) {
				if (!no(cdr(args))) return error("invalid arguments supplied to 'eval'", args);
				return eval(eval(car(args), env), env);
			} else if (op == sym_apply) {
				if (no(args)) return error("invalid arguments supplied to 'apply'", args);
				if (iserr(op = eval(car(args), env))) return op;
				if (iserr(args = evlis(cdr(args), env))) return args;
				return apply(eval(op, env), args, env);
			} else if (op == sym_if) {
				atom *cond;
				while (!no(args)) {
					if (iserr(cond = eval(car(args), env))) return cond;
					if (no(cdr(args))) return cond;
					if (!no(cond)) return eval(car(cdr(args)), env);
					args = cdr(cdr(args));
				}
				return nil;
			} else if (op == sym_while) {
				if (no(args) || no(cdr(args)))
					return error("invalid arguments supplied to 'while'", args);
				atom *result = nil, *err = nil;
				while (!no(err = eval(car(args), env))) {
					if (iserr(err)) return err;
					atom *expr = cdr(args);
					while (!no(expr)) {
						if (iserr(result = eval(car(expr), env))) return result;
						expr = cdr(expr);
					}
				}
				return result;
			} else if(op == sym_fn) {
				return new_fn(car(args), cdr(args), env);
			} else if (op == sym_quote) {
				return car(args);
			} else if (op == sym_assign || op == intern("=")) {
				if (type(car(args)) == type_sym)
					return env_assign_eq(env, car(args), eval(car(cdr(args)), env));
				else if (type(car(args)) == type_cons) {
					atom *iop = eval(car(car(args)), env);
					if (iserr(iop)) return iop;
					else if (type(iop) == type_table) {
						atom *key = eval(car(cdr(car(args))), env);
						if (iserr(key)) return key;
						return tset(iop, key, eval(car(cdr(args)), env));
					} else if (is(car(car(args)), intern("car"))) {
						atom *place = eval(car(cdr(car(args))), env);
						if (iserr(place)) return place;
						atom *val = eval(car(cdr(args)), env);
						if (iserr(val)) return val;
						return car(place) = val;
					} else if (is(car(car(args)), intern("cdr"))) {
						atom *place = eval(car(cdr(car(args))), env);
						if (iserr(place)) return place;
						atom *val = eval(car(cdr(args)), env);
						if (iserr(val)) return val;
						return cdr(place) = val;
					}
				}
				return error("cannot assign value", car(args));
			}
			if (iserr(op = eval(op, env))) return op;
			if (iserr(args = evlis(args, env))) return args;
			return apply(op, args, env);
		}
	}
	return exp; // not reached
}

atom *evlis(atom *exprs, atom *env) {
	if (type(exprs) != type_cons) return eval(exprs, env);
	atom *car, *cdr;
	if (iserr(car = eval(car(exprs), env))) return car;
	if (iserr(cdr = evlis(cdr(exprs), env))) return cdr;
	return cons(car, cdr);
}

atom *progn(atom *exprs, atom *env) {
	if(exprs == nil) return nil;
	for(;;) {
		if(cdr(exprs) == nil) return eval(car(exprs), env);
		eval(car(exprs), env);
		exprs = cdr(exprs);
	}
}

atom *apply(atom *fn, atom *args, atom *env) {
	switch (type(fn)) {
		case type_builtin: return (*builtin(fn))(args);
		case type_tagged: return apply(rep(fn), args, env);
		case type_str: return new_char(symval(fn)[(int)numval(car(args))]);
		case type_table: return tget(fn, car(args));
		case type_fn:
			if (type(car(fn)) == type_sym)
				env_assign(cdr(cdr(fn)), car(fn), args);
			else env_assign_multiple(cdr(cdr(fn)), car(fn), args);
			return progn(car(cdr(fn)), cdr(cdr(fn)));
		default: return error("bad argument to apply", fn);
	}
}

atom *prim_is(atom *args) {
	if (no(args) || no(cdr(args)))
		return error("invalid arguments supplied to 'is'", args);
	atom *a, *b;
	while (!no(cdr(args))) {
		a = car(args);
		b = car(cdr(args));
		if (!is(a, b)) return nil;
		args = cdr(args);
	}
	return t;
}

atom *prim_type(atom *args) {
	if (no(args) || !no(cdr(args)))
		return error("invalid arguments supplied to 'type'", args);
	switch (type(car(args))) {
		case type_num: return intern("num");
		case type_sym: return intern("sym");
		case type_str: return intern("str");
		case type_char: return intern("char");
		case type_cons: return intern("cons");
		case type_fn: return intern("fn");
		case type_table: return intern("table");
		case type_tagged: return tag(car(args));
		case type_exception: return intern("exception");
		case type_builtin: return intern("builtin");
		case type_stream: return intern("stream");
	}
	return error("unknown type of atom", car(args));
}

atom *prim_err(atom *args) {
	if (no(args)) args = cons(new_str("unspecified"), nil);
	return error(symval(car(args)), no(cdr(args)) ? nil : car(cdr(args)));
}

atom *prim_add(atom *args) {
	double sum;
	for(sum = 0; !no(args); sum += numval(car(args)), args = cdr(args));
	return new_num(sum);
}

atom *prim_sub(atom *args) {
	double sum;
	for(sum = numval(car(args)), args = cdr(args); 
	    !no(args); 
	    sum -= numval(car(args)), args = cdr(args));
	return new_num(sum);
}

atom *prim_mul(atom *args) {
	double prod;
	for(prod = 1; !no(args); prod *= numval(car(args)), args = cdr(args));
	return new_num(prod);
}

atom *prim_div(atom *args) {
	double rem;
	for (rem = numval(car(args)), args = cdr(args);
	     !no(args);
	     rem /= numval(car(args)), args = cdr(args));
	return new_num(rem);
}

atom *prim_lt(atom *args) {
	atom *a, *b;
	if (no(args) || no(cdr(args)) || !no(cdr(cdr(args))))
		return error("invalid arguments supplied to '<'", args);
	if (type(a = car(args)) == type_num &&
	    type(b = car(cdr(args))) == type_num)
		return numval(a) < numval(b) ? t : nil;
	return nil;
}

atom *prim_pr(atom *args) {
	while (!no(args)) {
		switch (type(car(args))) {
			case type_str: printf("%s", symval(car(args))); break;
			case type_char: printf("%c", charval(car(args))); break;
			default: write_expr(stdout, car(args)); break;
		}
		args = cdr(args);
	}
	return nil;
}

atom *prim_cons(atom *args) { return cons(car(args), car(cdr(args))); }
atom *prim_car(atom *args)  { return car(car(args)); }
atom *prim_cdr(atom *args)  { return cdr(car(args)); }

atom *arc_load_file(const char *path) {
	printf("loading \"%s\"...\n", path);
	set_input(fopen(path, "r+"));
	atom *expr = nil, *prev;
	while (prev = expr, !is(nil, expr = eval(read_expr(), env_root)))
		if (iserr(expr)) { printf("=> "); write_expr(stdout, expr); putchar('\n'); }
	fclose(ifp);
	set_input(NULL);
	return prev;
}

void arc_init() {
	nil = new_sym("nil");
	symbol_root = cons(nil, nil);
	env_root = env_create(nil);
	env_assign(env_root, t = intern("t"), t);
	sym_eval = intern("eval");
	sym_apply = intern("apply");
	sym_quote = intern("quote");
	sym_quasiquote = intern("quasiquote");
	sym_unquote = intern("unquote");
	sym_unquote_expand = intern("unquote-expand");
	sym_if = intern("if");
	sym_while = intern("while");
	sym_fn = intern("fn");
	sym_assign = intern("assign");
	env_assign(env_root, intern("is"), new_builtin(prim_is));
	env_assign(env_root, intern("type"), new_builtin(prim_type));
	env_assign(env_root, intern("table"), new_builtin(table));
	env_assign(env_root, intern("annotate"), new_builtin(annotate));
	env_assign(env_root, intern("err"), new_builtin(prim_err));
	env_assign(env_root, intern("+"), new_builtin(prim_add));
	env_assign(env_root, intern("-"), new_builtin(prim_sub));
	env_assign(env_root, intern("*"), new_builtin(prim_mul));
	env_assign(env_root, intern("/"), new_builtin(prim_div));
	env_assign(env_root, intern("<"), new_builtin(prim_lt));
	env_assign(env_root, intern("pr"), new_builtin(prim_pr));
	env_assign(env_root, intern("cons"), new_builtin(prim_cons));
	env_assign(env_root, intern("car"), new_builtin(prim_car));
	env_assign(env_root, intern("cdr"), new_builtin(prim_cdr));
	env_assign(env_root, intern("stdin"), new_stream(stdin));
	env_assign(env_root, intern("stdout"), new_stream(stdout));
	env_assign(env_root, intern("stderr"), new_stream(stderr));
	atom *result = arc_load_file("arc.arc");
}

int main(int argc, char **argv) {
	puts("  mtl-arc v0.1\n================");
	arc_init();
	set_input(stdin);
	for(;;) {
		printf("%s", "> ");
		atom *o = eval(read_expr(), env_root);
		printf("%s", "=> ");
		write_expr(stdout, o);
		printf("\n");
	}
	return 0;
}