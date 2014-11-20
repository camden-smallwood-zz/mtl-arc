#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define error(X, A) do { fprintf(stderr, "%s ", X); return A; } while (0)

typedef enum { type_num, type_sym, type_cons, type_str, type_char, type_fn, type_builtin } atype;

typedef struct atom {
	atype type;
	union {
		double num;
		char *sym, c;
		struct { struct atom *car, *cdr; };
		struct atom *(*builtin)(struct atom *);
	};
} atom;

atom *symbol_root, *env_root, *nil, *t;
atom *sym_quote, *sym_if, *sym_while, *sym_fn, *sym_assign;

#define type(atom) ((atom)->type)
#define numval(atom) ((atom)->num)
#define symval(atom) ((atom)->sym)
#define car(atom) ((atom)->car)
#define cdr(atom) ((atom)->cdr)
#define builtin(atom) ((atom)->builtin)
#define no(atom) ((atom) == nil)

atom *mkatom(atype type) {
	atom *result = (atom *)malloc(sizeof(atom));
	type(result) = type;
	return result;
}

atom *cons(atom *car, atom *cdr) {
	atom *result = mkatom(type_cons);
	car(result) = car;
	cdr(result) = cdr;
	return result;
}

atom *mknum(const double value) {
	atom *result = mkatom(type_num);
	numval(result) = value;
	return result;
}

atom *mksym(const char *value) {
	atom *result = mkatom(type_sym);
	symval(result) = strdup(value);
	return result;
}

atom *mkstr(const char *value) {
	atom *result = mkatom(type_str);
	symval(result) = strdup(value);
	return result;
}

atom *mkchar(const char value) {
	atom *result = mkatom(type_char);
	result->c = value;
	return result;
}

atom *mkbuiltin(atom *(*function)(atom *)) {
	atom *result = mkatom(type_builtin);
	builtin(result) = function;
	return result;
}

atom *mkfn(atom *args, atom *body, atom *env) {
	atom *result = mkatom(type_fn);
	car(result) = args;
	cdr(result) = cons(body, env);
	return result;
}

atom *findsym(const char *name) {
	for(atom *symlist = symbol_root; !no(symlist); symlist = cdr(symlist))
		if(!strcmp(name, symval(car(symlist)))) return symlist;
	return nil;
}

atom *intern(const char *name) {
	atom *p = findsym(name);
	if(!no(p)) return car(p);
	p = mksym(name);
	symbol_root = cons(p, symbol_root);
	return p;
}

atom *env_create(atom *parent) { return cons(parent, nil); }

atom *env_get(atom *env, atom *sym) {
	atom *parent = car(env), *bs = cdr(env);
	while (!no(bs)) {
		if (car(car(bs)) == sym) return cdr(car(bs));
		bs = cdr(bs);
	}
	if (no(parent)) error("Symbol not bound", sym);
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
char *buf2str() { buf[bufused++] = '\0'; return strdup(buf); }
void setinput(FILE *fp) { ifp = fp; }
void putback_token(char *token) { token_la = token; la_valid = 1; }

char *gettoken() {
	int ch;
	bufused = 0;
	if(la_valid) { la_valid = 0; return token_la; }
	do { if((ch = getc(ifp)) == EOF) return NULL; } while(isspace(ch));
	add_to_buf(ch);
	if(strchr("'()\"", ch)) return buf2str();
	for(;;) {
		if((ch = getc(ifp)) == EOF) exit(0);
		if(strchr("'()\"", ch) || isspace(ch)) {
			ungetc(ch, ifp);
			return buf2str();
		}
		add_to_buf(ch);
	}
}

atom *readlist();
atom *readstr();
atom *readexpr() {
	char *token = gettoken();
	if (token == NULL) return nil;
	if(!strcmp(token, "(")) return readlist();
	if(!strcmp(token, "\"")) return readstr();
	if(!strcmp(token, "\'")) return cons(sym_quote, cons(readexpr(), nil));
	if(token[strspn(token, "0123456789.-")] == '\0') return mknum((double)atof(token));
	return intern(token);
}

atom *readlist() {
	char *token = gettoken();
	atom *tmp;
	if(!strcmp(token, ")")) return nil;
	if(!strcmp(token, ".")) {
		tmp = readexpr();
		if(strcmp(gettoken(), ")")) error("Invalid syntax", tmp);
		return tmp;
	}
	putback_token(token);
	tmp = readexpr();
	return cons(tmp, readlist());
}

atom *readstr() {
	char c, cbuf[1024];
	memset(cbuf, 0, 1024);
	int n = 0;
	while ((c = fgetc(ifp)) != '\"') cbuf[n++] = c;
	return mkstr(cbuf);
}

char *charsym(const char value) {
	if (value == '\n') return "newline";
	else if (value == '\t') return "tab";
	else if (value == ' ') return "space";
	char cbuf[] = { value, 0 };
	return strdup(cbuf);
}

void writeexpr(FILE *stream, atom *expr) {
	switch(type(expr)) {
		case type_num: fprintf(stream, "%g", numval(expr)); break;
		case type_sym: fprintf(stream, "%s", symval(expr)); break;
		case type_str: fprintf(stream, "\"%s\"", symval(expr)); break;
		case type_char: fprintf(stream, "#\\%s", charsym(expr->c)); break;
		case type_builtin: fprintf(stream, "#<builtin %p>", builtin(expr)); break;
		case type_fn:
			fprintf(stream, "#<fn ");
			writeexpr(stream, car(expr));
			fprintf(stream, ": ");
			writeexpr(stream, car(cdr(expr)));
			fprintf(stream, ">");
			break;
		case type_cons: 
			fprintf(stream, "(");
			for(;;) {
				writeexpr(stream, car(expr));
				if (no(cdr(expr))) { fprintf(stream, ")"); break; }
				expr = cdr(expr);
				if (type(expr) != type_cons) {
					fprintf(stream, " . ");
					writeexpr(stream, expr);
					fprintf(stream, ")");
					break;
				}
				fprintf(stream, " ");
			}
			break;
		default: error("writeexpr not implemented for this type", nil);
	}
}

atom *evlis(atom *exprs, atom *env);
atom *apply(atom *fn, atom *args, atom *env);
atom *eval(atom *exp, atom *env) {
	atom *tmp;
	if(exp == nil) return nil;
	switch(exp->type) {
		case type_builtin:
		case type_fn:
		case type_str:
		case type_num: return exp;
		case type_sym:
			tmp = assoc(exp, env);
			if(tmp == nil) error("Unbound symbol", exp);
			return cdr(tmp);
		case type_cons: {
			atom *op = car(exp), *args = cdr(exp);
			if (op == sym_if) {
				atom *cond;
				while (!no(args)) {
					cond = eval(car(args), env);
					if (no(cdr(args))) return cond;
					if (!no(cond)) return eval(car(cdr(args)), env);
					args = cdr(cdr(args));
				}
				return nil;
			} else if (op == sym_while) {
				atom *result = nil;
				while (!no(eval(car(args), env))) {
					atom *expr = cdr(args);
					while (!no(expr)) {
						result = eval(car(expr), env);
						expr = cdr(expr);
					}
				}
				return result;
			} else if(op == sym_fn) {
				return mkfn(car(args), cdr(args), env);
			} else if (op == sym_quote) {
				return car(args);
			} else if (op == sym_assign) {
				if (type(car(args)) == type_sym)
					return env_assign_eq(env, car(args), eval(car(cdr(args)), env));
				error("Cannot assign value to non-sym", car(args));
			}
			return apply(eval(op, env), evlis(args, env), env);
		}
	}
	return exp; // not reached
}

atom *evlis(atom *exprs, atom *env) {
	if(exprs == nil) return nil;
	return cons(eval(car(exprs), env), evlis(cdr(exprs), env));
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
	if (type(fn) == type_builtin) return (*builtin(fn))(args);
	else if (type(fn) == type_fn) {
		if (type(car(fn)) == type_sym)
			env_assign(cdr(cdr(fn)), car(fn), args);
		else env_assign_multiple(cdr(cdr(fn)), car(fn), args);
		return progn(car(cdr(fn)), cdr(cdr(fn)));
	} else if (type(fn) == type_str) {
		return mkchar(symval(fn)[(int)numval(car(args))]);
	}
	error("Bad argument to apply", fn);
}

int is(atom *a, atom *b) {
	if (type(a) == type(b))
		switch (type(a)) {
			case type_cons:
			case type_fn:
				return is(car(a), car(b)) && is(cdr(a), cdr(b));
			case type_sym: return a == b;
			case type_num: return numval(a) == numval(b);
			case type_builtin: return builtin(a) == builtin(b);
		}
	return 0;
}

atom *prim_is(atom *args) {
	atom *a, *b;
	while (!no(cdr(args))) {
		a = car(args);
		b = car(cdr(args));
		if (!is(a, b)) return nil;
		args = cdr(args);
	}
	return t;
}

atom *prim_add(atom *args) {
	double sum;
	for(sum = 0; !no(args); sum += numval(car(args)), args = cdr(args));
	return mknum(sum);
}

atom *prim_sub(atom *args) {
	double sum;
	for(sum = numval(car(args)), args = cdr(args); 
	    !no(args); 
	    sum -= numval(car(args)), args = cdr(args));
	return mknum(sum);
}

atom *prim_mul(atom *args) {
	double prod;
	for(prod = 1; !no(args); prod *= numval(car(args)), args = cdr(args));
	return mknum(prod);
}

atom *prim_div(atom *args) {
	double rem;
	for (rem = numval(car(args)), args = cdr(args);
	     !no(args);
	     rem /= numval(car(args)), args = cdr(args));
	return mknum(rem);
}

atom *prim_lt(atom *args) {
	atom *a, *b;
	if (type(a = car(args)) == type_num &&
	    type(b = car(cdr(args))) == type_num)
		return numval(a) < numval(b) ? t : nil;
	return nil;
}

atom *prim_pr(atom *args) {
	while (!no(args)) {
		switch (type(car(args))) {
			case type_str: printf("%s", symval(car(args))); break;
			case type_char: printf("%c", car(args)->c); break;
			default: writeexpr(stdout, car(args)); break;
		}
		args = cdr(args);
	}
	return nil;
}

atom *prim_cons(atom *args) { return cons(car(args), car(cdr(args))); }
atom *prim_car(atom *args)  { return car(car(args)); }
atom *prim_cdr(atom *args)  { return cdr(car(args)); }

atom *load_arc_file(const char *path) {
	printf("Loading \"%s\"", path);
	setinput(fopen(path, "r+"));
	atom *expr = nil, *prev = nil;
	while (prev = expr, !no(expr = eval(readexpr(), env_root))) putchar('.');
	putchar('\n');
	fclose(ifp);
	setinput(NULL);
	return prev;
}

void init_arc() {
	nil = mksym("nil");
	symbol_root = cons(nil, nil);
	env_root = env_create(nil);
	env_assign(env_root, t = intern("t"), t);
	sym_quote = intern("quote");
	sym_if = intern("if");
	sym_while = intern("while");
	sym_fn = intern("fn");
	sym_assign = intern("assign");
	env_assign(env_root, intern("is"), mkbuiltin(prim_is));
	env_assign(env_root, intern("<"), mkbuiltin(prim_lt));
	env_assign(env_root, intern("+"), mkbuiltin(prim_add));
	env_assign(env_root, intern("-"), mkbuiltin(prim_sub));
	env_assign(env_root, intern("*"), mkbuiltin(prim_mul));
	env_assign(env_root, intern("/"), mkbuiltin(prim_div));
	env_assign(env_root, intern("pr"), mkbuiltin(prim_pr));
	env_assign(env_root, intern("cons"), mkbuiltin(prim_cons));
	env_assign(env_root, intern("car"), mkbuiltin(prim_car));
	env_assign(env_root, intern("cdr"), mkbuiltin(prim_cdr));
	load_arc_file("arc.arc");
}

int main(int argc, char **argv) {
	puts("  mtl-arc v0.1");
	puts("================");
	init_arc();
	setinput(stdin);
	for(;;) {
		printf("%s", "> ");
		atom *o = eval(readexpr(), env_root);
		printf("%s", "=> ");
		writeexpr(stdout, o);
		printf("\n");
	}
	return 0;
}