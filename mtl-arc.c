// mtl-arc
//  A new implementation of the Arc language
// Copyright (C) 2014 Camden Smallwood

#include "mtl-arc.h"

atom nil, t, syms, root,
     sym_quote, sym_quasiquote, sym_unquote, sym_unquote_expand,
     sym_if, sym_is, sym_while, sym_assign, sym_bound, sym_fn, sym_mac;

atom make(atom_type type) {
	atom result = malloc(sizeof(struct atom));
	type(result) = type;
	return result;
}

atom cons(atom car, atom cdr) {
	atom result = make(type_cons);
	car(result) = car;
	cdr(result) = cdr;
	return result;
}

atom err(const char *message, atom context) {
	atom result = make(type_exception);
	exctx(result) = context;
	exmsg(result) = new_string(message);
	return result;
}

atom new_num(const double num) {
	atom result = make(type_num);
	numval(result) = num;
	return result;
}

atom new_sym(const char *sym) {
	if (strchr(sym, ' ') != NULL ||
	    strchr(sym, '\t') != NULL ||
	    strchr(sym, '\r') != NULL ||
	    strchr(sym, '\n') != NULL)
		return err("sym names may not contain whitespace characters", nil);
	if (sym[strspn(sym, "0123456789")] == '\0')
		return err("sym names may not consist of only numbers", nil);
	atom result = make(type_sym);
	symname(result) = strdup(sym);
	return result;
}

atom sym(const char *token) {
	for (atom p = syms; !no(p); p = cdr(p))
		if (!strcmp(token, symname(car(p))))
			return car(p);
	return car(syms = cons(new_sym(token), syms));
}

atom new_string(const char *string) {
	atom result = make(type_string);
	stringval(result) = strdup(string);
	return result;
}

atom new_char(const char c) {
	atom result = make(type_char);
	result->sym = malloc(1);
	charval(result) = c;
	return result;
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
	help(result) = strdup(doc);
	prim(result) = prim;
	return result;
}

atom new_input(FILE *stream) {
	atom result = make(type_input);
	stream(result) = stream;
	return result;
}

atom new_output(FILE *stream) {
	atom result = make(type_output);
	stream(result) = stream;
	return result;
}

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

char *char_to_string(const char value) {
	if (value == '\n') return "#\\newline";
	if (value == '\r') return "#\\return";
	if (value == '\t') return "#\\tab";
	if (value == ' ') return "#\\space";
	char cbuf[] = { 0, 0, 0 };
	sprintf(cbuf, "#\\%c", value);
	return strdup(cbuf);
}

char string_to_char(const char *str) {
	if (!strcmp(str, "#\\newline")) return '\n';
	if (!strcmp(str, "#\\return")) return '\r';
	if (!strcmp(str, "#\\tab")) return '\t';
	if (!strcmp(str, "#\\space")) return ' ';
	return str[2];
}

char **split_string(char *a_str, const char a_delim) {
	int count = 0;
	char **result = 0;
	char *last_delim = 0;
	char delim[] = { a_delim, 0 };
	for (char *c = a_str; *c; c++)
		if (a_delim == *c) { count++; last_delim = c; }
	count += last_delim < (a_str + strlen(a_str) - 1);
	count++;
	result = (char **)malloc(sizeof(char *) * (count + 1));
	if (result) {
		int i = 0;
		for (char *token = strtok(a_str, delim); token; token = strtok(0, delim))
			*(result + i++) = strdup(token);
		*(result + i) = 0;
	}
	result[count + 1] = NULL;
	return result;
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
	if(strchr("()[]\"`',;", ch)) return buf_to_string();
	for(;;) {
		if((ch = getc(stream)) == EOF) exit(0);
		if(strchr("()[]\"`',;", ch) || isspace(ch)) {
			ungetc(ch, stream);
			return buf_to_string();
		}
		add_to_buf(ch);
	}
}

atom read_expr(FILE *stream) {
	char *token = get_token(stream);
	if (token == NULL) {
		return nil;
	} else if (!strcmp(token, "(")) {
		return read_list(stream);
	} else if (!strcmp(token, "[")) {
		atom body = read_bracket(stream);
		if (iserr(body)) return body;
		return cons(sym_fn, cons(cons(sym("_"), nil), cons(body, nil)));
	} else if (!strcmp(token, "\"")) {
		return read_string(stream);
	} else if (!strcmp(token, ";")) {
		while (fgetc(stream) != '\n');
		return read_expr(stream);
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
			return sym(token);
		return new_num(atof(token));
	} else if (token[0] == '-' && strlen(token) > 1) {
    	return cons(sym("-"), cons(sym(&token[1]), nil));
    } else if (token[0] != '.' &&
	           token[strlen(token) - 1] != '.' &&
	           strchr(token, '.') != NULL) {
    	char **syms = split_string(token, '.');
    	return cons(sym(syms[0]), cons(sym(syms[1]), nil));
    } else if (token[0] != '!' &&
	           token[strlen(token) - 1] != '!' &&
	           strchr(token, '!') != NULL) {
    	char **syms = split_string(token, '!');
    	return cons(sym(syms[0]), cons(cons(sym_quote, cons(sym(syms[1]), nil)), nil));
    } else if (token[0] != ':' &&
	           token[strlen(token) - 1] != ':' &&
	           strchr(token, ':') != NULL) {
		char **syms = split_string(token, ':');
		atom dims = nil, comps = nil;
		for (int i = 0; syms[i] != NULL; i++)
			dims = cons(sym(syms[i]), dims);
		for (; !no(dims); comps = cons(car(dims), comps), dims = cdr(dims));
		return cons(sym("compose"), comps);
    } else if (token[0] == '~' && strlen(token) > 1) {
    	return cons(sym("complement"), cons(sym(&token[1]), nil));
    } else if (strlen(token) > 2) { // possible reader macro
		if (token[0] == '#' && token[1] == '\\') { // char
			return new_char(string_to_char(token));
		} else if (token[0] != '/' && // ratio
		           token[strlen(token) - 1] != '/' &&
		           strchr(token, '/') != NULL) {
			char **nums = split_string(strdup(token), '/');
			if (!(nums[0][strspn(nums[0], "0123456789")] == '\0') ||
			    !(nums[1][strspn(nums[1], "0123456789")] == '\0'))
				return sym(token);
			return cons(sym("/"),
			            cons(new_num(atof(nums[0])),
			                 cons(new_num(atof(nums[1])), nil)));
		}
	}
	return sym(token);
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

atom read_bracket(FILE *stream) {
	char *token = get_token(stream);
	if (!strcmp(token, "]")) {
		return nil;
	}
	unget_token(token);
	return cons(read_expr(stream), read_bracket(stream));
}

atom read_string(FILE *stream) {
	char c, cbuf[1024];
	memset(cbuf, 0, 1024);
	int n = 0;
	while ((c = fgetc(stream)) != '\"') cbuf[n++] = c;
	return new_string(cbuf);
}

void write_expr(FILE *stream, atom expr) {
	if (anum(expr)) {
		fprintf(stream, "%g", numval(expr));
	} else if (asym(expr)) {
		fprintf(stream, "%s", symname(expr));
	} else if (astring(expr)) {
		fprintf(stream, "\"%s\"", stringval(expr));
	} else if (achar(expr)) {
		fprintf(stream, "%s", char_to_string(charval(expr)));
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
		fprintf(stream, "#<builtin %p>", prim(expr));
	} else if (isinput(expr)) {
		fprintf(stream, "#<input %p>", stream(expr));
	} else if (isoutput(expr)) {
		fprintf(stream, "#<output %p>", stream(expr));
	} else if (iserr(expr)) {
		fprintf(stream, "exception:\n==> %s", stringval(exmsg(expr)));
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

atom apply(atom fn, atom args) {
	if (abuiltin(fn)) {
		return (*prim(fn))(args);
	} else if (afn(fn)) {
		atom env = env_create(car(fn));
		for (atom names = cadr(fn); !no(names); names = cdr(names)) {
			if (asym(names)) {
				env_assign(env, names, args);
				args = nil;
				break;
			} else if (asym(car(names))) {
				if (no(args))
					return err("invalid arguments supplied to 'fn'", nil);
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
			return err("invalid arguments supplied to 'fn'", nil);
		atom result = nil;
		for (atom body = cddr(fn); !no(body); body = cdr(body))
			if (iserr(result = eval(car(body), env)))
				return result;
		return result;
	} else if (astring(fn)) {
		return new_char(stringval(fn)[(int)numval(car(args))]);
	} else if (atable(fn)) {
		return tget(fn, car(args));
	} else if (alist(fn)) {
		if (no(args) || !no(cdr(args)) || !anum(car(args)))
			return err("invalid arguments supplied as list index", args);
		for (int i = 0; !no(fn); fn = cdr(fn), i++)
			if (i == (int)numval(car(args)))
				return car(fn);
		return err("index is outside the bounds of the list", car(args));
	} else {
		return err("invalid operator supplied to 'apply'", fn);
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
				if (iserr(cond = eval(car(args), env))) return cond;
				if (no(cdr(args))) return cond;
				if (!no(cond)) return eval(cadr(args), env);
				args = cddr(args);
			}
			return nil;
		} else if (op == sym_is) {
			atom a, b;
			if (iserr(a = eval(car(args), env))) return a;
			if (iserr(b = eval(cadr(args), env))) return b;
			if (type(a) == type(b)) {
				switch (type(a)) {
					case type_num: return numval(a) == numval(b) ? t : nil;
					case type_sym: return a == b ? t : nil;
					case type_string: return !strcmp(stringval(a), stringval(b)) ? t : nil;
					case type_char: return charval(a) == charval(b) ? t : nil;
					case type_builtin: return prim(a) == prim(b) ? t : nil;
					case type_input:
					case type_output: return stream(a) == stream(b) ? t : nil;
					default: return nil;
				}
			}
			return nil;
		} else if (op == sym_while) {
			if (no(args))
				return err("invalid arguments supplied to 'while'", args);
			atom result, pred = car(args);
			while (!iserr(result = eval(pred, env)) && !no(result)) {
				if (iserr(result)) return result;
				for (atom e = cdr(args); !no(e); e = cdr(e))
					if (iserr(result = eval(car(e), env)))
						return result;
			}
			return result;
		} else if (op == sym_assign) {
			if (asym(car(args))) {
				atom val = eval(cadr(args), env);
				if (iserr(val)) return val;
				return env_assign_eq(env, car(args), val);
			} else if (acons(car(args))) {
				if (caar(args) == sym("car")) {
					atom place = eval(car(cdar(args)), env);
					if (iserr(place)) return place;
					atom val = eval(cadr(args), env);
					if (iserr(val)) return val;
					return car(place) = val;
				} else if (caar(args) == sym("cdr")) {
					atom place = eval(car(cdar(args)), env);
					if (iserr(place)) return place;
					atom val = eval(cadr(args), env);
					if (iserr(val)) return val;
					return cdr(place) = val;
				} else {
					atom iop = eval(caar(args), env);
					if (iserr(iop)) {
						return iop;
					} else if (alist(iop)) {
						atom index = eval(car(cdar(args)), env);
						if (iserr(index)) return index;
						if (!anum(index))
							return err("invalid index supplied to list", index);
						atom value = eval(cadr(args), env);
						if (iserr(value)) return value;
						for (int i = 0; !no(iop); iop = cdr(iop), i++)
							if (i == (int)numval(index))
								return car(iop) = value;
						return err("index is outside the bounds of the list", index);
					} else if (atable(iop)) {
						atom key = eval(car(cdar(args)), env);
						if (iserr(key)) return key;
						atom value = eval(cadr(args), env);
						if (iserr(value)) return value;
						return tset(iop, key, value);
					} else if (astring(iop)) {
						atom index = eval(car(cdar(args)), env);
						if (iserr(index)) return index;
						if (!anum(index) || numval(index) < 0 || numval(index) >= strlen(stringval(iop)))
							return err("invalid index applied to 'string'", index);
						atom value = eval(cadr(args), env);
						if (iserr(value)) return value;
						if (!achar(value))
							return err("value of a 'string' element must be a 'char'", value);
						stringval(iop)[(int)numval(index)] = charval(value);
						return value;
					}
				}
			}
			return err("cannot assign value to place", car(args));
		} else if (op == sym_bound) {
			if (no(args) || !no(cdr(args)))
				return err("invalid arguments supplied to 'bound'", args);
			atom arg = eval(car(args), env);
			if (iserr(arg)) return arg;
			if (!asym(arg))
				return err("invalid argument supplied to 'bound'", arg);
			for (; !no(env); env = car(env))
				for (atom bs = cdr(env); !no(bs); bs = cdr(bs))
					if (caar(bs) == arg)
						return t;
			return nil;
		} else if (op == sym_fn) {
			return new_fn(car(args), cdr(args), env);
		} else if (op == sym_mac) {
			if (no(args) || no(cdr(args)) || no(cdr(cdr(args))))
				return err("invalid arguments supplied to 'mac'", args);
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
			type(op) = type_fn;
			atom ex = apply(op, args);
			type(op) = type_mac;
			if (iserr(ex)) return ex;
			return eval(ex, env);
		}
		args = copy_list(args);
		for (atom p = args; !no(p); p = cdr(p))
			if (iserr(car(p) = eval(car(p), env)))
				return car(p);
		return apply(op, args);
	}
}

atom coerce_num(atom val) {
	if (no(val))
		return new_num(0);
	if (anum(val))
		return val;
	if (asym(val) || astring(val))
		return new_num(atof(val->sym));
	if (achar(val))
		return new_num((double)charval(val));
	return err("can't coerce to 'num'", val);
}

atom coerce_sym(atom val) {
	if (no(val) || asym(val))
		return val;
	if (astring(val))
		return sym(stringval(val));
	if (achar(val)) {
		char buf[] = { charval(val), '\0' };
		return sym(buf);
	}
	return err("can't coerce to 'sym'", val);
}

atom coerce_char(atom val) {
	return err("can't coerce to 'char'", val);
}

atom coerce_cons(atom val) {
	if (astring(val)) {
		atom result = nil;
		for (int i = strlen(stringval(val)) - 1; i != -1; i--)
			result = cons(new_char(stringval(val)[i]), result);
		return result;
	}
	return err("can't coerce to 'cons'", val);
}

atom coerce_table(atom val) {
	return err("can't coerce to 'table'", val);
}

atom builtin_cons(atom args) {
	if (no(args) || no(cdr(args)) || !no(cddr(args)))
		return err("invalid arguments supplied to 'cons'", args);
	return cons(car(args), cadr(args));
}

atom builtin_car(atom args) {
	if (no(args) || !no(cdr(args)))
		return err("invalid arguments supplied to 'car'", args);
	if (car(args) == nil) return nil;
	return caar(args);
}

atom builtin_cdr(atom args) {
	if (no(args) || !no(cdr(args)))
		return err("invalid arguments supplied to 'cdr'", args);
	if (car(args) == nil) return nil;
	return cdar(args);
}

atom builtin_type(atom args) {
	if (no(args) || !no(cdr(args)))
		return err("invalid arguments supplied to 'type'", args);
	switch (type(car(args))) {
		case type_num: return sym("num");
		case type_sym: return sym("sym");
		case type_string: return sym("string");
		case type_char: return sym("char");
		case type_cons: return sym("cons");
		case type_fn: return sym("fn");
		case type_mac: return sym("mac");
		case type_table: return sym("table");
		case type_builtin: return sym("builtin");
		case type_input: return sym("input");
		case type_output: return sym("output");
		case type_exception: return sym("exception");
	}
	return err("unknown type of atom", car(args));
}

atom builtin_err(atom args) {
	if (no(args))
		args = cons(new_string("unspecified"), nil);
	return err(stringval(car(args)), no(cdr(args)) ? nil : cadr(args));
}

atom builtin_help(atom args) {
	if (no(args) || !no(cdr(args)))
		return err("invalid arguments supplied to 'help'", args);
	atom place;
	if (asym(car(args)))
		place = eval(car(args), root);
	else place = car(args);
	if (abuiltin(place)) {
		if (help(place) == NULL) return nil;
		return new_string(help(place));
	}
	if (afn(place) || amac(place))
		if (astring(car(cddr(place))))
			return car(cddr(place));
	return nil;
}

atom builtin_add(atom args) {
	if (no(args) || !(anum(car(args)) || astring(car(args))))
		return err("invalid arguments supplied to '+'", args);
	if (anum(car(args))) {
		for (atom p = args; !no(p); p = cdr(p))
			if (!anum(car(p)))
				return err("invalid arguments supplied to '+'", args);
		double sum;
		for(sum = 0; !no(args); sum += numval(car(args)), args = cdr(args));
		return new_num(sum);
	} else if (astring(car(args))) {
		for (atom p = args; !no(p); p = cdr(p))
			if (!astring(car(p)))
				return err("invalid arguments supplied to '+'", args);
		char buf[1024];
		memset(buf, 0, 1024);
		for (; !no(args); args = cdr(args))
			sprintf(buf, "%s%s", buf, stringval(car(args)));
		return new_string(buf);
	}
}

atom builtin_sub(atom args) {
	if (!no(args))
		for (atom p = args; !no(p); p = cdr(p))
			if (!anum(car(p)))
				return err("invalid arguments supplied to '-'", args);
	double sum = no(cdr(args)) ? -numval(car(args)) : numval(car(args));
	for(args = cdr(args); !no(args); sum -= numval(car(args)), args = cdr(args));
	return new_num(sum);
}

atom builtin_mul(atom args) {
	if (!no(args))
		for (atom p = args; !no(p); p = cdr(p))
			if (!anum(car(p)))
				return err("invalid arguments supplied to '*'", args);
	double prod;
	for(prod = 1; !no(args); prod *= numval(car(args)), args = cdr(args));
	return new_num(prod);
}

atom builtin_div(atom args) {
	if (!no(args))
		for (atom p = args; !no(p); p = cdr(p))
			if (!anum(car(p)))
				return err("invalid arguments supplied to '/'", args);
	double rem = numval(car(args));
	for (args = cdr(args); !no(args); rem /= numval(car(args)), args = cdr(args));
	return new_num(rem);
}

atom builtin_lt(atom args) {
	if (no(args) || no(cdr(args)) || !no(cddr(args)))
		return err("invalid arguments supplied to '<'", args);
	atom a = car(args), b = cadr(args);
	if (anum(a) && anum(b))
		return numval(a) < numval(b) ? t : nil;
	else if (achar(a) && achar(b))
		return charval(a) < charval(b) ? t : nil;
	return nil;
}

atom builtin_gt(atom args) {
	if (no(args) || no(cdr(args)) || !no(cddr(args)))
		return err("invalid arguments supplied to '>'", args);
	atom a, b;
	if (anum(a = car(args)) && anum(b = cadr(args)))
		return numval(a) > numval(b) ? t : nil;
	else if (achar(a) && achar(b))
		return charval(a) > charval(b) ? t : nil;
	return nil;
}

atom builtin_cos(atom args) {
	if (no(args) || !no(cdr(args)) || !anum(car(args)))
		return err("invalid arguments supplied to 'cos'", args);
	return new_num(sin(numval(car(args))));
}

atom builtin_expt(atom args) {
	if (no(args) || no(cdr(args)) || !no(cddr(args)) || !anum(car(args)) || !anum(cadr(args)))
		return err("invalid arguments supplied to 'expt'", args);
	return new_num(pow(numval(car(args)), numval(cadr(args))));
}

atom builtin_log(atom args) {
	if (no(args) || !no(cdr(args)) || !anum(car(args)))
		return err("invalid arguments supplied to 'log'", args);
	return new_num(log(numval(car(args))));
}

atom builtin_mod(atom args) {
	if (no(args) || no(cdr(args)) || !no(cddr(args)) || !anum(car(args)) || !anum(cadr(args)))
		return err("invalid arguments supplied to 'mod'", args);
	return new_num((double)((long long)numval(car(args)) %
	                        (long long)numval(cadr(args))));
}

#define double_rand() ((double)rand() / ((double)RAND_MAX + 1.0))

atom builtin_rand(atom args) {
	if (no(args))
		return new_num(double_rand());
	else if (!no(args) && no(cdr(args)) && anum(car(args)))
		return new_num(floor(double_rand() * numval(car(args))));;
	return err("invalid arguments supplied to 'rand'", args);
}

atom builtin_sin(atom args) {
	if (no(args) || !no(cdr(args)) || !anum(car(args)))
		return err("invalid arguments supplied to 'sin'", args);
	return new_num(sin(numval(car(args))));
}

atom builtin_sqrt(atom args) {
	if (no(args) || !no(cdr(args)) || !anum(car(args)))
		return err("invalid arguments supplied to 'sqrt'", args);
	return new_num(sqrt(numval(car(args))));
}

atom builtin_tan(atom args) {
	if (no(args) || !no(cdr(args)) || !anum(car(args)))
		return err("invalid arguments supplied to 'tan'", args);
	return new_num(tan(numval(car(args))));
}

atom builtin_trunc(atom args) {
	if (no(args) || !no(cdr(args)) || !anum(car(args)))
		return err("invalid arguments supplied to 'trunc'", args);
	return new_num(trunc(numval(car(args))));
}

atom builtin_shl(atom args) {
	if (no(args) || no(cdr(args)) || !no(cddr(args)) || !anum(car(args)) || !anum(cadr(args)))
		return err("invalid arguments supplied to 'shl'", args);
	return new_num((double)((long long)numval(car(args)) <<
	                        (long long)numval(cadr(args))));
}

atom builtin_string(atom args) {
	if (no(args))
		return err("invalid arguments supplied to 'string'", args);
	char buf[1024];
	memset(buf, 0, sizeof(buf));
	for (; !no(args); args = cdr(args)) {
		if (alist(car(args))) {
			atom val = builtin_string(car(args));
			if (iserr(val)) return val;
			sprintf(buf, "%s%s", buf, stringval(val));
		} else {
			switch (type(car(args))) {
				case type_num: sprintf(buf, "%s%g", buf, numval(car(args))); break;
				case type_sym: sprintf(buf, "%s%s", buf, symname(car(args))); break;
				case type_string: sprintf(buf, "%s%s", buf, stringval(car(args))); break;
				case type_char: sprintf(buf, "%s%c", buf, charval(car(args))); break;
				default: return err("can't coerce to 'string'", car(args));
			}
		}
	}
	return new_string(buf);
}

atom builtin_newstring(atom args) {
	if (no(args) || !anum(car(args)))
		return err("invalid arguments supplied to 'newstring'", args);
	atom value = new_char(' ');
	if (!no(cdr(args))) {
		if (!achar(cadr(args)))
			return err("invalid arguments supplied to 'newstring'", args);
		value = cadr(args);
	}
	int size = (int)numval(car(args));
	char *val = malloc(size + 1);
	memset(val, 0, size + 1);
	for (int i = 0; i < size; i++)
		val[i] = charval(value);
	val[size + 1] = '\0';
	return new_string(val);
}

atom builtin_coerce(atom args) {
	if (no(args) || no(cdr(args)) || !asym(cadr(args)))
		return err("invalid arguments supplied to 'coerce'", args);
	atom val = car(args), type = cadr(args);
	if (type == sym("num"))
		return coerce_num(val);
	else if (type == sym("sym"))
		return coerce_sym(val);
	else if (type == sym("string"))
		return builtin_string(cons(val, nil));
	else if (type == sym("char"))
		return coerce_char(val);
	else if (type == sym("cons") || type == sym("list"))
		return coerce_cons(val);
	else if (type == sym("table"))
		return coerce_table(val);
	return err("can't coerce to type", type);
}

atom builtin_apply(atom args) {
	if (no(args)) return err("invalid arguments supplied to 'apply'", args);
	return apply(car(args), cdr(args));
}

atom builtin_eval(atom args) {
	if (no(args) || !no(cdr(args)))
		return err("invalid arguments supplied to 'eval'", args);
	return eval(car(args), root);
}

atom builtin_len(atom args) {
	if (no(args) || !no(cdr(args)))
		return err("invalid arguments supplied to 'len'", args);
	args = car(args);
	if (astring(args))
		return new_num((double)strlen(stringval(args)));
	if (alist(args)) {
		int count;
		for (count = 0; !no(args); args = cdr(args), count++);
		return new_num((double)count);
	}
	return err("invalid argument supplied to 'len'", args);
}

atom builtin_stdin(atom args) {
	if (!no(args))
		return err("invalid arguments supplied to 'stdin'", args);
	return new_input(stdin);
}

atom builtin_stdout(atom args) {
	if (!no(args))
		return err("invalid arguments supplied to 'stdout'", args);
	return new_output(stdout);
}

atom builtin_stderr(atom args) {
	if (!no(args))
		return err("invalid arguments supplied to 'stderr'", args);
	return new_output(stderr);
}

atom builtin_readb(atom args) {
	FILE *stream;
	if (no(args))
		stream = stdin;
	else if (!no(cdr(args)) || !isinput(car(args)))
		return err("invalid arguments supplied to 'readb'", args);
	else stream = stream(car(args));
	return new_num((double)fgetc(stream));
}

atom builtin_readc(atom args) {
	FILE *stream;
	if (no(args))
		stream = stdin;
	else if (!no(cdr(args)) || !isinput(car(args)))
		return err("invalid arguments supplied to 'readc'", args);
	else stream = stream(car(args));
	return new_char(fgetc(stream));
}

atom builtin_peekc(atom args) {
	FILE *stream;
	if (no(args))
		stream = stdin;
	else if (!no(cdr(args)) || !isinput(car(args)))
		return err("invalid arguments supplied to 'peekc'", args);
	else stream = stream(car(args));
	char val = fgetc(stream);
	ungetc(val, stream);
	return new_char(val);
}

atom builtin_readline(atom args) {
	FILE *stream;
	char buf[1024];
	int i = 0;
	memset(buf, 0, 1024);
	if (no(args))
		stream = stdin;
	else if (!no(cdr(args)) || !isinput(car(args)))
		return err("invalid arguments supplied to 'readline'", args);
	else stream = stream(car(args));
	for (buf[0] = fgetc(stream), i = 0;
	     buf[i] != '\n' && buf[i] != '\r';
	     buf[++i] = fgetc(stream));
	buf[i] = '\0';
	return new_string(buf);
}

atom builtin_sread(atom args) {
	FILE *stream;
	if (no(args))
		stream = stdin;
	else if (!no(cdr(args)) || !isinput(car(args)))
		return err("invalid arguments supplied to 'sread'", args);
	else stream = stream(car(args));
	return read_expr(stream);
}

atom arc_load_file(const char *path) {
	printf("loading \"%s\"...\n", path);
	FILE *stream = fopen(path, "r+");
	atom prev, expr = nil;
	while (prev = expr, (expr = eval(read_expr(stream), root)) != nil)
		if (iserr(expr)) { printf("=> "); write_expr(stdout, expr); putchar('\n'); }
	fclose(stream);
	return prev;
}

atom builtin_load(atom args) {
	if (no(args) || !no(cdr(args)) || !astring(car(args)))
		return err("invalid arguments supplied to 'load'", args);
	return arc_load_file(stringval(car(args)));
}

atom builtin_disp(atom args) {
	if (no(args))
		return nil;
	FILE *stream;
	if (no(cdr(args)))
		stream = stdout;
	else if (!isoutput(cadr(args)) || !no(cddr(args)))
		return err("invalid arguments supplied to 'disp'", args);
	else stream = stream(cadr(args));
	if (astring(car(args)))
		fprintf(stream, "%s", stringval(car(args)));
	else if (achar(car(args))) 
		fprintf(stream, "%c", charval(car(args)));
	else
		write_expr(stdout, car(args));
	return nil;
}

atom builtin_write(atom args) {
	if (no(args))
		return nil;
	FILE *stream;
	if (no(cdr(args)))
		stream = stdout;
	else if (!isoutput(cadr(args)) || !no(cddr(args)))
		return err("invalid arguments supplied to 'write'", args);
	else stream = stream(cadr(args));
	write_expr(stream, car(args));
	return nil;
}

atom builtin_writeb(atom args) {
	if (no(args) || !anum(car(args)))
		return err("invalid arguments supplied to 'writeb'", args);
	FILE *stream;
	if (no(cdr(args)))
		stream = stdout;
	else if (!isoutput(cadr(args)) || !no(cddr(args)))
		return err("invalid arguments supplied to 'writeb'", args);
	else stream = stream(cadr(args));
	fputc((unsigned char)numval(car(args)), stream);
	return nil;
}

atom builtin_writec(atom args) {
	if (no(args) || !achar(car(args)))
		return err("invalid arguments supplied to 'writeb'", args);
	FILE *stream;
	if (no(cdr(args)))
		stream = stdout;
	else if (!isoutput(cadr(args)) || !no(cddr(args)))
		return err("invalid arguments supplied to 'writeb'", args);
	else stream = stream(cadr(args));
	fputc(charval(car(args)), stream);
	return nil;
}

atom builtin_infile(atom args) {
	if (no(args) || !astring(car(args)) || (!no(cdr(args)) && !asym(cadr(args))))
		return err("invalid arguments supplied to 'infile'", args);
	FILE *stream = fopen(stringval(car(args)), "r");
	return new_input(stream);
}

atom builtin_outfile(atom args) {
	if (no(args) || !astring(car(args)) || (!no(cdr(args)) && !asym(cadr(args))))
		return err("invalid arguments supplied to 'outfile'", args);
	FILE *stream = fopen(stringval(car(args)), "w");
	return new_output(stream);
}

atom builtin_close(atom args) {
	if (no(args) || !no(cdr(args)) || !isinput(car(args)) || !isoutput(car(args)))
		return err("invalid arguments supplied to 'close'", args);
	fclose(stream(car(args)));
	return nil;
}

void arc_init() {
	nil = new_sym("nil");
	syms = cons(nil, nil);
	root = cons(nil, nil);
	sym_quote = sym("quote");
	sym_quasiquote = sym("quasiquote");
	sym_unquote = sym("unquote");
	sym_unquote_expand = sym("unquote-expand");
	sym_if = sym("if");
	sym_is = sym("is");
	sym_while = sym("while");
	sym_assign = sym("assign");
	sym_bound = sym("bound");
	sym_fn = sym("fn");
	sym_mac = sym("mac");
	env_assign(root, t = sym("t"), t);
	env_assign(root, sym("cons"), new_builtin(builtin_cons, ""));
	env_assign(root, sym("car"), new_builtin(builtin_car, ""));
	env_assign(root, sym("cdr"), new_builtin(builtin_cdr, ""));
	env_assign(root, sym("type"), new_builtin(builtin_type, ""));
	env_assign(root, sym("err"), new_builtin(builtin_err, ""));
	env_assign(root, sym("help"), new_builtin(builtin_help, ""));
	env_assign(root, sym("apply"), new_builtin(builtin_apply, ""));
	env_assign(root, sym("eval"), new_builtin(builtin_eval, ""));
	env_assign(root, sym("+"), new_builtin(builtin_add, ""));
	env_assign(root, sym("-"), new_builtin(builtin_sub, ""));
	env_assign(root, sym("*"), new_builtin(builtin_mul, ""));
	env_assign(root, sym("/"), new_builtin(builtin_div, ""));
	env_assign(root, sym("<"), new_builtin(builtin_lt, ""));
	env_assign(root, sym(">"), new_builtin(builtin_gt, ""));
	env_assign(root, sym("cos"), new_builtin(builtin_cos, ""));
	env_assign(root, sym("expt"), new_builtin(builtin_expt, ""));
	env_assign(root, sym("log"), new_builtin(builtin_log, ""));
	env_assign(root, sym("mod"), new_builtin(builtin_mod, ""));
	env_assign(root, sym("rand"), new_builtin(builtin_rand, ""));
	env_assign(root, sym("sin"), new_builtin(builtin_sin, ""));
	env_assign(root, sym("sqrt"), new_builtin(builtin_sqrt, ""));
	env_assign(root, sym("tan"), new_builtin(builtin_tan, ""));
	env_assign(root, sym("trunc"), new_builtin(builtin_trunc, ""));
	env_assign(root, sym("shl"), new_builtin(builtin_shl, ""));
	env_assign(root, sym("table"), new_builtin(table, ""));
	env_assign(root, sym("string"), new_builtin(builtin_string, ""));
	env_assign(root, sym("newstring"), new_builtin(builtin_newstring, ""));
	env_assign(root, sym("coerce"), new_builtin(builtin_coerce, ""));
	env_assign(root, sym("len"), new_builtin(builtin_len, ""));
	env_assign(root, sym("stdin"), new_builtin(builtin_stdin, ""));
	env_assign(root, sym("stdout"), new_builtin(builtin_stdout, ""));
	env_assign(root, sym("stderr"), new_builtin(builtin_stderr, ""));
	env_assign(root, sym("readb"), new_builtin(builtin_readb, ""));
	env_assign(root, sym("readc"), new_builtin(builtin_readc, ""));
	env_assign(root, sym("peekc"), new_builtin(builtin_peekc, ""));
	env_assign(root, sym("readline"), new_builtin(builtin_readline, ""));
	env_assign(root, sym("sread"), new_builtin(builtin_sread, ""));
	env_assign(root, sym("load"), new_builtin(builtin_load, ""));
	env_assign(root, sym("disp"), new_builtin(builtin_disp, ""));
	env_assign(root, sym("write"), new_builtin(builtin_write, ""));
	env_assign(root, sym("writeb"), new_builtin(builtin_writeb, ""));
	env_assign(root, sym("writec"), new_builtin(builtin_writec, ""));
	env_assign(root, sym("infile"), new_builtin(builtin_infile, ""));
	env_assign(root, sym("outfile"), new_builtin(builtin_outfile, ""));
	env_assign(root, sym("close"), new_builtin(builtin_close, ""));
	arc_load_file("arc.arc");
}

int main(int argc, char **argv) {
	puts("  mtl-arc v0.3\n================");
	arc_init();
	for(;;) {
		printf("%s", "> ");
		atom result = eval(read_expr(stdin), root);
		printf("%s", "=> ");
		write_expr(stdout, result);
		puts("");
	}
	return 0;
}