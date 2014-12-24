//==============================================================
// MTL-Arc
//   A new implementation of the Arc language
// Copyright (C) 2014 Camden Smallwood
//==============================================================
// port.c
//   Port system - I/O to files, strings and sockets
//==============================================================

#include <stdlib.h>
#include "port.h"

int port_readb(port_t *port) {
	unsigned char result;
	
	if (!is_input_port(port) || port_state(port) != PORT_OPEN)
		return EOF;

	switch (port_type(port)) {
		case PORT_FILE:
		case PORT_SOCKET:
			read(port_fd(port), &result, sizeof(result));
			break;
		default: // strings and other abstract io types
			if (port_pos(port) >= port_size(port))
				return EOF;
			result = port_data(port)[port_pos(port)];
			(*port).pos++;
			break;
	}
	
	return result;
}

int port_readc(port_t *port) {
	char result;
	
	if (!is_input_port(port) || port_state(port) != PORT_OPEN)
		return EOF;

	switch (port_type(port)) {
		case PORT_FILE:
		case PORT_SOCKET:
			read(port_fd(port), &result, sizeof(result));
			break;
		default: // strings and other abstract io types
			if (port_pos(port) >= port_size(port))
				return EOF;
			result = port_data(port)[port_pos(port)];
			(*port).pos++;
			break;
	}
	
	return result;
}

int port_peekc(port_t *port) {
	char result;
	FILE *file;
	
	if (!is_input_port(port) || port_state(port) != PORT_OPEN)
		return EOF;
	
	switch (port_type(port)) {
		case PORT_FILE:
		case PORT_SOCKET:
			file = fdopen(port_fd(port), "r");
			result = fgetc(file);
			ungetc(result, file);
			break;
		default: // strings and other abstract io types
			if (port_pos(port) >= port_size(port))
				return EOF;
			result = port_data(port)[port_pos(port) + 1];
			break;
	}
	
	return result;
}

int port_buf_size = 0, port_last_valid = 0;
#define PORT_BUF_MAX 1024
char port_buf[PORT_BUF_MAX], *port_last_token;

char *port_buf_to_string() {
	port_buf[port_buf_size++] = '\0';
	return strdup(port_buf);
}

void add_to_port_buf(const char c) {
	if (port_buf_size < PORT_BUF_MAX - 1)
		port_buf[port_buf_size++] = c;
}

void port_unget_token(char *token) {
	port_last_token = token;
	port_last_valid = 1;
}

char *port_get_token(port_t *port) {
	int c;
	
	if (!is_input_port(port) || port_state(port) != PORT_OPEN)
		return NULL;
	
	port_buf_size = 0;
	if (port_last_valid) {
		port_last_valid = 0;
		return port_last_token;
	}
	
	do {
		if ((c = port_readc(port)) == EOF)
			return NULL;
	} while (isspace(c));
	
	add_to_port_buf(c);
	if (strchr("(){}[]\"`',;", c))
		return port_buf_to_string();
	
	for (;;) {
		if ((c = port_readc(port)) == EOF)
			return NULL;
		if (strchr("(){}[]\"`',;", c) || isspace(c)) {
			port_seek(port, -1, SEEK_CUR);
			return port_buf_to_string();
		}
		add_to_port_buf(c);
	}
}

atom_t *port_read_list(port_t *port) {
	char *token;
	atom_t *head, *tail;
	
	if (!is_input_port(port))
		return err("port is not an input port", nil);
	if (port_state(port) != PORT_OPEN)
		return err("port is not open", nil);
	
	token = port_get_token(port);
	if (!strcmp(token, ")")) {
		return nil;
	} else if (!strcmp(token, ".")) {
		head = port_sread(port);
		if (iserr(head) || strcmp(port_get_token(port), ")"))
			return err("invalid syntax while reading list", nil);
		return head;
	}
	port_unget_token(token);
	
	if (iserr(head = port_sread(port)))
		return head;
	if (iserr(tail = port_read_list(port)))
		return tail;
	
	return cons(head, tail);
}

atom_t *port_read_bracket(port_t *port) {
	char *token;
	atom_t *head, *tail;
	
	if (!is_input_port(port))
		return err("port is not an input port", nil);
	if (port_state(port) != PORT_OPEN)
		return err("port is not open", nil);
	
	token = port_get_token(port);
	if (!strcmp(token, "]"))
		return nil;
	unget_token(token);
	
	if (iserr(head = port_sread(port)))
		return head;
	if (iserr(tail = port_read_bracket(port)))
		return tail;
	
	return cons(head, tail);
}

atom_t *port_read_brace(port_t *port) {
	char *token;
	atom_t *head, *tail;
	
	if (!is_input_port(port))
		return err("port is not an input port", nil);
	if (port_state(port) != PORT_OPEN)
		return err("port is not open", nil);
	
	token = port_get_token(port);
	if (!strcmp(token, "}"))
		return nil;
	unget_token(token);
	
	if (iserr(head = port_sread(port)))
		return head;
	if (iserr(tail = port_read_brace(port)))
		return tail;
	
	return cons(head, tail);
}

atom_t *port_read_string(port_t *port) {
	char c, cbuf[1024];
	memset(cbuf, 0, sizeof(cbuf));
	int n = 0;
	while ((c = port_readc(port)) != '\"') cbuf[n++] = c;
	return new_string(cbuf);
}

atom_t *port_sread(port_t *port) {
	char *token = port_get_token(port);
	
	if (token == NULL)
		return nil;
	
	// lists: (a b c)
	if (!strcmp(token, "("))
		return port_read_list(port);
	
	// infix: {1 + 2 / 3} => (/ (+ 1 2) 3) => 1
	if (!strcmp(token, "{"))
		return infix_expand(port_read_brace(port));
	
	// anonymous functions: [list _] => (fn (_) (list _))
	if (!strcmp(token, "[")) {
		atom_t *body = port_read_bracket(port);
		if (iserr(body))
			return body;
		return cons(sym_fn, cons(cons(sym_wildcard, nil), cons(body, nil)));
	}
	
	// strings: "This is a string, by golly!"
	if (!strcmp(token, "\""))
		return port_read_string(port);
	
	// end-of-line comments: ;This is a fancy comment
	if (!strcmp(token, ";")) {
		while (port_readc(port) != '\n');
		return port_read_expr(port);
	}
	
	// quote: 'x => (quote x)
	if (!strcmp(token, "'"))
		return cons(sym_quote, cons(port_read_expr(port), nil));
	
	// quasiquote: `(list 1 2 3) => (list 1 2 3)
	if (!strcmp(token, "`"))
		return cons(sym_quasiquote, cons(port_read_expr(port), nil));
	
	// unquote & unquote-expand: (with (x 1 y '(2 3 4))
	if (!strcmp(token, ",")) { //  `(list ,x ,@y)) => (1 2 3 4)
		char c = port_readc(port);
		if (c == '@')
			return cons(sym_unquote_expand, cons(port_read_expr(port), nil));
		port_seek(port, -1, SEEK_CUR);
		return cons(sym_unquote, cons(port_read_expr(port), nil));
	}
	
	// numbers: 1, -0.5, .007, 9999999, etc.
	if (token[strspn(token, "-.0123456789")] == '\0') {
		if (token[strspn(token, "-.")] == '\0')
			return ssexpand(token);
		return new_num(atof(token));
	}

	// reader macros
	if (strlen(token) > 2) {
		// characters
		if (token[0] == '#' && token[1] == '\\')
			return new_char(token_to_char(token));
		
		// fractions
		if (token[0] != '/' && token[strlen(token) - 1] != '/' && strchr(token, '/') != NULL) {
			char **nums = split_string(strdup(token), '/');
			if (nums[2] != NULL ||
			    !(nums[0][strspn(nums[0], "0123456789")] == '\0') ||
			    !(nums[1][strspn(nums[1], "0123456789")] == '\0'))
				return ssexpand(token);
			return cons(sym_divide,
			            cons(new_num(atof(nums[0])),
			                 cons(new_num(atof(nums[1])), nil)));
		}
	}

	// expand special syntax if present
	return ssexpand(token);
}