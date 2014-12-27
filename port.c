//==============================================================
// MTL-Arc
//   A new implementation of the Arc language
// Copyright (C) 2014 Camden Smallwood
//==============================================================
// port.c
//   Port system - I/O to files, strings and sockets
//==============================================================

#include <fcntl.h>
#include <stdio.h>
#include "port.h"

port_t *new_port(const port_type_t type) {
	port_t *result = malloc(sizeof(port_t));
	result->type = type;
	result->state = PORT_CLOSED;
	return result;
}

port_t *stream_port(const int fd) {
	port_t *result = new_port(PORT_STREAM);
	result->fd = fd;
	return result;
}

port_t *string_port(const char *string) {
	port_t *result = new_port(PORT_STRING);
	result->size = strlen(string) - 1;
	result->position = 0;
	result->string = strdup(string);
	return result;
}

port_t *stdin_port() {
	port_t *result = stream_port(fileno(stdin));
	result->state = PORT_INPUT;
	return result;
}

port_t *stdout_port() {
	port_t *result = stream_port(fileno(stdout));
	result->state = PORT_OUTPUT;
	return result;
}

port_t *stderr_port() {
	port_t *result = stream_port(fileno(stderr));
	result->state = PORT_OUTPUT;
	return result;
}

port_t *infile_port(const char *path) {
	port_t *result = stream_port(fileno(fopen(path, "r")));
	result->state = PORT_INPUT;
	return result;
}

port_t *outfile_port(const char *path) {
	port_t *result = stream_port(fileno(fopen(path, "w")));
	result->state = PORT_OUTPUT;
	return result;
}

port_t *instring_port(const char *string) {
	port_t *result = string_port(string);
	result->state = PORT_INPUT;
	return result;
}

port_t *outstring_port() {
	port_t *result = string_port("");
	result->state = PORT_OUTPUT;
	return result;
}

char *port_inside(port_t *port) {
	if (port->type != PORT_STRING)
		return NULL;
	return strdup(port->string);
}

void port_seek(port_t *port, long offset, int whence) {
	if (port->state == PORT_CLOSED)
		return;
	switch (port->type) {
		case PORT_STREAM:
			lseek(port->fd, offset, whence);
			break;
		case PORT_STRING:
			switch (whence) {
				case SEEK_SET:
					if (offset < 0)
						port->position = 0;
					else
						port->position = offset;
					break;
				case SEEK_CUR:
					port->position += offset;
					break;
				case SEEK_END:
					port->position = port->size - offset;
					break;
				default:
					break;
			}
			if (port->position > port->size)
				port->position = port->size;
			break;
	}
}

int port_readc(port_t *port) {
	char result;
	if (port->state != PORT_INPUT)
		return EOF;
	switch (port->type) {
		case PORT_STREAM:
			read(port->fd, &result, sizeof(result));
			break;
		case PORT_STRING:
			if (port->position >= port->size)
				return EOF;
			result = port->string[port->position++];
			break;
	}
	return result;
}

int port_readb(port_t *port) {
	unsigned char result;
	if (port->state != PORT_INPUT)
		return EOF;
	switch (port->type) {
		case PORT_STREAM:
			read(port->fd, &result, sizeof(result));
			break;
		case PORT_STRING:
			if (port->position >= port->size)
				return EOF;
			result = port->bytes[port->position++];
			break;
	}
	return result;
}

void port_writec(port_t *port, const char c) {
	if (port->state != PORT_OUTPUT)
		return;
	switch (port->type) {
		case PORT_STREAM:
			write(port->fd, &c, 1);
			break;
		case PORT_STRING:
			if (port->position >= port->size)
				realloc(port->string, port->position + 1);
			port->string[port->position++] = c;
			break;
	}
}

void port_writeb(port_t *port, const unsigned char b) {
	if (port->state != PORT_OUTPUT)
		return;
	switch (port->type) {
		case PORT_STREAM:
			write(port->fd, &b, 1);
			break;
		case PORT_STRING:
			if (port->position >= port->size)
				realloc(port->string, port->position + 1);
			port->bytes[port->position++] = b;
			break;
	}
}