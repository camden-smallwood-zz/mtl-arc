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