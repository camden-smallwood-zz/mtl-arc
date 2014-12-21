//==============================================================
// MTL-Arc
//   A new implementation of the Arc language
// Copyright (C) 2014 Camden Smallwood
//==============================================================
// port.h
//   Port system - I/O to files, strings and sockets
//==============================================================

#ifndef MTL_ARC_PORT_H
#define MTL_ARC_PORT_H

#include "mtl-arc.h"

typedef struct atom atom_t;

typedef struct {
	enum {
		PORT_FILE,
		PORT_STRING,
		PORT_SOCKET
	} type;
	int input  : 1; // Does the port support input?
	int output : 1; // Does the port support output?
	union {
		FILE *file;
		char *string;
		void *socket; // TODO: socket type
	};
} port_t;

// Input functions
unsigned char port_readb(port_t *port);
int port_readc(port_t *port);
int port_peekc(port_t *port);
atom_t *port_sread(port_t *port);

// Output functions
void port_disp(port_t *port, atom_t *arg);
void port_write(port_t *port, atom_t *arg);
void port_writeb(port_t *port, const unsigned char arg);
void port_writec(port_t *port, const int arg);

typedef enum {
	FORMAT_BINARY,
	FORMAT_TEXT
} port_format_t;

atom_t *port_infile(const char *path, port_format_t fmt);
atom_t *port_outfile(const char *path, port_format_t fmt);

atom_t *port_instring(char *string);
atom_t *port_outstring();

#endif /* MTL_ARC_PORT_H */
