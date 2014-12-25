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

#include <fcntl.h>
#include <stdio.h>

typedef enum {
	PORT_STREAM,
	PORT_STRING,
	PORT_SOCKET
} port_type_t;

typedef enum {
	PORT_CLOSED,
	PORT_OPEN
} port_state_t;

typedef struct {
	port_type_t type;
	port_state_t state;
	int input  : 1;
	int output : 1;
	union {
		int fd;
		struct {
			unsigned long size, position;
			union {
				char *string;
				unsigned char *bytes;
			};
		};
	};
} port_t;

port_t *new_port(const port_type_t type);
port_t *stream_port(const int fd, const int input, const int output);
port_t *string_port(const char *string, const int input, const int output);
port_t *stdin_port();
port_t *stdout_port();
port_t *stderr_port();
port_t *infile_port(const char *path);
port_t *outfile_port(const char *path);
port_t *instring_port(const char *string);
port_t *outstring_port();

char *port_inside(port_t *port);
void port_seek(port_t *port, long offset, int whence);
int port_readc(port_t *port);
int port_readb(port_t *port);
void port_writec(port_t *port, const char c);
void port_writeb(port_t *port, const unsigned char b);

#endif /* MTL_ARC_PORT_H */