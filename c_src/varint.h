#ifndef _VARINT_H_
#define _VARINT_H_

typedef unsigned long long uint64;

void varint_encode(uint64, unsigned char*, int);
uint64 varint_decode(const unsigned char*, int);
int varint_encoding_length(uint64);

#endif
