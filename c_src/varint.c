#include "varint.h"
#define NULL 0

static const char MSB = 0x80;
static const char MSBALL = ~0x7F;

static const uint64 N1 = 128; // 2 ^ 7
static const uint64 N2 = 16384;
static const uint64 N3 = 2097152;
static const uint64 N4 = 268435456;
static const uint64 N5 = 34359738368;
static const uint64 N6 = 4398046511104;
static const uint64 N7 = 562949953421312;
static const uint64 N8 = 72057594037927936;
static const uint64 N9 = 9223372036854775808U;

int varint_encoding_length(uint64 n) {
  return (
      n < N1 ? 1
    : n < N2 ? 2
    : n < N3 ? 3
    : n < N4 ? 4
    : n < N5 ? 5
    : n < N6 ? 6
    : n < N7 ? 7
    : n < N8 ? 8
    : n < N9 ? 9
    :         10
  );
}

void varint_encode(uint64 n, unsigned char* ptr, int len) {
  while (n & MSBALL) {
    *(ptr++) = (n & 0xFF) | MSB;
    n = n >> 7;
  }
  *ptr = n;
}

uint64 varint_decode(const unsigned char* ptr, int len) {
  uint64 result = 0;
  int bits = 0;
  uint64 cur;
  while (*ptr & MSB) {
    cur = *ptr;
    result += ((cur & 0x7F) << bits);
    bits += 7;
    ptr++;
  }
  cur = *ptr;
  result += ((cur & 0x7F) << bits);
  return result;
}
