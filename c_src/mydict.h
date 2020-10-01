#ifndef _MYDICT_H
#define _MYDICT_H

typedef unsigned long long index_t;

void mydict_init(void);
void mydict_free(void);

index_t mydict_add(const char*);
void mydict_del(const char*);
index_t mydict_find(const char*);

#endif

