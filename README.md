
# genc
A zig library and CLI program that generates C code for:

- struct field getters
- function prototypes

For instance, given the following source:
```c
// file: a-struct.c
struct AAA {
    int x;
    int y;
};

void some_func(int x) {
    return x+1;
}

void another func(int x, int y) {
    return x+y;
}
```
... the following can be generated:

```c
// getters with function body
extern inline int AAA_get_x(const struct AAA *self) { return self->x; }
extern inline int AAA_get_y(const struct AAA *self) { return self->y; }

// getters prototypes
int AAA_get_x(const struct AAA *self);
int AAA_get_y(const struct AAA *self);

// function prototypes
void some_func(int x);
void another func(int x, int y);

```


## Purpose
Currently, [zig does not support C bitfields](https://github.com/ziglang/zig/issues/1499), 
which results to opaque structs with inaccessible fields. The struct getters functions
serves as a workaround to access the opaque struct fields.

The idea is to let zig compile and link the object code in `a-struct-getters.c` and then
translate the function prototypes in `a-struct-getters.h`. See [my bfd wrapper](https://github.com/nvlled/bfd-zig/) as a practical example.

More importantly, I did this project as a means to further learn zig. There are 
probably better solutions/workarounds for the unsupported C bitfields.
