
# c-accessor
A zig library and CLI program to generate getter functions for C structs.

For instance, given the following source:
```c
// file: a-struct.c
struct AAA {
    int x;
    int y;
};
```
... the getters will be generated:

```c
// file: a-struct-getters.c
extern inline int AAA_get_x(const struct AAA *self) { return self->x; }
extern inline int AAA_get_y(const struct AAA *self) { return self->y; }

// file: a-struct-getters.h
int AAA_get_x(const struct AAA *self);
int AAA_get_y(const struct AAA *self);
```


## Purpose
Currently, [zig does not support C bitfields](https://github.com/ziglang/zig/issues/1499), 
which results to opaque structs with inaccessible fields. The struct getters functions
serves as a workaround to access the opaque struct fields.

The idea is to let zig compile and link the object code in `a-struct-getters.c` and then
translate the function prototypes in `a-struct-getters.h`. See [my bfd wrapper](https://github.com/nvlled/bfd-zig/) as a practical example.

More importantly, I did this project as a means to further learn zig. There are 
probably better solutions/workarounds for the unsupported C bitfields.