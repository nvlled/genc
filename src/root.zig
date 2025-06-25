const std = @import("std");
const aro = @import("aro");
const Node = aro.Tree.Node;
const stderr = std.io.getStdErr().writer();

const String = []const u8;

const field_names_buf_size = 128;

pub const StructItem = struct {
    name: String,
    has_bitfields: bool = false,
};

pub const FuncItem = struct {
    name: String,
    @"inline": bool,
    static: bool,
};

pub const StructFilter = struct {
    context: *anyopaque = undefined,
    predicate: *const fn (item: StructItem, context: *anyopaque) bool,
};

pub const FuncFilter = struct {
    context: *anyopaque = undefined,
    predicate: *const fn (item: FuncItem, context: *anyopaque) bool,
};

pub const AccessorOptions = struct {
    filter: ?StructFilter = null,
    generate_body: bool = true,
    include_path: []const []const u8 = &.{},
    prepend_str: []const u8 = &.{},
    append_str: []const u8 = &.{},
};

pub const ProtoOptions = struct {
    filter: ?FuncFilter = null,
    include_path: []const []const u8 = &.{},
    prepend_str: []const u8 = &.{},
    append_str: []const u8 = &.{},
};

fn generateStructAccessors(tree: *const aro.Tree, node_index: Node.Index, w: anytype, generate_body: bool) !void {
    const struct_node = node_index.get(tree);
    const decl: Node.ContainerDecl = val: switch (struct_node) {
        else => return,
        .struct_decl => |decl| break :val decl,
    };

    const struct_name = tree.tokSlice(decl.name_or_kind_tok);

    var buffer: [1024]u8 = undefined;
    var type_name_buf = std.io.StreamSource{ .buffer = std.io.fixedBufferStream(&buffer) };

    var prev_field: ?Node = null;
    loop: for (decl.fields) |field| {
        const sub_node = field.get(tree);
        const record: Node.RecordField = val: switch (sub_node) {
            else => continue :loop,
            .union_decl, .struct_decl, .enum_decl => {
                prev_field = sub_node;
                continue :loop;
            },
            .record_field => |r| break :val r,
        };

        const ident = tree.tokSlice(record.name_or_first_tok);
        var is_pointer = false;

        switch (record.qt.type(tree.comp)) {
            .@"union", .@"enum", .@"struct" => {
                var field_names = try std.BoundedArray(String, field_names_buf_size).init(0);

                if (!std.mem.eql(u8, ident, "struct")) {
                    try field_names.append(ident);
                }

                if (prev_field) |f| {
                    try generateSubAccessors(tree, f, w, .{
                        .struct_name = struct_name,
                        .field_names = &field_names,
                        .type_name_buf = &type_name_buf,
                        .generate_body = generate_body,
                    });
                    prev_field = null;
                    continue :loop;
                }
            },
            .pointer => |p| {
                is_pointer = true;
                switch (p.child.type(tree.comp)) {
                    .@"struct", .@"union" => |s| {
                        // when the field is a pointer to an anonymous struct,
                        // skip it, since the type shows "(anonymous TAG at path:line:col)",
                        // which is not a valid C type.
                        if (s.isAnonymous(tree.comp)) continue :loop;
                    },
                    else => {},
                }
            },
            else => {},
        }

        try type_name_buf.seekTo(0);
        try record.qt.print(tree.comp, type_name_buf.writer());
        const type_str = type_name_buf.buffer.getWritten();
        const return_type_space = if (is_pointer) "" else " ";

        if (generate_body) try w.writeAll("extern inline ");

        try w.print("{s}{s}{s}_get_{s}(const struct {s} *self)", .{ type_str, return_type_space, struct_name, ident, struct_name });
        if (generate_body) {
            try w.print(" {{ return self->{s}; }}\n", .{ident});
        } else {
            try w.writeAll(";\n");
        }
        try w.writeAll("\n");
    }
}

fn generateSubAccessors(tree: *const aro.Tree, node: Node, w: anytype, args: struct {
    struct_name: []const u8,
    field_names: *std.BoundedArray(String, field_names_buf_size),
    type_name_buf: *std.io.StreamSource,
    generate_body: bool,
}) !void {
    const struct_name = args.struct_name;
    const field_names = args.field_names;
    const type_name_buf = args.type_name_buf;
    const generate_body = args.generate_body;

    const container: Node.ContainerDecl = blk: switch (node) {
        .struct_decl, .enum_decl, .union_decl => |f| {
            break :blk f;
        },
        else => return,
    };

    var prev_field: ?Node = null;
    loop: for (container.fields) |field| {
        const sub_node = field.get(tree);
        const record: Node.RecordField = val: switch (sub_node) {
            else => continue :loop,
            .union_decl, .struct_decl, .enum_decl => {
                prev_field = sub_node;
                continue :loop;
            },
            .record_field => |r| break :val r,
        };

        const ident = tree.tokSlice(record.name_or_first_tok);
        var is_pointer = false;

        switch (record.qt.type(tree.comp)) {
            .@"union", .@"enum", .@"struct" => {
                if (prev_field) |f| {
                    const n = field_names.len;
                    // error can't happen since n is guaranteed <= max_capacity
                    defer field_names.resize(n) catch {};
                    try field_names.append(ident);

                    try generateSubAccessors(tree, f, w, .{
                        .struct_name = struct_name,
                        .field_names = field_names,
                        .type_name_buf = type_name_buf,
                        .generate_body = generate_body,
                    });
                    prev_field = null;
                    continue :loop;
                }
            },
            .pointer => |p| {
                is_pointer = true;
                switch (p.child.type(tree.comp)) {
                    .@"struct", .@"union" => |s| {
                        // when the field is a pointer to an anonymous struct,
                        // skip it, since the type shows "(anonymous TAG at path:line:col)",
                        // which is not a valid C type.
                        if (s.isAnonymous(tree.comp)) continue :loop;
                    },
                    else => {},
                }
            },
            else => {},
        }

        try type_name_buf.seekTo(0);
        try record.qt.print(tree.comp, type_name_buf.writer());

        const type_str = type_name_buf.buffer.getWritten();

        if (generate_body) try w.writeAll("extern inline ");

        try w.writeAll(type_str);
        if (!is_pointer) try w.writeAll(" ");
        try w.writeAll(struct_name);
        try w.writeAll("_get");
        for (field_names.slice()) |f| {
            try w.writeAll("_");
            try w.writeAll(f);
        }
        try w.writeAll("_");
        try w.writeAll(ident);
        try w.writeAll("(const struct ");
        try w.writeAll(struct_name);
        try w.writeAll(" *self)");

        if (generate_body) {
            try w.writeAll(" { return self->");
            for (field_names.slice()) |f| {
                try w.writeAll(f);
                try w.writeAll(".");
            }
            try w.writeAll(ident);
            try w.writeAll("; }");
        } else {
            try w.writeAll(";");
        }
        try w.writeAll("\n\n");
    }
}

pub fn generateAccessors(allocator: std.mem.Allocator, r: anytype, w: anytype, options: AccessorOptions) !void {
    var diagnostics: aro.Diagnostics = .{ .output = .ignore };
    var comp = aro.Compilation.init(allocator, &diagnostics, std.fs.cwd());
    defer comp.deinit();

    for (options.include_path) |path| {
        try comp.include_dirs.append(allocator, path);
    }

    const file = try comp.addSourceFromReader(r, "file.c", .user);
    const builtin_macros = try comp.generateBuiltinMacros(.no_system_defines);

    var pp = aro.Preprocessor.init(&comp, .default);
    defer pp.deinit();
    try pp.addBuiltinMacros();

    _ = try pp.preprocess(builtin_macros);

    const eof = try pp.preprocess(file);
    try pp.addToken(eof);

    var tree = try aro.Parser.parse(&pp);
    defer tree.deinit();

    try w.writeAll(options.prepend_str);

    loop: for (tree.root_decls.items) |node| {
        const decl = switch (node.get(&tree)) {
            else => continue :loop,
            .struct_decl => |e| e,
        };

        if (options.filter) |filter| {
            const struct_name = tree.tokSlice(decl.name_or_kind_tok);
            const include = filter.predicate(.{
                .has_bitfields = has_bitfield(&tree, decl),
                .name = struct_name,
            }, filter.context);

            if (!include) continue :loop;
        }

        try generateStructAccessors(&tree, node, w, options.generate_body);
    }

    try w.writeAll(options.append_str);
}

pub fn generateProto(allocator: std.mem.Allocator, r: anytype, w: anytype, options: ProtoOptions) !void {
    var diagnostics: aro.Diagnostics = .{ .output = .ignore };
    var comp = aro.Compilation.init(allocator, &diagnostics, std.fs.cwd());
    defer comp.deinit();

    comp.langopts.preserve_comments = true;
    for (options.include_path) |path| {
        try comp.include_dirs.append(allocator, path);
    }

    const file = try comp.addSourceFromReader(r, "file.c", .user);
    const builtin_macros = try comp.generateBuiltinMacros(.no_system_defines);
    var pp = aro.Preprocessor.init(&comp, .default);

    defer pp.deinit();
    try pp.addBuiltinMacros();
    _ = try pp.preprocess(builtin_macros);

    const eof = try pp.preprocess(file);
    try pp.addToken(eof);

    var tree = try aro.Parser.parse(&pp);
    defer tree.deinit();

    const ids = tree.tokens.items(.id);

    try w.writeAll(options.prepend_str);

    loop: for (tree.root_decls.items) |node_index| {
        const f = switch (node_index.get(&tree)) {
            else => continue :loop,
            .function => |f| f,
        };

        if (options.filter) |filter| {
            const name = tree.tokSlice(f.name_tok);
            const include = filter.predicate(.{
                .name = name,
                .@"inline" = f.@"inline",
                .static = f.static,
            }, filter.context);

            if (!include) continue :loop;
        }

        // search and print comments at the top of a function
        const fn_index = node_index.tok(&tree);
        if (fn_index >= 2) {
            // fn_index points to function name,
            // fn_index-1 to the return token,
            // so start looking at -2 offset
            const end_index = fn_index - 1;
            var idx: i32 = @intCast(end_index - 1); // cast to i32 to prevent overflow
            while (idx >= 0) : (idx -= 1) {
                switch (ids[@intCast(idx)]) {
                    .comment, .unterminated_comment => {},
                    else => break,
                }
            }

            const start_index: usize = @intCast(idx + 1);
            for (start_index..end_index) |i| {
                const content = tree.tokSlice(@intCast(i));
                try w.writeAll(content);
                try w.writeAll("\n");
            }
        }

        switch (f.qt.type(tree.comp)) {
            else => {},
            .func => |f_type| {
                try f_type.return_type.print(tree.comp, w);
                try w.writeAll(" ");
                try w.writeAll(tree.tokSlice(f.name_tok));
                try w.writeAll("(");
                for (f_type.params, 0..) |p, idx| {
                    if (idx > 0) try w.writeAll(", ");
                    const name = tree.tokSlice(p.name_tok);
                    try p.qt.printNamed(name, tree.comp, w);
                }
                try w.writeAll(");\n\n");
            },
        }
    }

    try w.writeAll(options.append_str);
}

// Caller should free returned string
pub fn generateAccessorsString(allocator: std.mem.Allocator, code: String, options: AccessorOptions) !String {
    const len: f32 = @floatFromInt(code.len);
    var buffer = try allocator.alloc(u8, @intFromFloat(@ceil(len / 1024) * 1024 * 2));
    defer allocator.free(buffer);

    var w = std.io.StreamSource{ .buffer = std.io.fixedBufferStream(buffer[0..]) };
    var r = std.io.StreamSource{ .const_buffer = std.io.fixedBufferStream(code) };
    try generateAccessors(allocator, r.reader(), w.writer(), options);

    return allocator.dupe(u8, w.buffer.getWritten());
}

// Caller should free returned string
pub fn generateProtoString(allocator: std.mem.Allocator, code: String, options: ProtoOptions) !String {
    const len: f32 = @floatFromInt(code.len);
    var buffer = try allocator.alloc(u8, @intFromFloat(@ceil(len / 1024) * 1024 * 10));
    defer allocator.free(buffer);

    var w = std.io.StreamSource{ .buffer = std.io.fixedBufferStream(buffer[0..]) };
    var r = std.io.StreamSource{ .const_buffer = std.io.fixedBufferStream(code) };
    try generateProto(allocator, r.reader(), w.writer(), options);

    return allocator.dupe(u8, w.buffer.getWritten());
}

fn has_bitfield(tree: *aro.Tree, decl: Node.ContainerDecl) bool {
    for (decl.fields) |field| {
        switch (field.get(tree)) {
            .record_field => |r| {
                if (r.bit_width != null) return true;
            },
            else => {},
        }
    }
    return false;
}

test "generate getters" {
    const code: []const u8 =
        \\int x;
        \\struct Foo {
        \\  int a;
        \\  char *b;
        \\};
    ;
    const expected =
        \\extern inline int Foo_get_a(const struct Foo *self) { return self->a; }
        \\
        \\extern inline char *Foo_get_b(const struct Foo *self) { return self->b; }
    ;

    const output = try generateAccessorsString(std.testing.allocator, code, .{});
    defer std.testing.allocator.free(output);
    try std.testing.expectEqualStrings(
        std.mem.trim(u8, output, "\n "),
        std.mem.trim(u8, expected, "\n "),
    );
}

test "generate getter prototype" {
    const code: []const u8 =
        \\int x;
        \\struct Foo {
        \\  int a;
        \\  char *b;
        \\};
    ;
    const expected =
        \\int Foo_get_a(const struct Foo *self);
        \\
        \\char *Foo_get_b(const struct Foo *self);
    ;

    const output = try generateAccessorsString(std.testing.allocator, code, .{ .generate_body = false });
    defer std.testing.allocator.free(output);
    try std.testing.expectEqualStrings(
        std.mem.trim(u8, output, "\n "),
        std.mem.trim(u8, expected, "\n "),
    );
}

test "struct fields" {
    const code: []const u8 =
        \\struct Foo {
        \\  int x;
        \\  struct {
        \\    int a;
        \\    char *b;
        \\  } data;
        \\};
    ;
    const expected =
        \\extern inline int Foo_get_x(const struct Foo *self) { return self->x; }
        \\
        \\extern inline int Foo_get_data_a(const struct Foo *self) { return self->data.a; }
        \\
        \\extern inline char *Foo_get_data_b(const struct Foo *self) { return self->data.b; }
    ;

    const output = try generateAccessorsString(std.testing.allocator, code, .{});
    defer std.testing.allocator.free(output);

    try std.testing.expectEqualStrings(
        std.mem.trim(u8, output, "\n "),
        std.mem.trim(u8, expected, "\n "),
    );
}

test "anonymous structs" {
    const code: []const u8 =
        \\struct Foo {
        \\  int x;
        \\  struct {
        \\    int a;
        \\    char *b;
        \\  };
        \\};
    ;
    const expected =
        \\extern inline int Foo_get_x(const struct Foo *self) { return self->x; }
        \\
        \\extern inline int Foo_get_a(const struct Foo *self) { return self->a; }
        \\
        \\extern inline char *Foo_get_b(const struct Foo *self) { return self->b; }
    ;

    const output = try generateAccessorsString(std.testing.allocator, code, .{});
    defer std.testing.allocator.free(output);

    try std.testing.expectEqualStrings(
        std.mem.trim(u8, output, "\n "),
        std.mem.trim(u8, expected, "\n "),
    );
}

test "unions" {
    const code: []const u8 =
        \\struct Foo {
        \\  int x;
        \\
        \\  union {
        \\    int a;
        \\    char *b;
        \\  } data;
        \\};
    ;
    const expected =
        \\extern inline int Foo_get_x(const struct Foo *self) { return self->x; }
        \\
        \\extern inline int Foo_get_data_a(const struct Foo *self) { return self->data.a; }
        \\
        \\extern inline char *Foo_get_data_b(const struct Foo *self) { return self->data.b; }
    ;

    const output = try generateAccessorsString(std.testing.allocator, code, .{});
    defer std.testing.allocator.free(output);

    try std.testing.expectEqualStrings(
        std.mem.trim(u8, output, "\n "),
        std.mem.trim(u8, expected, "\n "),
    );
}

test "anonymous unions" {
    const code: []const u8 =
        \\struct Foo {
        \\  int x;
        \\
        \\  union {
        \\    int a;
        \\    char *b;
        \\  };
        \\};
    ;
    const expected =
        \\extern inline int Foo_get_x(const struct Foo *self) { return self->x; }
        \\
        \\extern inline int Foo_get_a(const struct Foo *self) { return self->a; }
        \\
        \\extern inline char *Foo_get_b(const struct Foo *self) { return self->b; }
    ;

    const output = try generateAccessorsString(std.testing.allocator, code, .{});
    defer std.testing.allocator.free(output);

    try std.testing.expectEqualStrings(
        std.mem.trim(u8, output, "\n "),
        std.mem.trim(u8, expected, "\n "),
    );
}

test "skip pointers to anonymous structs" {
    const code: []const u8 =
        \\struct Foo {
        \\  int x;
        \\  int y;
        \\
        \\  struct {
        \\    int a;
        \\    char *b;
        \\  } *g;
        \\};
    ;
    const expected =
        \\extern inline int Foo_get_x(const struct Foo *self) { return self->x; }
        \\
        \\extern inline int Foo_get_y(const struct Foo *self) { return self->y; }
    ;

    const output = try generateAccessorsString(std.testing.allocator, code, .{});
    defer std.testing.allocator.free(output);

    try std.testing.expectEqualStrings(
        std.mem.trim(u8, output, "\n "),
        std.mem.trim(u8, expected, "\n "),
    );
}

test "skip pointers to anonymous unions" {
    const code: []const u8 =
        \\struct Foo {
        \\  int x;
        \\  int y;
        \\
        \\  union {
        \\    int a;
        \\    char *b;
        \\  } *g;
        \\};
    ;
    const expected =
        \\extern inline int Foo_get_x(const struct Foo *self) { return self->x; }
        \\
        \\extern inline int Foo_get_y(const struct Foo *self) { return self->y; }
    ;

    const output = try generateAccessorsString(std.testing.allocator, code, .{});
    defer std.testing.allocator.free(output);

    try std.testing.expectEqualStrings(
        std.mem.trim(u8, output, "\n "),
        std.mem.trim(u8, expected, "\n "),
    );
}

test "filter by struct name" {
    const code: []const u8 =
        \\struct AAA { int x; };
        \\struct BBB { int x; };
        \\struct CCC { int x; };
    ;
    const expected =
        \\int AAA_get_x(const struct AAA *self);
    ;

    const output = try generateAccessorsString(std.testing.allocator, code, .{
        .generate_body = false,
        .filter = .{
            .predicate = struct {
                fn apply(item: StructItem, _: *anyopaque) bool {
                    return std.mem.eql(u8, item.name, "AAA");
                }
            }.apply,
        },
    });

    defer std.testing.allocator.free(output);
    try std.testing.expectEqualStrings(
        std.mem.trim(u8, output, "\n "),
        std.mem.trim(u8, expected, "\n "),
    );
}

test "filter by struct with bitfields" {
    const code: []const u8 =
        \\struct AAA { int x: 3; int y;  };
        \\struct BBB { int x; };
        \\struct CCC { int x: 2; };
        \\struct DDD { char a; char b; };
    ;
    const expected =
        \\int AAA_get_x(const struct AAA *self);
        \\
        \\int AAA_get_y(const struct AAA *self);
        \\
        \\int CCC_get_x(const struct CCC *self);
    ;

    const output = try generateAccessorsString(std.testing.allocator, code, .{
        .generate_body = false,
        .filter = .{
            .predicate = struct {
                fn apply(item: StructItem, _: *anyopaque) bool {
                    return item.has_bitfields;
                }
            }.apply,
        },
    });

    defer std.testing.allocator.free(output);
    try std.testing.expectEqualStrings(
        std.mem.trim(u8, expected, "\n "),
        std.mem.trim(u8, output, "\n "),
    );
}

test "generateProto" {
    const code: []const u8 =
        \\// this is foo function
        \\// it adds two number
        \\int foo(int x, int y) { return x+y; }
        \\/** bar does something
        \\    but returns nothing
        \\*/
        \\void bar(char z) { }
        \\void baz() { }
        \\/*end of file*/
    ;
    const expected =
        \\// this is foo function
        \\// it adds two number
        \\int foo(int x, int y);
        \\
        \\/** bar does something
        \\    but returns nothing
        \\*/
        \\void bar(char z);
        \\
        \\void baz();
    ;

    const output = try generateProtoString(std.testing.allocator, code, .{});
    defer std.testing.allocator.free(output);

    try std.testing.expectEqualStrings(
        std.mem.trim(u8, output, "\n "),
        std.mem.trim(u8, expected, "\n "),
    );
}

test "generateProto with filter" {
    const code: []const u8 =
        \\int f1(int x, int y) { return x+y; }
        \\int f2(int x, int y) { return x+y; }
        \\int f3(int x, int y) { return x+y; }
        \\int f4(int x, int y) { return x+y; }
    ;
    const expected =
        \\int f1(int x, int y);
        \\
        \\int f3(int x, int y);
    ;

    const output = try generateProtoString(std.testing.allocator, code, .{
        .filter = .{
            .predicate = struct {
                fn _(item: FuncItem, _: *anyopaque) bool {
                    return std.mem.eql(u8, item.name, "f1") or std.mem.eql(u8, item.name, "f3");
                }
            }._,
        },
    });
    defer std.testing.allocator.free(output);

    try std.testing.expectEqualStrings(
        std.mem.trim(u8, output, "\n "),
        std.mem.trim(u8, expected, "\n "),
    );
}

test "include path 1" {
    const code: []const u8 =
        \\#include "funcs.h"
    ;

    const expected =
        \\int f1(int x, int y);
        \\
        \\int f2(int x, int y);
        \\
        \\int f3(int x, int y);
        \\
        \\int f4(int x, int y);
    ;

    const output = try generateProtoString(std.testing.allocator, code, .{
        .include_path = &[_][]const u8{"test"},
    });
    defer std.testing.allocator.free(output);

    try std.testing.expectEqualStrings(
        std.mem.trim(u8, output, "\n "),
        std.mem.trim(u8, expected, "\n "),
    );
}

test "include path 2" {
    const code: []const u8 =
        \\#include "struct.h"
    ;

    const expected =
        \\int Foo_get_x(const struct Foo *self);
        \\
        \\int Foo_get_y(const struct Foo *self);
        \\
        \\int Foo_get_z(const struct Foo *self);
    ;

    const output = try generateAccessorsString(std.testing.allocator, code, .{
        .generate_body = false,
        .include_path = &[_][]const u8{"test"},
    });

    defer std.testing.allocator.free(output);

    try std.testing.expectEqualStrings(
        std.mem.trim(u8, output, "\n "),
        std.mem.trim(u8, expected, "\n "),
    );
}

test "prepend and append string" {
    const code: []const u8 =
        \\void foo(int x) { return x+1; }
    ;

    const expected =
        \\//generated, do not edit!
        \\
        \\void foo(int x);
        \\
        \\//EOF
    ;

    const output = try generateProtoString(std.testing.allocator, code, .{
        .include_path = &[_][]const u8{"test"},
        .prepend_str = "//generated, do not edit!\n\n",
        .append_str = "//EOF\n",
    });

    defer std.testing.allocator.free(output);

    try std.testing.expectEqualStrings(
        std.mem.trim(u8, output, "\n "),
        std.mem.trim(u8, expected, "\n "),
    );
}
