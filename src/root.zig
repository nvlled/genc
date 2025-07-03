const std = @import("std");
const aro = @import("aro");
const intf = @import("intf");
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

const OutputOption = @FieldType(aro.Diagnostics, "output");

pub const AccessorOptions = struct {
    filter: ?StructFilter = null,
    generate_body: bool = true,
    include_path: []const []const u8 = &.{},
    prepend_str: []const u8 = &.{},
    append_str: []const u8 = &.{},
    output: OutputOption = .{ .to_file = .{
        .file = std.io.getStdErr(),
        .config = .escape_codes,
    } },
};

pub const ProtoOptions = struct {
    filter: ?FuncFilter = null,
    include_path: []const []const u8 = &.{},
    prepend_str: []const u8 = &.{},
    append_str: []const u8 = &.{},
    output: OutputOption = .{ .to_file = .{
        .file = std.io.getStdErr(),
        .config = .escape_codes,
    } },
};

fn generateStructAccessors(
    comptime WT: type,
    tree: *const aro.Tree,
    node_index: Node.Index,
    w: intf.io.Writer(WT),
    generate_body: bool,
) !void {
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
                    try generateSubAccessors(WT, tree, f, w, .{
                        struct_name,
                        &field_names,
                        &type_name_buf,
                        generate_body,
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

fn generateSubAccessors(
    comptime WT: type,
    tree: *const aro.Tree,
    node: Node,
    w: intf.io.Writer(WT),
    args: struct {
        []const u8,
        *std.BoundedArray(String, field_names_buf_size),
        *std.io.StreamSource,
        bool,
    },
) !void {
    const struct_name, const field_names, const type_name_buf, const generate_body = args;

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
                    defer field_names.resize(n) catch unreachable;
                    try field_names.append(ident);

                    try generateSubAccessors(WT, tree, f, w, .{
                        struct_name,
                        field_names,
                        type_name_buf,
                        generate_body,
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

pub fn generateAccessors(
    comptime RT: type,
    comptime WT: type,
    allocator: std.mem.Allocator,
    r: intf.io.Reader(RT),
    w: intf.io.Writer(WT),
    options: AccessorOptions,
) !void {
    var diagnostics: aro.Diagnostics = .{ .output = options.output };
    var comp = aro.Compilation.init(allocator, &diagnostics, std.fs.cwd());
    defer comp.deinit();

    for (options.include_path) |path| {
        try comp.addSystemIncludeDir(path);
    }

    var rr = RemoveIncludes.Wrap(RT).init(r);
    const file = try comp.addSourceFromReader(rr.reader(), "file.c", .user);
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

        try generateStructAccessors(WT, &tree, node, w, options.generate_body);
    }

    try w.writeAll(options.append_str);
}

pub fn generateProto(
    comptime RT: type,
    comptime WT: type,
    allocator: std.mem.Allocator,
    r: intf.io.Reader(RT),
    w: intf.io.Writer(WT),
    options: ProtoOptions,
) !void {
    var diagnostics: aro.Diagnostics = .{ .output = options.output };
    var comp = aro.Compilation.init(allocator, &diagnostics, std.fs.cwd());
    defer comp.deinit();

    // TODO: preserve function comments
    // Currently preserve_comments only works for the preprocessing step:
    // https://github.com/Vexu/arocc/issues/871
    //comp.langopts.preserve_comments = true;

    for (options.include_path) |path| {
        try comp.addSystemIncludeDir(path);
    }

    var rr = RemoveIncludes.Wrap(RT).init(r);
    const file = try comp.addSourceFromReader(rr.reader(), "file.c", .user);
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
    var w = std.ArrayList(u8).init(allocator);
    var r = std.io.fixedBufferStream(code);

    defer w.deinit();

    try generateAccessors(@TypeOf(r), @TypeOf(w), allocator, r.reader(), w.writer(), options);
    return try w.toOwnedSlice();
}

// Caller should free returned string
pub fn generateProtoString(allocator: std.mem.Allocator, code: String, options: ProtoOptions) !String {
    const len: f32 = @floatFromInt(code.len);
    var buffer = try allocator.alloc(u8, @intFromFloat(@ceil(len / 1024) * 1024 * 10));
    defer allocator.free(buffer);

    var w = std.io.StreamSource{ .buffer = std.io.fixedBufferStream(buffer[0..]) };
    var r = std.io.StreamSource{ .const_buffer = std.io.fixedBufferStream(code) };
    try generateProto(@TypeOf(r), @TypeOf(w), allocator, r.reader(), w.writer(), options);

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
        \\int foo(int x, int y) { return x+y; }
        \\void bar(char z) { }
        \\void baz() { }
    ;
    const expected =
        \\int foo(int x, int y);
        \\
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

// TODO:
//test "generateProto with comments" {
//    const code: []const u8 =
//        \\// this is foo function
//        \\// it adds two number
//        \\int foo(int x, int y) { return x+y; }
//        \\/** bar does something
//        \\    but returns nothing
//        \\*/
//        \\void bar(char z) { }
//        \\void baz() { }
//        \\/*end of file*/
//    ;
//    const expected =
//        \\// this is foo function
//        \\// it adds two number
//        \\int foo(int x, int y);
//        \\
//        \\/** bar does something
//        \\    but returns nothing
//        \\*/
//        \\void bar(char z);
//        \\
//        \\void baz();
//    ;
//
//    const output = try generateProtoString(std.testing.allocator, code, .{});
//    defer std.testing.allocator.free(output);
//
//    try std.testing.expectEqualStrings(
//        std.mem.trim(u8, output, "\n "),
//        std.mem.trim(u8, expected, "\n "),
//    );
//}

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
        .include_path = &.{"test"},
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
        \\#include <stdio.h>
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
        .include_path = &.{"test"},
    });

    defer std.testing.allocator.free(output);

    try std.testing.expectEqualStrings(
        std.mem.trim(u8, output, "\n "),
        std.mem.trim(u8, expected, "\n "),
    );
}

/// A set of functions that replaces #includes<...>
/// with an empty line from a string or reader.
/// This is because of I'm getting weird
/// errors related to headers not being found by aroccc,
/// despite setting up my path correctly and zig
/// finds the headers just fine. Probably a bug,
/// or I misconfigured something. In any case,
/// system headers aren't really needed since
/// only the file itself needs to be parsed.
/// So as a workaround, just filter those lines out.
const RemoveIncludes = struct {
    pub fn Wrap(comptime T: type) type {
        return struct {
            const Self = @This();

            r: intf.io.Reader(T),
            line_buf: [4096]u8 = undefined,
            leftover: ?[]u8 = null,

            pub fn read(self: *Self, buf: []u8) anyerror!usize {
                if (self.leftover) |line| {
                    if (line.len >= buf.len) { // -1 for the newline
                        @memcpy(buf, line[0..buf.len]);
                        self.leftover = line[buf.len..];
                        return buf.len;
                    } else {
                        @memcpy(buf[0..line.len], line);
                        buf[line.len] = '\n';
                        self.leftover = null;
                        return line.len + 1; // +1 for the newline
                    }
                }

                while (true) {
                    // TODO: fix added extra line at EOF
                    const line = try self.r.readUntilDelimiterOrEof(&self.line_buf, '\n') orelse {
                        break;
                    };

                    if (isSystemInclude(line)) {
                        buf[0] = '\n';
                        return 1;
                    } else if (line.len >= buf.len) {
                        @memcpy(buf, line[0..buf.len]);
                        self.leftover = line[buf.len..];
                        return buf.len;
                    } else {
                        @memcpy(buf[0..line.len], line);
                        buf[line.len] = '\n';
                        return line.len + 1; // +1 for the newline
                    }
                }
                return 0;
            }

            pub fn reader(self: *Self) intf.io.Reader(Self) {
                return .{
                    .context = self,
                };
            }

            pub fn init(r: intf.io.Reader(T)) Self {
                return .{ .r = r };
            }
        };
    }

    pub fn pipe(
        comptime RT: type,
        comptime WT: type,
        r: intf.io.Reader(RT),
        w: intf.io.Writer(WT),
    ) !void {
        var buf: [4096]u8 = undefined;

        while (true) {
            if (try r.readUntilDelimiterOrEof(&buf, '\n')) |line| {
                if (!isSystemInclude(line)) {
                    _ = try w.writeAll(line);
                    _ = try w.writeByte('\n');
                }
            } else {
                break;
            }
        }
    }

    // Caller should free returned string
    pub fn byString(allocator: std.mem.Allocator, c_code: []const u8) ![]const u8 {
        var buf = std.ArrayList(u8).init(allocator);
        var iter = std.mem.tokenizeScalar(u8, c_code, '\n');
        while (iter.next()) |line| {
            if (isSystemInclude(line)) continue;
            try buf.appendSlice(line);
            try buf.append('\n');
        }
        return try buf.toOwnedSlice();
    }

    fn isSystemInclude(s: []const u8) bool {
        if (!std.mem.startsWith(u8, s, "#include")) return false;
        const line = std.mem.trimStart(u8, s["#include".len..], " ");
        return line.len > 0 and line[0] == '<';
    }
};

test "prepend and append string" {
    const code: []const u8 =
        \\int foo(int x) { return x+1; }
    ;

    const expected =
        \\//generated, do not edit!
        \\
        \\int foo(int x);
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

test "remove includes by string" {
    const code =
        \\#include <stdio.h>
        \\#define X 100
        \\void foo() { }
        \\#include <bfd.h>
        \\void bar() { }
        \\// this one is fine
    ;
    const expected =
        \\#define X 100
        \\void foo() { }
        \\void bar() { }
        \\// this one is fine
    ;

    const output = try RemoveIncludes.byString(std.testing.allocator, code);
    defer std.testing.allocator.free(output);
    try std.testing.expectEqualStrings(
        std.mem.trim(u8, output, "\n "),
        std.mem.trim(u8, expected, "\n "),
    );
}

test "remove includes by wrapping" {
    const code =
        \\#include <stdio.h>
        \\#define X 100
        \\void foo() { }
        \\#include <bfd.h>
        \\void bar() { }
        \\// some comment
    ;

    var r = std.io.StreamSource{ .const_buffer = std.io.fixedBufferStream(code) };

    const expected =
        \\
        \\#define X 100
        \\void foo() { }
        \\
        \\void bar() { }
        \\// some comment
    ;

    var rr = RemoveIncludes.Wrap(@TypeOf(r)).init(r.reader());
    const str = try rr.reader().readAllAlloc(std.testing.allocator, std.math.maxInt(u8));
    defer std.testing.allocator.free(str);
    const output = std.mem.trimEnd(u8, str, "\n ");

    try std.testing.expectEqualStrings(expected, output);
}

test "remove includes by wrapping with tiny and large buffer" {
    const input = "abcdefghijklmno\npqrstuvwxyz";
    var r = std.io.fixedBufferStream(input);
    var temp = RemoveIncludes.Wrap(@TypeOf(r)).init(r.reader());
    var reader = temp.reader();

    var buf: [5]u8 = undefined;
    var n = try reader.read(&buf);
    try std.testing.expectEqualStrings("abcde", buf[0..n]);
    n = try reader.read(&buf);
    try std.testing.expectEqualStrings("fghij", buf[0..n]);
    n = try reader.read(&buf);
    try std.testing.expectEqualStrings("klmno", buf[0..n]);
    n = try reader.read(&buf);
    try std.testing.expectEqualStrings("\n", buf[0..n]);
    n = try reader.read(&buf);
    try std.testing.expectEqualStrings("pqrst", buf[0..n]);
    n = try reader.read(&buf);
    try std.testing.expectEqualStrings("uvwxy", buf[0..n]);
    n = try reader.read(&buf);
    try std.testing.expectEqualStrings("z\n", buf[0..n]);

    var buf_large: [100]u8 = undefined;
    try r.seekTo(0);
    n = try reader.read(&buf_large);
    try std.testing.expectEqualStrings("abcdefghijklmno\n", buf_large[0..n]);
    n = try reader.read(&buf_large);
    try std.testing.expectEqualStrings("pqrstuvwxyz\n", buf_large[0..n]);
}
