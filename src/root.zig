const std = @import("std");
const stderr = std.io.getStdErr();
const ts = @import("tree-sitter");
const intf = @import("intf");
const String = []const u8;

// TODO: use new io

pub extern fn tree_sitter_c() callconv(.C) *const ts.Language;

const Kind = enum(u16) {
    unknown = 0,
    identifier = 1,
    primitive_type = 93,
    pointer = 96,
    system_lib_string = 155,
    comment = 160,
    preproc_include = 164,
    preproc_def = 165,
    preproc_ifdef = 170,
    function_definition = 196,
    declaration = 198,
    type_definition = 199,
    parenthesized_declarator = 219,
    pointer_declarator = 226,
    function_declarator = 230,
    compound_statement = 241,
    storage_class_specifier = 242,
    enum_specifier = 247,
    struct_specifier = 249,
    union_specifier = 250,
    field_declaration_list = 251,
    field_declaration = 253,
    bitfield_clause = 255,
    parameter_list = 258,
    for_statement = 273,
    expression_statement = 266,
    string_literal = 320,
    macro_type_specifier = 323,
    field_identifier = 360,
    type_identifier = 362,

    // TODO: log a message when this kind is encountered
    ERROR = 65535,

    fn get(maybeNode: ?ts.Node) !Kind {
        const node = maybeNode orelse return Kind.unknown;
        return std.meta.intToEnum(Kind, node.kindId()) catch |err| {
            std.debug.print("unregistered node kind: {s} = {d}\n", .{ node.kind(), node.kindId() });
            return err;
        };
    }
};

const StorateClassSpec = enum {
    unknown,
    @"inline",
    static,
    @"extern",
};

const FuncFlags = packed struct(u3) {
    is_inline: bool = false,
    is_static: bool = false,
    is_extern: bool = false,
};

pub const FuncItem = struct {
    name: String,
    flags: FuncFlags,
};

pub const StructItem = struct {
    name: String,

    // internal use only, no touchie, authorized personnel only
    // bathroom is that way
    _context: *const anyopaque = undefined,

    const Self = @This();

    // since this is somewhat an expensive search
    // use a function instead of a flag to lazily search
    // only when required.
    pub fn hasBitFields(self: Self) bool {
        const struct_body: *const ts.Node = @alignCast(@ptrCast(self._context));

        var field_iter = struct_body.iterateChildren();
        defer field_iter.destroy();

        while (field_iter.nextNamed()) |field_decl| {
            // ignore intToEnum parse errors
            if ((Kind.get(field_decl) catch Kind.unknown) != .field_declaration) continue;

            if (field_decl.namedChild(2)) |n| {
                return (Kind.get(n) catch Kind.unknown) == .bitfield_clause;
            }
        }

        return false;
    }
};

pub const FuncFilter = struct {
    context: *anyopaque = undefined,
    predicate: *const fn (context: *anyopaque, item: FuncItem) bool,
};

pub const StructFilter = struct {
    context: *anyopaque = undefined,
    predicate: *const fn (context: *anyopaque, item: StructItem) bool,
};

// To make more sense of the dump functions, you need to understand
// the overall node tree (and pecularities) created by the language parser.
//
// To view a node tree, either use Tree.printDotGraph or Node.writeJSON:
//
//    try node.writeJSON(stderr.writer().any(), .{ .source = source });
//
// To view the whole tree starting from the root, set the option
//
//     .{ .debug_tree = true }

const GenAccessors = struct {
    source: []const u8,
    options: Options = .{},
    w: std.io.AnyWriter,

    const Self = @This();

    const Options = struct {
        retain_includes: bool = false,
        func_prototype: bool = false,
        prepend_str: []const u8 = &.{},
        append_str: []const u8 = &.{},
        filter: ?StructFilter = null,
        debug_tree: bool = false,
    };

    fn accessors(
        self: Self,
        options: Options,
    ) !void {
        const source = self.source;
        const language = tree_sitter_c();
        defer language.destroy();

        std.debug.assert(language.abiVersion() == ts.LANGUAGE_VERSION);
        const parser = ts.Parser.create();
        defer parser.destroy();
        try parser.setLanguage(language);

        const tree = parser.parseString(source, null) orelse @panic("failed to parse");
        defer tree.destroy();

        const root = tree.rootNode();

        if (options.debug_tree) {
            try root.writeJSON(stderr.writer().any(), .{ .source = source });
        }

        try self.dump(root, options);
    }

    fn dump(self: Self, root: ts.Node, options: Options) !void {
        const source = self.source;
        const w = self.w;

        var iter_children = root.iterateChildren();
        defer iter_children.destroy();

        while (iter_children.nextNamed()) |node| {
            const kind: Kind = try .get(node);
            switch (kind) {
                .preproc_include => {
                    if (options.retain_includes) {
                        try dumpInclude(source, w, node);
                        try w.writeAll("\n\n");
                    }
                },

                .type_definition, .declaration => {
                    if (node.namedChild(0)) |child| {
                        if (try Kind.get(child) == .struct_specifier) {
                            try self.dumpStructAccessors(child);
                        }
                    }
                },

                .preproc_ifdef => {
                    try self.dump(node, options);
                },

                .struct_specifier => {
                    try self.dumpStructAccessors(node);
                },

                else => {},
            }
        }
    }

    fn dumpStructAccessors(
        self: Self,
        struct_spec: ts.Node,
    ) !void {
        const source = self.source;
        const options = self.options;
        const w = self.w;

        std.debug.assert(try Kind.get(struct_spec) == .struct_specifier);
        std.debug.assert(struct_spec.childCount() >= 3);
        const struct_name = struct_spec.child(1).?.raw(source);
        const struct_body = struct_spec.child(2).?;
        std.debug.assert(try Kind.get(struct_body) == .field_declaration_list);

        if (options.filter) |filter| {
            const item = StructItem{
                .name = struct_name,
                ._context = @ptrCast(&struct_body),
            };

            const ok = filter.predicate(filter.context, item);
            if (!ok) return;
        }

        var field_iter = struct_body.iterateChildren();
        defer field_iter.destroy();

        while (field_iter.nextNamed()) |field_decl| {
            if (try Kind.get(field_decl) != .field_declaration) continue;

            const ftype = field_decl.namedChild(0).?;
            const second_child = field_decl.namedChild(1) orelse {
                // field has no identifier
                continue;
            };

            const identifier, const ptr_count = try unwrapPointerDeclarators(second_child);

            if (try isFieldAnonContainer(field_decl)) {
                // skip if it's a pointer to anonymous struct or union
                if (ptr_count > 0) continue;

                const c = ftype.child(1).?;
                var iter = c.iterateChildren();
                defer iter.destroy();

                while (iter.nextNamed()) |sub_field_decl| {
                    if (try Kind.get(sub_field_decl) != .field_declaration) continue;

                    // Do not recursively descend into madness of nested anonymous structs
                    // Even if C and the Law allows it, I won't
                    // (or rather I just want to keep it simple for now)
                    if (try isFieldAnonContainer(sub_field_decl)) continue;

                    const sub_ftype = sub_field_decl.namedChild(0).?;
                    const sub_child = sub_field_decl.namedChild(1) orelse {
                        // field has no identifier
                        continue;
                    };
                    const sub_identifier, const sub_ptr_count = try unwrapPointerDeclarators(sub_child);

                    try writeAccessor(
                        .{
                            .w = w,
                            .return_type = sub_ftype.raw(source),
                            .ptr_count = sub_ptr_count,
                            .struct_name = struct_name,
                            .prop_name = sub_identifier.raw(source),
                            .namespace = identifier.raw(source),
                            .prototype = options.func_prototype,
                        },
                    );
                }
            } else {
                try writeAccessor(.{
                    .w = w,
                    .return_type = ftype.raw(source),
                    .ptr_count = ptr_count,
                    .struct_name = struct_name,
                    .prop_name = identifier.raw(source),
                    .prototype = options.func_prototype,
                });
            }
        }
    }

    /// Returns true if field declaration is
    /// an anonymous struct, union or even enum,
    ///
    /// ```
    /// struct A {
    ///     int regularField;
    ///     union { int x; } fieldDecl1;  // anonymous union
    ///     struct { int x; } fieldDecl2; // anonymous struct
    /// }
    /// ```
    fn isFieldAnonContainer(field_decl: ts.Node) !bool {
        const ftype = field_decl.namedChild(0) orelse return false;

        if (ftype.child(1)) |c| {
            return try Kind.get(c) == .field_declaration_list;
        }
        return false;
    }

    fn writeAccessor(args: struct {
        w: std.io.AnyWriter,
        return_type: []const u8,
        ptr_count: usize,
        struct_name: []const u8,
        prop_name: []const u8,
        namespace: ?[]const u8 = null,
        prototype: bool,
    }) !void {
        const w = args.w;
        if (!args.prototype) try w.writeAll("extern inline ");
        try w.writeAll(args.return_type);
        try w.writeAll(" ");
        for (0..args.ptr_count) |_| {
            try w.writeAll("*");
        }
        try w.writeAll(args.struct_name);
        try w.writeAll("_get_");
        if (args.namespace) |name| {
            try w.writeAll(name);
            try w.writeAll("_");
        }
        try w.writeAll(args.prop_name);
        try w.writeAll("(const struct ");
        try w.writeAll(args.struct_name);
        try w.writeAll(" *self)");
        if (args.prototype) {
            try w.writeAll(";\n");
        } else {
            try w.writeAll(" { return self->");
            if (args.namespace) |name| {
                try w.writeAll(name);
                try w.writeAll(".");
            }
            try w.writeAll(args.prop_name);
            try w.writeAll("; }\n");
        }

        if (!args.prototype) try w.writeAll("extern inline ");
        try w.writeAll("void ");
        try w.writeAll(args.struct_name);
        try w.writeAll("_set_");
        if (args.namespace) |name| {
            try w.writeAll(name);
            try w.writeAll("_");
        }
        try w.writeAll(args.prop_name);
        try w.writeAll("(struct ");
        try w.writeAll(args.struct_name);
        try w.writeAll(" *self, ");
        try w.writeAll(args.return_type);
        try w.writeAll(" ");
        for (0..args.ptr_count) |_| {
            try w.writeAll("*");
        }
        try w.writeAll("val)");
        if (args.prototype) {
            try w.writeAll(";");
        } else {
            try w.writeAll(" { self->");
            if (args.namespace) |name| {
                try w.writeAll(name);
                try w.writeAll(".");
            }
            try w.writeAll(args.prop_name);
            try w.writeAll(" = val; }");
        }
        try w.writeAll("\n\n");
    }
};

const GenPrototype = struct {
    source: []const u8,
    options: Options = .{},
    w: std.io.AnyWriter,

    const Self = @This();

    const Options = struct {
        comments: bool = true,
        retain_includes: bool = false,
        prepend_str: []const u8 = &.{},
        append_str: []const u8 = &.{},
        filter: ?FuncFilter = null,
        debug_tree: bool = false,
    };

    fn functionProto(
        self: Self,
    ) !void {
        const source = self.source;
        const options = self.options;
        const w = self.w;

        const language = tree_sitter_c();
        defer language.destroy();

        std.debug.assert(language.abiVersion() == ts.LANGUAGE_VERSION);
        const parser = ts.Parser.create();
        defer parser.destroy();
        try parser.setLanguage(language);

        const tree = parser.parseString(source, null) orelse @panic("failed to parse");
        defer tree.destroy();

        const root = tree.rootNode();

        if (options.debug_tree) try root.writeJSON(stderr.writer().any(), .{ .source = source });

        if (options.prepend_str.len > 0) {
            try w.writeAll(options.prepend_str);
            try w.writeAll("\n");
        }

        try self.dump(root);
    }

    fn dump(self: Self, root: ts.Node) !void {
        const source = self.source;
        const options = self.options;
        const w = self.w;

        var iter_children = root.iterateChildren();
        defer iter_children.destroy();

        var i: usize = 0;
        var start_comment: ?usize = null;
        while (iter_children.nextNamed()) |node| {
            const kind: Kind = try .get(node);

            defer {
                i += 1;
                if (kind != .comment) {
                    start_comment = null;
                }
            }

            switch (kind) {
                .comment => {
                    const range = node.startByte();
                    if (range > 0 and range < source.len - 1 and source[range - 1] == '\n') {
                        // only include comments that start on a new line
                        start_comment = start_comment orelse i;
                    }
                },

                .preproc_include => {
                    if (options.retain_includes) {
                        try dumpInclude(source, w, node);
                        try w.writeAll("\n\n");
                    }
                },

                .preproc_ifdef => try self.dump(node),

                .function_definition => {
                    if (options.comments) {
                        for ((start_comment orelse i)..i) |k| {
                            const comment = root.namedChild(@intCast(k)) orelse continue;
                            try w.writeAll(comment.raw(source));
                            try w.writeAll("\n");
                        }
                    }

                    const ok = try self.dumpFnPrototype(w, node);
                    if (ok) try w.writeAll("\n\n");
                },

                else => {},
            }
        }

        if (options.append_str.len > 0) {
            try w.writeAll("\n");
            try w.writeAll(options.append_str);
        }
    }

    fn dumpFnPrototype(
        self: Self,
        w: std.io.AnyWriter,
        fn_def: ts.Node,
    ) !bool {
        const source = self.source;
        const options = self.options;

        std.debug.assert(try Kind.get(fn_def) == .function_definition);
        std.debug.assert(fn_def.childCount() >= 2);

        var start_index: u32 = 0;
        var flags = FuncFlags{};
        while (fn_def.child(start_index)) |n| {
            if (try Kind.get(n) != .storage_class_specifier) break;
            const tag = std.meta.stringToEnum(StorateClassSpec, n.raw(source));
            switch (tag orelse StorateClassSpec.unknown) {
                .@"extern" => flags.is_extern = true,
                .static => flags.is_static = true,
                .@"inline" => flags.is_inline = true,
                .unknown => {},
            }
            start_index += 1;
        }

        const decl, var ptr_count = try unwrapPointerDeclarators(fn_def.child(start_index + 1).?);
        const kind: Kind = try .get(decl);
        std.debug.assert(kind == .function_declarator or kind == .parenthesized_declarator);
        std.debug.assert(decl.childCount() >= 2);

        // the node structure is different for functions like: int int funcname(void) { }
        // where the parameter is just void identifier.
        const is_void_arg = kind == .parenthesized_declarator;

        const fn_name = switch (is_void_arg) {
            true => fn_def.child(start_index).?.raw(source),
            false => decl.child(0).?.raw(source),
        };

        if (options.filter) |filter| {
            const item: FuncItem = .{
                .name = fn_name,
                .flags = flags,
            };
            if (!filter.predicate(filter.context, item)) return false;
        }

        if (is_void_arg) {
            const paren_decl = decl;
            const identifier = paren_decl.child(1).?.raw(source);
            try w.writeAll(fn_name);
            try w.writeAll("(");
            try w.writeAll(identifier);
            try w.writeAll(");");
            return true;
        }

        const fn_decl = decl;
        const ret_type = fn_def.child(start_index).?.raw(source);

        try w.writeAll(ret_type);
        if (ret_type.len > 0) try w.writeAll(" ");

        for (0..ptr_count) |_| {
            try w.writeAll("*");
        }
        try w.writeAll(fn_name);

        try w.writeAll("(");
        {
            const parameters = fn_decl.child(1).?;
            std.debug.assert(try Kind.get(parameters) == .parameter_list);

            var param_iter = parameters.iterateChildren();
            defer param_iter.destroy();

            var i: usize = 0;
            const num_params = parameters.namedChildCount();
            while (param_iter.nextNamed()) |param| {
                std.debug.assert(param.childCount() >= 1);

                const ptype = param.child(0).?.raw(source);
                try w.writeAll(ptype);

                if (param.child(1)) |pointer_decls| {
                    try w.writeAll(" ");
                    const identifier, ptr_count = try unwrapPointerDeclarators(pointer_decls);
                    for (0..ptr_count) |_| {
                        try w.writeAll("*");
                    }
                    try w.writeAll(identifier.raw(source));
                }

                if (i < num_params - 1) {
                    try w.writeAll(", ");
                }
                i += 1;
            }
        }
        try w.writeAll(");");

        return true;
    }
};

// tree-sitter-c wraps the identifier with one or more pointer declarations,
// so this function returns the identifier and number of * before it
fn unwrapPointerDeclarators(node: ts.Node) !struct { ts.Node, usize } {
    var count: usize = 0;
    var current = node;
    while (try Kind.get(current) == .pointer_declarator) {
        current = current.child(1) orelse @panic("pointer_declarater must have two subnodes");
        count += 1;
    }
    return .{ current, count };
}

fn dumpInclude(
    source: []const u8,
    w: std.io.AnyWriter,
    include_node: ts.Node,
) !void {
    const path = include_node.child(1) orelse return;

    try w.writeAll("#include ");
    switch (try Kind.get(path)) {
        else => {},
        .string_literal => {
            if (path.child(1)) |str| {
                try w.writeByte('"');
                try w.writeAll(str.raw(source));
                try w.writeByte('"');
            }
        },
        .system_lib_string => {
            try w.writeAll(path.raw(source));
        },
    }
}

pub fn generatePrototype(
    source: String,
    w: std.io.AnyWriter,
    options: GenPrototype.Options,
) !void {
    const gen = GenPrototype{ .source = source, .options = options, .w = w };
    try gen.functionProto();
}

// Caller must free returned string
pub fn prototypeString(
    allocator: std.mem.Allocator,
    source: String,
    options: GenPrototype.Options,
) ![]const u8 {
    var buf = std.ArrayList(u8).init(allocator);
    const gen = GenPrototype{ .source = source, .options = options, .w = buf.writer().any() };
    try gen.functionProto();
    return buf.toOwnedSlice();
}

pub fn generateAccessors(
    source: String,
    w: std.io.AnyWriter,
    options: GenAccessors.Options,
) !void {
    const gen = GenAccessors{ .source = source, .options = options, .w = w };
    try gen.accessors(options);
}

pub fn accessorsString(
    allocator: std.mem.Allocator,
    source: String,
    options: GenAccessors.Options,
) ![]const u8 {
    var buf = std.ArrayList(u8).init(allocator);
    const gen = GenAccessors{ .source = source, .options = options, .w = buf.writer().any() };
    try gen.accessors(options);
    return buf.toOwnedSlice();
}

test "generate function prototype" {
    const source =
        \\ #include "def.h"
        \\
        \\ int x = 10; // will not be included
        \\ int z = 10; // will not be included
        \\struct Bar** foo(int x, char * y) { return 0; }
        \\
        \\int y = 123; // will not be included
        \\
        \\//this is bar
        \\//bar does something
        \\void bar() {}
        \\
        \\baz(void) {}
        \\
        \\/* This is function f,
        \\    it does nothing */
        \\void f() {}
        \\
        \\void g(void); // will not be included
        \\int *h(void); // will not be included
        \\
        \\void k(void) { }
        \\
    ;
    const expected =
        \\#include "def.h"
        \\
        \\struct Bar **foo(int x, char *y);
        \\
        \\//this is bar
        \\//bar does something
        \\void bar();
        \\
        \\baz(void);
        \\
        \\/* This is function f,
        \\    it does nothing */
        \\void f();
        \\
        \\void k(void);
        \\
        \\
    ;

    const expected_bare =
        \\struct Bar **foo(int x, char *y);
        \\
        \\void bar();
        \\
        \\baz(void);
        \\
        \\void f();
        \\
        \\void k(void);
        \\
        \\
    ;

    const allocator = std.testing.allocator;
    const output = try prototypeString(allocator, source, .{
        .comments = true,
        .retain_includes = true,
    });
    defer std.testing.allocator.free(output);

    try std.testing.expectEqualStrings(expected, output);

    const output_bare = try prototypeString(allocator, source, .{
        .comments = false,
        .retain_includes = false,
    });
    defer std.testing.allocator.free(output_bare);

    try std.testing.expectEqualStrings(expected_bare, output_bare);
}

test "generate nested function prototype" {
    const source =
        \\#ifdef X
        \\#ifdef Y
        \\void foo(void) { }
        \\int bar(int x) { return x; }
        \\#endif
        \\#endif
    ;
    const expected =
        \\void foo(void);
        \\
        \\int bar(int x);
        \\
        \\
    ;

    const allocator = std.testing.allocator;
    const output = try prototypeString(allocator, source, .{
        .comments = true,
        .retain_includes = true,
    });
    defer std.testing.allocator.free(output);

    try std.testing.expectEqualStrings(expected, output);
}

test "generate function prototype with filter" {
    const source =
        \\ extern inline static void foo() { }
        \\ void bar() { }
        \\
        \\ void baz() { }
    ;
    const expected =
        \\void foo();
        \\
        \\void bar();
        \\
        \\
    ;

    const allocator = std.testing.allocator;
    const output = try prototypeString(allocator, source, .{
        .comments = true,
        .retain_includes = true,
        .filter = .{
            .predicate = struct {
                fn _(_: *anyopaque, item: FuncItem) bool {
                    return item.flags.is_extern or std.mem.eql(u8, item.name, "bar");
                }
            }._,
        },
    });
    defer allocator.free(output);

    try std.testing.expectEqualStrings(expected, output);
}

test "generate accessors" {
    const code: []const u8 =
        \\int x;
        \\struct Foo {
        \\  int a;
        \\  char *b;
        \\};
    ;
    const expected =
        \\extern inline int Foo_get_a(const struct Foo *self) { return self->a; }
        \\extern inline void Foo_set_a(struct Foo *self, int val) { self->a = val; }
        \\
        \\extern inline char *Foo_get_b(const struct Foo *self) { return self->b; }
        \\extern inline void Foo_set_b(struct Foo *self, char *val) { self->b = val; }
    ;

    const output = try accessorsString(std.testing.allocator, code, .{});
    defer std.testing.allocator.free(output);
    try std.testing.expectEqualStrings(
        std.mem.trim(u8, expected, "\n "),
        std.mem.trim(u8, output, "\n "),
    );
}

test "generate accessor header" {
    const code: []const u8 =
        \\int x;
        \\struct Foo {
        \\  int a;
        \\  char *b;
        \\};
    ;
    const expected =
        \\int Foo_get_a(const struct Foo *self);
        \\void Foo_set_a(struct Foo *self, int val);
        \\
        \\char *Foo_get_b(const struct Foo *self);
        \\void Foo_set_b(struct Foo *self, char *val);
    ;

    const output = try accessorsString(std.testing.allocator, code, .{
        .func_prototype = true,
    });
    defer std.testing.allocator.free(output);

    try std.testing.expectEqualStrings(
        std.mem.trim(u8, expected, "\n "),
        std.mem.trim(u8, output, "\n "),
    );
}

test "generate accessor nested" {
    const code: []const u8 =
        \\int x;
        \\#ifdef X
        \\#ifdef Y
        \\struct Foo {
        \\  int a;
        \\  char *b;
        \\} X;
        \\typedef struct Bar {
        \\  int a;
        \\  char *b;
        \\} Y;
        \\#endif
        \\#endif
    ;
    const expected =
        \\int Foo_get_a(const struct Foo *self);
        \\void Foo_set_a(struct Foo *self, int val);
        \\
        \\char *Foo_get_b(const struct Foo *self);
        \\void Foo_set_b(struct Foo *self, char *val);
        \\
        \\int Bar_get_a(const struct Bar *self);
        \\void Bar_set_a(struct Bar *self, int val);
        \\
        \\char *Bar_get_b(const struct Bar *self);
        \\void Bar_set_b(struct Bar *self, char *val);
    ;

    const output = try accessorsString(std.testing.allocator, code, .{
        .func_prototype = true,
    });
    defer std.testing.allocator.free(output);

    try std.testing.expectEqualStrings(
        std.mem.trim(u8, expected, "\n "),
        std.mem.trim(u8, output, "\n "),
    );
}

test "generate accessors, filter bitfielded structs" {
    const source =
        \\#include <stdio.h>
        \\struct AAA {
        \\   int x; char y;
        \\   struct X *x;
        \\   union {struct V *rr; /*comment*/ int yy; struct {} zz;} xyz;
        \\  // comment
        \\};
        \\struct BBB {
        \\   int x;
        \\   int z: 100;
        \\};
    ;
    const expected =
        \\extern inline int BBB_get_x(const struct BBB *self) { return self->x; }
        \\extern inline void BBB_set_x(struct BBB *self, int val) { self->x = val; }
        \\
        \\extern inline int BBB_get_z(const struct BBB *self) { return self->z; }
        \\extern inline void BBB_set_z(struct BBB *self, int val) { self->z = val; }
        \\
        \\
    ;
    const output = try accessorsString(std.testing.allocator, source, .{
        .filter = .{
            .predicate = struct {
                fn _(_: *anyopaque, item: StructItem) bool {
                    return item.hasBitFields();
                }
            }._,
        },
    });
    defer std.testing.allocator.free(output);

    try std.testing.expectEqualStrings(
        std.mem.trim(u8, expected, "\n "),
        std.mem.trim(u8, output, "\n "),
    );
}

test "generate accessors with anonymous structs" {
    const source: []const u8 =
        \\struct Foo {
        \\  int x;
        \\  struct {
        \\    int a;
        \\    char *b;
        \\  } data;
        \\
        \\
        \\  union {
        \\    int c;
        \\  } data2;
        \\
        \\  struct {  } data2; // no fields
        \\
        \\  struct{}; // ignored
        \\  union{}; // ignored
        \\};
    ;
    const expected =
        \\extern inline int Foo_get_x(const struct Foo *self) { return self->x; }
        \\extern inline void Foo_set_x(struct Foo *self, int val) { self->x = val; }
        \\
        \\extern inline int Foo_get_data_a(const struct Foo *self) { return self->data.a; }
        \\extern inline void Foo_set_data_a(struct Foo *self, int val) { self->data.a = val; }
        \\
        \\extern inline char *Foo_get_data_b(const struct Foo *self) { return self->data.b; }
        \\extern inline void Foo_set_data_b(struct Foo *self, char *val) { self->data.b = val; }
        \\
        \\extern inline int Foo_get_data2_c(const struct Foo *self) { return self->data2.c; }
        \\extern inline void Foo_set_data2_c(struct Foo *self, int val) { self->data2.c = val; }
        \\
    ;

    const output = try accessorsString(std.testing.allocator, source, .{});
    defer std.testing.allocator.free(output);

    try std.testing.expectEqualStrings(
        std.mem.trim(u8, expected, "\n "),
        std.mem.trim(u8, output, "\n "),
    );
}

test "skip pointers to anonymous structs" {
    const source: []const u8 =
        \\struct Foo {
        \\  int x;
        \\  int y;
        \\
        \\  // ignore pointers to anonymous struct
        \\  struct {
        \\    int a;
        \\    char *b;
        \\  } *g;
        \\};
    ;
    const expected =
        \\int Foo_get_x(const struct Foo *self);
        \\void Foo_set_x(struct Foo *self, int val);
        \\
        \\int Foo_get_y(const struct Foo *self);
        \\void Foo_set_y(struct Foo *self, int val);
        \\
    ;

    const output = try accessorsString(std.testing.allocator, source, .{
        .func_prototype = true,
    });
    defer std.testing.allocator.free(output);

    try std.testing.expectEqualStrings(
        std.mem.trim(u8, expected, "\n "),
        std.mem.trim(u8, output, "\n "),
    );
}
