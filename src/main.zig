const std = @import("std");
const stdout = std.io.getStdOut().writer();
const genc = @import("genc");

const Commands = enum {
    accessor,
    proto,
};

const CommonOptionNames = enum {
    @"name-contains",
    help,
    header,
};

const AccessorOptionNames = enum {
    @"bitfields-only",
    output,
};

const ProtoOptionNames = enum {
    @"inline-only",
    @"static-only",
};

const Options = struct {
    name_filters: std.StringHashMapUnmanaged(bool),
    bitfields_only: bool = false,
    generate_header: bool = false,
    inline_only: bool = false,
    static_only: bool = false,
};

fn usage(argv: [][:0]u8) !void {
    const prog_name = std.fs.path.basename(argv[0]);

    try stdout.print("usage:\n", .{});
    try stdout.print("  {s} accessor <c-source-file> [destination]\n", .{prog_name});
    try stdout.print("  {s} proto <c-source-file> [destination]\n", .{prog_name});
    try stdout.print("\n", .{});
    try stdout.print("description:\n", .{});
    try stdout.print("  {s} accessor: generates getter functions from the structs\n", .{prog_name});
    try stdout.print("  {s} proto:    generates prototypes from the functions\n", .{prog_name});
    try stdout.print("\n", .{});
    try stdout.print("accessor command options:\n", .{});
    try stdout.print("  --{s}\n", .{@tagName(AccessorOptionNames.@"bitfields-only")});
    try stdout.print("      include all structs with bitfields only\n", .{});
    try stdout.print("\n", .{});
    try stdout.print("proto command options:\n", .{});
    try stdout.print("  --{s}\n", .{@tagName(ProtoOptionNames.@"inline-only")});
    try stdout.print("      include inline functions only\n", .{});
    try stdout.print("  --{s}\n", .{@tagName(ProtoOptionNames.@"static-only")});
    try stdout.print("      include static functions only\n", .{});
    try stdout.print("\n", .{});
    try stdout.print("common options:\n", .{});
    try stdout.print("  --help\n", .{});
    try stdout.print("      show usage help\n", .{});
    try stdout.print("  --{s}\n", .{@tagName(CommonOptionNames.header)});
    try stdout.print("      generate a header file\n", .{});
    try stdout.print("  --{s}\t<name>\n", .{@tagName(CommonOptionNames.@"name-contains")});
    try stdout.print("      show only structs or functions with matching name\n", .{});
    try stdout.print("  --{s}\t<filename>\n", .{@tagName(AccessorOptionNames.output)});
    try stdout.print("      destination file\n", .{});
    try stdout.print("\n", .{});
    try stdout.print("examples:\n", .{});
    try stdout.print("  $ {s} accessors file.c\n", .{prog_name});
    try stdout.print("  $ {s} accessors file.c --output file-getters.c\n", .{prog_name});
    try stdout.print("  $ {s} proto file.c --inline\n", .{prog_name});
    return error.UsageError;
}

pub fn _main() !void {
    const allocator: std.mem.Allocator = blk: {
        if (@import("builtin").mode == .Debug) {
            var gpa = std.heap.GeneralPurposeAllocator(.{}){};
            break :blk gpa.allocator();
        } else break :blk std.heap.c_allocator;
    };

    const argv = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, argv);

    var input_name: []const u8 = "";
    var output_name: []const u8 = "";

    var options = Options{
        .bitfields_only = false,
        .name_filters = std.StringHashMapUnmanaged(bool){},
    };
    defer options.name_filters.deinit(allocator);

    if (argv.len < 2) {
        try usage(argv);
    }

    const command = blk: {
        if (std.meta.stringToEnum(Commands, argv[1])) |value| {
            break :blk value;
        }
        try stdout.print("unknown command: {s}\n", .{argv[1]});
        try usage(argv);
        break :blk Commands.accessor;
    };

    var arg_i: usize = 2;

    while (arg_i < argv.len) : (arg_i += 1) {
        const arg = argv[arg_i];
        const next_arg = if (arg_i < argv.len - 1) argv[arg_i + 1] else null;
        if (arg[0] == '-' and arg.len > 1) {
            const option_name = std.mem.trimLeft(u8, arg, "-");

            if (std.meta.stringToEnum(CommonOptionNames, option_name)) |value| {
                switch (value) {
                    .@"name-contains" => {
                        if (!isValidArg(next_arg)) try usage(argv);
                        try options.name_filters.put(allocator, next_arg.?, true);
                        arg_i += 1;
                        continue;
                    },
                    .header => options.generate_header = true,
                    .help => try usage(argv),
                }
            }

            switch (command) {
                .proto => {
                    if (std.meta.stringToEnum(ProtoOptionNames, option_name)) |value| {
                        switch (value) {
                            .@"inline-only" => {
                                options.inline_only = true;
                            },
                            .@"static-only" => {
                                options.static_only = true;
                            },
                        }
                    } else {
                        try stdout.print("invalid option: {s}\n", .{arg});
                        try usage(argv);
                    }
                },
                .accessor => {
                    if (std.meta.stringToEnum(AccessorOptionNames, option_name)) |value| {
                        switch (value) {
                            .output => {
                                if (!isValidArg(next_arg)) try usage(argv);
                                output_name = next_arg.?;
                                arg_i += 1;
                            },
                            .@"bitfields-only" => {
                                options.bitfields_only = true;
                            },
                        }
                    } else {
                        try stdout.print("invalid option: {s}\n", .{arg});
                        try usage(argv);
                    }
                },
            }
        } else {
            if (input_name.len == 0) {
                input_name = arg;
            } else if (output_name.len == 0) {
                output_name = arg;
            } else {
                try usage(argv);
            }
        }
    }

    if (input_name.len == 0) try usage(argv);

    var dealloc_ouput_name = false;
    defer if (dealloc_ouput_name) allocator.free(output_name);

    output_name = if (output_name.len > 0) output_name else blk: {
        dealloc_ouput_name = true;
        const suffix = "-accessors.c";
        if (std.mem.indexOf(u8, input_name, ".c")) |i| {
            const base_name = input_name[0..i];
            break :blk try std.mem.concat(allocator, u8, &[_][]const u8{ base_name, suffix });
        } else {
            break :blk try std.mem.concat(allocator, u8, &[_][]const u8{ input_name, suffix });
        }
    };

    const header_name = blk: {
        const suffix = ".h";
        if (std.mem.indexOf(u8, output_name, ".c")) |i| {
            const base_name = output_name[0..i];
            break :blk try std.mem.concat(allocator, u8, &[_][]const u8{ base_name, suffix });
        } else {
            break :blk try std.mem.concat(allocator, u8, &[_][]const u8{ output_name, suffix });
        }
    };
    defer allocator.free(header_name);

    const to_stdout = std.mem.eql(u8, output_name, "-");
    const input_file = std.fs.cwd().openFile(input_name, .{ .mode = .read_only }) catch |e| {
        switch (e) {
            error.FileNotFound => {
                try stdout.print("file not found: {s}\n", .{input_name});
                return;
            },
            else => return e,
        }
    };
    const output_file = blk: {
        if (to_stdout) {
            break :blk std.io.getStdOut();
        } else {
            break :blk try std.fs.cwd().createFile(output_name, .{
                .read = false,
                .truncate = true,
            });
        }
    };

    defer input_file.close();
    defer if (!to_stdout) output_file.close();

    const source = try input_file.reader().readAllAlloc(allocator, 1024 * 1024 * 1024);
    defer allocator.free(source);

    switch (command) {
        .accessor => {
            const filter =
                if (options.name_filters.size == 0 and !options.bitfields_only)
                    null
                else
                    genc.StructFilter{
                        .context = &options,
                        .predicate = struct {
                            fn _(context: *anyopaque, item: genc.StructItem) bool {
                                const opt: *Options = @ptrCast(@alignCast(context));
                                const names = opt.name_filters;
                                if (opt.bitfields_only and !item.hasBitFields()) return false;
                                return names.size == 0 or names.contains(item.name);
                            }
                        }._,
                    };

            try genc.generateAccessors(source, output_file.writer().any(), .{
                .filter = filter,
                .func_prototype = false,
                .debug_tree = true,
            });

            if (options.generate_header) {
                const header_file =
                    if (to_stdout)
                        output_file
                    else
                        try std.fs.cwd().createFile(header_name, .{
                            .read = false,
                            .truncate = true,
                        });

                defer header_file.close();
                try input_file.seekTo(0);
                try genc.generateAccessors(source, header_file.writer().any(), .{
                    .filter = filter,
                    .func_prototype = true,
                    .debug_tree = true,
                });
            }
        },
        .proto => {
            const filter =
                if (options.name_filters.size == 0 and
                !options.inline_only and
                !options.static_only)
                    null
                else
                    genc.FuncFilter{
                        .context = &options,
                        .predicate = &struct {
                            fn _(context: *anyopaque, item: genc.FuncItem) bool {
                                const opt: *Options = @ptrCast(@alignCast(context));
                                const names = opt.name_filters;
                                if (opt.inline_only and !item.flags.is_inline) return false;
                                if (opt.static_only and !item.flags.is_static) return false;
                                return true or names.size == 0 or names.contains(item.name);
                            }
                        }._,
                    };

            try genc.generatePrototype(source, output_file.writer().any(), .{
                .filter = filter,
                .debug_tree = true,
            });
        },
    }
}

pub fn main() !void {
    _main() catch |err| switch (err) {
        error.UsageError => {},
        else => {
            return err;
        },
    };
}

fn isValidArg(opt_arg: ?[]const u8) bool {
    if (opt_arg) |arg| {
        if (arg.len == 0) return false;
        if (arg[0] == '-') return false;
        return true;
    } else {
        return false;
    }
}
