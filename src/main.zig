const std = @import("std");
const stdout = std.io.getStdOut().writer();
const accessor = @import("c_accessor");

const OptionNames = enum {
    @"with-name",
    bitfields,
    header,
    output,
    help,
};

const Options = struct {
    name_filters: std.StringHashMapUnmanaged(bool),
    include_bitfields: bool = false,
    generate_header: bool = false,
};

fn usage(argv: [][:0]u8) !void {
    try stdout.print("usage: {s} <c-source-file> [destination]\n", .{argv[0]});
    try stdout.print("  default destination is $filename-accessors.c\n", .{});
    try stdout.print("\n", .{});
    try stdout.print("description:\n", .{});
    try stdout.print("  generates struct getter/setter functions\n", .{});
    try stdout.print("\n", .{});
    try stdout.print("options:\n", .{});
    try stdout.print("  --help\n", .{});
    try stdout.print("      show usage help\n", .{});
    try stdout.print("  --{s}\t<struct-name>\n", .{@tagName(OptionNames.@"with-name")});
    try stdout.print("      show only structs with matching name\n", .{});
    try stdout.print("  --{s}\t<filename>\n", .{@tagName(OptionNames.output)});
    try stdout.print("      destination file\n", .{});
    try stdout.print("  --{s}\n", .{@tagName(OptionNames.bitfields)});
    try stdout.print("      include all structs with bitfields (even if name doesn't match)\n", .{});
    try stdout.print("  --{s}\n", .{@tagName(OptionNames.header)});
    try stdout.print("      generate a header file\n", .{});
    try stdout.print("\n", .{});
    try stdout.print("examples:\n", .{});
    try stdout.print("  {s} file.c\n", .{argv[0]});
    try stdout.print("  {s} file.c --output file-getters.c\n", .{argv[0]});
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
        .include_bitfields = false,
        .name_filters = std.StringHashMapUnmanaged(bool){},
    };
    defer options.name_filters.deinit(allocator);

    var arg_i: usize = 1;
    while (arg_i < argv.len) : (arg_i += 1) {
        const arg = argv[arg_i];
        const next_arg = if (arg_i < argv.len - 1) argv[arg_i + 1] else null;
        if (arg[0] == '-') {
            const option_name = std.mem.trimLeft(u8, arg, "-");
            if (std.meta.stringToEnum(OptionNames, option_name)) |value| {
                switch (value) {
                    .@"with-name" => {
                        if (!isValidArg(next_arg)) try usage(argv);
                        try options.name_filters.put(allocator, next_arg.?, true);
                        arg_i += 1;
                    },
                    .output => {
                        if (!isValidArg(next_arg)) try usage(argv);
                        output_name = next_arg.?;
                        arg_i += 1;
                    },
                    .bitfields => {
                        options.include_bitfields = true;
                    },
                    .header => {
                        options.generate_header = true;
                    },
                    .help => {
                        try usage(argv);
                    },
                }
            } else {
                try stdout.print("invalid option: {s}\n", .{arg});
                try usage(argv);
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

    const input_file = try std.fs.cwd().openFile(input_name, .{ .mode = .read_only });
    const output_file = try std.fs.cwd().createFile(output_name, .{
        .read = false,
        .truncate = true,
    });
    defer input_file.close();
    defer output_file.close();

    const filter =
        if (options.name_filters.size == 0 and !options.include_bitfields)
            null
        else
            accessor.FilterOption{
                .context = &options,
                .predicate = &struct {
                    fn apply(item: accessor.FilterItem, context: *anyopaque) bool {
                        const opt: *Options = @ptrCast(@alignCast(context));
                        const names = opt.name_filters;
                        if (names.contains(item.name)) return true;
                        return opt.include_bitfields and item.has_bitfields;
                    }
                }.apply,
            };

    try accessor.generate(allocator, input_file.reader(), output_file.writer(), .{
        .filter = filter,
        .include_path = true,
    });

    if (options.generate_header) {
        const header_file = try std.fs.cwd().createFile(header_name, .{
            .read = false,
            .truncate = true,
        });
        defer header_file.close();
        try input_file.seekTo(0);
        try accessor.generate(allocator, input_file.reader(), header_file.writer(), .{
            .filter = filter,
            .include_path = false,
        });
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

// Caller must free the returned string
fn concat(allocator: std.mem.Allocator, str1: []const u8, str2: []const u8) ![]const u8 {
    var dest = try allocator.alloc(u8, str1.len + str2.len);
    const n = str1.len;
    @memcpy(dest[0..n], str1);
    @memcpy(dest[n..], str2);
    return dest;
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
