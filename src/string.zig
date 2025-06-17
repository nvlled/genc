const std = @import("std");

// I wrote these before I discovered about std.io.StreamSource,
// but I'll just keep them here for reference

const StringReader = struct {
    buf: []const u8,
    pos: usize = 0,

    const Self = @This();
    const Error = error{EndOfStream};

    pub fn read(self: *Self, buffer: []u8) Error!usize {
        @memset(buffer, 0);
        const i = self.pos;
        if (i >= self.buf.len) {
            return 0;
        }
        const buf = self.buf[i..];
        const len = @min(buffer.len, buf.len);
        @memcpy(buffer[0..len], buf[0..len]);
        self.pos += len;
        return buffer.len;
    }

    pub fn reset(self: *Self) void {
        self.pos = 0;
    }

    pub fn reader(self: *Self) std.io.Reader(
        *Self,
        Error,
        read,
    ) {
        return .{ .context = self };
    }
};

fn StringWriter(comptime size: comptime_int) type {
    return struct {
        buf: [size]u8,
        pos: usize = 0,

        const Self = @This();
        const Error = error{StringBufferToOSmall};

        pub fn write(self: *Self, bytes: []const u8) Error!usize {
            const i = self.pos;
            if (i + bytes.len > self.buf.len) {
                return error.StringBufferToOSmall;
            }
            @memcpy(self.buf[i..(i + bytes.len)], bytes);
            self.pos += bytes.len;
            return bytes.len;
        }

        pub fn string(self: *Self) []const u8 {
            return self.buf[0..self.pos];
        }

        pub fn reset(self: *Self) void {
            self.pos = 0;
        }

        pub fn writer(self: *Self) std.io.Writer(
            *Self,
            Error,
            write,
        ) {
            return .{ .context = self };
        }
    };
}
