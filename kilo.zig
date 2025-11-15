const std = @import("std");
const c = @cImport({
    @cInclude("termios.h");
    @cInclude("ctype.h");
    @cInclude("unistd.h");
    @cInclude("errno.h");
    @cInclude("sys/ioctl.h");
    @cInclude("stdio.h");
    @cInclude("string.h");
    @cInclude("time.h");
});
const fs = std.fs;

var orig_termios: c.termios = undefined;
const kiloversion = "0.0.1";
const kilotabstop = 8;
const erow = struct { size: usize, chars: []u8, render: ?[]u8, rsize: usize };

const EditorSyntax = struct {
    filetype: [*c]u8,
    filematch: [*c][*c]u8,
    keywords: [*c][*c]u8,
    singleline_comment_start: [*c]u8,
    multiline_comment_start: [*c]u8,
    multiline_comment_end: [*c]u8,
    flags: c_int,
};

const editorConfig = struct {
    cx: usize,
    cy: usize,
    rx: usize,
    rowoff: usize,
    coloff: usize,
    screenrows: usize,
    screencols: usize,
    orig_termios: c.termios,
    numrows: usize,
    row: []erow,
    filename: ?[]u8,
    statusmsg: []u8,
    statusmsg_time: c.time_t,
    dirty: usize,
};
var E: editorConfig = .{
    .cx = 0,
    .cy = 0,
    .screenrows = 1,
    .screencols = 1,
    .orig_termios = undefined,
    .numrows = 1,
    .row = undefined,
    .rowoff = 0,
    .coloff = 0,
    .rx = 0,
    .filename = null,
    .statusmsg = undefined,
    .statusmsg_time = 0,
    .dirty = 0,
};

const TCSA: c_int = 2;
// terminal

pub fn die(message: []const u8) !void {
    const stderr = std.io.getStdErr().writer();

    _ = stderr.print("{s}: ", .{message}) catch {};

    if (c.write(std.io.getStdOut().handle, "\x1b[2J", 4) == -1) {
        return error.WriteFailed;
    }
    if (c.write(std.io.getStdOut().handle, "\x1b[H", 3) == 1) {
        return error.WriteFailed;
    }
    defer disableRawMode();
    std.process.exit(1);
}

fn ctrlKey(k: u8) u8 {
    return k & 0x1F;
}

const editorKey = enum(usize) {
    const ARROW_LEFT = 1000;
    const ARROW_RIGHT = 0;
    const ARROW_UP = 1;
    const ARROW_DOWN = 2;
    const DEL_KEY = 7;
    const PAGE_UP = 3;
    const PAGE_DOWN = 4;
    const HOME_KEY = 5;
    const END_KEY = 6;
    const BACKSPACE = 127;
};

pub fn disableRawMode() void {
    if (c.tcsetattr(std.c.STDIN_FILENO, c.TCSAFLUSH, &E.orig_termios) == -1) {
        die("tcsetattr") catch {};
    }
}

pub fn enableRawMode() void {
    if (c.tcgetattr(std.c.STDIN_FILENO, &E.orig_termios) == -1) {
        die("tcgetattr") catch {};
    }

    var raw = E.orig_termios;
    raw.c_iflag &= ~(@as(c_uint, c.BRKINT) | @as(c_uint, c.ICRNL) | @as(c_uint, c.INPCK) | @as(c_uint, c.ISTRIP) | @as(c_uint, c.IXON));
    raw.c_oflag &= ~(@as(c_uint, c.OPOST));
    raw.c_cflag |= @as(c_uint, c.CS8);
    raw.c_lflag &= ~(@as(c_uint, c.ECHO) | @as(c_uint, c.ICANON) | @as(c_uint, c.IEXTEN) | @as(c_uint, c.ISIG));
    raw.c_cc[c.VMIN] = 1;
    raw.c_cc[c.VTIME] = 0;

    if (c.tcsetattr(std.c.STDIN_FILENO, c.TCSAFLUSH, &raw) == -1) {
        std.debug.print("Error: tcsetattr failed\n", .{});
        die("tcsetattr") catch {};
    }

    // std.process.cleanExit(disableRawMode);
}

pub fn editorReadKey() !i32 {
    const stdin = std.io.getStdIn();

    var buf: [1]u8 = undefined;
    const bytes_read = try stdin.read(&buf);

    while (bytes_read != 1) {
        // Check if we read any data, and if the first byte is 'q'
        if (bytes_read == -1 and c.errno == c.EAGAIN) {
            try die("editorReadKey"); // Exit the loop if no bytes were read or 'q' was pressed
        }
    }

    if (buf[0] == '\x1B') {
        var seq: [3]u8 = undefined;

        if (try stdin.read(&seq) > 3) return '\x1b';

        if (seq[0] == '[') {
            if (seq[1] >= '0' and seq[1] <= '9') {
                if (seq[2] == '~') {
                    switch (seq[1]) {
                        '1' => {
                            return editorKey.HOME_KEY;
                        },
                        '3' => {
                            return editorKey.DEL_KEY;
                        },
                        '4' => {
                            return editorKey.END_KEY;
                        },
                        '5' => {
                            return editorKey.PAGE_UP;
                        },
                        '6' => {
                            return editorKey.PAGE_DOWN;
                        },
                        '7' => {
                            return editorKey.HOME_KEY;
                        },
                        '8' => {
                            return editorKey.END_KEY;
                        },
                        else => {},
                    }
                }
            } else {
                switch (seq[1]) {
                    'A' => {
                        return editorKey.ARROW_UP;
                    },
                    'B' => {
                        return editorKey.ARROW_DOWN;
                    },
                    'C' => {
                        return editorKey.ARROW_RIGHT;
                    },
                    'D' => {
                        return editorKey.ARROW_LEFT;
                    },
                    'H' => {
                        return editorKey.HOME_KEY;
                    },
                    'F' => {
                        return editorKey.END_KEY;
                    },
                    else => {},
                }
            }
        } else if (seq[0] == 'O') {
            switch (seq[1]) {
                'H' => {
                    return editorKey.HOME_KEY;
                },
                'F' => {
                    return editorKey.END_KEY;
                },
                else => {},
            }
        }
        return '\x1b';
    } else {
        return buf[0];
    }
}

pub fn getCursorPosition(rows: *usize, cols: *usize) !i32 {
    var buf: [32]u8 = undefined;
    var i: u32 = 0;

    if (c.write(std.io.getStdIn().handle, "\x1b[6n", 4) != 4) return -1;

    while (i < buf.len - 1) {
        if (c.read(std.io.getStdIn().handle, &buf[i], 1) != 1) break;
        if (buf[i] == 'R') break;
        i += 1;
    }
    buf[i] = '0';

    if (buf[0] != '\x1b' or buf[1] != '[') return -1;
    if (c.sscanf(buf[2..], "%d;%d", rows, cols) != 2) return -1;
    return 0;
}
const winsize = extern struct {
    ws_row: u16, // number of rows
    ws_col: u16, // number of columns
    ws_xpixel: u16, // pixel width (unused in this case)
    ws_ypixel: u16, // pixel height (unused in this case)
};

pub fn getWindowSize(rows: *usize, cols: *usize) !i32 {
    var ws: winsize = undefined;
    const ret = c.ioctl(std.io.getStdOut().handle, c.TIOCGWINSZ, &ws);

    if (ret == -1 or ws.ws_col == 0) {
        if (c.write(std.io.getStdOut().handle, "\x1b[999C\x1b[999B", 12) != 12) return -1;
        return try getCursorPosition(rows, cols);
    } else {
        cols.* = ws.ws_col;
        rows.* = ws.ws_row;
        return 0;
    }
}
// row operations
pub fn editorRowCxToRx(row: *erow, cx: usize) usize {
    var rx: usize = 0;
    for (0..cx) |j| {
        if (row.chars[j] == '\t') {
            rx += (kilotabstop - 1) - (rx % kilotabstop);
        }
        rx += 1;
    }
    return rx;
}

pub fn editorRowInsertChar(allocator: std.mem.Allocator, row: *erow, at: usize, ch: []u8) !void {
    var pos = at;
    if (at < 0 or at > row.size) {
        pos = row.size;
    }

    row.chars = try allocator.realloc(row.chars, row.size + 1);
    std.mem.copyBackwards(u8, row.chars[pos + 1 .. row.size + 1], row.chars[pos..row.size]);
    row.size += 1;
    row.chars[pos] = ch[0];
    try editorUpdateRow(allocator, row);
    E.dirty += 1;
}

pub fn editorRowDelChar(allocator: std.mem.Allocator, row: *erow, at: usize) !void {
    if (at < 0 or at >= row.size) return;
    std.mem.copyBackwards(u8, row.chars[at .. row.size - 1], row.chars[at + 1 .. row.size]);
    row.size -= 1;
    try editorUpdateRow(allocator, row);
    E.dirty += 1;
}

// editor operations
pub fn editorInsertChar(allocator: std.mem.Allocator, input: []u8) !void {
    if (E.cy == E.numrows) {
        try editorInsertRow(allocator, 0, input, input.len);
    }
    try editorRowInsertChar(allocator, &E.row[E.cy], E.cx, input);
    E.cx += 1;
}

pub fn editorDelChar(allocator: std.mem.Allocator) !void {
    if (E.cy == E.numrows) return;

    const row: *erow = &E.row[E.cy];
    if (E.cx > 0) {
        editorRowDelChar(allocator, row, E.cx - 1);
        E.cx -= 1;
    }
}
// file i/o
pub fn editorUpdateRow(allocator: std.mem.Allocator, row: *erow) !void {
    var tabs: usize = 0;
    for (0..row.chars.len) |j| {
        if (row.chars[j] == '\t') tabs += 1;
    }
    if (row.render != null) {
        if (row.render) |render| {
            allocator.free(render);
        }
    }
    row.render = try allocator.alloc(u8, row.chars.len + tabs * (kilotabstop - 1) + 1);

    var idx: usize = 0;

    for (0..row.chars.len) |j| {
        if (row.render) |render| {
            if (row.chars[j] == '\t') {
                idx += 1;
                row.render.?[idx] = ' ';
                while (idx % kilotabstop != 0) {
                    idx += 1;
                    render[idx] = 't';
                }
            } else {
                idx += 1;
                render[idx] = row.chars[j];
            }
        }
    }
    row.render.?[idx] = 0;

    row.rsize = idx;
}
pub fn editorInsertRow(allocator: std.mem.Allocator, at: usize, input: []u8, len: usize) !void {
    if (at < 0 or at > E.numrows) return;
    var temp: []erow = undefined;
    if (at == 0) {
        temp = try allocator.alloc(erow, 1);
    } else {
        temp = try allocator.realloc(E.row, E.numrows + 1);
    }

    E.row = temp;
    for (0..E.numrows) |index| {
        E.row[index].size += 1;
    }

    E.row[at].size = len;
    E.row[at].chars = try allocator.alloc(u8, input.len);
    @memcpy(E.row[at].chars, input);
    // E.row[at].chars = try allocator.realloc(E.row[at].chars, len + 1);
    // E.row[at].chars[input.len] = '0';

    E.row[at].rsize = 0;
    E.row[at].render = try std.fmt.allocPrint(allocator, "", .{});
    try editorUpdateRow(allocator, &E.row[at]);

    E.numrows += 1;
    E.dirty += 1;
}
pub fn editorRowsToString(allocator: std.mem.Allocator) ![]u8 {
    var totlen: usize = 0;
    for (0..E.numrows) |j| {
        totlen += E.row[j].chars.len;
    }

    const buf = try allocator.alloc(u8, totlen + E.numrows);
    var p: usize = 0;

    for (0..E.numrows) |j| {
        @memcpy(buf[p .. p + E.row[j].chars.len], E.row[j].chars);
        p += E.row[j].chars.len;
        buf[p] = '\n';
        p += 1;
    }

    const string = std.fmt.allocPrint(allocator, "{s}", .{buf});
    allocator.free(buf);
    return string;
}
pub fn editorOpen(filename: [*:0]u8) !void {
    var file = try std.fs.cwd().openFile(std.mem.span(filename), .{});
    defer file.close();

    var gpa = std.heap.GeneralPurposeAllocator(.{ .thread_safe = true }){};
    const allocator = gpa.allocator();

    E.filename = try std.fmt.allocPrint(allocator, "{s}", .{filename});

    // defer _ = gpa.deinit();

    var at: usize = 0;
    while (try file.reader().readUntilDelimiterOrEofAlloc(allocator, '\n', std.math.maxInt(usize))) |line| {
        try editorInsertRow(allocator, at, line, line.len);
        at += 1;
        defer allocator.free(line);
    }
    E.dirty = 0;
}
// append buffer
const abuf = struct { b: ?[]u8, len: usize };

const ABUF_INIT = abuf{
    .b = null,
    .len = 0,
};

pub fn abFree(ab: *abuf) !void {
    const allocator = std.heap.c_allocator;
    if (ab.b) |buffer| {
        defer allocator.free(buffer);
    }
}

//input

pub fn editorMoveCursor(key: i32) void {
    const row = if (E.cy >= E.numrows) null else E.row[E.cy];
    switch (key) {
        editorKey.ARROW_LEFT => {
            if (E.cx != 0) {
                E.cx = E.cx - 1;
            } else if (E.cy > 0) {
                E.cy -= 1;
                E.cx = E.row[E.cy].chars.len;
            }
            return;
        },
        editorKey.ARROW_RIGHT => {
            if (row != null and E.cx < row.?.chars.len) {
                E.cx += 1;
            } else if (row != null and E.cx == row.?.chars.len) {
                E.cy += 1;
                E.cx = 1;
            }
            return;
        },
        editorKey.ARROW_UP => {
            if (E.cy != 0)
                E.cy = E.cy - 1;
            return;
        },
        editorKey.ARROW_DOWN => {
            if (E.cy < E.numrows)
                E.cy += 1;
            return;
        },
        else => {
            return;
        },
    }
    row = if (E.cy >= E.numrows) null else E.row[E.cy];
    const rowlen = if (row) row.chars.len else 0;
    if (E.cx > rowlen) {
        E.cx = rowlen + 1;
    }
}

pub fn editorProcessKeyPress(allocator: std.mem.Allocator) !void {
    const key: i32 = try editorReadKey();

    switch (key) {
        '\r' => {
            return;
        },
        ctrlKey('q') => {
            if (E.dirty > 0) {
                const message = try std.fmt.allocPrint(allocator, "WARNING !!! Files Have unsaved changes press q again to quit without saving", .{});
                try editorSetStatusMessage(message);
                E.dirty = 0;
                return;
            }
            allocator.free(E.statusmsg);
            if (c.write(std.io.getStdOut().handle, "\x1b[2J", 4) == -1) {
                return error.WriteFailed;
            }
            if (c.write(std.io.getStdOut().handle, "\x1b[H", 3) == 1) {
                return error.WriteFailed;
            }
            return error.UserExit;
        },
        ctrlKey('s') => {
            try editorSave(allocator);
            return;
        },
        editorKey.HOME_KEY => {
            if (E.cy < E.numrows) {
                E.cx = E.row[E.cy].size;
            }
            return;
        },
        editorKey.END_KEY => {
            E.cx = E.screencols - 1;
            return;
        },
        editorKey.BACKSPACE, ctrlKey('h'), editorKey.DEL_KEY => {
            if (key == editorKey.DEL_KEY) {
                editorMoveCursor(editorKey.ARROW_RIGHT);
            }
            editorDelChar(allocator);
            return;
        },
        editorKey.PAGE_UP, editorKey.PAGE_DOWN => {
            if (key == editorKey.PAGE_UP) {
                E.cy = E.rowoff;
            } else if (key == editorKey.PAGE_DOWN) {
                E.cy = E.rowoff + E.screenrows - 1;
                if (E.cy > E.numrows) E.cy = E.numrows;
            }
            var times = E.screenrows;
            while (times > 0) {
                editorMoveCursor(if (key == editorKey.PAGE_UP) editorKey.ARROW_UP else editorKey.ARROW_DOWN);

                times -= 1;
            }
            return;
        },
        editorKey.ARROW_LEFT, editorKey.ARROW_RIGHT, editorKey.ARROW_UP, editorKey.ARROW_DOWN => {
            editorMoveCursor(key);
            return;
        },
        ctrlKey('l'), '\x1b' => {
            return;
        },
        else => {
            const tmp: []const u8 = &[1]u8{@intCast(key)};
            const ch: []u8 = try std.fmt.allocPrint(allocator, "{s}", .{tmp});
            try editorInsertChar(allocator, ch);
            allocator.free(ch);
            return;
        },
    }
}

//output
pub fn editorScroll() void {
    E.rx = 0;
    if (E.cy < E.numrows) {
        E.rx = editorRowCxToRx(&E.row[E.cy], E.cx);
    }
    if (E.cy < E.rowoff) {
        E.rowoff = E.cy;
    }
    if (E.cy >= E.rowoff + E.screenrows) {
        E.rowoff = E.cy - E.screenrows + 1;
    }
    if (E.rx < E.coloff) {
        E.coloff = E.rx;
    }
    if (E.rx >= E.coloff + E.screencols) {
        E.coloff = E.rx - E.screencols + 1;
    }
}

pub fn editorDrawRows(allocator: std.mem.Allocator, ab: *std.ArrayListUnmanaged(u8)) !void {
    const rowsize = @as(u32, @intCast(E.screenrows));
    const colsize = @as(u32, @intCast(E.screencols));

    for (0..rowsize) |y| {
        const filerow = y + E.rowoff;
        if (filerow >= E.numrows) {
            if (y == rowsize - 1 and E.numrows == 0) {
                const welcome = try std.fmt.allocPrint(allocator, "Kilo editor -- version {s}", .{kiloversion});

                var padding = (colsize - welcome.len) / 2;
                if (padding > 0) {
                    try ab.appendSlice(allocator, "~");
                    padding -= 1;
                }
                while (padding > 0) {
                    try ab.appendSlice(allocator, " ");
                    padding -= 1;
                }

                if (welcome.len > colsize) {
                    try ab.appendSlice(allocator, welcome[0..colsize]);
                } else {
                    try ab.appendSlice(allocator, welcome);
                }
                allocator.free(welcome);
            } else {
                try ab.appendSlice(allocator, "~");
            }
        } else {
            try ab.appendSlice(allocator, E.row[filerow].render.?[0..]);
        }
        try ab.appendSlice(allocator, "\x1b[K");
        try ab.appendSlice(allocator, "\r\n");
    }
}

pub fn editorDrawStatusBar(ab: *std.ArrayListUnmanaged(u8), allocator: std.mem.Allocator) !void {
    try ab.appendSlice(allocator, "\x1b[7m");

    const filename: []const u8 = if (E.filename != null) E.filename.? else "[No Name]";
    const status = try std.fmt.allocPrint(allocator, "{s:_^4} - {d} lines {d}/{d} {s}", .{ filename, E.numrows, E.cy + 1, E.numrows, if (E.dirty > 0) "(modified)" else " " });
    var len: usize = 0;
    while (len < E.screencols) {
        if (E.screencols - len == status.len) {
            try ab.appendSlice(allocator, status);
            break;
        } else {
            try ab.appendSlice(allocator, " ");
            len += 1;
        }
    }
    try ab.appendSlice(allocator, "\x1b[m");
    try ab.appendSlice(allocator, "\r\n");
    allocator.free(status);
}

pub fn editorDrawMessageBar(ab: *std.ArrayListUnmanaged(u8), allocator: std.mem.Allocator) !void {
    try ab.appendSlice(allocator, "\x1b[K");
    var msglen = E.statusmsg.len;
    if (msglen > E.screencols) {
        msglen = E.screencols;
    }
    if (msglen > 0 and (c.time(null) - E.statusmsg_time) < 5) {
        try ab.appendSlice(allocator, E.statusmsg);
    }
}

pub fn editorRefreshScreen(allocator: std.mem.Allocator) !void {
    editorScroll();

    var ab = std.ArrayListUnmanaged(u8){};
    defer ab.deinit(allocator);

    try ab.appendSlice(allocator, "\x1b[?25l");
    try ab.appendSlice(allocator, "\x1b[H");

    try editorDrawRows(allocator, &ab);
    try editorDrawStatusBar(&ab, allocator);
    try editorDrawMessageBar(&ab, allocator);

    const buf = try std.fmt.allocPrint(allocator, "\x1b[{};{}H", .{ (E.cy - E.rowoff) + 1, (E.rx - E.coloff) + 2 });
    try ab.appendSlice(allocator, buf);
    allocator.free(buf);

    try ab.appendSlice(allocator, "\x1b[?25h");

    try std.io.getStdOut().writeAll(ab.items);
}

pub fn editorSetStatusMessage(message: []u8) !void {
    E.statusmsg = message;
    E.statusmsg_time = c.time(null);
}

pub fn editorSave(allocator: std.mem.Allocator) !void {
    if (E.filename == null) return;

    const buf: []u8 = try editorRowsToString(allocator);
    const file = fs.cwd().createFile(E.filename.?, .{
        .read = true,
        .truncate = true,
    }) catch |err| {
        const message = try std.fmt.allocPrint(allocator, "Can't save! I/O error: {any}", .{err});
        try editorSetStatusMessage(message);
        return;
    };
    file.writeAll(buf) catch |err| {
        const message = try std.fmt.allocPrint(allocator, "Can't save! I/O error: {any}", .{err});
        try editorSetStatusMessage(message);
        return;
    };
    E.dirty = 0;
    file.close();
    const message = try std.fmt.allocPrint(allocator, "{d} bytes written to disk", .{buf.len});
    try editorSetStatusMessage(message);
    // allocator.free(message);
    return;
}

//init
pub fn initEditor() !void {
    const res = try getWindowSize(&E.screenrows, &E.screencols);
    E.cx = 0;
    E.cy = 0;
    E.rx = 0;
    E.numrows = 0;
    E.row = undefined;
    E.rowoff = 0;
    E.filename = null;
    E.statusmsg = undefined;
    E.statusmsg_time = 0;

    if (res == -1) {
        die("getwindowsize") catch {};
    }
    E.screenrows -= 3;
}

pub fn main() !void {
    defer disableRawMode();
    enableRawMode();
    try initEditor();
    const args = std.os.argv.len;
    if (args >= 2)
        try editorOpen(std.os.argv[1]);

    var gpa = std.heap.GeneralPurposeAllocator(.{ .thread_safe = true }){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    const formatted = try std.fmt.allocPrint(allocator, "HELP: Ctrl-S = save |  Ctrl-Q = quit", .{});
    try editorSetStatusMessage(formatted);
    // allocator.free(formatted);

    while (true) {
        try editorRefreshScreen(allocator);
        editorProcessKeyPress(allocator) catch {
            break;
        };
    }
}
