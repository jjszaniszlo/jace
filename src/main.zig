const std = @import("std");
const Lexer = @import("lexer");

pub fn main() void {
    std.debug.print("Hello, {s}", .{"world!"});
}
