const std = @import("std");

pub const Token = struct {
    kind: Kind,
    span: Span,

    const Self = @This();

    pub const Span = struct {
        start: usize,
        end: usize,
    };

    pub const Kind = enum {
        None, 

        Identifier,
        String,
        Integer,
        Float,
        Operator,

        Equals,
        ColonEquals,
        SemiColon,
        Colon,
        ColonColon,
        FatArrow,
        Comma,
        Dot,
        LeftParen,
        RightParen,
        LeftBrace,
        RightBrace,
        LeftBracket,
        RightBracket,

        TypeKeyword,
        ClassKeyword,
        InstanceKeyword,
        CaseKeyword,
        LetKeyword,
        InKeyword,
        IfKeyword,
        ThenKeyword,
        ElseKeyword,
        ElseIfKeyword,
        DefKeyword,
        ConstKeyword,
        WhereKeyword,
        DoKeyword,
    };

    pub const keywords = std.StaticStringMap(Kind).initComptime(.{
        .{ "type", .TypeKeyword },
        .{ "class", .ClassKeyword },
        .{ "instance", .InstanceKeyword },
        .{ "case", .CaseKeyword },
        .{ "let", .LetKeyword },
        .{ "in", .InKeyword },
        .{ "if", .IfKeyword },
        .{ "then", .ThenKeyword },
        .{ "else", .ElseKeyword },
        .{ "elseif", .ElseIfKeyword },
        .{ "def", .DefKeyword },
        .{ "const", .ConstKeyword },
        .{ "where", .WhereKeyword },
        .{ "do", .DoKeyword },
    });

    pub fn get_keyword(buff: []const u8) ?Kind {
        return keywords.get(buff);
    }

    pub fn get_lexeme(self: Self) ?[]const u8 {
        return switch (self.kind) {
            // these all have a payload
            .None, .Identifier, .Integer, .Float, .String, .Boolean, .Operator => null,

            // these are constant.
            .Equals => "=",
            .ColonEquals => ":=",
            .SemiColon => ";",
            .Colon => ":",
            .ColonColon => "::",
            .FatArrow => "=>",
            .Comma => ",",
            .Dot => ".",
            .LeftParen => "(",
            .RightParen => ")",
            .LeftBrace => "{",
            .RightBrace => "}",
            .LeftBracket => "[",
            .RightBracket => "]",

            .TypeKeyword => "type",
            .ClassKeyword => "class",
            .InstanceKeyword => "instance",
            .CaseKeyword => "case",
            .LetKeyword => "let",
            .InKeyword => "in",
            .IfKeyword => "if",
            .ThenKeyword => "then",
            .ElseKeyword => "else",
            .ElseIfKeyword => "elseif",
            .DefKeyword => "def",
            .ConstKeyword => "const",
            .WhereKeyword => "where",
            .DoKeyword => "do",
        };
    }
};

pub const Lexer = struct {
    const Self = @This();

    src: [:0]const u8,
    pos: usize,
    ctok: ?Token,

    pub fn init(src: [:0]const u8) Lexer {
        return Lexer{
            .src = src,
            .index = 0,
        };
    }

    pub const State = enum {
        Start,

        Identifier,
        Integer,
        Float,

        Comment,
    };

    pub fn next(self: *Self) Token {
        if (self.ctok) |token| {
            self.ctok = null;
            return token;
        }

        var result = Token{
            .kind = .None,
            .span = .{
                .start = self.pos,
                .end = undefined,
            },
        };

        while (true) : (self.pos += 1) {
        }
    }
};
