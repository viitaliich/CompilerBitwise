const char* typedef_keyword;
const char* enum_keyword;
const char* struct_keyword;
const char* union_keyword;
const char* var_keyword;
const char* const_keyword;
const char* func_keyword;
const char* sizeof_keyword;
const char* break_keyword;
const char* continue_keyword;
const char* return_keyword;
const char* if_keyword;
const char* else_keyword;
const char* while_keyword;
const char* do_keyword;
const char* for_keyword;
const char* switch_keyword;
const char* case_keyword;
const char* default_keyword;

const char* first_keyword;
const char* last_keyword;
const char** keywords;

#define KEYWORD(name) name##_keyword = str_intern(#name); buf_push(keywords, name##_keyword)	// ##  concatenates the arguments.

#if 0
const char* keyword_if;
const char* keyword_for;
const char* keyword_while;

void init_keywords() {
	keyword_if = str_intern("if");
	keyword_if = str_intern("for");
	keyword_if = str_intern("while");
}
#endif

// Where is it using?
void init_keywords() {
	static bool inited;
	if (inited) {
		return;
	}
	char* arena_end = str_arena.end;
	KEYWORD(typedef);
	KEYWORD(enum);
	KEYWORD(struct);
	KEYWORD(union);
	KEYWORD(const);
	KEYWORD(var);
	KEYWORD(func);
	KEYWORD(sizeof);
	KEYWORD(break);
	KEYWORD(continue);
	KEYWORD(return);
	KEYWORD(if);
	KEYWORD(else);
	KEYWORD(while);
	KEYWORD(do);
	KEYWORD(for);
	KEYWORD(switch);
	KEYWORD(case);
	KEYWORD(default);
	assert(str_arena.end == arena_end);		// ???
	first_keyword = typedef_keyword;
	last_keyword = default_keyword;
	inited = true;
}

#undef KEYWORD

bool is_keyword_str(const char* str) {
	return first_keyword <= str && str <= last_keyword;
}

typedef enum TokenKind {
	TOKEN_EOF = 0,
	// Reserve first 128 values for one-char tokens
	TOKEN_LAST_CHAR = 127,
	TOKEN_KEYWORD,
	TOKEN_INT,
	TOKEN_FLOAT,
	TOKEN_STR,
	TOKEN_NAME,
	TOKEN_LSHIFT,
	TOKEN_RSHIFT,
	TOKEN_EQ,
	TOKEN_NOTEQ,
	TOKEN_LTEQ,
	TOKEN_GTEQ,
	TOKEN_AND,
	TOKEN_OR,
	TOKEN_INC,
	TOKEN_DEC,
	TOKEN_COLON_ASSIGN,
	TOKEN_ADD_ASSIGN,
	TOKEN_FIRST_ASSIGN = TOKEN_ADD_ASSIGN,		// ???
	TOKEN_SUB_ASSIGN,
	TOKEN_OR_ASSIGN,
	TOKEN_AND_ASSIGN,
	TOKEN_XOR_ASSIGN,
	TOKEN_LSHIFT_ASSIGN,
	TOKEN_RSHIFT_ASSIGN,
	TOKEN_MUL_ASSIGN,
	TOKEN_DIV_ASSIGN,
	TOKEN_MOD_ASSIGN,
	TOKEN_LAST_ASSIGN = TOKEN_MOD_ASSIGN,		// ???
} TokenKind;

typedef enum TokenMod {
	TOKENMOD_NONE,		// for mod = 0
	TOKENMOD_HEX,
	TOKENMOD_OCT,
	TOKENMOD_BIN,
	TOKENMOD_CHAR,
}TokenMod;

const char* token_kind_names[] = {
	[TOKEN_EOF] = "EOF",
	[TOKEN_INT] = "int",
	[TOKEN_FLOAT] = "float",
	[TOKEN_STR] = "string",
	[TOKEN_NAME] = "name",
	[TOKEN_LSHIFT] = "<<",
	[TOKEN_RSHIFT] = ">>",
	[TOKEN_EQ] = "==",
	[TOKEN_NOTEQ] = "!=",
	[TOKEN_LTEQ] = "<=",
	[TOKEN_GTEQ] = ">=",
	[TOKEN_AND] = "&&",
	[TOKEN_OR] = "||",
	[TOKEN_INC] = "++",
	[TOKEN_DEC] = "--",
	[TOKEN_COLON_ASSIGN] = ":=",
	[TOKEN_ADD_ASSIGN] = "+=",
	[TOKEN_SUB_ASSIGN] = "-=",
	[TOKEN_OR_ASSIGN] = "|=",
	[TOKEN_AND_ASSIGN] = "&=",
	[TOKEN_XOR_ASSIGN] = "^=",
	[TOKEN_MUL_ASSIGN] = "+=",
	[TOKEN_DIV_ASSIGN] = "/=",
	[TOKEN_MOD_ASSIGN] = "%=",
	[TOKEN_LSHIFT_ASSIGN] = "<<=",
	[TOKEN_RSHIFT_ASSIGN] = ">>=",
};

const char* token_kind_name(TokenKind kind) {
	if (kind < sizeof(token_kind_names) / sizeof(*token_kind_names)) {		// ???
		return token_kind_names[kind];
	}
	else {
		return NULL;
	}
}

size_t copy_token_kind_str(char* dest, size_t dest_size, TokenKind kind) {
	size_t n = 0;
	const char* name = token_kind_name(kind);
	if (name) {
		n = snprintf(dest, dest_size, "%s", name);
	}
	else if (kind < 128 && isprint(kind)) {
		n = snprintf(dest, dest_size, "%c", kind);
	}
	else {
		n = snprintf(dest, dest_size, "<ASCII %d>", kind);
	}
	return n;
}

#if 0
size_t copy_token_kind_str(char* dest, size_t dest_size, TokenKind kind) {
	size_t n = 0;
	switch (kind) {
	case 0:
		n = snprintf(dest, dest_size, "end of file");
		break;
	case TOKEN_INT:
		n = snprintf(dest, dest_size, "integer");
		break;
	case TOKEN_FLOAT:
		n = snprintf(dest, dest_size, "float");
		break;
	case TOKEN_NAME:
		n = snprintf(dest, dest_size, "name");
		break;
	default:
		if (kind < 128 && isprint(kind)) {
			n = snprintf(dest, dest_size, "%c", kind);
		}
		else {
			n = snprintf(dest, dest_size, "<ASCII %d>", kind);
		}
		break;
	}
	return n;
}
#endif

// Warning: This returns a pointer to a static internal buffer, so the next call will overwrite it.
const char* temp_token_kind_str(TokenKind kind) {
	static char buf[256];
	size_t n = copy_token_kind_str(buf, sizeof(buf), kind);
	assert(n + 1 <= sizeof(buf));
	return buf;
}

typedef struct Token {
	TokenKind kind;
	TokenMod mod;
	const char* start;
	const char* end;
	union {
		uint64_t int_val;
		double float_val;
		const char* str_val;
		const char* name;
	};
} Token;

Token token;	// global token, corresponds to the current token.
const char* stream;

uint8_t char_to_digit[256] = {
	['0'] = 0,
	['1'] = 1,
	['2'] = 2,
	['3'] = 3,
	['4'] = 4,
	['5'] = 5,
	['6'] = 6,
	['7'] = 7,
	['8'] = 8,
	['9'] = 9,
	['a'] = 10,['A'] = 10,
	['b'] = 11,['B'] = 11,
	['c'] = 12,['C'] = 12,
	['d'] = 13,['D'] = 13,
	['e'] = 14,['E'] = 14,
	['f'] = 15,['F'] = 15,

};
// doing manual float parsing with proper rounding is really hard.
// only decimal floats, no hex floats etc.

void scan_int() {
	uint64_t base = 10;		// why uint64_t ???		uint8_t
	if (*stream == '0') {
		stream++;
		if (tolower(*stream) == 'x') {
			stream++;
			token.mod = TOKENMOD_HEX;
			base = 16;
		}
		else if (tolower(*stream) == 'b') {
			stream++;
			token.mod = TOKENMOD_BIN;
			base = 2;
		}
		// integer digit preceded by 0 is interpreted as an octal number (different in python)		???
		else if (isdigit(*stream)) {
			token.mod = TOKENMOD_OCT;
			base = 8;		// tell in labs about this method
		}
	}
	uint64_t val = 0;
	for (;;) {
		uint64_t digit = char_to_digit[*stream];		// ???
		if (digit == 0 && *stream != '0') {
			break;
		}
		if (digit >= base) {
			syntax_error("Digit '%c' out of range for base %" PRIu64, *stream, base);	// %llu  // ???
			digit = 0;
		}
		if (val > (UINT64_MAX - digit) / base) {
			syntax_error("Integer literal overflow");
			while (isdigit(*stream)) {
				stream++;
			}
			val = 0;
			break;
		}
		val = val * base + digit;	// shifts everything when you see a new digit
		stream++;
	}
	token.kind = TOKEN_INT;
	token.int_val = val;
}

void scan_float() {
	const char* start = stream;
	while (isdigit(*stream)) {
		stream++;
	}
	if (*stream == '.') {
		stream++;
	}
	while (isdigit(*stream)) {
		stream++;
	}
	if (tolower(*stream) == 'e') {
		stream++;
		if (*stream == '+' || *stream == '-') {		// don't need + for positive power ???
			stream++;
		}
		if (!isdigit(*stream)) {
			syntax_error("Expected digit after float literal exponent, found '%c'.", *stream);
		}
		while (isdigit(*stream)) {
			stream++;
		}
	}
	const char* end = stream;
	double val = strtod(start, NULL);
	if (val == HUGE_VAL || val == -HUGE_VAL) {
		syntax_error("Float literal overflow");
	}
	token.float_val = val;
	token.kind = TOKEN_FLOAT;
}

char escape_to_char[256] = {		// ???
	['n'] = '\n',
	['r'] = '\r',
	['t'] = '\t',
	['v'] = '\v',
	['b'] = '\b',
	['a'] = '\a',
	['0'] = 0,
};

void scan_char() {
	assert(*stream == '\'');		// start with single quote
	stream++;

	char val = 0;
	if (*stream == '\'') {
		syntax_error("Char literal cannot be empty");		// why not ???
		stream++;
	}
	else if (*stream == '\n') {						// use for comments ???
		syntax_error("Char literal cannot contain newline");			// ???
	}
	else if (*stream == '\\') {
		stream++;
		val = escape_to_char[*stream];
		if (val == 0 && *stream != '0') {
			syntax_error("Invalid char literal escape '\\%c'.", *stream);
		}
		stream++;
	}
	else {
		val = *stream;
		stream++;
	}
	if (*stream != '\'') {
		syntax_error("Expected closing char quote, got '%c'", *stream);
	}
	else {
		stream++;
	}
	token.kind = TOKEN_INT;
	token.int_val = val;
	token.mod = TOKENMOD_CHAR;
}

void scan_str() {
	assert(*stream == '"');
	stream++;
	char* str = NULL;
	while (*stream && *stream != '"') {
		char val = *stream;
		if (val == '\n') {
			syntax_error("String literal cannot contain newline");
			break;
		}
		else if (val == '\\') {
			stream++;
			val = escape_to_char[*stream];
			if (val == 0 && *stream != '0') {
				syntax_error("Invalid string literal escape '\\%c'", *stream);
			}
		}
		buf_push(str, val);
		stream++;
	}
	if (*stream) {
		assert(*stream == '"');
		stream++;
	}
	else {
		syntax_error("Unexpected end of file within string literal");
	}
	buf_push(str, 0);		// why not \0 ???
	token.kind = TOKEN_STR;
	token.str_val = str;
}

// These macros are very localised and not really reusable. 
// They are intrnded for a very specific purpose. 
// So they are pretty straightforward in a sence of syntax. 
//
// This is pretty common technique
#define CASE1(c, c1, k1) \
    case c: \
        token.kind = *stream++; \
        if (*stream == c1) { \
            token.kind = k1; \
            stream++; \
        } \
        break;

#define CASE2(c, c1, k1, c2, k2) \
    case c: \
        token.kind = *stream++; \
        if (*stream == c1) { \
            token.kind = k1; \
            stream++; \
        } else if (*stream == c2) { \
            token.kind = k2; \
            stream++; \
        } \
        break;

void next_token() {
repeat:								// that's not good ???
	token.start = stream;
	token.mod = 0;
	switch (*stream) {
		// could be (but not recomended!) if(isdigit(*stream)...)
	case ' ': case '\n': case '\r': case '\t': case '\v':
		while (isspace(*stream)) {
			stream++;
		}
		goto repeat;
		break;
	case '\'':
		scan_char();
		break;
	case '"':
		scan_str();
		break;
	case '.':		// float numbers can start with point			// ???
		scan_float();
		break;
	case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9': {
		while (isdigit(*stream)) {
			stream++;
		}
		char c = *stream;
		stream = token.start;
		if (c == '.' || tolower(c) == 'e') {
			scan_float();
		}
		else {
			scan_int();
		}
		break;
	}
	case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g': case 'h': case 'i': case 'j':
	case 'k': case 'l': case 'm': case 'n': case 'o': case 'p': case 'q': case 'r': case 's': case 't':
	case 'u': case 'v': case 'w': case 'x': case 'y': case 'z':
	case 'A': case 'B': case 'C': case 'D': case 'E': case 'F': case 'G': case 'H': case 'I': case 'J':
	case 'K': case 'L': case 'M': case 'N': case 'O': case 'P': case 'Q': case 'R': case 'S': case 'T':
	case 'U': case 'V': case 'W': case 'X': case 'Y': case 'Z':
	case '_': {
		while (isalnum(*stream) || *stream == '_') {
			stream++;
		}
		token.name = str_intern_range(token.start, stream);
		token.kind = is_keyword_str(token.name) ? TOKEN_KEYWORD : TOKEN_NAME;
		break;
	}
	case '<':
		token.kind = *stream++;
		if (*stream == '<') {
			token.kind = TOKEN_LSHIFT;
			stream++;
			if (*stream == '=') {
				token.kind = TOKEN_LSHIFT_ASSIGN;
				stream++;
			}
		}
		else if (*stream == '=') {
			token.kind = TOKEN_LTEQ;
			stream++;
		}
		break;
	case '>':
		token.kind = *stream++;
		if (*stream == '>') {
			token.kind = TOKEN_RSHIFT;
			stream++;
			if (*stream == '=') {
				token.kind = TOKEN_RSHIFT_ASSIGN;
				stream++;
			}
		}
		else if (*stream == '=') {
			token.kind = TOKEN_GTEQ;
			stream++;
		}
		break;

		CASE1('^', '=', TOKEN_XOR_ASSIGN)		// simple XOR ???
		CASE1(':', '=', TOKEN_COLON_ASSIGN)
		CASE1('*', '=', TOKEN_MUL_ASSIGN)
		CASE1('/', '=', TOKEN_DIV_ASSIGN)
		CASE1('%', '=', TOKEN_MOD_ASSIGN)
		CASE2('+', '=', TOKEN_ADD_ASSIGN, '+', TOKEN_INC)
		CASE2('-', '=', TOKEN_SUB_ASSIGN, '-', TOKEN_DEC)	// no differrence between unary and binary minus at lex time, its parse time job
		CASE2('&', '=', TOKEN_AND_ASSIGN, '&', TOKEN_AND)
		CASE2('|', '=', TOKEN_OR_ASSIGN, '|', TOKEN_OR)

	default:
		token.kind = *stream++;
		break;
	}
	token.end = stream;
}

#undef CASE1
#undef CASE2

void init_stream(const char* str) {
	stream = str;
	next_token();
}

bool is_token(TokenKind kind) {
	return token.kind == kind;
}


bool is_token_eof() {
	return token.kind == TOKEN_EOF;
}

bool is_token_name(const char* name) {
	return token.kind == TOKEN_NAME && token.name == name;
}

bool is_keyword(const char* name) {
	return is_token(TOKEN_KEYWORD) && token.name == name;
}

bool match_keyword(const char* name) {
	if (is_keyword(name)) {
		next_token();
		return true;
	}
	else {
		return false;
	}
}


bool match_token(TokenKind kind) {
	if (is_token(kind)) {
		next_token();
		return true;
	}
	else {
		return false;
	}
}

bool expect_token(TokenKind kind) {
	if (is_token(kind)) {
		next_token();
		return true;
	}
	else {
		char buf[256];
		copy_token_kind_str(buf, sizeof(buf), kind);
		fatal("expected token %s, got %s", buf, token_kind_str(token.kind));
		return false;
	}
}

void keyword_test() {
	init_keywords();
	assert(is_keyword_str(first_keyword));
	assert(is_keyword_str(last_keyword));
	for (const char** it = keywords; it != buf_end(keywords); it++) {
		assert(is_keyword_str(*it));
	}
	assert(!is_keyword_str(str_intern("foo")));
}

#define assert_token(x) assert(match_token(x))
#define assert_token_name(x) assert(token.name == str_intern(x) && match_token(TOKEN_NAME))
#define assert_token_int(x) assert(token.int_val == (x) && match_token(TOKEN_INT))
#define assert_token_float(x) assert(token.float_val == (x) && match_token(TOKEN_FLOAT))
#define assert_token_str(x) assert(strcmp(token.str_val, (x)) == 0 && match_token(TOKEN_STR))
#define assert_token_eof() assert(is_token(0))


void lex_test() {
	// Integer literal tests
	// Make sure UINT64_MAX doesn't trigger overflow
	init_stream("0 18446744073709551615 0xffffffffffffffff 042 0b11");
	assert_token_int(0);
	assert_token_int(18446744073709551615);
	assert(token.mod == TOKENMOD_HEX);
	assert_token_int(0xffffffffffffffff);
	assert(token.mod == TOKENMOD_OCT);
	assert_token_int(042);
	assert(token.mod == TOKENMOD_BIN);
	assert_token_int(0x3);		// 0x3 == 0b11		// that's bin
	// Make sure INT_MAX doesn't trigger overflow
	init_stream("2147483647");
	assert_token_int(2147483647);
	assert_token_eof();

	// Operator tests
	init_stream(": := + += ++ < <= << <<=");
	assert_token(':');
	assert_token(TOKEN_COLON_ASSIGN);
	assert_token('+');
	assert_token(TOKEN_ADD_ASSIGN);
	assert_token(TOKEN_INC);
	assert_token('<');
	assert_token(TOKEN_LTEQ);
	assert_token(TOKEN_LSHIFT);
	assert_token(TOKEN_LSHIFT_ASSIGN);
	assert_token_eof();

	// Float literal tests
	init_stream("3.14 .123 42. 3e10");
	assert_token_float(3.14);
	assert_token_float(.123);
	assert_token_float(42.);
	assert_token_float(3e10);
	assert_token_eof();

	// Char literal tests
	init_stream("'a' '\\n'");
	assert_token_int('a');
	assert_token_int('\n');
	assert_token_eof();

	// String literal tests
	init_stream("\"foo\" \"a\\nb\"");
	assert_token_str("foo");
	assert_token_str("a\nb");
	assert_token_eof();


	// Misc tests
	init_stream("XY+(XY)_HELLO1,234+994");
	assert_token_name("XY");
	assert_token('+');
	assert_token('(');
	assert_token_name("XY");
	assert_token(')');
	assert_token_name("_HELLO1");
	assert_token(',');
	assert_token_int(234);
	assert_token('+');
	assert_token_int(994);
	assert_token_eof();


}

// Expression grammar eith left recursion in order to get left asosiativity
// doesn't let you parse predictively easily with no preprocessing or backstrackin or so on
// ??? BUT I'M NOT SURE

#undef assert_token_eof
#undef assert_token_int
#undef assert_token_name
#undef assert_token

#if 0		// for commenting
expr3 = INT | '(' expr ')'
expr2 = '-' expr2 | expr3		// unary minus
expr1 = expr2([*/ ] expr2) *
expr0 = expr1([+-] expr1) *		// * means repetition
expr = expr0
#endif

#if 0
int parse_expr();

int parse_expr3() {
	if (is_token(TOKEN_INT)) {
		// ...	calculations
		int val = token.int_val;
		next_token();
		return val;
	}
	else if (match_token('(')) {
		int val = parse_expr();
		expect_token(')');
		return val;
	}
	else {
		fatal("expected integer or (, got %s", token_kind_str(token.kind));
		return 0;
	}
}

int parse_expr2() {
	if (match_token('-')) {
		return -parse_expr2();
		// ...	calculations
	}
	else {
		return parse_expr3();
	}
}

int parse_expr1() {
	int val = parse_expr2();
	while (is_token('*') || is_token('/')) {
		char op = token.kind;
		next_token();
		int rval = parse_expr2();
		if (op == '*') {
			val *= rval;
		}
		else {
			assert(op == '/');
			assert(rval != 0);
			val /= rval;
		}
	}
	return val;
}

int parse_expr0() {
	int val = parse_expr1();
	while (is_token('+') || is_token('-')) {
		char op = token.kind;
		next_token();
		int rval = parse_expr1();
		if (op == '+') {
			val += rval;
		}
		else {
			assert(op == '-');
			val -= rval;
		}
	}
	return val;
}

int parse_expr() {
	return parse_expr0();
}

int parse_expr_str(const char* str) {
	init_stream(str);
	return parse_expr();

}

//#define TEST_EXPR(x) assert(parse_expr_str(#x) == (x))	// # - Stringizing operator
#define assert_expr(x) assert(parse_expr_str(#x) == (x))

// Like calculator.		Interpreter
void parse_test() {
	assert_expr(1);
	assert_expr((1));
	assert_expr(-+1);
	assert_expr(1 - 2 - 3);
	assert_expr(2 * 3 + 4 * 5);
	assert_expr(2 * (3 + 4) * 5);
	assert_expr(2 + -3);
}
#undef assert_expr



// LL(1) is for token look ahead, not character look ahead 
//
// One of the good reasons for have a separation between lexing and parsing 
// is that you can have different techniques on these different stages
//
// You can do int and float scanners as a one whole thing
//
// Parsing may be easier, if you could believe it. 
// If you choose your grammar right, you can make parsing more mechanical and less kind of case analisys heavy than lexing. 
//
// Write code incrementally
//

#endif