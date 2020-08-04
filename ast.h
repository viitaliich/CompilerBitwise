typedef struct Expr Expr;
typedef struct Stmt Stmt;
typedef struct Decl Decl;
typedef struct Typespec Typespec;

typedef struct StmtList {
	Stmt** stmts;
	size_t num_stmts;
} StmtList;

typedef enum TypespecKind {
	TYPESPEC_NONE,
	TYPESPEC_NAME,
	TYPESPEC_FUNC,
	TYPESPEC_ARRAY,
	TYPESPEC_PTR,
} TypespecKind;

struct Typespec {
	TypespecKind kind;
	union {
		const char* name;
		struct {
			Typespec** args;
			size_t num_args;
			Typespec* ret;
		} func;
		struct {
			Typespec* elem;
			Expr* size;
		} array;
		struct {
			Typespec* elem;
		} ptr;
	};
};

typedef enum DeclKind {
	DECL_NONE,
	DECL_ENUM,
	DECL_STRUCT,
	DECL_UNION,
	DECL_VAR,
	DECL_CONST,
	DECL_TYPEDEF,
	DECL_FUNC,
} DeclKind;

typedef struct FuncParam {
	const char* name;
	Typespec* type;
} FuncParam;

typedef struct AggregateItem {
	const char** names;
	size_t num_names;
	Typespec* type;		// ???
} AggregateItem;

typedef struct EnumItem {
	const char* name;
	Expr* init;
} EnumItem;

struct Decl {
	DeclKind kind;
	const char* name;
	union {
		struct {
			EnumItem* items;
			size_t num_items;
		} enum_decl;
		struct {
			AggregateItem* items;
			size_t num_items;
		} aggregate;
		struct {
			FuncParam* params;
			size_t num_params;
			Typespec* ret_type;
			StmtList block;
		} func;
		struct {
			Typespec* type;
		} typedef_decl;
		struct {
			Typespec* type;
			Expr* expr;
		} var;
		struct {
			Expr* expr;
		} const_decl;
	};
};

typedef enum ExprKind {
	EXPR_NONE,
	EXPR_INT,
	EXPR_FLOAT,
	EXPR_STR,
	EXPR_NAME,
	EXPR_CAST,
	EXPR_CALL,
	EXPR_INDEX,
	EXPR_FIELD,
	EXPR_COMPOUND,
	EXPR_UNARY,
	EXPR_BINARY,
	EXPR_TERNARY,
	EXPR_SIZEOF_EXPR,
	EXPR_SIZEOF_TYPE,
} ExprKind;

#if 0
typedef struct CompoundExpr {
	Typespec* type;
	Expr** args;
	size_t num_args;
} CompoundExpr;

typedef struct CastExpr {
	Typespec* type;
	Expr* expr;
} CastExpr;

typedef struct UnaryExpr {
	TokenKind op;
	Expr* expr;
} UnaryExpr;

typedef struct BinaryExpr {
	TokenKind op;
	Expr* left;
	Expr* right;
} BinaryExpr;

typedef struct TernaryExpr {
	Expr* cond;
	Expr* then_expr;
	Expr* else_expr;
} TernaryExpr;

typedef struct CallExpr {
	Expr* expr;
	Expr** args;
	size_t num_args;
} CallExpr;

typedef struct IndexExpr {
	Expr* expr;
	Expr* index;
} IndexExpr;

typedef struct FieldExpr {
	Expr* expr;
	const char* name;
} FieldExpr;
#endif

struct Expr {
	ExprKind kind;
	union {
		int64_t int_val;
		double float_val;
		const char* str_val;
		const char* name;
		Expr* sizeof_expr;
		Typespec* sizeof_type;
#if 0
		CompoundExpr compound;
		CastExpr cast;
		UnaryExpr unary;
		BinaryExpr binary;
		TernaryExpr ternary;
		CallExpr call;
		IndexExpr index;
		FieldExpr field;
#endif
		struct {
			Typespec* type;
			Expr** args;
			size_t num_args;
		} compound;
		struct {
			Typespec* type;
			Expr* expr;
		} cast;
		struct {
			TokenKind op;
			Expr* expr;
		} unary;
		struct {
			TokenKind op;
			Expr* left;
			Expr* right;
		} binary;
		struct {
			Expr* cond;
			Expr* then_expr;
			Expr* else_expr;
		} ternary;
		struct {
			Expr* expr;
			Expr** args;
			size_t num_args;
		} call;
		struct {
			Expr* expr;
			Expr* index;
		} index;
		struct {
			Expr* expr;
			const char* name;
		} field;
	};
};

typedef enum StmtKind {
	STMT_NONE,
	STMT_DECL,
	STMT_RETURN,
	STMT_BREAK,
	STMT_CONTINUE,
	STMT_BLOCK,
	STMT_IF,
	STMT_WHILE,
	STMT_DO_WHILE,
	STMT_FOR,
	STMT_SWITCH,
	STMT_ASSIGN,
	STMT_INIT,
	STMT_EXPR,
} StmtKind;

typedef struct ElseIf {
	Expr* cond;
	StmtList block;
} ElseIf;

typedef struct SwitchCase {
	Expr** exprs;
	size_t num_exprs;
	bool is_default;
	StmtList block;
} SwitchCase;

struct Stmt {
	StmtKind kind;
	union {
		Expr* expr;
		Decl* decl;
		struct {
			Expr* cond;
			StmtList then_block;
			ElseIf* elseifs;
			size_t num_elseifs;
			StmtList else_block;
		} if_stmt;
		struct {
			Expr* cond;
			StmtList block;
		} while_stmt;
		struct {
			Stmt* init;
			Expr* cond;
			Stmt* next;
			StmtList block;
		} for_stmt;
		struct {
			Expr* expr;
			SwitchCase* cases;
			size_t num_cases;
		} switch_stmt;
		StmtList block;
		struct {
			TokenKind op;
			Expr* left;
			Expr* right;
		} assign;
		struct {
			const char* name;
			Expr* expr;
		} init;
	};
};
