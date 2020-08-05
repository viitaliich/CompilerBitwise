typedef enum TypeKind {
	TYPE_INT,
	TYPE_FLOAT,
	TYPE_PTR,
	TYPE_ARRAY,
	TYPE_STRUCT,
	TYPE_UNION,
	TYPE_FUNC,
} TypeKind;

typedef struct Type Type;		// not like a typespec in a syntactic sence, but an actual semantic type.

typedef struct TypeField {
	const char* name;			// why is it name here ???		why is it here?
	Type* type;
} TypeField;

struct Type {
	TypeKind kind;
	union {
		struct {
			Type* base;			// why is it pointer here?
		} ptr;
		struct {
			Type* base;
			size_t size;
		} array;
		struct {
			TypeField* fields;		// buffer but not buffer		// It could be Type** fields;		why we need 'name' ?
			size_t num_fields;
		} aggregate;
		struct {
			Type** params;		// buffer but not buffer
			size_t num_params;
			Type* ret;
		} func;
	};
};

Type* type_alloc(TypeKind kind) {
	Type* t = xcalloc(1, sizeof(Type));
	t->kind = kind;
	return t;
}

#if 0
Type* t_int(TypeKind kind) {
	Type* t = type_alloc(kind);
	t->kind = kind;
	return t;
}
#endif

Type type_int_val = { TYPE_INT };
Type type_float_val = { TYPE_FLOAT };

Type* type_int = &type_int_val;
Type* type_float = &type_float_val;

typedef struct CachedPtrType {
	Type* base;		// there is for convenience (for not this 'it->ptr->ptr.base == base' in a few lines below)
	Type* ptr;
} CachedPtrType;

CachedPtrType* cached_ptr_types;		// buf

Type* type_ptr(Type* base) {
	for (CachedPtrType* it = cached_ptr_types; it != buf_end(cached_ptr_types); it++) {
		if (it->base == base) {
			return it->ptr;
		}
	}
	Type* t = type_alloc(TYPE_PTR);
	t->ptr.base = base;
	buf_push(cached_ptr_types, (CachedPtrType) { base, t });
	return t;
}

typedef struct CachedArrayType {
	Type* base;
	size_t size;
	Type* array;
} CachedArrayType;

CachedArrayType* cached_array_types;

Type* type_array(Type* base, size_t size) {
	for (CachedArrayType* it = cached_array_types; it != buf_end(cached_array_types); it++) {
		if (it->base == base && it->size == size) {
			return it->array;
		}
	}
	Type* t = type_alloc(TYPE_ARRAY);
	t->array.base = base;
	t->array.size = size;
	buf_push(cached_array_types, (CachedArrayType) { base, size, t });
	return t;
}

typedef struct CachedFuncType {
	Type** params;
	size_t num_params;
	Type* ret;
	Type* func;
} CachedFuncType;

CachedFuncType* cached_func_types;

Type* type_func(Type** params, size_t num_params, Type* ret) {
	for (CachedFuncType* it = cached_func_types; it != buf_end(cached_func_types); it++) {
		if (it->num_params == num_params && it->ret == ret) {
			bool match = true;
			for (size_t i = 0; i < num_params; i++) {
				if (it->params[i] != params[i]) {
					match = false;
					break;
				}
			}
			if (match) {
				return it->func;
			}
		}
	}
	Type* t = type_alloc(TYPE_FUNC);
	t->func.params = xcalloc(num_params, sizeof(Type*));
	memcpy(t->func.params, params, num_params * sizeof(Type*));
	t->func.num_params = num_params;
	t->func.ret = ret;
	buf_push(cached_func_types, (CachedFuncType) { params, num_params, ret, t });
	return t;
}

Type* type_struct(TypeField* fields, size_t num_fields) {
	Type* t = type_alloc(TYPE_STRUCT);
	t->aggregate.fields = xcalloc(num_fields, sizeof(TypeField));
	memcpy(t->aggregate.fields, fields, num_fields * sizeof(TypeField));
	t->aggregate.num_fields = num_fields;
	return t;
}

Type* type_union(TypeField* fields, size_t num_fields) {
	Type* t = type_alloc(TYPE_UNION);
	t->aggregate.fields = xcalloc(num_fields, sizeof(TypeField));
	memcpy(t->aggregate.fields, fields, num_fields * sizeof(TypeField));
	t->aggregate.num_fields = num_fields;
	return t;
}

typedef struct ConstEntity {
	Type* type;
	union {
		uint64_t int_val;
		double float_val;
	};
} ConstEntity;

// ???
typedef struct Entity {
	int foo;
} Entity;

typedef enum SymState {
	SYM_UNORDERED,
	SYM_ORDERING, 
	SYM_ORDERED,
} SymState;

typedef struct Sym {		
	const char* name;		// declaration has a name...
	Decl* decl;				// ... what is this name for ? This is for future hash-table
	SymState state;
} Sym;

Sym* global_syms;		// symbol table

Sym* sym_get(const char* name) {
	for (Sym* it = global_syms; it != buf_end(global_syms); it++) {
		if (it->name == name) {
			return it;
		}
	}
	return NULL;
}

void sym_builtin(const char* name) {
	buf_push(global_syms, (Sym) { name, NULL, SYM_ORDERED });
}

void sym_decl(Decl* decl) {
	assert(decl->name);
	// good check on the same variable declarations (int a = 5, float a = 3.0)
	assert(!sym_get(decl->name));		// assert will terminate the program if its argument turns out to be false
	buf_push(global_syms, (Sym) { decl->name, decl, SYM_UNORDERED });		
}

// Kind of a graph walk.
// We are not trying produce any complicated result as we walk,
// but walking in the graph and trying to detect cycles. 
// There is flag assosiated with names of the symbol table that tell us
// whether we've already ordered (or tryed to do so) smth or we are in a middle of ordering.

Decl** ordered_decls = NULL;

void order_decl(Decl* decl);
void order_typespec(Decl* types);

void order_name(const char* name) {
	Sym* sym = sym_get(name);
	if (!sym) {
		fatal("Non-existent name '%s'", name);
		return;
	}
	if (sym->state == SYM_ORDERED) {
		return;
	}
	if (sym->state == SYM_ORDERING) {
		fatal("Cyclic dependency");
		return;
	}
	sym->state = SYM_ORDERING;
	order_decl(sym->decl);
	sym->state = SYM_ORDERED;
	buf_push(ordered_decls, sym->decl);		// once we ordered smth, we have to put it in a list
}

void order_expr(Expr* expr) {
	switch (expr->kind) {
	case EXPR_INT:
	case EXPR_FLOAT:
	case EXPR_STR:
		// do nothing
		break;
	case EXPR_NAME:
		order_name(expr->name);
		break;
	case EXPR_CAST:
		order_typespec(expr->cast.type);
		order_expr(expr->cast.expr);
		break;
	case EXPR_CALL:
		order_expr(expr->call.expr);
		for (size_t i = 0; i < expr->call.num_args; i++) {
			order_expr(expr->call.args[i]);
		}
		break;
	case EXPR_INDEX:
		order_expr(expr->index.expr);
		order_expr(expr->index.index);
		break;
	case EXPR_FIELD:
		order_expr(expr->field.expr);
		break;
	case EXPR_COMPOUND:
		if (expr->compound.type) {
			order_typespec(expr->compound.type);
		}
		for (size_t i = 0; i < expr->compound.num_args; i++) {
			order_expr(expr->compound.args[i]);
		}
		break;
	case EXPR_UNARY:
		order_expr(expr->unary.expr);
		break;
	case EXPR_BINARY:
		order_expr(expr->binary.left);
		order_expr(expr->binary.right);
		break;
	case EXPR_TERNARY:
		order_expr(expr->ternary.cond);
		order_expr(expr->ternary.then_expr);
		order_expr(expr->ternary.else_expr);
		break;
	case EXPR_SIZEOF_EXPR:
		order_expr(expr->sizeof_expr);
		break;
	case EXPR_SIZEOF_TYPE:
		order_typespec(expr->sizeof_type);
		break;
	default:
		assert(0);
		break;
	}
}

void order_typespec(Typespec* typespec) {
	switch (typespec->kind) {
	case TYPESPEC_NAME:
		order_name(typespec->name);
		break;
	case TYPESPEC_FUNC:
		for (size_t i = 0; i < typespec->func.num_args; i++) {
			order_typespec(typespec->func.args[i]);
		}
		order_typespec(typespec->func.ret);
		break;
	case TYPESPEC_ARRAY:
		order_typespec(typespec->array.elem);
		order_expr(typespec->array.size);
		break;
	case TYPESPEC_PTR:
		// TODO: Think about forward declaration, etc
		break;
	default:
		assert(0);
		break;
	}
}

void order_decl(Decl* decl) {
	switch (decl->kind) {
	case DECL_STRUCT:
	case DECL_UNION:
		for (size_t i = 0; i < decl->aggregate.num_items; i++) {
			order_typespec(decl->aggregate.items[i].type);
		}
		break;		
	case DECL_VAR:
		order_typespec(decl->var.type);		
		order_expr(decl->var.expr);
		break;
	case DECL_CONST:
		order_expr(decl->const_decl.expr);
		break;
	case DECL_TYPEDEF:
		order_typespec(decl->typedef_decl.type);
		break;
	case DECL_FUNC:
		// Don't need handle here
		break;
	default:
		assert(0);
		break;
	}
}

void order_decls(void) {
	for (size_t i = 0; i < buf_len(global_syms); i++) {
		order_name(global_syms[i].name);
	}
}

void resolve_test(void) {
	const char* foo = str_intern("foo");
	assert(sym_get(foo) == NULL);
	Decl* decl = decl_const(foo, expr_int(42));
	sym_decl(decl);
	Sym* sym = sym_get(foo);
	assert(sym && sym->decl == decl);		// ???

	Type* int_ptr = type_ptr(type_int);
	assert(type_ptr(type_int) == int_ptr);
	Type* float_ptr = type_ptr(type_float);
	assert(type_ptr(type_float) == float_ptr);
	assert(int_ptr != float_ptr);
	Type* int_ptr_ptr = type_ptr(type_ptr(type_int));
	assert(type_ptr(type_ptr(type_int)) == int_ptr_ptr);
	Type* float4_array = type_array(type_float, 4);
	assert(type_array(type_float, 4) == float4_array);
	Type* float3_array = type_array(type_float, 3);
	assert(type_array(type_float, 3) == float3_array);
	assert(float4_array != float3_array);
	Type* int_int_func = type_func(&type_int, 1, type_int);
	assert(type_func(&type_int, 1, type_int) == int_int_func);
	Type* int_func = type_func(NULL, 0, type_int);
	assert(int_int_func != int_func);
	assert(int_func == type_func(NULL, 0, type_int));
}

void order_test(void) {
	const char* code_decls[] = {
		"const a = b",
		"const b = 1",

		//"struct S { t: T; }",
		//"struct T { i: int; }",

		//"struct E { f: F; }",
		//"struct F { i: int[n]; }",
		//"const n = 1024;"
	};
	sym_builtin(str_intern("int"));		// ???		why?
	for (size_t i = 0; i < sizeof(code_decls) / sizeof(*code_decls); i++) {
		init_stream(code_decls[i]);		// init_stream = token analisys		// how does it work ???
		Decl* decl = parse_decl();
		sym_decl(decl);
	}
	
	order_decls();
	for (size_t i = 0; i < buf_len(ordered_decls); i++) {
		printf("%s\n", ordered_decls[i]->name);
	}
}

// We need a way to construct some sort of representations of types in a type system. 
// It'll also be used in code generator backend and so on.
// In str_intern we had the same idea: we take a value we interned 
// and then we expect for the same value to always get the same result back which is a canonical represent of that value. 
// Here we can do exact thing and check poiners for equality. 

// Caching here is like string interning. 
// Make sure by checking the cache first that you never create a new instance of some thing that you already have and match for in the cache.
// Structs and unions are always resolved in a new instance.

// Hash consing

// Day 6 - time 1:05:00
