// Type Checking/Inference

typedef enum TypeKind {
	TYPE_NONE,
	TYPE_INCOMPLETE,
	TYPE_COMPLETING,
	TYPE_INT,
	TYPE_FLOAT,
	TYPE_PTR,
	TYPE_ARRAY,
	TYPE_STRUCT,
	TYPE_UNION,
	TYPE_ENUM,
	TYPE_FUNC,
} TypeKind;

typedef struct Type Type;
typedef struct Entity Entity;

typedef struct TypeField {
	const char* name;
	Type* type;
} TypeField;

struct Type {
	TypeKind kind;
	size_t size;		
	Entity* entity;
	union {
		struct {
			Type* elem;		// type we point to
		} ptr;
		struct {
			Type* elem;
			size_t size;
		} array;
		struct {
			TypeField* fields;
			size_t num_fields;
		} aggregate;
		struct {
			Type** params;
			size_t num_params;
			Type* ret;
		} func;
	};
};

void complete_type(Type* type);

Type* type_alloc(TypeKind kind) {
	Type* type = xcalloc(1, sizeof(Type));
	type->kind = kind;
	return type;
}

Type type_int_val = { TYPE_INT, 4 };		
Type type_float_val = { TYPE_FLOAT, 4 };	// why 4 ???

Type* type_int = &type_int_val;
Type* type_float = &type_float_val;
const size_t PTR_SIZE = 8;		

size_t type_sizeof(Type* type) {
	assert(type->kind > TYPE_COMPLETING);
	assert(type->size != 0);
	return type->size;
}

typedef struct CachedPtrType {
	Type* elem;		// address of elemnt point to
	Type* ptr;		// address of pointer which points
} CachedPtrType;

CachedPtrType* cached_ptr_types;

Type* type_ptr(Type* elem) {
	for (CachedPtrType* it = cached_ptr_types; it != buf_end(cached_ptr_types); it++) {
		if (it->elem == elem) {
			return it->ptr;
		}
	}
	Type* type = type_alloc(TYPE_PTR);
	type->size = PTR_SIZE;
	type->ptr.elem = elem;
	buf_push(cached_ptr_types, (CachedPtrType) { elem, type });
	return type;
}

typedef struct CachedArrayType {
	Type* elem;
	size_t size;
	Type* array;
} CachedArrayType;

CachedArrayType* cached_array_types;

Type* type_array(Type* elem, size_t size) {
	for (CachedArrayType* it = cached_array_types; it != buf_end(cached_array_types); it++) {
		if (it->elem == elem && it->size == size) {
			return it->array;
		}
	}
	complete_type(elem);
	Type* type = type_alloc(TYPE_ARRAY);
	type->size = size * type_sizeof(elem);
	type->array.elem = elem;
	type->array.size = size;
	buf_push(cached_array_types, (CachedArrayType) { elem, size, type });
	return type;
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
	Type* type = type_alloc(TYPE_FUNC);
	type->size = PTR_SIZE;		
	type->func.params = memdup(params, num_params * sizeof(*params));
	type->func.num_params = num_params;
	type->func.ret = ret;
	buf_push(cached_func_types, (CachedFuncType) { params, num_params, ret, type });
	return type;
}

void type_complete_struct(Type* type, TypeField* fields, size_t num_fields) {
	assert(type->kind == TYPE_COMPLETING);
	type->kind = TYPE_STRUCT;
	type->size = 0;
	for (TypeField* it = fields; it != fields + num_fields; it++) {
		// TODO: Alignment, etc.
		type->size += type_sizeof(it->type);
	}
	type->aggregate.fields = memdup(fields, num_fields * sizeof(*fields));
	type->aggregate.num_fields = num_fields;
}

void type_complete_union(Type* type, TypeField* fields, size_t num_fields) {
	assert(type->kind == TYPE_COMPLETING);
	type->kind = TYPE_UNION;
	type->size = 0;
	for (TypeField* it = fields; it != fields + num_fields; it++) {
		assert(it->type->kind > TYPE_COMPLETING);
		type->size = MAX(type->size, type_sizeof(it->type));
	}
	type->aggregate.fields = memdup(fields, num_fields * sizeof(*fields));
	type->aggregate.num_fields = num_fields;
}

Type* type_incomplete(Entity* entity) {
	Type* type = type_alloc(TYPE_INCOMPLETE);
	type->entity = entity;
	return type;
}

typedef enum EntityKind {
	ENTITY_NONE,
	ENTITY_VAR,
	ENTITY_CONST,
	ENTITY_FUNC,
	ENTITY_TYPE,
	ENTITY_ENUM_CONST,
} EntityKind;

typedef enum EntityState {
	ENTITY_UNRESOLVED,
	ENTITY_RESOLVING,
	ENTITY_RESOLVED,
} EntityState;

typedef struct Entity {
	const char* name;
	EntityKind kind;
	EntityState state;
	Decl* decl;
	Type* type;
	int64_t val;
} Entity;

Entity** entities;

Entity* entity_new(EntityKind kind, const char* name, Decl* decl) {
	Entity* entity = xcalloc(1, sizeof(Decl));
	entity->kind = kind;
	entity->name = name;
	entity->decl = decl;
	return entity;
}

Entity* entity_decl(Decl* decl) {
	EntityKind kind = ENTITY_NONE;
	switch (decl->kind) {
	case DECL_STRUCT:
	case DECL_UNION:
	case DECL_TYPEDEF:
	case DECL_ENUM:
		kind = ENTITY_TYPE;
		break;
	case DECL_VAR:
		kind = ENTITY_VAR;
		break;
	case DECL_CONST:
		kind = ENTITY_CONST;
		break;
	case DECL_FUNC:
		kind = ENTITY_FUNC;
		break;
	default:
		assert(0);
		break;
	}
	Entity* entity = entity_new(kind, decl->name, decl);
	if (decl->kind == DECL_STRUCT || decl->kind == DECL_UNION) {
		entity->state = ENTITY_RESOLVED;
		entity->type = type_incomplete(entity);
	}
	return entity;
}

Entity* entity_enum_const(const char* name, Decl* decl) {
	return entity_new(ENTITY_ENUM_CONST, name, decl);
}

Entity* entity_get(const char* name) {
	for (Entity** it = entities; it != buf_end(entities); it++) {
		Entity* entity = *it;
		if (entity->name == name) {
			return entity;
		}
	}
	return NULL;
}

Entity* entity_install_decl(Decl* decl) {
	Entity* entity = entity_decl(decl);
	buf_push(entities, entity);
	if (decl->kind == DECL_ENUM) {
		for (size_t i = 0; i < decl->enum_decl.num_items; i++) {
			buf_push(entities, entity_enum_const(decl->enum_decl.items[i].name, decl));
		}
	}
	return entity;
}

Entity* entity_install_type(const char* name, Type* type) {
	Entity* entity = entity_new(ENTITY_TYPE, name, NULL);
	entity->state = ENTITY_RESOLVED;
	entity->type = type;
	buf_push(entities, entity);
	return entity;
}

typedef struct ResolvedExpr {
	Type* type;
	bool is_lvalue;
	bool is_const;
	int64_t val;
} ResolvedExpr;

ResolvedExpr resolved_null;

ResolvedExpr resolved_rvalue(Type* type) {
	return (ResolvedExpr) {
		.type = type,
	};
}

ResolvedExpr resolved_lvalue(Type* type) {
	return (ResolvedExpr) {
		.type = type,
			.is_lvalue = true,
	};
}

ResolvedExpr resolved_const(int64_t val) {
	return (ResolvedExpr) {
		.type = type_int,
			.is_const = true,
			.val = val,
	};
}

Entity* resolve_name(const char* name);
int64_t resolve_int_const_expr(Expr* expr);
ResolvedExpr resolve_expr(Expr* expr, Type* expected_type);

Type* resolve_typespec(Typespec* typespec) {
	switch (typespec->kind) {
	case TYPESPEC_NAME: {
		Entity* entity = resolve_name(typespec->name);
		if (entity->kind != ENTITY_TYPE) {
			fatal("%s must denote a type", typespec->name);
			return NULL;
		}
		return entity->type;
	}
	case TYPESPEC_PTR:
		return type_ptr(resolve_typespec(typespec->ptr.elem));
	case TYPESPEC_ARRAY:
		return type_array(resolve_typespec(typespec->array.elem), resolve_int_const_expr(typespec->array.size));
	case TYPESPEC_FUNC: {
		Type** args = NULL;
		for (size_t i = 0; i < typespec->func.num_args; i++) {
			buf_push(args, resolve_typespec(typespec->func.args[i]));
		}
		return type_func(args, buf_len(args), resolve_typespec(typespec->func.ret));
	}
	default:
		assert(0);
		return NULL;
	}
}

Entity** ordered_entities;

void complete_type(Type* type) {
	if (type->kind == TYPE_COMPLETING) {
		fatal("Type completion cycle");
		return;
	}
	else if (type->kind != TYPE_INCOMPLETE) {
		return;
	}
	type->kind = TYPE_COMPLETING;
	Decl* decl = type->entity->decl;
	assert(decl->kind == DECL_STRUCT || decl->kind == DECL_UNION);
	TypeField* fields = NULL;
	for (size_t i = 0; i < decl->aggregate.num_items; i++) {
		AggregateItem item = decl->aggregate.items[i];
		Type* item_type = resolve_typespec(item.type);
		complete_type(item_type);
		for (size_t j = 0; j < item.num_names; j++) {
			buf_push(fields, (TypeField) { item.names[j], item_type });
		}
	}
	if (decl->kind == DECL_STRUCT) {
		type_complete_struct(type, fields, buf_len(fields));
	}
	else {
		assert(decl->kind == DECL_UNION);
		type_complete_union(type, fields, buf_len(fields));
	}
	buf_push(ordered_entities, type->entity);
}

Type* resolve_decl_type(Decl* decl) {
	assert(decl->kind == DECL_TYPEDEF);
	return resolve_typespec(decl->typedef_decl.type);
}

Type* resolve_decl_var(Decl* decl) {
	assert(decl->kind == DECL_VAR);
	Type* type = NULL;
	if (decl->var.type) {
		type = resolve_typespec(decl->var.type);
	}
	if (decl->var.expr) {
		ResolvedExpr result = resolve_expr(decl->var.expr, NULL);
		if (type && result.type != type) {
			fatal("Declared var type does not match inferred type");
		}
		type = result.type;
	}
	complete_type(type);
	return type;
}

Type* resolve_decl_const(Decl* decl, int64_t* val) {
	assert(decl->kind == DECL_CONST);
	ResolvedExpr result = resolve_expr(decl->const_decl.expr, NULL);
	*val = result.val;
	return result.type;
}

Type* resolve_decl_func(Decl* decl) {
	assert(decl->kind == DECL_FUNC);
	Type** params = NULL;
	for (size_t i = 0; i < decl->func.num_params; i++) {
		buf_push(params, resolve_typespec(decl->func.params[i].type));
	}
	return type_func(params, buf_len(params), resolve_typespec(decl->func.ret_type));
}

void resolve_entity(Entity* entity) {
	if (entity->state == ENTITY_RESOLVED) {
		return;
	}
	else if (entity->state == ENTITY_RESOLVING) {
		fatal("Cyclic dependency");
		return;
	}
	assert(entity->state == ENTITY_UNRESOLVED);
	entity->state = ENTITY_RESOLVING;
	switch (entity->kind) {
	case ENTITY_TYPE:
		entity->type = resolve_decl_type(entity->decl);
		break;
	case ENTITY_VAR:
		entity->type = resolve_decl_var(entity->decl);
		break;
	case ENTITY_CONST:
		entity->type = resolve_decl_const(entity->decl, &entity->val);
		break;
	case ENTITY_FUNC:
		entity->type = resolve_decl_func(entity->decl);
		break;
	default:
		assert(0);
		break;
	}
	entity->state = ENTITY_RESOLVED;
	buf_push(ordered_entities, entity);
}

void complete_entity(Entity* entity) {
	resolve_entity(entity);
	if (entity->kind == ENTITY_TYPE) {
		complete_type(entity->type);
	}
}

Entity* resolve_name(const char* name) {
	Entity* entity = entity_get(name);
	if (!entity) {
		fatal("Non-existent name");
		return NULL;
	}
	resolve_entity(entity);
	return entity;
}

ResolvedExpr resolve_expr_field(Expr* expr) {
	assert(expr->kind == EXPR_FIELD);
	ResolvedExpr left = resolve_expr(expr->field.expr, NULL);
	Type* type = left.type;
	complete_type(type);
	if (type->kind != TYPE_STRUCT && type->kind != TYPE_UNION) {
		fatal("Can only access fields on aggregate types");
		return resolved_null;
	}
	for (size_t i = 0; i < type->aggregate.num_fields; i++) {
		TypeField field = type->aggregate.fields[i];
		if (field.name == expr->field.name) {
			return left.is_lvalue ? resolved_lvalue(field.type) : resolved_rvalue(field.type);
		}
	}
	fatal("No field named '%s'", expr->field.name);
	return resolved_null;
}

ResolvedExpr resolve_expr_name(Expr* expr) {
	assert(expr->kind == EXPR_NAME);
	Entity* entity = resolve_name(expr->name);
	if (entity->kind == ENTITY_VAR) {
		return resolved_lvalue(entity->type);
	}
	else if (entity->kind == ENTITY_CONST) {
		return resolved_const(entity->val);
	} else if (entity->kind == ENTITY_FUNC){
		return resolved_rvalue(entity->type);
	}
	else {
		fatal("%s must be a var or const", expr->name);
		return resolved_null;
	}
}

ResolvedExpr resolve_expr_unary(Expr* expr) {
	assert(expr->kind == EXPR_UNARY);
	ResolvedExpr operand = resolve_expr(expr->unary.expr, NULL);
	Type* type = operand.type;
	switch (expr->unary.op) {
	case TOKEN_MUL:
		if (type->kind != TYPE_PTR) {
			fatal("Cannot deref non-ptr type");
		}
		return resolved_lvalue(type->ptr.elem);
	case TOKEN_AND:
		if (!operand.is_lvalue) {
			fatal("Cannot take address of non-lvalue");
		}
		return resolved_rvalue(type_ptr(type));
	default:
		assert(0);
		return resolved_null;
	}
}

ResolvedExpr resolve_expr_binary(Expr* expr) {
	assert(expr->kind == EXPR_BINARY);
	assert(expr->binary.op == TOKEN_ADD);
	ResolvedExpr left = resolve_expr(expr->binary.left, NULL);
	ResolvedExpr right = resolve_expr(expr->binary.right, NULL);
	if (left.type != type_int) {
		fatal("left operand of + must be int");
	}
	if (right.type != left.type) {
		fatal("left and right operand of + must have same type");
	}
	if (left.is_const && right.is_const) {
		return resolved_const(left.val + right.val);
	}
	else {
		return resolved_rvalue(left.type);
	}
}



ResolvedExpr resolve_expr_compound(Expr* expr, Type* expected_type) {
	assert(expr->kind == EXPR_COMPOUND);
	if (!expected_type && !expr->compound.type) {
		fatal("Implicitly typed compound literals used in context without expected type");
	}
	Type* type = NULL;
	if (expr->compound.type) {
		type = resolve_typespec(expr->compound.type);
		if (expected_type && expected_type != type) {
			fatal("Explicit compound literal type does not match expected type");
		}
	} else {
		type = expected_type;
	}
	complete_type(type);
	if (type->kind != TYPE_STRUCT && type->kind != TYPE_UNION && type->kind != TYPE_ARRAY) {
		fatal("Compound literals can only be used with struct, union and array types");
	}
	if (type->kind == TYPE_STRUCT || type->kind == TYPE_UNION) {
		if (expr->compound.num_args > type->aggregate.num_fields) {
			fatal("Compound literal has too many fields");
		}
		for (size_t i = 0; i < expr->compound.num_args; i++) {
			ResolvedExpr field = resolve_expr(expr->compound.args[i], NULL);
			if (field.type != type->aggregate.fields[i].type) {
				fatal("Compound literal field type mismatch");
			}
		}
	}
	else {
		assert(type->kind == TYPE_ARRAY);
		if (expr->compound.num_args > type->array.size) {
			fatal("Compound literal has too many elements");
		}
		for (size_t i = 0; i < expr->compound.num_args; i++) {
			ResolvedExpr elem = resolve_expr(expr->compound.args[i], NULL);
			if (elem.type != type->array.elem) {
				fatal("Compound literal element type mismatch");
			}
		}
	}
	return resolved_rvalue(type);
}

ResolvedExpr resolve_expr_call(Expr* expr) {
	assert(expr->kind == EXPR_CALL);
	ResolvedExpr func = resolve_expr(expr->call.expr);
	if (func.type->kind != TYPE_FUNC) {
		fatal("Trying to call non-function value");
	}
	if (expr->call.num_args != func.type->func.num_params) {
		fatal("Tried to call function with wrong number of arguments");
	}
	for (size_t i = 0; i < expr->call.num_args; i++) {
		Type* param_type = func.type->func.params[i];
		ResolvedExpr arg = resolve_expected_expr(expr->call.args[i], param_type);
		if (arg.type != param_type) {
			fatal("Call argument expression type doesn't match expected param type");
		}
	}
	return resolved_rvalue(func.type->func.ret);
}

ResolvedExpr resolve_expr(Expr* expr, Type* expected_type) {
	switch (expr->kind) {
	case EXPR_INT:
		return resolved_const(expr->int_val);
	case EXPR_NAME:
		return resolve_expr_name(expr);
	case EXPR_COMPOUND:
		return resolve_expr_compound(expr, expected_type);
	case EXPR_FIELD:
		return resolve_expr_field(expr);
	case EXPR_UNARY:
		return resolve_expr_unary(expr);
	case EXPR_BINARY:
		return resolve_expr_binary(expr);
	case EXPR_CALL:
		return resolve_expr_call(expr);
	case EXPR_SIZEOF_EXPR: {
		ResolvedExpr result = resolve_expr(expr->sizeof_expr, NULL);
		Type* type = result.type;
		complete_type(type);
		return resolved_const(type_sizeof(type));
	}
	case EXPR_SIZEOF_TYPE: {
		Type* type = resolve_typespec(expr->sizeof_type);
		complete_type(type);
		return resolved_const(type_sizeof(type));
	}
	default:
		assert(0);
		return resolved_null;
	}
}

int64_t resolve_int_const_expr(Expr* expr) {
	ResolvedExpr result = resolve_expr(expr, NULL);
	if (!result.is_const) {
		fatal("Expected constant expression");
	}
	return result.val;
}

void resolve_test(void) {
	Type* int_ptr = type_ptr(type_int);
	assert(type_ptr(type_int) == int_ptr);		// i think this assert is foolish
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
	Type* int_int_func = type_func(&type_int, 1, type_int);		// why &	???
	assert(type_func(&type_int, 1, type_int) == int_int_func);
	Type* int_func = type_func(NULL, 0, type_int);
	assert(int_int_func != int_func);
	assert(int_func == type_func(NULL, 0, type_int));

	const char* int_name = str_intern("int");
	entity_install_type(int_name, type_int);
	const char* code[] = {
		"struct Vector { x, y: int; }",
		//"var v = Vector{1,2}",
		"var v: Vector = Vector{1,2}",
		"var w = Vector {3, 4} ",
		"union IntOrPtr { i: int; p: int*; }",
		"var i = 42",
		"var u = IntOrPtr{i, &i}",
		//"var u = IntOrPtr{&i, i}",
		//"var a = (:int[3]){1, 2, 3}",
		//"var a = (:int[3]){1, 2}",
		//"var a = (:int[3]){1, 2, 3, 4}",
		//"var a: int[3] = (:int[3]){1, 2, 3}",
		//"var a: int[3] = {1,2,3}",				// must work

		/*"const n = 1+sizeof(p)",
		"var p: T*",
		"var u = *p",
		"struct T { a: int[n]; }",
		"var r = &t.a",
		"var t: T",
		"typedef S = int[n+m]",
		"const m = sizeof(t.a)",
		"var i = n+m",
		"var q = &i",*/
		//        "const n = sizeof(x)",
		//        "var x: T",
		//        "struct T { s: S*; }",
		//        "struct S { t: T[n]; }",
	};
	for (size_t i = 0; i < sizeof(code) / sizeof(*code); i++) {
		init_stream(code[i]);
		Decl* decl = parse_decl();
		entity_install_decl(decl);
	}
	for (Entity** it = entities; it != buf_end(entities); it++) {
		Entity* entity = *it;
		complete_entity(entity);
	}
	for (Entity** it = ordered_entities; it != buf_end(ordered_entities); it++) {
		Entity* entity = *it;
		if (entity->decl) {
			print_decl(entity->decl);
		}
		else {
			printf("%s", entity->name);
		}
		printf("\n");
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

// Nice to separete the ordering from everything else so when you go to compute sizes of all the types,
// they are already in an order. 
// All other tings will be more straigtforward. They don't have to be worried about ordering on demand, only that ordering is consistent.

// LL1 resolver

// That's sort of what C does [Day 8. 28:00]

// Day 8. 1:18:00

#if 0
void sym_decl(Decl* decl) {
	assert(decl->name);
	// good check on the same variable declarations (int a = 5, float a = 3.0)
	assert(!sym_get(decl->name));		// assert will terminate the program if its argument turns out to be false
	buf_push(global_syms, (Sym) { SYM_DECL, decl->name, decl, SYM_UNORDERED });
}
#endif