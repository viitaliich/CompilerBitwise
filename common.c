#define MAX(x, y) ((x) >= (y) ? (x) : (y))

#define ALIGN_DOWN(n, a) ((n) & ~((a) - 1))				//			 n & ~(a - 1)
#define ALIGN_UP(n, a) ALIGN_DOWN((n) + (a) - 1, (a))	// (n + a - 1) & ~(a - 1)
#define ALIGN_DOWN_PTR(p, a) ((void *)ALIGN_DOWN((uintptr_t)(p), (a)))	// uintptr_t - 
#define ALIGN_UP_PTR(p, a) ((void *)ALIGN_UP((uintptr_t)(p), (a)))		// manipulate it as a usual integer number, then cast back

void* xrealloc(void* ptr, size_t num_bytes) {
	ptr = realloc(ptr, num_bytes);
	if (!ptr) {
		perror("xrealloc failed");
		exit(1);
	}
	return ptr;
}

void* xmalloc(size_t num_bytes) {
	void* ptr = malloc(num_bytes);
	if (!ptr) {
		perror("xmalloc failed");
		exit(1);
	}
	return ptr;
}

void* xcalloc(size_t num_items, size_t item_size) {
	void* ptr = calloc(num_items, item_size);
	if (!ptr) {
		perror("xcalloc failed");
		exit(1);
	}
	return ptr;
}

void* memdup(void* src, size_t size) {
	void* dest = xmalloc(size);
	memcpy(dest, src, size);
	return dest;
}

void fatal(const char* fmt, ...) {
	va_list args;
	va_start(args, fmt);
	printf("FATAL: ");
	vprintf(fmt, args);
	printf("\n");
	va_end(args);
	exit(1);
}

void syntax_error(const char* fmt, ...) {
	va_list args;
	va_start(args, fmt);
	printf("Syntax error: ");
	vprintf(fmt, args);
	printf("\n");
	va_end(args);
}

// where is it using?	???
void fatal_syntax_error(const char* fmt, ...) {
	va_list args;
	va_start(args, fmt);
	printf("Syntax Error: ");
	vprintf(fmt, args);
	printf("\n");
	va_end(args);
	exit(1);
}

// stretchy buffer (dynamically growen array / cpp vector like). sean burret

typedef struct BufHdr {		//Buf header
	size_t len;
	size_t cap;
	int8_t buf[];
} BufHdr;

// two underscores - PRIVATE
// b - buffer pointer. for the pointer arithmetic we need to subtract the size of 2 fields that precede the buffer
#define buf__hdr(b)	((BufHdr*)((int8_t*)b - offsetof(BufHdr, buf)))
#define buf__fit(b, n) ((n) <= buf_cap(b) ? 0 : ((b) = buf__grow((b), (n), sizeof(*(b)))))

// one uderscores - PUBLIC
#define buf_len(b) ((b) ? buf__hdr(b) -> len : 0) 
#define buf_cap(b) ((b) ? buf__hdr(b) -> cap : 0)
#define buf_end(b) ((b) + buf_len(b))
#define buf_sizeof(b) ((b) ? buf_len(b)*sizeof(*b) : 0)

#define buf_free(b) ((b) ? (free(buf__hdr(b)), (b) = NULL) : 0)
#define buf_push(b, ...) (buf__fit((b), 1 + buf_len(b)), (b)[buf__hdr(b)->len++] = (__VA_ARGS__))
#define buf_printf(b, ...) ((b) = buf__printf((b), __VA_ARGS__))
#define buf_clear(b) ((b) ? buf__hdr(b)->len = 0 : 0)		// resets the buffer but doesn't free it

// rare case when use it, so it will not polute the cashe. 
void* buf__grow(const void* buf, size_t new_len, size_t elem_size) {
	assert(buf_cap(buf) <= (SIZE_MAX - 1) / 2);
	size_t new_cap = MAX(16, MAX(1 + 2 * buf_cap(buf), new_len));		// why 16?
	assert(new_len <= new_cap);
	assert(new_cap <= (SIZE_MAX - offsetof(BufHdr, buf)) / elem_size);	
	size_t new_size = offsetof(BufHdr, buf) + new_cap * elem_size;
	BufHdr* new_hdr;
	if (buf) {
		new_hdr = xrealloc(buf__hdr(buf), new_size);
	}
	else {
		new_hdr = xmalloc(new_size);
		new_hdr->len = 0;
	}
	new_hdr->cap = new_cap;
	return new_hdr->buf;
}

// Treating a character buffer as an append only string buffer.
// It's like write add to a file using fprintf() or sth. like this
// concatenate outputs
char* buf__printf(char* buf, const char* fmt, ...) {
	va_list args;
	va_start(args, fmt);
	size_t cap = buf_cap(buf) - buf_len(buf);
	size_t n = 1 + vsnprintf(buf_end(buf), cap, fmt, args);
	va_end(args);
	if (n > cap) {
		buf_fit(buf, n + buf_len(buf));
		va_start(args, fmt);
		cap = buf_cap(buf) - buf_len(buf);
		n = 1 + vsnprintf(buf_end(buf), cap, fmt, args);
		assert(n <= cap);
		va_end(args);
	}
	buf__hdr(buf)->len += n - 1;
	return buf;
}

void buf_test() {
	char* ch = NULL;
	buf_push(ch, 'a');
	//printf("%c\n", ch[0]);
	
	int* buf = NULL;		
	assert(buf_len(buf) == 0);
	
	int n = 1024;
	for (int i = 0; i < n; i++) {
		buf_push(buf, i);
	}
	assert(buf_len(buf) == n);
	for (int i = 0; i < buf_len(buf); i++) {
		assert(buf[i] == i);
	}
	
	buf_free(buf);
	assert(buf == NULL);
	assert(buf_len(buf) == 0);

	char* str = NULL;
	buf_printf(str, "One: %d\n", 1);
	assert(strcmp(str, "One: 1\n") == 0);
	buf_printf(str, "Hex: 0x%x\n", 0x12345678);
	assert(strcmp(str, "One: 1\nHex: 0x12345678\n") == 0);
	
}

// Arena allocator

typedef struct Arena {
	int8_t* ptr;		
	int8_t* end;
	int8_t** blocks;	// stretchy buffer of poiters on memory blocks
} Arena;

// how to manage (choose) block sizes ???
#define ARENA_ALIGNMENT 8	
//#define ARENA_BLOCK_SIZE (1024 * 1024)
#define ARENA_BLOCK_SIZE 1024

void arena_grow(Arena* arena, size_t min_size) {
	size_t size = ALIGN_UP(MAX(ARENA_BLOCK_SIZE, min_size), ARENA_ALIGNMENT);
	arena->ptr = xmalloc(size);
	assert(arena->ptr == ALIGN_DOWN_PTR(arena->ptr, ARENA_ALIGNMENT));
	arena->end = arena->ptr + size;
	buf_push(arena->blocks, arena->ptr);
}

void* arena_alloc(Arena* arena, size_t size) {
	if (size > (size_t)(arena->end - arena->ptr)) {
		arena_grow(arena, size);
		assert(size <= (size_t)(arena->end - arena->ptr));
	}
	void* ptr = arena->ptr;
	arena->ptr = ALIGN_UP_PTR(arena->ptr + size, ARENA_ALIGNMENT);
	assert(arena->ptr <= arena->end);
	assert(ptr == ALIGN_DOWN_PTR(ptr, ARENA_ALIGNMENT));
	return ptr;
}

void arena_free(Arena* arena) {
	for (int8_t** it = arena->blocks; it != buf_end(arena->blocks); it++) {
		free(*it);
	}
	buf_free(arena->blocks);	// fix not freeing arena block array		???
}

// string interning

typedef struct Intern {
	size_t len;
	const char* str;
} Intern;

Arena str_arena;
Intern* interns = NULL;

const char* str_intern_range(const char* start, const char* end) {
	size_t len = end - start;
	// search through the table
	for (Intern* it = interns; it != buf_end(interns); it++) {
		if (it->len == len && strncmp(it->str, start, len) == 0) {
			return it->str;
		}
	}
	char* str = arena_alloc(&str_arena, len + 1);
	memcpy(str, start, len);
	str[len] = 0;	// NULL terminate		//???
	buf_push(interns, (Intern) { len, str });
	return str;
}

const char* str_intern(const char* str) {
	return str_intern_range(str, str + strlen(str));
}

void intern_test() {
	char a[] = "hello";		// why not char* ?
	assert(strcmp(a, str_intern(a)) == 0);
	assert(str_intern(a) == str_intern(a));
	assert(str_intern(str_intern(a)) == str_intern(a));
	char b[] = "hello";
	assert(a != b);
	assert(str_intern(a) == str_intern(b));
	char c[] = "hello!";
	assert(str_intern(a) != str_intern(c));
	char d[] = "hell";
	assert(str_intern(a) != str_intern(d));
}

void common_test() {
	buf_test();
	intern_test();
}
