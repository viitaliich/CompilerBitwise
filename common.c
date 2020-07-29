#define MAX(x, y) ((x) >= (y) ? (x) : (y))

// stretchy buffer (dynamically growen array / cpp vector like). sean burret

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
		perror("malloc failed");
		exit(1);
	}
	return ptr;
}

void* xcalloc(size_t num_items, size_t item_size) {
	void* ptr = calloc(num_items, item_size);		// ???
	if (!ptr) {
		perror("xcalloc failed");
		exit(1);
	}
	return ptr;
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
	//exit(1);
}

typedef struct BufHdr {		//Buf header
	size_t len;
	size_t cap;
	int8_t buf[0];	// ???  buf[]
} BufHdr;

#define BUF(x) x		// ???

// two underscores - PRIVATE
#define buf__hdr(b)	((BufHdr*)((int8_t*)b - offsetof(BufHdr, buf)))
								// b - buffer pointer. for the pointer arithmetic we need to subtract the size of 2 fields that precede the buffer
#define buf_fit(b, n) ((n) <= buf_cap(b) ? 0 : ((b) = buf__grow((b), (n), sizeof(*(b)))))

// one uderscores - PUBLIC
#define buf_len(b) ((b) ? buf__hdr(b) -> len : 0) 
#define buf_cap(b) ((b) ? buf__hdr(b) -> cap : 0)
#define buf_end(b) ((b) + buf_len(b))
#define buf_free(b) ((b) ? free(buf__hdr(b)), (b) = NULL : 0)		// to set len = 0
#define buf_push(b, ...) (buf__fit(b, 1), (b)[buf__hdr(b)->len++] = (__VA_ARGS__))

// rare case when use it, so it will not polute the cashe. 
void* buf__grow(const void* buf, size_t new_len, size_t elem_size) {
	assert(buf_cap(buf) <= (SIZE_MAX - 1) / 2);
	size_t new_cap = MAX(16, MAX(1 + 2 * buf_cap(buf), new_len));		// ???
	assert(new_len <= new_cap);
	assert(new_cap <= (SIZE_MAX - offsetof(BufHdr, buf)) / elem_size);	// ???
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

void buf_test() {
	int* ch = NULL;			// buffer of chars
	buf_push(ch, 'a');
	printf("%c\n", ch[0]);

	int* buf = NULL;		// buffer of integers
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
}

// string interning

typedef struct Intern {
	size_t len;
	const char* str;
} Intern;

static Intern* interns = NULL;	// static variable - this var is visible only in this file

const char* str_intern_range(const char* start, const char* end) {
	size_t len = end - start;
	// search through the table
	for (Intern* it = interns; it != buf_end(interns); it++) {
		if (it->len == len && strncmp(it->str, start, len) == 0) {
			return it->str;
		}
	}
	char* str = xmalloc(len + 1);		// +1 for a NULL terminator
	memcpy(str, start, len);
	str[len] = 0;	// NULL terminate		//???
	buf_push(interns, (Intern) { len, str });
	return str;
}

const char* str_intern(const char* str) {
	return str_intern_range(str, str + strlen(str));
}

void intern_test() {
	char x[] = "hello";		// why not char* x  ???
	char y[] = "hello";
	assert(x != y);
	assert(str_intern(x) == str_intern(y));
	char z[] = "hello!";
	assert(str_intern(x) != str_intern(z));
}

void common_test() {
	buf_test();
	intern_test();
}