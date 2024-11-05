#ifndef ALLOCATOR_CUH
#define ALLOCATOR_CUH

#include <map>
#include <string>

#define MALLOC_HOST(size) _alloc.mallocHost((size), __LINE__, __FILE__)
#define MALLOC_PINNED(size) _alloc.mallocPinned((size), __LINE__, __FILE__)
#define MALLOC_DEVICE(size) _alloc.mallocDevice((size), __LINE__, __FILE__)
#define FREE_HOST(ptr) _alloc.freeHost((void*) (ptr))
#define FREE_PINNED(ptr) _alloc.freePinned((void*) (ptr))
#define FREE_DEVICE(ptr) _alloc.freeDevice((void*) (ptr))

class Allocator {
	std::map<void*, std::string> loc;
public:
	void* mallocHost(std::size_t size, int line, std::string file);
	void* mallocPinned(std::size_t size, int line, std::string file);
	void* mallocDevice(std::size_t size, int line, std::string file);
	void freeHost(void* ptr);
	void freePinned(void* ptr);
	void freeDevice(void* ptr);
	~Allocator();
};

extern Allocator _alloc;

#endif
