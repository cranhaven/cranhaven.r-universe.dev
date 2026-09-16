#ifndef LAUNCHABLE_CUH
#define LAUNCHABLE_CUH

class Launchable {
public:
	virtual ~Launchable() {}
	virtual void workLoop() = 0;
};

void Launch(Launchable *w);

#endif
