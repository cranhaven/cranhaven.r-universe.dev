#ifndef _GINTERVALSSETBIG_H_INCLUDED_
#define _GINTERVALSSETBIG_H_INCLUDED_

//------------------------------------- GIntervalsBigSet ----------------------------------------
// !!!!!!!!! IN CASE OF ERROR THIS CLASS THROWS TGLException  !!!!!!!!!!!!!!!!

class GIntervalsBigSet {
public:
	static SEXP get_meta(const char *path);
	static bool isbig(const char *path);
	static bool is1d(SEXP meta);

	static GIntervalsSetBig *create(const char *path) {}

protected:
	GIntervalsSetBig() {}
	GIntervalsSetBig(const GIntervalsSetBig &) {}
	GIntervalsSetBig &operator=(const GIntervalsSetBig &) {}
};


//------------------------------------- GIntervalsSetBig ----------------------------------------
// !!!!!!!!! IN CASE OF ERROR THIS CLASS THROWS TGLException  !!!!!!!!!!!!!!!!

class GIntervalsSetBig1D {
public:
	GIntervalsSetBig() {}
	GIntervalsSetBig(const char *path) { init(path); }
	virtual ~GIntervalsSetSmall() {}

	void init(const char *path);

private:
	GIntervals m_intervals;
};



#endif
