#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>

#include "GIntervalsBigSet.h"
#include "GIntervalsBigSet1D.h"
#include "GIntervalsBigSet2D.h"
#include "rdbutils.h"

//------------------------------------- GIntervalsBigSet ----------------------------------------

bool GIntervalsBigSet::isbig(const char *intervset, const IntervUtils &iu)
{
	string path = interv2path(iu.get_env(), intervset);
	bool interv_found = false;
	SEXP gintervs;

	rprotect(gintervs = findVar(install("GINTERVS"), findVar(install(".misha"), iu.get_env())));
	for (int iinterv = 0; iinterv < length(gintervs); ++iinterv) {
		const char *interv = CHAR(STRING_ELT(gintervs, iinterv));
		if (!strcmp(intervset, interv)) {
			interv_found = true;
			break;
		}
	}

	if (!interv_found)
		return false;

	struct stat stat_res;

	return path.size() > rdb::INTERV_FILE_EXT.size() && !path.compare(path.size() - rdb::INTERV_FILE_EXT.size(), rdb::INTERV_FILE_EXT.size(), rdb::INTERV_FILE_EXT) &&
		!stat(path.c_str(), &stat_res) && S_ISDIR(stat_res.st_mode);
}

void GIntervalsBigSet::init(const char *intervset, const IntervUtils &iu)
{
	m_intervset = intervset;
	m_iu = (IntervUtils *)&iu;
}

