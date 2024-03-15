#include <fcntl.h>
#include <string.h>

#include "BufferedFile.h"
#include "TGLException.h"

int64_t BufferedFile::file_size(const char *path)
{
	struct stat st;

	if (::stat(path, &st))
		TGLError("Cannot stat file %s: %s\n", path, strerror(errno));
	return (int64_t)st.st_size;
}

int BufferedFile::open(const char *path, const char *mode, bool lock)
{
	close();
	m_filename = path;
	m_fp = fopen(path, mode);
	if (m_fp) {
        if (lock) {
            struct flock fl;

            // according to fcntl() manual, lock is automatically released when the file description is closed
            memset(&fl, 0, sizeof(fl));
            fl.l_type = strcmp(mode, "r") ? F_WRLCK : F_RDLCK;
        	while (fcntl(fileno(m_fp), F_SETLKW, &fl) == -1) {
        		if (errno != EINTR) {
        			close();
                    return -1;
                }
        	}
        }

		m_eof = false;
		m_virt_pos = m_phys_pos = 0;
		m_sbuf_pos = m_ebuf_pos = 0;

		fseeko(m_fp, 0, SEEK_END);
		m_file_size = ftello(m_fp);
		fseeko(m_fp, 0, SEEK_SET);
		return 0;
	}
	return -1;
}

int BufferedFile::close()
{
	if (m_fp) {
		int retv = fclose(m_fp);
		m_fp = NULL;
		m_eof = true;
		m_phys_pos = -1;
		return retv;
	}
	return 0;
}

int BufferedFile::truncate()
{
    if (m_fp) {
        int retv = ftruncate(fileno(m_fp), m_virt_pos);

        if (!retv) {
            m_phys_pos = -1;
            m_file_size = m_virt_pos;
        }
        return retv;
    }
    return 0;
}
