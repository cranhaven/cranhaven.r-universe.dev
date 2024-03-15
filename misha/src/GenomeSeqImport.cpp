#include <ctype.h>

#include "rdbutils.h"
#include "BufferedFile.h"

using namespace std;
using namespace rdb;

extern "C" {

SEXP gseqimport(SEXP _fasta, SEXP _seq, SEXP _envir)
{
	try {
		RdbInitializer rdb_init;

		if (!isString(_fasta) || length(_fasta) != 1)
			verror("Fasta argument is not a string");

		if (!isString(_seq) || length(_seq) != 1)
			verror("Seq argument is not a string");

		const char *fasta_fname = CHAR(STRING_ELT(_fasta, 0));
		const char *seq_fname = CHAR(STRING_ELT(_seq, 0));
		BufferedFile fasta_file;
		BufferedFile seq_file;

		if (fasta_file.open(fasta_fname, "r"))
			verror("Failed to open file %s: %s", fasta_file.file_name().c_str(), strerror(errno));
		if (seq_file.open(seq_fname, "w"))
			verror("Failed to open file %s: %s", seq_file.file_name().c_str(), strerror(errno));

		int c;
		bool is_newline = true;
		bool is_comment = false;
		vector<char> buf;

		while ((c = fasta_file.getc()) != EOF) {
			if (c == '\n') {
				is_newline = true;
				is_comment = false;
				continue;
			}

			if (is_newline && (c == '>' || c == ';')) 
				is_comment = true;

			if (!is_comment) {
				if (!isalpha(c) && c != '-') 
					verror("Invalid format of FASTA file %s", fasta_file.file_name().c_str());

				buf.push_back((char)c);
				if (buf.size() >= 1024) {
					seq_file.write(&buf.front(), buf.size());
					if (seq_file.error())
						verror("Error while writing %s: %s", seq_file.file_name().c_str(), strerror(errno));
					buf.clear();
				}
			}
		}

		if (fasta_file.error())
			verror("Error while reading %s: %s", fasta_file.file_name().c_str(), strerror(errno));

		if (buf.size()) {
			seq_file.write(&buf.front(), buf.size());
			if (seq_file.error())
				verror("Error while writing %s: %s", seq_file.file_name().c_str(), strerror(errno));
		}

		return R_NilValue;
	} catch (TGLException &e) {
		rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }
	return R_NilValue;
}

}
