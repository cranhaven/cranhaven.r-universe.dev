#include "TGLException.h"
#include "strutil.h"

using namespace std;

int split_line_by_space_chars(istream &in, vector<string> &fields, int estimated_num_fields)
{
	int num_lines = 0;
	fields.resize(estimated_num_fields);
	for (vector<string>::iterator istr = fields.begin(); istr != fields.end(); ++istr)
		istr->resize(0);
	vector<string>::iterator istr = fields.begin();
	while(in) {
		int c = in.get();
		if(c == '\r')
			continue;

		if (c == '\n')
			num_lines++;

		if (c == '\n' || !in.good()) {
			if (istr == fields.begin() && istr->empty()) {
				if (!in.good()) {
					fields.clear();
					break;
				}

				// eat up empty lines
				continue;
			}
			fields.resize(istr - fields.begin() + 1);
			break;
		}

		if (isspace(c)) {
			if (!istr->empty()) {
				istr++;
				if (istr == fields.end()) {
					fields.push_back(string());
					istr = fields.begin() + fields.size() - 1;
				}
			}
		} else
			istr->push_back(c);
	}
	return num_lines;
}

int split_line_by_space_chars(BufferedFile &in, vector<string> &fields, int estimated_num_fields)
{
	int num_lines = 0;
	fields.resize(estimated_num_fields);
	for (vector<string>::iterator istr = fields.begin(); istr != fields.end(); ++istr)
		istr->resize(0);
	vector<string>::iterator istr = fields.begin();
	while (1) {
		int c = in.getc();

		if (in.error()) {
			fields.clear();
			break;
		}

		if (c == '\r')
			continue;

		if (c == '\n' || in.eof()) {
			num_lines++;
			if (istr == fields.begin() && istr->empty()) {
				if (in.eof()) {
					fields.clear();
					break;
				}

				// eat up empty lines
				continue;
			}
			fields.resize(istr - fields.begin() + 1);
			break;
		}

		if (isspace(c)) {
			if (!istr->empty()) {
				istr++;
				if (istr == fields.end()) {
					fields.push_back(string());
					istr = fields.begin() + fields.size() - 1;
				}
			}
		} else
			istr->push_back(c);
	}
	return num_lines;
}

int split_line(BufferedFile &in, vector<string> &fields, char delim, int estimated_num_fields)
{
	int num_lines = 0;
	fields.resize(estimated_num_fields);
	for (vector<string>::iterator istr = fields.begin(); istr != fields.end(); ++istr)
		istr->resize(0);
	vector<string>::iterator istr = fields.begin();
	while (1) {
		int c = in.getc();

		if (in.error()) {
			fields.clear();
			break;
		}

		if (c == '\r')
			continue;

		if (c == '\n' || in.eof()) {
			num_lines++;
			if (istr == fields.begin() && istr->empty()) {
				if (in.eof()) {
					fields.clear();
					break;
				}

				// eat up empty lines
				continue;
			}
			fields.resize(istr - fields.begin() + 1);
			break;
		}

		if (c == delim) {
			istr++;
			if (istr == fields.end()) {
				fields.push_back(string());
				istr = fields.begin() + fields.size() - 1;
			}
		} else
			istr->push_back(c);
	}
	return num_lines;
}

int split_line(istream &in, vector<string> &fields, char delim, int estimated_num_fields)
{
	int num_lines = 0;
	fields.resize(estimated_num_fields);
	for (vector<string>::iterator istr = fields.begin(); istr != fields.end(); ++istr)
		istr->resize(0);
	vector<string>::iterator istr = fields.begin();
	while(in) {
		int c = in.get();
		if(c == '\r') {
			continue;
		}

		if (c == '\n')
			num_lines++;

		if (c == '\n' || !in.good()) {
			if (istr == fields.begin() && istr->empty()) {
				if (!in.good()) {
					fields.clear();
					break;
				}

				// eat up empty lines
				continue;
			}
			fields.resize(istr - fields.begin() + 1);
			break;
		}
		if (c == delim) {
			istr++;
			if (istr == fields.end()) {
				fields.push_back(string());
				istr = fields.begin() + fields.size() - 1;
			}
		} else
			istr->push_back(c);
	}
	return num_lines;
}

void split_line(const string &s, vector<string> &fields, char delim)
{
	fields.resize(0);
	string field;
	for(string::const_iterator si = s.begin(); si != s.end(); si++){
		if(*si == delim) {
			fields.push_back(field);
			field.resize(0);
		} else {
			field.push_back(*si);
		}
	}
    fields.push_back(field);
}

void split_line(const string &s, vector<float> &fields, char delim)
{
	fields.resize(0);
	string field;
	for(string::const_iterator si = s.begin(); si != s.end(); si++){
		if(*si == delim) {
			float f = atof(field.c_str());
			fields.push_back(f);
			field.resize(0);
		} else {
			field.push_back(*si);
		}
	}
	float f = atof(field.c_str());
	fields.push_back(f);
}

void split_line(const string &s, vector<int> &fields, char delim)
{
	fields.resize(0);
	string field;
	for(string::const_iterator si = s.begin(); si != s.end(); si++){
		if(*si == delim) {
			int v = atoi(field.c_str());
			fields.push_back(v);
			field.resize(0);
		} else {
			field.push_back(*si);
		}
	}
	int v = atoi(field.c_str());
	fields.push_back(v);
}

int get_one_field(istream &in, string &field, char delim, int num, bool eat_line)
{
	int count = 0;
	field = "";
	while(in && count < num) {
		char c = in.get();
		if(c == '\r') {
			continue;
		}
		if(c == '\n') {
			break;
		}
		if(c == delim) {
			count++;
		}
	}
	if(!in) {
		return -1;
	}
	if(count == num) {
		while(in) {
			char c = in.get();
			if(c == delim || c == '\r' || c == '\n') {
				if(c== '\n') {
					eat_line = false;
				}
				break;
			}
			field.push_back(c);
		}
	}
	if(eat_line) {
		while(in) {
			char c = in.get();
			if(c == '\n') {
				break;
			}
		}
	}
	return(count);
}

int count_match(const string &targ, const string &mot)
{
	int count = 0;
	string::size_type pos = targ.find(mot, 0);
	while(pos != string::npos) {
		pos = targ.find(mot, pos + 1);
		count++;
	}
	return(count);
}

void read_int_table(istream &in, int width, vector<vector<int> > &data)
{
	vector<string> fields;
	int row = 0;
	while(in) {
		split_line(in, fields);
		if(fields.size() == 0) {
			return;
		}
		TGLAssert((int)fields.size() == width, "Bad table width (%d instead %d) when parsing int table", fields.size(), width);
		data.resize(row + 1, vector<int>(width));
		vector<int>::iterator dt = data[row].begin();
		vector<string>::const_iterator inp = fields.begin();
		while(inp != fields.end()) {
			char *fin;
			*dt = strtol((*inp).c_str(), &fin, 0);
			TGLAssert((fin - (*inp).c_str()) == (int)inp->size(), "Cannot parse int at row %d col %d", row, inp-fields.begin());
			dt++;
			inp++;
		}
		row++;
	}
}

void read_float_table(istream &in, int width, vector<vector<float> > &data)
{
	vector<string> fields;
	int row = 0;
	while(in) {
		split_line(in, fields);
		if(fields.size() == 0) {
			return;
		}
		TGLAssert((int)fields.size() == width, "Bad table width (%d instead %d) when parsing float table", fields.size(), width);
		data.resize(row + 1, vector<float>(width));
		vector<float>::iterator dt = data[row].begin();
		vector<string>::const_iterator inp = fields.begin();
		while(inp != fields.end()) {
			char *fin;
			*dt = strtof((*inp).c_str(), &fin);
			TGLAssert((fin - (*inp).c_str()) == (int)inp->size(), "Cannot parse float at row %d col %d", row, inp-fields.begin());
			dt++;
			inp++;
		}
		row++;
	}
}

void read_string_table(istream &in, int width, vector<vector<string> > &data){
	int row = 0;
	vector<string> fields;
	while(in) {
		split_line(in, fields);
		if(fields.size() == 0) {
			return;
		}
		TGLAssert((int)fields.size() == width, "Bad table width (%d instead %d) when parsing string table", fields.size(), width);
		data.resize(row + 1, vector<string>(width));
		vector<string>::iterator dt = data[row].begin();
		vector<string>::const_iterator inp = fields.begin();
		while(inp != fields.end()) {
			*dt = *inp;
			dt++;
			inp++;
		}
		row++;
	}
}


void read_float_table_with_rowname(istream &in, vector<vector<float> > &data, vector<string> &row_name, int with_header, int subst_nas, float na_value)
{
	vector<string> fields;
	int width = -1;
	if(with_header) {
		split_line(in, fields);
		width = fields.size() - 1;
	}
	int row = 0;
	while(in) {
		split_line(in, fields);
		if(fields.size() == 0) {
			return;
		}
		if(width == -1) {
			width = fields.size() - 1;
		}
		TGLAssert((int)fields.size() == width + with_header ? 1 : 0, "Bad table width (%d instead %d) at row %d of float table", fields.size(), width, row);
		data.resize(row + 1);
		data[row].resize(width);
		vector<float>::iterator dt = data[row].begin();
		row_name.push_back(fields[0]);
		vector<string>::const_iterator inp = fields.begin() + 1;
		while(inp != fields.end()) {
			char *fin;
			if(*inp == "NA" && subst_nas) {
				*dt = na_value;
			} else {
    			*dt = strtof((*inp).c_str(), &fin);
    			TGLAssert((fin - (*inp).c_str()) == (int)inp->size(), "Cannot parse float at row %d col %d", fields.size(), inp-fields.begin());
			}
    		dt++;
			inp++;
		}
		row++;
	}
}
