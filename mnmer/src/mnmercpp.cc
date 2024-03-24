#include <fstream>
#include <map>
#include <set>
#include <vector>
#include <algorithm>
#include <random>
#include <chrono>
#include <unordered_set>
#include <R.h>
#include <Rinternals.h> 
#include <Rdefines.h>

using namespace std;

inline map<string,float> get_mmers (int m, int n, const string &s)
{
    string q, p1, p2;
    multimap<string,string> mmp;
    set<string> sm;

    for (int i = 0; i < s.size()-m-n; ++i){
        q = s.substr (i,m+n);
        p1 = q.substr (0,m);
        p2 = q.substr (m,n);

        mmp.insert ({p1,p2});
        sm.insert (p1);
    }

    pair<multimap<string,string>::iterator,multimap<string,string>::iterator> ret;
    multimap<string,string>::iterator it;

    int u;

    float nc = float(sm.size());

    map<string,map<string,float>> mptab;
    map<string,float> mpf;

    for (auto m: sm){
        ret = mmp.equal_range(m);


        u = distance (ret.first,ret.second);

        for (it=ret.first; it != ret.second; ++it)
            mpf[it->second] += 1.0/(float(u)*nc);
        
        mptab[m] = mpf;
        mpf.clear();
    }

    map<string,float> mdat;

    for (auto t: mptab)
        for (auto r: t.second)
            mdat[t.first+r.first] = r.second;
    

    return mdat;
}

//Generates base 4 number  
string conv4 (int num)
{
    string st = to_string(num%4);
    int k = num/4;

    while (k > 3){
        st += to_string (k%4);
        k = k/4;
    }
    st = st + to_string (k%4);
    reverse (st.begin(), st.end());
    return st;
}

//Converte para nucleotideo maiusculo
inline string convNUC (string s)
{
    for (int i = 0; i < s.size(); ++i)
        s[i] = (s[i] == '0') ? 'A' : (s[i] == '1') ? 'T' : (s[i] == '2') ? 'C' : (s[i] == '3') ? 'G' : s[i];
    return s;
}

//Generates a map with nucleotide with base 4 number 
vector<string> lexnucl (int num, int k)
{
    string s;
    vector<string>  vnu;
    int j;

    for (int i = 0; i < num; ++i) {
        s = conv4 (i);

        if (s.size()  < k)
            for (j=s.size(); j < k; ++j)
                s = '0' + s;
    
        vnu.push_back (convNUC(s));    
    }

    return vnu;
}


void save_string (string &res, vector<string> &vp, map<string,float> &mpta)
{
    map<string,float>::iterator it;
    
    for (auto p: vp)
        if ((it=mpta.find (p)) != mpta.end())
            res += to_string(it->second) + ",";
        else
            res += "0.0,";
    
    res.back() = '\n';
}


//read FASTA

inline bool checkseq (const string &sq, float pni)
{
    if (sq == "") return false;
    
    unsigned sz = sq.size();
    unsigned n = 0;
    unordered_set<char> snuc ( {'A','T','C','G','a','t','c','g'} );

    for (auto c: sq){
        if (snuc.find (c) == snuc.end())
            ++n;
        if ( (float(n)/float(sz)) >= pni )
            return false;
    }

    return true;
    
}

map<string,string> read_fasta_rand (string file, unsigned num, int size, float pni)
{
    unordered_set<int> su;
    mt19937 eng (chrono::system_clock::now().time_since_epoch().count());
    uniform_int_distribution<int> unif (0, num-1);

    map<string,string> mps;

    while (su.size() != size)
        su.insert (unif(eng));

    int i = 0;
    ifstream f (file);
    string s, id, seq = "";

    while (getline (f,s))
        if (s[0] == '>'){
            if ( checkseq (seq,pni) && (su.find(i-1) != su.end()) ){
                mps[id] = seq;         
            }

            if (mps.size() == size){
                f.close();
                return mps;
            }

            id = s.substr (1,s.find(" ")-1);
            seq = "";
            ++i;
        }       
        else
            seq += s;

    if ( checkseq (seq,pni) && (su.find(i-1) != su.end()) )
        mps[id] = seq;

    f.close();

    return mps;
}

inline unsigned num_seqs (string &file)
{
    ifstream f (file);
    string s;
    unsigned n = 0;
    while (getline (f,s))
        if (s[0] == '>')
            ++n;
    f.close();
    return n;
}


extern "C" {

SEXP cmnmer (SEXP seq, SEXP mm, SEXP nn)
{
    string sequence = CHAR(STRING_ELT(seq,0));

    int m = asInteger (mm);
    int n = asInteger (nn);
    int k = m + n;

//Get the lexicography of the 4 nucloetide
    vector<string> vp4 = lexnucl(pow(4,k), k);

//Result
    string result = "";
//Header
    for (auto h: vp4)
        result += h.insert(m,"|") + ",";
    result.back() = '\n';
    
    map<string,float> mtab = get_mmers (m, n, sequence);

    save_string (result, vp4, mtab);

    SEXP Stab = allocVector(STRSXP, result.size());

    PROTECT (Stab);
    Stab = mkString(result.c_str());
  	UNPROTECT(1);

    return Stab;
}

//Read random FASTA
    SEXP readrandFASTA (SEXP rfile, SEXP rsize, SEXP rpni)
    {
        string  file = CHAR(STRING_ELT(rfile,0));
        unsigned size = asInteger (rsize);
        float pni = asReal (rpni);

        string sf = "";

        unsigned num = num_seqs (file);

        if (size > num)
            sf = "101";
        else {
        
            map<string,string> mpse = read_fasta_rand (file, num ,size, pni);

            for (auto p: mpse)
                sf += p.first + "\t" + p.second + "\n";
        
        }

        SEXP cstr = allocVector(STRSXP, sf.size());
        
        PROTECT (cstr);
        cstr = mkString(sf.c_str());
  	    UNPROTECT(1);

        return cstr;
    }

}

static const R_CallMethodDef CallEntries[] = {
    {"cmnmer",        (DL_FUNC) &cmnmer,        3},
    {"readrandFASTA", (DL_FUNC) &readrandFASTA, 3},
    {NULL, NULL, 0}
};

void R_init_mnmer(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
