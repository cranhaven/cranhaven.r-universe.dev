
/*
 * rdbutils.h
 *
 *  Created on: Mar 26, 2010
 *      Author: hoichman
 */

#ifndef RDBUTILS_H_
#define RDBUTILS_H_

#include <cstdint>
#include <string>
#include <vector>
#include <pthread.h>
#include <semaphore.h>
#include <signal.h>
#include <time.h>
#include <unistd.h>
#include <sys/types.h>

#include "rdbinterval.h"
#include "Thread.h"

#include <R.h>
#include <Rinternals.h>
#include <Rinterface.h>

#include "TGLException.h"

#define MISHA_EXIT_SIG SIGTERM

using namespace std;

//------------------------------- UTILITY FUNCTIONS -------------------------------------

namespace rdb {

class IntervUtils;

extern const string TRACK_FILE_EXT;
extern const string INTERV_FILE_EXT;

// should be used instead of R_CheckUserInterrupt. Throws exception if the command is interrupted.
void check_interrupt();

// adds timeout to the time that is already in req
void set_abs_timeout(int64_t delay_msec, struct timespec &req);

// sets timeout to req
void set_rel_timeout(int64_t delay_msec, struct timespec &req);

// returns true if current time exceeds start_time + delay
bool is_time_elapsed(int64_t delay_msec, const struct timespec &start_time);

// use rerror/verror instead of error!
void rerror(const char *fmt, ...);

void verror(const char *fmt, ...);

// Use rprotect instead of PROTECT!
SEXP rprotect(SEXP &expr);

// Unprotect the last "count" object
void runprotect(int count);

// Unprotects object expr and sets it to R_NilValue. Works slower than runprotect(unsigned)!
void runprotect(SEXP &expr);

// Unprotects objects exprs and sets them to R_NilValue. Works slower than runprotect(unsigned)!
void runprotect(vector<SEXP> &exprs);

// Call runprotect_all if you wish to unprotect all object that are still protected
void runprotect_all();

struct SEXPCleaner {
    SEXPCleaner(SEXP &_var) : var(&_var) {}
    ~SEXPCleaner() { runprotect(*var); }
    SEXP *var;
};

void get_chrom_files(const char *dirname, vector<string> &chrom_files);

const char *get_groot(SEXP envir);

const char *get_gwd(SEXP envir);

const char *get_glib_dir(SEXP envir);

inline bool is_R_var_char(char c) { return isalnum(c) || c == '_' || c == '.'; }

// accepts track name, returns the path
string track2path(SEXP envir, const string &trackname);

// accepts track name, returns the path
string track2attrs_path(SEXP envir, const string &trackname);

// accepts track name, returns the path
string interv2path(SEXP envir, const string &intervname);

// Creates trackset and trackname directories using trackset.trackname. Verifies that the track name is valid.
string create_track_dir(SEXP envir, const string &trackname);

string get_bounded_colname(const char *str, unsigned maxlen = 40);

// the result is already protected
SEXP eval_in_R(SEXP parsed_command, SEXP envir);

// the result is already protected
SEXP run_in_R(const char *command, SEXP envir);

// This function writes R object to a file.
// Unlike R_Serialize function that just stops the execution if anything goes wrong (meaning: no clean up, destructors, etc.)
// RSaneSerialize throws an exception in case of error.
void RSaneSerialize(SEXP rexp, FILE *fp);
void RSaneSerialize(SEXP rexp, const char *fname);

// This function reads R object from a file. Object is expected to be saved using R's serialize() function or RSaneSerialize().
// The returned value is already protected.
// Unlike R_Unserialize function that just stops the execution if anything goes wrong (meaning: no clean up, destructors, etc.)
// RSaneUnserialize throws an exception in case of error.
SEXP RSaneUnserialize(FILE *fp);
SEXP RSaneUnserialize(const char *fname);

// Same as above: replaces allocVector which can fail on memory allocation and then R makes a longmp, skipping all the destructors
SEXP RSaneAllocVector(SEXPTYPE type, R_xlen_t len);

SEXP get_rvector_col(SEXP v, const char *colname, const char *varname, bool error_if_missing);

void prepare4multitasking(uint64_t res_const_size, uint64_t res_var_size, uint64_t max_res_size, uint64_t max_mem_usage, unsigned num_planned_kids);

pid_t launch_process();

void wait_for_kids(IntervUtils &iu);

int get_num_kids();

void update_progress(unsigned char progress);

void update_res_data_size(uint64_t size);

// returns memory where the child process can write its result
void *allocate_res(uint64_t res_num_records);

// returns memory for the parent where the child process wrote its result
void *get_kid_res(int kid_index);

// returns result size of the child process in the number of data
uint64_t get_kid_res_size(int kid_index);

// keeps track of allocations in child processes; if the total memory consumption exceeds the limit,
// the child processes is suspended unless all the rest of the processes have been already suspended
void report_alloc(int64_t bytes);

// for child processes this function checks the memory usage and if needed suspends the child process up until
// the memory is freed
void monitor_memusage();

template<typename T> void pack_data(void *&ptr, const T &data, uint64_t n) {
	uint64_t size = sizeof(data) * n;
	memcpy(ptr, &data, size);
	ptr = (char *)ptr + size;
}

template<typename T> void unpack_data(void *&ptr, T &data, uint64_t n) {
	uint64_t size = sizeof(data) * n;
	memcpy(&data, ptr, size);
	ptr = (char *)ptr + size;
}

}

#define MAX_KIDS 1000
#define rreturn(retv) { if (RdbInitializer::is_kid()) rexit(); return(retv); }

void rexit();

// Define RdbInitializer instance in your main function that is called by R.
// RdbInitializer should be defined inside "try-catch" statement that catches TGLException.
// RdbInitializer performs the following actions:
//   1. Installs a new SIGINT handler. ONE MUST CALL check_interrupt() INSTEAD OF R_CheckUserInterrupt()!!!!!!!
//   2. Installs out-of-memory handler.
//   3. suppresses the default error report behaviour.
//   4. Makes sure all file descriptors are closed on exit / error / interrupt.
//   5. Makes sure all objects are destructed on exit / error / interrupt.

class RdbInitializer {
public:
	RdbInitializer();
	~RdbInitializer();

	static bool   is_kid() { return s_is_kid; }
	static int    get_kid_idx() { return s_kid_index; }
    static void   get_open_fds(set<int> &fds);

	// allows to safely write to stdout even from a child process
	// (before doing so please make sure launch_process() does not close stdout)
	static void vdebug_print(const char *fmt, ...);

private:
	struct LiveStat {
		pid_t pid;
		int   index;

		LiveStat(pid_t _pid, int _index) : pid(_pid), index(_index) {}
	};

	struct Shm {
		char          error_msg[10000];
		uint64_t        res_offset;
		int64_t       total_mem_usage;                 // cumulative memory usage of the kids
		uint64_t        num_kids_running;
		uint64_t        num_kids_suspended;
		int           untouchable_kid_idx;
		bool          is_alive[MAX_KIDS];
		int64_t       mem_usage[MAX_KIDS];
		unsigned char kid_progress[MAX_KIDS];          // progress report for each pid
		uint64_t        kid_res_offset[MAX_KIDS];        // offset for kid's result
		uint64_t        kid_res_num_records[MAX_KIDS];   // size of kid's result in number of data
		char          res;
	};

    struct SigBlocker {
        SigBlocker() {
            sigemptyset(&sigset);
            sigaddset(&sigset, SIGCHLD);
            sigaddset(&sigset, SIGINT);
            sigprocmask(SIG_BLOCK, &sigset, &oldsigset);
        }

        ~SigBlocker() { sigprocmask(SIG_UNBLOCK, &sigset, NULL); }

        sigset_t sigset;
        sigset_t oldsigset;
    };

	// all delays are in milliseconds
	static const int64_t        LAUNCH_DELAY;
	static const int64_t        MEM_SYNC_DELAY;
	static const int64_t        REPORT_INTERVAL_DELAY;

	static uint64_t               s_shm_size;
	static uint64_t               s_res_const_size;
	static uint64_t               s_res_var_size;
	static uint64_t               s_max_res_size;
	static uint64_t               s_max_mem_usage;
	static bool                 s_is_kid;
	static pid_t                s_parent_pid;
	static sem_t               *s_shm_sem;
	static sem_t               *s_alloc_suspend_sem;

	static int                  s_kid_index;
	static vector<LiveStat>     s_running_pids;
	static Shm                 *s_shm;

	static struct sigaction     s_old_sigint_act;
	static struct sigaction     s_old_sigchld_act;

	static int                  s_ref_count;
	static int                  s_sigint_fired;
	static unsigned             s_protect_counter;

	mode_t                      m_old_umask;
	TGLException::Error_handler m_old_error_handler;
	unsigned                    m_old_protect_count;
	set<int>                    m_old_open_fds;

	static string  get_shm_sem_name();
	static string  get_alloc_suspend_sem_name();
	static void    sigint_handler(int);
	static void    sigchld_handler(int);
	static void    prepare4multitasking(uint64_t res_const_size, uint64_t res_var_size, uint64_t max_res_size, uint64_t max_mem_usage, unsigned num_planned_kids);
	static pid_t   launch_process();
    static void    check_kids_state(bool ignore_errors);
	static void    wait_for_kids(rdb::IntervUtils &iu);
	static int64_t update_kids_mem_usage();
	static int     get_num_kids() { return s_kid_index; }
	static void    handle_error(const char *msg);
	static void    update_progress(unsigned char progress);
	static void    update_res_data_size(uint64_t size);
	static void   *allocate_res(uint64_t res_num_records);
	static void   *get_kid_res(int kid_index);
	static uint64_t  get_kid_res_size(int kid_index);

	// report_alloc function keeps track of how much memory the child process has consumed so far.
	// Use positive value for new allocations and negative when the memory is freed.
	// If the total memory consumption of all the child processes exceeds the user defined limit, report_alloc
	// pauses the process (unless all the rest of the processes have been already paused).
	// This way the mechanism allows only one child to continue increasing the memory consumption.
	// When the child finishes and releases the memory, the paused processes are waken up.
	//
	// This mechanism should have been implemented using POSIX condition variables. However condition variables that can
	// be shared between processes are not compatible with some Linux systems. We don't want to rely on them. We implent the
	// mechanism with just one semaphore (s_alloc_suspend_sem) on which all the processes are going to sleep.
	// Since a check comes prior to the decision to sleep or allocate memory, without proper condition variables we are exposed
	// to race condition. But we consider the consequences of an error to be mild.
	// 1. If allocation is done whenever it should have been paused - not a big deal.
	// 2. If a child process is unnecesserily awaken - no problem: after a new check it will put itself to sleep again.
	// 3. If the process is paused due to race condition - this might create a deadlock. To battle it we are going to wake up
	//    everybody each 3 seconds in wait_for_kids(). So in the worst case even if we paused the process due to a very rare race condition,
	//    we will simply delay the execution by 3 seconds.
	//      Example: Consider 2 child processes and their interaction in time...
	//           T1 Child process 1 tries to make allocation and concludes that the limit is exceeded and child process 2 is still running.
	//           T2 Child process 2 dies
	//           T3 Parent process detects child process 2 death but doesn't wake up child process 1 because it is still not paused
	//           T4 Child process 1 pauses itself due to the decision it took at T1
	//           T5 (After 3 seconds) Parent process wakes up everybody (i.e. child process 1)
	//
	// Since report_alloc should practically be called before each and every memory allocation (or at least before big allocations)
	// incorporating this into the code might be somewhat problematic. We solve the problem by periodic checking of the memory usage
	// which is performed by the parent process. This check does not rely on what has been reported by report_alloc, but rather calls
	// Linux /proc interface to achieve the kids' memory consumption. Unfortunately this interface works only for Linux...
	// This check is performed every 3 seconds and the memory consumption counters are updated accordingly. By this one can guarantee
	// that high memory usage will be detected at some stage and the child processes will suspend themselves.
	//
	// The periodic memory consumption check has two issues:
	// 1. It works only on Linux. (We decided to ignore this problem for now.)
	// 2. It has a delay of 3 seconds. Since the memory consumption check is resource intensive we do not want to increase the rate of checking
	//    unless it is absolutely necessary.
	// 
	// The long delay between the checks creates an issue that child processes might breach the memory limit during these 3 seconds. The issue
	// is especially likely to happen right after the processes are created: very frequently a freshly created process performs significant
	// memory allocations that are required initiate its work. For example: each child process creates TrackExpressionScanner that might
	// load the whole track chromosome into memory (for example if the iterator is a sparse track). Thus if 10 processes are spawned,
	// 10 chromosomes might be loaded into memory which might cause high memory usage. Indeed after 3 seconds the parent will notice abnormal
	// memory usage, update memory usage counters and cause the child processes to pause themselves. Yet the damage is already done, the memory
	// is already consumed and the memory limit might be exceeded.
	//
	// We solve the problem via a "creation delay": before each child is created a delay is introduced. Child process number N delays itself
	// by N x creation_delay. Thus the first child is not being delayed at all while the last one delayes itself the longest.
	// By the end of the delay we hope that the previously created processes complete their initial bulk alloctions.
	// The memory consumption is then checked and the memory usage counters are updated accordingly. The newly created child in turn
	// will check the total memory usage and if it exceeds the limit it will pause itself. A similar delay is introduced after the child process
	// wakes up after suspension (which results from bridging the memory limit).
	//
	// The parent process in turn increases the rate of memory consumption checking while the processes are spawned or awaken. In the period
	// of "quiet" the rate slows down until it reaches 3 seconds.
	//
	// In addition the parent process chooses one process to be "untouchable", i.e. not suspendable. The untouchable child process never suspends
	// itself. This mechanism guarantees that each time only one child process is given a chance to finish its work without interrupts.
	// Switching from one process to another would not just possibly prolong the run-time, but it would certainly increase the total memory
	// consumption.
	//
	// After "untouchable" process dies, the next one is selected by choosing the process with the highest memory consumption.
	static void report_alloc(int64_t bytes);

	friend void rdb::check_interrupt();
	friend SEXP rdb::rprotect(SEXP &expr);
	friend void rdb::runprotect(int count);
	friend void rdb::runprotect(SEXP &expr);
	friend void rdb::runprotect(vector<SEXP> &exprs);
	friend void rdb::runprotect_all();
	friend void rdb::rerror(const char *fmt, ...);
	friend void rdb::verror(const char *fmt, ...);
	friend void rdb::prepare4multitasking(uint64_t res_const_size, uint64_t res_var_size, uint64_t max_res_size, uint64_t max_mem_usage, unsigned num_planned_kids);
	friend pid_t rdb::launch_process();
	friend void rdb::wait_for_kids(rdb::IntervUtils &iu);
	friend int rdb::get_num_kids();
	friend void rdb::update_progress(unsigned char progress);
	friend void rdb::update_res_data_size(uint64_t size);
	friend void *rdb::allocate_res(uint64_t res_num_records);
	friend void *rdb::get_kid_res(int kid_index);
	friend uint64_t rdb::get_kid_res_size(int kid_index);
	friend void rdb::report_alloc(int64_t bytes);

	friend class ChildShm;
};


// ------------------------------- IMPLEMENTATION --------------------------------

inline void rexit() {
	if (RdbInitializer::is_kid()){
		// Normally we should have called exit() here. However "R CMD check"
		// doesn't like calls to exit/abort/etc because they end R session
		// itself. It prints a warning message and packages with warning
		// messages cannot be submitted to CRAN. Yet the child process MUST end
		// the R sessions, that's the whole point. Solution? Send a signal to
		// itself. Fortunately "R CMD check" allows signals.
		kill(getpid(), MISHA_EXIT_SIG);
	} else {
		rdb::verror("rexit is called from parent process");
	}
}

inline void rdb::set_abs_timeout(int64_t delay_msec, struct timespec &req)
{
	req.tv_nsec += delay_msec * 1000000L;
	req.tv_sec += req.tv_nsec / 1000000000L;
	req.tv_nsec %= 1000000000L;
}

inline void rdb::set_rel_timeout(int64_t delay_msec, struct timespec &req)
{
	req.tv_sec = delay_msec / 1000;
	req.tv_nsec = (delay_msec - req.tv_sec * 1000) * 1000000L;
}

inline bool rdb::is_time_elapsed(int64_t delay_msec, const struct timespec &start_time)
{
	struct timespec t1 = start_time;
	struct timespec t2;
	set_abs_timeout(delay_msec, t1);
	clock_gettime(CLOCK_REALTIME, &t2);
	return t2.tv_sec > t1.tv_sec || (t2.tv_sec == t1.tv_sec && t2.tv_nsec > t1.tv_nsec);
}

inline string rdb::track2attrs_path(SEXP envir, const string &trackname) {
	return rdb::track2path(envir, trackname) + "/.attributes";
}

inline void rdb::prepare4multitasking(uint64_t res_const_size, uint64_t res_var_size, uint64_t max_res_size, uint64_t max_mem_usage, unsigned num_planned_kids)
{
	RdbInitializer::prepare4multitasking(res_const_size, res_var_size, max_res_size, max_mem_usage, num_planned_kids);
}

inline pid_t rdb::launch_process() { return RdbInitializer::launch_process(); }

inline void rdb::wait_for_kids(IntervUtils &iu) { RdbInitializer::wait_for_kids(iu); }

inline int rdb::get_num_kids() { return RdbInitializer::get_num_kids(); }

inline void rdb::update_progress(unsigned char progress) { RdbInitializer::update_progress(progress); }

inline void rdb::update_res_data_size(uint64_t size) { RdbInitializer::update_res_data_size(size); }

inline void *rdb::allocate_res(uint64_t res_num_records) { return RdbInitializer::allocate_res(res_num_records); }

inline void *rdb::get_kid_res(int kid_index) { return RdbInitializer::get_kid_res(kid_index); }

inline uint64_t rdb::get_kid_res_size(int kid_index) { return RdbInitializer::get_kid_res_size(kid_index); }

inline void rdb::report_alloc(int64_t bytes) { RdbInitializer::report_alloc(bytes); }

inline void rdb::monitor_memusage() { report_alloc(0); }

inline void RdbInitializer::update_progress(unsigned char progress)
{
	if (s_is_kid)
		// update of progress is atomic => don't use a semaphore
		s_shm->kid_progress[s_kid_index] = progress;
}

inline void RdbInitializer::update_res_data_size(uint64_t size)
{
	if (s_is_kid)
		// update of progress is atomic => don't use a semaphore
		s_shm->kid_res_num_records[s_kid_index] = size;
}

inline void *RdbInitializer::get_kid_res(int kid_index)
{
	return &s_shm->res + s_shm->kid_res_offset[kid_index];
}

inline uint64_t RdbInitializer::get_kid_res_size(int kid_index)
{
	return s_shm->kid_res_num_records[kid_index];
}

inline void RdbInitializer::report_alloc(int64_t bytes)
{
	if (s_is_kid) {
//vdebug_print("%*s%d (%d): ATTEMPT TO ALLOC %ld, total: %ld, running: %ld, suspended: %ld\n", s_kid_index + 1, "", (int)s_kid_index, (int)getpid(), bytes,
//s_shm->total_mem_usage, s_shm->num_kids_running, s_shm->num_kids_suspended);
		if (s_kid_index != s_shm->untouchable_kid_idx) {  // never suspend untouchable kid
			while ((uint64_t)s_shm->total_mem_usage + bytes > s_max_mem_usage && s_shm->num_kids_running > 1) {
				{
					SemLocker sl(s_shm_sem);
					s_shm->num_kids_running--;
					s_shm->num_kids_suspended++;
				}
//vdebug_print("%*s%d (%d): SUSPENDING on ALLOC %ld, total: %ld, running: %ld, suspended: %ld\n", s_kid_index + 1, "", (int)s_kid_index, (int)getpid(), bytes,
//s_shm->total_mem_usage, s_shm->num_kids_running, s_shm->num_kids_suspended);

				while (sem_wait(s_alloc_suspend_sem) < 0 && errno == EINTR)
					;

				{
					SemLocker sl(s_shm_sem);
					s_shm->num_kids_suspended--;
					s_shm->num_kids_running++;
				}

				int num_preceding_kids = 0;
				for (int i = 0; i < s_kid_index; ++i) {
					if (s_shm->is_alive[i]) 
						++num_preceding_kids;
				}

				if (num_preceding_kids) {
					struct timespec req;
					rdb::set_rel_timeout(MEM_SYNC_DELAY, req);

					for (int i = 0; i < num_preceding_kids; ++i) {
						if (RdbInitializer::s_sigint_fired)
							TGLError("Command interrupted!");
						nanosleep(&req, NULL);
					}
				}

//vdebug_print("%*s%d (%d): WOKE up on ALLOC %ld, total: %ld, running: %ld, suspended: %ld\n", s_kid_index + 1, "", (int)s_kid_index, (int)getpid(), bytes,
//s_shm->total_mem_usage, s_shm->num_kids_running, s_shm->num_kids_suspended);
				if (RdbInitializer::s_sigint_fired)
					TGLError("Command interrupted!");
			}
		}

		// It would be too expensive to protect the next statements with a mutex/semaphore.
		// We're ready to suffer some errors in memory accounting on behalf of speed.
		if (bytes) { 
			s_shm->total_mem_usage += bytes;
			s_shm->mem_usage[s_kid_index] += bytes;
		}
//vdebug_print("%*s%d (%d): ALLOC %ld, total per process: %ld, total: %ld\n", s_kid_index + 1, "",
//(int)s_kid_index, (int)getpid(), bytes, s_shm->mem_usage[s_kid_index], s_shm->total_mem_usage);
	}
}

#endif /* RDBUTILS_H_ */

