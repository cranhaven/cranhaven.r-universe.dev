/*
 * GenomeTrackSmooth.cpp
 *
 *  Created on: May 16, 2010
 *      Author: hoichman
 */

#include <sys/stat.h>
#include <sys/types.h>

#include <cmath>
#include <stdio.h>

#include <limits>
#include <string>
#include <vector>

#include "rdbinterval.h"
#include "rdbutils.h"
#include "GenomeTrackFixedBin.h"
#include "TrackExpressionFixedBinIterator.h"
#include "TrackExpressionIterator.h"
#include "TrackExpressionScanner.h"

using namespace std;
using namespace rdb;

enum Smooth_type { LINEAR_RAMP, MEAN, NUM_SMOOTH_TYPES, INVALID_TYPE };

const char *SMOOTH_TYPE_NAMES[NUM_SMOOTH_TYPES] = { "LINEAR_RAMP", "MEAN" };

//---------------------------------- Smoother ----------------------------------

class Smoother {
protected:
	GenomeTrackFixedBin &m_gtrack;
	int m_sample_skip;
	bool m_smooth_nans;
	int m_counter;

public:
	Smoother(GenomeTrackFixedBin &gtrack, int sample_skip, bool smooth_nans) : m_gtrack(gtrack), m_sample_skip(sample_skip), m_smooth_nans(smooth_nans), m_counter(0) {}
	virtual ~Smoother() {}

	virtual void set_next_sample(double sample) = 0;
};


//---------------------------------- Linear_ramp_smoother ----------------------

class Linear_ramp_smoother : public Smoother {
	vector<double> m_vals;

	unsigned m_num_read_samples;
	unsigned m_num_samples;
	unsigned m_num_samples_aside;
	double   m_weight_thr;

	// left = samples left to the peak
	// right = peak sample + samples right to the peak
	unsigned m_left_nans;
	unsigned m_right_nans;
	double   m_left_weights_sum;
	double   m_right_weights_sum;
	double   m_left_vals_sum;
	double   m_right_vals_sum;
	double   m_left_weighted_vals_sum;
	double   m_right_weighted_vals_sum;

	unsigned m_left_idx;
	unsigned m_peak_idx;

public:
	Linear_ramp_smoother(GenomeTrackFixedBin &gtrack, unsigned wnd_size, double weight_thr, int sample_skip, bool smooth_nans, IntervUtils &iu);
	~Linear_ramp_smoother();

	virtual void set_next_sample(double sample);
};


Linear_ramp_smoother::Linear_ramp_smoother(GenomeTrackFixedBin &gtrack, unsigned wnd_size, double weight_thr, int sample_skip, bool smooth_nans, IntervUtils &iu) :
	Smoother(gtrack, sample_skip, smooth_nans)
{
	m_num_samples_aside = (unsigned)(0.5 * wnd_size / gtrack.get_bin_size() + 0.5);
	m_num_samples = 2 * m_num_samples_aside + 1;

	if (!m_num_samples_aside)
		verror("Smoothing window is narrow and does not cover any other values around the center");

	m_weight_thr = weight_thr * (m_num_samples_aside + 1);

	iu.verify_max_data_size(m_num_samples, "Smoothing window");
	m_vals.resize(m_num_samples, numeric_limits<double>::quiet_NaN());

	m_left_nans = m_num_samples_aside;
	m_right_nans = m_num_samples_aside + 1;
	m_left_weights_sum = m_right_weights_sum = 0;
	m_left_vals_sum = m_right_vals_sum = 0;
	m_left_weighted_vals_sum = m_right_weighted_vals_sum = 0;

	m_left_idx = 0;
	m_peak_idx = m_left_idx + m_num_samples_aside;

	m_num_read_samples = 0;
}

Linear_ramp_smoother::~Linear_ramp_smoother()
{
	// write down the samples that are right to the peak
	for (unsigned i = 0; i <= m_num_samples_aside; ++i)
		set_next_sample(numeric_limits<double>::quiet_NaN());
}

void Linear_ramp_smoother::set_next_sample(double sample)
{
	if (m_num_read_samples > m_num_samples_aside) {
		float mean;
		double weight = m_left_weights_sum + m_right_weights_sum;

		if ((!m_smooth_nans && std::isnan(m_vals[m_peak_idx])) || !weight || weight < m_weight_thr)
			mean = numeric_limits<double>::quiet_NaN();
		else
			mean = (m_left_weighted_vals_sum + m_right_weighted_vals_sum) / weight;

		if (m_counter % m_sample_skip == 0)
    		m_gtrack.write_next_bin(mean);

		m_counter++;
	} else
		m_num_read_samples++;

	// shift all elements left to the peak one position left
	m_left_weights_sum -= m_num_samples_aside - m_left_nans;
	m_left_weighted_vals_sum -= m_left_vals_sum;

	// release the lefmost element
	if (std::isnan(m_vals[m_left_idx]))
		m_left_nans--;
	else
		m_left_vals_sum -= m_vals[m_left_idx];

	// shift the peak element left
	if (std::isnan(m_vals[m_peak_idx])) {
		m_left_nans++;
		m_right_nans--;
	}
	else {
		m_left_weights_sum += m_num_samples_aside;
		m_left_vals_sum += m_vals[m_peak_idx];
		m_left_weighted_vals_sum += m_vals[m_peak_idx] * m_num_samples_aside;

		m_right_weights_sum -= m_num_samples_aside + 1;
		m_right_vals_sum -= m_vals[m_peak_idx];
		m_right_weighted_vals_sum -= m_vals[m_peak_idx] * (m_num_samples_aside + 1);
	}

	// shift all elements right to the peak one position left
	m_right_weights_sum += m_num_samples_aside - m_right_nans;
	m_right_weighted_vals_sum += m_right_vals_sum;

	// add new element at the rightmost position
	if (std::isnan(sample))
		m_right_nans++;
	else {
		m_right_weights_sum++;
		m_right_vals_sum += sample;
		m_right_weighted_vals_sum += sample;
	}
	m_vals[m_left_idx] = sample;

	m_left_idx = (m_left_idx + 1) % m_num_samples;
	m_peak_idx = (m_peak_idx + 1) % m_num_samples;

	// over time the loss of precision in floating point operations is accumulated =>
	// recalculate all from scratch each 1/2 window size samples
	if (m_counter % m_num_samples_aside == 0) {
		// m_left/right_weights_sum never lose precision since they are always updated with only integer values,
		// the rest should be recalculated
		double val;

		m_left_vals_sum = m_right_vals_sum = 0;
		m_left_weighted_vals_sum = m_right_weighted_vals_sum = 0;

		for (unsigned i = 1; i <= m_num_samples_aside; ++i) {
			// left of the peak
			val = m_vals[(m_peak_idx + m_num_samples - i) % m_num_samples];
			if (!std::isnan(val))
				m_left_vals_sum += val;
			m_left_weighted_vals_sum += m_left_vals_sum;

			// right of the peak
			val = m_vals[(m_peak_idx + i) % m_num_samples];
			if (!std::isnan(val))
				m_right_vals_sum += val;
			m_right_weighted_vals_sum += m_right_vals_sum;
		}

		// update the peak itself (it is counted at the right)
		val = m_vals[m_peak_idx];
		if (!std::isnan(val)) {
			m_right_weighted_vals_sum += val * (m_num_samples_aside + 1);
			m_right_vals_sum += val;
		}
	}
}


//---------------------------------- Mean_smoother ----------------------

class Mean_smoother : public Smoother {
	vector<double> m_vals;

	double   m_weight_thr;
	unsigned m_num_read_samples;
	unsigned m_num_samples;
	unsigned m_num_samples_aside;

	double   m_weights_sum;
	double   m_vals_sum;

	unsigned m_left_idx;
	unsigned m_peak_idx;

public:
	Mean_smoother(GenomeTrackFixedBin &gtrack, unsigned wnd_size, double weight_thr, int sample_skip, bool smooth_nans, IntervUtils &iu);
	~Mean_smoother();

	virtual void set_next_sample(double sample);
};


Mean_smoother::Mean_smoother(GenomeTrackFixedBin &gtrack, unsigned wnd_size, double weight_thr, int sample_skip, bool smooth_nans, IntervUtils &iu) :
	Smoother(gtrack, sample_skip, smooth_nans)
{
	m_weight_thr = weight_thr;
	m_num_samples_aside = (unsigned)(0.5 * wnd_size / gtrack.get_bin_size() + 0.5);
	m_num_samples = 2 * m_num_samples_aside + 1;

	if (!m_num_samples_aside)
		verror("Smoothing window is narrow and does not cover any other values around the center");

	iu.verify_max_data_size(m_num_samples, "Smoothing window");
	m_vals.resize(m_num_samples, numeric_limits<double>::quiet_NaN());

	m_weights_sum = 0;
	m_vals_sum = 0;

	m_left_idx = 0;
	m_peak_idx = m_left_idx + m_num_samples_aside;

	m_num_read_samples = 0;
}

Mean_smoother::~Mean_smoother()
{
	// write down the samples that are right to the peak
	for (unsigned i = 0; i <= m_num_samples_aside; ++i)
		set_next_sample(numeric_limits<double>::quiet_NaN());
}

void Mean_smoother::set_next_sample(double sample)
{
	if (m_num_read_samples > m_num_samples_aside) {
		float mean = (!m_smooth_nans && std::isnan(m_vals[m_peak_idx])) || !m_weights_sum || m_weights_sum < m_weight_thr ? numeric_limits<double>::quiet_NaN() : m_vals_sum / m_weights_sum;

		if (m_counter % m_sample_skip == 0)
    		m_gtrack.write_next_bin(mean);

		m_counter++;
	} else
		m_num_read_samples++;

	if (!std::isnan(m_vals[m_left_idx])) {
		m_weights_sum--;
		m_vals_sum -= m_vals[m_left_idx];
	}

	if (!std::isnan(sample)) {
		m_weights_sum++;
		m_vals_sum += sample;
	}

	m_vals[m_left_idx] = sample;

	m_left_idx = (m_left_idx + 1) % m_num_samples;
	m_peak_idx = (m_peak_idx + 1) % m_num_samples;

	// over time the loss of precision in floating point operations is accumulated =>
	// recalculate all from scratch each 1/2 window size samples
	if (m_counter % m_num_samples_aside == 0) {
		m_vals_sum = 0;
		for (vector<double>::iterator i = m_vals.begin(); i != m_vals.end(); ++i) {
			if (!std::isnan(m_vals_sum))
				m_vals_sum += *i;
		}
	}
}



//------------------------------------ gsmooth ----------------------------------

extern "C" {

SEXP gsmooth(SEXP _track, SEXP _expr, SEXP _winsize, SEXP _weight_thr, SEXP _smooth_nans, SEXP _type, SEXP _iterator_policy, SEXP _envir)
{
	try {
		RdbInitializer rdb_init;

		if (!isString(_track) || length(_track) != 1)
			verror("Track argument is not a string");

		if (!isString(_expr) || length(_expr) != 1)
			verror("Track expression argument is not a string");

		if (!isReal(_winsize) || length(_winsize) != 1)
			verror("Winsize is not numeric");

		if (!isReal(_weight_thr) || length(_weight_thr) != 1)
			verror("Weight threshold is not numeric");

		if (!isLogical(_smooth_nans) || length(_smooth_nans) != 1)
			verror("Smooth nans argument is not logical");

		if (!isString(_type) || length(_type) != 1)
			verror("Algorithm is not a string");

		const char *track = CHAR(STRING_ELT(_track, 0));
		double winsize = REAL(_winsize)[0];
		double weight_thr = REAL(_weight_thr)[0];
		bool smooth_nans = LOGICAL(_smooth_nans)[0];

		if (winsize < 0)
			verror ("Winsize cannot be a negative number");

		string type = CHAR(STRING_ELT(_type, 0));
		Smooth_type smooth_type = INVALID_TYPE;

		for (int i = 0; type[i]; ++i)
			type[i] = toupper(type[i]);

		for (int i = 0; i < NUM_SMOOTH_TYPES; ++i) {
			if (type == SMOOTH_TYPE_NAMES[i]) {
				smooth_type = (Smooth_type)i;
				break;
			}
		}

		if (smooth_type == INVALID_TYPE)
			verror("Unrecognized smoothing algorithm: %s\n", type.c_str());

		string dirname = create_track_dir(_envir, track);

		IntervUtils iu(_envir);
		TrackExprScanner scanner(iu);
		int cur_scope_idx = -1;
		char filename[FILENAME_MAX];
		GenomeTrackFixedBin gtrack;
		GIntervals all_genome_intervs;
		iu.get_all_genome_intervs(all_genome_intervs);

		if (iu.get_multitasking() && !iu.prepare4multitasking(_expr, &all_genome_intervs, NULL, _iterator_policy))
			rreturn(R_NilValue);

		if (!iu.get_multitasking() || iu.distribute_task(0, 0)) {  // child process
			GIntervalsFetcher1D &scanner_intervals1d = iu.get_multitasking() ? *iu.get_kid_intervals1d() : all_genome_intervs;
			Smoother *smoother = NULL;

			scanner.begin(_expr, &scanner_intervals1d, NULL, _iterator_policy);

			if (scanner.get_iterator()->get_type() != TrackExpressionIteratorBase::FIXED_BIN)
				verror("gtrack.smooth() requires the iterator policy to be a fixed bin size.\n");

			unsigned bin_size = ((TrackExpressionFixedBinIterator *)scanner.get_iterator())->get_bin_size();

			for (; !scanner.isend(); scanner.next()) {
				if (cur_scope_idx != scanner.last_scope_idx()) {
					cur_scope_idx = scanner.last_scope_idx();

					// smoother must be deleted before gtrack.init_write() is called
					delete smoother;
					smoother = NULL;

					snprintf(filename, sizeof(filename), "%s/%s", dirname.c_str(), iu.id2chrom(scanner.last_interval1d().chromid).c_str());
					gtrack.init_write(filename, bin_size, scanner.last_interval1d().chromid);

					if (smooth_type == LINEAR_RAMP)
						smoother = new Linear_ramp_smoother(gtrack, (unsigned)winsize, weight_thr, 1, smooth_nans, iu);
					else if (smooth_type == MEAN)
						smoother = new Mean_smoother(gtrack, (unsigned)winsize, weight_thr, 1, smooth_nans, iu);
				}

				smoother->set_next_sample(scanner.last_real(0));
			}

			delete smoother; // destructor of smoother should be protected by try/catch (it flushes data to file)
		}
	} catch (TGLException &e) {
		rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }

	rreturn(R_NilValue);
}

}
