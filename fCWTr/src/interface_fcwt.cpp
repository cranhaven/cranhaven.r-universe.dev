#include "cpp11.hpp"
#include "fcwt.h"
#include <algorithm>
#include <complex>

double my_abs(const std::complex<float> &arg) {
  return std::abs(arg);
}

double my_real(const std::complex<float> &arg) {
  return std::real(arg);
}

double my_imag(const std::complex<float> &arg) {
  return std::imag(arg);
}

float dbl_to_float(const double &dbl) {
  return static_cast<float>(dbl);
}

[[cpp11::register]]
std::vector<double> fcwt_raw(
    std::vector<double> signal,
    int fs, // sample frequency
    double f0, // beginning of frequency range
    double f1, // end of frequency range
    int fn, // number of wavelets to generate across frequency range
    double sigma,
    int nthreads,
    bool optplans,
    bool abs)
{
  int n = signal.size(); //signal length

  // cast double input to float
  std::vector<float> signal_f(n);
  std::transform(signal.cbegin(), signal.cend(), signal_f.begin(), dbl_to_float);

  //output: n x scales complex numbers
  // std::vector<float> output(n*noctaves*nsuboctaves*2);
  std::vector<std::complex<float>> output(n*fn);

  //Create a wavelet object
  Wavelet *wavelet;

  //Initialize a Morlet wavelet having sigma=sigma;
  Morlet morl(sigma);
  wavelet = &morl;

  //Create the continuous wavelet transform object
  //constructor(wavelet, nthreads, optplan)
  //
  //Arguments
  //wavelet   - pointer to wavelet object
  //nthreads  - number of threads to use
  //optplan   - use FFTW optimization plans if true
  //normalization - take extra time to normalize time-frequency matrix
  FCWT fcwt(wavelet, nthreads, optplans, true);

  //Generate frequencies
  //constructor(wavelet, dist, fs, f0, f1, fn)
  //
  //Arguments
  //wavelet   - pointer to wavelet object
  //dist      - FCWT_LOGSCALES | FCWT_LINFREQS for logarithmic or linear distribution frequency range
  //fs        - sample frequency
  //f0        - beginning of frequency range
  //f1        - end of frequency range
  //fn        - number of wavelets to generate across frequency range
  Scales scs(wavelet, FCWT_LINFREQS, fs, f0, f1, fn);

  //Perform a CWT
  //cwt(input, length, output, scales)
  //
  //Arguments:
  //input     - floating pointer to input array
  //length    - integer signal length
  //output    - floating pointer to output array
  //scales    - pointer to scales object
  fcwt.cwt(&signal_f[0], n, &output[0], &scs);

  std::vector<double> abs_output;

  if (abs) {
    // convert to absolute value
    abs_output = std::vector<double> (n*fn);
    std::transform(output.cbegin(), output.cend(), abs_output.begin(), my_abs);
    //static_cast<float (*)(const &std::complex<float>)>(&std::abs));
  } else {
    // transform to an array of real and imaginary part
    abs_output = std::vector<double> (2 * n*fn);

    auto it = std::transform(output.cbegin(), output.cend(), abs_output.begin(), my_real);
    std::transform(output.cbegin(), output.cend(), it, my_imag);
  }


  return(abs_output);
}
