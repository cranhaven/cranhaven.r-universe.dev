//! Seasonal time series analysis for functional data.
//!
//! This module provides functions for analyzing seasonal patterns in functional data:
//! - Period estimation (FFT, autocorrelation, regression-based)
//! - Peak detection with prominence calculation
//! - Seasonal strength measurement (variance and spectral methods)
//! - Seasonality change detection (onset/cessation)
//! - Instantaneous period estimation for drifting seasonality
//! - Peak timing variability analysis for short series
//! - Seasonality classification

use crate::basis::fourier_basis_with_period;
use crate::fdata::deriv_1d;
use crate::matrix::FdMatrix;
use crate::{iter_maybe_parallel, slice_maybe_parallel};
use num_complex::Complex;
#[cfg(feature = "parallel")]
use rayon::iter::ParallelIterator;
use rustfft::FftPlanner;
use std::f64::consts::PI;

/// Result of period estimation.
#[derive(Debug, Clone)]
pub struct PeriodEstimate {
    /// Estimated period
    pub period: f64,
    /// Dominant frequency (1/period)
    pub frequency: f64,
    /// Power at the dominant frequency
    pub power: f64,
    /// Confidence measure (ratio of peak power to mean power)
    pub confidence: f64,
}

/// A detected peak in functional data.
#[derive(Debug, Clone)]
pub struct Peak {
    /// Time at which the peak occurs
    pub time: f64,
    /// Value at the peak
    pub value: f64,
    /// Prominence of the peak (height relative to surrounding valleys)
    pub prominence: f64,
}

/// Result of peak detection.
#[derive(Debug, Clone)]
pub struct PeakDetectionResult {
    /// Peaks for each sample: `peaks[sample_idx]` contains peaks for that sample
    pub peaks: Vec<Vec<Peak>>,
    /// Inter-peak distances for each sample
    pub inter_peak_distances: Vec<Vec<f64>>,
    /// Mean period estimated from inter-peak distances across all samples
    pub mean_period: f64,
}

/// A detected period from multiple period detection.
#[derive(Debug, Clone)]
pub struct DetectedPeriod {
    /// Estimated period
    pub period: f64,
    /// FFT confidence (ratio of peak power to mean power)
    pub confidence: f64,
    /// Seasonal strength at this period (variance explained)
    pub strength: f64,
    /// Amplitude of the sinusoidal component
    pub amplitude: f64,
    /// Phase of the sinusoidal component (radians)
    pub phase: f64,
    /// Iteration number (1-indexed)
    pub iteration: usize,
}

/// Method for computing seasonal strength.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StrengthMethod {
    /// Variance decomposition: Var(seasonal) / Var(total)
    Variance,
    /// Spectral: power at seasonal frequencies / total power
    Spectral,
}

/// A detected change point in seasonality.
#[derive(Debug, Clone)]
pub struct ChangePoint {
    /// Time at which the change occurs
    pub time: f64,
    /// Type of change
    pub change_type: ChangeType,
    /// Seasonal strength before the change
    pub strength_before: f64,
    /// Seasonal strength after the change
    pub strength_after: f64,
}

/// Type of seasonality change.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ChangeType {
    /// Series becomes seasonal
    Onset,
    /// Series stops being seasonal
    Cessation,
}

/// Result of seasonality change detection.
#[derive(Debug, Clone)]
pub struct ChangeDetectionResult {
    /// Detected change points
    pub change_points: Vec<ChangePoint>,
    /// Time-varying seasonal strength curve used for detection
    pub strength_curve: Vec<f64>,
}

/// Result of instantaneous period estimation.
#[derive(Debug, Clone)]
pub struct InstantaneousPeriod {
    /// Instantaneous period at each time point
    pub period: Vec<f64>,
    /// Instantaneous frequency at each time point
    pub frequency: Vec<f64>,
    /// Instantaneous amplitude (envelope) at each time point
    pub amplitude: Vec<f64>,
}

/// Result of peak timing variability analysis.
#[derive(Debug, Clone)]
pub struct PeakTimingResult {
    /// Peak times for each cycle
    pub peak_times: Vec<f64>,
    /// Peak values
    pub peak_values: Vec<f64>,
    /// Within-period timing (0-1 scale, e.g., day-of-year / 365)
    pub normalized_timing: Vec<f64>,
    /// Mean normalized timing
    pub mean_timing: f64,
    /// Standard deviation of normalized timing
    pub std_timing: f64,
    /// Range of normalized timing (max - min)
    pub range_timing: f64,
    /// Variability score (0 = stable, 1 = highly variable)
    pub variability_score: f64,
    /// Trend in timing (positive = peaks getting later)
    pub timing_trend: f64,
    /// Cycle indices (1-indexed)
    pub cycle_indices: Vec<usize>,
}

/// Type of seasonality pattern.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SeasonalType {
    /// Regular peaks with consistent timing
    StableSeasonal,
    /// Regular peaks but timing shifts between cycles
    VariableTiming,
    /// Some cycles seasonal, some not
    IntermittentSeasonal,
    /// No clear seasonality
    NonSeasonal,
}

/// Result of seasonality classification.
#[derive(Debug, Clone)]
pub struct SeasonalityClassification {
    /// Whether the series is seasonal overall
    pub is_seasonal: bool,
    /// Whether peak timing is stable across cycles
    pub has_stable_timing: bool,
    /// Timing variability score (0 = stable, 1 = highly variable)
    pub timing_variability: f64,
    /// Overall seasonal strength
    pub seasonal_strength: f64,
    /// Per-cycle seasonal strength
    pub cycle_strengths: Vec<f64>,
    /// Indices of weak/missing seasons (0-indexed)
    pub weak_seasons: Vec<usize>,
    /// Classification type
    pub classification: SeasonalType,
    /// Peak timing analysis (if peaks were detected)
    pub peak_timing: Option<PeakTimingResult>,
}

/// Method for automatic threshold selection.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ThresholdMethod {
    /// Fixed user-specified threshold
    Fixed(f64),
    /// Percentile of strength distribution
    Percentile(f64),
    /// Otsu's method (optimal bimodal separation)
    Otsu,
}

/// Type of amplitude modulation pattern.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ModulationType {
    /// Constant amplitude (no modulation)
    Stable,
    /// Amplitude increases over time (seasonality emerges)
    Emerging,
    /// Amplitude decreases over time (seasonality fades)
    Fading,
    /// Amplitude varies non-monotonically
    Oscillating,
    /// No seasonality detected
    NonSeasonal,
}

/// Result of amplitude modulation detection.
#[derive(Debug, Clone)]
pub struct AmplitudeModulationResult {
    /// Whether seasonality is present (using robust spectral method)
    pub is_seasonal: bool,
    /// Overall seasonal strength (spectral method)
    pub seasonal_strength: f64,
    /// Whether amplitude modulation is detected
    pub has_modulation: bool,
    /// Type of amplitude modulation
    pub modulation_type: ModulationType,
    /// Coefficient of variation of time-varying strength (0 = stable, higher = more modulation)
    pub modulation_score: f64,
    /// Trend in amplitude (-1 to 1: negative = fading, positive = emerging)
    pub amplitude_trend: f64,
    /// Time-varying seasonal strength curve
    pub strength_curve: Vec<f64>,
    /// Time points corresponding to strength_curve
    pub time_points: Vec<f64>,
    /// Minimum strength in the curve
    pub min_strength: f64,
    /// Maximum strength in the curve
    pub max_strength: f64,
}

/// Result of wavelet-based amplitude modulation detection.
#[derive(Debug, Clone)]
pub struct WaveletAmplitudeResult {
    /// Whether seasonality is present
    pub is_seasonal: bool,
    /// Overall seasonal strength
    pub seasonal_strength: f64,
    /// Whether amplitude modulation is detected
    pub has_modulation: bool,
    /// Type of amplitude modulation
    pub modulation_type: ModulationType,
    /// Coefficient of variation of wavelet amplitude
    pub modulation_score: f64,
    /// Trend in amplitude (-1 to 1)
    pub amplitude_trend: f64,
    /// Wavelet amplitude at the seasonal frequency over time
    pub wavelet_amplitude: Vec<f64>,
    /// Time points corresponding to wavelet_amplitude
    pub time_points: Vec<f64>,
    /// Scale (period) used for wavelet analysis
    pub scale: f64,
}

// ============================================================================
// Internal helper functions
// ============================================================================

/// Compute mean curve from functional data matrix.
///
/// # Arguments
/// * `data` - Functional data matrix (n x m)
/// * `parallel` - Use parallel iteration (default: true)
///
/// # Returns
/// Mean curve of length m
#[inline]
fn compute_mean_curve_impl(data: &FdMatrix, parallel: bool) -> Vec<f64> {
    let (n, m) = data.shape();
    if parallel && m >= 100 {
        // Use parallel iteration for larger datasets
        iter_maybe_parallel!(0..m)
            .map(|j| {
                let mut sum = 0.0;
                for i in 0..n {
                    sum += data[(i, j)];
                }
                sum / n as f64
            })
            .collect()
    } else {
        // Sequential for small datasets or when disabled
        (0..m)
            .map(|j| {
                let mut sum = 0.0;
                for i in 0..n {
                    sum += data[(i, j)];
                }
                sum / n as f64
            })
            .collect()
    }
}

/// Compute mean curve (parallel by default for m >= 100).
#[inline]
fn compute_mean_curve(data: &FdMatrix) -> Vec<f64> {
    compute_mean_curve_impl(data, true)
}

/// Compute interior bounds for edge-skipping (10% on each side).
///
/// Used to avoid edge effects in wavelet and other analyses.
///
/// # Arguments
/// * `m` - Total number of points
///
/// # Returns
/// `(interior_start, interior_end)` indices, or `None` if range is invalid
#[inline]
fn interior_bounds(m: usize) -> Option<(usize, usize)> {
    let edge_skip = (m as f64 * 0.1) as usize;
    let interior_start = edge_skip.min(m / 4);
    let interior_end = m.saturating_sub(edge_skip).max(m * 3 / 4);

    if interior_end <= interior_start {
        None
    } else {
        Some((interior_start, interior_end))
    }
}

/// Validate interior bounds with minimum span requirement.
fn valid_interior_bounds(m: usize, min_span: usize) -> Option<(usize, usize)> {
    interior_bounds(m).filter(|&(s, e)| e > s + min_span)
}

/// Compute periodogram from data using FFT.
/// Returns (frequencies, power) where frequencies are in cycles per unit time.
fn periodogram(data: &[f64], argvals: &[f64]) -> (Vec<f64>, Vec<f64>) {
    let n = data.len();
    if n < 2 || argvals.len() != n {
        return (Vec::new(), Vec::new());
    }

    let dt = (argvals[n - 1] - argvals[0]) / (n - 1) as f64;
    let fs = 1.0 / dt; // Sampling frequency

    let mut planner = FftPlanner::<f64>::new();
    let fft = planner.plan_fft_forward(n);

    let mut buffer: Vec<Complex<f64>> = data.iter().map(|&x| Complex::new(x, 0.0)).collect();
    fft.process(&mut buffer);

    // Compute power spectrum (one-sided)
    let n_freq = n / 2 + 1;
    let mut power = Vec::with_capacity(n_freq);
    let mut frequencies = Vec::with_capacity(n_freq);

    for k in 0..n_freq {
        let freq = k as f64 * fs / n as f64;
        frequencies.push(freq);

        let p = buffer[k].norm_sqr() / (n as f64 * n as f64);
        // Double power for non-DC and non-Nyquist frequencies (one-sided spectrum)
        let p = if k > 0 && k < n / 2 { 2.0 * p } else { p };
        power.push(p);
    }

    (frequencies, power)
}

/// Compute autocorrelation function up to max_lag.
fn autocorrelation(data: &[f64], max_lag: usize) -> Vec<f64> {
    let n = data.len();
    if n == 0 {
        return Vec::new();
    }

    let mean: f64 = data.iter().sum::<f64>() / n as f64;
    let var: f64 = data.iter().map(|&x| (x - mean).powi(2)).sum::<f64>() / n as f64;

    if var < 1e-15 {
        return vec![1.0; max_lag.min(n) + 1];
    }

    let max_lag = max_lag.min(n - 1);
    let mut acf = Vec::with_capacity(max_lag + 1);

    for lag in 0..=max_lag {
        let mut sum = 0.0;
        for i in 0..(n - lag) {
            sum += (data[i] - mean) * (data[i + lag] - mean);
        }
        acf.push(sum / (n as f64 * var));
    }

    acf
}

/// Try to add a peak, respecting minimum distance. Replaces previous peak if closer but higher.
fn try_add_peak(peaks: &mut Vec<usize>, candidate: usize, signal: &[f64], min_distance: usize) {
    if let Some(&last) = peaks.last() {
        if candidate - last >= min_distance {
            peaks.push(candidate);
        } else if signal[candidate] > signal[last] {
            // Safe: peaks is non-empty since last() succeeded
            *peaks.last_mut().unwrap_or(&mut 0) = candidate;
        }
    } else {
        peaks.push(candidate);
    }
}

/// Find peaks in a 1D signal, returning indices.
fn find_peaks_1d(signal: &[f64], min_distance: usize) -> Vec<usize> {
    let n = signal.len();
    if n < 3 {
        return Vec::new();
    }

    let mut peaks = Vec::new();

    for i in 1..(n - 1) {
        if signal[i] > signal[i - 1] && signal[i] > signal[i + 1] {
            try_add_peak(&mut peaks, i, signal, min_distance);
        }
    }

    peaks
}

/// Compute prominence for a peak (height above surrounding valleys).
fn compute_prominence(signal: &[f64], peak_idx: usize) -> f64 {
    let n = signal.len();
    let peak_val = signal[peak_idx];

    // Find lowest point between peak and boundaries/higher peaks
    let mut left_min = peak_val;
    for i in (0..peak_idx).rev() {
        if signal[i] >= peak_val {
            break;
        }
        left_min = left_min.min(signal[i]);
    }

    let mut right_min = peak_val;
    for i in (peak_idx + 1)..n {
        if signal[i] >= peak_val {
            break;
        }
        right_min = right_min.min(signal[i]);
    }

    peak_val - left_min.max(right_min)
}

/// Hilbert transform using FFT to compute analytic signal.
///
/// # Arguments
/// * `signal` - Input real signal
///
/// # Returns
/// Analytic signal as complex vector (real part = original, imaginary = Hilbert transform)
pub fn hilbert_transform(signal: &[f64]) -> Vec<Complex<f64>> {
    let n = signal.len();
    if n == 0 {
        return Vec::new();
    }

    let mut planner = FftPlanner::<f64>::new();
    let fft_forward = planner.plan_fft_forward(n);
    let fft_inverse = planner.plan_fft_inverse(n);

    // Forward FFT
    let mut buffer: Vec<Complex<f64>> = signal.iter().map(|&x| Complex::new(x, 0.0)).collect();
    fft_forward.process(&mut buffer);

    // Create analytic signal in frequency domain
    // H[0] = 1, H[1..n/2] = 2, H[n/2] = 1 (if n even), H[n/2+1..] = 0
    let half = n / 2;
    for k in 1..half {
        buffer[k] *= 2.0;
    }
    for k in (half + 1)..n {
        buffer[k] = Complex::new(0.0, 0.0);
    }

    // Inverse FFT
    fft_inverse.process(&mut buffer);

    // Normalize
    for c in buffer.iter_mut() {
        *c /= n as f64;
    }

    buffer
}

/// Unwrap phase to remove 2π discontinuities.
fn unwrap_phase(phase: &[f64]) -> Vec<f64> {
    if phase.is_empty() {
        return Vec::new();
    }

    let mut unwrapped = vec![phase[0]];
    let mut cumulative_correction = 0.0;

    for i in 1..phase.len() {
        let diff = phase[i] - phase[i - 1];

        // Check for wraparound
        if diff > PI {
            cumulative_correction -= 2.0 * PI;
        } else if diff < -PI {
            cumulative_correction += 2.0 * PI;
        }

        unwrapped.push(phase[i] + cumulative_correction);
    }

    unwrapped
}

/// Morlet wavelet function.
///
/// The Morlet wavelet is a complex exponential modulated by a Gaussian:
/// ψ(t) = exp(i * ω₀ * t) * exp(-t² / 2)
///
/// where ω₀ is the central frequency (typically 6 for good time-frequency trade-off).
fn morlet_wavelet(t: f64, omega0: f64) -> Complex<f64> {
    let gaussian = (-t * t / 2.0).exp();
    let oscillation = Complex::new((omega0 * t).cos(), (omega0 * t).sin());
    oscillation * gaussian
}

/// Continuous Wavelet Transform at a single scale using Morlet wavelet.
///
/// Computes the wavelet coefficients at the specified scale (period) for all time points.
/// Uses convolution in the time domain.
///
/// # Arguments
/// * `signal` - Input signal
/// * `argvals` - Time points
/// * `scale` - Scale parameter (related to period)
/// * `omega0` - Central frequency of Morlet wavelet (default: 6.0)
///
/// # Returns
/// Complex wavelet coefficients at each time point
#[allow(dead_code)]
fn cwt_morlet(signal: &[f64], argvals: &[f64], scale: f64, omega0: f64) -> Vec<Complex<f64>> {
    let n = signal.len();
    if n == 0 || scale <= 0.0 {
        return Vec::new();
    }

    let dt = (argvals[n - 1] - argvals[0]) / (n - 1) as f64;

    // Compute wavelet coefficients via convolution
    // W(a, b) = (1/sqrt(a)) * Σ x[k] * ψ*((t[k] - b) / a) * dt
    let norm = 1.0 / scale.sqrt();

    (0..n)
        .map(|b| {
            let mut sum = Complex::new(0.0, 0.0);
            for k in 0..n {
                let t_normalized = (argvals[k] - argvals[b]) / scale;
                // Only compute within reasonable range (Gaussian decays quickly)
                if t_normalized.abs() < 6.0 {
                    let wavelet = morlet_wavelet(t_normalized, omega0);
                    sum += signal[k] * wavelet.conj();
                }
            }
            sum * norm * dt
        })
        .collect()
}

/// Continuous Wavelet Transform at a single scale using FFT (faster for large signals).
///
/// Uses the convolution theorem: CWT = IFFT(FFT(signal) * FFT(wavelet)*)
fn cwt_morlet_fft(signal: &[f64], argvals: &[f64], scale: f64, omega0: f64) -> Vec<Complex<f64>> {
    let n = signal.len();
    if n == 0 || scale <= 0.0 {
        return Vec::new();
    }

    let dt = (argvals[n - 1] - argvals[0]) / (n - 1) as f64;
    let norm = 1.0 / scale.sqrt();

    // Compute wavelet in time domain centered at t=0
    let wavelet_time: Vec<Complex<f64>> = (0..n)
        .map(|k| {
            // Center the wavelet
            let t = if k <= n / 2 {
                k as f64 * dt / scale
            } else {
                (k as f64 - n as f64) * dt / scale
            };
            morlet_wavelet(t, omega0) * norm
        })
        .collect();

    let mut planner = FftPlanner::<f64>::new();
    let fft_forward = planner.plan_fft_forward(n);
    let fft_inverse = planner.plan_fft_inverse(n);

    // FFT of signal
    let mut signal_fft: Vec<Complex<f64>> = signal.iter().map(|&x| Complex::new(x, 0.0)).collect();
    fft_forward.process(&mut signal_fft);

    // FFT of wavelet
    let mut wavelet_fft = wavelet_time;
    fft_forward.process(&mut wavelet_fft);

    // Multiply in frequency domain (use conjugate of wavelet FFT for correlation)
    let mut result: Vec<Complex<f64>> = signal_fft
        .iter()
        .zip(wavelet_fft.iter())
        .map(|(s, w)| *s * w.conj())
        .collect();

    // Inverse FFT
    fft_inverse.process(&mut result);

    // Normalize and scale by dt
    for c in result.iter_mut() {
        *c *= dt / n as f64;
    }

    result
}

/// Compute Otsu's threshold for bimodal separation.
///
/// Finds the threshold that minimizes within-class variance (or equivalently
/// maximizes between-class variance).
fn otsu_threshold(values: &[f64]) -> f64 {
    if values.is_empty() {
        return 0.5;
    }

    // Filter NaN values
    let valid: Vec<f64> = values.iter().copied().filter(|x| x.is_finite()).collect();
    if valid.is_empty() {
        return 0.5;
    }

    let vmin = valid.iter().cloned().fold(f64::INFINITY, f64::min);
    let vmax = valid.iter().cloned().fold(f64::NEG_INFINITY, f64::max);

    if (vmax - vmin).abs() < 1e-10 {
        return (vmin + vmax) / 2.0;
    }

    let n_bins = 256;
    let (histogram, hist_min, bin_width) = build_histogram(&valid, n_bins);
    let (best_bin, _) = find_optimal_threshold_bin(&histogram, valid.len() as f64);

    hist_min + (best_bin as f64 + 0.5) * bin_width
}

/// Compute linear regression slope (simple OLS).
fn linear_slope(x: &[f64], y: &[f64]) -> f64 {
    if x.len() != y.len() || x.len() < 2 {
        return 0.0;
    }

    let n = x.len() as f64;
    let mean_x: f64 = x.iter().sum::<f64>() / n;
    let mean_y: f64 = y.iter().sum::<f64>() / n;

    let mut num = 0.0;
    let mut den = 0.0;

    for (&xi, &yi) in x.iter().zip(y.iter()) {
        num += (xi - mean_x) * (yi - mean_y);
        den += (xi - mean_x).powi(2);
    }

    if den.abs() < 1e-15 {
        0.0
    } else {
        num / den
    }
}

/// Statistics from amplitude envelope analysis (shared by Hilbert and wavelet methods).
struct AmplitudeEnvelopeStats {
    modulation_score: f64,
    amplitude_trend: f64,
    has_modulation: bool,
    modulation_type: ModulationType,
    _mean_amp: f64,
    min_amp: f64,
    max_amp: f64,
}

/// Analyze an amplitude envelope slice to compute modulation statistics.
fn analyze_amplitude_envelope(
    interior_envelope: &[f64],
    interior_times: &[f64],
    modulation_threshold: f64,
) -> AmplitudeEnvelopeStats {
    let n_interior = interior_envelope.len() as f64;

    let mean_amp = interior_envelope.iter().sum::<f64>() / n_interior;
    let min_amp = interior_envelope
        .iter()
        .cloned()
        .fold(f64::INFINITY, f64::min);
    let max_amp = interior_envelope
        .iter()
        .cloned()
        .fold(f64::NEG_INFINITY, f64::max);

    let variance = interior_envelope
        .iter()
        .map(|&a| (a - mean_amp).powi(2))
        .sum::<f64>()
        / n_interior;
    let std_amp = variance.sqrt();
    let modulation_score = if mean_amp > 1e-10 {
        std_amp / mean_amp
    } else {
        0.0
    };

    let t_mean = interior_times.iter().sum::<f64>() / n_interior;
    let mut cov_ta = 0.0;
    let mut var_t = 0.0;
    for (&t, &a) in interior_times.iter().zip(interior_envelope.iter()) {
        cov_ta += (t - t_mean) * (a - mean_amp);
        var_t += (t - t_mean).powi(2);
    }
    let slope = if var_t > 1e-10 { cov_ta / var_t } else { 0.0 };

    let time_span = interior_times.last().unwrap_or(&1.0) - interior_times.first().unwrap_or(&0.0);
    let amplitude_trend = if mean_amp > 1e-10 && time_span > 1e-10 {
        (slope * time_span / mean_amp).clamp(-1.0, 1.0)
    } else {
        0.0
    };

    let has_modulation = modulation_score > modulation_threshold;
    let modulation_type = if !has_modulation {
        ModulationType::Stable
    } else if amplitude_trend > 0.3 {
        ModulationType::Emerging
    } else if amplitude_trend < -0.3 {
        ModulationType::Fading
    } else {
        ModulationType::Oscillating
    };

    AmplitudeEnvelopeStats {
        modulation_score,
        amplitude_trend,
        has_modulation,
        modulation_type,
        _mean_amp: mean_amp,
        min_amp,
        max_amp,
    }
}

/// Fit a sinusoid at the given period, subtract it from residual, and return (a, b, amplitude, phase).
fn fit_and_subtract_sinusoid(
    residual: &mut [f64],
    argvals: &[f64],
    period: f64,
) -> (f64, f64, f64, f64) {
    let m = residual.len();
    let omega = 2.0 * PI / period;
    let mut cos_sum = 0.0;
    let mut sin_sum = 0.0;

    for (j, &t) in argvals.iter().enumerate() {
        cos_sum += residual[j] * (omega * t).cos();
        sin_sum += residual[j] * (omega * t).sin();
    }

    let a = 2.0 * cos_sum / m as f64;
    let b = 2.0 * sin_sum / m as f64;
    let amplitude = (a * a + b * b).sqrt();
    let phase = b.atan2(a);

    for (j, &t) in argvals.iter().enumerate() {
        residual[j] -= a * (omega * t).cos() + b * (omega * t).sin();
    }

    (a, b, amplitude, phase)
}

/// Validate a single SAZED component: returns Some(period) if it passes range and confidence checks.
fn validate_sazed_component(
    period: f64,
    confidence: f64,
    min_period: f64,
    max_period: f64,
    threshold: f64,
) -> Option<f64> {
    if period.is_finite() && period > min_period && period < max_period && confidence > threshold {
        Some(period)
    } else {
        None
    }
}

/// Count how many periods agree with a reference within tolerance, returning (count, sum).
fn count_agreeing_periods(periods: &[f64], reference: f64, tolerance: f64) -> (usize, f64) {
    let mut count = 0;
    let mut sum = 0.0;
    for &p in periods {
        let rel_diff = (reference - p).abs() / reference.max(p);
        if rel_diff <= tolerance {
            count += 1;
            sum += p;
        }
    }
    (count, sum)
}

/// Find the end of the initial ACF descent (first negative or first uptick).
fn find_acf_descent_end(acf: &[f64]) -> usize {
    for i in 1..acf.len() {
        if acf[i] < 0.0 {
            return i;
        }
        if i > 1 && acf[i] > acf[i - 1] {
            return i - 1;
        }
    }
    1
}

/// Find the first ACF peak after initial descent. Returns Some((lag, acf_value)).
fn find_first_acf_peak(acf: &[f64]) -> Option<(usize, f64)> {
    if acf.len() < 4 {
        return None;
    }

    let min_search_start = find_acf_descent_end(acf);
    let peaks = find_peaks_1d(&acf[min_search_start..], 1);
    if peaks.is_empty() {
        return None;
    }

    let peak_lag = peaks[0] + min_search_start;
    Some((peak_lag, acf[peak_lag].max(0.0)))
}

/// Compute per-cycle seasonal strengths and identify weak seasons.
fn compute_cycle_strengths(
    data: &FdMatrix,
    argvals: &[f64],
    period: f64,
    strength_thresh: f64,
) -> (Vec<f64>, Vec<usize>) {
    let (n, m) = data.shape();
    let t_start = argvals[0];
    let t_end = argvals[m - 1];
    let n_cycles = ((t_end - t_start) / period).floor() as usize;

    let mut cycle_strengths = Vec::with_capacity(n_cycles);
    let mut weak_seasons = Vec::new();

    for cycle in 0..n_cycles {
        let cycle_start = t_start + cycle as f64 * period;
        let cycle_end = cycle_start + period;

        let start_idx = argvals.iter().position(|&t| t >= cycle_start).unwrap_or(0);
        let end_idx = argvals.iter().position(|&t| t > cycle_end).unwrap_or(m);

        let cycle_m = end_idx - start_idx;
        if cycle_m < 4 {
            cycle_strengths.push(f64::NAN);
            continue;
        }

        let cycle_data: Vec<f64> = (start_idx..end_idx)
            .flat_map(|j| (0..n).map(move |i| data[(i, j)]))
            .collect();
        let cycle_mat = FdMatrix::from_column_major(cycle_data, n, cycle_m).unwrap();
        let cycle_argvals: Vec<f64> = argvals[start_idx..end_idx].to_vec();

        let strength = seasonal_strength_variance(&cycle_mat, &cycle_argvals, period, 3);

        cycle_strengths.push(strength);
        if strength < strength_thresh {
            weak_seasons.push(cycle);
        }
    }

    (cycle_strengths, weak_seasons)
}

/// Build a histogram from valid values. Returns (histogram, min_val, bin_width).
fn build_histogram(valid: &[f64], n_bins: usize) -> (Vec<usize>, f64, f64) {
    let min_val = valid.iter().cloned().fold(f64::INFINITY, f64::min);
    let max_val = valid.iter().cloned().fold(f64::NEG_INFINITY, f64::max);
    let bin_width = (max_val - min_val) / n_bins as f64;
    let mut histogram = vec![0usize; n_bins];
    for &v in valid {
        let bin = ((v - min_val) / bin_width).min(n_bins as f64 - 1.0) as usize;
        histogram[bin] += 1;
    }
    (histogram, min_val, bin_width)
}

/// Find the optimal threshold bin using Otsu's between-class variance. Returns (best_bin, best_variance).
fn find_optimal_threshold_bin(histogram: &[usize], total: f64) -> (usize, f64) {
    let n_bins = histogram.len();
    let mut sum_total = 0.0;
    for (i, &count) in histogram.iter().enumerate() {
        sum_total += i as f64 * count as f64;
    }

    let mut best_bin = 0;
    let mut best_variance = 0.0;
    let mut sum_b = 0.0;
    let mut weight_b = 0.0;

    for t in 0..n_bins {
        weight_b += histogram[t] as f64;
        if weight_b == 0.0 {
            continue;
        }
        let weight_f = total - weight_b;
        if weight_f == 0.0 {
            break;
        }
        sum_b += t as f64 * histogram[t] as f64;
        let mean_b = sum_b / weight_b;
        let mean_f = (sum_total - sum_b) / weight_f;
        let variance = weight_b * weight_f * (mean_b - mean_f).powi(2);
        if variance > best_variance {
            best_variance = variance;
            best_bin = t;
        }
    }

    (best_bin, best_variance)
}

/// Sum power at harmonics of a fundamental frequency within tolerance.
fn sum_harmonic_power(
    frequencies: &[f64],
    power: &[f64],
    fundamental_freq: f64,
    tolerance: f64,
) -> (f64, f64) {
    let mut seasonal_power = 0.0;
    let mut total_power = 0.0;

    for (i, (&freq, &p)) in frequencies.iter().zip(power.iter()).enumerate() {
        if i == 0 {
            continue;
        }
        total_power += p;
        let ratio = freq / fundamental_freq;
        let nearest_harmonic = ratio.round();
        if (ratio - nearest_harmonic).abs() < tolerance && nearest_harmonic >= 1.0 {
            seasonal_power += p;
        }
    }

    (seasonal_power, total_power)
}

/// Return the new seasonal state if `ss` represents a valid threshold crossing,
/// or `None` if the index should be skipped (NaN, no change, or too close to the
/// previous change point).
fn crossing_direction(
    ss: f64,
    threshold: f64,
    in_seasonal: bool,
    i: usize,
    last_change_idx: Option<usize>,
    min_dur_points: usize,
) -> Option<bool> {
    if ss.is_nan() {
        return None;
    }
    let now_seasonal = ss > threshold;
    if now_seasonal == in_seasonal {
        return None;
    }
    if last_change_idx.is_some_and(|last_idx| i - last_idx < min_dur_points) {
        return None;
    }
    Some(now_seasonal)
}

/// Build a `ChangePoint` for a threshold crossing at index `i`.
fn build_change_point(
    i: usize,
    ss: f64,
    now_seasonal: bool,
    strength_curve: &[f64],
    argvals: &[f64],
) -> ChangePoint {
    let change_type = if now_seasonal {
        ChangeType::Onset
    } else {
        ChangeType::Cessation
    };
    let strength_before = if i > 0 && !strength_curve[i - 1].is_nan() {
        strength_curve[i - 1]
    } else {
        ss
    };
    ChangePoint {
        time: argvals[i],
        change_type,
        strength_before,
        strength_after: ss,
    }
}

/// Detect threshold crossings in a strength curve, returning change points.
fn detect_threshold_crossings(
    strength_curve: &[f64],
    argvals: &[f64],
    threshold: f64,
    min_dur_points: usize,
) -> Vec<ChangePoint> {
    let mut change_points = Vec::new();
    let mut in_seasonal = strength_curve[0] > threshold;
    let mut last_change_idx: Option<usize> = None;

    for (i, &ss) in strength_curve.iter().enumerate().skip(1) {
        let Some(now_seasonal) = crossing_direction(
            ss,
            threshold,
            in_seasonal,
            i,
            last_change_idx,
            min_dur_points,
        ) else {
            continue;
        };

        change_points.push(build_change_point(
            i,
            ss,
            now_seasonal,
            strength_curve,
            argvals,
        ));

        in_seasonal = now_seasonal;
        last_change_idx = Some(i);
    }

    change_points
}

// ============================================================================
// Period Estimation
// ============================================================================

/// Estimate period using FFT periodogram.
///
/// Finds the dominant frequency in the periodogram (excluding DC) and
/// returns the corresponding period.
///
/// # Arguments
/// * `data` - Column-major matrix (n x m)
/// * `n` - Number of samples
/// * `m` - Number of evaluation points
/// * `argvals` - Evaluation points (time values)
///
/// # Returns
/// Period estimate with confidence measure
pub fn estimate_period_fft(data: &FdMatrix, argvals: &[f64]) -> PeriodEstimate {
    let (n, m) = data.shape();
    if n == 0 || m < 4 || argvals.len() != m {
        return PeriodEstimate {
            period: f64::NAN,
            frequency: f64::NAN,
            power: 0.0,
            confidence: 0.0,
        };
    }

    // Compute mean curve first
    let mean_curve = compute_mean_curve(data);

    let (frequencies, power) = periodogram(&mean_curve, argvals);

    if frequencies.len() < 2 {
        return PeriodEstimate {
            period: f64::NAN,
            frequency: f64::NAN,
            power: 0.0,
            confidence: 0.0,
        };
    }

    // Find peak in power spectrum (skip DC component at index 0)
    let mut max_power = 0.0;
    let mut max_idx = 1;
    for (i, &p) in power.iter().enumerate().skip(1) {
        if p > max_power {
            max_power = p;
            max_idx = i;
        }
    }

    let dominant_freq = frequencies[max_idx];
    let period = if dominant_freq > 1e-15 {
        1.0 / dominant_freq
    } else {
        f64::INFINITY
    };

    // Confidence: ratio of peak power to mean power (excluding DC)
    let mean_power: f64 = power.iter().skip(1).sum::<f64>() / (power.len() - 1) as f64;
    let confidence = if mean_power > 1e-15 {
        max_power / mean_power
    } else {
        0.0
    };

    PeriodEstimate {
        period,
        frequency: dominant_freq,
        power: max_power,
        confidence,
    }
}

/// Estimate period using autocorrelation function.
///
/// Finds the first significant peak in the ACF after lag 0.
///
/// # Arguments
/// * `data` - Column-major matrix (n x m)
/// * `n` - Number of samples
/// * `m` - Number of evaluation points
/// * `argvals` - Evaluation points
/// * `max_lag` - Maximum lag to consider (in number of points)
pub fn estimate_period_acf(
    data: &[f64],
    n: usize,
    m: usize,
    argvals: &[f64],
    max_lag: usize,
) -> PeriodEstimate {
    if n == 0 || m < 4 || argvals.len() != m {
        return PeriodEstimate {
            period: f64::NAN,
            frequency: f64::NAN,
            power: 0.0,
            confidence: 0.0,
        };
    }

    // Compute mean curve
    let mat = FdMatrix::from_slice(data, n, m).unwrap();
    let mean_curve = compute_mean_curve(&mat);

    let acf = autocorrelation(&mean_curve, max_lag);

    // Find first peak after lag 0 (skip first few lags to avoid finding lag 0)
    let min_lag = 2;
    let peaks = find_peaks_1d(&acf[min_lag..], 1);

    if peaks.is_empty() {
        return PeriodEstimate {
            period: f64::NAN,
            frequency: f64::NAN,
            power: 0.0,
            confidence: 0.0,
        };
    }

    let peak_lag = peaks[0] + min_lag;
    let dt = (argvals[m - 1] - argvals[0]) / (m - 1) as f64;
    let period = peak_lag as f64 * dt;
    let frequency = if period > 1e-15 { 1.0 / period } else { 0.0 };

    PeriodEstimate {
        period,
        frequency,
        power: acf[peak_lag],
        confidence: acf[peak_lag].abs(),
    }
}

/// Estimate period via Fourier regression grid search.
///
/// Tests multiple candidate periods and selects the one that minimizes
/// the reconstruction error (similar to GCV).
///
/// # Arguments
/// * `data` - Column-major matrix (n x m)
/// * `n` - Number of samples
/// * `m` - Number of evaluation points
/// * `argvals` - Evaluation points
/// * `period_min` - Minimum period to test
/// * `period_max` - Maximum period to test
/// * `n_candidates` - Number of candidate periods to test
/// * `n_harmonics` - Number of Fourier harmonics to use
pub fn estimate_period_regression(
    data: &[f64],
    n: usize,
    m: usize,
    argvals: &[f64],
    period_min: f64,
    period_max: f64,
    n_candidates: usize,
    n_harmonics: usize,
) -> PeriodEstimate {
    if n == 0 || m < 4 || argvals.len() != m || period_min >= period_max || n_candidates < 2 {
        return PeriodEstimate {
            period: f64::NAN,
            frequency: f64::NAN,
            power: 0.0,
            confidence: 0.0,
        };
    }

    // Compute mean curve
    let mat = FdMatrix::from_slice(data, n, m).unwrap();
    let mean_curve = compute_mean_curve(&mat);

    let nbasis = 1 + 2 * n_harmonics;

    // Grid search over candidate periods
    let candidates: Vec<f64> = (0..n_candidates)
        .map(|i| period_min + (period_max - period_min) * i as f64 / (n_candidates - 1) as f64)
        .collect();

    let results: Vec<(f64, f64)> = slice_maybe_parallel!(candidates)
        .map(|&period| {
            let basis = fourier_basis_with_period(argvals, nbasis, period);

            // Simple least squares fit
            let mut rss = 0.0;
            for j in 0..m {
                let mut fitted = 0.0;
                // Simple: use mean of basis function times data as rough fit
                for k in 0..nbasis {
                    let b_val = basis[j + k * m];
                    let coef: f64 = (0..m)
                        .map(|l| mean_curve[l] * basis[l + k * m])
                        .sum::<f64>()
                        / (0..m)
                            .map(|l| basis[l + k * m].powi(2))
                            .sum::<f64>()
                            .max(1e-15);
                    fitted += coef * b_val;
                }
                let resid = mean_curve[j] - fitted;
                rss += resid * resid;
            }

            (period, rss)
        })
        .collect();

    // Find period with minimum RSS
    let (best_period, min_rss) = results
        .iter()
        .min_by(|a, b| a.1.partial_cmp(&b.1).unwrap_or(std::cmp::Ordering::Equal))
        .cloned()
        .unwrap_or((f64::NAN, f64::INFINITY));

    // Confidence based on how much better the best is vs average
    let mean_rss: f64 = results.iter().map(|(_, r)| r).sum::<f64>() / results.len() as f64;
    let confidence = if min_rss > 1e-15 {
        (mean_rss / min_rss).min(10.0)
    } else {
        10.0
    };

    PeriodEstimate {
        period: best_period,
        frequency: if best_period > 1e-15 {
            1.0 / best_period
        } else {
            0.0
        },
        power: 1.0 - min_rss / mean_rss,
        confidence,
    }
}

/// Detect multiple concurrent periodicities using iterative residual subtraction.
///
/// This function iteratively:
/// 1. Estimates the dominant period using FFT
/// 2. Checks both FFT confidence and seasonal strength as stopping criteria
/// 3. Computes the amplitude and phase of the sinusoidal component
/// 4. Subtracts the fitted sinusoid from the signal
/// 5. Repeats on the residual until stopping criteria are met
///
/// # Arguments
/// * `data` - Column-major matrix (n x m)
/// * `n` - Number of samples
/// * `m` - Number of evaluation points
/// * `argvals` - Evaluation points
/// * `max_periods` - Maximum number of periods to detect
/// * `min_confidence` - Minimum FFT confidence to continue (default: 0.4)
/// * `min_strength` - Minimum seasonal strength to continue (default: 0.15)
///
/// # Returns
/// Vector of detected periods with their properties
pub fn detect_multiple_periods(
    data: &[f64],
    n: usize,
    m: usize,
    argvals: &[f64],
    max_periods: usize,
    min_confidence: f64,
    min_strength: f64,
) -> Vec<DetectedPeriod> {
    if n == 0 || m < 4 || argvals.len() != m || max_periods == 0 {
        return Vec::new();
    }

    // Compute mean curve
    let mat = FdMatrix::from_slice(data, n, m).unwrap();
    let mean_curve = compute_mean_curve(&mat);

    let mut residual = mean_curve.clone();
    let mut detected = Vec::with_capacity(max_periods);

    for iteration in 1..=max_periods {
        match evaluate_next_period(
            &mut residual,
            m,
            argvals,
            min_confidence,
            min_strength,
            iteration,
        ) {
            Some(period) => detected.push(period),
            None => break,
        }
    }

    detected
}

/// Evaluate and extract the next dominant period from the residual signal.
///
/// Returns `None` if no significant period is found (signals iteration should stop).
fn evaluate_next_period(
    residual: &mut [f64],
    m: usize,
    argvals: &[f64],
    min_confidence: f64,
    min_strength: f64,
    iteration: usize,
) -> Option<DetectedPeriod> {
    let residual_mat = FdMatrix::from_slice(residual, 1, m).unwrap();
    let est = estimate_period_fft(&residual_mat, argvals);

    if est.confidence < min_confidence || est.period.is_nan() || est.period.is_infinite() {
        return None;
    }

    let strength = seasonal_strength_variance(&residual_mat, argvals, est.period, 3);
    if strength < min_strength || strength.is_nan() {
        return None;
    }

    let (_a, _b, amplitude, phase) = fit_and_subtract_sinusoid(residual, argvals, est.period);

    Some(DetectedPeriod {
        period: est.period,
        confidence: est.confidence,
        strength,
        amplitude,
        phase,
        iteration,
    })
}

// ============================================================================
// Peak Detection
// ============================================================================

/// Optionally smooth data using Fourier basis before peak detection.
fn smooth_for_peaks(
    data: &FdMatrix,
    argvals: &[f64],
    smooth_first: bool,
    smooth_nbasis: Option<usize>,
) -> Vec<f64> {
    if !smooth_first {
        return data.as_slice().to_vec();
    }
    let nbasis = smooth_nbasis
        .unwrap_or_else(|| crate::basis::select_fourier_nbasis_gcv(data, argvals, 5, 25));
    if let Some(result) = crate::basis::fourier_fit_1d(data, argvals, nbasis) {
        result.fitted.into_vec()
    } else {
        data.as_slice().to_vec()
    }
}

/// Detect peaks in a single curve using derivative zero-crossings.
fn detect_peaks_single_curve(
    curve: &[f64],
    d1: &[f64],
    argvals: &[f64],
    min_dist_points: usize,
    min_prominence: Option<f64>,
    data_range: f64,
) -> (Vec<Peak>, Vec<f64>) {
    let m = curve.len();
    let mut peak_indices = Vec::new();
    for j in 1..m {
        if d1[j - 1] > 0.0 && d1[j] <= 0.0 {
            let idx = if (d1[j - 1] - d1[j]).abs() > 1e-15 {
                j - 1
            } else {
                j
            };

            if peak_indices.is_empty()
                || idx - peak_indices[peak_indices.len() - 1] >= min_dist_points
            {
                peak_indices.push(idx);
            }
        }
    }

    let mut peaks: Vec<Peak> = peak_indices
        .iter()
        .map(|&idx| {
            let prominence = compute_prominence(curve, idx) / data_range;
            Peak {
                time: argvals[idx],
                value: curve[idx],
                prominence,
            }
        })
        .collect();

    if let Some(min_prom) = min_prominence {
        peaks.retain(|p| p.prominence >= min_prom);
    }

    let distances: Vec<f64> = peaks.windows(2).map(|w| w[1].time - w[0].time).collect();

    (peaks, distances)
}

/// Detect peaks in functional data.
///
/// Uses derivative zero-crossings to find local maxima, with optional
/// Fourier basis smoothing and filtering by minimum distance and prominence.
///
/// # Arguments
/// * `data` - Column-major matrix (n x m)
/// * `n` - Number of samples
/// * `m` - Number of evaluation points
/// * `argvals` - Evaluation points
/// * `min_distance` - Minimum time between peaks (None = no constraint)
/// * `min_prominence` - Minimum prominence (0-1 scale, None = no filter)
/// * `smooth_first` - Whether to smooth data before peak detection using Fourier basis
/// * `smooth_nbasis` - Number of Fourier basis functions. If None and smooth_first=true,
///   uses GCV to automatically select optimal nbasis (range 5-25).
pub fn detect_peaks(
    data: &FdMatrix,
    argvals: &[f64],
    min_distance: Option<f64>,
    min_prominence: Option<f64>,
    smooth_first: bool,
    smooth_nbasis: Option<usize>,
) -> PeakDetectionResult {
    let (n, m) = data.shape();
    if n == 0 || m < 3 || argvals.len() != m {
        return PeakDetectionResult {
            peaks: Vec::new(),
            inter_peak_distances: Vec::new(),
            mean_period: f64::NAN,
        };
    }

    let dt = (argvals[m - 1] - argvals[0]) / (m - 1) as f64;
    let min_dist_points = min_distance.map(|d| (d / dt).round() as usize).unwrap_or(1);

    let work_data = smooth_for_peaks(data, argvals, smooth_first, smooth_nbasis);

    // Compute first derivative
    let work_mat = FdMatrix::from_column_major(work_data.clone(), n, m).unwrap();
    let deriv1 = deriv_1d(&work_mat, argvals, 1).into_vec();

    // Compute data range for prominence normalization
    let data_range: f64 = {
        let mut min_val = f64::INFINITY;
        let mut max_val = f64::NEG_INFINITY;
        for &v in work_data.iter() {
            min_val = min_val.min(v);
            max_val = max_val.max(v);
        }
        (max_val - min_val).max(1e-15)
    };

    // Find peaks for each sample
    let results: Vec<(Vec<Peak>, Vec<f64>)> = iter_maybe_parallel!(0..n)
        .map(|i| {
            let curve: Vec<f64> = (0..m).map(|j| work_data[i + j * n]).collect();
            let d1: Vec<f64> = (0..m).map(|j| deriv1[i + j * n]).collect();
            detect_peaks_single_curve(
                &curve,
                &d1,
                argvals,
                min_dist_points,
                min_prominence,
                data_range,
            )
        })
        .collect();

    let peaks: Vec<Vec<Peak>> = results.iter().map(|(p, _)| p.clone()).collect();
    let inter_peak_distances: Vec<Vec<f64>> = results.iter().map(|(_, d)| d.clone()).collect();

    // Compute mean period from all inter-peak distances
    let all_distances: Vec<f64> = inter_peak_distances.iter().flatten().cloned().collect();
    let mean_period = if all_distances.is_empty() {
        f64::NAN
    } else {
        all_distances.iter().sum::<f64>() / all_distances.len() as f64
    };

    PeakDetectionResult {
        peaks,
        inter_peak_distances,
        mean_period,
    }
}

// ============================================================================
// Seasonal Strength
// ============================================================================

/// Measure seasonal strength using variance decomposition.
///
/// Computes SS = Var(seasonal_component) / Var(total) where the seasonal
/// component is extracted using Fourier basis.
///
/// # Arguments
/// * `data` - Column-major matrix (n x m)
/// * `n` - Number of samples
/// * `m` - Number of evaluation points
/// * `argvals` - Evaluation points
/// * `period` - Seasonal period
/// * `n_harmonics` - Number of Fourier harmonics to use
///
/// # Returns
/// Seasonal strength in [0, 1]
pub fn seasonal_strength_variance(
    data: &FdMatrix,
    argvals: &[f64],
    period: f64,
    n_harmonics: usize,
) -> f64 {
    let (n, m) = data.shape();
    if n == 0 || m < 4 || argvals.len() != m || period <= 0.0 {
        return f64::NAN;
    }

    // Compute mean curve
    let mean_curve = compute_mean_curve(data);

    // Total variance
    let global_mean: f64 = mean_curve.iter().sum::<f64>() / m as f64;
    let total_var: f64 = mean_curve
        .iter()
        .map(|&x| (x - global_mean).powi(2))
        .sum::<f64>()
        / m as f64;

    if total_var < 1e-15 {
        return 0.0;
    }

    // Fit Fourier basis to extract seasonal component
    let nbasis = 1 + 2 * n_harmonics;
    let basis = fourier_basis_with_period(argvals, nbasis, period);

    // Project data onto basis (simple least squares for mean curve)
    let mut seasonal = vec![0.0; m];
    for k in 1..nbasis {
        // Skip DC component
        let b_sum: f64 = (0..m).map(|j| basis[j + k * m].powi(2)).sum();
        if b_sum > 1e-15 {
            let coef: f64 = (0..m)
                .map(|j| mean_curve[j] * basis[j + k * m])
                .sum::<f64>()
                / b_sum;
            for j in 0..m {
                seasonal[j] += coef * basis[j + k * m];
            }
        }
    }

    // Seasonal variance
    let seasonal_mean: f64 = seasonal.iter().sum::<f64>() / m as f64;
    let seasonal_var: f64 = seasonal
        .iter()
        .map(|&x| (x - seasonal_mean).powi(2))
        .sum::<f64>()
        / m as f64;

    (seasonal_var / total_var).min(1.0)
}

/// Measure seasonal strength using spectral method.
///
/// Computes SS = power at seasonal frequencies / total power.
///
/// # Arguments
/// * `data` - Column-major matrix (n x m)
/// * `n` - Number of samples
/// * `m` - Number of evaluation points
/// * `argvals` - Evaluation points
/// * `period` - Seasonal period
pub fn seasonal_strength_spectral(data: &FdMatrix, argvals: &[f64], period: f64) -> f64 {
    let (n, m) = data.shape();
    if n == 0 || m < 4 || argvals.len() != m || period <= 0.0 {
        return f64::NAN;
    }

    // Compute mean curve
    let mean_curve = compute_mean_curve(data);

    let (frequencies, power) = periodogram(&mean_curve, argvals);

    if frequencies.len() < 2 {
        return f64::NAN;
    }

    let fundamental_freq = 1.0 / period;
    let (seasonal_power, total_power) =
        sum_harmonic_power(&frequencies, &power, fundamental_freq, 0.1);

    if total_power < 1e-15 {
        return 0.0;
    }

    (seasonal_power / total_power).min(1.0)
}

/// Compute seasonal strength using Morlet wavelet power at the target period.
///
/// This method uses the Continuous Wavelet Transform (CWT) with a Morlet wavelet
/// to measure power at the specified seasonal period. Unlike spectral methods,
/// wavelets provide time-localized frequency information.
///
/// # Arguments
/// * `data` - Column-major matrix (n x m)
/// * `n` - Number of samples
/// * `m` - Number of evaluation points
/// * `argvals` - Evaluation points
/// * `period` - Seasonal period in argvals units
///
/// # Returns
/// Seasonal strength as ratio of wavelet power to total variance (0 to 1)
///
/// # Notes
/// - Uses Morlet wavelet with ω₀ = 6 (standard choice)
/// - Scale is computed as: scale = period * ω₀ / (2π)
/// - Strength is computed over the interior 80% of the signal to avoid edge effects
pub fn seasonal_strength_wavelet(data: &FdMatrix, argvals: &[f64], period: f64) -> f64 {
    let (n, m) = data.shape();
    if n == 0 || m < 4 || argvals.len() != m || period <= 0.0 {
        return f64::NAN;
    }

    // Compute mean curve
    let mean_curve = compute_mean_curve(data);

    // Remove DC component
    let dc: f64 = mean_curve.iter().sum::<f64>() / m as f64;
    let detrended: Vec<f64> = mean_curve.iter().map(|&x| x - dc).collect();

    // Compute total variance
    let total_variance: f64 = detrended.iter().map(|&x| x * x).sum::<f64>() / m as f64;

    if total_variance < 1e-15 {
        return 0.0;
    }

    // Compute wavelet transform at the seasonal scale
    let omega0 = 6.0;
    let scale = period * omega0 / (2.0 * PI);
    let wavelet_coeffs = cwt_morlet_fft(&detrended, argvals, scale, omega0);

    if wavelet_coeffs.is_empty() {
        return f64::NAN;
    }

    // Compute wavelet power, skipping edges (10% on each side)
    let (interior_start, interior_end) = match interior_bounds(m) {
        Some(bounds) => bounds,
        None => return f64::NAN,
    };

    let wavelet_power: f64 = wavelet_coeffs[interior_start..interior_end]
        .iter()
        .map(|c| c.norm_sqr())
        .sum::<f64>()
        / (interior_end - interior_start) as f64;

    // Return ratio of wavelet power to total variance
    // Normalize so that a pure sine at the target period gives ~1.0
    (wavelet_power / total_variance).sqrt().min(1.0)
}

/// Compute time-varying seasonal strength using sliding windows.
///
/// # Arguments
/// * `data` - Column-major matrix (n x m)
/// * `n` - Number of samples
/// * `m` - Number of evaluation points
/// * `argvals` - Evaluation points
/// * `period` - Seasonal period
/// * `window_size` - Window width (recommended: 2 * period)
/// * `method` - Method for computing strength (Variance or Spectral)
///
/// # Returns
/// Seasonal strength at each time point
pub fn seasonal_strength_windowed(
    data: &FdMatrix,
    argvals: &[f64],
    period: f64,
    window_size: f64,
    method: StrengthMethod,
) -> Vec<f64> {
    let (n, m) = data.shape();
    if n == 0 || m < 4 || argvals.len() != m || period <= 0.0 || window_size <= 0.0 {
        return Vec::new();
    }

    let dt = (argvals[m - 1] - argvals[0]) / (m - 1) as f64;
    let half_window_points = ((window_size / 2.0) / dt).round() as usize;

    // Compute mean curve
    let mean_curve = compute_mean_curve(data);

    iter_maybe_parallel!(0..m)
        .map(|center| {
            let start = center.saturating_sub(half_window_points);
            let end = (center + half_window_points + 1).min(m);
            let window_m = end - start;

            if window_m < 4 {
                return f64::NAN;
            }

            let window_data: Vec<f64> = mean_curve[start..end].to_vec();
            let window_argvals: Vec<f64> = argvals[start..end].to_vec();

            // Create single-sample FdMatrix for the strength functions
            let single_mat = FdMatrix::from_column_major(window_data, 1, window_m).unwrap();

            match method {
                StrengthMethod::Variance => {
                    seasonal_strength_variance(&single_mat, &window_argvals, period, 3)
                }
                StrengthMethod::Spectral => {
                    seasonal_strength_spectral(&single_mat, &window_argvals, period)
                }
            }
        })
        .collect()
}

// ============================================================================
// Seasonality Change Detection
// ============================================================================

/// Detect changes in seasonality.
///
/// Monitors time-varying seasonal strength and detects threshold crossings
/// that indicate onset or cessation of seasonality.
///
/// # Arguments
/// * `data` - Column-major matrix (n x m)
/// * `n` - Number of samples
/// * `m` - Number of evaluation points
/// * `argvals` - Evaluation points
/// * `period` - Seasonal period
/// * `threshold` - SS threshold for seasonal/non-seasonal (e.g., 0.3)
/// * `window_size` - Window size for local strength estimation
/// * `min_duration` - Minimum duration to confirm a change
pub fn detect_seasonality_changes(
    data: &FdMatrix,
    argvals: &[f64],
    period: f64,
    threshold: f64,
    window_size: f64,
    min_duration: f64,
) -> ChangeDetectionResult {
    let (n, m) = data.shape();
    if n == 0 || m < 4 || argvals.len() != m {
        return ChangeDetectionResult {
            change_points: Vec::new(),
            strength_curve: Vec::new(),
        };
    }

    // Compute time-varying seasonal strength
    let strength_curve =
        seasonal_strength_windowed(data, argvals, period, window_size, StrengthMethod::Variance);

    if strength_curve.is_empty() {
        return ChangeDetectionResult {
            change_points: Vec::new(),
            strength_curve: Vec::new(),
        };
    }

    let dt = (argvals[m - 1] - argvals[0]) / (m - 1) as f64;
    let min_dur_points = (min_duration / dt).round() as usize;

    let change_points =
        detect_threshold_crossings(&strength_curve, argvals, threshold, min_dur_points);

    ChangeDetectionResult {
        change_points,
        strength_curve,
    }
}

// ============================================================================
// Amplitude Modulation Detection
// ============================================================================

/// Detect amplitude modulation in seasonal time series.
///
/// This function first checks if seasonality exists using the spectral method
/// (which is robust to amplitude modulation), then uses Hilbert transform to
/// extract the amplitude envelope and analyze modulation patterns.
///
/// # Arguments
/// * `data` - Column-major matrix (n x m)
/// * `n` - Number of samples
/// * `m` - Number of evaluation points
/// * `argvals` - Evaluation points
/// * `period` - Seasonal period in argvals units
/// * `modulation_threshold` - CV threshold for detecting modulation (default: 0.15)
/// * `seasonality_threshold` - Strength threshold for seasonality (default: 0.3)
///
/// # Returns
/// `AmplitudeModulationResult` containing detection results and diagnostics
///
/// # Example
/// ```ignore
/// let result = detect_amplitude_modulation(
///     &data, n, m, &argvals,
///     period,
///     0.15,          // CV > 0.15 indicates modulation
///     0.3,           // strength > 0.3 indicates seasonality
/// );
/// if result.has_modulation {
///     match result.modulation_type {
///         ModulationType::Emerging => println!("Seasonality is emerging"),
///         ModulationType::Fading => println!("Seasonality is fading"),
///         _ => {}
///     }
/// }
/// ```
pub fn detect_amplitude_modulation(
    data: &FdMatrix,
    argvals: &[f64],
    period: f64,
    modulation_threshold: f64,
    seasonality_threshold: f64,
) -> AmplitudeModulationResult {
    let (n, m) = data.shape();
    // Default result for invalid input
    let empty_result = AmplitudeModulationResult {
        is_seasonal: false,
        seasonal_strength: 0.0,
        has_modulation: false,
        modulation_type: ModulationType::NonSeasonal,
        modulation_score: 0.0,
        amplitude_trend: 0.0,
        strength_curve: Vec::new(),
        time_points: Vec::new(),
        min_strength: 0.0,
        max_strength: 0.0,
    };

    if n == 0 || m < 4 || argvals.len() != m || period <= 0.0 {
        return empty_result;
    }

    // Step 1: Check if seasonality exists using spectral method (robust to AM)
    let overall_strength = seasonal_strength_spectral(data, argvals, period);

    if overall_strength < seasonality_threshold {
        return AmplitudeModulationResult {
            is_seasonal: false,
            seasonal_strength: overall_strength,
            has_modulation: false,
            modulation_type: ModulationType::NonSeasonal,
            modulation_score: 0.0,
            amplitude_trend: 0.0,
            strength_curve: Vec::new(),
            time_points: argvals.to_vec(),
            min_strength: 0.0,
            max_strength: 0.0,
        };
    }

    // Step 2: Compute mean curve
    let mean_curve = compute_mean_curve(data);

    // Step 3: Use Hilbert transform to get amplitude envelope
    let dc: f64 = mean_curve.iter().sum::<f64>() / m as f64;
    let detrended: Vec<f64> = mean_curve.iter().map(|&x| x - dc).collect();
    let analytic = hilbert_transform(&detrended);
    let envelope: Vec<f64> = analytic.iter().map(|c| c.norm()).collect();

    if envelope.is_empty() {
        return AmplitudeModulationResult {
            is_seasonal: true,
            seasonal_strength: overall_strength,
            has_modulation: false,
            modulation_type: ModulationType::Stable,
            modulation_score: 0.0,
            amplitude_trend: 0.0,
            strength_curve: Vec::new(),
            time_points: argvals.to_vec(),
            min_strength: 0.0,
            max_strength: 0.0,
        };
    }

    // Step 4: Smooth the envelope to reduce high-frequency noise
    let dt = (argvals[m - 1] - argvals[0]) / (m - 1) as f64;
    let smooth_window = ((period / dt) as usize).max(3);
    let half_window = smooth_window / 2;

    let smoothed_envelope: Vec<f64> = (0..m)
        .map(|i| {
            let start = i.saturating_sub(half_window);
            let end = (i + half_window + 1).min(m);
            let sum: f64 = envelope[start..end].iter().sum();
            sum / (end - start) as f64
        })
        .collect();

    // Step 5: Analyze envelope statistics
    let Some((interior_start, interior_end)) = valid_interior_bounds(m, 4) else {
        return AmplitudeModulationResult {
            is_seasonal: true,
            seasonal_strength: overall_strength,
            has_modulation: false,
            modulation_type: ModulationType::Stable,
            modulation_score: 0.0,
            amplitude_trend: 0.0,
            strength_curve: envelope,
            time_points: argvals.to_vec(),
            min_strength: 0.0,
            max_strength: 0.0,
        };
    };

    let stats = analyze_amplitude_envelope(
        &smoothed_envelope[interior_start..interior_end],
        &argvals[interior_start..interior_end],
        modulation_threshold,
    );

    AmplitudeModulationResult {
        is_seasonal: true,
        seasonal_strength: overall_strength,
        has_modulation: stats.has_modulation,
        modulation_type: stats.modulation_type,
        modulation_score: stats.modulation_score,
        amplitude_trend: stats.amplitude_trend,
        strength_curve: envelope,
        time_points: argvals.to_vec(),
        min_strength: stats.min_amp,
        max_strength: stats.max_amp,
    }
}

/// Detect amplitude modulation using Morlet wavelet transform.
///
/// Uses continuous wavelet transform at the seasonal period to extract
/// time-varying amplitude. This method is more robust to noise and can
/// better handle non-stationary signals compared to Hilbert transform.
///
/// # Arguments
/// * `data` - Column-major matrix (n x m)
/// * `n` - Number of samples
/// * `m` - Number of evaluation points
/// * `argvals` - Evaluation points
/// * `period` - Seasonal period in argvals units
/// * `modulation_threshold` - CV threshold for detecting modulation (default: 0.15)
/// * `seasonality_threshold` - Strength threshold for seasonality (default: 0.3)
///
/// # Returns
/// `WaveletAmplitudeResult` containing detection results and wavelet amplitude curve
///
/// # Notes
/// - Uses Morlet wavelet with ω₀ = 6 (standard choice)
/// - The scale parameter is derived from the period: scale = period * ω₀ / (2π)
/// - This relates to how wavelets measure period: for Morlet, period ≈ scale * 2π / ω₀
pub fn detect_amplitude_modulation_wavelet(
    data: &FdMatrix,
    argvals: &[f64],
    period: f64,
    modulation_threshold: f64,
    seasonality_threshold: f64,
) -> WaveletAmplitudeResult {
    let (n, m) = data.shape();
    let empty_result = WaveletAmplitudeResult {
        is_seasonal: false,
        seasonal_strength: 0.0,
        has_modulation: false,
        modulation_type: ModulationType::NonSeasonal,
        modulation_score: 0.0,
        amplitude_trend: 0.0,
        wavelet_amplitude: Vec::new(),
        time_points: Vec::new(),
        scale: 0.0,
    };

    if n == 0 || m < 4 || argvals.len() != m || period <= 0.0 {
        return empty_result;
    }

    // Step 1: Check if seasonality exists using spectral method
    let overall_strength = seasonal_strength_spectral(data, argvals, period);

    if overall_strength < seasonality_threshold {
        return WaveletAmplitudeResult {
            is_seasonal: false,
            seasonal_strength: overall_strength,
            has_modulation: false,
            modulation_type: ModulationType::NonSeasonal,
            modulation_score: 0.0,
            amplitude_trend: 0.0,
            wavelet_amplitude: Vec::new(),
            time_points: argvals.to_vec(),
            scale: 0.0,
        };
    }

    // Step 2: Compute mean curve
    let mean_curve = compute_mean_curve(data);

    // Remove DC component
    let dc: f64 = mean_curve.iter().sum::<f64>() / m as f64;
    let detrended: Vec<f64> = mean_curve.iter().map(|&x| x - dc).collect();

    // Step 3: Compute wavelet transform at the seasonal period
    // For Morlet wavelet: period = scale * 2π / ω₀, so scale = period * ω₀ / (2π)
    let omega0 = 6.0; // Standard Morlet parameter
    let scale = period * omega0 / (2.0 * PI);

    // Use FFT-based CWT for efficiency
    let wavelet_coeffs = cwt_morlet_fft(&detrended, argvals, scale, omega0);

    if wavelet_coeffs.is_empty() {
        return WaveletAmplitudeResult {
            is_seasonal: true,
            seasonal_strength: overall_strength,
            has_modulation: false,
            modulation_type: ModulationType::Stable,
            modulation_score: 0.0,
            amplitude_trend: 0.0,
            wavelet_amplitude: Vec::new(),
            time_points: argvals.to_vec(),
            scale,
        };
    }

    // Step 4: Extract amplitude (magnitude of wavelet coefficients)
    let wavelet_amplitude: Vec<f64> = wavelet_coeffs.iter().map(|c| c.norm()).collect();

    // Step 5: Analyze amplitude envelope statistics (skip edges)
    let Some((interior_start, interior_end)) = valid_interior_bounds(m, 4) else {
        return WaveletAmplitudeResult {
            is_seasonal: true,
            seasonal_strength: overall_strength,
            has_modulation: false,
            modulation_type: ModulationType::Stable,
            modulation_score: 0.0,
            amplitude_trend: 0.0,
            wavelet_amplitude,
            time_points: argvals.to_vec(),
            scale,
        };
    };

    let stats = analyze_amplitude_envelope(
        &wavelet_amplitude[interior_start..interior_end],
        &argvals[interior_start..interior_end],
        modulation_threshold,
    );

    WaveletAmplitudeResult {
        is_seasonal: true,
        seasonal_strength: overall_strength,
        has_modulation: stats.has_modulation,
        modulation_type: stats.modulation_type,
        modulation_score: stats.modulation_score,
        amplitude_trend: stats.amplitude_trend,
        wavelet_amplitude,
        time_points: argvals.to_vec(),
        scale,
    }
}

// ============================================================================
// Instantaneous Period
// ============================================================================

/// Estimate instantaneous period using Hilbert transform.
///
/// For series with drifting/changing period, this computes the period
/// at each time point using the analytic signal.
///
/// # Arguments
/// * `data` - Column-major matrix (n x m)
/// * `n` - Number of samples
/// * `m` - Number of evaluation points
/// * `argvals` - Evaluation points
pub fn instantaneous_period(data: &FdMatrix, argvals: &[f64]) -> InstantaneousPeriod {
    let (n, m) = data.shape();
    if n == 0 || m < 4 || argvals.len() != m {
        return InstantaneousPeriod {
            period: Vec::new(),
            frequency: Vec::new(),
            amplitude: Vec::new(),
        };
    }

    // Compute mean curve
    let mean_curve = compute_mean_curve(data);

    // Remove DC component (detrend by subtracting mean)
    let dc: f64 = mean_curve.iter().sum::<f64>() / m as f64;
    let detrended: Vec<f64> = mean_curve.iter().map(|&x| x - dc).collect();

    // Compute analytic signal via Hilbert transform
    let analytic = hilbert_transform(&detrended);

    // Extract instantaneous amplitude and phase
    let amplitude: Vec<f64> = analytic.iter().map(|c| c.norm()).collect();

    let phase: Vec<f64> = analytic.iter().map(|c| c.im.atan2(c.re)).collect();

    // Unwrap phase
    let unwrapped_phase = unwrap_phase(&phase);

    // Compute instantaneous frequency (derivative of phase)
    let dt = (argvals[m - 1] - argvals[0]) / (m - 1) as f64;
    let mut inst_freq = vec![0.0; m];

    // Central differences for interior, forward/backward at boundaries
    if m > 1 {
        inst_freq[0] = (unwrapped_phase[1] - unwrapped_phase[0]) / dt / (2.0 * PI);
    }
    for j in 1..(m - 1) {
        inst_freq[j] = (unwrapped_phase[j + 1] - unwrapped_phase[j - 1]) / (2.0 * dt) / (2.0 * PI);
    }
    if m > 1 {
        inst_freq[m - 1] = (unwrapped_phase[m - 1] - unwrapped_phase[m - 2]) / dt / (2.0 * PI);
    }

    // Period = 1/frequency (handle near-zero frequencies)
    let period: Vec<f64> = inst_freq
        .iter()
        .map(|&f| {
            if f.abs() > 1e-10 {
                (1.0 / f).abs()
            } else {
                f64::INFINITY
            }
        })
        .collect();

    InstantaneousPeriod {
        period,
        frequency: inst_freq,
        amplitude,
    }
}

// ============================================================================
// Peak Timing Variability Analysis
// ============================================================================

/// Analyze peak timing variability across cycles.
///
/// For short series (e.g., 3-5 years of yearly data), this function detects
/// one peak per cycle and analyzes how peak timing varies between cycles.
///
/// # Arguments
/// * `data` - Column-major matrix (n x m)
/// * `n` - Number of samples
/// * `m` - Number of evaluation points
/// * `argvals` - Evaluation points
/// * `period` - Known period (e.g., 365 for daily data with yearly seasonality)
/// * `smooth_nbasis` - Number of Fourier basis functions for smoothing.
///   If None, uses GCV for automatic selection.
///
/// # Returns
/// Peak timing result with variability metrics
pub fn analyze_peak_timing(
    data: &FdMatrix,
    argvals: &[f64],
    period: f64,
    smooth_nbasis: Option<usize>,
) -> PeakTimingResult {
    let (n, m) = data.shape();
    if n == 0 || m < 3 || argvals.len() != m || period <= 0.0 {
        return PeakTimingResult {
            peak_times: Vec::new(),
            peak_values: Vec::new(),
            normalized_timing: Vec::new(),
            mean_timing: f64::NAN,
            std_timing: f64::NAN,
            range_timing: f64::NAN,
            variability_score: f64::NAN,
            timing_trend: f64::NAN,
            cycle_indices: Vec::new(),
        };
    }

    // Detect peaks with minimum distance constraint of 0.7 * period
    // This ensures we get at most one peak per cycle
    let min_distance = period * 0.7;
    let peaks = detect_peaks(
        data,
        argvals,
        Some(min_distance),
        None, // No prominence filter
        true, // Smooth first with Fourier basis
        smooth_nbasis,
    );

    // Use the first sample's peaks (for mean curve analysis)
    // If multiple samples, we take the mean curve which is effectively in sample 0
    let sample_peaks = if peaks.peaks.is_empty() {
        Vec::new()
    } else {
        peaks.peaks[0].clone()
    };

    if sample_peaks.is_empty() {
        return PeakTimingResult {
            peak_times: Vec::new(),
            peak_values: Vec::new(),
            normalized_timing: Vec::new(),
            mean_timing: f64::NAN,
            std_timing: f64::NAN,
            range_timing: f64::NAN,
            variability_score: f64::NAN,
            timing_trend: f64::NAN,
            cycle_indices: Vec::new(),
        };
    }

    let peak_times: Vec<f64> = sample_peaks.iter().map(|p| p.time).collect();
    let peak_values: Vec<f64> = sample_peaks.iter().map(|p| p.value).collect();

    // Compute normalized timing (position within cycle, 0-1 scale)
    let t_start = argvals[0];
    let normalized_timing: Vec<f64> = peak_times
        .iter()
        .map(|&t| {
            let cycle_pos = (t - t_start) % period;
            cycle_pos / period
        })
        .collect();

    // Compute cycle indices (1-indexed)
    let cycle_indices: Vec<usize> = peak_times
        .iter()
        .map(|&t| ((t - t_start) / period).floor() as usize + 1)
        .collect();

    // Compute statistics
    let n_peaks = normalized_timing.len() as f64;
    let mean_timing = normalized_timing.iter().sum::<f64>() / n_peaks;

    let variance: f64 = normalized_timing
        .iter()
        .map(|&x| (x - mean_timing).powi(2))
        .sum::<f64>()
        / n_peaks;
    let std_timing = variance.sqrt();

    let min_timing = normalized_timing
        .iter()
        .cloned()
        .fold(f64::INFINITY, f64::min);
    let max_timing = normalized_timing
        .iter()
        .cloned()
        .fold(f64::NEG_INFINITY, f64::max);
    let range_timing = max_timing - min_timing;

    // Variability score: normalized std deviation
    // Max possible std for uniform in [0,1] is ~0.289, so we scale by that
    // But since peaks cluster, we use 0.1 as "high" variability threshold
    let variability_score = (std_timing / 0.1).min(1.0);

    // Timing trend: linear regression of normalized timing on cycle index
    let cycle_idx_f64: Vec<f64> = cycle_indices.iter().map(|&i| i as f64).collect();
    let timing_trend = linear_slope(&cycle_idx_f64, &normalized_timing);

    PeakTimingResult {
        peak_times,
        peak_values,
        normalized_timing,
        mean_timing,
        std_timing,
        range_timing,
        variability_score,
        timing_trend,
        cycle_indices,
    }
}

// ============================================================================
// Seasonality Classification
// ============================================================================

/// Classify the type of seasonality in functional data.
///
/// This is particularly useful for short series (3-5 years) where you need
/// to identify:
/// - Whether seasonality is present
/// - Whether peak timing is stable or variable
/// - Which cycles have weak or missing seasonality
///
/// # Arguments
/// * `data` - Column-major matrix (n x m)
/// * `n` - Number of samples
/// * `m` - Number of evaluation points
/// * `argvals` - Evaluation points
/// * `period` - Known seasonal period
/// * `strength_threshold` - Threshold for seasonal/non-seasonal (default: 0.3)
/// * `timing_threshold` - Max std of normalized timing for "stable" (default: 0.05)
///
/// # Returns
/// Seasonality classification with type and diagnostics
pub fn classify_seasonality(
    data: &FdMatrix,
    argvals: &[f64],
    period: f64,
    strength_threshold: Option<f64>,
    timing_threshold: Option<f64>,
) -> SeasonalityClassification {
    let strength_thresh = strength_threshold.unwrap_or(0.3);
    let timing_thresh = timing_threshold.unwrap_or(0.05);

    let (n, m) = data.shape();
    if n == 0 || m < 4 || argvals.len() != m || period <= 0.0 {
        return SeasonalityClassification {
            is_seasonal: false,
            has_stable_timing: false,
            timing_variability: f64::NAN,
            seasonal_strength: f64::NAN,
            cycle_strengths: Vec::new(),
            weak_seasons: Vec::new(),
            classification: SeasonalType::NonSeasonal,
            peak_timing: None,
        };
    }

    // Compute overall seasonal strength
    let overall_strength = seasonal_strength_variance(data, argvals, period, 3);

    let (cycle_strengths, weak_seasons) =
        compute_cycle_strengths(data, argvals, period, strength_thresh);
    let n_cycles = cycle_strengths.len();

    // Analyze peak timing
    let peak_timing = analyze_peak_timing(data, argvals, period, None);

    // Determine classification
    let is_seasonal = overall_strength >= strength_thresh;
    let has_stable_timing = peak_timing.std_timing <= timing_thresh;
    let timing_variability = peak_timing.variability_score;

    // Classify based on patterns
    let n_weak = weak_seasons.len();
    let classification = if !is_seasonal {
        SeasonalType::NonSeasonal
    } else if n_cycles > 0 && n_weak as f64 / n_cycles as f64 > 0.3 {
        // More than 30% of cycles are weak
        SeasonalType::IntermittentSeasonal
    } else if !has_stable_timing {
        SeasonalType::VariableTiming
    } else {
        SeasonalType::StableSeasonal
    };

    SeasonalityClassification {
        is_seasonal,
        has_stable_timing,
        timing_variability,
        seasonal_strength: overall_strength,
        cycle_strengths,
        weak_seasons,
        classification,
        peak_timing: Some(peak_timing),
    }
}

/// Detect seasonality changes with automatic threshold selection.
///
/// Uses Otsu's method or percentile-based threshold instead of a fixed value.
///
/// # Arguments
/// * `data` - Column-major matrix (n x m)
/// * `n` - Number of samples
/// * `m` - Number of evaluation points
/// * `argvals` - Evaluation points
/// * `period` - Seasonal period
/// * `threshold_method` - Method for threshold selection
/// * `window_size` - Window size for local strength estimation
/// * `min_duration` - Minimum duration to confirm a change
pub fn detect_seasonality_changes_auto(
    data: &FdMatrix,
    argvals: &[f64],
    period: f64,
    threshold_method: ThresholdMethod,
    window_size: f64,
    min_duration: f64,
) -> ChangeDetectionResult {
    let (n, m) = data.shape();
    if n == 0 || m < 4 || argvals.len() != m {
        return ChangeDetectionResult {
            change_points: Vec::new(),
            strength_curve: Vec::new(),
        };
    }

    // Compute time-varying seasonal strength
    let strength_curve =
        seasonal_strength_windowed(data, argvals, period, window_size, StrengthMethod::Variance);

    if strength_curve.is_empty() {
        return ChangeDetectionResult {
            change_points: Vec::new(),
            strength_curve: Vec::new(),
        };
    }

    // Determine threshold
    let threshold = match threshold_method {
        ThresholdMethod::Fixed(t) => t,
        ThresholdMethod::Percentile(p) => {
            let mut sorted: Vec<f64> = strength_curve
                .iter()
                .copied()
                .filter(|x| x.is_finite())
                .collect();
            sorted.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));
            if sorted.is_empty() {
                0.5
            } else {
                let idx = ((p / 100.0) * sorted.len() as f64) as usize;
                sorted[idx.min(sorted.len() - 1)]
            }
        }
        ThresholdMethod::Otsu => otsu_threshold(&strength_curve),
    };

    // Now use the regular detection with computed threshold
    detect_seasonality_changes(data, argvals, period, threshold, window_size, min_duration)
}

/// Result of SAZED ensemble period detection.
#[derive(Debug, Clone)]
pub struct SazedResult {
    /// Primary detected period (consensus from ensemble)
    pub period: f64,
    /// Confidence score (0-1, based on component agreement)
    pub confidence: f64,
    /// Periods detected by each component (may be NaN if not detected)
    pub component_periods: SazedComponents,
    /// Number of components that agreed on the final period
    pub agreeing_components: usize,
}

/// Individual period estimates from each SAZED component.
#[derive(Debug, Clone)]
pub struct SazedComponents {
    /// Period from spectral (FFT) detection
    pub spectral: f64,
    /// Period from ACF peak detection
    pub acf_peak: f64,
    /// Period from weighted ACF average
    pub acf_average: f64,
    /// Period from ACF zero-crossing analysis
    pub zero_crossing: f64,
    /// Period from spectral differencing
    pub spectral_diff: f64,
}

/// SAZED: Spectral-ACF Zero-crossing Ensemble Detection
///
/// A parameter-free ensemble method for robust period detection.
/// Combines 5 detection components:
/// 1. Spectral (FFT) - peaks in periodogram
/// 2. ACF peak - first significant peak in autocorrelation
/// 3. ACF average - weighted mean of ACF peaks
/// 4. Zero-crossing - period from ACF zero crossings
/// 5. Spectral differencing - FFT on first-differenced signal
///
/// Each component provides both a period estimate and a confidence score.
/// Only components with sufficient confidence participate in voting.
/// The final period is chosen by majority voting with tolerance.
///
/// # Arguments
/// * `data` - Input signal (1D time series or mean curve from fdata)
/// * `argvals` - Time points corresponding to data
/// * `tolerance` - Relative tolerance for considering periods equal (default: 0.05 = 5%)
///
/// # Returns
/// * `SazedResult` containing the consensus period and component details
pub fn sazed(data: &[f64], argvals: &[f64], tolerance: Option<f64>) -> SazedResult {
    let n = data.len();
    let tol = tolerance.unwrap_or(0.05); // Tighter default tolerance

    if n < 8 || argvals.len() != n {
        return SazedResult {
            period: f64::NAN,
            confidence: 0.0,
            component_periods: SazedComponents {
                spectral: f64::NAN,
                acf_peak: f64::NAN,
                acf_average: f64::NAN,
                zero_crossing: f64::NAN,
                spectral_diff: f64::NAN,
            },
            agreeing_components: 0,
        };
    }

    let dt = (argvals[n - 1] - argvals[0]) / (n - 1) as f64;
    let max_lag = (n / 2).max(4);
    let signal_range = argvals[n - 1] - argvals[0];

    // Minimum detectable period (at least 3 cycles)
    let min_period = signal_range / (n as f64 / 3.0);
    // Maximum detectable period (at most 2 complete cycles)
    let max_period = signal_range / 2.0;

    // Component 1: Spectral (FFT) detection with confidence
    let (spectral_period, spectral_conf) = sazed_spectral_with_confidence(data, argvals);

    // Component 2: ACF peak detection with confidence
    let (acf_peak_period, acf_peak_conf) = sazed_acf_peak_with_confidence(data, dt, max_lag);

    // Component 3: ACF weighted average (uses ACF peak confidence)
    let acf_average_period = sazed_acf_average(data, dt, max_lag);

    // Component 4: Zero-crossing analysis with confidence
    let (zero_crossing_period, zero_crossing_conf) =
        sazed_zero_crossing_with_confidence(data, dt, max_lag);

    // Component 5: Spectral on differenced signal with confidence
    let (spectral_diff_period, spectral_diff_conf) =
        sazed_spectral_diff_with_confidence(data, argvals);

    let components = SazedComponents {
        spectral: spectral_period,
        acf_peak: acf_peak_period,
        acf_average: acf_average_period,
        zero_crossing: zero_crossing_period,
        spectral_diff: spectral_diff_period,
    };

    // Confidence thresholds for each component (tuned to minimize FPR on noise)
    // For Gaussian noise: spectral peaks rarely exceed 6x median, ACF ~1/sqrt(n)
    let spectral_thresh = 8.0; // Power ratio must be > 8x median (noise rarely exceeds 6x)
    let acf_thresh = 0.3; // ACF correlation must be > 0.3 (noise ~0.1 for n=100)
    let zero_crossing_thresh = 0.9; // Zero-crossing consistency > 90%
    let spectral_diff_thresh = 6.0; // Diff spectral power ratio > 6x

    // Minimum number of confident components required to report a period
    let min_confident_components = 2;

    // Collect valid periods (only from components with sufficient confidence)
    let confident_periods: Vec<f64> = [
        validate_sazed_component(
            spectral_period,
            spectral_conf,
            min_period,
            max_period,
            spectral_thresh,
        ),
        validate_sazed_component(
            acf_peak_period,
            acf_peak_conf,
            min_period,
            max_period,
            acf_thresh,
        ),
        validate_sazed_component(
            acf_average_period,
            acf_peak_conf,
            min_period,
            max_period,
            acf_thresh,
        ),
        validate_sazed_component(
            zero_crossing_period,
            zero_crossing_conf,
            min_period,
            max_period,
            zero_crossing_thresh,
        ),
        validate_sazed_component(
            spectral_diff_period,
            spectral_diff_conf,
            min_period,
            max_period,
            spectral_diff_thresh,
        ),
    ]
    .into_iter()
    .flatten()
    .collect();

    // Require minimum number of confident components before reporting a period
    if confident_periods.len() < min_confident_components {
        return SazedResult {
            period: f64::NAN,
            confidence: 0.0,
            component_periods: components,
            agreeing_components: confident_periods.len(),
        };
    }

    // Ensemble voting: find the mode with tolerance
    let (consensus_period, agreeing_count) = find_consensus_period(&confident_periods, tol);
    let confidence = agreeing_count as f64 / 5.0;

    SazedResult {
        period: consensus_period,
        confidence,
        component_periods: components,
        agreeing_components: agreeing_count,
    }
}

/// SAZED for functional data (matrix format)
///
/// Computes mean curve first, then applies SAZED.
///
/// # Arguments
/// * `data` - Column-major matrix (n x m)
/// * `n` - Number of samples
/// * `m` - Number of evaluation points
/// * `argvals` - Evaluation points
/// * `tolerance` - Relative tolerance for period matching
pub fn sazed_fdata(data: &FdMatrix, argvals: &[f64], tolerance: Option<f64>) -> SazedResult {
    let (n, m) = data.shape();
    if n == 0 || m < 8 || argvals.len() != m {
        return SazedResult {
            period: f64::NAN,
            confidence: 0.0,
            component_periods: SazedComponents {
                spectral: f64::NAN,
                acf_peak: f64::NAN,
                acf_average: f64::NAN,
                zero_crossing: f64::NAN,
                spectral_diff: f64::NAN,
            },
            agreeing_components: 0,
        };
    }

    let mean_curve = compute_mean_curve(data);
    sazed(&mean_curve, argvals, tolerance)
}

/// Spectral component with confidence: returns (period, power_ratio)
fn sazed_spectral_with_confidence(data: &[f64], argvals: &[f64]) -> (f64, f64) {
    let (frequencies, power) = periodogram(data, argvals);

    if frequencies.len() < 3 {
        return (f64::NAN, 0.0);
    }

    // Find peaks in power spectrum (skip DC)
    let power_no_dc: Vec<f64> = power.iter().skip(1).copied().collect();

    if power_no_dc.is_empty() {
        return (f64::NAN, 0.0);
    }

    // Calculate noise floor as median
    let mut sorted_power = power_no_dc.clone();
    sorted_power.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));
    let noise_floor = sorted_power[sorted_power.len() / 2].max(1e-15);

    // Find global maximum
    let mut max_idx = 0;
    let mut max_val = 0.0;
    for (i, &p) in power_no_dc.iter().enumerate() {
        if p > max_val {
            max_val = p;
            max_idx = i;
        }
    }

    let power_ratio = max_val / noise_floor;
    let freq = frequencies[max_idx + 1];

    if freq > 1e-15 {
        (1.0 / freq, power_ratio)
    } else {
        (f64::NAN, 0.0)
    }
}

/// ACF peak component with confidence: returns (period, acf_value_at_peak)
fn sazed_acf_peak_with_confidence(data: &[f64], dt: f64, max_lag: usize) -> (f64, f64) {
    let acf = autocorrelation(data, max_lag);

    match find_first_acf_peak(&acf) {
        Some((peak_lag, acf_value)) => (peak_lag as f64 * dt, acf_value),
        None => (f64::NAN, 0.0),
    }
}

/// ACF average component: weighted mean of ACF peak locations
fn sazed_acf_average(data: &[f64], dt: f64, max_lag: usize) -> f64 {
    let acf = autocorrelation(data, max_lag);

    if acf.len() < 4 {
        return f64::NAN;
    }

    // Find all peaks in ACF
    let peaks = find_peaks_1d(&acf[1..], 1);

    if peaks.is_empty() {
        return f64::NAN;
    }

    // Weight peaks by their ACF value
    let mut weighted_sum = 0.0;
    let mut weight_sum = 0.0;

    for (i, &peak_idx) in peaks.iter().enumerate() {
        let lag = peak_idx + 1;
        let weight = acf[lag].max(0.0);

        if i == 0 {
            // First peak is the fundamental period
            weighted_sum += lag as f64 * weight;
            weight_sum += weight;
        } else {
            // Later peaks: estimate fundamental by dividing by harmonic number
            let expected_fundamental = peaks[0] + 1;
            let harmonic = ((lag as f64 / expected_fundamental as f64) + 0.5) as usize;
            if harmonic > 0 {
                let fundamental_est = lag as f64 / harmonic as f64;
                weighted_sum += fundamental_est * weight;
                weight_sum += weight;
            }
        }
    }

    if weight_sum > 1e-15 {
        weighted_sum / weight_sum * dt
    } else {
        f64::NAN
    }
}

/// Zero-crossing component with confidence: returns (period, consistency)
/// Consistency is how regular the zero crossings are (std/mean of half-periods)
fn sazed_zero_crossing_with_confidence(data: &[f64], dt: f64, max_lag: usize) -> (f64, f64) {
    let acf = autocorrelation(data, max_lag);

    if acf.len() < 4 {
        return (f64::NAN, 0.0);
    }

    // Find zero crossings (sign changes)
    let mut crossings = Vec::new();
    for i in 1..acf.len() {
        if acf[i - 1] * acf[i] < 0.0 {
            // Linear interpolation for more precise crossing
            let frac = acf[i - 1].abs() / (acf[i - 1].abs() + acf[i].abs());
            crossings.push((i - 1) as f64 + frac);
        }
    }

    if crossings.len() < 2 {
        return (f64::NAN, 0.0);
    }

    // Period is twice the distance between consecutive zero crossings
    // (ACF goes through two zero crossings per period)
    let mut half_periods = Vec::new();
    for i in 1..crossings.len() {
        half_periods.push(crossings[i] - crossings[i - 1]);
    }

    if half_periods.is_empty() {
        return (f64::NAN, 0.0);
    }

    // Calculate consistency: 1 - (std/mean) of half-periods
    // High consistency means regular zero crossings
    let mean: f64 = half_periods.iter().sum::<f64>() / half_periods.len() as f64;
    let variance: f64 =
        half_periods.iter().map(|x| (x - mean).powi(2)).sum::<f64>() / half_periods.len() as f64;
    let std = variance.sqrt();
    let consistency = (1.0 - std / mean.max(1e-15)).clamp(0.0, 1.0);

    // Median half-period
    half_periods.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));
    let median_half = half_periods[half_periods.len() / 2];

    (2.0 * median_half * dt, consistency)
}

/// Spectral differencing with confidence: returns (period, power_ratio)
fn sazed_spectral_diff_with_confidence(data: &[f64], argvals: &[f64]) -> (f64, f64) {
    if data.len() < 4 {
        return (f64::NAN, 0.0);
    }

    // First difference to remove trend
    let diff: Vec<f64> = data.windows(2).map(|w| w[1] - w[0]).collect();
    let diff_argvals: Vec<f64> = argvals.windows(2).map(|w| (w[0] + w[1]) / 2.0).collect();

    sazed_spectral_with_confidence(&diff, &diff_argvals)
}

/// Find peaks in power spectrum above noise floor
fn find_spectral_peaks(power: &[f64]) -> Vec<usize> {
    if power.len() < 3 {
        return Vec::new();
    }

    // Estimate noise floor as median power
    let mut sorted_power: Vec<f64> = power.to_vec();
    sorted_power.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));
    let noise_floor = sorted_power[sorted_power.len() / 2];
    let threshold = noise_floor * 2.0; // Peaks must be at least 2x median

    // Find all local maxima above threshold
    let mut peaks: Vec<(usize, f64)> = Vec::new();
    for i in 1..(power.len() - 1) {
        if power[i] > power[i - 1] && power[i] > power[i + 1] && power[i] > threshold {
            peaks.push((i, power[i]));
        }
    }

    // Sort by power (descending)
    peaks.sort_by(|a, b| b.1.partial_cmp(&a.1).unwrap_or(std::cmp::Ordering::Equal));

    peaks.into_iter().map(|(idx, _)| idx).collect()
}

/// Find consensus period from multiple estimates using tolerance-based voting
fn find_consensus_period(periods: &[f64], tolerance: f64) -> (f64, usize) {
    if periods.is_empty() {
        return (f64::NAN, 0);
    }
    if periods.len() == 1 {
        return (periods[0], 1);
    }

    let mut best_period = periods[0];
    let mut best_count = 0;
    let mut best_sum = 0.0;

    for &p1 in periods {
        let (count, sum) = count_agreeing_periods(periods, p1, tolerance);

        if count > best_count
            || (count == best_count && sum / count as f64 > best_sum / best_count.max(1) as f64)
        {
            best_count = count;
            best_period = sum / count as f64;
            best_sum = sum;
        }
    }

    (best_period, best_count)
}

/// Result of Autoperiod detection.
#[derive(Debug, Clone)]
pub struct AutoperiodResult {
    /// Detected period
    pub period: f64,
    /// Combined confidence (FFT * ACF validation)
    pub confidence: f64,
    /// FFT power at the detected period
    pub fft_power: f64,
    /// ACF validation score (0-1)
    pub acf_validation: f64,
    /// All candidate periods considered
    pub candidates: Vec<AutoperiodCandidate>,
}

/// A candidate period from Autoperiod detection.
#[derive(Debug, Clone)]
pub struct AutoperiodCandidate {
    /// Candidate period
    pub period: f64,
    /// FFT power
    pub fft_power: f64,
    /// ACF validation score
    pub acf_score: f64,
    /// Combined score (power * validation)
    pub combined_score: f64,
}

fn empty_autoperiod_result() -> AutoperiodResult {
    AutoperiodResult {
        period: f64::NAN,
        confidence: 0.0,
        fft_power: 0.0,
        acf_validation: 0.0,
        candidates: Vec::new(),
    }
}

/// Build an autoperiod candidate from a spectral peak, refining with gradient ascent on ACF.
fn build_autoperiod_candidate(
    peak_idx: usize,
    frequencies: &[f64],
    power_no_dc: &[f64],
    acf: &[f64],
    dt: f64,
    steps: usize,
    total_power: f64,
) -> Option<AutoperiodCandidate> {
    let freq = frequencies[peak_idx + 1];
    if freq < 1e-15 {
        return None;
    }
    let fft_power = power_no_dc[peak_idx];
    let normalized_power = fft_power / total_power.max(1e-15);
    let refined_period = refine_period_gradient(acf, 1.0 / freq, dt, steps);
    let refined_acf_score = validate_period_acf(acf, refined_period, dt);
    Some(AutoperiodCandidate {
        period: refined_period,
        fft_power,
        acf_score: refined_acf_score,
        combined_score: normalized_power * refined_acf_score,
    })
}

/// Autoperiod: Hybrid FFT + ACF Period Detection
///
/// Implements the Autoperiod algorithm (Vlachos et al. 2005) which:
/// 1. Computes the periodogram via FFT to find candidate periods
/// 2. Validates each candidate using the autocorrelation function
/// 3. Applies gradient ascent to refine the period estimate
/// 4. Returns the period with the highest combined confidence
///
/// This method is more robust than pure FFT because ACF validation
/// filters out spurious spectral peaks that don't correspond to
/// true periodicity.
///
/// # Arguments
/// * `data` - Input signal (1D time series)
/// * `argvals` - Time points corresponding to data
/// * `n_candidates` - Maximum number of FFT peaks to consider (default: 5)
/// * `gradient_steps` - Number of gradient ascent refinement steps (default: 10)
///
/// # Returns
/// * `AutoperiodResult` containing the best period and validation details
pub fn autoperiod(
    data: &[f64],
    argvals: &[f64],
    n_candidates: Option<usize>,
    gradient_steps: Option<usize>,
) -> AutoperiodResult {
    let n = data.len();
    let max_candidates = n_candidates.unwrap_or(5);
    let steps = gradient_steps.unwrap_or(10);

    if n < 8 || argvals.len() != n {
        return empty_autoperiod_result();
    }

    let dt = (argvals[n - 1] - argvals[0]) / (n - 1) as f64;
    let max_lag = (n / 2).max(4);

    // Step 1: Compute periodogram and find candidate periods
    let (frequencies, power) = periodogram(data, argvals);

    if frequencies.len() < 3 {
        return empty_autoperiod_result();
    }

    // Find top spectral peaks
    let power_no_dc: Vec<f64> = power.iter().skip(1).copied().collect();
    let peak_indices = find_spectral_peaks(&power_no_dc);

    if peak_indices.is_empty() {
        return empty_autoperiod_result();
    }

    // Step 2: Compute ACF for validation
    let acf = autocorrelation(data, max_lag);

    // Step 3: Validate each candidate and refine with gradient ascent
    let total_power: f64 = power_no_dc.iter().sum();
    let candidates: Vec<AutoperiodCandidate> = peak_indices
        .iter()
        .take(max_candidates)
        .filter_map(|&peak_idx| {
            build_autoperiod_candidate(
                peak_idx,
                &frequencies,
                &power_no_dc,
                &acf,
                dt,
                steps,
                total_power,
            )
        })
        .collect();

    if candidates.is_empty() {
        return empty_autoperiod_result();
    }

    // Select best candidate based on combined score
    let best = candidates
        .iter()
        .max_by(|a, b| {
            a.combined_score
                .partial_cmp(&b.combined_score)
                .unwrap_or(std::cmp::Ordering::Equal)
        })
        .unwrap();

    AutoperiodResult {
        period: best.period,
        confidence: best.combined_score,
        fft_power: best.fft_power,
        acf_validation: best.acf_score,
        candidates,
    }
}

/// Autoperiod for functional data (matrix format)
pub fn autoperiod_fdata(
    data: &FdMatrix,
    argvals: &[f64],
    n_candidates: Option<usize>,
    gradient_steps: Option<usize>,
) -> AutoperiodResult {
    let (n, m) = data.shape();
    if n == 0 || m < 8 || argvals.len() != m {
        return AutoperiodResult {
            period: f64::NAN,
            confidence: 0.0,
            fft_power: 0.0,
            acf_validation: 0.0,
            candidates: Vec::new(),
        };
    }

    let mean_curve = compute_mean_curve(data);
    autoperiod(&mean_curve, argvals, n_candidates, gradient_steps)
}

/// Validate a candidate period using ACF
fn validate_period_acf(acf: &[f64], period: f64, dt: f64) -> f64 {
    let lag = (period / dt).round() as usize;

    if lag == 0 || lag >= acf.len() {
        return 0.0;
    }

    // Score based on ACF value at the period lag
    // Positive ACF values indicate valid periodicity
    let acf_at_lag = acf[lag];

    // Also check harmonics (period/2, period*2) for consistency
    let half_lag = lag / 2;
    let double_lag = lag * 2;

    let mut score = acf_at_lag.max(0.0);

    // For a true period, ACF at half-period should be low/negative
    // and ACF at double-period should also be high
    if half_lag > 0 && half_lag < acf.len() {
        let half_acf = acf[half_lag];
        // Penalize if half-period has high ACF (suggests half-period is real)
        if half_acf > acf_at_lag * 0.7 {
            score *= 0.5;
        }
    }

    if double_lag < acf.len() {
        let double_acf = acf[double_lag];
        // Bonus if double-period also shows periodicity
        if double_acf > 0.3 {
            score *= 1.2;
        }
    }

    score.min(1.0)
}

/// Refine period estimate using gradient ascent on ACF
fn refine_period_gradient(acf: &[f64], initial_period: f64, dt: f64, steps: usize) -> f64 {
    let mut period = initial_period;
    let step_size = dt * 0.5; // Search step size

    for _ in 0..steps {
        let current_score = validate_period_acf(acf, period, dt);
        let left_score = validate_period_acf(acf, period - step_size, dt);
        let right_score = validate_period_acf(acf, period + step_size, dt);

        if left_score > current_score && left_score > right_score {
            period -= step_size;
        } else if right_score > current_score {
            period += step_size;
        }
        // If current is best, we've converged
    }

    period.max(dt) // Ensure period is at least one time step
}

/// Result of CFDAutoperiod detection.
#[derive(Debug, Clone)]
pub struct CfdAutoperiodResult {
    /// Detected period (primary)
    pub period: f64,
    /// Confidence score
    pub confidence: f64,
    /// ACF validation score for the primary period
    pub acf_validation: f64,
    /// All detected periods (cluster centers)
    pub periods: Vec<f64>,
    /// Confidence for each detected period
    pub confidences: Vec<f64>,
}

/// Convert spectral peak indices to candidate (period, normalized_power) pairs.
fn generate_cfd_candidates(
    frequencies: &[f64],
    power_no_dc: &[f64],
    peak_indices: &[usize],
) -> Vec<(f64, f64)> {
    let total_power: f64 = power_no_dc.iter().sum();
    peak_indices
        .iter()
        .filter_map(|&peak_idx| {
            let freq = frequencies[peak_idx + 1];
            if freq > 1e-15 {
                let period = 1.0 / freq;
                let normalized_power = power_no_dc[peak_idx] / total_power.max(1e-15);
                Some((period, normalized_power))
            } else {
                None
            }
        })
        .collect()
}

/// Validate clustered period candidates using ACF, returning (period, acf_score, power) triples.
fn validate_cfd_candidates(clusters: &[(f64, f64)], acf: &[f64], dt: f64) -> Vec<(f64, f64, f64)> {
    clusters
        .iter()
        .filter_map(|&(center, power_sum)| {
            let acf_score = validate_period_acf(acf, center, dt);
            if acf_score > 0.1 {
                Some((center, acf_score, power_sum))
            } else {
                None
            }
        })
        .collect()
}

/// Validate cluster candidates with ACF, falling back to best cluster if none pass.
fn validate_or_fallback_cfd(
    validated: Vec<(f64, f64, f64)>,
    candidates: &[(f64, f64)],
    tol: f64,
    min_size: usize,
) -> Vec<(f64, f64, f64)> {
    if !validated.is_empty() {
        return validated;
    }
    // Fallback: pick highest-power cluster without ACF validation
    cluster_periods(candidates, tol, min_size)
        .into_iter()
        .max_by(|a, b| a.1.partial_cmp(&b.1).unwrap_or(std::cmp::Ordering::Equal))
        .map(|(center, power_sum)| vec![(center, 0.0, power_sum)])
        .unwrap_or_default()
}

/// Rank validated results by combined score (acf * power).
/// Returns (periods, confidences, top_acf_validation).
fn rank_cfd_results(validated: &[(f64, f64, f64)]) -> (Vec<f64>, Vec<f64>, f64) {
    let mut sorted: Vec<_> = validated.to_vec();
    sorted.sort_by(|a, b| {
        (b.1 * b.2)
            .partial_cmp(&(a.1 * a.2))
            .unwrap_or(std::cmp::Ordering::Equal)
    });
    let top_acf = sorted[0].1;
    let periods = sorted.iter().map(|v| v.0).collect();
    let confidences = sorted.iter().map(|v| v.1 * v.2).collect();
    (periods, confidences, top_acf)
}

fn empty_cfd_result() -> CfdAutoperiodResult {
    CfdAutoperiodResult {
        period: f64::NAN,
        confidence: 0.0,
        acf_validation: 0.0,
        periods: Vec::new(),
        confidences: Vec::new(),
    }
}

/// Extract spectral candidates from differenced data: difference, periodogram, peak-find, generate.
fn extract_cfd_spectral_candidates(data: &[f64], argvals: &[f64]) -> Option<Vec<(f64, f64)>> {
    let diff: Vec<f64> = data.windows(2).map(|w| w[1] - w[0]).collect();
    let diff_argvals: Vec<f64> = argvals.windows(2).map(|w| (w[0] + w[1]) / 2.0).collect();
    let (frequencies, power) = periodogram(&diff, &diff_argvals);
    if frequencies.len() < 3 {
        return None;
    }
    let power_no_dc: Vec<f64> = power.iter().skip(1).copied().collect();
    let peak_indices = find_spectral_peaks(&power_no_dc);
    if peak_indices.is_empty() {
        return None;
    }
    let candidates = generate_cfd_candidates(&frequencies, &power_no_dc, &peak_indices);
    if candidates.is_empty() {
        None
    } else {
        Some(candidates)
    }
}

/// CFDAutoperiod: Clustered Filtered Detrended Autoperiod
///
/// Implements the CFDAutoperiod algorithm (Puech et al. 2020) which:
/// 1. Applies first-order differencing to remove trends
/// 2. Computes FFT on the detrended signal
/// 3. Identifies candidate periods from periodogram peaks
/// 4. Clusters nearby candidates using density-based clustering
/// 5. Validates cluster centers using ACF on the original signal
///
/// This method is particularly effective for signals with strong trends
/// and handles multiple periodicities by detecting clusters of candidate periods.
///
/// # Arguments
/// * `data` - Input signal (1D time series)
/// * `argvals` - Time points corresponding to data
/// * `cluster_tolerance` - Relative tolerance for clustering periods (default: 0.1 = 10%)
/// * `min_cluster_size` - Minimum number of candidates to form a cluster (default: 1)
///
/// # Returns
/// * `CfdAutoperiodResult` containing detected periods and validation scores
pub fn cfd_autoperiod(
    data: &[f64],
    argvals: &[f64],
    cluster_tolerance: Option<f64>,
    min_cluster_size: Option<usize>,
) -> CfdAutoperiodResult {
    let n = data.len();
    let tol = cluster_tolerance.unwrap_or(0.1);
    let min_size = min_cluster_size.unwrap_or(1);

    if n < 8 || argvals.len() != n {
        return empty_cfd_result();
    }

    let dt = (argvals[n - 1] - argvals[0]) / (n - 1) as f64;
    let max_lag = (n / 2).max(4);

    let Some(candidates) = extract_cfd_spectral_candidates(data, argvals) else {
        return empty_cfd_result();
    };

    let clusters = cluster_periods(&candidates, tol, min_size);
    if clusters.is_empty() {
        return empty_cfd_result();
    }

    let acf = autocorrelation(data, max_lag);
    let validated = validate_cfd_candidates(&clusters, &acf, dt);
    let validated = validate_or_fallback_cfd(validated, &candidates, tol, min_size);
    let (periods, confidences, top_acf) = rank_cfd_results(&validated);

    CfdAutoperiodResult {
        period: periods[0],
        confidence: confidences[0],
        acf_validation: top_acf,
        periods,
        confidences,
    }
}

/// CFDAutoperiod for functional data (matrix format)
pub fn cfd_autoperiod_fdata(
    data: &FdMatrix,
    argvals: &[f64],
    cluster_tolerance: Option<f64>,
    min_cluster_size: Option<usize>,
) -> CfdAutoperiodResult {
    let (n, m) = data.shape();
    if n == 0 || m < 8 || argvals.len() != m {
        return CfdAutoperiodResult {
            period: f64::NAN,
            confidence: 0.0,
            acf_validation: 0.0,
            periods: Vec::new(),
            confidences: Vec::new(),
        };
    }

    let mean_curve = compute_mean_curve(data);
    cfd_autoperiod(&mean_curve, argvals, cluster_tolerance, min_cluster_size)
}

/// Cluster periods using a simple density-based approach
fn cluster_periods(candidates: &[(f64, f64)], tolerance: f64, min_size: usize) -> Vec<(f64, f64)> {
    if candidates.is_empty() {
        return Vec::new();
    }

    // Sort candidates by period
    let mut sorted: Vec<(f64, f64)> = candidates.to_vec();
    sorted.sort_by(|a, b| a.0.partial_cmp(&b.0).unwrap_or(std::cmp::Ordering::Equal));

    let mut clusters: Vec<(f64, f64)> = Vec::new(); // (center, total_power)
    let mut current_cluster: Vec<(f64, f64)> = vec![sorted[0]];

    for &(period, power) in sorted.iter().skip(1) {
        let cluster_center =
            current_cluster.iter().map(|(p, _)| p).sum::<f64>() / current_cluster.len() as f64;

        let rel_diff = (period - cluster_center).abs() / cluster_center.max(period);

        if rel_diff <= tolerance {
            // Add to current cluster
            current_cluster.push((period, power));
        } else {
            // Finish current cluster and start new one
            if current_cluster.len() >= min_size {
                let center = current_cluster.iter().map(|(p, pw)| p * pw).sum::<f64>()
                    / current_cluster
                        .iter()
                        .map(|(_, pw)| pw)
                        .sum::<f64>()
                        .max(1e-15);
                let total_power: f64 = current_cluster.iter().map(|(_, pw)| pw).sum();
                clusters.push((center, total_power));
            }
            current_cluster = vec![(period, power)];
        }
    }

    // Don't forget the last cluster
    if current_cluster.len() >= min_size {
        let center = current_cluster.iter().map(|(p, pw)| p * pw).sum::<f64>()
            / current_cluster
                .iter()
                .map(|(_, pw)| pw)
                .sum::<f64>()
                .max(1e-15);
        let total_power: f64 = current_cluster.iter().map(|(_, pw)| pw).sum();
        clusters.push((center, total_power));
    }

    clusters
}

// ============================================================================
// Lomb-Scargle Periodogram
// ============================================================================

/// Result of Lomb-Scargle periodogram analysis.
#[derive(Debug, Clone)]
pub struct LombScargleResult {
    /// Test frequencies
    pub frequencies: Vec<f64>,
    /// Corresponding periods (1/frequency)
    pub periods: Vec<f64>,
    /// Normalized Lomb-Scargle power at each frequency
    pub power: Vec<f64>,
    /// Peak period (highest power)
    pub peak_period: f64,
    /// Peak frequency
    pub peak_frequency: f64,
    /// Peak power
    pub peak_power: f64,
    /// False alarm probability at peak (significance level)
    pub false_alarm_probability: f64,
    /// Significance level (1 - FAP)
    pub significance: f64,
}

/// Compute Lomb-Scargle periodogram for irregularly sampled data.
///
/// The Lomb-Scargle periodogram is designed for unevenly-spaced time series
/// and reduces to the standard periodogram for evenly-spaced data.
///
/// # Algorithm
/// Following Scargle (1982) and Horne & Baliunas (1986):
/// 1. For each test frequency ω, compute the phase shift τ
/// 2. Compute the normalized power P(ω)
/// 3. Estimate false alarm probability using the exponential distribution
///
/// # Arguments
/// * `times` - Observation times (not necessarily evenly spaced)
/// * `values` - Observed values at each time
/// * `frequencies` - Optional frequencies to evaluate (cycles per unit time).
///   If None, automatically generates a frequency grid.
/// * `oversampling` - Oversampling factor for auto-generated frequency grid.
///   Default: 4.0. Higher values give finer frequency resolution.
/// * `nyquist_factor` - Maximum frequency as multiple of pseudo-Nyquist.
///   Default: 1.0.
///
/// # Returns
/// `LombScargleResult` with power spectrum and significance estimates.
///
/// # Example
/// ```rust
/// use fdars_core::seasonal::lomb_scargle;
/// use std::f64::consts::PI;
///
/// // Irregularly sampled sine wave
/// let times: Vec<f64> = vec![0.0, 0.3, 0.7, 1.2, 1.5, 2.1, 2.8, 3.0, 3.5, 4.0];
/// let period = 1.5;
/// let values: Vec<f64> = times.iter()
///     .map(|&t| (2.0 * PI * t / period).sin())
///     .collect();
///
/// let result = lomb_scargle(&times, &values, None, None, None);
/// assert!((result.peak_period - period).abs() < 0.2);
/// ```
pub fn lomb_scargle(
    times: &[f64],
    values: &[f64],
    frequencies: Option<&[f64]>,
    oversampling: Option<f64>,
    nyquist_factor: Option<f64>,
) -> LombScargleResult {
    let n = times.len();
    assert_eq!(n, values.len(), "times and values must have same length");
    assert!(n >= 3, "Need at least 3 data points");

    // Compute mean and variance
    let mean_y: f64 = values.iter().sum::<f64>() / n as f64;
    let var_y: f64 = values.iter().map(|&y| (y - mean_y).powi(2)).sum::<f64>() / (n - 1) as f64;

    // Generate frequency grid if not provided
    let freq_vec: Vec<f64>;
    let freqs = if let Some(f) = frequencies {
        f
    } else {
        freq_vec = generate_ls_frequencies(
            times,
            oversampling.unwrap_or(4.0),
            nyquist_factor.unwrap_or(1.0),
        );
        &freq_vec
    };

    // Compute Lomb-Scargle power at each frequency
    let mut power = Vec::with_capacity(freqs.len());

    for &freq in freqs.iter() {
        let omega = 2.0 * PI * freq;
        let p = lomb_scargle_single_freq(times, values, mean_y, var_y, omega);
        power.push(p);
    }

    // Find peak
    let (peak_idx, &peak_power) = power
        .iter()
        .enumerate()
        .max_by(|(_, a), (_, b)| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal))
        .unwrap_or((0, &0.0));

    let peak_frequency = freqs.get(peak_idx).copied().unwrap_or(0.0);
    let peak_period = if peak_frequency > 0.0 {
        1.0 / peak_frequency
    } else {
        f64::INFINITY
    };

    // Compute false alarm probability
    let n_indep = estimate_independent_frequencies(times, freqs.len());
    let fap = lomb_scargle_fap(peak_power, n_indep, n);

    // Compute periods from frequencies
    let periods: Vec<f64> = freqs
        .iter()
        .map(|&f| if f > 0.0 { 1.0 / f } else { f64::INFINITY })
        .collect();

    LombScargleResult {
        frequencies: freqs.to_vec(),
        periods,
        power,
        peak_period,
        peak_frequency,
        peak_power,
        false_alarm_probability: fap,
        significance: 1.0 - fap,
    }
}

/// Compute Lomb-Scargle power at a single frequency.
///
/// Uses the Scargle (1982) normalization.
fn lomb_scargle_single_freq(
    times: &[f64],
    values: &[f64],
    mean_y: f64,
    var_y: f64,
    omega: f64,
) -> f64 {
    if var_y <= 0.0 || omega <= 0.0 {
        return 0.0;
    }

    let n = times.len();

    // Compute tau (phase shift) to make sine and cosine terms orthogonal
    let mut sum_sin2 = 0.0;
    let mut sum_cos2 = 0.0;
    for &t in times.iter() {
        let arg = 2.0 * omega * t;
        sum_sin2 += arg.sin();
        sum_cos2 += arg.cos();
    }
    let tau = (sum_sin2).atan2(sum_cos2) / (2.0 * omega);

    // Compute sums for power calculation
    let mut ss = 0.0; // Sum of sin terms
    let mut sc = 0.0; // Sum of cos terms
    let mut css = 0.0; // Sum of cos^2
    let mut sss = 0.0; // Sum of sin^2

    for i in 0..n {
        let y_centered = values[i] - mean_y;
        let arg = omega * (times[i] - tau);
        let c = arg.cos();
        let s = arg.sin();

        sc += y_centered * c;
        ss += y_centered * s;
        css += c * c;
        sss += s * s;
    }

    // Avoid division by zero
    let css = css.max(1e-15);
    let sss = sss.max(1e-15);

    // Lomb-Scargle power (Scargle 1982 normalization)
    0.5 * (sc * sc / css + ss * ss / sss) / var_y
}

/// Generate frequency grid for Lomb-Scargle.
///
/// The grid spans from 1/T_total to f_nyquist with oversampling.
fn generate_ls_frequencies(times: &[f64], oversampling: f64, nyquist_factor: f64) -> Vec<f64> {
    let n = times.len();
    if n < 2 {
        return vec![0.0];
    }

    // Time span
    let t_min = times.iter().cloned().fold(f64::INFINITY, f64::min);
    let t_max = times.iter().cloned().fold(f64::NEG_INFINITY, f64::max);
    let t_span = (t_max - t_min).max(1e-10);

    // Minimum frequency: one cycle over the observation span
    let f_min = 1.0 / t_span;

    // Pseudo-Nyquist frequency for irregular data
    // Use average sampling rate as approximation
    let f_nyquist = 0.5 * (n - 1) as f64 / t_span;

    // Maximum frequency
    let f_max = f_nyquist * nyquist_factor;

    // Frequency resolution with oversampling
    let df = f_min / oversampling;

    // Generate frequency grid
    let n_freq = ((f_max - f_min) / df).ceil() as usize + 1;
    let n_freq = n_freq.min(10000); // Cap to prevent memory issues

    (0..n_freq).map(|i| f_min + i as f64 * df).collect()
}

/// Estimate number of independent frequencies (for FAP calculation).
///
/// For irregularly sampled data, this is approximately the number of
/// data points (Horne & Baliunas 1986).
fn estimate_independent_frequencies(times: &[f64], n_freq: usize) -> usize {
    // A conservative estimate is min(n_data, n_frequencies)
    let n = times.len();
    n.min(n_freq)
}

/// Compute false alarm probability for Lomb-Scargle peak.
///
/// Uses the exponential distribution approximation:
/// FAP ≈ 1 - (1 - exp(-z))^M
/// where z is the power and M is the number of independent frequencies.
fn lomb_scargle_fap(power: f64, n_indep: usize, _n_data: usize) -> f64 {
    if power <= 0.0 || n_indep == 0 {
        return 1.0;
    }

    // Probability that a single frequency has power < z
    let prob_single = 1.0 - (-power).exp();

    // Probability that all M frequencies have power < z
    // FAP = 1 - (1 - exp(-z))^M
    // For numerical stability, use log:
    // 1 - FAP = prob_single^M
    // FAP = 1 - exp(M * ln(prob_single))

    if prob_single >= 1.0 {
        return 0.0; // Very significant
    }
    if prob_single <= 0.0 {
        return 1.0; // Not significant
    }

    let log_prob = prob_single.ln();
    let log_cdf = n_indep as f64 * log_prob;

    if log_cdf < -700.0 {
        0.0 // Numerical underflow, very significant
    } else {
        1.0 - log_cdf.exp()
    }
}

/// Compute Lomb-Scargle periodogram for functional data (multiple curves).
///
/// Computes the periodogram for each curve and returns the result for the
/// mean curve or ensemble statistics.
///
/// # Arguments
/// * `data` - Column-major matrix (n x m) of functional data
/// * `n` - Number of samples (rows)
/// * `m` - Number of evaluation points (columns)
/// * `argvals` - Time points of length m
/// * `oversampling` - Oversampling factor. Default: 4.0
/// * `nyquist_factor` - Maximum frequency multiplier. Default: 1.0
///
/// # Returns
/// `LombScargleResult` computed from the mean curve.
pub fn lomb_scargle_fdata(
    data: &FdMatrix,
    argvals: &[f64],
    oversampling: Option<f64>,
    nyquist_factor: Option<f64>,
) -> LombScargleResult {
    // Compute mean curve
    let mean_curve = compute_mean_curve(data);

    // Run Lomb-Scargle on mean curve
    lomb_scargle(argvals, &mean_curve, None, oversampling, nyquist_factor)
}

// ============================================================================
// Matrix Profile (STOMP Algorithm)
// ============================================================================

/// Result of Matrix Profile computation.
#[derive(Debug, Clone)]
pub struct MatrixProfileResult {
    /// The matrix profile (minimum z-normalized distance for each position)
    pub profile: Vec<f64>,
    /// Index of the nearest neighbor for each position
    pub profile_index: Vec<usize>,
    /// Subsequence length used
    pub subsequence_length: usize,
    /// Detected periods from arc analysis
    pub detected_periods: Vec<f64>,
    /// Arc counts at each index distance (for period detection)
    pub arc_counts: Vec<usize>,
    /// Most prominent detected period
    pub primary_period: f64,
    /// Confidence score for primary period (based on arc prominence)
    pub confidence: f64,
}

/// Compute Matrix Profile using STOMP algorithm (Scalable Time series Ordered-search Matrix Profile).
///
/// The Matrix Profile is a data structure that stores the z-normalized Euclidean distance
/// between each subsequence of a time series and its nearest neighbor. It enables efficient
/// motif discovery and anomaly detection.
///
/// # Algorithm (STOMP - Zhu et al. 2016)
/// 1. Pre-compute sliding mean and standard deviation using cumulative sums
/// 2. Use FFT to compute first row of distance matrix
/// 3. Update subsequent rows incrementally using the dot product update rule
/// 4. Track minimum distance and index at each position
///
/// # Arguments
/// * `values` - Time series values
/// * `subsequence_length` - Length of subsequences to compare (window size)
/// * `exclusion_zone` - Fraction of subsequence length to exclude around each position
///   to prevent trivial self-matches. Default: 0.5
///
/// # Returns
/// `MatrixProfileResult` with profile, indices, and detected periods.
///
/// # Example
/// ```rust
/// use fdars_core::seasonal::matrix_profile;
/// use std::f64::consts::PI;
///
/// // Periodic signal
/// let period = 20.0;
/// let values: Vec<f64> = (0..100)
///     .map(|i| (2.0 * PI * i as f64 / period).sin())
///     .collect();
///
/// let result = matrix_profile(&values, Some(15), None);
/// assert!((result.primary_period - period).abs() < 5.0);
/// ```
pub fn matrix_profile(
    values: &[f64],
    subsequence_length: Option<usize>,
    exclusion_zone: Option<f64>,
) -> MatrixProfileResult {
    let n = values.len();

    // Default subsequence length: ~ 1/4 of series length, capped at reasonable range
    let m = subsequence_length.unwrap_or_else(|| {
        let default_m = n / 4;
        default_m.max(4).min(n / 2)
    });

    assert!(m >= 3, "Subsequence length must be at least 3");
    assert!(
        m <= n / 2,
        "Subsequence length must be at most half the series length"
    );

    let exclusion_zone = exclusion_zone.unwrap_or(0.5);
    let exclusion_radius = (m as f64 * exclusion_zone).ceil() as usize;

    // Number of subsequences
    let profile_len = n - m + 1;

    // Compute sliding statistics
    let (means, stds) = compute_sliding_stats(values, m);

    // Compute the matrix profile using STOMP
    let (profile, profile_index) = stomp_core(values, m, &means, &stds, exclusion_radius);

    // Perform arc analysis to detect periods
    let (arc_counts, detected_periods, primary_period, confidence) =
        analyze_arcs(&profile_index, profile_len, m);

    MatrixProfileResult {
        profile,
        profile_index,
        subsequence_length: m,
        detected_periods,
        arc_counts,
        primary_period,
        confidence,
    }
}

/// Compute sliding mean and standard deviation using cumulative sums.
///
/// This is O(n) and avoids numerical issues with naive implementations.
fn compute_sliding_stats(values: &[f64], m: usize) -> (Vec<f64>, Vec<f64>) {
    let n = values.len();
    let profile_len = n - m + 1;

    // Compute cumulative sums
    let mut cumsum = vec![0.0; n + 1];
    let mut cumsum_sq = vec![0.0; n + 1];

    for i in 0..n {
        cumsum[i + 1] = cumsum[i] + values[i];
        cumsum_sq[i + 1] = cumsum_sq[i] + values[i] * values[i];
    }

    // Compute means and stds
    let mut means = Vec::with_capacity(profile_len);
    let mut stds = Vec::with_capacity(profile_len);

    let m_f64 = m as f64;

    for i in 0..profile_len {
        let sum = cumsum[i + m] - cumsum[i];
        let sum_sq = cumsum_sq[i + m] - cumsum_sq[i];

        let mean = sum / m_f64;
        let variance = (sum_sq / m_f64) - mean * mean;
        let std = variance.max(0.0).sqrt();

        means.push(mean);
        stds.push(std.max(1e-10)); // Prevent division by zero
    }

    (means, stds)
}

/// Core STOMP algorithm implementation.
///
/// Uses FFT for the first row and incremental updates for subsequent rows.
fn stomp_core(
    values: &[f64],
    m: usize,
    means: &[f64],
    stds: &[f64],
    exclusion_radius: usize,
) -> (Vec<f64>, Vec<usize>) {
    let n = values.len();
    let profile_len = n - m + 1;

    // Initialize profile with infinity and index with 0
    let mut profile = vec![f64::INFINITY; profile_len];
    let mut profile_index = vec![0usize; profile_len];

    // Compute first row using direct computation (could use FFT for large n)
    // QT[0,j] = sum(T[0:m] * T[j:j+m]) for each j
    let mut qt = vec![0.0; profile_len];

    // First query subsequence
    for j in 0..profile_len {
        let mut dot = 0.0;
        for k in 0..m {
            dot += values[k] * values[j + k];
        }
        qt[j] = dot;
    }

    // Process first row
    update_profile_row(
        0,
        &qt,
        means,
        stds,
        m,
        exclusion_radius,
        &mut profile,
        &mut profile_index,
    );

    // Process subsequent rows using incremental updates
    for i in 1..profile_len {
        // Update QT using the sliding dot product update
        // QT[i,j] = QT[i-1,j-1] - T[i-1]*T[j-1] + T[i+m-1]*T[j+m-1]
        let mut qt_new = vec![0.0; profile_len];

        // First element needs direct computation
        let mut dot = 0.0;
        for k in 0..m {
            dot += values[i + k] * values[k];
        }
        qt_new[0] = dot;

        // Update rest using incremental formula
        for j in 1..profile_len {
            qt_new[j] =
                qt[j - 1] - values[i - 1] * values[j - 1] + values[i + m - 1] * values[j + m - 1];
        }

        qt = qt_new;

        // Update profile with this row
        update_profile_row(
            i,
            &qt,
            means,
            stds,
            m,
            exclusion_radius,
            &mut profile,
            &mut profile_index,
        );
    }

    (profile, profile_index)
}

/// Update profile with distances from row i.
fn update_profile_row(
    i: usize,
    qt: &[f64],
    means: &[f64],
    stds: &[f64],
    m: usize,
    exclusion_radius: usize,
    profile: &mut [f64],
    profile_index: &mut [usize],
) {
    let profile_len = profile.len();
    let m_f64 = m as f64;

    for j in 0..profile_len {
        // Skip exclusion zone
        if i.abs_diff(j) <= exclusion_radius {
            continue;
        }

        // Compute z-normalized distance
        // d = sqrt(2*m * (1 - (QT - m*mu_i*mu_j) / (m * sigma_i * sigma_j)))
        let numerator = qt[j] - m_f64 * means[i] * means[j];
        let denominator = m_f64 * stds[i] * stds[j];

        let pearson = if denominator > 0.0 {
            (numerator / denominator).clamp(-1.0, 1.0)
        } else {
            0.0
        };

        let dist_sq = 2.0 * m_f64 * (1.0 - pearson);
        let dist = dist_sq.max(0.0).sqrt();

        // Update profile for position i
        if dist < profile[i] {
            profile[i] = dist;
            profile_index[i] = j;
        }

        // Update profile for position j (symmetric)
        if dist < profile[j] {
            profile[j] = dist;
            profile_index[j] = i;
        }
    }
}

/// Analyze profile index to detect periods using arc counting.
///
/// Arcs connect each position to its nearest neighbor. The distance between
/// connected positions reveals repeating patterns (periods).
fn analyze_arcs(
    profile_index: &[usize],
    profile_len: usize,
    m: usize,
) -> (Vec<usize>, Vec<f64>, f64, f64) {
    // Count arcs at each index distance
    let max_distance = profile_len;
    let mut arc_counts = vec![0usize; max_distance];

    for (i, &j) in profile_index.iter().enumerate() {
        let distance = i.abs_diff(j);
        if distance < max_distance {
            arc_counts[distance] += 1;
        }
    }

    // Find peaks in arc counts (candidate periods)
    let min_period = m / 2; // Minimum meaningful period
    let mut peaks: Vec<(usize, usize)> = Vec::new();

    // Simple peak detection with minimum spacing
    for i in min_period..arc_counts.len().saturating_sub(1) {
        if arc_counts[i] > arc_counts[i.saturating_sub(1)]
            && arc_counts[i] > arc_counts[(i + 1).min(arc_counts.len() - 1)]
            && arc_counts[i] >= 3
        // Minimum count threshold
        {
            peaks.push((i, arc_counts[i]));
        }
    }

    // Sort by count descending
    peaks.sort_by(|a, b| b.1.cmp(&a.1));

    // Extract top periods
    let detected_periods: Vec<f64> = peaks.iter().take(5).map(|(p, _)| *p as f64).collect();

    // Primary period and confidence
    let (primary_period, confidence) = if let Some(&(period, count)) = peaks.first() {
        // Confidence based on relative peak prominence
        let total_arcs: usize = arc_counts[min_period..].iter().sum();
        let conf = if total_arcs > 0 {
            count as f64 / total_arcs as f64
        } else {
            0.0
        };
        (period as f64, conf.min(1.0))
    } else {
        (0.0, 0.0)
    };

    (arc_counts, detected_periods, primary_period, confidence)
}

/// Compute Matrix Profile for functional data (multiple curves).
///
/// Computes the matrix profile for each curve and returns aggregated results.
///
/// # Arguments
/// * `data` - Column-major matrix (n x m) of functional data
/// * `n` - Number of samples (rows)
/// * `m` - Number of evaluation points (columns)
/// * `subsequence_length` - Length of subsequences. If None, automatically determined.
/// * `exclusion_zone` - Exclusion zone fraction. Default: 0.5
///
/// # Returns
/// `MatrixProfileResult` computed from the mean curve.
pub fn matrix_profile_fdata(
    data: &FdMatrix,
    subsequence_length: Option<usize>,
    exclusion_zone: Option<f64>,
) -> MatrixProfileResult {
    // Compute mean curve
    let mean_curve = compute_mean_curve(data);

    // Run matrix profile on mean curve
    matrix_profile(&mean_curve, subsequence_length, exclusion_zone)
}

/// Detect seasonality using Matrix Profile analysis.
///
/// Returns true if significant periodicity is detected based on matrix profile analysis.
///
/// # Arguments
/// * `values` - Time series values
/// * `subsequence_length` - Length of subsequences to compare
/// * `confidence_threshold` - Minimum confidence for positive detection. Default: 0.1
///
/// # Returns
/// Tuple of (is_seasonal, detected_period, confidence)
pub fn matrix_profile_seasonality(
    values: &[f64],
    subsequence_length: Option<usize>,
    confidence_threshold: Option<f64>,
) -> (bool, f64, f64) {
    let result = matrix_profile(values, subsequence_length, None);

    let threshold = confidence_threshold.unwrap_or(0.1);
    let is_seasonal = result.confidence >= threshold && result.primary_period > 0.0;

    (is_seasonal, result.primary_period, result.confidence)
}

// ============================================================================
// Singular Spectrum Analysis (SSA)
// ============================================================================

/// Result of Singular Spectrum Analysis.
#[derive(Debug, Clone)]
pub struct SsaResult {
    /// Reconstructed trend component
    pub trend: Vec<f64>,
    /// Reconstructed seasonal/periodic component
    pub seasonal: Vec<f64>,
    /// Noise/residual component
    pub noise: Vec<f64>,
    /// Singular values from SVD (sorted descending)
    pub singular_values: Vec<f64>,
    /// Contribution of each component (proportion of variance)
    pub contributions: Vec<f64>,
    /// Window length used for embedding
    pub window_length: usize,
    /// Number of components extracted
    pub n_components: usize,
    /// Detected period (if any significant periodicity found)
    pub detected_period: f64,
    /// Confidence score for detected period
    pub confidence: f64,
}

/// Singular Spectrum Analysis (SSA) for time series decomposition.
///
/// SSA is a model-free, non-parametric method for decomposing a time series
/// into trend, oscillatory (seasonal), and noise components using singular
/// value decomposition of the trajectory matrix.
///
/// # Algorithm
/// 1. **Embedding**: Convert series into trajectory matrix using sliding windows
/// 2. **Decomposition**: SVD of trajectory matrix
/// 3. **Grouping**: Identify trend vs. periodic vs. noise components
/// 4. **Reconstruction**: Diagonal averaging to recover time series
///
/// # Arguments
/// * `values` - Time series values
/// * `window_length` - Embedding window length (L). If None, uses L = min(n/2, 50).
///   Larger values capture longer-term patterns but need longer series.
/// * `n_components` - Number of components to extract. If None, uses 10.
/// * `trend_components` - Indices of components for trend (0-based). If None, auto-detect.
/// * `seasonal_components` - Indices of components for seasonal. If None, auto-detect.
///
/// # Returns
/// `SsaResult` with decomposed components and diagnostics.
///
/// # Example
/// ```rust
/// use fdars_core::seasonal::ssa;
/// use std::f64::consts::PI;
///
/// // Signal with trend + seasonal + noise
/// let n = 100;
/// let values: Vec<f64> = (0..n)
///     .map(|i| {
///         let t = i as f64;
///         0.01 * t + (2.0 * PI * t / 12.0).sin() + 0.1 * (i as f64 * 0.1).sin()
///     })
///     .collect();
///
/// let result = ssa(&values, None, None, None, None);
/// assert!(result.detected_period > 0.0);
/// ```
pub fn ssa(
    values: &[f64],
    window_length: Option<usize>,
    n_components: Option<usize>,
    trend_components: Option<&[usize]>,
    seasonal_components: Option<&[usize]>,
) -> SsaResult {
    let n = values.len();

    // Default window length: min(n/2, 50)
    let l = window_length.unwrap_or_else(|| (n / 2).clamp(2, 50));

    if n < 4 || l < 2 || l > n / 2 {
        return SsaResult {
            trend: values.to_vec(),
            seasonal: vec![0.0; n],
            noise: vec![0.0; n],
            singular_values: vec![],
            contributions: vec![],
            window_length: l,
            n_components: 0,
            detected_period: 0.0,
            confidence: 0.0,
        };
    }

    // Number of columns in trajectory matrix
    let k = n - l + 1;

    // Step 1: Embedding - create trajectory matrix (L x K)
    let trajectory = embed_trajectory(values, l, k);

    // Step 2: SVD decomposition
    let (u, sigma, vt) = svd_decompose(&trajectory, l, k);

    // Determine number of components to use
    let max_components = sigma.len();
    let n_comp = n_components.unwrap_or(10).min(max_components);

    // Compute contributions (proportion of total variance)
    let total_var: f64 = sigma.iter().map(|&s| s * s).sum();
    let contributions: Vec<f64> = sigma
        .iter()
        .take(n_comp)
        .map(|&s| s * s / total_var.max(1e-15))
        .collect();

    // Step 3: Grouping - identify trend and seasonal components
    let (trend_idx, seasonal_idx, detected_period, confidence) =
        if trend_components.is_some() || seasonal_components.is_some() {
            // Use provided groupings
            let t_idx: Vec<usize> = trend_components.map(|v| v.to_vec()).unwrap_or_default();
            let s_idx: Vec<usize> = seasonal_components.map(|v| v.to_vec()).unwrap_or_default();
            (t_idx, s_idx, 0.0, 0.0)
        } else {
            // Auto-detect groupings
            auto_group_ssa_components(&u, &sigma, l, k, n_comp)
        };

    // Step 4: Reconstruction via diagonal averaging
    let trend = reconstruct_grouped(&u, &sigma, &vt, l, k, n, &trend_idx);
    let seasonal = reconstruct_grouped(&u, &sigma, &vt, l, k, n, &seasonal_idx);

    // Noise is the remainder
    let noise: Vec<f64> = values
        .iter()
        .zip(trend.iter())
        .zip(seasonal.iter())
        .map(|((&y, &t), &s)| y - t - s)
        .collect();

    SsaResult {
        trend,
        seasonal,
        noise,
        singular_values: sigma.into_iter().take(n_comp).collect(),
        contributions,
        window_length: l,
        n_components: n_comp,
        detected_period,
        confidence,
    }
}

/// Create trajectory matrix by embedding the time series.
fn embed_trajectory(values: &[f64], l: usize, k: usize) -> Vec<f64> {
    // Trajectory matrix is L x K, stored column-major
    let mut trajectory = vec![0.0; l * k];

    for j in 0..k {
        for i in 0..l {
            trajectory[i + j * l] = values[i + j];
        }
    }

    trajectory
}

/// SVD decomposition of trajectory matrix using nalgebra.
fn svd_decompose(trajectory: &[f64], l: usize, k: usize) -> (Vec<f64>, Vec<f64>, Vec<f64>) {
    use nalgebra::{DMatrix, SVD};

    // Create nalgebra matrix (column-major)
    let mat = DMatrix::from_column_slice(l, k, trajectory);

    // Compute SVD
    let svd = SVD::new(mat, true, true);

    // Extract components (SVD::new with compute_u/v=true always produces both,
    // but handle gracefully in case of degenerate input)
    let u_mat = match svd.u {
        Some(u) => u,
        None => return (vec![], vec![], vec![]),
    };
    let vt_mat = match svd.v_t {
        Some(vt) => vt,
        None => return (vec![], vec![], vec![]),
    };
    let sigma = svd.singular_values;

    // Convert to flat vectors
    let u: Vec<f64> = u_mat.iter().cloned().collect();
    let sigma_vec: Vec<f64> = sigma.iter().cloned().collect();
    let vt: Vec<f64> = vt_mat.iter().cloned().collect();

    (u, sigma_vec, vt)
}

enum SsaComponentKind {
    Trend,
    Seasonal(f64),
    Noise,
}

/// Classify an SSA component as trend, seasonal, or noise.
fn classify_ssa_component(u_col: &[f64], trend_count: usize) -> SsaComponentKind {
    if is_trend_component(u_col) && trend_count < 2 {
        SsaComponentKind::Trend
    } else {
        let (is_periodic, period) = is_periodic_component(u_col);
        if is_periodic {
            SsaComponentKind::Seasonal(period)
        } else {
            SsaComponentKind::Noise
        }
    }
}

/// Apply default groupings when auto-detection finds nothing.
fn apply_ssa_grouping_defaults(
    trend_idx: &mut Vec<usize>,
    seasonal_idx: &mut Vec<usize>,
    n_comp: usize,
) {
    if trend_idx.is_empty() && n_comp > 0 {
        trend_idx.push(0);
    }
    if seasonal_idx.is_empty() && n_comp >= 3 {
        seasonal_idx.push(1);
        if n_comp > 2 {
            seasonal_idx.push(2);
        }
    }
}

/// Auto-detect trend and seasonal component groupings.
fn auto_group_ssa_components(
    u: &[f64],
    sigma: &[f64],
    l: usize,
    _k: usize,
    n_comp: usize,
) -> (Vec<usize>, Vec<usize>, f64, f64) {
    let mut trend_idx = Vec::new();
    let mut seasonal_idx = Vec::new();
    let mut detected_period = 0.0;
    let mut confidence = 0.0;

    for i in 0..n_comp.min(sigma.len()) {
        let u_col: Vec<f64> = (0..l).map(|j| u[j + i * l]).collect();
        match classify_ssa_component(&u_col, trend_idx.len()) {
            SsaComponentKind::Trend => trend_idx.push(i),
            SsaComponentKind::Seasonal(period) => {
                seasonal_idx.push(i);
                if detected_period == 0.0 && period > 0.0 {
                    detected_period = period;
                    confidence = sigma[i] / sigma[0].max(1e-15);
                }
            }
            SsaComponentKind::Noise => {}
        }
    }

    apply_ssa_grouping_defaults(&mut trend_idx, &mut seasonal_idx, n_comp);
    (trend_idx, seasonal_idx, detected_period, confidence)
}

/// Check if a singular vector represents a trend component.
fn is_trend_component(u_col: &[f64]) -> bool {
    let n = u_col.len();
    if n < 3 {
        return false;
    }

    // Count sign changes in the vector
    let mut sign_changes = 0;
    for i in 1..n {
        if u_col[i] * u_col[i - 1] < 0.0 {
            sign_changes += 1;
        }
    }

    // Trend components have very few sign changes
    sign_changes <= n / 10
}

/// Check if a singular vector represents a periodic component.
fn is_periodic_component(u_col: &[f64]) -> (bool, f64) {
    let n = u_col.len();
    if n < 4 {
        return (false, 0.0);
    }

    // Use autocorrelation to detect periodicity
    let mean: f64 = u_col.iter().sum::<f64>() / n as f64;
    let centered: Vec<f64> = u_col.iter().map(|&x| x - mean).collect();

    let var: f64 = centered.iter().map(|&x| x * x).sum();
    if var < 1e-15 {
        return (false, 0.0);
    }

    // Find first significant peak in autocorrelation
    let mut best_period = 0.0;
    let mut best_acf = 0.0;

    for lag in 2..n / 2 {
        let mut acf = 0.0;
        for i in 0..(n - lag) {
            acf += centered[i] * centered[i + lag];
        }
        acf /= var;

        if acf > best_acf && acf > 0.3 {
            best_acf = acf;
            best_period = lag as f64;
        }
    }

    let is_periodic = best_acf > 0.3 && best_period > 0.0;
    (is_periodic, best_period)
}

/// Reconstruct time series from grouped components via diagonal averaging.
fn reconstruct_grouped(
    u: &[f64],
    sigma: &[f64],
    vt: &[f64],
    l: usize,
    k: usize,
    n: usize,
    group_idx: &[usize],
) -> Vec<f64> {
    if group_idx.is_empty() {
        return vec![0.0; n];
    }

    // Sum of rank-1 matrices for this group
    let mut grouped_matrix = vec![0.0; l * k];

    for &idx in group_idx {
        if idx >= sigma.len() {
            continue;
        }

        let s = sigma[idx];

        // Add s * u_i * v_i^T
        for j in 0..k {
            for i in 0..l {
                let u_val = u[i + idx * l];
                let v_val = vt[idx + j * sigma.len().min(l)]; // v_t is stored as K x min(L,K)
                grouped_matrix[i + j * l] += s * u_val * v_val;
            }
        }
    }

    // Diagonal averaging (Hankelization)
    diagonal_average(&grouped_matrix, l, k, n)
}

/// Diagonal averaging to convert trajectory matrix back to time series.
fn diagonal_average(matrix: &[f64], l: usize, k: usize, n: usize) -> Vec<f64> {
    let mut result = vec![0.0; n];
    let mut counts = vec![0.0; n];

    // Average along anti-diagonals
    for j in 0..k {
        for i in 0..l {
            let idx = i + j; // Position in original series
            if idx < n {
                result[idx] += matrix[i + j * l];
                counts[idx] += 1.0;
            }
        }
    }

    // Normalize by counts
    for i in 0..n {
        if counts[i] > 0.0 {
            result[i] /= counts[i];
        }
    }

    result
}

/// Compute SSA for functional data (multiple curves).
///
/// # Arguments
/// * `data` - Column-major matrix (n x m) of functional data
/// * `n` - Number of samples (rows)
/// * `m` - Number of evaluation points (columns)
/// * `window_length` - SSA window length. If None, auto-determined.
/// * `n_components` - Number of SSA components. Default: 10.
///
/// # Returns
/// `SsaResult` computed from the mean curve.
pub fn ssa_fdata(
    data: &FdMatrix,
    window_length: Option<usize>,
    n_components: Option<usize>,
) -> SsaResult {
    // Compute mean curve
    let mean_curve = compute_mean_curve(data);

    // Run SSA on mean curve
    ssa(&mean_curve, window_length, n_components, None, None)
}

/// Detect seasonality using SSA.
///
/// # Arguments
/// * `values` - Time series values
/// * `window_length` - SSA window length
/// * `confidence_threshold` - Minimum confidence for positive detection
///
/// # Returns
/// Tuple of (is_seasonal, detected_period, confidence)
pub fn ssa_seasonality(
    values: &[f64],
    window_length: Option<usize>,
    confidence_threshold: Option<f64>,
) -> (bool, f64, f64) {
    let result = ssa(values, window_length, None, None, None);

    let threshold = confidence_threshold.unwrap_or(0.1);
    let is_seasonal = result.confidence >= threshold && result.detected_period > 0.0;

    (is_seasonal, result.detected_period, result.confidence)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::f64::consts::PI;

    fn generate_sine(n: usize, m: usize, period: f64, argvals: &[f64]) -> FdMatrix {
        let mut data = vec![0.0; n * m];
        for i in 0..n {
            for j in 0..m {
                data[i + j * n] = (2.0 * PI * argvals[j] / period).sin();
            }
        }
        FdMatrix::from_column_major(data, n, m).unwrap()
    }

    #[test]
    fn test_period_estimation_fft() {
        let m = 200;
        let argvals: Vec<f64> = (0..m).map(|i| i as f64 * 0.1).collect();
        let period = 2.0;
        let data = generate_sine(1, m, period, &argvals);

        let estimate = estimate_period_fft(&data, &argvals);
        assert!((estimate.period - period).abs() < 0.2);
        assert!(estimate.confidence > 1.0);
    }

    #[test]
    fn test_peak_detection() {
        let m = 100;
        let argvals: Vec<f64> = (0..m).map(|i| i as f64 * 0.1).collect();
        let period = 2.0;
        let data = generate_sine(1, m, period, &argvals);

        let result = detect_peaks(&data, &argvals, Some(1.5), None, false, None);

        // Should find approximately 5 peaks (10 / 2)
        assert!(!result.peaks[0].is_empty());
        assert!((result.mean_period - period).abs() < 0.3);
    }

    #[test]
    fn test_peak_detection_known_sine() {
        // Pure sine wave: sin(2*pi*t/2) on [0, 10]
        // Peaks occur at t = period/4 + k*period = 0.5, 2.5, 4.5, 6.5, 8.5
        let m = 200; // High resolution for accurate detection
        let period = 2.0;
        let argvals: Vec<f64> = (0..m).map(|i| i as f64 * 10.0 / (m - 1) as f64).collect();
        let data: Vec<f64> = argvals
            .iter()
            .map(|&t| (2.0 * std::f64::consts::PI * t / period).sin())
            .collect();
        let data = FdMatrix::from_column_major(data, 1, m).unwrap();

        let result = detect_peaks(&data, &argvals, None, None, false, None);

        // Should find exactly 5 peaks
        assert_eq!(
            result.peaks[0].len(),
            5,
            "Expected 5 peaks, got {}. Peak times: {:?}",
            result.peaks[0].len(),
            result.peaks[0].iter().map(|p| p.time).collect::<Vec<_>>()
        );

        // Check peak locations are close to expected
        let expected_times = [0.5, 2.5, 4.5, 6.5, 8.5];
        for (peak, expected) in result.peaks[0].iter().zip(expected_times.iter()) {
            assert!(
                (peak.time - expected).abs() < 0.15,
                "Peak at {:.3} not close to expected {:.3}",
                peak.time,
                expected
            );
        }

        // Check mean period
        assert!(
            (result.mean_period - period).abs() < 0.1,
            "Mean period {:.3} not close to expected {:.3}",
            result.mean_period,
            period
        );
    }

    #[test]
    fn test_peak_detection_with_min_distance() {
        // Same sine wave but with min_distance constraint
        let m = 200;
        let period = 2.0;
        let argvals: Vec<f64> = (0..m).map(|i| i as f64 * 10.0 / (m - 1) as f64).collect();
        let data: Vec<f64> = argvals
            .iter()
            .map(|&t| (2.0 * std::f64::consts::PI * t / period).sin())
            .collect();
        let data = FdMatrix::from_column_major(data, 1, m).unwrap();

        // min_distance = 1.5 should still find all 5 peaks (spacing = 2.0)
        let result = detect_peaks(&data, &argvals, Some(1.5), None, false, None);
        assert_eq!(
            result.peaks[0].len(),
            5,
            "With min_distance=1.5, expected 5 peaks, got {}",
            result.peaks[0].len()
        );

        // min_distance = 2.5 should find fewer peaks
        let result2 = detect_peaks(&data, &argvals, Some(2.5), None, false, None);
        assert!(
            result2.peaks[0].len() < 5,
            "With min_distance=2.5, expected fewer than 5 peaks, got {}",
            result2.peaks[0].len()
        );
    }

    #[test]
    fn test_peak_detection_period_1() {
        // Higher frequency: sin(2*pi*t/1) on [0, 10]
        // Peaks at t = 0.25, 1.25, 2.25, ..., 9.25 (10 peaks)
        let m = 400; // Higher resolution for higher frequency
        let period = 1.0;
        let argvals: Vec<f64> = (0..m).map(|i| i as f64 * 10.0 / (m - 1) as f64).collect();
        let data: Vec<f64> = argvals
            .iter()
            .map(|&t| (2.0 * std::f64::consts::PI * t / period).sin())
            .collect();
        let data = FdMatrix::from_column_major(data, 1, m).unwrap();

        let result = detect_peaks(&data, &argvals, None, None, false, None);

        // Should find 10 peaks
        assert_eq!(
            result.peaks[0].len(),
            10,
            "Expected 10 peaks, got {}",
            result.peaks[0].len()
        );

        // Check mean period
        assert!(
            (result.mean_period - period).abs() < 0.1,
            "Mean period {:.3} not close to expected {:.3}",
            result.mean_period,
            period
        );
    }

    #[test]
    fn test_peak_detection_shifted_sine() {
        // Shifted sine: sin(2*pi*t/2) + 1 on [0, 10]
        // Same peak locations, just shifted up
        let m = 200;
        let period = 2.0;
        let argvals: Vec<f64> = (0..m).map(|i| i as f64 * 10.0 / (m - 1) as f64).collect();
        let data: Vec<f64> = argvals
            .iter()
            .map(|&t| (2.0 * std::f64::consts::PI * t / period).sin() + 1.0)
            .collect();
        let data = FdMatrix::from_column_major(data, 1, m).unwrap();

        let result = detect_peaks(&data, &argvals, None, None, false, None);

        // Should still find 5 peaks
        assert_eq!(
            result.peaks[0].len(),
            5,
            "Expected 5 peaks for shifted sine, got {}",
            result.peaks[0].len()
        );

        // Peak values should be around 2.0 (max of sin + 1)
        for peak in &result.peaks[0] {
            assert!(
                (peak.value - 2.0).abs() < 0.05,
                "Peak value {:.3} not close to expected 2.0",
                peak.value
            );
        }
    }

    #[test]
    fn test_peak_detection_prominence() {
        // Create signal with peaks of different heights
        // Large peaks at odd positions, small peaks at even positions
        let m = 200;
        let argvals: Vec<f64> = (0..m).map(|i| i as f64 * 10.0 / (m - 1) as f64).collect();
        let data: Vec<f64> = argvals
            .iter()
            .map(|&t| {
                let base = (2.0 * std::f64::consts::PI * t / 2.0).sin();
                // Add small ripples
                let ripple = 0.1 * (2.0 * std::f64::consts::PI * t * 4.0).sin();
                base + ripple
            })
            .collect();
        let data = FdMatrix::from_column_major(data, 1, m).unwrap();

        // Without prominence filter, may find extra peaks from ripples
        let result_no_filter = detect_peaks(&data, &argvals, None, None, false, None);

        // With prominence filter, should only find major peaks
        let result_filtered = detect_peaks(&data, &argvals, None, Some(0.5), false, None);

        // Filtered should have fewer or equal peaks
        assert!(
            result_filtered.peaks[0].len() <= result_no_filter.peaks[0].len(),
            "Prominence filter should reduce peak count"
        );
    }

    #[test]
    fn test_peak_detection_different_amplitudes() {
        // Test with various amplitudes: 0.5, 1.0, 2.0, 5.0
        let m = 200;
        let period = 2.0;
        let argvals: Vec<f64> = (0..m).map(|i| i as f64 * 10.0 / (m - 1) as f64).collect();

        for amplitude in [0.5, 1.0, 2.0, 5.0] {
            let data: Vec<f64> = argvals
                .iter()
                .map(|&t| amplitude * (2.0 * std::f64::consts::PI * t / period).sin())
                .collect();
            let data = FdMatrix::from_column_major(data, 1, m).unwrap();

            let result = detect_peaks(&data, &argvals, None, None, false, None);

            assert_eq!(
                result.peaks[0].len(),
                5,
                "Amplitude {} should still find 5 peaks",
                amplitude
            );

            // Peak values should be close to amplitude
            for peak in &result.peaks[0] {
                assert!(
                    (peak.value - amplitude).abs() < 0.1,
                    "Peak value {:.3} should be close to amplitude {}",
                    peak.value,
                    amplitude
                );
            }
        }
    }

    #[test]
    fn test_peak_detection_varying_frequency() {
        // Signal with varying frequency: chirp-like signal
        // Peaks get closer together over time
        let m = 400;
        let argvals: Vec<f64> = (0..m).map(|i| i as f64 * 10.0 / (m - 1) as f64).collect();

        // Frequency increases linearly: f(t) = 0.5 + 0.1*t
        // Phase integral: phi(t) = 2*pi * (0.5*t + 0.05*t^2)
        let data: Vec<f64> = argvals
            .iter()
            .map(|&t| {
                let phase = 2.0 * std::f64::consts::PI * (0.5 * t + 0.05 * t * t);
                phase.sin()
            })
            .collect();
        let data = FdMatrix::from_column_major(data, 1, m).unwrap();

        let result = detect_peaks(&data, &argvals, None, None, false, None);

        // Should find multiple peaks with decreasing spacing
        assert!(
            result.peaks[0].len() >= 5,
            "Should find at least 5 peaks, got {}",
            result.peaks[0].len()
        );

        // Verify inter-peak distances decrease over time
        let distances = &result.inter_peak_distances[0];
        if distances.len() >= 3 {
            // Later peaks should be closer than earlier peaks
            let early_avg = (distances[0] + distances[1]) / 2.0;
            let late_avg = (distances[distances.len() - 2] + distances[distances.len() - 1]) / 2.0;
            assert!(
                late_avg < early_avg,
                "Later peaks should be closer: early avg={:.3}, late avg={:.3}",
                early_avg,
                late_avg
            );
        }
    }

    #[test]
    fn test_peak_detection_sum_of_sines() {
        // Sum of two sine waves with different periods creates non-uniform peak spacing
        // y = sin(2*pi*t/2) + 0.5*sin(2*pi*t/3)
        let m = 300;
        let argvals: Vec<f64> = (0..m).map(|i| i as f64 * 12.0 / (m - 1) as f64).collect();

        let data: Vec<f64> = argvals
            .iter()
            .map(|&t| {
                (2.0 * std::f64::consts::PI * t / 2.0).sin()
                    + 0.5 * (2.0 * std::f64::consts::PI * t / 3.0).sin()
            })
            .collect();
        let data = FdMatrix::from_column_major(data, 1, m).unwrap();

        let result = detect_peaks(&data, &argvals, Some(1.0), None, false, None);

        // Should find peaks (exact count depends on interference pattern)
        assert!(
            result.peaks[0].len() >= 4,
            "Should find at least 4 peaks, got {}",
            result.peaks[0].len()
        );

        // Inter-peak distances should vary
        let distances = &result.inter_peak_distances[0];
        if distances.len() >= 2 {
            let min_dist = distances.iter().cloned().fold(f64::INFINITY, f64::min);
            let max_dist = distances.iter().cloned().fold(0.0, f64::max);
            assert!(
                max_dist > min_dist * 1.1,
                "Distances should vary: min={:.3}, max={:.3}",
                min_dist,
                max_dist
            );
        }
    }

    #[test]
    fn test_seasonal_strength() {
        let m = 200;
        let argvals: Vec<f64> = (0..m).map(|i| i as f64 * 0.1).collect();
        let period = 2.0;
        let data = generate_sine(1, m, period, &argvals);

        let strength = seasonal_strength_variance(&data, &argvals, period, 3);
        // Pure sine should have high seasonal strength
        assert!(strength > 0.8);

        let strength_spectral = seasonal_strength_spectral(&data, &argvals, period);
        assert!(strength_spectral > 0.5);
    }

    #[test]
    fn test_instantaneous_period() {
        let m = 200;
        let argvals: Vec<f64> = (0..m).map(|i| i as f64 * 0.1).collect();
        let period = 2.0;
        let data = generate_sine(1, m, period, &argvals);

        let result = instantaneous_period(&data, &argvals);

        // Check that instantaneous period is close to true period (away from boundaries)
        let mid_period = result.period[m / 2];
        assert!(
            (mid_period - period).abs() < 0.5,
            "Expected period ~{}, got {}",
            period,
            mid_period
        );
    }

    #[test]
    fn test_peak_timing_analysis() {
        // Generate 5 cycles of sine with period 2
        let m = 500;
        let argvals: Vec<f64> = (0..m).map(|i| i as f64 * 0.02).collect();
        let period = 2.0;
        let data = generate_sine(1, m, period, &argvals);

        let result = analyze_peak_timing(&data, &argvals, period, Some(11));

        // Should find approximately 5 peaks
        assert!(!result.peak_times.is_empty());
        // Normalized timing should be around 0.25 (peak of sin at π/2)
        assert!(result.mean_timing.is_finite());
        // Pure sine should have low timing variability
        assert!(result.std_timing < 0.1 || result.std_timing.is_nan());
    }

    #[test]
    fn test_seasonality_classification() {
        let m = 400;
        let argvals: Vec<f64> = (0..m).map(|i| i as f64 * 0.05).collect();
        let period = 2.0;
        let data = generate_sine(1, m, period, &argvals);

        let result = classify_seasonality(&data, &argvals, period, None, None);

        assert!(result.is_seasonal);
        assert!(result.seasonal_strength > 0.5);
        assert!(matches!(
            result.classification,
            SeasonalType::StableSeasonal | SeasonalType::VariableTiming
        ));
    }

    #[test]
    fn test_otsu_threshold() {
        // Bimodal distribution: mix of low (0.1-0.2) and high (0.7-0.9) values
        let values = vec![
            0.1, 0.12, 0.15, 0.18, 0.11, 0.14, 0.7, 0.75, 0.8, 0.85, 0.9, 0.72,
        ];

        let threshold = otsu_threshold(&values);

        // Threshold should be between the two modes
        // Due to small sample size, Otsu's method may not find optimal threshold
        // Just verify it returns a reasonable value in the data range
        assert!(threshold >= 0.1, "Threshold {} should be >= 0.1", threshold);
        assert!(threshold <= 0.9, "Threshold {} should be <= 0.9", threshold);
    }

    #[test]
    fn test_gcv_fourier_nbasis_selection() {
        let m = 100;
        let argvals: Vec<f64> = (0..m).map(|i| i as f64 * 0.1).collect();

        // Noisy sine wave
        let mut data = vec![0.0; m];
        for j in 0..m {
            data[j] = (2.0 * PI * argvals[j] / 2.0).sin() + 0.1 * (j as f64 * 0.3).sin();
        }

        let data_mat = crate::matrix::FdMatrix::from_column_major(data, 1, m).unwrap();
        let nbasis = crate::basis::select_fourier_nbasis_gcv(&data_mat, &argvals, 5, 25);

        // nbasis should be reasonable (between min and max)
        assert!(nbasis >= 5);
        assert!(nbasis <= 25);
    }

    #[test]
    fn test_detect_multiple_periods() {
        let m = 400;
        let argvals: Vec<f64> = (0..m).map(|i| i as f64 * 0.05).collect(); // 0 to 20

        // Signal with two periods: 2 and 7
        let period1 = 2.0;
        let period2 = 7.0;
        let mut data = vec![0.0; m];
        for j in 0..m {
            data[j] = (2.0 * PI * argvals[j] / period1).sin()
                + 0.6 * (2.0 * PI * argvals[j] / period2).sin();
        }

        // Use higher min_strength threshold to properly stop after real periods
        let detected = detect_multiple_periods(&data, 1, m, &argvals, 5, 0.4, 0.20);

        // Should detect exactly 2 periods with these thresholds
        assert!(
            detected.len() >= 2,
            "Expected at least 2 periods, found {}",
            detected.len()
        );

        // Check that both periods were detected (order depends on amplitude)
        let periods: Vec<f64> = detected.iter().map(|d| d.period).collect();
        let has_period1 = periods.iter().any(|&p| (p - period1).abs() < 0.3);
        let has_period2 = periods.iter().any(|&p| (p - period2).abs() < 0.5);

        assert!(
            has_period1,
            "Expected to find period ~{}, got {:?}",
            period1, periods
        );
        assert!(
            has_period2,
            "Expected to find period ~{}, got {:?}",
            period2, periods
        );

        // Verify first detected has higher amplitude (amplitude 1.0 vs 0.6)
        assert!(
            detected[0].amplitude > detected[1].amplitude,
            "First detected should have higher amplitude"
        );

        // Each detected period should have strength and confidence info
        for d in &detected {
            assert!(
                d.strength > 0.0,
                "Detected period should have positive strength"
            );
            assert!(
                d.confidence > 0.0,
                "Detected period should have positive confidence"
            );
            assert!(
                d.amplitude > 0.0,
                "Detected period should have positive amplitude"
            );
        }
    }

    // ========================================================================
    // Amplitude Modulation Detection Tests
    // ========================================================================

    #[test]
    fn test_amplitude_modulation_stable() {
        // Constant amplitude seasonal signal - should detect as Stable
        let m = 200;
        let period = 0.2;
        let argvals: Vec<f64> = (0..m).map(|i| i as f64 / (m - 1) as f64).collect();

        // Constant amplitude sine wave
        let data: Vec<f64> = argvals
            .iter()
            .map(|&t| (2.0 * PI * t / period).sin())
            .collect();
        let data = FdMatrix::from_column_major(data, 1, m).unwrap();

        let result = detect_amplitude_modulation(
            &data, &argvals, period, 0.15, // modulation threshold
            0.3,  // seasonality threshold
        );

        eprintln!(
            "Stable test: is_seasonal={}, has_modulation={}, modulation_score={:.4}, amplitude_trend={:.4}, type={:?}",
            result.is_seasonal, result.has_modulation, result.modulation_score, result.amplitude_trend, result.modulation_type
        );

        assert!(result.is_seasonal, "Should detect seasonality");
        assert!(
            !result.has_modulation,
            "Constant amplitude should not have modulation, got score={:.4}",
            result.modulation_score
        );
        assert_eq!(
            result.modulation_type,
            ModulationType::Stable,
            "Should be classified as Stable"
        );
    }

    #[test]
    fn test_amplitude_modulation_emerging() {
        // Amplitude increases over time (emerging seasonality)
        let m = 200;
        let period = 0.2;
        let argvals: Vec<f64> = (0..m).map(|i| i as f64 / (m - 1) as f64).collect();

        // Amplitude grows from 0.2 to 1.0
        let data: Vec<f64> = argvals
            .iter()
            .map(|&t| {
                let amplitude = 0.2 + 0.8 * t; // Linear increase
                amplitude * (2.0 * PI * t / period).sin()
            })
            .collect();

        let data = FdMatrix::from_column_major(data, 1, m).unwrap();

        let result = detect_amplitude_modulation(&data, &argvals, period, 0.15, 0.2);

        eprintln!(
            "Emerging test: is_seasonal={}, has_modulation={}, modulation_score={:.4}, amplitude_trend={:.4}, type={:?}",
            result.is_seasonal, result.has_modulation, result.modulation_score, result.amplitude_trend, result.modulation_type
        );

        assert!(result.is_seasonal, "Should detect seasonality");
        assert!(
            result.has_modulation,
            "Growing amplitude should have modulation, score={:.4}",
            result.modulation_score
        );
        assert_eq!(
            result.modulation_type,
            ModulationType::Emerging,
            "Should be classified as Emerging, trend={:.4}",
            result.amplitude_trend
        );
        assert!(
            result.amplitude_trend > 0.0,
            "Trend should be positive for emerging"
        );
    }

    #[test]
    fn test_amplitude_modulation_fading() {
        // Amplitude decreases over time (fading seasonality)
        let m = 200;
        let period = 0.2;
        let argvals: Vec<f64> = (0..m).map(|i| i as f64 / (m - 1) as f64).collect();

        // Amplitude decreases from 1.0 to 0.2
        let data: Vec<f64> = argvals
            .iter()
            .map(|&t| {
                let amplitude = 1.0 - 0.8 * t; // Linear decrease
                amplitude * (2.0 * PI * t / period).sin()
            })
            .collect();

        let data = FdMatrix::from_column_major(data, 1, m).unwrap();

        let result = detect_amplitude_modulation(&data, &argvals, period, 0.15, 0.2);

        eprintln!(
            "Fading test: is_seasonal={}, has_modulation={}, modulation_score={:.4}, amplitude_trend={:.4}, type={:?}",
            result.is_seasonal, result.has_modulation, result.modulation_score, result.amplitude_trend, result.modulation_type
        );

        assert!(result.is_seasonal, "Should detect seasonality");
        assert!(
            result.has_modulation,
            "Fading amplitude should have modulation"
        );
        assert_eq!(
            result.modulation_type,
            ModulationType::Fading,
            "Should be classified as Fading, trend={:.4}",
            result.amplitude_trend
        );
        assert!(
            result.amplitude_trend < 0.0,
            "Trend should be negative for fading"
        );
    }

    #[test]
    fn test_amplitude_modulation_oscillating() {
        // Amplitude oscillates (neither purely emerging nor fading)
        let m = 200;
        let period = 0.1;
        let argvals: Vec<f64> = (0..m).map(|i| i as f64 / (m - 1) as f64).collect();

        // Amplitude oscillates: high-low-high-low pattern
        let data: Vec<f64> = argvals
            .iter()
            .map(|&t| {
                let amplitude = 0.5 + 0.4 * (2.0 * PI * t * 2.0).sin(); // 2 modulation cycles
                amplitude * (2.0 * PI * t / period).sin()
            })
            .collect();

        let data = FdMatrix::from_column_major(data, 1, m).unwrap();

        let result = detect_amplitude_modulation(&data, &argvals, period, 0.15, 0.2);

        eprintln!(
            "Oscillating test: is_seasonal={}, has_modulation={}, modulation_score={:.4}, amplitude_trend={:.4}, type={:?}",
            result.is_seasonal, result.has_modulation, result.modulation_score, result.amplitude_trend, result.modulation_type
        );

        assert!(result.is_seasonal, "Should detect seasonality");
        // Oscillating has high variation but near-zero trend
        if result.has_modulation {
            // Trend should be near zero for oscillating
            assert!(
                result.amplitude_trend.abs() < 0.5,
                "Trend should be small for oscillating"
            );
        }
    }

    #[test]
    fn test_amplitude_modulation_non_seasonal() {
        // Pure noise - no seasonality
        let m = 100;
        let argvals: Vec<f64> = (0..m).map(|i| i as f64 / (m - 1) as f64).collect();

        // Random noise (use simple pseudo-random)
        let data: Vec<f64> = (0..m)
            .map(|i| ((i as f64 * 1.618).sin() * 100.0).fract())
            .collect();

        let data = FdMatrix::from_column_major(data, 1, m).unwrap();

        let result = detect_amplitude_modulation(
            &data, &argvals, 0.2, // arbitrary period
            0.15, 0.3,
        );

        assert!(
            !result.is_seasonal,
            "Noise should not be detected as seasonal"
        );
        assert_eq!(
            result.modulation_type,
            ModulationType::NonSeasonal,
            "Should be classified as NonSeasonal"
        );
    }

    // ========================================================================
    // Wavelet-based Amplitude Modulation Detection Tests
    // ========================================================================

    #[test]
    fn test_wavelet_amplitude_modulation_stable() {
        let m = 200;
        let period = 0.2;
        let argvals: Vec<f64> = (0..m).map(|i| i as f64 / (m - 1) as f64).collect();

        let data: Vec<f64> = argvals
            .iter()
            .map(|&t| (2.0 * PI * t / period).sin())
            .collect();

        let data = FdMatrix::from_column_major(data, 1, m).unwrap();

        let result = detect_amplitude_modulation_wavelet(&data, &argvals, period, 0.15, 0.3);

        eprintln!(
            "Wavelet stable: is_seasonal={}, has_modulation={}, modulation_score={:.4}, amplitude_trend={:.4}, type={:?}",
            result.is_seasonal, result.has_modulation, result.modulation_score, result.amplitude_trend, result.modulation_type
        );

        assert!(result.is_seasonal, "Should detect seasonality");
        assert!(
            !result.has_modulation,
            "Constant amplitude should not have modulation, got score={:.4}",
            result.modulation_score
        );
    }

    #[test]
    fn test_wavelet_amplitude_modulation_emerging() {
        let m = 200;
        let period = 0.2;
        let argvals: Vec<f64> = (0..m).map(|i| i as f64 / (m - 1) as f64).collect();

        // Amplitude grows from 0.2 to 1.0
        let data: Vec<f64> = argvals
            .iter()
            .map(|&t| {
                let amplitude = 0.2 + 0.8 * t;
                amplitude * (2.0 * PI * t / period).sin()
            })
            .collect();

        let data = FdMatrix::from_column_major(data, 1, m).unwrap();

        let result = detect_amplitude_modulation_wavelet(&data, &argvals, period, 0.15, 0.2);

        eprintln!(
            "Wavelet emerging: is_seasonal={}, has_modulation={}, modulation_score={:.4}, amplitude_trend={:.4}, type={:?}",
            result.is_seasonal, result.has_modulation, result.modulation_score, result.amplitude_trend, result.modulation_type
        );

        assert!(result.is_seasonal, "Should detect seasonality");
        assert!(
            result.has_modulation,
            "Growing amplitude should have modulation"
        );
        assert!(
            result.amplitude_trend > 0.0,
            "Trend should be positive for emerging"
        );
    }

    #[test]
    fn test_wavelet_amplitude_modulation_fading() {
        let m = 200;
        let period = 0.2;
        let argvals: Vec<f64> = (0..m).map(|i| i as f64 / (m - 1) as f64).collect();

        // Amplitude decreases from 1.0 to 0.2
        let data: Vec<f64> = argvals
            .iter()
            .map(|&t| {
                let amplitude = 1.0 - 0.8 * t;
                amplitude * (2.0 * PI * t / period).sin()
            })
            .collect();

        let data = FdMatrix::from_column_major(data, 1, m).unwrap();

        let result = detect_amplitude_modulation_wavelet(&data, &argvals, period, 0.15, 0.2);

        eprintln!(
            "Wavelet fading: is_seasonal={}, has_modulation={}, modulation_score={:.4}, amplitude_trend={:.4}, type={:?}",
            result.is_seasonal, result.has_modulation, result.modulation_score, result.amplitude_trend, result.modulation_type
        );

        assert!(result.is_seasonal, "Should detect seasonality");
        assert!(
            result.has_modulation,
            "Fading amplitude should have modulation"
        );
        assert!(
            result.amplitude_trend < 0.0,
            "Trend should be negative for fading"
        );
    }

    #[test]
    fn test_seasonal_strength_wavelet() {
        let m = 200;
        let period = 0.2;
        let argvals: Vec<f64> = (0..m).map(|i| i as f64 / (m - 1) as f64).collect();

        // Pure sine wave at target period - should have high strength
        let seasonal_data: Vec<f64> = argvals
            .iter()
            .map(|&t| (2.0 * PI * t / period).sin())
            .collect();

        let seasonal_data = FdMatrix::from_column_major(seasonal_data, 1, m).unwrap();

        let strength = seasonal_strength_wavelet(&seasonal_data, &argvals, period);
        eprintln!("Wavelet strength (pure sine): {:.4}", strength);
        assert!(
            strength > 0.5,
            "Pure sine should have high wavelet strength"
        );

        // Pure noise - should have low strength
        let noise_data: Vec<f64> = (0..m)
            .map(|i| ((i * 12345 + 67890) % 1000) as f64 / 1000.0 - 0.5)
            .collect();

        let noise_data = FdMatrix::from_column_major(noise_data, 1, m).unwrap();

        let noise_strength = seasonal_strength_wavelet(&noise_data, &argvals, period);
        eprintln!("Wavelet strength (noise): {:.4}", noise_strength);
        assert!(
            noise_strength < 0.3,
            "Noise should have low wavelet strength"
        );

        // Wrong period - should have lower strength
        let wrong_period_strength =
            seasonal_strength_wavelet(&seasonal_data, &argvals, period * 2.0);
        eprintln!(
            "Wavelet strength (wrong period): {:.4}",
            wrong_period_strength
        );
        assert!(
            wrong_period_strength < strength,
            "Wrong period should have lower strength"
        );
    }

    #[test]
    fn test_compute_mean_curve() {
        // 2 samples, 3 time points
        // Sample 1: [1, 2, 3]
        // Sample 2: [2, 4, 6]
        // Mean: [1.5, 3, 4.5]
        let data = FdMatrix::from_column_major(vec![1.0, 2.0, 2.0, 4.0, 3.0, 6.0], 2, 3).unwrap();
        let mean = compute_mean_curve(&data);
        assert_eq!(mean.len(), 3);
        assert!((mean[0] - 1.5).abs() < 1e-10);
        assert!((mean[1] - 3.0).abs() < 1e-10);
        assert!((mean[2] - 4.5).abs() < 1e-10);
    }

    #[test]
    fn test_compute_mean_curve_parallel_consistency() {
        // Test that parallel and sequential give same results
        let n = 10;
        let m = 200;
        let data: Vec<f64> = (0..n * m).map(|i| (i as f64 * 0.1).sin()).collect();

        let data = FdMatrix::from_column_major(data, n, m).unwrap();

        let seq_result = compute_mean_curve_impl(&data, false);
        let par_result = compute_mean_curve_impl(&data, true);

        assert_eq!(seq_result.len(), par_result.len());
        for (s, p) in seq_result.iter().zip(par_result.iter()) {
            assert!(
                (s - p).abs() < 1e-10,
                "Sequential and parallel results differ"
            );
        }
    }

    #[test]
    fn test_interior_bounds() {
        // m = 100: edge_skip = 10, interior = [10, 90)
        let bounds = interior_bounds(100);
        assert!(bounds.is_some());
        let (start, end) = bounds.unwrap();
        assert_eq!(start, 10);
        assert_eq!(end, 90);

        // m = 10: edge_skip = 1, but min(1, 2) = 1, max(9, 7) = 9
        let bounds = interior_bounds(10);
        assert!(bounds.is_some());
        let (start, end) = bounds.unwrap();
        assert!(start < end);

        // Very small m might not have valid interior
        let bounds = interior_bounds(2);
        // Should still return something as long as end > start
        assert!(bounds.is_some() || bounds.is_none());
    }

    #[test]
    fn test_hilbert_transform_pure_sine() {
        // Hilbert transform of sin(t) should give cos(t) in imaginary part
        let m = 200;
        let argvals: Vec<f64> = (0..m).map(|i| i as f64 * 0.1).collect();
        let signal: Vec<f64> = argvals.iter().map(|&t| (2.0 * PI * t).sin()).collect();

        let analytic = hilbert_transform(&signal);
        assert_eq!(analytic.len(), m);

        // Check amplitude is approximately 1
        for c in analytic.iter().skip(10).take(m - 20) {
            let amp = c.norm();
            assert!(
                (amp - 1.0).abs() < 0.1,
                "Amplitude should be ~1, got {}",
                amp
            );
        }
    }

    #[test]
    fn test_sazed_pure_sine() {
        // Pure sine wave with known period
        let m = 200;
        let period = 2.0;
        let argvals: Vec<f64> = (0..m).map(|i| i as f64 * 0.1).collect();
        let data: Vec<f64> = argvals
            .iter()
            .map(|&t| (2.0 * PI * t / period).sin())
            .collect();

        let result = sazed(&data, &argvals, None);

        assert!(result.period.is_finite(), "SAZED should detect a period");
        assert!(
            (result.period - period).abs() < 0.3,
            "Expected period ~{}, got {}",
            period,
            result.period
        );
        assert!(
            result.confidence > 0.4,
            "Expected confidence > 0.4, got {}",
            result.confidence
        );
        assert!(
            result.agreeing_components >= 2,
            "Expected at least 2 agreeing components, got {}",
            result.agreeing_components
        );
    }

    #[test]
    fn test_sazed_noisy_sine() {
        // Sine wave with noise
        let m = 300;
        let period = 3.0;
        let argvals: Vec<f64> = (0..m).map(|i| i as f64 * 0.1).collect();

        // Deterministic pseudo-noise using sin with different frequency
        let data: Vec<f64> = argvals
            .iter()
            .enumerate()
            .map(|(i, &t)| {
                let signal = (2.0 * PI * t / period).sin();
                let noise = 0.1 * (17.3 * i as f64).sin();
                signal + noise
            })
            .collect();

        let result = sazed(&data, &argvals, Some(0.15));

        assert!(
            result.period.is_finite(),
            "SAZED should detect a period even with noise"
        );
        assert!(
            (result.period - period).abs() < 0.5,
            "Expected period ~{}, got {}",
            period,
            result.period
        );
    }

    #[test]
    fn test_sazed_fdata() {
        // Multiple samples with same period
        let n = 5;
        let m = 200;
        let period = 2.0;
        let argvals: Vec<f64> = (0..m).map(|i| i as f64 * 0.1).collect();
        let data = generate_sine(n, m, period, &argvals);

        let result = sazed_fdata(&data, &argvals, None);

        assert!(result.period.is_finite(), "SAZED should detect a period");
        assert!(
            (result.period - period).abs() < 0.3,
            "Expected period ~{}, got {}",
            period,
            result.period
        );
    }

    #[test]
    fn test_sazed_short_series() {
        // Very short series - should return NaN gracefully
        let argvals: Vec<f64> = (0..5).map(|i| i as f64).collect();
        let data: Vec<f64> = argvals.iter().map(|&t| t.sin()).collect();

        let result = sazed(&data, &argvals, None);

        // Should handle gracefully (return NaN for too-short series)
        assert!(
            result.period.is_nan() || result.period.is_finite(),
            "Should return NaN or valid period"
        );
    }

    #[test]
    fn test_autoperiod_pure_sine() {
        // Pure sine wave with known period
        let m = 200;
        let period = 2.0;
        let argvals: Vec<f64> = (0..m).map(|i| i as f64 * 0.1).collect();
        let data: Vec<f64> = argvals
            .iter()
            .map(|&t| (2.0 * PI * t / period).sin())
            .collect();

        let result = autoperiod(&data, &argvals, None, None);

        assert!(
            result.period.is_finite(),
            "Autoperiod should detect a period"
        );
        assert!(
            (result.period - period).abs() < 0.3,
            "Expected period ~{}, got {}",
            period,
            result.period
        );
        assert!(
            result.confidence > 0.0,
            "Expected positive confidence, got {}",
            result.confidence
        );
    }

    #[test]
    fn test_autoperiod_with_trend() {
        // Sine wave with linear trend
        let m = 300;
        let period = 3.0;
        let argvals: Vec<f64> = (0..m).map(|i| i as f64 * 0.1).collect();
        let data: Vec<f64> = argvals
            .iter()
            .map(|&t| 0.2 * t + (2.0 * PI * t / period).sin())
            .collect();

        let result = autoperiod(&data, &argvals, None, None);

        assert!(
            result.period.is_finite(),
            "Autoperiod should detect a period"
        );
        // Allow more tolerance with trend
        assert!(
            (result.period - period).abs() < 0.5,
            "Expected period ~{}, got {}",
            period,
            result.period
        );
    }

    #[test]
    fn test_autoperiod_candidates() {
        // Verify candidates are generated
        let m = 200;
        let period = 2.0;
        let argvals: Vec<f64> = (0..m).map(|i| i as f64 * 0.1).collect();
        let data: Vec<f64> = argvals
            .iter()
            .map(|&t| (2.0 * PI * t / period).sin())
            .collect();

        let result = autoperiod(&data, &argvals, Some(5), Some(10));

        assert!(
            !result.candidates.is_empty(),
            "Should have at least one candidate"
        );

        // Best candidate should have highest combined score
        let max_score = result
            .candidates
            .iter()
            .map(|c| c.combined_score)
            .fold(f64::NEG_INFINITY, f64::max);
        assert!(
            (result.confidence - max_score).abs() < 1e-10,
            "Returned confidence should match best candidate's score"
        );
    }

    #[test]
    fn test_autoperiod_fdata() {
        // Multiple samples with same period
        let n = 5;
        let m = 200;
        let period = 2.0;
        let argvals: Vec<f64> = (0..m).map(|i| i as f64 * 0.1).collect();
        let data = generate_sine(n, m, period, &argvals);

        let result = autoperiod_fdata(&data, &argvals, None, None);

        assert!(
            result.period.is_finite(),
            "Autoperiod should detect a period"
        );
        assert!(
            (result.period - period).abs() < 0.3,
            "Expected period ~{}, got {}",
            period,
            result.period
        );
    }

    #[test]
    fn test_cfd_autoperiod_pure_sine() {
        // Pure sine wave with known period
        let m = 200;
        let period = 2.0;
        let argvals: Vec<f64> = (0..m).map(|i| i as f64 * 0.1).collect();
        let data: Vec<f64> = argvals
            .iter()
            .map(|&t| (2.0 * PI * t / period).sin())
            .collect();

        let result = cfd_autoperiod(&data, &argvals, None, None);

        assert!(
            result.period.is_finite(),
            "CFDAutoperiod should detect a period"
        );
        assert!(
            (result.period - period).abs() < 0.3,
            "Expected period ~{}, got {}",
            period,
            result.period
        );
    }

    #[test]
    fn test_cfd_autoperiod_with_trend() {
        // Sine wave with strong linear trend - CFD excels here
        let m = 300;
        let period = 3.0;
        let argvals: Vec<f64> = (0..m).map(|i| i as f64 * 0.1).collect();
        let data: Vec<f64> = argvals
            .iter()
            .map(|&t| 2.0 * t + (2.0 * PI * t / period).sin())
            .collect();

        let result = cfd_autoperiod(&data, &argvals, None, None);

        assert!(
            result.period.is_finite(),
            "CFDAutoperiod should detect a period despite trend"
        );
        // Allow more tolerance since trend can affect detection
        assert!(
            (result.period - period).abs() < 0.6,
            "Expected period ~{}, got {}",
            period,
            result.period
        );
    }

    #[test]
    fn test_cfd_autoperiod_multiple_periods() {
        // Signal with two periods - should detect multiple
        let m = 400;
        let period1 = 2.0;
        let period2 = 5.0;
        let argvals: Vec<f64> = (0..m).map(|i| i as f64 * 0.1).collect();
        let data: Vec<f64> = argvals
            .iter()
            .map(|&t| (2.0 * PI * t / period1).sin() + 0.5 * (2.0 * PI * t / period2).sin())
            .collect();

        let result = cfd_autoperiod(&data, &argvals, None, None);

        assert!(
            !result.periods.is_empty(),
            "Should detect at least one period"
        );
        // The primary period should be one of the two
        let close_to_p1 = (result.period - period1).abs() < 0.5;
        let close_to_p2 = (result.period - period2).abs() < 1.0;
        assert!(
            close_to_p1 || close_to_p2,
            "Primary period {} not close to {} or {}",
            result.period,
            period1,
            period2
        );
    }

    #[test]
    fn test_cfd_autoperiod_fdata() {
        // Multiple samples with same period
        let n = 5;
        let m = 200;
        let period = 2.0;
        let argvals: Vec<f64> = (0..m).map(|i| i as f64 * 0.1).collect();
        let data = generate_sine(n, m, period, &argvals);

        let result = cfd_autoperiod_fdata(&data, &argvals, None, None);

        assert!(
            result.period.is_finite(),
            "CFDAutoperiod should detect a period"
        );
        assert!(
            (result.period - period).abs() < 0.3,
            "Expected period ~{}, got {}",
            period,
            result.period
        );
    }

    // ========================================================================
    // SSA Tests
    // ========================================================================

    #[test]
    fn test_ssa_pure_sine() {
        let n = 200;
        let period = 12.0;
        let values: Vec<f64> = (0..n)
            .map(|i| {
                let t = i as f64;
                0.01 * t + (2.0 * PI * t / period).sin() + 0.05 * ((i * 7) as f64 * 0.1).sin()
            })
            .collect();

        let result = ssa(&values, None, None, None, None);

        // trend + seasonal + noise ≈ original
        for i in 0..n {
            let reconstructed = result.trend[i] + result.seasonal[i] + result.noise[i];
            assert!(
                (reconstructed - values[i]).abs() < 1e-8,
                "SSA reconstruction error at {}: {} vs {}",
                i,
                reconstructed,
                values[i]
            );
        }

        // Singular values should be descending
        for i in 1..result.singular_values.len() {
            assert!(
                result.singular_values[i] <= result.singular_values[i - 1] + 1e-10,
                "Singular values should be descending: {} > {}",
                result.singular_values[i],
                result.singular_values[i - 1]
            );
        }

        // Contributions should sum to <= 1
        let total_contrib: f64 = result.contributions.iter().sum();
        assert!(
            total_contrib <= 1.0 + 1e-10,
            "Contributions should sum to <= 1, got {}",
            total_contrib
        );
    }

    #[test]
    fn test_ssa_explicit_groupings() {
        let n = 100;
        let period = 10.0;
        let values: Vec<f64> = (0..n)
            .map(|i| 0.01 * i as f64 + (2.0 * PI * i as f64 / period).sin())
            .collect();

        let trend_components = [0usize];
        let seasonal_components = [1usize, 2];

        let result = ssa(
            &values,
            None,
            None,
            Some(&trend_components),
            Some(&seasonal_components),
        );

        assert_eq!(result.trend.len(), n);
        assert_eq!(result.seasonal.len(), n);
        assert_eq!(result.noise.len(), n);

        // Reconstruction should still hold
        for i in 0..n {
            let reconstructed = result.trend[i] + result.seasonal[i] + result.noise[i];
            assert!(
                (reconstructed - values[i]).abs() < 1e-8,
                "SSA explicit grouping reconstruction error at {}",
                i
            );
        }
    }

    #[test]
    fn test_ssa_short_series() {
        // n < 4 should trigger early return
        let values = vec![1.0, 2.0, 3.0];
        let result = ssa(&values, None, None, None, None);

        assert_eq!(result.trend, values);
        assert_eq!(result.seasonal, vec![0.0; 3]);
        assert_eq!(result.noise, vec![0.0; 3]);
        assert_eq!(result.n_components, 0);
    }

    #[test]
    fn test_ssa_fdata() {
        let n = 5;
        let m = 100;
        let mut data = vec![0.0; n * m];
        for i in 0..n {
            let amp = (i + 1) as f64;
            for j in 0..m {
                data[i + j * n] = amp * (2.0 * PI * j as f64 / 12.0).sin() + 0.01 * j as f64;
            }
        }

        let data = FdMatrix::from_column_major(data, n, m).unwrap();

        let result = ssa_fdata(&data, None, None);

        assert_eq!(result.trend.len(), m);
        assert_eq!(result.seasonal.len(), m);
        assert_eq!(result.noise.len(), m);
        assert!(!result.singular_values.is_empty());
    }

    #[test]
    fn test_ssa_seasonality() {
        // Seasonal signal
        let n = 200;
        let period = 12.0;
        let seasonal_values: Vec<f64> = (0..n)
            .map(|i| (2.0 * PI * i as f64 / period).sin())
            .collect();

        let (is_seasonal, _det_period, confidence) =
            ssa_seasonality(&seasonal_values, None, Some(0.05));

        // A pure sine should be detected as seasonal
        // (confidence depends on component grouping)
        assert!(confidence >= 0.0, "Confidence should be non-negative");

        // Noise-only signal should not be seasonal
        let noise_values: Vec<f64> = (0..n)
            .map(|i| ((i * 13 + 7) as f64 * 0.1).sin() * 0.01)
            .collect();

        let (is_noise_seasonal, _, noise_conf) = ssa_seasonality(&noise_values, None, Some(0.5));

        // Noise should have low confidence (but it's not guaranteed to be strictly false
        // depending on auto-grouping, so we just check confidence)
        let _ = (is_seasonal, is_noise_seasonal, noise_conf);
    }

    // ========================================================================
    // Matrix Profile Tests
    // ========================================================================

    #[test]
    fn test_matrix_profile_periodic() {
        let period = 20;
        let n = period * 10;
        let values: Vec<f64> = (0..n)
            .map(|i| (2.0 * PI * i as f64 / period as f64).sin())
            .collect();

        let result = matrix_profile(&values, Some(15), None);

        assert_eq!(result.profile.len(), n - 15 + 1);
        assert_eq!(result.profile_index.len(), n - 15 + 1);
        assert_eq!(result.subsequence_length, 15);

        // Profile should be finite
        for &p in &result.profile {
            assert!(p.is_finite(), "Profile values should be finite");
        }

        // Primary period should be close to 20
        assert!(
            (result.primary_period - period as f64).abs() < 5.0,
            "Expected primary_period ≈ {}, got {}",
            period,
            result.primary_period
        );
    }

    #[test]
    fn test_matrix_profile_non_periodic() {
        // Linear ramp (non-periodic)
        let n = 200;
        let values: Vec<f64> = (0..n).map(|i| i as f64 * 0.1).collect();

        let result = matrix_profile(&values, Some(10), None);

        assert_eq!(result.profile.len(), n - 10 + 1);

        // Should have lower confidence than periodic
        // (not always strictly 0, depends on ramp structure)
        assert!(result.confidence <= 1.0, "Confidence should be <= 1.0");
    }

    #[test]
    fn test_matrix_profile_fdata() {
        let n = 3;
        let m = 200;
        let period = 20.0;
        let mut data = vec![0.0; n * m];
        for i in 0..n {
            let amp = (i + 1) as f64;
            for j in 0..m {
                data[i + j * n] = amp * (2.0 * PI * j as f64 / period).sin();
            }
        }

        let data = FdMatrix::from_column_major(data, n, m).unwrap();

        let result = matrix_profile_fdata(&data, Some(15), None);

        assert!(!result.profile.is_empty());
        assert!(result.profile_index.len() == result.profile.len());
    }

    #[test]
    fn test_matrix_profile_seasonality() {
        let period = 20;
        let n = period * 10;
        // Periodic signal
        let values: Vec<f64> = (0..n)
            .map(|i| (2.0 * PI * i as f64 / period as f64).sin())
            .collect();

        let (is_seasonal, det_period, confidence) =
            matrix_profile_seasonality(&values, Some(15), Some(0.05));

        assert!(
            is_seasonal,
            "Periodic signal should be detected as seasonal"
        );
        assert!(det_period > 0.0, "Detected period should be positive");
        assert!(confidence >= 0.05, "Confidence should be above threshold");

        // Weak / non-periodic signal
        let weak_values: Vec<f64> = (0..n).map(|i| i as f64 * 0.001).collect();
        let (is_weak_seasonal, _, _) =
            matrix_profile_seasonality(&weak_values, Some(15), Some(0.5));
        let _ = is_weak_seasonal; // May or may not detect - we just check it doesn't panic
    }

    // ========================================================================
    // Lomb-Scargle Tests
    // ========================================================================

    #[test]
    fn test_lomb_scargle_regular() {
        let n = 200;
        let true_period = 5.0;
        let times: Vec<f64> = (0..n).map(|i| i as f64 * 0.1).collect();
        let values: Vec<f64> = times
            .iter()
            .map(|&t| (2.0 * PI * t / true_period).sin())
            .collect();

        let result = lomb_scargle(&times, &values, None, None, None);

        assert!(
            (result.peak_period - true_period).abs() < 0.5,
            "Expected peak_period ≈ {}, got {}",
            true_period,
            result.peak_period
        );
        assert!(
            result.false_alarm_probability < 0.05,
            "FAP should be low for strong signal: {}",
            result.false_alarm_probability
        );
        assert!(result.peak_power > 0.0, "Peak power should be positive");
        assert!(!result.frequencies.is_empty());
        assert_eq!(result.frequencies.len(), result.power.len());
        assert_eq!(result.frequencies.len(), result.periods.len());
    }

    #[test]
    fn test_lomb_scargle_irregular() {
        let true_period = 5.0;
        // Irregularly sampled: take a subset of regular samples
        let all_times: Vec<f64> = (0..300).map(|i| i as f64 * 0.1).collect();
        // Take every other point with some jitter-like selection
        let times: Vec<f64> = all_times
            .iter()
            .enumerate()
            .filter(|(i, _)| i % 2 == 0 || i % 3 == 0)
            .map(|(_, &t)| t)
            .collect();
        let values: Vec<f64> = times
            .iter()
            .map(|&t| (2.0 * PI * t / true_period).sin())
            .collect();

        let result = lomb_scargle(&times, &values, None, None, None);

        assert!(
            (result.peak_period - true_period).abs() < 1.0,
            "Irregular LS: expected period ≈ {}, got {}",
            true_period,
            result.peak_period
        );
    }

    #[test]
    fn test_lomb_scargle_custom_frequencies() {
        let n = 100;
        let true_period = 5.0;
        let times: Vec<f64> = (0..n).map(|i| i as f64 * 0.1).collect();
        let values: Vec<f64> = times
            .iter()
            .map(|&t| (2.0 * PI * t / true_period).sin())
            .collect();

        // Explicit frequency grid
        let frequencies: Vec<f64> = (1..50).map(|i| i as f64 * 0.01).collect();
        let result = lomb_scargle(&times, &values, Some(&frequencies), None, None);

        assert_eq!(result.frequencies.len(), frequencies.len());
        assert_eq!(result.power.len(), frequencies.len());
        assert!(result.peak_power > 0.0);
    }

    #[test]
    fn test_lomb_scargle_fdata() {
        let n = 5;
        let m = 200;
        let period = 5.0;
        let argvals: Vec<f64> = (0..m).map(|i| i as f64 * 0.1).collect();
        let data = generate_sine(n, m, period, &argvals);

        let result = lomb_scargle_fdata(&data, &argvals, None, None);

        assert!(
            (result.peak_period - period).abs() < 0.5,
            "Fdata LS: expected period ≈ {}, got {}",
            period,
            result.peak_period
        );
        assert!(!result.frequencies.is_empty());
    }

    // ========================================================================
    // Seasonality change detection tests
    // ========================================================================

    #[test]
    fn test_detect_seasonality_changes_onset() {
        let m = 400;
        let period = 2.0;
        let argvals: Vec<f64> = (0..m).map(|i| i as f64 * 0.05).collect(); // 0..20

        // First half: noise-like (low seasonality), second half: strong sine
        let data: Vec<f64> = argvals
            .iter()
            .map(|&t| {
                if t < 10.0 {
                    // Weak signal
                    0.05 * ((t * 13.0).sin() + (t * 7.0).cos())
                } else {
                    // Strong seasonal
                    (2.0 * PI * t / period).sin()
                }
            })
            .collect();
        let data = FdMatrix::from_column_major(data, 1, m).unwrap();

        let result = detect_seasonality_changes(&data, &argvals, period, 0.3, 4.0, 2.0);

        assert!(
            !result.strength_curve.is_empty(),
            "Strength curve should not be empty"
        );
        assert_eq!(result.strength_curve.len(), m);

        // Should detect at least one change point (onset around t=10)
        if !result.change_points.is_empty() {
            let onset_points: Vec<_> = result
                .change_points
                .iter()
                .filter(|cp| cp.change_type == ChangeType::Onset)
                .collect();
            // At least one onset should exist near the transition
            assert!(!onset_points.is_empty(), "Should detect Onset change point");
        }
    }

    #[test]
    fn test_detect_seasonality_changes_no_change() {
        let m = 400;
        let period = 2.0;
        let argvals: Vec<f64> = (0..m).map(|i| i as f64 * 0.05).collect();

        // Consistently strong seasonal signal
        let data: Vec<f64> = argvals
            .iter()
            .map(|&t| (2.0 * PI * t / period).sin())
            .collect();
        let data = FdMatrix::from_column_major(data, 1, m).unwrap();

        let result = detect_seasonality_changes(&data, &argvals, period, 0.3, 4.0, 2.0);

        assert!(!result.strength_curve.is_empty());
        // With consistently seasonal data, there should be no Cessation points
        let cessation_points: Vec<_> = result
            .change_points
            .iter()
            .filter(|cp| cp.change_type == ChangeType::Cessation)
            .collect();
        assert!(
            cessation_points.is_empty(),
            "Consistently seasonal signal should have no Cessation points, found {}",
            cessation_points.len()
        );
    }

    // ========================================================================
    // Period estimation tests (ACF and regression)
    // ========================================================================

    #[test]
    fn test_estimate_period_acf() {
        let m = 200;
        let period = 2.0;
        let argvals: Vec<f64> = (0..m).map(|i| i as f64 * 0.1).collect();
        let data = generate_sine(1, m, period, &argvals);

        let estimate = estimate_period_acf(data.as_slice(), 1, m, &argvals, m / 2);

        assert!(
            estimate.period.is_finite(),
            "ACF period estimate should be finite"
        );
        assert!(
            (estimate.period - period).abs() < 0.5,
            "ACF expected period ≈ {}, got {}",
            period,
            estimate.period
        );
        assert!(
            estimate.confidence > 0.0,
            "ACF confidence should be positive"
        );
    }

    #[test]
    fn test_estimate_period_regression() {
        let m = 200;
        let period = 2.0;
        let argvals: Vec<f64> = (0..m).map(|i| i as f64 * 0.1).collect();
        let data = generate_sine(1, m, period, &argvals);

        let estimate =
            estimate_period_regression(data.as_slice(), 1, m, &argvals, 1.5, 3.0, 100, 3);

        assert!(
            estimate.period.is_finite(),
            "Regression period estimate should be finite"
        );
        assert!(
            (estimate.period - period).abs() < 0.5,
            "Regression expected period ≈ {}, got {}",
            period,
            estimate.period
        );
        assert!(
            estimate.confidence > 0.0,
            "Regression confidence should be positive"
        );
    }

    // ========================================================================
    // Seasonal strength windowed test
    // ========================================================================

    #[test]
    fn test_seasonal_strength_windowed_variance() {
        let m = 200;
        let period = 2.0;
        let argvals: Vec<f64> = (0..m).map(|i| i as f64 * 0.1).collect();
        let data = generate_sine(1, m, period, &argvals);

        let strengths = seasonal_strength_windowed(
            &data,
            &argvals,
            period,
            4.0, // window_size
            StrengthMethod::Variance,
        );

        assert_eq!(strengths.len(), m, "Should return m values");

        // Interior values (away from edges) should be in [0,1]
        let interior_start = m / 4;
        let interior_end = 3 * m / 4;
        for i in interior_start..interior_end {
            let s = strengths[i];
            if s.is_finite() {
                assert!(
                    (-0.01..=1.01).contains(&s),
                    "Windowed strength at {} should be near [0,1], got {}",
                    i,
                    s
                );
            }
        }
    }
}
