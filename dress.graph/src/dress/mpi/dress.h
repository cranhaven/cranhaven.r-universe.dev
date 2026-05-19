/**
 * dress/mpi/dress.h — MPI-distributed DRESS via include-based switching.
 *
 * Drop-in replacement for dress/dress.h (+ optionally dress/cuda/dress.h).
 * Including this header redirects dress_delta_fit() to the MPI-distributed
 * implementation — no source changes required:
 *
 *   // Single-process (CPU)
 *   #include "dress/dress.h"
 *   dress_delta_fit(g, 2, 100, 1e-6, &hs, 0, NULL, &ns);
 *
 *   // MPI-distributed (CPU) — same call, different include
 *   #include "dress/mpi/dress.h"
 *   dress_delta_fit(g, 2, 100, 1e-6, &hs, 0, NULL, &ns);
 *
 *   // MPI-distributed (GPU) — include CUDA header first
 *   #include "dress/cuda/dress_cuda.h"
 *   #include "dress/mpi/dress.h"
 *   dress_delta_fit(g, 2, 100, 1e-6, &hs, 0, NULL, &ns);
 *
 * The redirect macros append MPI_COMM_WORLD as the communicator.
 * For a custom communicator, call dress_delta_fit_mpi() or
 * dress_delta_fit_mpi_cuda() directly from dress/mpi/dress_mpi.h.
 *
 * Do not include both this header and dress/dress.h (or dress/cuda/dress.h)
 * in the same translation unit — the macros will conflict.
 */

#ifndef DRESS_MPI_REDIRECT_H
#define DRESS_MPI_REDIRECT_H

#include "dress/dress.h"
#include "dress/mpi/dress_mpi.h"

/* Redirect Δ^k-DRESS to MPI implementation. */

#ifdef DRESS_CUDA_H

/* CUDA header was included before us — redirect to GPU + MPI.
   Undefine the simple CUDA aliases so we can replace them with
   MPI+CUDA function-like macros. */
#undef dress_delta_fit
#undef dress_delta_fit_strided
#undef dress_nabla_fit
#undef dress_nabla_fit_strided

#define dress_delta_fit(g, k, it, eps, nsamp, sd, hs, km, ms, ns) \
    dress_delta_fit_mpi_cuda((g), (k), (it), (eps), (nsamp), (sd), (hs), (km), (ms), (ns), \
                             MPI_COMM_WORLD)

#define dress_delta_fit_strided(g, k, it, eps, nsamp, sd, hs, km, ms, ns, off, str) \
    dress_delta_fit_cuda_strided((g), (k), (it), (eps), (nsamp), (sd), (hs), (km), (ms), \
                                 (ns), (off), (str))

#define dress_nabla_fit(g, k, it, eps, nsamp, sd, hs, km, ms, ns) \
    dress_nabla_fit_mpi_cuda((g), (k), (it), (eps), (nsamp), (sd), (hs), (km), (ms), (ns), \
                             MPI_COMM_WORLD)

#define dress_fit dress_fit_cuda

#elif defined(DRESS_OMP_H)

/* OMP header was included before us — redirect to OMP + MPI. */
#undef dress_delta_fit
#undef dress_delta_fit_strided
#undef dress_nabla_fit
#undef dress_nabla_fit_strided

#define dress_delta_fit(g, k, it, eps, nsamp, sd, hs, km, ms, ns) \
    dress_delta_fit_mpi_omp((g), (k), (it), (eps), (nsamp), (sd), (hs), (km), (ms), (ns), \
                            MPI_COMM_WORLD)

#define dress_nabla_fit(g, k, it, eps, nsamp, sd, hs, km, ms, ns) \
    dress_nabla_fit_mpi_omp((g), (k), (it), (eps), (nsamp), (sd), (hs), (km), (ms), (ns), \
                            MPI_COMM_WORLD)

#define dress_fit dress_fit_omp

#else

/* CPU + MPI. */
#define dress_delta_fit(g, k, it, eps, nsamp, sd, hs, km, ms, ns) \
    dress_delta_fit_mpi((g), (k), (it), (eps), (nsamp), (sd), (hs), (km), (ms), (ns), \
                        MPI_COMM_WORLD)

#define dress_nabla_fit(g, k, it, eps, nsamp, sd, hs, km, ms, ns) \
    dress_nabla_fit_mpi((g), (k), (it), (eps), (nsamp), (sd), (hs), (km), (ms), (ns), \
                        MPI_COMM_WORLD)

#endif /* DRESS_CUDA_H / DRESS_OMP_H */

#endif /* DRESS_MPI_REDIRECT_H */
