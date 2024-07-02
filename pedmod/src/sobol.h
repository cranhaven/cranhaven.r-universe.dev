/**
 * Copyright (C) May 2021, Benjamin Christoffersen, re-wrote the Fortran code
 * in C++. All the data needed to generate the next number in the Sobol
 * sequence is kept as private members of a class.
 *
 * Copyright (C) Apr. 2011, Christophe Dutang, remove implicit declaration: the
 * code now pass
 * > gfortran -c -fsyntax-only -fimplicit-none LowDiscrepancy.f
 * without error.
 *
 * Copyright (C) Oct. 2009, Christophe Dutang, slightly modified (better
 * accuracy and speed).
 *
 * Copyright (C) Sept. 2002, Diethelm Wuertz, ETH Zurich. All rights reserved.
 *
 * The new BSD License is applied to this software.
 * Copyright (c) Diethelm Wuertz, ETH Zurich. All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 *     Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *
 *     Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in
 *     the documentation and/or other materials provided with the
 *     distribution.
 *
 *     Neither the name of the <ORGANIZATION> nor the names of its
 *     contributors may be used to endorse or promote products derived
 *     from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#ifndef PEDMOD_SOBOL_H
#define PEDMOD_SOBOL_H

#include <memory.h>
#include <vector>
#include <algorithm>
#include <memory>
#include <stdexcept>

namespace pedmod {
/// simple matrix class
template<typename T>
class simple_matrix {
  std::unique_ptr<T[]> dat;
  size_t const n_rows = 0;

public:
  simple_matrix(size_t const n_rows, size_t const n_cols):
  dat(new T[n_rows * n_cols]), n_rows(n_rows)  { }

  /// initialize an 0x0 matrix
  simple_matrix() = default;

  /// returns a pointer to the data
  T * data() noexcept {
    return dat.get();
  }

  /// returns a pointer to the column. Zero-based indices are used
  T * col_ptr(size_t const col) noexcept {
    return data() + col * n_rows;
  }

  /// returns the element at a given index. Zero-based indices are used
  T& operator()(size_t const row, size_t const col) noexcept {
    return *(data() + row + col * n_rows);
  }
};

/// computes a sobol sequence
class sobol {
public:
  /// dimension of the variable
  size_t const dimen = 0;

private:
  static constexpr size_t max_dim = 1111,
                          max_deg = 13,
                          max_bit = 30;

  std::unique_ptr<double[]> quasi;
  unsigned count = 0;
  simple_matrix<int> sv;
  int ll = 0;

public:
  /// the types of scrambling methods
  enum scrambling_type : int {
    none = 0,
      owen = 1,
      faure_tezuka = 2,
      owen_and_faure_tezuka = 3
  };

  /**
   * returns a pointer to the current qausi-random numbers. This is
   * beneficial as we can avoid a copy when one uses the C++ interface.
   */
  double const * get_qausi() const noexcept {
    return quasi.get();
  }

  /**
   * initialize the members to compute the first quasi-random number in the
   * sequence.
   */
  sobol(unsigned const dimen_arg, scrambling_type const scrambling_method,
        int const i_seed);

  /// initialize an object with dimension zero
  sobol() = default;

  /// computes the next quasi random number
  void next() noexcept {
    size_t const l = ([](unsigned count) -> size_t {
      size_t out(0);
      while((count % 2L) == 1L){
        count /= 2L;
        ++out;
      }

      return out;
    })(count);

    for(size_t i = 0; i < dimen; ++i){
      int const tmp = static_cast<int>(quasi[i] * ll);
      quasi[i] = static_cast<double>(tmp ^ sv(i, l)) /
        static_cast<double>(ll);
    }

    ++count;
  }
};

} // namespace pedmod

#endif
