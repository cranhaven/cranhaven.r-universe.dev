//*******************************************
//   Copyright (C) 2014 by Ignace Bogaert   *
//*******************************************

// This software package is based on the paper
//    I. Bogaert, "Iteration-Free Computation of Gauss-Legendre Quadrature Nodes and Weights",
//    to be published in the SIAM Journal of Scientific Computing.

// The main features of this software are:
// - Speed: due to the simple formulas and the O(1) complexity computation of individual Gauss-Legendre
//   quadrature nodes and weights. This makes it compatible with parallel computing paradigms.
// - Accuracy: the error on the nodes and weights is within a few ulps (see the paper for details).

// Disclaimer:
// THIS SOFTWARE IS PROVIDED "AS IS" AND ANY EXPRESSED OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
// WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR
// CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
// BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
// HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
// OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

#ifndef FASTGL_H
#define FASTGL_H

#include <stddef.h>
#include <vector>
#include <cmath>

// Functions for fastgl in double precision
namespace fastgl {
// A struct for containing a Node-Weight pair
template<class Type = double>
struct QuadPair {
  Type theta, weight,
       x = cos(theta);

	template<class T>
	QuadPair(T const t, T const w):
	  theta(t), weight(w) {}

	template<class T>
	QuadPair(QuadPair<T> const &other){
	  theta = other.theta;
	  weight = other.weight;
	  x = other.x;
	}

	QuadPair() = default;
};

// Function for getting Gauss-Legendre nodes & weights
// Theta values of the zeros are in [0,pi], and monotonically increasing.
// The index of the zero k should always be in [1,n].
// Compute a node-weight pair:
QuadPair<double> GLPair(size_t const n, size_t const k);

template<class Type>
std::vector<QuadPair<Type> > const& GLPairsCached(size_t const);

constexpr size_t fastglCachedMaxArg(){
  return 100L;
}
} // namespace fastgl

#endif
