#    RcppExports.R: Wrapper for C++ functions in dpparmadillo.cpp.
#    Copyright (C) 2018  Matthew T. Pratola <mpratola@stat.osu.edu>
#
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU Affero General Public License as published
#    by the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU Affero General Public License for more details.
#
#    You should have received a copy of the GNU Affero General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.



sqrt_ <- function(X) {
    .Call('_demu_sqrt_', PACKAGE = 'demu', X)
}

subspace_ <- function(V) {
    .Call('_demu_subspace_', PACKAGE = 'demu', V)
}

simDppModal_ <- function(R, n) {
    .Call('_demu_simDppModal_', PACKAGE = 'demu', R, n)
}

