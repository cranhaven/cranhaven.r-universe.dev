# This file is part of econetwork

# econetwork is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# econetwork is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with econetwork.  If not, see <http://www.gnu.org/licenses/>

getMetaweb <- function(gList){#get the metaweb of a list of graph (as the union on a set of graph)
  gMetaweb <- gList[[1]]
  for(i in 2:length(gList)){
    gMetaweb <- gMetaweb %u% gList[[i]]
  }
  return(gMetaweb)
}
