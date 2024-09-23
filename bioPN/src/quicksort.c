// -*- mode: C; c-indent-level: 2; c-basic-offset: 2; tab-width: 8 -*-
//
// Copyright (C) 2009-2014 Roberto Bertolusso and Marek Kimmel
//
// This file is part of bioPN.
//
// bioPN is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// bioPN is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with bioPN. If not, see <http://www.gnu.org/licenses/>.

// Based on quick sort algorithm found on Wikibooks:
// http://en.wikibooks.org/wiki/Algorithm_implementation/Sorting/Quicksort#C
// No licence specified. Considered public domain.
// Modifications performed on this file by Roberto Bertolusso released as 
// above copyright.


int partition(int* piIndex, int* piValue, int left, int right);
int findMedianOfMedians(int* piIndex, int* piValue, int left, int right);
int findMedianIndex(int* piIndex, int* piValue, int left, int right, int shift);
void swap(int* a, int* b);

//Quicksort the array
void quicksort(int* piIndex, int* piValue, int left, int right)
{
  if(left >= right)
    return;
  
  int index = partition(piIndex, piValue, left, right);
  quicksort(piIndex, piValue, left, index - 1);
  quicksort(piIndex, piValue, index + 1, right);
}

//Partition the piIndex into two halves and return the
//index about which the piIndex is partitioned
int partition(int* piIndex, int* piValue, int left, int right)
{
  //Makes the leftmost element a good pivot,
  //specifically the median of medians
  findMedianOfMedians(piIndex, piValue, left, right);
  int pivotIndex = left, pivotValue = piValue[piIndex[pivotIndex]], index = left, i;
  
  swap(&piIndex[pivotIndex], &piIndex[right]);
  for(i = left; i < right; i++)
    {
      if(piValue[piIndex[i]] > pivotValue)
        {
	  swap(&piIndex[i], &piIndex[index]);
	  index += 1;
        }
    }
  swap(&piIndex[right], &piIndex[index]);
  
  return index;
}

//Computes the median of each group of 5 elements and stores
//it as the first element of the group. Recursively does this
//till there is only one group and hence only one Median
int findMedianOfMedians(int* piIndex, int* piValue, int left, int right)
{
  if(left == right)
    return piValue[piIndex[left]];
  
  int i, shift = 1;
  while(shift <= (right - left))
    {
      for(i = left; i <= right; i+=shift*5)
        {
	  int endIndex = (i + shift*5 - 1 < right) ? i + shift*5 - 1 : right;
	  int medianIndex = findMedianIndex(piIndex, piValue, i, endIndex, shift);
	  
            swap(&piIndex[i], &piIndex[medianIndex]);
        }
      shift *= 5;
    }
  
  return piValue[piIndex[left]];
}

//Find the index of the Median of the elements
//of piIndex that occur at every "shift" positions.
int findMedianIndex(int* piIndex, int* piValue, int left, int right, int shift)
{
  int i, groups = (right - left)/shift + 1, k = left + groups/2*shift;
  for(i = left; i <= k; i+= shift)
    {
      int minIndex = i, minValue = piValue[piIndex[minIndex]], j;
      for(j = i; j <= right; j+=shift)
	if(piValue[piIndex[j]] > minValue)
	  {
	    minIndex = j;
	    minValue = piValue[piIndex[minIndex]];
	  }
      swap(&piIndex[i], &piIndex[minIndex]);
    }
  
  return k;
}

//Swap two integers
void swap(int* a, int* b)
{
  int temp;
  temp = *a;
  *a = *b;
  *b = temp;
}
