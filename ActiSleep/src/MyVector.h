/*
 *  MyVector.h
 *  Segments
 *
 *  Created by Michel Koskas on 09/12/11.
 *  Copyright 2011 INRA, INA. All rights reserved.
 *
 */

#ifndef MyVector_h_
#define MyVector_h_

#include <iostream>
#include <stdlib.h>
#include <algorithm>
#include "GeneralFunctions.h"

template<typename T>
class MyVector
{
	T *MyData;
	unsigned int Size;
	unsigned int Capacity;
public:
	//unsigned int sizeof(){return Capacity * sizeof(T);}
	typedef T& reference;
	typedef T* iterator;
	typedef const T* const_iterator;
	bool AscendingOrder();
	void operator=(const MyVector<T> &Other)
	{
		if (MyData == Other.MyData)
			return;
		clear();
		if (Capacity < Other.Size)
		{
			if (MyData != NULL)
				delete[] MyData;
			MyData = new T[Other.Capacity];
			Capacity = Other.Capacity;
		}
        Size = Other.Size;
		//Size = 0;
		for (unsigned int i = 0; i < Other.Size; i++)
			MyData[i] = Other.MyData[i];

	}

	~MyVector();
	void push_back(const T &Element);
	void clear();
	MyVector();
	MyVector(int k);
	MyVector(unsigned int S, const T &Value);
	int size();
	void remove(const T &Element);
	bool empty();
	void reverse();
	void erase(const iterator &First, const iterator &Last);
	iterator begin()
	{
		return MyData;
	}

	iterator end()
	{
		return MyData + Size;
	}

	const_iterator begin() const
	{
		return MyData;
	}

	const_iterator end() const
	{
		return MyData + Size;
	}
	reference operator[](size_t Rank)
	{
		return *(begin() + Rank);
	}
	void sort();
};

MyVector<int> IntersectLists(const MyVector<int> &A, const MyVector<int> &B);
MyVector<int> GetBreakpoints(int k, int n, int** M);
MyVector<double> GetParameters(int k, int n, int** M, double** Par);

template<typename T>
void MyVector<T>::erase(const MyVector::iterator &First, const MyVector::iterator &Last)
{
	unsigned int Gap = Last - First;
	for (typename MyVector<T>::iterator I = First; ((I != Last) && (I != end())); I++)
		*I = *(I + Gap);
	Size -= Gap;
}

template<typename T>
void MyVector<T>::sort()
{
	std::sort(begin(), end());
}


template<typename T>
bool MyVector<T>::empty()
{
	return (Size == 0);
}

template<typename T>
void MyVector<T>::reverse()
{
  for (unsigned int i = 0; i < ((Size - 1) / 2 + 1); i++)
		Swap(MyData[i], MyData[Size - 1 - i]);
}

template<typename T>
void MyVector<T>::remove(const T &Element)
{
	int TheIndex = 0;
	while (((unsigned int) TheIndex < Size) && (MyData[TheIndex] != Element))
		TheIndex++;
	if ((unsigned int) TheIndex == Size)
		return;
	for (unsigned int i = TheIndex + 1; i < Size; i++)
		MyData[i - 1] = MyData[i];
	Size--;
}

template<typename T>
int MyVector<T>::size()
{
	return Size;
}

template<typename T>
bool MyVector<T>::AscendingOrder()
{
  for (int i = 0; i < Size - 1; i++)
    if (MyData[i] > MyData[i + 1])
      return false;
  return true;
}

template<typename T>
void MyVector<T>::clear()
{
	Size = 0;

}

template<typename T>
MyVector<T>::~MyVector()
{
	if (MyData != NULL)
		delete[] MyData;
}

template<typename T>
MyVector<T>::MyVector()
{
	MyData = NULL;
	Size = 0;
	Capacity = 0;
}

template<typename T>
MyVector<T>::MyVector(int k)
{
	MyData = new T[k];
	Size = 0;
	Capacity = k;
}

template<typename T>
MyVector<T>::MyVector(unsigned int S, const T &Value)
{
	MyData = new T[S];
	Capacity = 2*S;
	Size = S;
	for (unsigned int i = 0; i < Size; i++)
		MyData[i] = Value;
}

template<typename T>
void MyVector<T>::push_back(const T &NewData)
{
	if (Size == Capacity)
	{
		if (Capacity == 0)
			Capacity = 2;
		T *NewArray = new T[2 * Capacity];
		for (unsigned int i = 0; i < Size; i++)
			NewArray[i] = MyData[i];
		Capacity *= 2;
		if (MyData != NULL)
			delete[] MyData;
		MyData = NewArray;
	}
	MyData[Size] = NewData;
	Size++;
}







#endif

