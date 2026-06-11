/*
 *  GeneralFunctions.h
 *  Segments
 *
 *  Created by Michel Koskas on 22/08/11.
 *  Copyright 2011 INRA, INA. All rights reserved.
 *
 */
#ifndef _GeneralFunctions_h_
#define _GeneralFunctions_h_

#include <iostream>
#include <stdio.h>
#include <stdlib.h>
#include "Constants.h"


bool IsDigit(char &x);
bool ToNext(char *Buffer, int &BuffIndex, int BufferSize, char Separator=Separateur, char Terminator=FinDeLigne);
int GetRandomNumber(int MinValue, int MaxValue);


template <typename T>
void Swap(T &A, T &B)
{
	T C = A;
	A = B;
	B = C;
}

#endif
