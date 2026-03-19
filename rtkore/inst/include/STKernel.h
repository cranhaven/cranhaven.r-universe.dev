/*--------------------------------------------------------------------*/
/*     Copyright (C) 2004-2016  Serge Iovleff, Université Lille 1, Inria

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as
    published by the Free Software Foundation; either version 2 of the
    License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this program; if not, write to the
    Free Software Foundation, Inc.,
    59 Temple Place,
    Suite 330,
    Boston, MA 02111-1307
    USA

    Contact : S..._Dot_I..._At_stkpp_Dot_org (see copyright for ...)
*/

/*
 * Project:  stkpp::STKernel
 * Purpose:  Primary include file for the STKernel project.
 * Author:   Serge Iovleff, S..._Dot_I..._At_stkpp_Dot_org (see copyright for ...)
 **/

/** @file STKernel.h
 *  @brief This file include all the header files of the project STKernel.
 **/

/** @defgroup STKernel Kernel tools
 *  @brief The STKernel project is the low-level core library that forms the
 *  basis of the project. It provides data class handling for C++.
 *  It contains the sub-projects:
 *  - @ref Base
 *  - @ref Functors
 **/

/** @ingroup STKernel
 *  @defgroup Base Fundamental data types
 *  @brief In this subproject we define the fundamental types.
 *  The Base subproject is composed of
 * <ul>
 * <li> the standard types for handling data:
 * <ul>
 *   <li> String  for string data  </li>
 *   <li> Integer for discrete data </li>
 *   <li> Real    for quantitative data  </li>
 *   <li> Binary  for binary data in {0,1}   </li>
 *   <li> Sign    for signed data in {-1, 1}  </li>
 *   <li> Range   for contiguous range of Index in Arrays  </li>
 * </ul>
 * For all these types, a not available special value is defined which can be
 * displayed in a transparent way using proxy classes @sa Proxy, @sa Proxy.
 * <li> standard input and output streams for all these types </li>
 * <li> a standard Runtime-type identification mechanism (RTTI) @sa IdType,
 * @sa IdTypeImpl </li>
 * <li> and miscellaneous utilities functions </li>
 * </ul>
 **/

/** @ingroup STKernel
 *  @defgroup Functors Functors
 *  In the Functors subproject, we implement the main functors that can be used
 *  throughout the STK++ project, especially in the Arrays project.
 **/

#ifndef STKERNEL_H
#define STKERNEL_H

/* Fundamental types */
#include <STKernel/include/STK_TypeBase.h>
/* Functors */
#include <STKernel/include/STK_Functors.h>
/* Miscellaneous functions. */
#include <STKernel/include/STK_Misc.h>
/* Chrono functions.  */
#include <STKernel/include/STK_Chrono.h>

#endif  /* STKERNEL_H */
