/*--------------------------------------------------------------------*/
/*     Copyright (C) 2004-2016 Serge Iovleff

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

    Contact : S..._DOT_I..._AT_stkpp.org (see copyright for ...)
*/

/*
 * Project:  stkpp::Clustering
 * created on: 15 nov. 2013
 * Author:   iovleff, S..._Dot_I..._At_stkpp_Dot_org (see copyright for ...)
 **/

/** @file STK_DataBridge.h
 *  @brief In this file we define the data wrapper class
 **/

#ifndef STK_DATABRIDGE_H
#define STK_DATABRIDGE_H

#include <Arrays.h>
#include "STK_IDataBridge.h"

namespace STK
{

/** @ingroup Clustering
 *  @brief bridge a data set in order to handle its missing values.
 *
 * @tparam Data The data bridged by the DataBridge class
 */
template<class Data>
class DataBridge: public IDataBridge
{
  public:
    /** data set */
    Data dataij_;

    /** default constructor. */
    DataBridge( std::string const& idData): IDataBridge(idData), dataij_() {}
    /** constructor with data. */
    DataBridge( std::string const& idData, Data const& dataij): IDataBridge(idData), dataij_(dataij) {}
    /** copy constructor (Warning: will copy the data set)
     *  @param bridge the DataBridge to copy
     **/
    DataBridge( DataBridge const& bridge): IDataBridge(bridge), dataij_(bridge.dataij_) {}
    /** destructor */
    virtual ~DataBridge() {}

    /** @return data set */
    inline Data const& dataij() const { return dataij_;}
    /** @return data set */
    inline Data& dataij() { return dataij_;}
};

} // namespace STK

#endif /* STK_DATABRIDGE_H */
