
#include "modeling/modeling.h"
using namespace models;


#include "wrap.h"

#include <Rcpp.h>

#include "RRandom.h"

// Expose the classes to R
RCPP_EXPOSED_AS(util::SortedList)

void init_Module_util(){
class_<util::Object>("CppObject")
    .property("hash", &util::Object::hash)
    .property("className", &util::Object::className)
    ;
class_<util::IntMap>("CppIntMap")
    .derives<util::Object>("CppObject")
    .property("size", &util::IntMap::size)
    .property("hasNext", &util::IntMap::hasNext)
    .method("init", &util::IntMap::init)
    .method("step", &util::IntMap::next)
    .method("nextValue", &util::IntMap::nextValue)
    ;
class_<util::List>("CppList")
    .derives<util::Object>("CppObject")
    .property("size", &util::List::size)
    .property("hasNext", &util::List::hasNext)
    .method("init", &util::List::init)
    .method("step", &util::List::next)
    ;
class_<util::SortedList>("CppSortedList")
    .derives<util::List>("CppList")
    ;
class_<util::Map>("CppMap")
    .derives<util::Object>("CppObject")
    .property("size", &util::Map::size)
    .property("hasNext", &util::Map::hasNext)
    .method("init", &util::Map::init)
    .method("step", &util::Map::next)
    .method("nextValue", static_cast<util::Object* (util::Map::*)()>(&util::Map::nextValue))
    .method("FirstKey", &util::Map::getFirstKey)
    .method("FirstValue", &util::Map::getFirstValue)
    .method("LastKey", &util::Map::getLastKey)
    .method("LastValue", &util::Map::getLastValue)
    ;



}
