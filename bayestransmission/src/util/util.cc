
#include "util/util.h"

// /* Default Object writer */

std::ostream& operator<<(std::ostream &os, util::Object *x)
{
        if (x == 0)
        {
                os << "null";
        }
        else
        {
                x->write(os);
        }

        return os;
}

/* Class constants */

unsigned long util::Object::indexcounter = 0;
