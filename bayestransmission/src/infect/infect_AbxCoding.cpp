#include "infect/infect.h"

Map *infect::AbxCoding::sysabx = new Map();
Map *infect::AbxCoding::syseverabx = new Map();
string infect::AbxCoding::abxCodeString(AbxStatus x)
{
    switch (x)
    {
    case nullstatus:
        return "null abx status";
    case offabx:
        return "offabx";
    case onabx:
        return "onabx";
    default:
        return "CAN'T REACH THIS";
    }
}
