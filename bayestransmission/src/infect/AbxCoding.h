// infect/AbxCoding.h
#ifndef ALUN_INFECT_ABXCODING_H
#define ALUN_INFECT_ABXCODING_H

#include "../util/util.h"

class AbxCoding
{
public:
    static Map *sysabx;
    static Map *syseverabx;

    enum AbxStatus
    {
        nullstatus = 0,
        offabx = 1,
        onabx = 2
    };

    static string abxCodeString(AbxStatus x);
};

#endif // ALUN_INFECT_ABXCODING_H
