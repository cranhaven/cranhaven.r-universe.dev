//
// Created by andrew on 3/14/2024.
//

#include "utils.h"

std::unordered_map<std::string, algotype> algomap{
    {"MU", MU},
    {"HALS", HALS},
    {"ANLSBPP", ANLSBPP},
    {"NAIVEANLSBPP", NAIVEANLSBPP},
    {"AOADMM", AOADMM},
    {"NESTEROV", NESTEROV},
    {"CPALS", CPALS},
    {"GNSYM", GNSYM},
    {"R2", R2},
    {"PGD", PGD},
    {"PGNCG", PGNCG},
    {"mu", MU},
    {"hals", HALS},
    {"anlsbpp", ANLSBPP},
    {"naiveanlsbpp", NAIVEANLSBPP},
    {"aoadmm", AOADMM},
    {"admm", AOADMM},
    {"nesterov", NESTEROV},
    {"cpals", CPALS},
    {"gnsym", GNSYM},
    {"r2", R2},
    {"pgd", PGD},
    {"pgncg", PGNCG}
};

std::unordered_map<std::string, algotype> symmap{
    {"ANLSBPP", ANLSBPP},
    {"GNSYM", GNSYM},
    {"anlsbpp", ANLSBPP},
    {"gnsym", GNSYM}
};
std::unordered_map<std::string, normtype> normmap{{"NONE", NONE}, {"L2NORM", L2NORM}, {"MAXNORM", MAXNORM}};
