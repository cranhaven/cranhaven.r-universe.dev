#pragma once
/* Copyright Ramakrishnan Kannan 2017 */



// usage scenarios
// NMFLibrary algotype lowrank AfileName numIteration
// NMFLibrary algotype lowrank m n numIteration
// NMFLibrary algotype lowrank Afile WInitfile HInitfile numIteration
// NMFLibrary algotype lowrank Afile WInitfile HInitfile WoutputFile HoutputFile
// numIteration #define WINITFLAG 1000 #define HINITFLAG 1001 #define REGWFLAG
// 1002 #define REGHFLAG  1003

// distnmf related defines
// #define PROCROWS        2002
// #define PROCCOLS        2003
constexpr auto NUMKBLOCKS = 2004;
constexpr auto NORMALIZATION = 2005;
constexpr auto DIMTREE = 2006;
constexpr auto SYMMETRICREG = 2007;
constexpr auto ADJRAND = 2008;
constexpr auto NUMLUCITERS = 2009;
constexpr auto INITSEED = 2010;
constexpr auto ALPHAREG=2011;
constexpr auto BETAREG=2012;
constexpr auto MAT_TYPE=2013;
constexpr auto MOMENTUM=2014;
constexpr auto UNPARTITIONED=2015;
