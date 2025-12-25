#include <vector>
#include <list>
#include <set>
#include <cinttypes>
#include <iostream>
#include <algorithm>

extern "C"
int gpav_cpp (
    double  * const W,
    double  * const adj,
    int     * const n,
    double  * Y,
    int     * corr) {

    const uint32_t N = *n;

    uint32_t idx = 0;
    std::vector<uint32_t>   items;
    std::vector<uint32_t>   jMinus;
    std::vector<uint32_t>   lookingIn;
    std::set<uint32_t>      jMinusSet;  

    std::vector<std::list<uint32_t>> blocks(N);
    std::for_each( blocks.begin(), blocks.end(), [idx](std::list<uint32_t> &blk) mutable { blk.push_back(idx++); } );
    idx = 0;

    for (uint32_t i = 0; i < N; i++) {

        items.clear();
        items.push_back(1);

        while (!items.empty()) {
            jMinus.clear();
            jMinusSet.clear();

            for (auto it = blocks[i].begin(); it !=  blocks[i].end(); ++it) {
                for (uint32_t k = 0; k < N; k++) {
                    if (adj[k + (*it)*N] == 1)
                        jMinus.push_back( k );
                }
            }

            std::for_each( jMinus.begin(), jMinus.end(), [corr, &jMinusSet]( uint32_t nn ) { jMinusSet.emplace( corr[nn] - 1 ); } );
            
            items.clear();
            idx = 0;
            for (auto it = jMinusSet.begin(); it != jMinusSet.end(); ++it){
                if (Y[(*it)] > Y[i])
                    items.push_back( idx );
                idx++;
            }

            if (!items.empty()) {
                std::vector<uint32_t> iXmax {};
                double xMax = 0;
                lookingIn.clear();
                uint32_t ii = 0;
                idx = 0;

                for (auto it = jMinusSet.begin(); it != jMinusSet.end(); ++it) {
                    if(idx == items[ii]) {
                        lookingIn.push_back( *it );
                        ii++;
                        if (ii >= items.size())
                            break;
                    }
                    idx++;
                }

                // Looking for maximum value
                for (uint32_t iii = 0; iii <  lookingIn.size(); iii++) {
                    if (Y[lookingIn[iii]] >= xMax) {
                        xMax = Y[lookingIn[iii]];
                    }
                }

                // Looking for maximum indices
                for (uint32_t iii = 0; iii < lookingIn.size(); iii++) {
                    if (Y[lookingIn[iii]] == xMax)
                        iXmax.push_back(lookingIn[iii]);
                }

                uint32_t ndStr = iXmax[0];

                Y[i] = (W[ndStr]*Y[ndStr] + W[i]*Y[i])/(W[i] + W[ndStr]);
                W[i] = W[i] + W[ndStr];

                for (auto it = blocks[ndStr].rbegin(); it != blocks[ndStr].rend(); ++it)
                    blocks[i].push_front( *it );
                blocks[ndStr].clear();

                for (auto it = blocks[i].begin(); it != blocks[i].end(); ++it)
                    corr[(*it)] = i + 1;
                Y[ndStr] = 0;
            }
        }
    }
    return 0;
}
