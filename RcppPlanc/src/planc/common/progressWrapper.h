//
// Created by andrew on 11/28/2023.
//

#ifndef PLANC_PROGRESSWRAPPER_H
#define PLANC_PROGRESSWRAPPER_H

#include <cstdint>
#include <indicators/block_progress_bar.hpp>

class Progress {
private:
    std::unique_ptr<indicators::BlockProgressBar> wrappedBar;
public:
    explicit Progress(unsigned long max, bool display_progress = true);
    void increment() const;
};


#endif //PLANC_PROGRESSWRAPPER_H
