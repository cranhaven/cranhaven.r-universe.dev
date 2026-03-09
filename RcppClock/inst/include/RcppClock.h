#include <Rcpp.h>
#include <chrono>

#define duration(a) std::chrono::duration_cast<std::chrono::nanoseconds>(a).count()
#define now() std::chrono::high_resolution_clock::now()

namespace Rcpp {
  class Clock {
  private:
    std::vector<std::chrono::high_resolution_clock::time_point> tick_times;
    std::vector<std::chrono::high_resolution_clock::time_point> tock_times;
    std::vector<std::string> tick_names;
    std::vector<std::string> tock_names;
    std::vector<double> timers;
    std::vector<std::string> tickers;

  public:
    // start a timer
    void tick(std::string name) {
      tick_names.push_back(name);
      tick_times.push_back(now());
    }

    // stop a timer
    void tock(std::string name) {
      tock_names.push_back(name);
      tock_times.push_back(now());
    }

    // calculate timer durations
    void stop(std::string var_name) {
      for (unsigned int i = 0; i < tick_names.size(); ++i) {
        for (unsigned int j = 0; j < tock_names.size(); ++j) {
          if (tick_names[i] == tock_names[j]) {
            timers.push_back(duration(tock_times[j] - tick_times[i]));
            tickers.push_back(tick_names[i]);
            tock_times.erase(tock_times.begin() + j);
            tock_names.erase(tock_names.begin() + j);
            break;
          }
        }
      }
      DataFrame df = DataFrame::create(Named("ticker") = tickers, Named("timer") = timers);
      df.attr("class") = "RcppClock";
      Environment env = Environment::global_env();
      env[var_name] = df;
    }
  };
}
