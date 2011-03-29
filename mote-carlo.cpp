#include <stdio.h>
#include <stdlib.h>
#include <functional>
#include <random>
#include <sys/time.h>

namespace {
const unsigned long MAX_RANGE = 100000;
std::mt19937 eng;
std::uniform_int<> dist(0, MAX_RANGE);
std::variate_generator<std::mt19937&, std::uniform_int<> > roll(eng, dist);
}

using std::placeholders::_1;
using std::placeholders::_2;

double random_in_range(long low, long high)
{

    double range(high - low);
    double rand = double(roll());
    return  double(low) + range * rand / double(MAX_RANGE);
}

double mote_carlo(size_t trials, std::function<bool()> f)
{
    size_t trials_passed = 0;
    for (size_t i = trials; i > 0; --i)
    {
        if (f()) {
            ++trials_passed;
        }
    }
    return double(trials_passed) / double(trials);
}

template<typename F>
bool experiment(F pred, long x1, long x2, long y1, long y2)
{
    return pred(random_in_range(x1, x2), random_in_range(y1, y2));
}

template<typename F>
std::function<bool()> make_experiment(F f, long x1, long x2, long y1, long y2)
{
    return std::bind(experiment<F>, f, x1, x2, y1, y2);
}

template<typename F, typename P>
double make_result(F f, size_t trials, P pred)
{
    return f(trials, pred);
}

std::function<double(int)> estimate_integral(std::function<bool(double, double)> pred,
                                             long x1, long x2, long y1, long y2)
{
    auto mote_expriment = make_experiment(pred, x1, x2, y1, y2);
    typedef decltype(mote_expriment) P;
    typedef decltype(mote_carlo) F;
    return std::bind(make_result<F, P>, mote_carlo, _1, mote_expriment);
}

inline double square(double x)
{
    return x*x;
}

bool estimate_integral_pred(double x, double y, double o_x, double o_y, double r)

{
    return square(x-o_x) + square(y-o_y) <= square(r);
}

double mote_carlo_pi (size_t trials, size_t radius)
{
    double o_x = 0;
    double o_y = 0;
    double r = radius;
    double x1 = o_x - r;
    double x2 = o_x + r;
    double y1 = o_y - r;
    double y2 = o_y + r;
    auto f = std::bind(estimate_integral_pred, _1, _2, o_x, o_y, r);
    return (x2-x1) * (y2-y1) * estimate_integral(f, x1, x2, y1, y2)(trials) / square(r);
}

int main(int argc, char **argv)
{
    if (argc != 3) {
        printf("Usage: <trials> <radius>\n");
        exit(1);
    }
    struct timeval tmp;
    gettimeofday(&tmp, 0);
    eng.seed(tmp.tv_sec);
    
    size_t r = atoi(argv[1]);
    size_t trials = atoi(argv[2]);
    double pi = mote_carlo_pi(trials, r);
    printf("%.8f\n", pi);
    exit(0);
}
