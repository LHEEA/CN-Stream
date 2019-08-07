#include <string>
#include "commCNS.h"

int main()
{
    const std::string waveConfigFile("RF_input.dict");

    struct RF_type RF;
    struct Option_type option;
    struct Output_type output;

    const double x = 0.0;
    const double y = 0.0;
    const double z = 0.0;
    const double t = 0.0;
    const double thetaincident = 0.0;
    const bool hydrostatic = false;

    calcRF(waveConfigFile, RF, option);
    recRF(RF, option, x, y, z, t, thetaincident, hydrostatic, output);

    return 0;
}
