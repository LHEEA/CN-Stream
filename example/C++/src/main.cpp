#include <iostream>
#include <string>
#include "commCNS.h"

using namespace std;

int main()
{
     string waveConfigFile;

     struct RF_type RF;
     struct Option_type option;
     struct Output_type output;

    waveConfigFile = "RF_input.dict";

    calcRF(waveConfigFile,RF,option);
    recRF(RF, option, 0., 0., 0., 0., 0., false, output);

    return 0;
}
