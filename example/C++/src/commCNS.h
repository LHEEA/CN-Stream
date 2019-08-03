#ifndef DEF_COMMCNS_H
#define DEF_COMMCNS_H

#include <string>

struct Output_type{
    double eta;
    double pressure;
    double Vx, Vy, Vz;

    double detadt, detadx,detady;
    double dVxdx, dVxdy, dVxdz;
    double dVydx, dVydy, dVydz;
    double dVzdx, dVzdy, dVzdz;
};

struct RF_type{
    int dimen;          // dimension
    int current;
    int lorT;           // input wavelength (=1) or period (=0)

    double g;             // gravity acceleration
    double hdepth;        // depth
    double H;             // wave height
    double k;             // wave number
    double lambda;        // wavelength
    double T;             // period
    double U;             //
    double C;

    double R;
    double Q;
    double C_E, C_S;
};

struct Option_type{
    int n_H;
    int err_type;
    double eps_err;
    double err_max;
    double eps_inc;
    double eps_N1;
    int itermax;
    int increment_type;  // 0 linear ; 1 exponentiel
    int N1;              // number of modes for stream function
    int N2;
    int N1_eff;
    int N2_eff;
    int modes;
    int printonscreen;   // 1 yes ; 0 no
    int writeoutput;   // 1 yes ; 0 no
};

void calcRF(const std::string& ConfigFile, RF_type &RF, Option_type &option);

void recRF(RF_type &RF, Option_type &option, const double &x, const double &y,const double &z,const double &t,const double &thetaincident,const bool &hydrostatic, Output_type &output);

void initAiry(const std::string& ConfigFile, RF_type &RF, Option_type &option);

void airy(RF_type &RF, Option_type &option, const double &x, const double &y,const double &z,const double &t,const double &thetaincident,const bool &hydrostatic, Output_type &output);


extern "C" void __modcns_MOD_calcrf(const char ConfigFile[], const RF_type* RF,const Option_type* option);

extern "C" void __modcns_MOD_recrf(const RF_type* RF, const Option_type* option,const double* x,const double* y,const double* z,const double* t, const double* thetaincident,const bool *hydrostatic,const Output_type* output);

extern "C" void __modcns_MOD_initairy(const char ConfigFile[], const RF_type* RF,const Option_type* option);

extern "C" void __modcns_MOD_airy(const RF_type* RF, const Option_type* option,const double* x,const double* y,const double* z,const double* t, const double* thetaincident,const bool *hydrostatic,const Output_type* output);

#endif