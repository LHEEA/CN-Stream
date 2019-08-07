#ifndef DEF_COMMCNS_H
#define DEF_COMMCNS_H

#include <string>
#include <iostream>

struct Output_type{
    double eta;
    double pressure;
    double Vx, Vy, Vz;

    double detadt, detadx, detady;
    double dVxdx, dVxdy, dVxdz;
    double dVydx, dVydy, dVydz;
    double dVzdx, dVzdy, dVzdz;
};

inline std::ostream& operator<<(std::ostream& out, const Output_type& s) {
  out << "eta                     = " << s.eta << std::endl
      << "pressure                = " << s.pressure << std::endl
      << "Vx, Vy, Vz              = " << s.Vx << ", " << s.Vy << ", "<< s.Vz << std::endl
      << "detadt, detadx, detady  = " << s.detadt << ", " << s.detadx << ", "<< s.detady << std::endl
      << "dVxdx, dVxdy, dVxdz     = " << s.dVxdx << ", " << s.dVxdy << ", "<< s.dVxdz << std::endl
      << "dVydx, dVydy, dVydz     = " << s.dVydx << ", " << s.dVydy << ", "<< s.dVydz << std::endl
      << "dVzdx, dVzdy, dVzdz     = " << s.dVzdx << ", " << s.dVzdy << ", "<< s.dVzdz << std::endl;
  return out;
}

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

#endif