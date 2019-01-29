/*---------------------------------------------------------------------------*\
  =========                 |
  \\      /  F ield         | OpenFOAM: The Open Source CFD Toolbox
   \\    /   O peration     |
    \\  /    A nd           | Copyright (C) 2011-2015 OpenFOAM Foundation
     \\/     M anipulation  |
-------------------------------------------------------------------------------
License
    This file is part of OpenFOAM.

    OpenFOAM is free software: you can redistribute it and/or modify it
    under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    OpenFOAM is distributed in the hope that it will be useful, but WITHOUT
    ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
    FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
    for more details.

    You should have received a copy of the GNU General Public License
    along with OpenFOAM.  If not, see <http://www.gnu.org/licenses/>.

Application
    testGrid2Grid

Description
    Example program hot to use Grid2Grid (HOS Wrapper) library in OpenFOAM.
    It needs shared library "libGrid2Grid.so".

Author
    YoungMyung Choi (Ecole Centrale de Nantes)

\*---------------------------------------------------------------------------*/

#include "fvCFD.H"

namespace Foam
{
    //- Grid2Grid Initial Character Length
    const int nCharGridGrid(300);

    extern "C" void __mod_cn_stream_MOD_initializerf
    (
        const char[nCharGridGrid]
    );


    //- Get HOS Wave Elevation, Flow Velocity and Dynamic Pressure

    extern "C" void __mod_cn_stream_MOD_getcnstreamflow
    (
        double *,
        double *,
        double *,
        double *,
        double *,
        double *,
        double *,
        double *,
        double *,
        double *,
        double *,
        double *,
        double *,
        double *,
        double *,
        double *,
        double *,
        double *,
        double *,
        double *,
        double *,
        double *,
        double *
    );

}

int main(int argc, char *argv[])
{
	Info << "OpenFOAM Program Example to Call CNSTREAM in OpenFOAM" << endl;

    // Set HOS Solver Type
    const word inputFilePath("./../../input/CN_Stream_input_v2.dict");

    // Set File Name
    string strinputFilePath = string(inputFilePath);

    // Set HOS Solver Type
    const char *ptrInputFilePath = strinputFilePath.c_str();

    // Initialize Grid2Grid
    __mod_cn_stream_MOD_initializerf(ptrInputFilePath);

    // Set Position
    double theta(0.0);
    double x(0.0);
    double y(0.0);
    double z(0.0);
    double eta, u, v, w, pd, detadx,detady,detadt, dudx,dvdx,dwdx,dudy,dvdy,dwdy,dudz,dvdz,dwdz;
    double max_exp(1.0);

    // Set Simulation Time and Time Difference
    double simulTime(0.0);
    double dt(0.1);

    // Time Loop
    for (int it = 0; it < 1; it++)
    {
        // Correct Grid2Grid
        __mod_cn_stream_MOD_getcnstreamflow
        (
		  &theta, &x, &y, &z, &simulTime,
		  &eta, &u, &v, &w, &pd,
		  &detadx,&detady,&detadt,
		  &dudx,&dvdx,&dwdx,&dudy,&dvdy,&dwdy,&dudz,
		  &dvdz,&dwdz, &max_exp
		);


        Info << " simulTime : " << simulTime << endl;
        Info << "   eta     : " << eta << endl;
        Info << "   u, v, w : " << u << " " << v << " " << w << endl;
        Info << "   pd      : " << pd << nl << endl;


        simulTime+=dt;
    }


	return 0;
}
