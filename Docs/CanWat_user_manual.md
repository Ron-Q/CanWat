# Introduction

Interception of precipitation has a significant influence on the availability of water in ecosystems. In forest ecosystems evaporates up to 50 % of precipitation via interception and is not available to plants. The high spatiotemporal variability of the process is not yet described by models satisfactorily, which causes considerable uncertainty in water balances. Here, the interception model CanWat is introduced, which is able to take the three-dimensional spatial heterogeneity of vegetation stands and the resulting small scale variability of the process into account. Rainfall interception, evaporation, transpiration and drainage can be simulated on a voxel space (voxel = volume-pixel) within a grid scale of cubic meters. CanWat was developed as a flexible research tool for the investigation of the interception process on different spatial scales.

Features of CanWat are:

- developed in R for an easy interactive work with the source code
- modular setup: exchange of single approaches (i.e. drainage or evaporation) is easy possible
- regard of 3D vegetation structure with a grid size down to less than 1 m³
- diagnostic model, driven by measurements of precipitation, wind, temperature, humidity, radiation
- scalability (drainage parameter do not depend on resolution)
- regard of the re-interception of drained water
- variable time step of 1 s - 3600 s
- work on long time series is possible (dekades)
- applicable to "large" areas/catchments (e.g. Wernersbach 4.6 km²)

The time step of the simulation is variable and automatically calculated according to the actual water fluxes within the domain. The water balance equation is solved numerically applying a Runge-Kutta approach (4th order) within each voxel.

High spatio-temporal resolution allows the assignment of the simulation results to individual throughfall collectors, as well as the comparison with micro-meteorological measurements of evapotranspiration from a larger footprint area. The required detailed vegetation model can be derived from terrestrial laser scans (Bienert et al. 2010, Queck et al. 2012).

The interception process consists of two interwoven parts, the water balance and the energy balance, they are connected by evaporation. The latter is driven by the energy input from sensible heat and from radiation, which is mostly low during rain events. Therefore evaporation depends mainly on the sensible heat flux which is a function of aerodynamic conductance, i.e. wind speed, temperature and water vapour pressure. The interception of falling water as well as the meteorological conditions for the evaporation of that water depend on the spatial distribution of vegetation. I.e. in forests all of these quantities are spatially highly variable.

In the current stage CanWat has elements of a revised Rutter Model. However, it simulates the water balance within the canopy using a cascade of storages. The depletion of the storages is simulated by an exponential drainage approach and by the evaporation calculated by a form of the Penman-Monteith (PM) equation. The latter combines the flux-gradiant equation and the energy balance to eliminate the surface temperature, which is mostly unknown. The variability of the necessary meteorological quantities within the canopy is modeled by a special module for each quantity. Based on measurements the wind distribution is modeled using a mixing length approach (and - in development - a diagnostic wind field model which assures the continuity of mass). It includes a radiation transfer module that provides the distribution of radiative energy as well as of shortwave radiation. The latter is used for the calculation or the canopy conductance and so the transpiration, which also consumes a small part of the available energy during and after rain events. The variability of temperature and water vapor pressure is estimated by interpolation of measurements currently. A module that includes the energy storage of the vegetation material is planed.

# Overview and general use

In the current stage, CanWat is used in interpreter mode, i. e. there is no compiled version. Users prepare there input files with any appropriate editor and start the program CanWat from any R environment (RStudio is recommended).

# Input

Several inputs are necessary to individualize the CanWat run. The runs are controlled by main routine CanWat as well as <case>\_input and <case>\_output files in the Cases folder. The domain (i.e. the boundary conditions) is defined by a Static Driver and the meteorological conditions should be provided by a Dynamic Driver. Furthermore, one can adjust the parameter settings in CW_parameter.r

## CanWat_Start.r

Change only entries within the "INPUT" section.

The variable "act" controls the selection of the input and output files in the folder "path_Cases" (e.g.:   act <- "ASTW0").

Further you may need to adapt the paths to your system.

## <Case>\_input.r

This is the main input file, which describes the individual Dynamic and Static Drivers, and sets switches for the output. Please see the examples that are given within the *Cases* Folder.

## <Case>\_TS_output.r

<Case>\_TS_output.r is called at the end of every time step. The user can here define the output of selected ranges, sums, plots etc. individually.

## Static Driver

The Static Driver contains the surface distribution within the model domain. Currently only vegetation is regarded. The plant area density PAD should be given in m²/m³, i.e. even if the voxel size is bigger than 1 m³, you should give the average PAD for each voxel.

It has to be stored as a one dimensional vector with highest frequency in z (height above ground), then y and x (thus, z is the most inner loop and x is the outer loop). Anyway, each Static Driver file should be accompanied by a (<name of the Static Driver>\_description.txt file, where the origin and processing of the data is described. The Static Driver is loaded by *Load_Static_Driver.r* .

## Dynamic Driver

Within these files the meteorological input is provided. It should contain

- wind speed in m/s
- temperature in °C
- water vapour pressure in Pa
- net radiation
- shortwave radiation downwards

Each of these variables can be provided at different height levels. The measurement height must be given in the input file. Only one vertical profile is considered up to now. According to the requirements of the user the horizontal variability of the meteorological variables may be included. The Dynamic Driver is loaded by *Load_Dynamic_Driver.r* .

## Parameters

A change of parameters for interception, drainage and evaporation could be done in the CW_parameter.r

# CanWat run

# Output

## Standard Output

As a basis CanWat calculates cumulative sums of the water balance and saves them in two tables within the "\~/Output" folder. The file "<case>\_WB\_<timestamp>.csv" contains values for the same time steps as the input and the file "case>\_WBa\_<timestamp>.csv" all calculated time steps.

The values are mean fluxes in mm per timestep (ts), except C_3D which is the mean storage content

| Variable     |     | Unit    | Description                        |
|--------------|-----|---------|------------------------------------|
| *UTC*        | in  | s       | start time of the time interval    |
| *dt*         | in  | s       | length of the time interval        |
| *C_3D*       | in  | mm      | current canopy storage per voxel   |
| *dC*         | in  | mm/*dt* | change of the storage              |
| *PF*         | in  | mm/*dt* | gross precipitation                |
| *Pintercept* | in  | mm/*dt* | intercepted precipitation          |
| *Pthroughf*  | in  | mm/*dt* | throughfall from rain (direct)     |
| *ET*         | in  | mm/*dt* | transpiration                      |
| *EV*         | in  | mm/*dt* | evaporation                        |
| *Drainage*   | in  | mm/*dt* | drainage (water in the canopy air) |
| *Dintercept* | in  | mm/*dt* | reintercepted water from drainage  |
| *Dthroughf*  | in  | mm/*dt* | throughfall from drainage          |

Please note, that the Drainage describes the water which is in the air at the end of the time step. At the beginning of the next time step Drainage is distributed and lands either on the plant surface again (Dintercept) or falls through the bottom of the domain (Dthroughf). Thus at a certain time step i

Drainage(i-1) = Dintersept(i) + Dtroughf(i)

A rough overview of the input and output is given by the Figure in file "<case><timestamp>.png"

## Individual Output

Each input file <Case>_input.r should contain a section called "OUTPUT DEFINITION". Within this section several output switches can be set. This settings control the work of "output_timestep.R".

Additionally, you can create an individual script which you name using the variable *"fnam.out"*, this script must be located in the "*Cases*" folder. An examples are given by "Output_yz-cross-section.R" and "ASTW0_output.R".

Note that CanWat adds a bottom layer to the domain

# Example Sessions

- provide Static Driver (e.g.: Tree01_3D_10L.csv)
- provide Dynamic Driver (e.g.: Case01_DynD.csv)
- provide case description (e.g.: Case01_Tree01_3D_10L_input.R)
- provide output description (e.g.: Output_yz-cross-section.R)
- edit and run CanWat_Start.R

# Subroutines of CanWat

## allocation.r

Purpose: Declaration of variables, i.e. allocates the storage for the arrays 
Note: the first index stays nearest, => should be the fastest / the core loop
Input:  Dimensions
Output:

## CW_parameter.R

PARAMETERS & CONSTANTS for the use in CanWat

# References

Bienert A, Queck R, Schmidt A, Bernhofer C, Maas H-G (2010) Voxel space analysis of terrestrial laser scans in forests for wind field modelling. International Archives of Photogrammetry, Remote Sensing and Spatial Information Sciences, XXXVIII, Part 5:S,92--97

Queck R, Bienert A, Maas H-G, Harmansa S, Goldberg V, Bernhofer C (2012) Wind fields in heterogeneous conifer canopies: parameterisation of momentum absorption using high-resolution 3D vegetation scans. European Journal of Forest Research, 131:S,165--176
