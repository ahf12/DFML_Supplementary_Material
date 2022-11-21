# Pressure Loss in Flow Through Sudden Expansion: Digital Fluid Mechanics Laboratory

## Description

These files are associated with the laboratory described in the (in-submission) publication, *Pressure Loss in Flow Through Sudden Expansion: Digital Fluid Mechanics Laboratory*. Files 1 - 3 are required to complete the laboratory assignment. Files 4 and 5 are provided for the readers edification.

1. **_DFML_LaboratoryInstructions.nb_**: this Mathematica Notebook contains the theory and instructions required to complete this laboratory assignment.
2. **_DFML_SolverModulePackage.wl_**: this Wolfram Language Package contains the three Modules required to solve the Navier Stokes equations and visualize flow through the channel geometry. 
    - **_GeoModule_**: this Module allows users to manipulate and visualize the geometry of the sudden expansion and change the inlet flow velocity to determine the corresponding Reynolds Number. 
    - **_MeshModule_**: this Module provides users the ability to refine the interior and exterior elements of the solver mesh applied to the sudden expansion geometry.
    -	**_FlowModule_**: this Module calculates the average pressure and flow velocity at two points along the geometry midline before and after the sudden expansion. 
3.	**_DFML_MathematicaInstructions.nb_**: this interactive Mathematica Notebook contains the instructions required to utilize the three Modules described above.
4.	**_DFML_DemonstrationResults.nb_**: this Mathematica Notebook provides a brief overview of the results the reader should expect to obtain from the solver module.
5.	**_DFML_Mathematics.nb_**: this Mathematica Notebook examines the mathematics underlying the physical models used in this laboratory in detail.

**_Note_**: The format of Wolfram Language Packages is slightly different than that of Mathematica Notebooks. Use the following [Wolfram resource](https://reference.wolfram.com/language/guide/PackageDevelopment.html) to better understand this formatting.

## Getting Started
* For users intersted in simulating sudden expansion/contraction flow, download files 2 and 3. The solver module packages contains the necessary calculations, and the instruction file describes how to use the solver module package.
* For users interested in assigning this laboratory assignement as a classroom exercise, download all files.

### Dependencies

* To view: [Wolfram Player](https://www.wolfram.com/player/)
* To run/edit: [Wolfram Mathematica](https://www.wolfram.com/mathematica/)

**_Note_**: Notebook files ('.nb') are semi-interactive when opened with the Wolfram Player. 
For example, cells are not evaluatable, but Dynamic Modules allow for user input.

### Installing

Save all files to the same location and set the working directory to this location (as described file 3).

### Executing program

A detailed walk-through is provided in file 3.

## Authors

**August H. Young (Frechette)**,
Duke University Department of Mechanical Engineering and Materials Science, Durham, NC, USA
young.augusth@gmail.com

**Anil Ganti**,
Duke University Department of Electrical and Computer Engineering, Durham, NC, USA

**Zbigniew J. Kabala**,
Duke University Department of Civil and Environmental Engineering, Durham, NC, 

## Version History

Initial Release: November 22, 2022

Anticipated Edits: Wolfram Demonstration file to provide a completely free version of this laboratory exercise.

## License

Creative Commons Attribution 4.0 Generic License (CC BY 4.0)

This work is, in part, supported by a grant, OPP1173370 (Sanitation Technology Cluster), from the Bill & Melinda Gates Foundation through Duke University’s Center for WaSH-AID. All opinions, findings, and conclusions or recommendations expressed in these works are those of the author(s) and do not necessarily reflect the views of the Foundation, Duke, or the Center.
