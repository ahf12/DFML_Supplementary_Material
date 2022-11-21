(* ::Package:: *)

(*written: November 15 2021*)
(*updated: November 18 2022*)
(*author: August Frechette*)

BeginPackage["VLSolver`"]

GeoModule::usage =
"\!\(\*
StyleBox[\"GeoModule\",\nFontWeight->\"Bold\"]\)[L, W1, W2, U, X1, X2] calculates the Reynolds number of the flow at the channel inlet and plots the flow domain given L, W1, and W2";
MeshModule::usage =
"\!\(\*
StyleBox[\"MeshModule\",\nFontWeight->\"Bold\"]\)[L, W1, W2, MCM, MBCM] generates the mesh for the geometry created by the GeoModule function";
FlowModule::usage =
"\!\(\*
StyleBox[\"FlowModule\",\nFontWeight->\"Bold\"]\)[L, W1, W2, U, X1, X2, MP] calculates the flow and pressure fields that result from a sudden expansion in channel geometry";

Begin["`Private`"]

Needs["NDSolve`FEM`"]

\[Mu] = 1.0005*10^(-3);(*kg m^-1s^-1*)
\[Rho] = 9.99*10^2;(*kg m^-3*)

GeoModule :=DynamicModule[{L=0.04,W1=0.01,W2=0.015,U=0.04,X1=0.01,X2=0.03},Deploy[Grid[{{Style[Panel[Grid[Transpose[{{Row[{Style["Channel Length",Bold]," (L)"}],Row[{Style["Inlet Channel Width",Bold]," (W1)"}],Row[{Style["Expansion Width",Bold]," (W2)"}],Row[{Style["Average Inlet Velocity",Bold]," (U)"}],"X1","X2",Style["Inlet Reynolds Number",Bold]},{InputField[Dynamic[L],Number],InputField[Dynamic[W1],Number],InputField[Dynamic[W2],Number],InputField[Dynamic[U],Number],InputField[Dynamic[X1],Number],InputField[Dynamic[X2],Number],InputField[Dynamic[U*W1*\[Rho]/\[Mu]],Enabled->False]}}],Alignment->Right],
ImageMargins->10],DefaultOptions->{InputField->{ContinuousAction->False,FieldSize->{{8,Infinity},{1,1}}}}],
Dynamic[RegionPlot[RegionUnion[Rectangle[{0.,0},{L/3,W1}],Rectangle[{L/3,W1/2-W2/2},{L,W1/2+W2/2}]],AspectRatio->Automatic,ImageSize->Medium, Epilog->{ PointSize[0.01], Red,Point[{{X1,W1/2},{X2,W1/2}}],Text[Style["X1",12],{X1,2W1/3}],Text[Style["X2",12],{X2,2W1/3}]}]
]}}]]];

MeshModule :=DynamicModule[{L=0.04,W1=0.01,W2=0.015,MCM=0.025,MBCM=0.005},

Deploy[Grid[{{Style[Panel[Grid[Transpose[{{Row[{Style["Channel Length",Bold]," (L)"}],Row[{Style["Inlet Channel Width",Bold]," (W1)"}],Row[{Style["Expansion Width",Bold]," (W2)"}],Style["Max Cell Measure",Bold],Style["Max Boundary Cell Measure",Bold]},{InputField[Dynamic[L],Number],InputField[Dynamic[W1],Number],InputField[Dynamic[W2],Number],InputField[Dynamic[MCM],Number],InputField[Dynamic[MBCM],Number]}}],Alignment->Right],
ImageMargins->10],DefaultOptions->{InputField->{ContinuousAction->False,FieldSize->{{8,Infinity},{1,1}}}}],
Dynamic[Show[ToElementMesh[RegionUnion[Rectangle[{0.,0},{L/3,W1}],Rectangle[{L/3,W1/2-W2/2},{L,W1/2+W2/2}]],"MaxCellMeasure"->MCM,"MaxBoundaryCellMeasure"->MBCM]["Wireframe"],ImageSize->Medium
]]}}]]];

FlowModule[L_,W1_,W2_,U_,X1_,X2_,MP_] := Module[{x,y,p,xVel,yVel,pressure},

(*solver domain*)
width1 = W1; (*m*)
width2 = W2;(*m*)
length = L;(*m*)
SER=RegionUnion[Rectangle[{0.,0},{length/3,width1}],Rectangle[{length/3,width1/2-width2/2},{length,width1/2+width2/2}]];

(*solver mesh*)
mesh=ToElementMesh[SER,"MaxCellMeasure"->MP[[1]],"MaxBoundaryCellMeasure"->MP[[2]]];
refinementRegion=Rectangle[{length/3, width1/2-width2/2},{ 2*length/3,(width1/2+width2/2)}];
mrf=With[{rmf=RegionMember[refinementRegion]},Function[{vertices,area},Block[{x,y},{x,y}=Mean[vertices];
If[rmf[{x,y}],area>0.00025,area>0.00025]]]];

(*governing equations*)
EQs ={
			\[Rho](u[x,y]\!\(
\*SubscriptBox[\(\[PartialD]\), \(x\)]\(u[x, y]\)\)+v[x,y]\!\(
\*SubscriptBox[\(\[PartialD]\), \(y\)]\(u[x, y]\)\))==-\!\(
\*SubscriptBox[\(\[PartialD]\), \(x\)]\(p[x, y]\)\)+\[Mu](\!\(
\*SubscriptBox[\(\[PartialD]\), \(x, x\)]\(u[x, y]\)\)+\!\(
\*SubscriptBox[\(\[PartialD]\), \(y, y\)]\(u[x, y]\)\)),
			\[Rho](u[x,y]\!\(
\*SubscriptBox[\(\[PartialD]\), \(x\)]\(v[x, y]\)\)+v[x,y]\!\(
\*SubscriptBox[\(\[PartialD]\), \(y\)]\(v[x, y]\)\))==-\!\(
\*SubscriptBox[\(\[PartialD]\), \(y\)]\(p[x, y]\)\)+\[Mu](\!\(
\*SubscriptBox[\(\[PartialD]\), \(x, x\)]\(v[x, y]\)\)+\!\(
\*SubscriptBox[\(\[PartialD]\), \(y, y\)]\(v[x, y]\)\)),
			\!\(
\*SubscriptBox[\(\[PartialD]\), \(x\)]\(u[x, y]\)\)+\!\(
\*SubscriptBox[\(\[PartialD]\), \(y\)]\(v[x, y]\)\)==0
		  };

(*boundary conditions*)
PDiff=(- 12U \[Mu])/width1^2 (*N/m^3*);
bcWalls = DirichletCondition[{u[x,y]==0.,v[x,y]==0.}, x>0. && x<length];
bcVelocity = DirichletCondition[{u[x,y]== 1/(2\[Mu]) PDiff(y^2-width1 y),v[x,y]==0.},x==0.];
bcOutlet = DirichletCondition[p[x,y]==10^5,x==length];
BCs={bcVelocity,bcWalls, bcOutlet };

(*solver*)
{xVel,yVel,pressure}=NDSolveValue[{EQs,BCs},{u,v,p},{x,y}\[Element] mesh,Method->{"FiniteElement","InterpolationOrder"->{u->2,v->2,p->1},"MeshOptions"->{AccuracyGoal->16,PrecisionGoal->16,MeshRefinementFunction ->mrf}}];

(*outputs*)
U1 = (1/W1)*NIntegrate[xVel[X1,y],{y,0,W1}];
U2 = (1/W2)*NIntegrate[xVel[X2,y],{y,W1/2-W2/2,W1/2+W2/2}];
P1 =(1/W1)*NIntegrate[pressure[X1,y],{y,0,W1}];
P2 =(1/W2)*NIntegrate[pressure[X2,y],{y,W1/2-W2/2,W1/2+W2/2}];
A = StreamPlot[{xVel[x,y],yVel[x,y]},{x,y} \[Element] mesh, AspectRatio->Automatic, PlotLegends->Automatic, PlotRange->Full, StreamPoints->Fine];

{{L,W1,W2,U,X1,X2,xVel[X1,width1/2],U1, yVel[X1,width1/2],pressure[X1,width1/2],P1,xVel[X2,width1/2],U2,yVel[X2,width1/2],pressure[X2,width1/2],P2},A,xVel,yVel,pressure,mesh}

]

End[]
EndPackage[]













































