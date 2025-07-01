<<Graphics`Colors`
(* FUNCTIONS *)
Clear[ExtendedLine];
ExtendedLine[points_]:=Module[
      {first, final},
      If[Length[points] ? 2, Throw[extendedLineNeedsTwoPoints]];
      first = points[[1]];
      final = first + 100000(points[[2]] - first);
      Return[Line[{first,final}]];
      ];
LineIntersectCircle[pt1_,pt2_,center_,radius_] := Module[
      {t,x,y,solutions,myt,point},
      x = pt1[[1]] + t (pt2[[1]] - pt1[[1]]);
      y = pt1[[2]] + t(pt2[[2]] - pt1[[2]]);
      solutions = t/. Solve[(x - center[[
      1]])^2 + (y - center[[2]])^2 \[Equal] radius^2,t];
      myt = Min[solutions[[1]], solutions[[2]]];
      point = {pt1[[
      1]] + myt (pt2[[1]] - pt1[[1]]),pt1[[2]] + myt(pt2[[2]] - pt1[[2]])};
      Return[point];
      ];

(* CONSTANT SETUP *)
SUNOFFSET = {0.2, 0};
MARSCENTER = {0,0.3};
MARSEQUANT = {0,0.9};
MARSSIZE = 1.52;
SUNSIZE = 1;
sunsize = 1;
marssize = 1.52;
marsratio = 365/687;


(* PLANETARY FUNCTIONS *)
FirstInequality[days_]:=Module[
      {place},
      place = LineIntersectCircle[MARSEQUANT, MARSEQUANT + {
      Cos[0°+2 Pi days/
        686.6], Sin[0 ° + 2 Pi days / 686.6]}, MARSCENTER, MARSSIZE];
      Return[place];
      ];
SecondInequality[days_]:=Module[
      {place},
      place = SUNOFFSET+{Cos[2 Pi days / 365.25], Sin[2 Pi days / 365.25]};
      Return[place];
      ];

(* Ptolemy *)

ptolemygraphics =  {
      (* mars orbit *)
      {Raspberry, Thickness[.005], Circle[MARSCENTER, MARSSIZE]},
      (* mars epicycle *)
      {RGBColor[.8,0,
        0], Thickness[.005], Circle[FirstInequality[d](*+SUNOFFSET*), SUNSIZE]},
      (* line to mars *)
      {LightViridian, 
      Thickness[.008], Line[{FirstInequality[d],FirstInequality[d]+SecondInequality[d]-SUNOFFSET}]},
      (* sun line of sight *)
      {LightGoldenrod, Thickness[.005], Line[{{0,0},10000 SecondInequality[d]}]},
      (* mars line of sight *)
      {LightCadmiumRed, Thickness[.005], Line[{{0,0}, 10000 (FirstInequality[d]+SecondInequality[d]-SUNOFFSET)}]},
      (* sun orbit *)
      {Gold, Thickness[.005], Circle[SUNOFFSET, SUNSIZE]},
      (* sun *)
      {Yellow, PointSize[.025], Point[SecondInequality[d]]},
      (* center of mars epicycle *)
      {Raspberry, PointSize[.025], Point[FirstInequality[d]]},
      (* mars the planet *)
      {Red, PointSize[.025], Point[FirstInequality[d]+SecondInequality[d]-SUNOFFSET]},
      (* earth *)
      {DodgerBlue, PointSize[.025], Point[{0,0}]}
      };

myptolemytable = Table[Show[Graphics[
          ptolemygraphics
          ],AspectRatio->1,
           ImageSize->480, PlotRange->{{-4,4},{-4,4}}, Background->MidnightBlue
        ] ,{d, 0, 365*6, 10}];

(* Brahe *)

brahegraphics = {
      (* sun orbit *)
      {Gold, Thickness[.005], Circle[SUNOFFSET, SUNSIZE]},
      (* sun line of sight *)
      {LightGoldenrod, Thickness[.005], Line[{{
          0,0},10000 SecondInequality[d]}]},
      (* mars orbit *)
      {RGBColor[.8,0,0], 
      Thickness[.005], Circle[SecondInequality[d]-SUNOFFSET+MARSCENTER, MARSSIZE]},
      (* line to mars *)
      {LightViridian, Thickness[.008], Line[{ SecondInequality[d],
            FirstInequality[d]+SecondInequality[d]-SUNOFFSET}]},
      (* mars line of sight *)
      {LightCadmiumRed, Thickness[.005], Line[{{0,0}, 10000 (FirstInequality[d]+SecondInequality[d]-SUNOFFSET)}]},
      (* mars *)
      {Red, PointSize[.025], Point[FirstInequality[d]+SecondInequality[d]-
      SUNOFFSET]},
      (* sun *)
      {Yellow, PointSize[.025], Point[SecondInequality[d]]},
      (* earth *)
      {DodgerBlue, PointSize[.025], Point[{0,0}]}
      };

mybrahetable = Table[Show[Graphics[
          brahegraphics
          ],AspectRatio->1,
           ImageSize->480, PlotRange->{{-4,4},{-4,4}},
         Background->MidnightBlue
        ] ,{d,0,6 * 365.25,10}];

(* Copernicus *)

copernicusgraphics  = {
      (* earth orbit *)
      {RGBColor[.1,.3,.9], Thickness[.005], Circle[-SUNOFFSET, SUNSIZE]},
      (* sun line of sight *)
      {LightGoldenrod, Thickness[.005], ExtendedLine[{-SecondInequality[d], {0,0}}]},
      (* mars orbit *)
      {RGBColor[.8,0,0], Thickness[.005], Circle[
      MARSCENTER-SUNOFFSET, MARSSIZE]},
      (* mars line of sight *)
      {LightCadmiumRed, Thickness[.005], ExtendedLine[{-SecondInequality[d],
            FirstInequality[d]-SUNOFFSET}]},
      (* earth *)
      {DodgerBlue, PointSize[.025], Point[-SecondInequality[d]]},
 (* Mars is pushed back by SUNOFFSET so that its center is reckoned with respect to the mean, rather than the apparent sun *)
      (* mars *)
      {Red, PointSize[.025], Point[FirstInequality[d]-SUNOFFSET]},
      (* sun *)
      {Yellow, PointSize[.025], Point[{0,0}]}
      };

mycopernicustable = Table[ Show[Graphics[
          copernicusgraphics
          ],AspectRatio->1, 
          ImageSize->480, PlotRange->{{-4,4},{-4,4}}, Background->MidnightBlue
        ] ,{d, 0,  365.25*6, 10}];

(* Array *)

zoom = 4;
combinedtable = Table[Show[GraphicsArray[{
            (* Ptolemy *)
            Show[Graphics[ptolemygraphics],AspectRatio->1, ImageSize->480, PlotRange->
            zoom{{-1,1},{-1,1}}, Background->MidnightBlue, DisplayFunction->Identity]
            ,
            (* Copernicus *)
            Show[Graphics[copernicusgraphics],
                AspectRatio->1, ImageSize->480, PlotRange->zoom{{-1,1},{-1,1}}, Background->MidnightBlue, 
              DisplayFunction->Identity
              ] 
            ,
            (* Tycho Brahe *)
            Show[Graphics[brahegraphics],AspectRatio->1, 
            ImageSize->480, 
            PlotRange->zoom{{-1,1},{-1,1}}, Background->MidnightBlue, DisplayFunction->Identity
              ] 
            }]
        ,
        DisplayFunction->$DisplayFunction, ImageSize->800, 
		PlotLabel->StyleForm[" Ptolemy                   Copernicus            
                      Brahe   ",FontSize->24, 
                FontFamily->Helvetica]
        ], {d, 0, 8*365.25, 15}];