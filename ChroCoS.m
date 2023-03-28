(* ::Package:: *)

(* ::Title:: *)
(*ChroCoS*)


(* ::Subtitle:: *)
(* Chromatic community structure detection*)


(* ::Author:: *)
(*Franck Delaplace*)


(* ::Affiliation:: *)
(*IBISC Lab, PARIS SACLAY UNIVERSITY*)


(* ::Affiliation:: *)
(*March 2023*)


(* ::Text:: *)
(*Package of function related to Chromatic community structure detection*)


BeginPackage["ChroCoS`"];


(* ::Chapter:: *)
(*Signatures & Usage*)


$CHROCOLOR::usage="Basic palette of 6 colors (Association)"


$SEEDS::usage="Seeds for RandomColoring."


Transparency::usage="Option of RandomGraphColoring stipulating the probability of transparent nodes."


ChangeColor::usage="ChangeColor[G] interactively change the colors of the nodes of graph G. 
Select a color, drag a region where the color must be changed, then validate the modifications or reset it."


ShowChroCoS::usage="ShowChroCoS[G,P,thickness:0.015,communitycolor:ColorData[97],colors:$CHROCOLOR) show the community structure on the graph. The nodes in the same community have the same boundary color.
PARAMETER:
\[FilledSmallSquare] G: Graph,
\[FilledSmallSquare] P: community structure (list on lists),
\[FilledSmallSquare] thickness (optional): thickness of the boundary (default 0.015),
 communitycolor (optional): function of association defining the colors of the communities (default ColorData[97]),
\[FilledSmallSquare] colors (optional): function or association defining the colors of the nodes (default $CHROCOLOR palette),
"


ShowQG::usage="ShowQG[GC,P,size,colors] Show the quotient graph nicely.
PARAMETER:
\[FilledSmallSquare] GC:colored graph,
\[FilledSmallSquare] P: the community structure,
\[FilledSmallSquare] size (optional):image size (default Large).
\[FilledSmallSquare] colors: color palette (function or association)
OPTION: GraphLayout, ImageSize"


RandomGraphColoring::usage="RandomGraphColoring[G,c, \[Lambda]] color the graph G randomly. From a set of seeds {\!\(\*SubscriptBox[\(s\), \(1\)]\), \[Ellipsis],\!\(\*SubscriptBox[\(s\), \(r\)]\)} with a distinct color each, the probability of being of the same color as seeds \!\(\*SubscriptBox[\(s\), \(i\)]\) is proportional to \!\(\*SuperscriptBox[\(\[ExponentialE]\), \(\(-\[Lambda]d\) \((\*SubscriptBox[\(s\), \(i\)], v)\)\)]\).
PARAMETERS:
\[FilledSmallSquare] G : a Graph that must be connected.
\[FilledSmallSquare] c : list of color seeds {\!\(\*SubscriptBox[\(s\), \(1\)]\), \[Ellipsis],\!\(\*SubscriptBox[\(s\), \(r\)]\)} or number of colors \!\(\*
StyleBox[\"r\",\nFontSlant->\"Italic\"]\).
\[FilledSmallSquare] \[Lambda] : probabilistic parameter.
OPTION 
\[FilledSmallSquare] Transparency: probability of transparent nodes."


\[Kappa]::usage="\[Kappa][\!\(\*
StyleBox[\"r\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"n\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"d\",\nFontSlant->\"Italic\"]\)] enumeration of \!\(\*
StyleBox[\"d\",\nFontSlant->\"Italic\"]\)-color profile for a community of size \!\(\*
StyleBox[\"n\",\nFontSlant->\"Italic\"]\) with \!\(\*
StyleBox[\"r\",\nFontSlant->\"Italic\"]\) colors. "


\[Gamma]::usage="\[Gamma][r,n,d] enumeration of d-dominant color profile for a community of size n with r colors. "


Kcore::usage="Kcore[r,n,d,fenum] compute the core chromarity for a community of size n considering a d-color profile according to r colors with regard to the enumeration function fenum which is \[Kappa] or \[Gamma]."


ChroCoDe::usage="ChroCoDe[G,\[Delta]:2,\[Omega]] computes the chromatic community structure maximizing the \[Omega]-chromarity for a colored graph G. \[Delta] is the neighborhood distance (default 2)."


(* ::Chapter:: *)
(*Functions*)


 Begin["`Private`"]


(* ::Section:: *)
(*Miscellaneous*)


$CHROCOLOR=<|0->GrayLevel[0.9],1->RGBColor[0.55, 0.8300000000000001, 0.54],2->RGBColor[0.89, 0.42, 0.38],3->RGBColor[1, 0.86, 0.13],4->RGBColor[0.403921568627451, 0.6, 0.7764705882352941], 5->RGBColor[0.96, 0.63, 0.59], 6-> RGBColor[0.55, 0.2, 0.5700000000000001]|>


(* ::Subsection:: *)
(*Extend Power rule 0^0=1*)


Unprotect[Power];
Power[0,0]=1;
Protect[Power]


(* ::Section:: *)
(*View*)


Clear[ChangeColor];
SetAttributes[ChangeColor,HoldFirst]
ChangeColor[G0_,colors_:$CHROCOLOR]:=With[{coordinates=AssociationThread[VertexList[G0], First@Values@AbsoluteOptions[G0,VertexCoordinates]],
ok=Graphics[{RGBColor[0.41000000000000003`, 0.7000000000000001, 0.43],Rectangle[],Thickness[0.2],White,CapForm["Round"],Line[{{0.3,0.5},{0.5,0.3},{0.7,0.7}}] }, ImageSize->15],reset=Graphics[{RGBColor[0.9400000000000001, 0.2, 0.23529411764705882`],Rectangle[{-1,-1},{1,1}],Thickness[0.15],White,CapForm["Round"],Rotate[Triangle[ConstantArray[{Cos[0.4\[Pi]+0.3],Sin[0.4\[Pi]-0.7]},3]+0.2{{-2,-1},{0,1.5},{2,-1}}],\[Pi]/3.2],Line[Table[0.5{Cos[x],Sin[x]},{x,-\[Pi] 0.8,0.4\[Pi],\[Pi]/40}]]},ImageSize->15]},
DynamicModule[{id=0,mousepos={0,0}, color=0,G=G0},
Labeled[
EventHandler[
Dynamic[G],{
"MouseClicked":> (mousepos=MousePosition["Graphics"];
 id=First@Keys@MinimalBy[ coordinates,EuclideanDistance[mousepos,#]&];
G=Annotate[{G,id},{VertexWeight->color,VertexStyle->colors[color]}]),
"MouseDragged":> 
(mousepos=MousePosition["Graphics"];
 id=First@Keys@MinimalBy[ coordinates,EuclideanDistance[mousepos,#]&];
G=Annotate[{G,id},{VertexWeight->color,VertexStyle->colors[color]}])}],
Column[{SetterBar[Dynamic[color],colors, Appearance->"Vertical"],ButtonBar[{ok:> (G0=G),reset:>(G=G0)},Appearance->"Vertical"]
},Alignment->Center]
,Left]]]


ShowChroCoS[GC_Graph,P_List,thickness_Real:0.015,communitycolors_:ColorData[97],colors_:$CHROCOLOR]:=
Annotate[GC, 
{
VertexStyle->Flatten@Table[  Function[v,v->Directive[ EdgeForm[{Opacity[1],Thickness[thickness],communitycolors[i]}],colors[AnnotationValue[{GC,v},VertexWeight]]]]/@P[[i]],{i,Length[P]}]}]


Clear[Sector]
Sector::usage="Sector[coord,size,data,border,colors] Draw an annulus pie chart. 
PARAMETER:
coord: XY coordinates of the sector - Integer pair,
size: size - Integer,
border: border height (Integer - default 0.6),
data: data - Assocation label \[Rule] value,
color: colors - Association label \[Rule] color."
Sector[coord_,size_,border_Integer:0.6,data_Association,colors_:$CHROCOLOR]:=Module[{cumulate=0,total=Total[data]},
{EdgeForm[{RGBColor[0.15, 0.15, 0.15],Thick}],Disk[coord,size],
KeyValueMap[Function[{k,nb}, With[{pie=Annulus[coord +{Cos[2 (cumulate) \[Pi]/total],Sin[2(cumulate) \[Pi]/total]}10^-3,size{border,1},{2\[Pi] cumulate/total,2 \[Pi] (cumulate+nb)/total}]},
cumulate+= nb;{colors[k],pie}]],data]}]


Options[ShowQG]={GraphLayout->Automatic, ImageSize->Large};
ShowQG[GC_Graph,P:{_List...}, thecolors_:$CHROCOLOR,OptionsPattern[]]:=
Enclose[
ConfirmAssert[MatchQ[AnnotationValue[GC,VertexWeight],_List]];
With[{QG=RelationGraph[Function[{p1,p2}, p1!= p2 \[And] AnyTrue[p1,
Function[ v1, AnyTrue[ p2,Function[v2,EdgeQ[GC,v1 \[UndirectedEdge]v2]]]]]],P], nbcolors=Max@AnnotationValue[GC,VertexWeight]},
Framed@Legended[Annotate[QG,{
VertexLabels->None,
VertexShape->None,
VertexShapeFunction->  Function[ {coord,community,size}, 
Tooltip[
{
Sector[coord,size,Counts[Function[v,AnnotationValue[{GC,v},VertexWeight]]/@community],thecolors],
Black,
Text[Style[Length[community],FontFamily->"Impact",11],coord]
},
Multicolumn[
Style[#,thecolors[AnnotationValue[{GC,#},VertexWeight]],Bold, FontFamily->"Franklin Gothic",10]&/@community
,4]
,TooltipStyle->{Background->White}]
],
EdgeStyle ->  Directive[Black,Thick], 
VertexSize->0.6,
ImageSize->OptionValue[ImageSize],
 VertexStyle->(# -> ColorData["LightTemperatureMap",
Max[Length/@Gather[Function[p,AnnotationValue[{GC,p},VertexWeight]]/@#]]/Length[#]]
)&/@VertexList[QG]
, GraphLayout->OptionValue[GraphLayout]
}],{
Placed[BarLegend["LightTemperatureMap", LegendLayout->"Row",LegendLabel->Placed["Density",Left], LabelStyle->Directive[FontFamily->"Franklin Gothic",11]],Top]}]]
,$Failed&]


(* ::Section:: *)
(*Random Graph*)


ProbColor[G_Graph,color_Integer,seeds_List,\[Lambda]_Real,v_]:= E^(-\[Lambda] GraphDistance[G,v,seeds[[color]]])/\!\(
\*UnderoverscriptBox[\(\[Sum]\), \(s\), \(seeds\)]\ 
\*SuperscriptBox[\(E\), \(\(-\[Lambda]\)\ GraphDistance[G, v, s]\)]\)


Protect[Transparency]


Options[RandomGraphColoring]={Transparency->0.};
RandomGraphColoring[G_Graph/;ConnectedGraphQ[G],c_Integer/;c>= 1, \[Lambda]_Real:1.0,OptionsPattern[]]:= Module[{seeds, nbcolors,v,V},
(* Most distanced seeds selection *)
V=VertexList[G];
If[c>1,
seeds= RandomChoice@MaximalBy[Tuples[V,2],GraphDistance[G,#[[1]],#[[2]]]&],
seeds={First[V]}]; (* single color case *)

V=Complement[V,seeds];
nbcolors=c-2;
While[ nbcolors-->0,
v=RandomChoice@MaximalBy[V,Function[v,GeometricMean@Table[GraphDistance[G,s,v],{s,seeds}]]]; 
AppendTo[seeds,v];
V=DeleteCases[V,v]];
(* Graph coloring *)
RandomGraphColoring[G,seeds,\[Lambda],Transparency->OptionValue[Transparency]]]

RandomGraphColoring[G_Graph/;ConnectedGraphQ[G],seeds_List, \[Lambda]_Real:1.0,OptionsPattern[]]:= 
With[{colors=Range[1,Length[seeds]]},
$SEEDS=seeds;
Annotate[
G,VertexWeight-> Thread[seeds->colors] ~Join~Table[v-> If[ RandomReal[]< OptionValue[Transparency],0,
 RandomChoice[Table[E^(-\[Lambda] GraphDistance[G,v,s]),{s,seeds}]->colors]],{v,Complement[VertexList[G],seeds]}]]]



(* ::Section:: *)
(*Chromarity*)


(* ::Subsection:: *)
(*Chromatic signatures*)


\[ScriptCapitalS]::usage= "\[ScriptCapitalS][r,n,d] finds all chromatic signatures \[Sigma] such that:
\[FilledSmallSquare] \!\(\*FormBox[SubsuperscriptBox[\(\[Sum]\), \(i = 1\), \(r\)],
TraditionalForm]\) \!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\) = n,
\[FilledSmallSquare] \!\(\*FormBox[\(\*SubscriptBox[\(\[ForAll]\), \(i\)]\(\(:\)\(\\\ \)\(\(0\)\(\\\ \)\(\[LessEqual]\)\*SubscriptBox[\(\[Sigma]\), \(i\)]\(\[LessEqual]\)\(d\)\(\\\ \)\)\)\),
TraditionalForm]\),
\[FilledSmallSquare] Subscript[\[Sigma], r]=d,
\[FilledSmallSquare] \!\(\*FormBox[\(\[ForAll] 1\\\  \[LessEqual] i, j \[LessEqual] \(r : \\\ i \[LessEqual] j\\\  \[Implies] \\\ \*SubscriptBox[\(\[Sigma]\), \(i\)]\\\  \[LessEqual] \\\ \*SubscriptBox[\(\[Sigma]\), \(j\)]\)\),
TraditionalForm]\)."

\[ScriptCapitalS][1,n_,n_]:={{n}} (* To enable a symbolic computation *)
\[ScriptCapitalS][0,0,_]:={{}} (* No variables with a null sum provides an empty solution *)
\[ScriptCapitalS][0,_,_]:={} (* Otherwise no solution *)
\[ScriptCapitalS][r_Integer/;r>0,n_Integer/;n>=0,d_Integer/;d>0]:= Module[ {S},
	S[depth_,0,_,seq_]:={ ConstantArray[0,depth] ~Join~ seq};
	S[_,_,0,_]:={};
	S[0,0,_,seq_]:={seq};
	S[0,_,_,_]:= {};
S[depth_,rest_,dbound_,seq_]:=Fold[Function[{l,v}, Join[S[depth-1,rest-v,v,Prepend[seq,v]],l ]],{},Range[\[LeftCeiling]rest/depth\[RightCeiling],Min[dbound,rest]]];
S[ r-1,n-d,d,{d}]]


(* ::Subsection:: *)
(*Combinatorics Enumeration of color profile*)


Clear[\[Kappa]]
\[Kappa][r_,n_,d_]:=   Sum[ (-1)^(k-1) Binomial[r,k] n!/((n-k d)! (d!)^k) (r-k)^(n-k d),{k,1,Min[r,\[LeftFloor]n/d\[RightFloor]]}]


Clear[\[Gamma]]
\[Gamma][r_,n_,d_]:=n!r!  Sum[1/(Product[s!,{s,Counts[\[Sigma]]}] Product[s!,{s,\[Sigma]}]),{\[Sigma],\[ScriptCapitalS][r,n,d]}]


(* ::Subsection:: *)
(*Chromarity Core*)


Kcore[r_Integer,n_Integer,d_Integer, fenum_]:=d/n (1 - fenum[r,n,d]/r^n)


(* ::Subsection:: *)
(*Chromarity  K *)


K[G_Graph,P:{_List..}, \[Omega]_]:= With[{r= Max@AnnotationValue[G,VertexWeight],colorcount=Table[ Counts[AnnotationValue[{G,#},VertexWeight]&/@p],{p,P}]},
Mean@Table[With[{n=Total[p], d=Max@KeyDrop[p,0]},If[d==-\[Infinity] \[Or] n==0, 0.,Kcore[r,n,d,\[Omega]]]],{p,colorcount}]]


(* ::Input:: *)
(**)


(* ::Section:: *)
(*ChroCoDe*)


ChroCoDe[G_Graph, \[Delta]_Integer:2, \[Omega]_]:= Module[{QG,V,v,c, neighbors, community,newincommunity, P,k,kmax, Pscan,p,q,improved,communitypath,maxpath, Pedges},
P={};

(* Build the first communities composed of connected vertices with the same color *)
V=VertexList[G];
While[V !={},
v= First[V];V=Rest[V];

newincommunity= {v};
c= AnnotationValue[{G,v},VertexWeight];
community={};

While[newincommunity != {},
v=First[newincommunity]; newincommunity=Rest[newincommunity];
AppendTo[community,v];
neighbors= Cases[EdgeList[G], (v\[UndirectedEdge]w_ |w_ \[UndirectedEdge] v)/;( AnnotationValue[{G,w},VertexWeight]==c  \[And] \[Not] MemberQ[community,w] )->w];
newincommunity=newincommunity \[Union] neighbors];

V=Complement[V,community];
AppendTo[P,community]];
(* Build the quotient graph of communities with nodes of the same color *) 
QG=RelationGraph[Function[{p1,p2}, p1!= p2 \[And] AnyTrue[p1,
Function[ v1, AnyTrue[ p2,Function[v2,EdgeQ[G,v1 \[UndirectedEdge]v2]]]]]],P];

(* Fusion of the colorful communities to improve the K-score *)
Pscan =P;
While[Pscan != {},
(* Select a community according to an heuristic *)
 p=First@MinimalBy[Pscan,K[G,{#},\[Omega]]&];
Pscan=DeleteCases[Pscan,p] ;

(* Find in the d-neighborhood of p the communities with the same color than p, d \[GreaterEqual] 2 *)
neighbors=DeleteCases[VertexList@NeighborhoodGraph[QG,p,\[Delta]],p];

(* Best fusion of communities with p in the neighborhood of p maximizing K *)

kmax= K[G,P,\[Omega]];
improved=False;
Do[
communitypath=FindShortestPath[QG,p,q];
k=K[G, Append[Complement[P,communitypath],Join@@communitypath],\[Omega]];
If[kmax < k ,
improved = True;
kmax=k;
maxpath= communitypath;
],{q,neighbors}];

If[improved, 
P=   Append[Complement[P,maxpath],v=Join@@maxpath];
Pedges= EdgeList[QG];
 QG= SimpleGraph[ 
Complement[Pedges,Cases[Pedges,( (w_\[UndirectedEdge]_ )|(_\[UndirectedEdge]w_ ))/;MemberQ[maxpath,w],{1}]] ~Join~
Cases[Pedges,((( u_\[UndirectedEdge]w_)|(w_ \[UndirectedEdge]u_))/;(MemberQ[maxpath,w] \[And] \[Not] MemberQ[maxpath,u]))-> v \[UndirectedEdge] u,{1}]
];
Pscan=P]];
P]


(* ::Section:: *)
(*END*)


End[]


EndPackage[]
