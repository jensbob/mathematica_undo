(* ::Package:: *)
$ContextPath = Union[$ContextPath, {"SharedContext`"}]
Begin["SharedContext`"];


BeginPackage["undo`"]

undo::usage="This package adds a poor mans version control and a simple undo functionality to Mathematica notebooks.

For 'notebook.nb':
* a version information file ('notebook.nb.undo.mx') is created.
* every time changes are commited a backup file ('notebook.nb[version].bak') is created. 

Keyboard shortcuts allow an easy way to commit versions and move in between them.

Keyboard Shortcuts
------------------
Alt+z : Undo 
Alt+x : Redo 
Alt+s : Commit
Alt+d : Show CommitInfo dialog
------------------

Functions Defined: ManualCommit, AutoCommit, CronCommit[n], CommitInfo, CommitClean, CommitNow Undo, Redo, GotoCommit[n], SetVersionLimit[n]
";


CommitNow::usage="CommitNow makes a commit of the current Notebook.";
ManualCommit::usage="ManualCommit sets the commiting mode to manual.";
AutoCommit::usage="AutoCommit: Commit before the each evaluation.";
CronCommit::usage="CronCommit[n] makes an automatic commit every n minutes.";
CommitClean::usage="CommitClean deletes all backup files.";
CommitInfo::usage="CommitInfo opens a dialog with the commited versions.";
Undo::usage="Undo moves back to the previous version. 

If you are currently working on the latest version (highest version number) the current changes are automaically commited. Otherwise data might be lost.";
Redo::usage="Redo moves forward in the commited versions.";
GotoCommit::usage="GotoCommit[n] loads version n.";
SetVersionLimit::usage="SetVersionLimit[n] sets the limit of saved commits to n."

Begin["`Private`"];

warning1 = "You need to save the notebook, in order \n to use the commiting and undo system.";
AutoCo"manual";
VersionLimit=1000;
SetVersionLimit[n_]:=(VersionLimit=n;);

(*Define copy and delete for different operating systmes*)
   OSCopy=If[$OperatingSystem == "Windows","copy","cp"];
   OSDelete=If[$OperatingSystem == "Windows","del","rm"];

(* check file save and shows a dialog if the file was not saved, but only when we are not in autocommit mode*)
CheckCommitFile[nb_] := 
    If[! TrueQ[Quiet[FileExistsQ[nb]]]&&AutoCo!="on evaluation",
       CreateDialog[Column[{warning1, "", Item[DefaultButton[], Alignment -> Right]}]];
       Abort[];
      ];

(*----Define Commit Info----*)
CommitInfo :=
    Module[{nb = Quiet[NotebookFileName[]], RecentVersion, MaxVersion, CommitList}, 
	   CheckCommitFile[nb];
	   If[FileExistsQ[ToString[nb] <> ".undo.mx"], 
	      {RecentVersion, MaxVersion, CommitList} = 
	      Import[ToString[nb] <> ".undo.mx"];
	     ]; 
	   If[! NumberQ[RecentVersion], 
	      CreateDialog[
		  Column[{"Nothing Commited", "", "Commit mode: " <> ToString[AutoCo],"",Item[DefaultButton[], Alignment -> Right]}]
			  ],
	      CreateDialog[
		  Column[{"Currently working on version: " <> ToString[RecentVersion], "", "Backed up versions limited to the last: " <>ToString[VersionLimit],"","Commit mode: " <> ToString[AutoCo], "", Row[{TableForm[Take[CommitList,Max[-VersionLimit,-Length[CommitList]]]]}], "", Item[DefaultButton[], Alignment -> Right]}] (*open dialog with a list of the last 30 versions*)
			  ];
	     ];
	  ];

(*----Define Commit----*)
   CommitNow:= Commit[Quiet[NotebookFileName[]]];

   Commit[nb_] := (       
       Module[{RecentVersion, MaxVersion, CommitList},
	      CheckCommitFile[nb];
	      If[FileExistsQ[ToString[nb] <> ".undo.mx"],
		 {RecentVersion, MaxVersion, CommitList} = Import[ToString[nb] <> ".undo.mx"];
		]; (*If there is an undo file load the parameters*)

	      If[!NumberQ[RecentVersion],   
		 RecentVersion = 1;MaxVersion=0; CommitList = {}, RecentVersion=MaxVersion+1;
		]; (*check if parameters are set and add new version*)

	      MaxVersion = RecentVersion; (*update latest version number*)
	      AppendTo[CommitList, {MaxVersion, DateString[]}]; (*update commit list*)
	     	      (*Save the Notebook*)
	      NotebookSave[EvaluationNotebook[], nb];
	      (*Create Backup Copy*)
	      Import["!"<>ToString[OSCopy]<>" "<> ToString[nb] <> " " <> ToString[nb] <> ToString[Mod[RecentVersion,VersionLimit,1]] <> ".bak", "Table"];
	      (*update undo file*)
	      Export[ToString[nb] <> ".undo.mx", {RecentVersion, MaxVersion, CommitList}];
	     ];
	     );


(*----Define CommitClean-----*)
   (*Removes all backups*)
   CommitClean :=
       Module[{nb = Quiet[NotebookFileName[]]},
	      CheckCommitFile[nb];
	      Import["!"<>ToString[OSDelete]<>" " <> ToString[nb] <> "[0-9]*.bak", "Table"];
	      Import["!"<>ToString[OSDelete]<>" " <> ToString[nb] <> ".undo.mx", "Table"];
	      Print["Commit: Clean"]
	     ];
		 

(*----------------Undo--------------*)
   Undo := ( 
       CheckCommitFile[Quiet[NotebookFileName[]]];
       (*If we are working with the recent version and changed it, make a commit*)
       If[FileExistsQ[ToString[NotebookFileName[]] <> ".undo.mx"],
	  If[Import[ToString[NotebookFileName[]] <> ".undo.mx"][[1]]==Import[ToString[NotebookFileName[]] <> ".undo.mx"][[2]]&&("ModifiedInMemory" /. NotebookInformation@SelectedNotebook[]),
	     Commit[NotebookFileName[]];
	    ];
	 ];
       (*Actual Undo*)
       Module[{nb = NotebookFileName[], RecentVersion, MaxVersion, CommitList}, 
	      If[FileExistsQ[ToString[nb] <> ".undo.mx"], 
		 {RecentVersion, MaxVersion, CommitList} = Import[ToString[nb] <> ".undo.mx"];
		]; 

	      Import["!"<>ToString[OSCopy]<>" " <> ToString[nb] <> ToString[Mod[RecentVersion-1,VersionLimit,1]] <> ".bak" <> " " <> ToString[nb], "Table"]; (*copy last backup on notebook file*)
	      If[RecentVersion>1,
		 FrontEndExecute[FrontEndToken["Revert",False]];(*false - show no warning*)
		 RecentVersion -= 1;
		];(*revert to that version*)
	      
	      Export[ToString[nb] <> ".undo.mx", {RecentVersion, MaxVersion, CommitList}]; 
	     ];
	   );

(*-------Define Redo---------*)
   Redo := 
       Module[{nb = Quiet[NotebookFileName[]], RecentVersion, MaxVersion, CommitList}, 
	      CheckCommitFile[nb]; 
	      If[FileExistsQ[ToString[nb] <> ".undo.mx"], 
		 {RecentVersion, MaxVersion, CommitList} = Import[ToString[nb] <> ".undo.mx"];
		]; 
	      If[RecentVersion < MaxVersion, 
		 RecentVersion += 1; 
		 Import["!"<>ToString[OSCopy]<>" " <> ToString[nb] <> ToString[Mod[RecentVersion,VersionLimit,1]] <> ".bak" <> " " <> ToString[nb], "Table"]; 
		 FrontEndExecute[FrontEndToken["Revert",False]];
		];
	      Export[ToString[nb] <> ".undo.mx", {RecentVersion, MaxVersion, CommitList}];
	     ];
	   

(*------------GotoCommit-----------*)
   GotoCommit[a_]:=
       Module[{nb = Quiet[NotebookFileName[]], RecentVersion, MaxVersion,CommitList},
	      CheckCommitFile[nb];
	      If[FileExistsQ[ToString[nb] <> ".undo.mx"], 
		 {RecentVersion, MaxVersion, CommitList} = Import[ToString[nb] <> ".undo.mx"];
		]; 
	      If[a>=1&&a<=MaxVersion&&RecentVersion!=0,
		 RecentVersion=a;
		 Import["!"<>ToString[OSCopy]<>" "<>ToString[nb]<>ToString[Mod[RecentVersion,VersionLimit,1]]<>".bak"<> " "<>ToString[nb],"Table"];
		 FrontEndExecute[FrontEndToken["Revert",False]];,Print["Invalid Version"]
		];
	     ] ;
		

(*----- Auto and manual commit ----*)
   AutoCo="manual";(*Flag for auto commit*)

   AutoCommit := Module[{nb = Quiet[NotebookFileName[]]},
       CheckCommitFile[nb];
       AutoCo="on evaluation";
       Quiet[RemoveScheduledTask[croncommit[NotebookFileName[]]];];
       NotebookEvaluate[$PreRead=(If[!StringFreeQ[ToString[#],{"Undo","Redo","GotoCommit","CommitInfo","CommitNow","Commit"}],Null,CommitNow];#)&];];

   ManualCommit := Module[{nb = Quiet[NotebookFileName[]]},
       AutoCo="manual";
       CheckCommitFile[nb];
       Quiet[RemoveScheduledTask[croncommit[nb]];];
       NotebookEvaluate[Clear@$PreRead];];

   CronCommit[n_] := With[{nb = Quiet[NotebookFileName[]]},AutoCo = "every " <> ToString[n] <> " min";
       CheckCommitFile[nb];
		      croncommit[nb] = 
		      CreateScheduledTask[Commit[nb], n*60];
		      StartScheduledTask[croncommit[nb]];
			 ];


End[];

Protect[ManualCommit, AutoCommit, CronCommit, CommitInfo, CommitClean, CommitNow, Undo, Redo, GotoCommit, SetVersionLimit]

EndPackage[];


(*----- Keyboard  Shortcuts ----*)
FrontEndExecute[FrontEnd`ResetMenusPacket[{Automatic}]];
   (*Reset menus*)

FrontEndExecute[
 FrontEnd`AddMenuCommands["Undo",
  {Delimiter, MenuItem["CommitInfo",
    FrontEnd`KernelExecute[
     nb = SelectedNotebook[];
     SelectionMove[nb, After, Cell]; 
     NotebookWrite[nb, Cell[BoxData[RowBox[{CommitInfo}]], "Input"]];
     SelectionMove[nb, Previous, Cell];
     SelectionEvaluate[nb];
     SelectionMove[nb, Previous, Cell]; 
     NotebookDelete[nb]
],
    MenuKey["d", Modifiers -> {"Command"}],
    System`MenuEvaluator -> Automatic]}]];
 



FrontEndExecute[
 FrontEnd`AddMenuCommands["Undo",
  {Delimiter, MenuItem["Redo Commit",
    FrontEnd`KernelExecute[
     nb = SelectedNotebook[];
     SelectionMove[nb, After, Cell]; 
     NotebookWrite[nb, Cell[BoxData[RowBox[{Redo}]], "Input"]];
     SelectionMove[nb, Previous, Cell];
     SelectionEvaluate[nb];
     SelectionMove[nb, Previous, Cell]; 
     NotebookDelete[nb];
     (*Save the Notebook after redo*)
     NotebookSave[]
],
    MenuKey["x", Modifiers -> {"Command"}],
    System`MenuEvaluator -> Automatic]}]];


FrontEndExecute[
 FrontEnd`AddMenuCommands["Undo",
  {Delimiter, MenuItem["Undo Commit",
    FrontEnd`KernelExecute[
     nb = SelectedNotebook[];
     SelectionMove[nb, After, Cell]; 
     NotebookWrite[nb, Cell[BoxData[RowBox[{Undo}]], "Input"]];
     SelectionMove[nb, Previous, Cell];
     SelectionEvaluate[nb];
     SelectionMove[nb, Previous, Cell]; 
     NotebookDelete[nb];
     (*Save the Notebook after undo to have the right notebook save satus*)
     NotebookSave[]
],
    MenuKey["z", Modifiers -> {"Command"}],
    System`MenuEvaluator -> Automatic]}]];

FrontEndExecute[
 FrontEnd`AddMenuCommands["Undo",
  {Delimiter, MenuItem["Commit this version",
    FrontEnd`KernelExecute[
     nb = SelectedNotebook[];
     SelectionMove[nb, After, Cell]; 
     NotebookWrite[nb, Cell[BoxData[RowBox[{CommitNow}]], "Input"]];
     SelectionMove[nb, Previous, Cell];
     SelectionEvaluate[nb];
     SelectionMove[nb, Previous, Cell]; 
     NotebookDelete[nb];
     (*Save the Notebook after undo to have the right notebook save satus*)
     NotebookSave[]
],
    MenuKey["s", Modifiers -> {"Command"}],
    System`MenuEvaluator -> Automatic]}]];

End[];


(*
Copyright 2012 Jens Boberski

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*)
