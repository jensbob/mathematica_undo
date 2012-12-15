(* ::Package:: *)

If[!TrueQ[$UNDODEF], (*only load if not loaded before*)
   $UNDODEF=True;  


(* check file save*)
   CheckCommitFile[nb_] := If[! TrueQ[Quiet[FileExistsQ[nb]]],
			 CreateDialog[Column[{"You need to save the notebook, in order \n to use the commiting and undo system.", "", Item[DefaultButton[], Alignment -> Right]}]];
			 Abort[];
			];

(*Define copy and delete for different operating systmes*)
   OSCopy=If[$OperatingSystem == "Windows","copy","cp"];
   OSDelete=If[$OperatingSystem == "Windows","del","rm"];

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
	      Import["!"<>ToString[OSCopy]<>" "<> ToString[nb] <> " " <> ToString[nb] <> ToString[RecentVersion] <> ".bak", "Table"];
	      (*update undo file*)
	      Export[ToString[nb] <> ".undo.mx", {RecentVersion, MaxVersion, CommitList}];
	     ];
	     );

(*----Define Commit Info----*)
   CommitInfo :=
       Module[{nb = Quiet[NotebookFileName[]], RecentVersion, MaxVersion, CommitList}, CheckCommitFile[nb];
	      If[FileExistsQ[ToString[nb] <> ".undo.mx"], 
		 {RecentVersion, MaxVersion, CommitList} = 
		 Import[ToString[nb] <> ".undo.mx"];
		]; 
	      If[! NumberQ[RecentVersion], 
		 CreateDialog[
		     Column[{"Nothing Commited", "", "Commit mode: " <> ToString[AutoCo],"",Item[DefaultButton[], Alignment -> Right]}]
			     ],
		 CreateDialog[
		     Column[{"Currently working on version: " <> ToString[RecentVersion], "", "Commit mode: " <> ToString[AutoCo], "", Row[{TableForm[Take[CommitList,Max[-30,-Length[CommitList]]]]}], "", Item[DefaultButton[], Alignment -> Right]}] (*open dialog with a list of the last 30 versions*)
			     ];
		];
	     ];

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

	      Import["!"<>ToString[OSCopy]<>" " <> ToString[nb] <> ToString[RecentVersion-1] <> ".bak" <> " " <> ToString[nb], "Table"]; (*copy last backup on notebook file*)
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
		 Import["!"<>ToString[OSCopy]<>" " <> ToString[nb] <> ToString[RecentVersion] <> ".bak" <> " " <> ToString[nb], "Table"]; 
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
		 Import["!"<>ToString[OSCopy]<>" "<>ToString[nb]<>ToString[RecentVersion]<>".bak"<> " "<>ToString[nb],"Table"];
		 FrontEndExecute[FrontEndToken["Revert",False]];,Print["Invalid Version"]
		];
	     ] ;
		

(*----- Auto and manual commit ----*)
   AutoCo="manual";(*Flag for auto commit*)

   AutoCommit := Module[{nb = Quiet[NotebookFileName[]]},
       AutoCo="on evaluation";
       	      CheckCommitFile[nb];
       Quiet[RemoveScheduledTask[croncommit[nb]];];
       NotebookEvaluate[$PreRead=(If[!StringFreeQ[ToString[#],{"Undo","Redo","GotoCommit","CommitInfo"}],Null,Commit[nb]];#)&];];

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

undo::usage="This package adds a poor mans version control and undo functionality to Mathematica notebooks.

For a 'notebook.nb' a version information file ('notebook.nb.undo.mx') is created and every time changes are commited a backup file ('notebook.nb[version].bak') is created. Keyboard shortcuts allow an easy way to commit versions and move in between them.

------------------
Keyboard Shortcuts
------------------

Alt+z : Undo 
Alt+x : Redo 
Alt+s : Commit
Alt+d : Show CommitInfo dialog

--------------------------------------
Usage: Evaluate the following commands
--------------------------------------

ManualCommit (Default) - Only do commits manually.

AutoCommit - Turn on automatic commits. Every time a cell is evaluated a new commit is made. (This can lead to a lot of files)

CronCommit[n] - makes a new commit every n minutes.

CommitNow - Making a commit
CommitInfo - Show a list of all Versions
CommitClean - Remove all commited files

Undo - Undo to the previous commit
Redo - Undo to the next commit
GotoCommit[n] - Go to the nth version
";


CommitNow::usage="Makes a commit of the current Notebook";
ManualCommit::usage="Set the commiting mode to manual";
AutCommit::usage="Commit before the each evaluation";
CronCommit::usage="CronCommit[n] makes an automatic commit every n minutes";


 ];

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