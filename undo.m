(* ::Package:: *)

If[!TrueQ[$UNDODEF], (*only load if not loaded before*)
   $UNDODEF=True;  

   (* check file save*)
   CheckCommitFile := If[! TrueQ[Quiet[FileExistsQ[NotebookFileName[]]]],
			 CreateDialog[Column[{"You need to save the notebook, in order use the commiting and undo system.", "", Item[DefaultButton[], Alignment -> Center]}], WindowFrame -> "Normal"];
			 Abort[];
			];
   (*Define copy and delete for different operating systmes*)

   OSCopy=If[$OperatingSystem == "Windows","copy","cp"];
   OSDelete=If[$OperatingSystem == "Windows","del","rm"];

   (*----Define Commit----*)
   Commit := (
       CheckCommitFile;
       Module[{nb = NotebookFileName[], RecentVersion, MaxVersion, CommitList},

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

   CommitInfo := (
       CheckCommitFile;
       Module[{nb = NotebookFileName[], RecentVersion, MaxVersion, CommitList}, 
	      If[FileExistsQ[ToString[nb] <> ".undo.mx"], 
		 {RecentVersion, MaxVersion, CommitList} = 
		 Import[ToString[nb] <> ".undo.mx"];
		]; 
	      If[! NumberQ[RecentVersion], 
		 CreateDialog[
		     Column[{"Nothing Commited", "", "Auto Commit Status: " <> ToString[TrueQ[AutoCo]],"",Item[DefaultButton[], Alignment -> Center]}], WindowFrame -> "Normal"
			     ],
		 CreateDialog[
		     Column[{"Currently working on version: " <> ToString[RecentVersion], "", "Auto Commit Status: " <> ToString[TrueQ[AutoCo]], "", Row[{TableForm[CommitList]}], Item[DefaultButton[], Alignment -> Center]}], WindowFrame -> "Normal"
			     ];
		];
	     ];
		 );

   (*----Define CommitClean-----*)
   (*Removes all backups*)
   CommitClean := (
       CheckCommitFile;
       Module[{nb = NotebookFileName[]},
	      Import["!"<>ToString[OSDelete]<>" " <> ToString[nb] <> "[0-9]*.bak", "Table"];
	      Import["!"<>ToString[OSDelete]<>" " <> ToString[nb] <> ".undo.mx", "Table"];
	      Print["Commit: Clean"]
	     ];
		  );

   (*----------------Undo--------------*)

   Undo := ( 
       CheckCommitFile;
       (*If we are working with the recent version and changed it, make a commit*)
       If[FileExistsQ[ToString[NotebookFileName[]] <> ".undo.mx"],
	  If[Import[ToString[NotebookFileName[]] <> ".undo.mx"][[1]]==Import[ToString[NotebookFileName[]] <> ".undo.mx"][[2]]&&("ModifiedInMemory" /. NotebookInformation@SelectedNotebook[]),
	     Commit;
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

   (*-------Define Redo---------_*)

   Redo :=  (CheckCommitFile; Module[{nb = NotebookFileName[], RecentVersion, MaxVersion, 
   CommitList}, 
  If[FileExistsQ[
    ToString[nb] <> 
     ".undo.mx"], {RecentVersion, MaxVersion, CommitList} = 
     Import[ToString[nb] <> ".undo.mx"];]; 
  If[RecentVersion < MaxVersion, RecentVersion += 1; 
   Import["!"<>ToString[OSCopy]<>" " <> ToString[nb] <> ToString[RecentVersion] <> 
     ".bak" <> " " <> ToString[nb], "Table"]; 
   FrontEndExecute[FrontEndToken["Revert",False]];(*Print["This is the newest version"]*)]; 
  Export[ToString[nb] <> ".undo.mx", {RecentVersion, MaxVersion, 
    CommitList}]; (*Print["Version: ", RecentVersion]*)];
	 );
(*------------GotoCommit-----------_*)

GotoCommit[a_]:= (CheckCommitFile;
Module[{nb = NotebookFileName[], RecentVersion, MaxVersion, 
   CommitList}, 
  If[FileExistsQ[
    ToString[nb] <> 
     ".undo.mx"], {RecentVersion, MaxVersion, CommitList} = 
     Import[ToString[nb] <> ".undo.mx"];]; 
If[a>=1&&a<=MaxVersion&&RecentVersion!=0,
RecentVersion=a;
Import["!"<>ToString[OSCopy]<>" "<>ToString[nb]<>ToString[RecentVersion]<>".bak"<> " "<>ToString[nb],"Table"];
FrontEndExecute[FrontEndToken["Revert",False]];,Print["Invalid Version"]];
(*Print["Version: ",RecentVersion]*)
		      ] ;
		 );

(*----- Auto and manual commit ----*)

AutoCo=False;(*Flag for auto commit*)

AutoCommit:=(AutoCo=True;NotebookEvaluate[
$PreRead=(If[!StringFreeQ[ToString[#],{"Undo","Redo","GotoCommit","CommitInfo"}],Null,Commit];#)&];);

ManualCommit:=(AutoCo=False;NotebookEvaluate[Clear@$PreRead];);
  

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
     NotebookWrite[nb, Cell[BoxData[RowBox[{Commit}]], "Input"]];
     SelectionMove[nb, Previous, Cell];
     SelectionEvaluate[nb];
     SelectionMove[nb, Previous, Cell]; 
     NotebookDelete[nb];
     (*Save the Notebook after undo*)
     NotebookSave[]
],
    MenuKey["s", Modifiers -> {"Command"}],
    System`MenuEvaluator -> Automatic]}]];

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