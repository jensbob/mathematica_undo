(* ::Package:: *)

(*-------Commit---------------*)
Commit := 
  Module[{nb = NotebookFileName[], RecentVersion, MaxVersion, 
    CommitList},

   If[FileExistsQ[ToString[nb] <> ".undo.mx"],
    {RecentVersion, MaxVersion, CommitList} = 
      Import[ToString[nb] <> ".undo.mx"];];

   If[RecentVersion == 0 || !NumberQ[RecentVersion], 
    RecentVersion = 1;MaxVersion=0; CommitList = {}, RecentVersion++;];

If[MaxVersion+1==RecentVersion,
   MaxVersion = RecentVersion;
   AppendTo[CommitList, {MaxVersion, DateString[]}];
   (*Save the Notebook*)NotebookSave[EvaluationNotebook[], nb];
   (*Create Backup Copy*)
   Import["!cp " <> ToString[nb] <> " " <> ToString[nb] <> 
     ToString[RecentVersion] <> ".bak", "Table"];
   
     Export[
     ToString[nb] <> ".undo.mx", {RecentVersion, MaxVersion, 
      CommitList}];
  ,Print["[WARNING] You are working on an old version. Use CommitForce to commit anyways and delete all newer commits. Autocommit does not work until you switched to the newest verison or used CommitForce"]; RecentVersion--;];
   Print["Version ", RecentVersion]
	];
       



CommitForce := 
  Module[{nb = NotebookFileName[], RecentVersion, MaxVersion, 
    CommitList},
   If[FileExistsQ[ToString[nb] <> ".undo.mx"],
    {RecentVersion, MaxVersion, CommitList} = 
      Import[ToString[nb] <> ".undo.mx"];];
   If[RecentVersion == 0 || ! NumberQ[RecentVersion], 
    RecentVersion = 1; CommitList = {}, RecentVersion++;

    Do[Import["!rm " <> ToString[nb] <> ToString[i] <> ".bak", 
       "Table"];, {i, RecentVersion + 1, MaxVersion}];
    CommitList = 
     DeleteCases[CommitList, {first_, _} /; first >= RecentVersion]];
   MaxVersion = RecentVersion;
   AppendTo[CommitList, {MaxVersion, DateString[]}];
   (*Save the Notebook*)NotebookSave[EvaluationNotebook[], nb];
   (*Create Backup Copy*)
   Import["!cp " <> ToString[nb] <> " " <> ToString[nb] <> 
     ToString[RecentVersion] <> ".bak", "Table"];
   
     Export[
     ToString[nb] <> ".undo.mx", {RecentVersion, MaxVersion, 
      CommitList}];
   
   Print["Version: ", RecentVersion]
	];

(*-------Commit Info------------*)
CommitInfo := 
 Module[{nb = NotebookFileName[], RecentVersion, MaxVersion, 
   CommitList},
  If[FileExistsQ[ToString[nb] <> ".undo.mx"],
   {RecentVersion, MaxVersion, CommitList} = 
     Import[ToString[nb] <> ".undo.mx"];];
  If[RecentVersion == 0 || ! NumberQ[RecentVersion], 
   Print["CommitInfo: Nothing commited"], 
   Print["CommitInfo: Working on version: ", RecentVersion]; 
   Print[TableForm[CommitList]]];
  Print["Auto Commit Status: ", TrueQ[AutoCo]]]

(*----------CommitClean---------*)
(*Removes all backups*)
CommitClean := Module[{nb = NotebookFileName[]},
   Import["!rm " <> ToString[nb] <> "[0-9]*.bak", "Table"];
   Import["!rm " <> ToString[nb] <> ".undo.mx", "Table"];
   Print["Commit: Clean"]];

(*----------------Undo--------------*)

Undo := Module[{nb = NotebookFileName[], RecentVersion, MaxVersion, CommitList}, 
       If[FileExistsQ[ToString[nb] <> ".undo.mx"], 
	  {RecentVersion, MaxVersion, CommitList} = Import[ToString[nb] <> ".undo.mx"];]; 
	   Import["!cp " <> ToString[nb] <> ToString[RecentVersion] <> ".bak" <> " " <> ToString[nb], "Table"]; 
           FrontEndExecute[FrontEndToken["Revert"]]; 
           RecentVersion -= 1;
	   If[RecentVersion<1,RecentVersion = 1; Print["Noting to undo"]]
           Export[ToString[nb] <> ".undo.mx", {RecentVersion, MaxVersion, 
    CommitList}]; 
           Print["Version: ", RecentVersion]];

(*-------------REDO-------------------_*)

Redo := Module[{nb = NotebookFileName[], RecentVersion, MaxVersion, 
   CommitList}, 
  If[FileExistsQ[
    ToString[nb] <> 
     ".undo.mx"], {RecentVersion, MaxVersion, CommitList} = 
     Import[ToString[nb] <> ".undo.mx"];]; 
  If[RecentVersion < MaxVersion, RecentVersion += 1; 
   Import["!cp " <> ToString[nb] <> ToString[RecentVersion] <> 
     ".bak" <> " " <> ToString[nb], "Table"]; 
   FrontEndExecute[FrontEndToken["Revert"]];,Print["This is the newest version"]]; 
  Export[ToString[nb] <> ".undo.mx", {RecentVersion, MaxVersion, 
    CommitList}]; Print["Version: ", RecentVersion]];


GotoCommit[a_]:=Module[{nb = NotebookFileName[], RecentVersion, MaxVersion, 
   CommitList}, 
  If[FileExistsQ[
    ToString[nb] <> 
     ".undo.mx"], {RecentVersion, MaxVersion, CommitList} = 
     Import[ToString[nb] <> ".undo.mx"];]; 
If[a>=1&&a<=MaxVersion&&RecentVersion!=0,
If[RecentVersion==MaxVersion,Commit;];
RecentVersion=a;
Import["!cp "<>ToString[nb]<>ToString[RecentVersion]<>".bak"<> " "<>ToString[nb],"Table"];
FrontEndExecute[FrontEndToken["Revert"]];,Print["Invalid Version"]];
Print["Version: ",RecentVersion]
		      ] 


(****AUTOCOMMIT******)

AutoCo=False;(*Flag for auto commit*)

AutoCommit:=(AutoCo=True;NotebookEvaluate[
$PreRead=(If[!StringFreeQ[ToString[#],{"Undo","Redo","GotoCommit","CommitInfo"}],Null,Commit];#)&];);
ManualCommit:=(AutoCo=False;
NotebookEvaluate[Clear@$PreRead];)



(** Keyboard  Shortcuts ***)

FrontEndExecute[
 FrontEnd`AddMenuCommands["DuplicatePreviousOutput",
  {Delimiter, MenuItem["Undo Commit",
    FrontEnd`KernelExecute[
     nb = SelectedNotebook[];
     SelectionMove[nb, After, Cell]; 
     NotebookWrite[nb, Cell[BoxData[RowBox[{"Undo"}]], "Input"]];
     SelectionMove[nb, Previous, Cell];
     SelectionEvaluate[nb]],
    MenuKey["z", Modifiers -> {"Command"}],
    System`MenuEvaluator -> Automatic]}]]

FrontEndExecute[
 FrontEnd`AddMenuCommands["DuplicatePreviousOutput",
  {Delimiter, MenuItem["Redo Commit",
    FrontEnd`KernelExecute[
     nb = SelectedNotebook[];
     SelectionMove[nb, After, Cell]; 
     NotebookWrite[nb, Cell[BoxData[RowBox[{"Redo"}]], "Input"]];
     SelectionMove[nb, Previous, Cell];
     SelectionEvaluate[nb]],
    MenuKey["x", Modifiers -> {"Command"}],
    System`MenuEvaluator -> Automatic]}]]

FrontEndExecute[
 FrontEnd`AddMenuCommands["DuplicatePreviousOutput",
  {Delimiter, MenuItem["Make a commit",
    FrontEnd`KernelExecute[
     nb = SelectedNotebook[];
     SelectionMove[nb, After, Cell]; 
     NotebookWrite[nb, Cell[BoxData[RowBox[{"Commit"}]], "Input"]];
     SelectionMove[nb, Previous, Cell];
     SelectionEvaluate[nb]],
    MenuKey["s", Modifiers -> {"Command"}],
    System`MenuEvaluator -> Automatic]}]]

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