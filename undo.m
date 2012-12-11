(* ::Package:: *)

Commit:=(
nb=NotebookFileName[]; (*Notebook Name*)

(*Check if  first commit*)
If[RecentVersion[nb]==0||!NumberQ[RecentVersion[nb]],
RecentVersion[nb]=1; CommitList={},
RecentVersion[nb]++;

(*Remove higher versions after Undo on a new commit*)
Do[Import["!rm "<>ToString[nb]<>ToString[i]<>".bak","Table"];,{i,RecentVersion[nb]+1,MaxVersion[nb]}];

(*Clean up CommitList*)
CommitList=DeleteCases[CommitList,{first_,_}/;first>=RecentVersion[nb]]
];
(*Set RecentVersion as Max and update CommitList*)
MaxVersion[nb]=RecentVersion[nb];
AppendTo[CommitList,{MaxVersion[nb],DateString[]}];

(*Save the Notebook*)
NotebookSave[EvaluationNotebook[],nb];

(*Create Backup Copy*)
Import["!cp "<>ToString[nb]<> " "<>ToString[nb]<>ToString[RecentVersion[nb]]<>".bak","Table"];
);

(****** INFO AND CLEANING OF COMMITS ****)

(*Short Versions Info*)
CommitInfo:=(
nb=NotebookFileName[];
If[RecentVersion[nb]==0||!NumberQ[RecentVersion[nb]],
Print["CommitInfo: Nothing commited"],
Print["CommitInfo: Working on version: ",RecentVersion[nb]];TableForm[CommitList]];
Print["Auto Commit Status: ",AutoCo])

(*Removes all backups*)
CommitClean:=(
nb=NotebookFileName[];
Import["!rm "<>ToString[nb]<>"[0-9]*.bak","Table"];
RecentVersion[nb]=0;
CommitList={};
Print["Commit: Clean"]
);


(*****UNDO, REDO AND GOTO SPEC. VERSION*****)
Undo:=(
nb=NotebookFileName[];

If[RecentVersion[nb]>1,(*only if we have something to undo*)

If[!AutoCo&&RecentVersion[nb]==MaxVersion[nb],Commit;];(*If we are on the latest version, make a commit if auto commit is off - this does not check if the commit is really needed but otherwise it is possible to undo uncommited work *)

(*Load Previous Version*)
RecentVersion[nb]-=1;
Import["!cp "<>ToString[nb]<>ToString[RecentVersion[nb]]<>".bak"<> " "<>ToString[nb],"Table"];
FrontEndExecute[FrontEndToken["Revert"]];];
Print["Version: ",RecentVersion[nb]])

Redo:=(
nb=NotebookFileName[];If[RecentVersion[nb]<MaxVersion[nb],RecentVersion[nb]+=1;Import["!cp "<>ToString[nb]<>ToString[RecentVersion[nb]]<>".bak"<> " "<>ToString[nb],"Table"];FrontEndExecute[FrontEndToken["Revert"]];];
Print["Version: ",RecentVersion[nb]])


GotoCommit[a_]:=(
nb=NotebookFileName[];
If[a>=1&&a<=MaxVersion[nb]&&RecentVersion[nb]!=0,
If[RecentVersion[nb]==MaxVersion[nb],Commit;];
RecentVersion[nb]=a;
Import["!cp "<>ToString[nb]<>ToString[RecentVersion[nb]]<>".bak"<> " "<>ToString[nb],"Table"];
FrontEndExecute[FrontEndToken["Revert"]];];
Print["Version: ",RecentVersion[nb]]

)


(****AUTOCOMMIT******)

AutoCo=False;(*Flag for auto commit*)

AutoCommit:=(AutoCo=True;NotebookEvaluate[
$PreRead=(If[!StringFreeQ[ToString[#],{"Undo","Redo","GotoCommit","CommitInfo"}],Null,Commit];#)&];);
ManualCommit:=(AutoCo=False;
NotebookEvaluate[Clear@$PreRead];)


(****ALSO SAVE THE MEMORY STATE***)
MemCommit=False;(*Flag*)



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