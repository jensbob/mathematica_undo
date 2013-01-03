(* ::Package:: *)


undo`AutoCo="manual";
undo`VersionLimit=1000;
undo`SetVersionLimit[n_]:=(Undo`VersionLimit=n;);

(*Define copy and delete for different operating systmes*)
   undo`OSCopy=If[$OperatingSystem == "Windows","copy","cp"];
   undo`OSDelete=If[$OperatingSystem == "Windows","del","rm"];

Protect[undo`VersionLimit,undo`SetVersionLimit,undo`OSCopy,undo`OSDelete];

(* check file save and shows a dialog if the file was not saved, but only when we are not in autocommit mode*)
undo`CheckCommitFile[nb_] := 
    If[! TrueQ[Quiet[FileExistsQ[nb]]]&&undo`AutoCo!="on evaluation",
       CreateDialog[Column[{"You need to save the notebook, in order \n to use the commiting and undo system.", "", Item[DefaultButton[], Alignment -> Right]}]];
       Abort[];
      ];

Protect[undo`CheckCommitFile];

(*----Define Commit Info----*)
undo`CommitInfo[nb_] :=
    Module[{RecentVersion, MaxVersion, CommitList}, 
	   undo`CheckCommitFile[nb];
	   If[FileExistsQ[ToString[nb] <> ".undo.mx"], 
	      {RecentVersion, MaxVersion, CommitList} = 
	      Import[ToString[nb] <> ".undo.mx"];
	     ]; 
	   If[! NumberQ[RecentVersion], 
	      CreateDialog[
		  Column[{"Nothing Commited", "", "Commit mode: " <> ToString[undo`AutoCo],"",Item[DefaultButton[], Alignment -> Right]}]
			  ],
	      CreateDialog[
		  Column[{"Currently working on version: " <> ToString[RecentVersion], "", "Backed up versions limited to the last: " <>ToString[undo`VersionLimit],"","Commit mode: " <> ToString[undo`AutoCo], "", Row[{TableForm[Take[CommitList,Max[-undo`VersionLimit,-Length[CommitList]]]]}], "", Item[DefaultButton[], Alignment -> Right]}] (*open dialog with a list of the last 30 versions*)
			  ];
	     ];
	  ];

Protect[undo`CommitInfo]


(*----Define Commit----*)
   undo`CommitNow[nb_]:= undo`Commit[nb];

   undo`Commit[nb_] := (       
       Module[{RecentVersion, MaxVersion, CommitList},
	      undo`CheckCommitFile[nb];
	      If[FileExistsQ[ToString[nb] <> ".undo.mx"],
		 {RecentVersion, MaxVersion, CommitList} = Import[ToString[nb] <> ".undo.mx"];
		]; (*If there is an undo file load the parameters*)

	      If[!NumberQ[RecentVersion],   
		 RecentVersion = 1;MaxVersion=0; CommitList = {}, RecentVersion=MaxVersion+1;
		]; (*check if parameters are set and add new version*)

	      MaxVersion = RecentVersion; (*update latest version number*)
	      AppendTo[CommitList, {MaxVersion, DateString[]}]; (*update commit list*)
	     	      (*Save the Notebook*)
	      NotebookSave[SelectedNotebook[], nb];
	      (*Create Backup Copy*)
	      Import["!"<>ToString[undo`OSCopy]<>" "<> ToString[nb] <> " " <> ToString[nb] <> ToString[Mod[RecentVersion,undo`VersionLimit,1]] <> ".bak", "Table"];
	      (*update undo file*)
	      Export[ToString[nb] <> ".undo.mx", {RecentVersion, MaxVersion, CommitList}];
	     ];
	     );

Protect[Commit,CommitNow]

(*----Define CommitClean-----*)
   (*Removes all backups*)
   undo`Clean :=With[{nb=Quiet[NotebookFileName[]]},
	      undo`CheckCommitFile[nb];
	      Import["!"<>ToString[undo`OSDelete]<>" " <> ToString[nb] <> "[0-9]*.bak", "Table"];
	      Import["!"<>ToString[undo`OSDelete]<>" " <> ToString[nb] <> ".undo.mx", "Table"];
	      Print["Commit: Clean"]
		    ];

Protect[undo`Clean]


(*----------------Undo--------------*)
   undo`Undo[nb2_] := ( 
       nb=NotebookFileName[nb2];
       undo`CheckCommitFile[nb];
       (*If we are working with the recent version and changed it, make a commit*)
       If[FileExistsQ[ToString[nb] <> ".undo.mx"],
	  If[Import[ToString[nb] <> ".undo.mx"][[1]]==Import[ToString[nb] <> ".undo.mx"][[2]]&&("ModifiedInMemory" /. NotebookInformation@nb2),
	     undo`Commit[nb];
	    ];
	 ];
       (*Actual Undo*)
       Module[{RecentVersion, MaxVersion, CommitList}, 
	      If[FileExistsQ[ToString[nb] <> ".undo.mx"], 
		 {RecentVersion, MaxVersion, CommitList} = Import[ToString[nb] <> ".undo.mx"];
		]; 

	      Import["!"<>ToString[undo`OSCopy]<>" " <> ToString[nb] <> ToString[Mod[RecentVersion-1,undo`VersionLimit,1]] <> ".bak" <> " " <> ToString[nb], "Table"]; (*copy last backup on notebook file*)
	      If[RecentVersion>1,
		 FrontEndExecute[FrontEndToken["Revert",False]];(*false - show no warning*)
		 RecentVersion -= 1;
		];(*revert to that version*)
	      
	      Export[ToString[nb] <> ".undo.mx", {RecentVersion, MaxVersion, CommitList}]; 
	     ];
	   );

(*-------Define Redo---------*)
   undo`Redo[nb_] := 
       Module[{RecentVersion, MaxVersion, CommitList}, 
	      undo`CheckCommitFile[nb]; 
	      If[FileExistsQ[ToString[nb] <> ".undo.mx"], 
		 {RecentVersion, MaxVersion, CommitList} = Import[ToString[nb] <> ".undo.mx"];
		]; 
	      If[RecentVersion < MaxVersion, 
		 RecentVersion += 1; 
		 Import["!"<>ToString[undo`OSCopy]<>" " <> ToString[nb] <> ToString[Mod[RecentVersion,undo`VersionLimit,1]] <> ".bak" <> " " <> ToString[nb], "Table"]; 
		 FrontEndExecute[FrontEndToken["Revert",False]];
		];
	      Export[ToString[nb] <> ".undo.mx", {RecentVersion, MaxVersion, CommitList}];
	     ];
	   
Protect[undo`Undo,undo`Redo];

	   

(*------------GotoCommit-----------*)
   undo`GotoCommit[a_]:=
       Module[{nb = Quiet[NotebookFileName[]], RecentVersion, MaxVersion,CommitList},
	      undo`CheckCommitFile[nb];
	      If[FileExistsQ[ToString[nb] <> ".undo.mx"], 
		 {RecentVersion, MaxVersion, CommitList} = Import[ToString[nb] <> ".undo.mx"];
		]; 
	      If[a>=1&&a<=MaxVersion&&RecentVersion!=0,
		 RecentVersion=a;
		 Import["!"<>ToString[undo`OSCopy]<>" "<>ToString[nb]<>ToString[Mod[RecentVersion,undo`VersionLimit,1]]<>".bak"<> " "<>ToString[nb],"Table"];
		 FrontEndExecute[FrontEndToken["Revert",False]];,Print["Invalid Version"]
		];
	     ] ;
		
Protect[undo`GotoCommit];
(*----- Auto and manual commit ----*)

   undo`AutoCommit := Module[{nb = Quiet[NotebookFileName[]]},
       undo`CheckCommitFile[nb];
       undo`AutoCo="on evaluation";
       Quiet[RemoveScheduledTask[croncommit[NotebookFileName[]]];];
       NotebookEvaluate[$PreRead=(If["ModifiedInMemory" /. NotebookInformation@SelectedNotebook[],undo`CommitNow[NotebookFileName[]]];#)&];];

   undo`ManualCommit := Module[{nb = Quiet[NotebookFileName[]]},
       undo`AutoCo="manual";
       undo`CheckCommitFile[nb];
       Quiet[RemoveScheduledTask[croncommit[nb]];];
       NotebookEvaluate[Clear@$PreRead];];

   undo`CronCommit[n_] := With[{nb = Quiet[NotebookFileName[]]},undo`AutoCo = "every " <> ToString[n] <> " min";
       undo`CheckCommitFile[nb];
		      croncommit[nb] = 
		      CreateScheduledTask[undo`Commit[nb], n*60];
		      StartScheduledTask[croncommit[nb]];
			 ];

Protect[undo`AutoCommit,undo`ManualCommit,undo`CronCommit];
(*Keyboard Shortcuts*)

FrontEndExecute[FrontEnd`AddMenuCommands["SelectAll",{Delimiter,MenuItem["Delete Outer Brackets",FrontEnd`KernelExecute[nb=CreateDocument[Null,WindowSelected->False,Visible->False];
NotebookWrite[nb,Cell[BoxData[RowBox[{"undo`CommitInfo[NotebookFileName[SelectedNotebook[]]]"}]],"Input"]];
SelectionMove[nb,Previous,Cell];
SelectionEvaluate[nb];
NotebookClose[nb];
],MenuKey["d",Modifiers->{"Command"}],System`MenuEvaluator->Automatic]}]];


FrontEndExecute[FrontEnd`AddMenuCommands["Undo",{Delimiter,MenuItem["Redo Commit",FrontEnd`KernelExecute[nb=CreateDocument[Null,WindowSelected->False,Visible->False];
NotebookWrite[nb,Cell[BoxData[RowBox[{"undo`Redo[NotebookFileName[SelectedNotebook[]]]"}]],"Input"]];
SelectionMove[nb,Previous,Cell];
SelectionEvaluate[nb];
NotebookClose[nb];
],MenuKey["x",Modifiers->{"Command"}],System`MenuEvaluator->Automatic]}]];


FrontEndExecute[FrontEnd`AddMenuCommands["Undo",{Delimiter,MenuItem["Undo Commit",FrontEnd`KernelExecute[nb=CreateDocument[Null,WindowSelected->False,Visible->False];
NotebookWrite[nb,Cell[BoxData[RowBox[{"undo`Undo[SelectedNotebook[]]"}]],"Input"]];
SelectionMove[nb,Previous,Cell];
SelectionEvaluate[nb];
NotebookClose[nb];
],MenuKey["z",Modifiers->{"Command"}],System`MenuEvaluator->Automatic]}]];


FrontEndExecute[FrontEnd`AddMenuCommands["Undo",{Delimiter,MenuItem["Commit",FrontEnd`KernelExecute[nb=CreateDocument[Null,WindowSelected->False,Visible->False];
NotebookWrite[nb,Cell[BoxData[RowBox[{"undo`CommitNow[NotebookFileName[SelectedNotebook[]]]"}]],"Input"]];
SelectionMove[nb,Previous,Cell];
SelectionEvaluate[nb];
NotebookClose[nb];
],MenuKey["s",Modifiers->{"Command"}],System`MenuEvaluator->Automatic]}]];
