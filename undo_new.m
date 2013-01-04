(* ::Package:: *)
(*TODO - menu  for auto manual and cron commit
       - button for Clean and warning
       - button for set limit
       - button for goto commit
*)


(*update undo file*)
undo`UpdateInfoFile[nb_,RecentVersion_, MaxVersion_, CommitList_,VersionLimit_,CommitMode_]:=Export[ToString[nb] <> ".undo.mx", {RecentVersion, MaxVersion, CommitList,VersionLimit,CommitMode}];

(*set variables*)
undo`SetLimit[nb_,n_]:=Module[{RecentVersion, MaxVersion, CommitList, VersionLimit, CommitMode},
			undo`CheckCommitFile[nb];
			If[FileExistsQ[ToString[nb] <> ".undo.mx"], 
			   {RecentVersion, MaxVersion, CommitList, VersionLimit, CommitMode} = Import[ToString[nb] <> ".undo.mx"];
			  ];
			VersionLimit=n;
			undo`UpdateInfoFile[NotebookFileName[SelectedNotebook[]],RecentVersion, MaxVersion, CommitList,VersionLimit,CommitMode]
		       ];
	    
(*Define copy and delete for different operating systmes*)
undo`OSCopy=If[$OperatingSystem == "Windows","copy","cp"];
undo`OSDelete=If[$OperatingSystem == "Windows","del","rm"];

(* check file save and shows a dialog if the file was not saved, but only when we are not in autocommit mode*)
undo`CheckCommitFile[nb_] := Module[{},
   If[! FileExistsQ[ToString[nb]],
     undo`WarningDialog;
     Abort[];];
   ];

(*Dialog Windows*)
undo`WarningDialog := CreateDialog[Column[{"You need to save the notebook, in order \n to use the commiting and undo system.", "", Item[DefaultButton[], Alignment -> Right]}]]

undo`NothingCommitedDialog[nb_]:=Module[{RecentVersion, MaxVersion, CommitList, VersionLimit, CommitMode}, 
					undo`CheckCommitFile[nb];
					If[FileExistsQ[ToString[nb] <> ".undo.mx"], 
					   {RecentVersion, MaxVersion, CommitList, VersionLimit, CommitMode} = Import[ToString[nb] <> ".undo.mx"];,
CommitMode="manulal";
					  ];
				  CreateDialog[Column[{"Nothing Commited", "", "Commit mode: " <> ToString[CommitMode],"",Item[DefaultButton[], Alignment -> Right]}]];
				 ];
			    
undo`InfoDialog[nb_]:=Module[{RecentVersion, MaxVersion, CommitList, VersionLimit, CommitMode}, 
			     undo`CheckCommitFile[nb];
			     If[FileExistsQ[ToString[nb] <> ".undo.mx"], 
				{RecentVersion, MaxVersion, CommitList, VersionLimit, CommitMode} = Import[ToString[nb] <> ".undo.mx"];
			       ];
			     CreateDialog[Column[{"Currently working on version: " <> ToString[RecentVersion], "", "Backed up versions limited to the last: " <>ToString[VersionLimit],"","Commit mode: " <> ToString[CommitMode], "", Row[{TableForm[Take[CommitList,Max[-VersionLimit,-Length[CommitList]]]]}], "", Item[DefaultButton[], Alignment -> Right]}]];
			    ];
			    
(*----Define Commit Info----*)
undo`CommitInfo[nb_] := Module[{RecentVersion, MaxVersion, CommitList, VersionLimit, CommitMode}, 
			       undo`CheckCommitFile[nb];
			       If[FileExistsQ[ToString[nb] <> ".undo.mx"], 
				  {RecentVersion, MaxVersion, CommitList, VersionLimit, CommitMode} = Import[ToString[nb] <> ".undo.mx"];
				 ];
			       If[! NumberQ[RecentVersion], undo`NothingCommitedDialog[nb],undo`InfoDialog[nb]];
			      ];

(*----Define Commit----*)
undo`Commit[nb_] := (Module[{RecentVersion, MaxVersion, CommitList, VersionLimit, CommitMode},
			    undo`CheckCommitFile[nb];
			    If[FileExistsQ[ToString[nb] <> ".undo.mx"], 
			       {RecentVersion, MaxVersion, CommitList, VersionLimit, CommitMode} = Import[ToString[nb] <> ".undo.mx"];
			      ];
			    If[! TrueQ[RecentVersion > 0],   
			       RecentVersion = 1;MaxVersion=0; CommitList = {}; VersionLimit = 1000; CommitMode = "manual";, RecentVersion=MaxVersion+1;
			      ]; (*check if parameters are set and add new version*)
			    MaxVersion = RecentVersion; (*update latest version number*)
			    AppendTo[CommitList, {MaxVersion, DateString[]}]; (*update commit list*)
			    (*Save the Notebook*)
			    NotebookSave[SelectedNotebook[], nb];
			    (*Create Backup Copy*)
			    Import["!"<>ToString[undo`OSCopy]<>" "<> ToString[nb] <> " " <> ToString[nb] <> ToString[Mod[RecentVersion,VersionLimit,1]] <> ".bak", "Table"];
			    undo`UpdateInfoFile[nb,RecentVersion, MaxVersion, CommitList,VersionLimit,CommitMode];
			   ];
		    );
(*----Define CommitClean-----*)
undo`Clean[nb_]:=(undo`CheckCommitFile[nb];
		  (***ADD ARE YOU SURE DIALOG***)
		  Import["!"<>ToString[undo`OSDelete]<>" " <> ToString[nb] <> "[0-9]*.bak","Table"];
		  Import["!"<>ToString[undo`OSDelete]<>" " <> ToString[nb] <> ".undo.mx","Table"];
		  Print["Commit: Clean"]
		 );

(*----------------Undo--------------*)
   undo`Undo[nbobj_] := Module[{nb=NotebookFileName[nbobj];},
			 undo`CheckCommitFile[nb];
			 (*If we are working with the recent version and changed it, make a commit*)
			 If[FileExistsQ[ToString[nb] <> ".undo.mx"],
			    If[Import[ToString[nb] <> ".undo.mx"][[1]]==Import[ToString[nb] <> ".undo.mx"][[2]]&&("ModifiedInMemory" /. NotebookInformation@nbobj),
			       undo`Commit[nb];
			      ];
			   ];
		       (*Actual Undo*)
		       Module[{RecentVersion, MaxVersion, CommitList, VersionLimit, CommitMode}, 
			      undo`CheckCommitFile[nb];
			      If[FileExistsQ[ToString[nb] <> ".undo.mx"], 
				 {RecentVersion, MaxVersion, CommitList, VersionLimit, CommitMode} = Import[ToString[nb] <> ".undo.mx"];
				];
			      Import["!"<>ToString[undo`OSCopy]<>" " <> ToString[nb] <> ToString[Mod[RecentVersion-1,undo`VersionLimit,1]] <> ".bak" <> " " <> ToString[nb], "Table"]; (*copy last backup on notebook file*)
			      If[RecentVersion>1,
				 FrontEndExecute[FrontEndToken["Revert",False]];(*false - show no warning*)
				 RecentVersion -= 1;
				];(*revert to that version*)
			      undo`UpdateInfoFile[nb,RecentVersion, MaxVersion, CommitList,VersionLimit,CommitMode];
			     ];
			    ];

(*-------Define Redo---------*)
   undo`Redo[nb_] := 
       Module[{RecentVersion, MaxVersion, CommitList, VersionLimit, CommitMode}, 
	      undo`CheckCommitFile[nb];
	      If[FileExistsQ[ToString[nb] <> ".undo.mx"], 
		 {RecentVersion, MaxVersion, CommitList, VersionLimit, CommitMode} = Import[ToString[nb] <> ".undo.mx"];
		];
	      If[RecentVersion < MaxVersion, 
		 RecentVersion += 1; 
		 Import["!"<>ToString[undo`OSCopy]<>" " <> ToString[nb] <> ToString[Mod[RecentVersion,VersionLimit,1]] <> ".bak" <> " " <> ToString[nb], "Table"]; 
		 FrontEndExecute[FrontEndToken["Revert",False]];
		];
	      undo`UpdateInfoFile[nb,RecentVersion, MaxVersion, CommitList,VersionLimit,CommitMode];
	     ];
	   
(*------------GotoCommit-----------*)
undo`GotoCommit[nb_,a_]:= Module[{RecentVersion, MaxVersion, CommitList, VersionLimit, CommitMode}, 
				 undo`CheckCommitFile[nb];
				 If[FileExistsQ[ToString[nb] <> ".undo.mx"], 
					   {RecentVersion, MaxVersion, CommitList, VersionLimit, CommitMode} = Import[ToString[nb] <> ".undo.mx"];
					  ];
			     If[a>=1&&a<=MaxVersion&&RecentVersion!=0,
				RecentVersion=a;
				Import["!"<>ToString[undo`OSCopy]<>" "<>ToString[nb]<>ToString[Mod[RecentVersion,VersionLimit,1]]<>".bak"<> " "<>ToString[nb],"Table"];
				FrontEndExecute[FrontEndToken["Revert",False]];,Print["Invalid Version"];
			       ];
	      undo`UpdateInfoFile[nb,RecentVersion, MaxVersion, CommitList,VersionLimit,CommitMode];
				];
			       
		
(*----- Auto and manual commit ----*)

undo`AutoCommit[nbobj_] := Module[{nb,RecentVersion, MaxVersion, CommitList, VersionLimit, CommitMode},
				  nb=NotebookFileName[nbobj];
				  undo`CheckCommitFile[nb];
				  If[FileExistsQ[ToString[nb] <> ".undo.mx"], 
				     {RecentVersion, MaxVersion, CommitList, VersionLimit, CommitMode} = Import[ToString[nb] <> ".undo.mx"];
				    ];
				  CommitMode="on evaluation";
				  Quiet[RemoveScheduledTask[undo`croncommit[nb]];];
				  NotebookEvaluate[$Pre=(If["ModifiedInMemory" /. NotebookInformation@SelectedNotebook[],undo`Commit[NotebookFileName[]]];#)&];
				  undo`UpdateInfoFile[nb,RecentVersion, MaxVersion, CommitList,VersionLimit,CommitMode];
				 ];

undo`ManualCommit[nb_] := Module[{RecentVersion, MaxVersion, CommitList, VersionLimit, CommitMode}, 
				 undo`CheckCommitFile[nb];
				 If[FileExistsQ[ToString[nb] <> ".undo.mx"], 
				    {RecentVersion, MaxVersion, CommitList, VersionLimit, CommitMode} = Import[ToString[nb] <> ".undo.mx"];
				   ];
				 CommitMode="manual";
				 Quiet[RemoveScheduledTask[undo`croncommit[nb]];];
				 NotebookEvaluate[Clear@$Pre];
				 undo`UpdateInfoFile[nb,RecentVersion, MaxVersion, CommitList,VersionLimit,CommitMode];
				];

undo`CronCommit[nb_, n_] := 
  Module[{RecentVersion, MaxVersion, CommitList, VersionLimit, 
    CommitMode}, undo`CheckCommitFile[nb];
   If[FileExistsQ[
     ToString[nb] <> 
      ".undo.mx"], {RecentVersion, MaxVersion, CommitList, 
       VersionLimit, CommitMode} = 
      Import[ToString[nb] <> ".undo.mx"];];
   CommitMode = "every " <> ToString[n] <> " min";
   NotebookEvaluate[Clear@$Pre];
   undo`croncommit[nb] = CreateScheduledTask[undo`Commit[nb], n*60];
   StartScheduledTask[undo`croncommit[nb]];
   undo`UpdateInfoFile[nb, RecentVersion, MaxVersion, CommitList, 
    VersionLimit, CommitMode];];

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
NotebookWrite[nb,Cell[BoxData[RowBox[{"undo`Commit[NotebookFileName[SelectedNotebook[]]]"}]],"Input"]];
SelectionMove[nb,Previous,Cell];
SelectionEvaluate[nb];
NotebookClose[nb];
],MenuKey["s",Modifiers->{"Command"}],System`MenuEvaluator->Automatic]}]];
