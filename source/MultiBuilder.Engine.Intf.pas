unit MultiBuilder.Engine.Intf;

interface

uses
  System.Generics.Collections;

type
  TExecuteResult = TPair<integer,string>; //exit code, program output

  TJobDoneEvent = reference to procedure (const environment: string; const result: TExecuteResult);
  TRunCompletedEvent = reference to procedure;

  IMultiBuilderEngine = interface ['{CAEEE225-C0B7-4C23-9575-93B06A4957A8}']
    function  GetOnJobDone: TJobDoneEvent;
    function  GetOnRunCompleted: TRunCompletedEvent;
    procedure SetOnJobDone(const value: TJobDoneEvent);
    procedure SetOnRunCompleted(const Value: TRunCompletedEvent);
  //
    procedure ClearProject;
    function Environments: TArray<string>;
    function LoadFrom(const iniFile: string): boolean;
    function LoadProject(const projFile: string): boolean;
    procedure Run;
    property OnJobDone: TJobDoneEvent read GetOnJobDone write SetOnJobDone;
    property OnRunCompleted: TRunCompletedEvent read GetOnRunCompleted write
      SetOnRunCompleted;
  end;

implementation

end.
