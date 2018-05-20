unit MultiBuilder.Engine.Intf;

interface

uses
  System.Generics.Collections;

type
  TExecuteResult = record
    Command : string;
    ExitCode: integer;
    Output  : string;
    constructor Create(const ACommand: string; AExitCode: integer; const AOutput: string);
  end;

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
    procedure RunSelected(const environment: string);
    property OnJobDone: TJobDoneEvent read GetOnJobDone write SetOnJobDone;
    property OnRunCompleted: TRunCompletedEvent read GetOnRunCompleted write
      SetOnRunCompleted;
  end;

implementation

constructor TExecuteResult.Create(const ACommand: string; AExitCode: integer;
  const AOutput: string);
begin
  Command := ACommand;
  ExitCode := AExitCode;
  Output := AOutput;
end;

end.