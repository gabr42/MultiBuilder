//Thread-safe variable management and macro replacement

unit MultiBuilder.Engine.Variables;

interface

uses
  MultiBuilder.Engine.Variables.Intf;

function  CreateMBVariables(const globalSection, defaultSection,
  environmentMacro: string): IMultiBuilderVariables;

implementation

uses
  System.SysUtils, System.SyncObjs, System.Classes,
  System.Generics.Collections;

type
  TMBVariables = class(TInterfacedObject, IMultiBuilderVariables)
  strict private
  const
    FS = #28;
  var
    FDefaultSection: string;
    FEnvMacro      : string;
    FGlobalSection : string;
    FLockedBy      : int64;
    FVariables     : TDictionary<string,string>;
  strict protected
    function  CurrentThreadID: int64;
  public
    constructor Create(const globalSection, defaultSection, environmentMacro: string);
    destructor  Destroy; override;
    procedure BeginUpdate;
    procedure Add(const section, name, value: string);
    procedure Clear;
    procedure EndUpdate;
    function  Evaluate(const environment, name: string): string;
  end;

{ exports }

function CreateMBVariables(const globalSection, defaultSection,
  environmentMacro: string): IMultiBuilderVariables;
begin
  Result := TMBVariables.Create(globalSection, defaultSection, environmentMacro);
end;

{ TMBVariables }

constructor TMBVariables.Create(const globalSection, defaultSection,
  environmentMacro: string);
begin
  inherited Create;
  FGlobalSection := globalSection;
  FDefaultSection := defaultSection;
  FEnvMacro := environmentMacro;
  FVariables := TDictionary<string,string>.Create;
end;

destructor TMBVariables.Destroy;
begin
  FreeAndNil(FVariables);
  inherited;
end;

procedure TMBVariables.Add(const section, name, value: string);
begin
  if TInterlocked.CompareExchange(FLockedBy, 0, 0) <> CurrentThreadID then
    raise Exception.Create('Not locked by the current thread!');
  FVariables.Add(section + FS + name, value);
end;

procedure TMBVariables.BeginUpdate;
begin
  MonitorEnter(Self);
  if FLockedBy <> 0 then
    raise Exception.Create('Already in update mode!');
  TInterlocked.Exchange(FLockedBy, CurrentThreadID);
end;

procedure TMBVariables.Clear;
begin
  if TInterlocked.CompareExchange(FLockedBy, 0, 0) <> CurrentThreadID then
    raise Exception.Create('Not locked by the current thread!');
  FVariables.Clear;
end;

function TMBVariables.CurrentThreadID: int64;
begin
  Result := int64(TThread.Current.ThreadID);
end;

procedure TMBVariables.EndUpdate;
begin
  if TInterlocked.CompareExchange(FLockedBy, 0, 0) <> CurrentThreadID then
    raise Exception.Create('Not locked by the current thread!');
  TInterlocked.Exchange(FLockedBy, 0);
  MonitorExit(Self);
end;

function TMBVariables.Evaluate(const environment, name: string): string;
begin
  MonitorEnter(Self);
  try
    if SameText(name, FEnvMacro) then
      Result := environment
    else if (not FVariables.TryGetValue(environment + FS + name, Result))
             and (not FVariables.TryGetValue(FDefaultSection + FS + name, Result))
             and (not FVariables.TryGetValue(FGlobalSection + FS + name, Result))
    then
      Result := '';
  finally MonitorExit(Self); end;
end;

end.


