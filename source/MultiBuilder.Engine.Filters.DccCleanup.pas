unit MultiBuilder.Engine.Filters.DccCleanup;

interface

implementation

uses
  System.SysUtils, System.Classes,
  MultiBuilder.Engine.Filters.Intf, MultiBuilder.Engine.Intf;

type
  TMBCleanupFilter = class(TInterfacedObject, IMultiBuilderFilter)
  strict protected
    function  IsMeaninglessCompilerLine(const line: string): boolean;
  public
    function Process(const origResult: TExecuteResult): TExecuteResult;
  end;

{ exports }

function CreateMultiBuilderCleanupFilter: IMultiBuilderFilter;
begin
  Result := TMBCleanupFilter.Create;
end;

function TMBCleanupFilter.IsMeaninglessCompilerLine(const line: string): boolean;
var
  lineNum: integer;
  parts  : TArray<string>;
begin
  parts := Trim(line).Split(['(', ')']);
  if Length(parts) = 0 then
    Result := true
  else if Length(parts) = 1 then
    Result := false
  else if not TryStrToInt(parts[1], lineNum) then
    Result := false
  else if Length(parts) > 2 then
    Result := false
  else
    Result := true;
end;

function TMBCleanupFilter.Process(const origResult: TExecuteResult): TExecuteResult;
var
  i : integer;
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.Text := origResult.Output;
    for i := sl.Count - 1 downto 0 do
      if IsMeaninglessCompilerLine(sl[i]) then
        sl.Delete(i);
    Result := TExecuteResult.Create(origResult.Command, origResult.ExitCode, sl.Text);
  finally FreeAndNil(sl); end;
end;

initialization
  FilterManager.RegisterFilter('DccCleanup', CreateMultiBuilderCleanupFilter);
end.
