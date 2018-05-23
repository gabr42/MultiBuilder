unit MultiBuilder.Engine.Filters.DccCleanup;

interface

implementation

uses
  System.SysUtils, System.Classes, System.Math,
  MultiBuilder.Engine.Filters.Intf, MultiBuilder.Engine.Intf;

type
  TMBCleanupFilter = class(TInterfacedObject, IMultiBuilderFilter)
  strict private
  const
    COptErrorOnHint    = 'ErrorOnHint';
    COptErrorOnWarning = 'ErrorOnWarning';
  type
    TOption = (optErrorOnHint, optErrorOnWarning);
    TOptions = set of TOption;
  var
    FForceError: boolean;
    FOptions   : TOptions;
  strict protected
    function  IsMeaninglessCompilerLine(const line: string): boolean;
    procedure ProcessParams(const params: TArray<string>);
  public
    constructor Create(const params: string);
    function Process(const origResult: TExecuteResult): TExecuteResult;
  end;

{ exports }

function CreateMultiBuilderCleanupFilter(params: string): IMultiBuilderFilter;
begin
  Result := TMBCleanupFilter.Create(params);
end;

{ TMBCleanupFilter }

constructor TMBCleanupFilter.Create(const params: string);
begin
  inherited Create;
  ProcessParams(params.Split([' ']));
end;

function TMBCleanupFilter.IsMeaninglessCompilerLine(const line: string): boolean;
var
  lineNum: integer;
  part1  : string;
  parts  : TArray<string>;
begin
  parts := Trim(line).Split(['(', ')']);
  if Length(parts) = 0 then
    Result := true
  else if Length(parts) = 1 then
    Result := false
  else if not TryStrToInt(parts[1], lineNum) then
    Result := false
  else if Length(parts) > 2 then begin
    part1 := Trim(parts[2]).Split([' '])[0];
    if (optErrorOnHint in FOptions) and SameText(part1, 'Hint:') then
      FForceError := true
    else if (optErrorOnWarning in FOptions) and SameText(part1, 'Warning:') then
      FForceError := true;
    Result := false
  end
  else
    Result := true;
end;

function TMBCleanupFilter.Process(const origResult: TExecuteResult): TExecuteResult;
var
  i : integer;
  sl: TStringList;
begin
  FForceError := false;
  sl := TStringList.Create;
  try
    sl.Text := origResult.Output;
    for i := sl.Count - 1 downto 0 do
      if IsMeaninglessCompilerLine(sl[i]) then
        sl.Delete(i);
    Result := TExecuteResult.Create(origResult.Command,
                IfThen((origResult.ExitCode <> 0) or (not FForceError), origResult.ExitCode, 255),
                sl.Text);
  finally FreeAndNil(sl); end;
end;

procedure TMBCleanupFilter.ProcessParams(const params: TArray<string>);
var
  par: string;
begin
  FOptions := [];
  for par in params do
    if SameText(par, '-'+COptErrorOnHint) or SameText(par, '/'+COptErrorOnHint) then
      Include(FOptions, optErrorOnHint)
    else if SameText(par, '-'+COptErrorOnWarning) or SameText(par, '/'+COptErrorOnWarning) then
      Include(FOptions, optErrorOnWarning);
end;

initialization
  FilterManager.RegisterFilter('DccCleanup', CreateMultiBuilderCleanupFilter);
end.
