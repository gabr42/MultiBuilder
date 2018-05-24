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
    COptFailOnHint    = 'FailOnHint';
    COptFailOnWarning = 'FailOnWarning';
  type
    TOption = (optFailOnHint, optFailOnWarning);
    TOptions = set of TOption;
  var
    FForceFail: boolean;
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
    if (optFailOnHint in FOptions) and SameText(part1, 'Hint:') then
      FForceFail := true
    else if (optFailOnWarning in FOptions) and SameText(part1, 'Warning:') then
      FForceFail := true;
    Result := false
  end
  else
    Result := true;
end;

function TMBCleanupFilter.Process(const origResult: TExecuteResult): TExecuteResult;
var
  exitCode: integer;
  i       : integer;
  sl      : TStringList;
begin
  FForceFail := false;
  sl := TStringList.Create;
  try
    sl.Text := origResult.Output;
    for i := sl.Count - 1 downto 0 do
      if IsMeaninglessCompilerLine(sl[i]) then
        sl.Delete(i);
    exitCode := origResult.ExitCode;
    if (exitCode = 0) and FForceFail then begin
      sl.Add('*** DccCleanup filter prevented further execution');
      exitCode := 255;
    end;
    Result := TExecuteResult.Create(origResult.Command, exitCode, sl.Text);
  finally FreeAndNil(sl); end;
end;

procedure TMBCleanupFilter.ProcessParams(const params: TArray<string>);
var
  par: string;
begin
  FOptions := [];
  for par in params do
    if SameText(par, '-'+COptFailOnHint) or SameText(par, '/'+COptFailOnHint) then
      Include(FOptions, optFailOnHint)
    else if SameText(par, '-'+COptFailOnWarning) or SameText(par, '/'+COptFailOnWarning) then
      Include(FOptions, optFailOnWarning);
end;

initialization
  FilterManager.RegisterFilter('DccCleanup', CreateMultiBuilderCleanupFilter);
end.
