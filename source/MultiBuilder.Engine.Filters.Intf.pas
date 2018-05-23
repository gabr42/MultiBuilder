unit MultiBuilder.Engine.Filters.Intf;

interface

uses
  System.SysUtils,
  MultiBuilder.Engine.Intf;

type
  IMultiBuilderFilter = interface ['{993D98BE-5C92-4764-A9B7-1537554C57AA}']
    function Process(const origResult: TExecuteResult): TExecuteResult;
  end;

  TMultiBuilderFilterFactory = TFunc<string, IMultiBuilderFilter>;

  IMultiBuilderFilterManager = interface ['{44D46644-B49C-46BE-9871-2382F45A605D}']
    function  CreateNewInstance(const filterName, filterParams: string): IMultiBuilderFilter;
    procedure RegisterFilter(const filterName: string; const filterFactory: TMultiBuilderFilterFactory);
  end;

var
  FilterManager: IMultiBuilderFilterManager; //thread-safe

implementation

uses
  System.Generics.Defaults, System.Generics.Collections;

type
  TMultiBuilderFilterManager = class(TInterfacedObject, IMultiBuilderFilterManager)
  strict private
    FFactory: TDictionary<string, TMultiBuilderFilterFactory>;
  public
    constructor Create;
    destructor  Destroy; override;
    function  CreateNewInstance(const filterName, filterParams: string):
      IMultiBuilderFilter;
    procedure RegisterFilter(const filterName: string;
      const filterFactory: TMultiBuilderFilterFactory);
  end;

{ TMultiBuilderFilterManager }

constructor TMultiBuilderFilterManager.Create;
begin
  inherited;
  FFactory := TDictionary<string, TMultiBuilderFilterFactory>.Create(
                TIStringComparer.Ordinal);
end;

destructor TMultiBuilderFilterManager.Destroy;
begin
  FreeAndNil(FFactory);
  inherited;
end;

function TMultiBuilderFilterManager.CreateNewInstance(const filterName,
  filterParams: string): IMultiBuilderFilter;
var
  filterFactory: TMultiBuilderFilterFactory;
begin
  MonitorEnter(FFactory);
  try
    if FFactory.TryGetValue(filterName, filterFactory) then
      Result := filterFactory(filterParams);
  finally MonitorExit(FFactory); end;
end;

procedure TMultiBuilderFilterManager.RegisterFilter(const filterName: string;
  const filterFactory: TMultiBuilderFilterFactory);
begin
  MonitorEnter(FFactory);
  try
    FFactory.Add(filterName, filterFactory);
  finally MonitorExit(FFactory); end;
end;

initialization
  FilterManager := TMultiBuilderFilterManager.Create;
end.
