unit MultiBuilder.Engine.Filters.DccCleanup;

interface

implementation

uses
  MultiBuilder.Engine.Filters.Intf;

type
  TMBCleanupFilter = class(TInterfacedObject, IMultiBuilderFilter)

  end;

{ exports }

function CreateMultiBuilderCleanupFilter: IMultiBuilderFilter;
begin
  Result := TMBCleanupFilter.Create;
end;

initialization
  FilterManager.RegisterFilter('DccCleanup', CreateMultiBuilderCleanupFilter);
end.
