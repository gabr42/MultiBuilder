unit MultiBuilder.Engine.Filters; 
//
interface

uses
  MultiBuilder.Engine.Filters.Intf;

function CreateFilter(const filterName: string): IMultiBuilderFilter;

implementation

uses
  MultiBuilder.Engine.Filters.DccCleanup;

function CreateFilter(const filterName: string): IMultiBuilderFilter;
begin
  // TODO -cMM: CreateFilter default body inserted
  Result := ;
end;

end.
