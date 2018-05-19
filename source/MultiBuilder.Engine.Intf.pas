unit MultiBuilder.Engine.Intf;

interface

type
  IMultiBuilderEngine = interface ['{CAEEE225-C0B7-4C23-9575-93B06A4957A8}']
    procedure ClearProject;
    function Environments: TArray<string>;
    function LoadFrom(const iniFile: string): boolean;
    function LoadProject(const projFile: string): boolean;
  end;

implementation

end.
