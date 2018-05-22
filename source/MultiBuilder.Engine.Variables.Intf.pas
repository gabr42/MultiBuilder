unit MultiBuilder.Engine.Variables.Intf;

interface

type
  IMultiBuilderVariables = interface ['{F3509B10-7FC1-421E-AFCB-98C465C47AD4}']
    procedure Add(const section, name, value: string);
    procedure BeginUpdate;
    procedure Clear;
    procedure EndUpdate;
    function  Evaluate(const environment, name: string): string;
  end;

implementation

end.
