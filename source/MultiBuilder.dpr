program MultiBuilder;

uses
  System.StartUpCopy,
  FMX.Forms,
  MultiBuilder.Engine.Filters.DccCleanup,
  MultiBuilder.Main in 'MultiBuilder.Main.pas' {frmMultiBuilderMain},
  MultiBuilder.Editor.Environment in 'MultiBuilder.Editor.Environment.pas' {frmEditEnvironment},
  MultiBuilder.Editor.Project in 'MultiBuilder.Editor.Project.pas' {frmEditProject};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMultiBuilderMain, frmMultiBuilderMain);
  Application.Run;
end.
