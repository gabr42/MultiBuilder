program MultiBuilder;

uses
  System.StartUpCopy,
  FMX.Forms,
  MultiBuilder.Platform in 'MultiBuilder.Platform.pas',
  MultiBuilder.Engine in 'MultiBuilder.Engine.pas',
  MultiBuilder.Engine.Intf in 'MultiBuilder.Engine.Intf.pas',
  MultiBuilder.Main in 'MultiBuilder.Main.pas' {frmMultiBuilderMain},
  MultiBuilder.Editor.Environment in 'MultiBuilder.Editor.Environment.pas' {frmEditEnvironment},
  MultiBuilder.Editor.Project in 'MultiBuilder.Editor.Project.pas' {frmEditProject};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMultiBuilderMain, frmMultiBuilderMain);
  Application.Run;
end.
