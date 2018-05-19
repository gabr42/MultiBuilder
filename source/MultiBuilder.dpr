program MultiBuilder;

uses
  System.StartUpCopy,
  FMX.Forms,
  MultiBuilder.Main in 'MultiBuilder.Main.pas' {frmMultiBuilderMain},
  MultiBuilder.Platform in 'MultiBuilder.Platform.pas',
  MultiBuilder.Editor.Environment in 'MultiBuilder.Editor.Environment.pas' {frmEditEnvironment},
  MultiBuilder.Engine in 'MultiBuilder.Engine.pas',
  MultiBuilder.Engine.Intf in 'MultiBuilder.Engine.Intf.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMultiBuilderMain, frmMultiBuilderMain);
  Application.Run;
end.
