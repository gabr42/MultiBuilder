program MultiBuilder;

uses
  System.StartUpCopy,
  FMX.Forms,
  MultiBuilder.Main in 'MultiBuilder.Main.pas' {Form77};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm77, Form77);
  Application.Run;
end.
