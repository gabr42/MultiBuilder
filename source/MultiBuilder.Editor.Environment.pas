unit MultiBuilder.Editor.Environment;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ScrollBox,
  FMX.Memo, FMX.Controls.Presentation, FMX.StdCtrls;

type
  TfrmEditEnvironment = class(TForm)
    btnSave: TButton;
    btnDiscard: TButton;
    inpEnvironment: TMemo;
  strict protected
    function  GetEnvironment: string;
    procedure SetEnvironment(const value: string);
  public
    property Environment: string read GetEnvironment write SetEnvironment;
  end;

implementation

uses
  MultiBuilder.Main;

{$R *.fmx}

function TfrmEditEnvironment.GetEnvironment: string;
begin
  Result := inpEnvironment.Text;
end;

procedure TfrmEditEnvironment.SetEnvironment(const value: string);
begin
  inpEnvironment.Text := value;
end;

end.
