unit MultiBuilder.Editor.Project;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ScrollBox,
  FMX.Memo, FMX.Controls.Presentation, FMX.StdCtrls;

type
  TfrmEditProject = class(TForm)
    btnSave: TButton;
    btnDiscard: TButton;
    inpProject: TMemo;
  strict protected
    function  GetProject: string;
    procedure SetProject(const value: string);
  public
    property Project: string read GetProject write SetProject;
  end;

implementation

uses
  MultiBuilder.Main;

{$R *.fmx}

function TfrmEditProject.GetProject: string;
begin
  Result := inpProject.Text;
end;

procedure TfrmEditProject.SetProject(const value: string);
begin
  inpProject.Text := value;
end;

end.
