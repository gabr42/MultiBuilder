unit MultiBuilder.About;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation;

type
  TfrmAbout = class(TForm)
    btnOK: TButton;
    Label1: TLabel;
    lblPG: TLabel;
    Label2: TLabel;
    lblGitHub: TLabel;
    lblSoodesign: TLabel;
    Label4: TLabel;
    lblFlaticon: TLabel;
    Label5: TLabel;
    Label3: TLabel;
    lblCC30: TLabel;
    procedure OpenURL(Sender: TObject);
  private
  public
  end;

var
  frmAbout: TfrmAbout;

implementation

uses
  MultiBuilder.Platform,
  MultiBuilder.Main;

{$R *.fmx}

procedure TfrmAbout.OpenURL(Sender: TObject);
begin
  if Sender = lblPG then
    MBPlatform.OpenURL('http://primoz.gabrijelcic.org')
  else if Sender = lblGitHub then
    MBPlatform.OpenURL('https://github.com/gabr42/MultiBuilder')
  else if Sender = lblSoodesign then
    MBPlatform.OpenURL('https://www.flaticon.com/authors/soodesign')
  else if Sender = lblFlaticon then
    MBPlatform.OpenURL('https://www.flaticon.com/')
  else if Sender = lblCC30 then
    MBPlatform.OpenURL('http://creativecommons.org/licenses/by/3.0/');
end;


end.
