unit MultiBuilder.Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls;

type
  TForm77 = class(TForm)
    StyleBook1: TStyleBook;
    Toolbar: TPanel;
    tbConfigureEnvironment: TButton;
    procedure FormCreate(Sender: TObject);
    procedure tbConfigureEnvironmentClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form77: TForm77;

implementation

{$R *.fmx}

procedure TForm77.FormCreate(Sender: TObject);
begin
  Button1.TextSettings.VertAlign := TTextAlign.Trailing;
end;

procedure TForm77.tbConfigureEnvironmentClick(Sender: TObject);
begin
//
//[Global]
//Scratch=c:\0\MultiBuilder\$(Env)
//
//[Delphi 2007]
//Path=d:\Delphi\5.0


end;

end.
// Project:
//[Global]
//  dcc32 CompileAllUnits -b -u..;..\src;..\..\fastmm -i.. -nsSystem;System.Win;Winapi;Vcl;Vcl.Imaging;Vcl.Samples;Data;Xml -e%otl_ut_root%\exe -n0%otl_ut_root%\dcu\win32 -dCONSOLE_TESTRUNNER >%otl_ut_root%\build.log 2>&1
//  dcc32 TestRunner -b -u..;..\src;..\..\fastmm -i.. -nsSystem;System.Win;Winapi;Vcl;Vcl.Imaging;Vcl.Samples;Data;Xml -e%otl_ut_root%\exe -n0%otl_ut_root%\dcu\win32 -dCONSOLE_TESTRUNNER >%otl_ut_root%\build.log 2>&1
//  %otl_ut_root%\exe\TestRunner >%otl_ut_root%\unittest.log 2>&1
//  dcc64 CompileAllUnits -b -u..;..\src;..\..\fastmm -i.. -nsSystem;System.Win;Winapi;Vcl;Vcl.Imaging;Vcl.Samples;Data;Xml -e%otl_ut_root%\exe -n0%otl_ut_root%\dcu\win64 -dCONSOLE_TESTRUNNER >%otl_ut_root%\build.log 2>&1
//  dcc64 TestRunner -b -u..;..\src;..\..\fastmm -i.. -nsSystem;System.Win;Winapi;Vcl;Vcl.Imaging;Vcl.Samples;Data;Xml -e%otl_ut_root%\exe -n0%otl_ut_root%\dcu\win64 -dCONSOLE_TESTRUNNER >%otl_ut_root%\build.log 2>&1
//  %otl_ut_root%\exe\TestRunner >%otl_ut_root%\unittest.log 2>&1


