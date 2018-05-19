unit MultiBuilder.Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, FMX.ListBox,
  MultiBuilder.Engine.Intf;

type
  TfrmMultiBuilderMain = class(TForm)
    StyleBook1: TStyleBook;
    Toolbar: TPanel;
    tbConfigureEnvironment: TButton;
    lbEnvironments: TListBox;
    tbLoadProject: TButton;
    tbEditProject: TButton;
    OpenProject: TOpenDialog;
    SaveProject: TSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure tbConfigureEnvironmentClick(Sender: TObject);
    procedure tbLoadProjectClick(Sender: TObject);
  private
    FEngine: IMultiBuilderEngine;
    FProjectFile: string;
    procedure RecreateEnvironment;
  public
  end;

var
  frmMultiBuilderMain: TfrmMultiBuilderMain;

implementation

uses
  MultiBuilder.Engine,
  MultiBuilder.Platform,
  MultiBuilder.Editor.Environment;

{$R *.fmx}

procedure TfrmMultiBuilderMain.FormCreate(Sender: TObject);
begin
  OpenProject.Filter := StringReplace(OpenProject.Filter,
                          '$(mbproj)', MBPlatform.ProjConfigExt,
                          [rfReplaceAll]);
  SaveProject.Filter := OpenProject.Filter;

  FEngine := CreateMultiBUilderEngine;
  if FEngine.LoadFrom(MBPlatform.EnvConfigName) then
    RecreateEnvironment;
end;

procedure TfrmMultiBuilderMain.RecreateEnvironment;
var
  envName: string;
begin
  lbEnvironments.Clear;
  for envName in FEngine.Environments do
    lbEnvironments.Items.Add(envName);
end;

procedure TfrmMultiBuilderMain.tbConfigureEnvironmentClick(Sender: TObject);
var
  changed   : boolean;
  frmEnvEdit: TfrmEditEnvironment;
  iniContent: TStringList;
  iniFile   : string;
begin
  iniFile := MBPlatform.EnvConfigName;

  iniContent := TStringList.Create;
  try
    changed := false;
    if FileExists(iniFile) then
      iniContent.LoadFromFile(iniFile);

    frmEnvEdit := TfrmEditEnvironment.Create(Self);
    try
      frmEnvEdit.Environment := iniContent.Text;
      if frmEnvEdit.ShowModal = mrOK then begin
        iniContent.Text := frmEnvEdit.Environment;
        changed := true;
      end;
    finally FreeAndNil(frmEnvEdit); end;

    if changed then begin
      iniContent.SaveToFile(iniFile);
      if not FEngine.LoadFrom(iniFile) then
        ShowMessage('Bad configuration file');
      RecreateEnvironment;
    end;
  finally FreeAndNil(iniContent); end;
end;

procedure TfrmMultiBuilderMain.tbLoadProjectClick(Sender: TObject);
begin
  if OpenProject.Execute then begin
    FProjectFile := OpenProject.FileName;
    if not FEngine.LoadProject(FProjectFile) then
      ShowMessage('Bad project file');
    Caption := 'MultiBuilder [' + FProjectFile + ']';
  end;
end;

end.

// Environment:

//[Global]
//Scratch=c:\0\MultiBuilder\$(EnvironmentName)
//ForceDir=$(Scratch)\exe;$(Scratch)\dcu;$(Scratch)\log
//Output=$(Scratch)\log\$(EnvironmentName)
//
//[Delphi 2007]
//Path=d:\Delphi\5.0

// Project:

//[Global]
//  Root=h:\razvoj\omnithreadlibrary-v4\unittests
//  $(Path)\dcc32 CompileAllUnits -b -u..;..\src;..\..\fastmm -i.. -nsSystem;System.Win;Winapi;Vcl;Vcl.Imaging;Vcl.Samples;Data;Xml -e$(Scratch)\exe -n0$(Scratch)\dcu\win32 -dCONSOLE_TESTRUNNER
//  $(Path)\dcc32 TestRunner -b -u..;..\src;..\..\fastmm -i.. -nsSystem;System.Win;Winapi;Vcl;Vcl.Imaging;Vcl.Samples;Data;Xml -e$(Scratch)\exe -n0$(Scratch)\dcu\win32 -dCONSOLE_TESTRUNNER
//  $(Scratch)\exe\TestRunner
//  $(Path)\dcc64 CompileAllUnits -b -u..;..\src;..\..\fastmm -i.. -nsSystem;System.Win;Winapi;Vcl;Vcl.Imaging;Vcl.Samples;Data;Xml -e$(Scratch)\exe -n0$(Scratch)\dcu\win64 -dCONSOLE_TESTRUNNER
//  $(Path)\dcc64 TestRunner -b -u..;..\src;..\..\fastmm -i.. -nsSystem;System.Win;Winapi;Vcl;Vcl.Imaging;Vcl.Samples;Data;Xml -e$(Scratch)\exe -n0$(Scratch)\dcu\win64 -dCONSOLE_TESTRUNNER
//  $(Scratch)\exe\TestRunner


