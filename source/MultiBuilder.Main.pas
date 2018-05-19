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
    procedure tbEditProjectClick(Sender: TObject);
    procedure tbLoadProjectClick(Sender: TObject);
  private
    FEngine: IMultiBuilderEngine;
    FEngineConfig: string;
    FProjectFile: string;
    procedure ParseCommandLine;
    procedure RecreateEnvironment;
    procedure ReloadEngine;
    procedure ReloadProject;
  public
  end;

var
  frmMultiBuilderMain: TfrmMultiBuilderMain;

implementation

uses
  MultiBuilder.Engine,
  MultiBuilder.Platform,
  MultiBuilder.Editor.Project,
  MultiBuilder.Editor.Environment;

{$R *.fmx}

procedure TfrmMultiBuilderMain.FormCreate(Sender: TObject);
begin
  OpenProject.Filter := StringReplace(OpenProject.Filter,
                          '$(mbproj)', MBPlatform.ProjConfigExt,
                          [rfReplaceAll]);
  SaveProject.Filter := OpenProject.Filter;

  FEngine := CreateMultiBUilderEngine;
  FEngineConfig := MBPlatform.EnvConfigName;
  ReloadEngine;

  ParseCommandLine;
end;

procedure TfrmMultiBuilderMain.ParseCommandLine;
var
  ext: string;
  i  : integer;
begin
  for i := 1 to ParamCount do begin
    ext := ExtractFileExt(ParamStr(i));
    if SameText(ext, MBPlatform.EnvConfigExt) then begin
      FEngineConfig := ParamStr(i);
      ReloadEngine;
    end
    else if SameText(ext, MBPlatform.ProjConfigExt) then begin
      FProjectFile := ParamStr(i);
      ReloadProject;
    end;
  end;
end;

procedure TfrmMultiBuilderMain.RecreateEnvironment;
var
  envName: string;
begin
  lbEnvironments.Clear;
  for envName in FEngine.Environments do
    lbEnvironments.Items.Add(envName);
end;

procedure TfrmMultiBuilderMain.ReloadEngine;
begin
  if not FEngine.LoadFrom(FEngineConfig) then
    ShowMessage('Bad configuration file ' + FEngineConfig);
  RecreateEnvironment;
end;

procedure TfrmMultiBuilderMain.ReloadProject;
var
  projFile: string;
begin
  projFile := FProjectFile;
  if (FProjectFile <> '') and FileExists(FProjectFile) then begin
    if not FEngine.LoadProject(FProjectFile) then begin
      ShowMessage('Bad project file ' + FProjectFile);
      projFile := '';
    end;
  end;
  if projFile <> '' then
    Caption := 'MultiBuilder [' + FProjectFile + ']'
  else begin
    Caption := 'MultiBuilder';
    FEngine.ClearProject;
  end;
end;

procedure TfrmMultiBuilderMain.tbConfigureEnvironmentClick(Sender: TObject);
var
  changed   : boolean;
  frmEnvEdit: TfrmEditEnvironment;
  iniContent: TStringList;
begin
  iniContent := TStringList.Create;
  try
    changed := false;
    if FileExists(FEngineConfig) then
      iniContent.LoadFromFile(FEngineConfig);

    frmEnvEdit := TfrmEditEnvironment.Create(Self);
    try
      frmEnvEdit.Environment := iniContent.Text;
      if frmEnvEdit.ShowModal = mrOK then begin
        iniContent.Text := frmEnvEdit.Environment;
        changed := true;
      end;
    finally FreeAndNil(frmEnvEdit); end;

    if changed then begin
      iniContent.SaveToFile(FEngineConfig);
      ReloadEngine;
    end;
  finally FreeAndNil(iniContent); end;
end;

procedure TfrmMultiBuilderMain.tbEditProjectClick(Sender: TObject);
var
  changed    : boolean;
  frmProjEdit: TfrmEditProject;
  projContent: TStringList;
begin
  projContent := TStringList.Create;
  try
    changed := false;
    if (FProjectFile <> '') and FileExists(FProjectFile) then
      projContent.LoadFromFile(FProjectFile);

    frmProjEdit := TfrmEditProject.Create(Self);
    try
      frmProjEdit.Project := projContent.Text;
      if frmProjEdit.ShowModal = mrOK then begin
        projContent.Text := frmProjEdit.Project;
        changed := true;
      end;
    finally FreeAndNil(frmProjEdit); end;

    if changed then begin
      if FProjectFile = '' then begin
        if not SaveProject.Execute then
          Exit;
        FProjectFile := SaveProject.FileName;
      end;
      projContent.SaveToFile(FProjectFile);
    end;
  finally FreeAndNil(projContent); end;
end;

procedure TfrmMultiBuilderMain.tbLoadProjectClick(Sender: TObject);
begin
  if OpenProject.Execute then begin
    FProjectFile := OpenProject.FileName;
    ReloadProject;
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
// Root=h:\razvoj\omnithreadlibrary-v4\unittests
// Cmd=$(Path)\dcc32 CompileAllUnits -b -u..;..\src;..\..\fastmm -i.. -nsSystem;System.Win;Winapi;Vcl;Vcl.Imaging;Vcl.Samples;Data;Xml -e$(Scratch)\exe -n0$(Scratch)\dcu\win32 -dCONSOLE_TESTRUNNER
// Cmd=$(Path)\dcc32 TestRunner -b -u..;..\src;..\..\fastmm -i.. -nsSystem;System.Win;Winapi;Vcl;Vcl.Imaging;Vcl.Samples;Data;Xml -e$(Scratch)\exe -n0$(Scratch)\dcu\win32 -dCONSOLE_TESTRUNNER
// Cmd=$(Scratch)\exe\TestRunner
// Cmd=$(Path)\dcc64 CompileAllUnits -b -u..;..\src;..\..\fastmm -i.. -nsSystem;System.Win;Winapi;Vcl;Vcl.Imaging;Vcl.Samples;Data;Xml -e$(Scratch)\exe -n0$(Scratch)\dcu\win64 -dCONSOLE_TESTRUNNER
// Cmd=$(Path)\dcc64 TestRunner -b -u..;..\src;..\..\fastmm -i.. -nsSystem;System.Win;Winapi;Vcl;Vcl.Imaging;Vcl.Samples;Data;Xml -e$(Scratch)\exe -n0$(Scratch)\dcu\win64 -dCONSOLE_TESTRUNNER
// Cmd=$(Scratch)\exe\TestRunner


