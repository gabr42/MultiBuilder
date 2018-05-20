unit MultiBuilder.Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Generics.Defaults, System.Generics.Collections,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, FMX.ListBox,
  MultiBuilder.Engine.Intf, System.Actions, FMX.ActnList, FMX.ScrollBox, FMX.Memo;

type
  TfrmMultiBuilderMain = class(TForm)
    StyleBook1: TStyleBook;
    Toolbar: TPanel;
    tbEditEnvironment: TButton;
    lbEnvironments: TListBox;
    tbLoadProject: TButton;
    tbEditProject: TButton;
    OpenProject: TOpenDialog;
    SaveProject: TSaveDialog;
    tbRun: TButton;
    ActionList1: TActionList;
    actRun: TAction;
    outLog: TMemo;
    tbRunSelected: TButton;
    actRunSelected: TAction;
    tbLoadEnvironment: TButton;
    SaveEnvironment: TSaveDialog;
    OpenEnvironment: TOpenDialog;
    procedure actRunExecute(Sender: TObject);
    procedure actRunSelectedExecute(Sender: TObject);
    procedure actRunSelectedUpdate(Sender: TObject);
    procedure actRunUpdate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lbEnvironmentsChange(Sender: TObject);
    procedure tbEditEnvironmentClick(Sender: TObject);
    procedure tbEditProjectClick(Sender: TObject);
    procedure tbLoadEnvironmentClick(Sender: TObject);
    procedure tbLoadProjectClick(Sender: TObject);
  private
    FEngine: IMultiBuilderEngine;
    FEngineConfig: string;
    FProjectFile: string;
    FResults: TDictionary<string, TExecuteResult>;
    procedure ParseCommandLine;
    procedure RecreateEnvironment;
    procedure ReloadEngine;
    procedure ReloadProject;
  strict protected
    function CleanEnvironment(const environment: string): string;
    function FindEnvironment(const environment: string): integer;
    procedure MarkEnvironment(const environment: string);
    procedure MarkJobDone(const environment: string; const result: TExecuteResult);
  public
  end;

var
  frmMultiBuilderMain: TfrmMultiBuilderMain;

implementation

uses
  MultiBuilder.Platform,
  MultiBuilder.Engine,
  MultiBuilder.Editor.Project,
  MultiBuilder.Editor.Environment;

{$R *.fmx}

procedure TfrmMultiBuilderMain.actRunExecute(Sender: TObject);
var
  i: integer;
begin
  FResults.Clear;
  Toolbar.Enabled := false;
  lbEnvironments.ItemIndex := -1; //force reload
  for i := 0 to lbEnvironments.Items.Count - 1 do
    lbEnvironments.Items[i] := #$2BC8 + ' ' + CleanEnvironment(lbEnvironments.Items[i]);
  FEngine.OnRunCompleted :=
    procedure
    begin
      Toolbar.Enabled := true;
    end;
  FEngine.Run;
end;

procedure TfrmMultiBuilderMain.actRunSelectedExecute(Sender: TObject);
var
  env: string;
begin
  env := CleanEnvironment(lbEnvironments.Items[lbEnvironments.ItemIndex]);
  FResults.Remove(env);
  lbEnvironments.Items[lbEnvironments.ItemIndex] := #$2BC8 + ' ' + env;
  lbEnvironmentsChange(lbEnvironments);
  FEngine.OnRunCompleted := nil;
  FEngine.RunSelected(env);
end;

procedure TfrmMultiBuilderMain.actRunSelectedUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := Toolbar.Enabled and (lbEnvironments.ItemIndex >= 0);
end;

procedure TfrmMultiBuilderMain.actRunUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := Toolbar.Enabled;
end;

function TfrmMultiBuilderMain.CleanEnvironment(const environment: string): string;
begin
  Result := environment;
  if (Result <> '') and ((Result[1] = #$2611) or (Result[1] = #$2612) or (Result[1] = #$2BC8)) then
    Delete(Result, 1, 2);
end;

function TfrmMultiBuilderMain.FindEnvironment(const environment: string): integer;
begin
  for Result := 0 to lbEnvironments.Count - 1 do
    if SameText(CleanEnvironment(lbEnvironments.Items[Result]), environment) then
      Exit;
  Result := -1;
end;

procedure TfrmMultiBuilderMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FResults);
end;

procedure TfrmMultiBuilderMain.FormCreate(Sender: TObject);
begin
  OpenEnvironment.Filter := StringReplace(OpenEnvironment.Filter,
                              '$(mbenv)', MBPlatform.EnvConfigExt,
                              [rfReplaceAll]);
  SaveEnvironment.Filter := OpenEnvironment.Filter;
  OpenProject.Filter := StringReplace(OpenProject.Filter,
                          '$(mbproj)', MBPlatform.ProjConfigExt,
                          [rfReplaceAll]);
  SaveProject.Filter := OpenProject.Filter;

  FResults := TDictionary<string, TExecuteResult>.Create(TIStringComparer.Ordinal);
  FEngine := CreateMultiBUilderEngine;
  FEngine.OnJobDone :=
    procedure (const environment: string; const result: TExecuteResult)
    begin
      MarkJobDone(environment, result);
    end;

  FEngineConfig := MBPlatform.EnvConfigName;
  ReloadEngine;

  ParseCommandLine;
end;

procedure TfrmMultiBuilderMain.lbEnvironmentsChange(Sender: TObject);
var
  result: TExecuteResult;
begin
  if lbEnvironments.ItemIndex = -1 then
    outLog.Text := ''
  else if not FResults.TryGetValue(CleanEnvironment(lbEnvironments.Items[lbEnvironments.ItemIndex]), result) then begin
    if CleanEnvironment(lbEnvironments.Items[lbEnvironments.ItemIndex]) = lbEnvironments.Items[lbEnvironments.ItemIndex] then
      outLog.Text := 'Not started'
    else
      outLog.Text := 'Running';
  end
  else if result.ExitCode = 0 then
    outLog.Text := 'OK'
  else
    outLog.Text := '[' + result.ExitCode.ToString + '] ' + result.Command + #13#10#13#10 + result.Output;
end;

procedure TfrmMultiBuilderMain.MarkEnvironment(const environment: string);
var
  idx   : integer;
  result: TExecuteResult;
begin
  idx := FindEnvironment(environment);
  if idx < 0 then
    Exit;

  if not FResults.TryGetValue(environment, result) then
    lbEnvironments.Items[idx] := environment
  else if result.ExitCode = 0 then
    lbEnvironments.Items[idx] := #$2611 + ' ' + environment
  else
    lbEnvironments.Items[idx] := #$2612 + ' ' + environment;
end;

procedure TfrmMultiBuilderMain.MarkJobDone(const environment: string;
  const result: TExecuteResult);
begin
  FResults.AddOrSetValue(environment, result);
  MarkEnvironment(environment);
  if (lbEnvironments.ItemIndex >= 0) and (CleanEnvironment(lbEnvironments.Items[lbEnvironments.ItemIndex]) = environment) then
    lbEnvironmentsChange(lbEnvironments);
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

procedure TfrmMultiBuilderMain.tbEditEnvironmentClick(Sender: TObject);
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
      ReloadProject;
    end;
  finally FreeAndNil(projContent); end;
end;

procedure TfrmMultiBuilderMain.tbLoadEnvironmentClick(Sender: TObject);
begin
  if OpenEnvironment.Execute then begin
    FEngineConfig := OpenEnvironment.FileName;
    ReloadEngine;
  end;
end;

procedure TfrmMultiBuilderMain.tbLoadProjectClick(Sender: TObject);
begin
  if OpenProject.Execute then begin
    FProjectFile := OpenProject.FileName;
    ReloadProject;
  end;
end;

end.

