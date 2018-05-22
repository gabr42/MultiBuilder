unit MultiBuilder.Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Actions, System.Classes,
  System.Variants, System.Generics.Defaults, System.Generics.Collections,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, FMX.ListBox, FMX.ActnList,
  FMX.ScrollBox, FMX.Memo,
  MultiBuilder.Engine.Intf;

type
  TProjectResult = record
    CommandResults: TArray<TExecuteResult>;
    Completed     : boolean;
    constructor Create(ACompleted: boolean);
    procedure AppendCommandResult(result: TExecuteResult);
    function  FindFirstError: integer;
  end;

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
    cbxCommands: TComboBox;
    actLoadEnvironment: TAction;
    actEditEnvironment: TAction;
    actLoadProject: TAction;
    actEditProject: TAction;
    procedure actEditEnvironmentExecute(Sender: TObject);
    procedure actEditProjectExecute(Sender: TObject);
    procedure actLoadEnvironmentExecute(Sender: TObject);
    procedure actLoadProjectExecute(Sender: TObject);
    procedure EnableWhenNoJobs(Sender: TObject);
    procedure actRunExecute(Sender: TObject);
    procedure actRunSelectedExecute(Sender: TObject);
    procedure actRunSelectedUpdate(Sender: TObject);
    procedure actRunUpdate(Sender: TObject);
    procedure cbxCommandsChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lbEnvironmentsChange(Sender: TObject);
  private
  const
    CMarkOK      = #$2611;
    CMarkError   = #$2612;
    CMarkRunning = #$2BC8;
  var
    FEngine: IMultiBuilderEngine;
    FEngineConfig: string;
    FProjectFile: string;
    FResults: TDictionary<string, TProjectResult>;
    procedure ParseCommandLine;
    procedure RecreateEnvironment;
    procedure ReloadEngine;
    procedure ReloadProject;
  strict protected
    function  CleanEnvironment(const environment: string): string;
    function  FindEnvironment(const environment: string): integer;
    function  GetActiveEnv: string;
    procedure MarkEnvironment(const environment: string);
    procedure MarkCommandDone(const environment: string; const result: TExecuteResult);
    procedure MarkJobDone(const environment: string; const result: TExecuteResult);
    property ActiveEnv: string read GetActiveEnv;
  public
  end;

var
  frmMultiBuilderMain: TfrmMultiBuilderMain;

implementation

uses
  MultiBuilder.Platform, MultiBuilder.Engine,
  MultiBuilder.Editor.Project, MultiBuilder.Editor.Environment;

{$R *.fmx}

type
  TExecuteResultHelper = record helper for TExecuteResult
    function ExitCodeStr: string;
  end;

procedure TfrmMultiBuilderMain.actEditEnvironmentExecute(Sender: TObject);
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

procedure TfrmMultiBuilderMain.actEditProjectExecute(Sender: TObject);
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

procedure TfrmMultiBuilderMain.actLoadEnvironmentExecute(Sender: TObject);
begin
  if OpenEnvironment.Execute then begin
    FEngineConfig := OpenEnvironment.FileName;
    ReloadEngine;
  end;
end;

procedure TfrmMultiBuilderMain.actLoadProjectExecute(Sender: TObject);
begin
  if OpenProject.Execute then begin
    FProjectFile := OpenProject.FileName;
    ReloadProject;
  end;
end;

procedure TfrmMultiBuilderMain.actRunExecute(Sender: TObject);
var
  env: string;
  i  : integer;
begin
  FResults.Clear;
  Toolbar.Enabled := false;
  lbEnvironments.ItemIndex := -1; //force reload
  for i := 0 to lbEnvironments.Items.Count - 1 do begin
    env := CleanEnvironment(lbEnvironments.Items[i]);
    FResults.Add(env, TProjectResult.Create(false));
    MarkEnvironment(env);
  end;
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
  env := ActiveEnv;
  FResults.Remove(env);
  FResults.Add(env, TProjectResult.Create(false));
  MarkEnvironment(env);
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

procedure TfrmMultiBuilderMain.cbxCommandsChange(Sender: TObject);
var
  cmdResult    : TExecuteResult;
  projectResult: TProjectResult;
begin
  if ActiveEnv = '' then
    Exit;

  if cbxCommands.ItemIndex < 0 then
    outLog.Text := ''
  else if not FResults.TryGetValue(ActiveEnv, projectResult) then
    outLog.Text := 'No results yes'
  else if High(projectResult.CommandResults) < (cbxCommands.Items.Count - 1) then
    outLog.Text := 'No results yes'
  else begin
    cmdResult := projectResult.CommandResults[cbxCommands.ItemIndex];
    outLog.Text := cmdResult.Output
  end;
end;

function TfrmMultiBuilderMain.CleanEnvironment(const environment: string): string;
begin
  Result := environment;
  if (Result <> '') and ((Result[1] = CMarkOK) or (Result[1] = CMarkError) or (Result[1] = CMarkRunning)) then
    Delete(Result, 1, 2);
end;

procedure TfrmMultiBuilderMain.EnableWhenNoJobs(Sender: TObject);
begin
  (Sender as TAction).Enabled := Toolbar.Enabled and (FEngine.NumRunningProjects = 0);
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

  FResults := TDictionary<string, TProjectResult>.Create(TIStringComparer.Ordinal);
  FEngine := CreateMultiBUilderEngine;
  FEngine.OnCommandDone :=
    procedure (const environment: string; const result: TExecuteResult)
    begin
      MarkCommandDone(environment, result);
    end;
  FEngine.OnJobDone :=
    procedure (const environment: string; const result: TExecuteResult)
    begin
      MarkJobDone(environment, result);
    end;

  FEngineConfig := MBPlatform.EnvConfigName;
  ReloadEngine;

  ParseCommandLine;
end;

function TfrmMultiBuilderMain.GetActiveEnv: string;
begin
  if lbEnvironments.ItemIndex < 0 then
    Result := ''
  else
    Result := CleanEnvironment(lbEnvironments.Items[lbEnvironments.ItemIndex]);
end;

procedure TfrmMultiBuilderMain.lbEnvironmentsChange(Sender: TObject);
var
  cmdRes: TExecuteResult;
  result: TProjectResult;
begin
  if lbEnvironments.ItemIndex = -1 then
    outLog.Text := ''
  else if not FResults.TryGetValue(ActiveEnv, result) then begin
    if ActiveEnv = lbEnvironments.Items[lbEnvironments.ItemIndex] then
      outLog.Text := 'Not started'
    else
      outLog.Text := 'Running';
  end
  else begin
    cbxCommands.Items.Clear;
    outLog.Text := '';
    for cmdRes in result.CommandResults do
      cbxCommands.Items.Add(Format('[%s] %s', [cmdRes.ExitCodeStr, cmdRes.Command]));
    if cbxCommands.Items.Count > 0 then
      cbxCommands.ItemIndex := 0;
  end;
end;

procedure TfrmMultiBuilderMain.MarkCommandDone(const environment: string;
  const result: TExecuteResult);
var
  cmdRes    : TExecuteResult;
  projectRes: TProjectResult;
begin
  if not FResults.TryGetValue(environment, projectRes) then begin
    projectRes := Default(TProjectResult);
    projectRes.Completed := false;
  end;
  cmdRes := result;
  projectRes.AppendCommandResult(result);
  FResults.AddOrSetValue(environment, projectRes);
end;

procedure TfrmMultiBuilderMain.MarkEnvironment(const environment: string);
var
  idx   : integer;
  result: TProjectResult;
begin
  idx := FindEnvironment(environment);
  if idx < 0 then
    Exit;

  if not FResults.TryGetValue(environment, result) then
    lbEnvironments.Items[idx] := environment
  else if not result.Completed then
    lbEnvironments.Items[idx] := CMarkRunning + ' ' + environment
  else if result.FindFirstError < 0 then
    lbEnvironments.Items[idx] := CMarkOK + ' ' + environment
  else
    lbEnvironments.Items[idx] := CMarkError + ' ' + environment;
end;

procedure TfrmMultiBuilderMain.MarkJobDone(const environment: string;
  const result: TExecuteResult);
var
  projectRes: TProjectResult;
begin
  if not FResults.TryGetValue(environment, projectRes) then
    projectRes.CommandResults := TArray<TExecuteResult>.Create(result);
  projectRes.Completed := true;
  FResults.AddOrSetValue(environment, projectRes);

  MarkEnvironment(environment);
  if (lbEnvironments.ItemIndex >= 0) and (ActiveEnv = environment) then
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

{ TProjectResult }

constructor TProjectResult.Create(ACompleted: boolean);
begin
  Completed := ACompleted;
  SetLength(CommandResults, 0);
end;

procedure TProjectResult.AppendCommandResult(result: TExecuteResult);
begin
  SetLength(CommandResults, Length(CommandResults) + 1);
  CommandResults[High(CommandResults)] := result;
end;

function TProjectResult.FindFirstError: integer;
begin
  for Result := Low(CommandResults) to High(CommandResults) do
    if CommandResults[Result].ExitCode > 0 then
      Exit;

  Result := -1;
end;

{ TExecuteResultHelper }

function TExecuteResultHelper.ExitCodeStr: string;
begin
  if ExitCode = 0 then
    Result := 'OK'
  else
    Result := ExitCode.ToString;
end;

end.

