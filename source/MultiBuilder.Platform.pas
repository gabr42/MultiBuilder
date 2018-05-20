unit MultiBuilder.Platform;

interface

type
  MBPlatform = class
    const EnvConfigExt = '.mbenv';
    const ProjConfigExt = '.mbproj';
    const EnvConfigFile = 'multibuilder' + EnvConfigExt;
    class function EnvConfigName: string;
    class procedure Execute(const workDir: string; const command: string;
      var exitCode: integer; var output: string);
  end;

implementation

uses
  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  {$ENDIF}
  System.SysUtils, System.IOUtils, System.Classes;

{ MBPlatform }

class function MBPlatform.EnvConfigName: string;
begin
  Result := IncludeTrailingPathDelimiter(TPath.GetSharedDocumentsPath) + EnvConfigFile;
end;

function ReadFromFile(const fileName: string): string;
var
  sl: TStringList;
begin
  Result := '';
  sl := TStringList.Create;
  try
    try
      sl.LoadFromFile(fileName);
    except end;
    Result := sl.Text;
  finally FreeAndNil(sl); end;
end;

class procedure MBPlatform.Execute(const workDir, command: string; var exitCode: integer;
  var output: string);
var
  exitC   : DWORD;
  procInfo: TProcessInformation;
  swInfo  : TStartupInfo;
  tempName: string;
begin
  tempName := TPath.GetTempFileName;
  try
    {$IFDEF MSWINDOWS}
    FillChar(swInfo, SizeOf(swInfo), 0);
    swInfo.cb := SizeOf(swInfo);
    swInfo.dwFlags := STARTF_USESHOWWINDOW;
    swInfo.wShowWindow := SW_HIDE;
    if not CreateProcess(PChar('c:\windows\system32\cmd.exe'), PChar('/C ' + command + ' >' + tempName),
             nil, nil, false, CREATE_NEW_CONSOLE OR NORMAL_PRIORITY_CLASS, nil,
             PChar(workDir), swInfo, procInfo)
    then begin
      exitCode := GetLastError;
      output := SysErrorMessage(exitCode);
    end
    else begin
      WaitForSingleObject(procInfo.hProcess, INFINITE);
      GetExitCodeProcess(procInfo.hProcess, exitC);
      exitCode := integer(exitC);
      CloseHandle(procInfo.hThread);
      CloseHandle(procInfo.hProcess);
      output := ReadFromFile(tempName);
    end;
    {$ELSE}
    exitCode := 255;
    output := 'MultiBuilder: Not implemented on this platform';
    {$ENDIF}
  finally DeleteFile(tempName); end;
end;

end.
