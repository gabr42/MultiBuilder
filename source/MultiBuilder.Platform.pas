unit MultiBuilder.Platform;

interface

type
  MBPlatform = class
    const EnvConfigExt = '.mbenv';
    const ProjConfigExt = '.mbproj';
    const EnvConfigFile = 'multibuilder' + EnvConfigExt;
    class function EnvConfigName: string;
  end;

implementation

uses
  System.SysUtils, System.IOUtils;

{ MBPlatform }

class function MBPlatform.EnvConfigName: string;
begin

  Result := IncludeTrailingPathDelimiter(TPath.GetSharedDocumentsPath) + EnvConfigFile;
end;

end.
