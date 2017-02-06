unit UConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles;

type
  TImaginaryConfig = class;

  IConfigProvider = interface
    function GetConfig: TImaginaryConfig;
  end;

{ TImaginaryConfig }

TImaginaryConfig = class
private
  iniOptions: TIniFile;
  FVersion: string;
public
  constructor Create(ini: TIniFile; AVersion: string);
  destructor Destroy; override;

  // window
  function DefaultScreenSize: TRect;
  function ScreenSizeChanged: boolean;
  procedure SetDefaultScreenSize(value: TRect);
end;

function GetActualConfig: TIniFile;

var
  ActualConfigDirUTF8: string;

implementation

uses forms, uparse, LCLProc, LazFileUtils;

function GetActualConfig: TIniFile;
var
  PortableConfig: TIniFile;
  AppDirSys: string;
  PortableConfigFilenameSys: string;
  ActualConfigFilenameSys: string;
  //i: integer;
  begin
    ActualConfigFilenameSys := '';
    // check if a config file path is defined
    AppDirSys := ExtractFilePath(Application.ExeName);
    PortableConfigFilenameSys := AppDirSys + 'imaginary.ini';
    If FileExists(PortableConfigFilenameSys) then
    begin
      PortableConfig := TIniFile.Create(PortableConfigFilenameSys);
      ActualConfigFilenameSys := PortableConfig.ReadString('General', 'ConfigFile', '');
      if ActualConfigFilenameSys <> '' then
      begin
        ActualConfigFilenameSys := ExpandFileName(AppDirSys + ActualConfigFilenameSys);
      end;
      PortableConfig.Free;
    end;
    // Otherwise, use default path
    if ActualConfigFilenameSys = '' then
    begin
      CreateDir(GetAppConfigDir(False));
      ActualConfigFilenameSys := GetAppConfigFile(False, False);
    end;
    result := TIniFile.Create(ActualConfigFilenameSys, True);
    //ActualConfigDirUTF8 := SysToUTF8(ExtractFilePath(ActualConfigFilenameSys));
  end;

{ TImaginaryConfig }

function TImaginaryConfig.DefaultScreenSize: TRect;
begin
  result := StrToRect(iniOptions.ReadString('Window', 'ScreenSize', ''));
end;

function TImaginaryConfig.ScreenSizeChanged: boolean;
var
  currentScreenSize, previousScreenSize: TRect;
begin
  currentScreenSize := Rect(0, 0, screen.Width, screen.Height);
  previousScreenSize := DefaultScreenSize;
  if not CompareRect(@previousScreenSize, @currentScreenSize.Left) then
  begin
    SetDefaultScreenSize(currentScreenSize);
    result := true;
  end
  else
  begin
    result := false;
  end;
end;

procedure TImaginaryConfig.SetDefaultScreenSize(value: TRect);
begin
  iniOptions.WriteString('Window', 'ScreenSize', RectToStr(value));
end;

constructor TImaginaryConfig.Create(ini: TIniFile; AVersion: string);
begin
  FVersion := AVersion;
  iniOptions := ini;
end;

destructor TImaginaryConfig.Destroy;
begin
  iniOptions.Free;
end;

end.

