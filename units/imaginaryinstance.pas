unit ImaginaryInstance;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ImaginaryTypes, IniFiles, ImaginaryConfig, ImaginaryForm;

type
  TImaginaryInstance = class(TImaginaryCustomInstance)
  private
    function GetMainFormVisible: boolean;
  protected
    MainForm: TMainForm;
    FFullscreen: boolean;
    FConfig: TImaginaryConfig;
    FInFormsNeeded: boolean;
    procedure Init(AEmbedded: boolean);
    function GetFullscreen: boolean; override;
    procedure SetFullscreen(AValue: boolean); override;
    function GetConfig: TImaginaryConfig; override;
    procedure FormsNeeded;
  public
    constructor Create; override;
    constructor Create(AEmbedded: boolean); override;
    property MainFormVisible: boolean read GetMainFormVisible;
    procedure Show; override;
    procedure Hide; override;
    procedure Run; override;
    procedure SaveMainWindowPosition; override;
    procedure RestoreMainWindowPosition; override;
    procedure UseConfig(ini: TInifile); override;

  end;


implementation

uses
  LCLType, Types, Forms, Dialogs, FileUtil, LCLIntf;

procedure TImaginaryInstance.Init(AEmbedded: boolean);
begin
  Title := 'Imaginary ' + ImaginaryCurrentVersion;
end;

constructor TImaginaryInstance.Create;
begin
  Init(False);
end;

constructor TImaginaryInstance.Create(AEmbedded: boolean);
begin
  Init(AEmbedded);
end;

procedure TImaginaryInstance.UseConfig(ini: TInifile);
begin
  FreeAndNil(FConfig);
  FConfig := TImaginaryConfig.Create(ini, ImaginaryCurrentVersionOnly);
end;

function TImaginaryInstance.GetConfig: TImaginaryConfig;
begin
  Result := FConfig;
end;

procedure TImaginaryInstance.Show;
begin
  //EmbeddedResult := mrNone;
  FormsNeeded;
  MainForm.Show;
end;

procedure TImaginaryInstance.Hide;
begin
  if MainFormVisible then
    MainForm.Hide;
end;

procedure TImaginaryInstance.Run;
begin
  if not MainFormVisible then
    Show;
  repeat
    application.ProcessMessages;
    Sleep(10);
  until not MainFormVisible;
end;

procedure TImaginaryInstance.FormsNeeded;
begin
  if (MainForm <> nil) or FInFormsNeeded then
    exit;

  FInFormsNeeded := True;
  Application.CreateForm(TMainForm, MainForm);
  MainForm.ImaginaryInstance := self;

  FInFormsNeeded := False;
end;

function TImaginaryInstance.GetMainFormVisible: boolean;
begin
  if MainForm <> nil then
    Result := MainForm.Visible
  else
    Result := False;
end;

function TImaginaryInstance.GetFullscreen: boolean;
begin
  Result := FFullscreen;
end;

procedure TImaginaryInstance.SetFullscreen(AValue: boolean);
begin
  if (AValue = FFullscreen) or not MainFormVisible or
    (MainForm.WindowState = wsMinimized) then
    exit;
  FFullscreen := AValue;
  if AValue then
  begin
    SaveMainWindowPosition;
    MainForm.BorderStyle := bsNone;
    MainForm.WindowState := wsFullScreen;
  end
  else
  begin
    MainForm.BorderStyle := bsSizeable;
    MainForm.WindowState := wsNormal;
    RestoreMainWindowPosition;
  end;
end;

procedure TImaginaryInstance.SaveMainWindowPosition;
var
  r: TRect;
begin
  if MainForm.WindowState = wsMinimized then
    exit;
  if MainForm.WindowState = wsMaximized then
    Config.SetDefaultMainWindowMaximized(True)
  else
  if MainForm.WindowState = wsNormal then
  begin
    r.left := MainForm.Left;
    r.top := MainForm.Top;
    r.right := r.left + MainForm.ClientWidth;
    r.Bottom := r.top + MainForm.ClientHeight;
    Config.SetDefaultMainWindowPosition(r);
  end;
end;

procedure TImaginaryInstance.RestoreMainWindowPosition;
var
  r: TRect;
begin
  if not MainFormVisible then
    exit;
  if Config.DefaultMainWindowMaximized then
    MainForm.WindowState := wsMaximized
  else
  begin
    r := Config.DefaultMainWindowPosition;
    if (r.right > r.left) and (r.bottom > r.top) then
    begin
      MainForm.Position := poDesigned;
      MainForm.Left := r.Left;
      MainForm.Top := r.Top;
      MainForm.ClientWidth := r.right - r.left;
      MainForm.ClientHeight := r.bottom - r.top;
    end;
  end;
end;

end.
