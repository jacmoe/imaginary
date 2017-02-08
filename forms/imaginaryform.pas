unit ImaginaryForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ImaginaryConfig, ImaginaryTypes,
  Menus;

type

  { TMainForm }

  TMainForm = class(TForm)
    MainMenu: TMainMenu;
    MenuFile: TMenuItem;
    MenuFileExit: TMenuItem;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MenuFileExitClick(Sender: TObject);
  private
    { private declarations }
    Config: TImaginaryConfig;
    FImaginaryInstance: TImaginaryCustomInstance;
    initialized: boolean;
    procedure SetImaginaryInstance(const AValue: TImaginaryCustomInstance);
    procedure Init;
  public
    { public declarations }
    property ImaginaryInstance: TImaginaryCustomInstance
      read FImaginaryInstance write SetImaginaryInstance;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }
procedure TMainForm.SetImaginaryInstance(const AValue: TImaginaryCustomInstance);
begin
  if (FImaginaryInstance = nil) and (AValue <> nil) then
  begin
    FImaginaryInstance := AValue;
    //FLayout.FImaginaryInstance := AValue;
    Init;
  end;
end;

procedure TMainForm.Init;
begin
  initialized := False;
  Config := ImaginaryInstance.Config;

  initialized := True;

  //UpdateWindowCaption;
end;


procedure TMainForm.MenuFileExitClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  //if Position = poDefault then
  ImaginaryInstance.RestoreMainWindowPosition;
end;

procedure TMainForm.FormHide(Sender: TObject);
begin
  ImaginaryInstance.SaveMainWindowPosition;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  ImaginaryInstance.SaveMainWindowPosition;
end;

end.
