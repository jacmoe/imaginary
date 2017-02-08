unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  IniPropStorage, Menus;

type

  { TMainForm }

  TMainForm = class(TForm)
    AppPropertyStore: TIniPropStorage;
    MainMenu: TMainMenu;
    MenuFile: TMenuItem;
    MenuFileExit: TMenuItem;
    procedure StoreformState;
    procedure RestoreFormState;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure MenuFileExitClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

//uses
//  magick_wand, ImageMagick, IntfGraphics, FPimage, LazUTF8;
uses
  uparse;

procedure TMainForm.StoreFormState;
var
  currentScreensize, restoredScreensize: TRect;
begin
  with AppPropertyStore do
  begin
    currentScreensize := Rect(Left, Top, Left + Width, Top + Height);
    WriteString('ScreenSize', RectToStr(currentScreensize));

    restoredScreensize := Rect(RestoredLeft, RestoredTop, RestoredLeft + RestoredWidth, RestoredTop + RestoredHeight);
    WriteString('RestoredScreenSize', RectToStr(restoredScreensize));

    WriteInteger('WindowState', integer(WindowState));
  end;
end;

procedure TMainForm.RestoreFormState;
var
  LastWindowState: TWindowState;
  rect : TRect;
begin
  with AppPropertyStore do
  begin
    LastWindowState := TWindowState(ReadInteger('WindowState', integer(WindowState)));

    if LastWindowState = wsMaximized then
    begin
      WindowState := wsNormal;
      rect := StrToRect(ReadString('ScreenSize', ''));
      //BoundsRect := Bounds();
      WindowState := wsMaximized;
    end
    else
    begin
      WindowState := wsNormal;
      rect := StrToRect(ReadString('RestoredScreenSize', ''));
      //BoundsRect := Bounds();
    end;
  end;
end;

procedure TMainForm.MenuFileExitClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  StoreFormState;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  RestoreFormState;
end;

end.
