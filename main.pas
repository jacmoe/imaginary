unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  IniPropStorage, Menus, XMLPropStorage;

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

procedure TMainForm.StoreFormState;
begin
  with AppPropertyStore do begin
       WriteInteger('NormalLeft', Left);
       WriteInteger('NormalTop', Top);
       WriteInteger('NormalWidth', Width);
       WriteInteger('NormalHeight', Height);

       WriteInteger('RestoredLeft', RestoredLeft);
       WriteInteger('RestoredTop', RestoredTop);
       WriteInteger('RestoredWidth', RestoredWidth);
       WriteInteger('RestoredHeight', RestoredHeight);

       WriteInteger('WindowState', Integer(WindowState));
     end;
end;

procedure TMainForm.RestoreFormState;
var
  LastWindowState: TWindowState;
begin
  with AppPropertyStore do begin
       LastWindowState := TWindowState(ReadInteger('WindowState', Integer(WindowState)));

       if LastWindowState = wsMaximized then begin
         WindowState := wsNormal;
         BoundsRect := Bounds(
           ReadInteger('RestoredLeft', RestoredLeft),
           ReadInteger('RestoredTop', RestoredTop),
           ReadInteger('RestoredWidth', RestoredWidth),
           ReadInteger('RestoredHeight', RestoredHeight));
         WindowState := wsMaximized;
       end else begin
         WindowState := wsNormal;
         BoundsRect := Bounds(
           ReadInteger('NormalLeft', Left),
           ReadInteger('NormalTop', Top),
           ReadInteger('NormalWidth', Width),
           ReadInteger('NormalHeight', Height));
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

