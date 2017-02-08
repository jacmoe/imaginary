program Imaginary;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, main, Inifiles, ImaginaryConfig, ImaginaryTypes, ImaginaryInstance
  { you can add units after this };

{$R *.res}

var
  ActualConfig: TIniFile;

begin
  ActualConfig := GetActualConfig;
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

