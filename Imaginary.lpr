program Imaginary;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, ImaginaryForm, Inifiles, ImaginaryConfig, ImaginaryTypes,
  ImaginaryInstance, ImageMagick;

{$R *.res}

type

  { TMyImaginaryInstance }

  TMyImaginaryInstance = class(TImaginaryInstance);

var
  ActualConfig: TIniFile;
  ImaginaryApplication: TMyImaginaryInstance;

begin
  ActualConfig := GetActualConfig;
  RequireDerivedFormResource:=True;
  Application.Initialize;

  ImaginaryApplication := TMyImaginaryInstance.Create;
  ImaginaryApplication.UseConfig(ActualConfig);

  begin
    ImaginaryApplication.Show;
    Application.Run;
  end;

  ImaginaryApplication.Hide;
  Application.ProcessMessages;

  ImaginaryApplication.Free;

end.

