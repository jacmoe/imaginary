unit ImaginaryTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, Controls, IniFiles, ImaginaryConfig;

const
  ImaginaryCurrentVersionOnly = '0.1.0';
  {$IFDEF CPU64}
    ImaginaryProcessorInfo = ' (64-bit)';
  {$ELSE}
    ImaginaryProcessorInfo = ' (32-bit)';
  {$ENDIF}
  ImaginaryCurrentVersion : String=ImaginaryCurrentVersionOnly + ImaginaryProcessorInfo;

type
  TImaginaryCustomInstance = class;
  TImaginaryInstanceEvent = procedure(AInstance : TImaginaryCustomInstance) of object;

  TImaginaryCustomInstance = class(TInterfacedObject,IConfigProvider)
  protected
    FRestartQuery: boolean;
    function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} IID: TGUID; out Obj): HResult; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
    function _AddRef: Integer; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
    function _Release: Integer; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};

    function GetConfig: TImaginaryConfig; virtual; abstract;

    function GetFullscreen: boolean; virtual; abstract;
    procedure SetFullscreen(AValue: boolean); virtual; abstract;
  public
    Title, AboutText: string;
    constructor Create; virtual; abstract;
    constructor Create(AEmbedded: boolean); virtual; abstract;
    procedure Show; virtual; abstract;
    procedure Hide; virtual; abstract;
    procedure Run; virtual; abstract;
    procedure SaveMainWindowPosition; virtual; abstract;
    procedure RestoreMainWindowPosition; virtual; abstract;
    procedure UseConfig(ini: TInifile); virtual; abstract;
    property Config: TImaginaryConfig read GetConfig;
  end;

implementation

{ Interface gateway }
function TImaginaryCustomInstance.QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} IID: TGUID; out Obj): HResult; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
begin
  if GetInterface(iid, obj) then
    Result := S_OK
  else
    Result := longint(E_NOINTERFACE);
end;

{ There is no automatic reference counting, but it is compulsory to define these functions }
function TImaginaryCustomInstance._AddRef: Integer; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
begin
  result := 0;
end;

function TImaginaryCustomInstance._Release: Integer; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
begin
  result := 0;
end;

end.

