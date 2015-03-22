{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Maciej Izak

  DaThoX 2004-2015
  FreeSparta.com
}

unit dtx_sparta_ProfessionalRegister;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, dtx_sparta_StarterRegister, SpartaAPI, sparta_EDTU_AnchorDocking, sparta_EDTU_Form, sparta_EDTU_NonVisualComponents,
  Generics.Collections;

type

  { TProfessionalDesignTimeUtilsManager }

  TProfessionalDesignTimeUtilsManager = class(TStarterDesignTimeUtilsManager)
  private
    FEDTU: TList<TEDTUClass>;
  protected
    function GetEDTUCount: Integer; override;
    function GetEDTU(Index: Integer): TEDTUClass; override;
  public
    procedure RegisterEDTU(AEDTUClass: TEDTUClass); override;
    procedure UnregisterEDTU(AEDTUClass: TEDTUClass); override;
    constructor Create;
    destructor Destroy; override;
  end;

procedure Register;

implementation

{$R edtu.res}

var
  Manager: TProfessionalDesignTimeUtilsManager = nil;

procedure Register;
begin
  Manager := TProfessionalDesignTimeUtilsManager.Create;
  DTUManager := Manager;
  DTUManager.RegisterEDTU(TedtuNonVisual);
  DTUManager.RegisterEDTU(TedtuAnchor);
  DTUManager.RegisterEDTU(TedtuForm);
end;

{ TProfessionalDesignTimeUtilsManager }

function TProfessionalDesignTimeUtilsManager.GetEDTUCount: Integer;
begin
  Result := FEDTU.Count;
end;

function TProfessionalDesignTimeUtilsManager.GetEDTU(Index: Integer
  ): TEDTUClass;
begin
  Result := FEDTU[Index];
end;

procedure TProfessionalDesignTimeUtilsManager.RegisterEDTU(
  AEDTUClass: TEDTUClass);
begin
  FEDTU.Add(AEDTUClass);
end;

procedure TProfessionalDesignTimeUtilsManager.UnregisterEDTU(
  AEDTUClass: TEDTUClass);
begin
  FEDTU.Remove(AEDTUClass);
end;

constructor TProfessionalDesignTimeUtilsManager.Create;
begin
  FEDTU := TList<TEDTUClass>.Create;
end;

destructor TProfessionalDesignTimeUtilsManager.Destroy;
begin
  FEDTU.Free;
  inherited Destroy;
end;

finalization
  Manager.Free;
end.

