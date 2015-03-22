
{**********************************************************************
 Package pl_GlassDocking.pkg
 is a modification of AnchorDocking Library  (http://wiki.lazarus.freepascal.org/Anchor_Docking)
 for CodeTyphon Studio
 This unit is part of CodeTyphon Project (http://www.pilotlogic.com/)
***********************************************************************}


unit gd_dockingideframe;

{$mode objfpc}{$H+}

interface

uses
  Math, Classes, SysUtils, LCLProc, Forms, Controls, FileUtil, LazFileCache,
  Dialogs, LazConfigStorage, XMLConf, XMLPropStorage, StdCtrls, LCLIntf,
  BaseIDEIntf, ProjectIntf, MacroIntf, IDEDialogs, MenuIntf, LazIDEIntf,
  IDEWindowIntf, IDEOptionsIntf, LResources,
  gd_dockingStr, gd_dockingbase, gd_dockingOptionsDlg,
  gd_dockingideboss;

type


  { TGlassDockIDEFrame }

  TGlassDockIDEFrame = class(TAbstractIDEOptionsEditor)
    EnableCheckBox: TCheckBox;
    NoteLabel: TLabel;
  private
    FSettings: TGlassDockSettings;
  public
    OptionsFrame: TGlassDockOptionsFrame;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function GetTitle: string; override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;


implementation

 {$R *.lfm}


//====================== TGlassDockIDEFrame ======================================

constructor TGlassDockIDEFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FSettings := TGlassDockSettings.Create;
  OptionsFrame := TGlassDockOptionsFrame.Create(Self);

  with OptionsFrame do
  begin
    Name := 'OptionsFrame';
    Flags := [adofShow_ShowHeader];
  end;
end;

destructor TGlassDockIDEFrame.Destroy;
begin
  if FSettings <> nil then
    FSettings.Free;
  FSettings := nil;
  inherited Destroy;
end;

function TGlassDockIDEFrame.GetTitle: string;
begin
  Result := adrsDockingAnchordocking;
end;

procedure TGlassDockIDEFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  if ADialog = nil then
  ;

  if IDEDockMaster = nil then
    exit;

  if IDEDockMaster = varIDEGlassDockMaster then
  begin
    NoteLabel.Visible := False;
    OptionsFrame.Align := alClient;
    OptionsFrame.Parent := Self;
  end
  else
  begin
    NoteLabel.Visible := True;
    NoteLabel.Caption := Format(adrsToUseAnchordockingYouMustFirstUninstall, [DbgSName(IDEDockMaster)]);
    NoteLabel.Hint := Format(adrsThereIsAnotherDockMasterInstalledOnlyOneDockingPac, [DbgSName(IDEDockMaster)]);
    OptionsFrame.Parent := nil;
  end;
end;

procedure TGlassDockIDEFrame.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  if not (AOptions is SupportedOptionsClass) then
    exit;
  if FSettings = nil then
    exit;
  if VarDockMaster = nil then
    exit;

  VarDockMaster.SaveSettings(FSettings);
  OptionsFrame.LoadFromSettings(FSettings);
end;

procedure TGlassDockIDEFrame.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  if not (AOptions is SupportedOptionsClass) then
    exit;
  if FSettings = nil then
    exit;

  OptionsFrame.SaveToSettings(FSettings);
  if (not VarDockMaster.SettingsAreEqual(FSettings)) or (not FileExistsUTF8(varIDEGlassDockMaster.GetRuntimeFilename)) then
  begin
    VarDockMaster.LoadSettings(FSettings);
    VarIDEGlassDockMaster.SaveRuntimeLayout;
  end;
end;

class function TGlassDockIDEFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := IDEEditorGroups.GetByIndex(GroupEnvironment)^.GroupClass;
end;


initialization
 {$i gd_dockingideframe.lrs}

end.
