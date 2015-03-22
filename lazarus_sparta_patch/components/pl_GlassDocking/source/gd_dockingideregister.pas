
{**********************************************************************
 Package pl_GlassDocking.pkg
 is a modification of AnchorDocking Library  (http://wiki.lazarus.freepascal.org/Anchor_Docking)
 for CodeTyphon Studio
 This unit is part of CodeTyphon Project (http://www.pilotlogic.com/)
***********************************************************************}

unit gd_dockingideregister;

{$mode objfpc}{$H+}

interface

uses
  Math, Classes, SysUtils, LCLProc, Forms, Controls, FileUtil, LazFileCache,
  Dialogs, LazConfigStorage, XMLConf, XMLPropStorage, StdCtrls, LCLIntf,
  ProjectIntf, MenuIntf, LazIDEIntf,
  IDEWindowIntf, IDEOptionsIntf, LResources,
  gd_dockingStr, gd_dockingbase, gd_dockingOptionsDlg,
  gd_dockingideboss, gd_dockingideframe;

var
  AnchorDockOptionsID: integer = 1000;

procedure Register;

implementation

procedure Register;
begin
  if not (IDEDockMaster is TIDEGlassDockMaster) then
    exit;

  LazarusIDE.AddHandlerOnIDERestoreWindows(@varIDEGlassDockMaster.OnIDERestoreWindows);
  LazarusIDE.AddHandlerOnProjectClose(@varIDEGlassDockMaster.OnProjectClose);

  Screen.AddHandlerActiveFormChanged(@varIDEGlassDockMaster.ActiveFormChanged);
  Screen.AddHandlerActiveControlChanged(@varIDEGlassDockMaster.ActiveControlChanged);
  Application.AddOnUserInputHandler(@varIDEGlassDockMaster.OnUserInputHandler);

  // add menu section
  // As this procedure seems to be called too early, menuitems names will be
  // not localized. So we will localize them in TIDEGlassDockMaster.OnIDERestoreWindows for now
  mnuGlassDockSection := RegisterIDEMenuSection(mnuWindow, 'AnchorDocking');

  mnuGDSaveLayoutAsDefault := RegisterIDEMenuCommand(mnuGlassDockSection, 'ADSaveLayoutAsDefault',
    adrsSaveWindowLayoutAsDefault, @varIDEGlassDockMaster.SaveLayoutAsDefaultClicked);
  mnuGDSaveLayoutToFile := RegisterIDEMenuCommand(mnuGlassDockSection, 'ADSaveLayoutToFile',
    adrsSaveWindowLayoutToFile, @varIDEGlassDockMaster.SaveLayoutToFileClicked);
  mnuGDLoadLayoutFromFile := RegisterIDEMenuCommand(mnuGlassDockSection, 'ADLoadLayoutFromFile',
    adrsLoadWindowLayoutFromFile, @varIDEGlassDockMaster.LoadLayoutFromFileClicked);
  mnuGDRestoreDefaultLayout := RegisterIDEMenuCommand(mnuGlassDockSection, 'ADRestoreDefaultLayout',
    adrsRestoreDefaultLayout, @varIDEGlassDockMaster.RestoreDefaultLayoutClicked);

  // add options frame

  AnchorDockOptionsID := RegisterIDEOptionsEditor(GroupEnvironment, TGlassDockIDEFrame, AnchorDockOptionsID)^.Index;
end;

procedure _GlassDockingInit;
begin
  if varDockMaster = nil then
    varDockMaster := TGlassDockMaster.Create(nil);
  if IDEDockMaster = nil then
  begin
    varIDEGlassDockMaster := TIDEGlassDockMaster.Create;
    IDEDockMaster := varIDEGlassDockMaster;
  end;
end;

procedure _GlassDockingfree;
begin
  if IDEDockMaster <> nil then
    IDEDockMaster.Free;
  IDEDockMaster := nil;
  if varDockMaster <> nil then
    varDockMaster.Free;
  varDockMaster := nil;
end;

//==============================================
initialization
  _GlassDockingInit;

finalization
  _GlassDockingfree;

end.
