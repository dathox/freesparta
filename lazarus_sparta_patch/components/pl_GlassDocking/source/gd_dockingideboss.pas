
{**********************************************************************
 Package pl_GlassDocking.pkg
 is a modification of AnchorDocking Library  (http://wiki.lazarus.freepascal.org/Anchor_Docking)
 for CodeTyphon Studio
 This unit is part of CodeTyphon Project (http://www.pilotlogic.com/)
***********************************************************************}

unit gd_dockingideboss;

{$mode objfpc}{$H+}

interface

uses
  Math, Classes, SysUtils, LCLProc, Forms, Controls, FileUtil, LazFileCache,
  Dialogs, LazConfigStorage, XMLConf, XMLPropStorage, StdCtrls, LCLIntf,
  BaseIDEIntf, ProjectIntf, MacroIntf, IDEDialogs, MenuIntf, LazIDEIntf,
  IDEWindowIntf, IDEOptionsIntf, LResources,
  ExtendedNotebook, SrcEditorIntf, LMessages, FormEditingIntf, // DaThoX
  gd_dockingStr, gd_dockingbase, gd_dockingOptionsDlg;

const
  RuntimeConfigFileName = 'GlassDockingRuntime.xml';
  DefaultConfigFileName = 'GlassDockingDefault.xml';

var
  mnuGlassDockSection: TIDEMenuSection;
  mnuGDSaveLayoutAsDefault: TIDEMenuCommand;
  mnuGDSaveLayoutToFile: TIDEMenuCommand;
  mnuGDLoadLayoutFromFile: TIDEMenuCommand;
  mnuGDRestoreDefaultLayout: TIDEMenuCommand;

type

  { TIDEGlassDockMaster }

  TIDEGlassDockMaster = class(TIDEDockMaster)
  private
    FChangeStamp: int64;
    FEnabled: boolean;
    FSavedChangeStamp: int64;
    FSavedDMChangeStamp: int64;
    FUserLayoutLoaded: boolean;
    // DaThoX
    class var FSelectedForm: TCustomForm;
    procedure DockMasterCreateControl(Sender: TObject; aName: string; var AControl: TControl; DoDisableAutoSizing: boolean);
    procedure GetDefaultBounds(AForm: TCustomForm; out Creator: TIDEWindowCreator; out NewBounds: TRect;
      out DockSiblingName: string; out DockAlign: TAlign);
    function GetModified: boolean;
    procedure SetEnabled(const AValue: boolean);
    procedure SetModified(const AValue: boolean);
    procedure SetUserLayoutLoaded(const AValue: boolean);
    // DaThoX
    function ActivateGlassDockHeader(ACtrl: TControl): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure MakeIDEWindowDockSite(AForm: TCustomForm; ASides: TDockSides = [alBottom]); override;
    procedure MakeIDEWindowDockable(AControl: TWinControl); override;
    function  AddableInWindowMenu(AForm: TCustomForm): boolean; override;

    procedure LoadLayoutFromFile(Filename: string);
    procedure SaveLayoutToFile(Filename: string);

    function  GetRuntimeFilename: string;
    procedure LoadRuntimeLayout;
    procedure SaveRuntimeLayout;

    procedure LoadDefaultLayout;
    procedure SaveAsDefaultLayout;    

    procedure ShowForm(AForm: TCustomForm; BringToFront: boolean); override;
    procedure CloseAll; override;
    procedure IncreaseChangeStamp; inline;
    property  UserLayoutLoaded: boolean read FUserLayoutLoaded write SetUserLayoutLoaded;
    
    procedure OnIDERestoreWindows(Sender: TObject);
    function  OnProjectClose(Sender: TObject; AProject: TLazProject): TModalResult;
    // dathox begin
    procedure ActiveFormChanged(Sender: TObject; Form: TCustomForm);
    procedure ActiveControlChanged(Sender: TObject; LastControl: TControl);
    procedure OnUserInputHandler(Sender: TObject; Msg: Cardinal);
    // dathox end
    procedure RestoreDefaultLayoutClicked(Sender: TObject);
    procedure LoadLayoutFromFileClicked(Sender: TObject);
    procedure SaveLayoutToFileClicked(Sender: TObject);
    procedure SaveLayoutAsDefaultClicked(Sender: TObject);

    property  Enabled: boolean read FEnabled write SetEnabled;
    property  ChangeStamp: int64 read FChangeStamp;
    property  Modified: boolean read GetModified write SetModified;
    
  end;

var
  varIDEGlassDockMaster: TIDEGlassDockMaster = nil;

implementation

//===============================================
//===============================================
//===============================================


//====== TIDEGlassDockMaster ==========================

constructor TIDEGlassDockMaster.Create;
begin
  inherited Create;
  fEnabled := True;

  varDockMaster.OnCreateControl := @DockMasterCreateControl;
  varDockMaster.OnShowOptions := @ShowAnchorDockOptions;

  varDockMaster.ShowMenuItemShowHeader := True;
  FHideSimpleLayoutOptions := True;
end;

destructor TIDEGlassDockMaster.Destroy;
begin
  if varDockMaster <> nil then
  begin
    varDockMaster.OnCreateControl := nil;
    varDockMaster.OnShowOptions := nil;
  end;

  inherited Destroy;
end;

procedure TIDEGlassDockMaster.ShowForm(AForm: TCustomForm; BringToFront: boolean);
var
  Parent: TCustomForm;
  Creator: TIDEWindowCreator;
  NewBounds: TRect;
  DockSiblingName: string;
  DockAlign: TAlign;
  DockSibling: TCustomForm;
  NewDockSite: TCustomForm;
  Site: TGlassDockHostSite;
  AControl: TControl;
  NeedPlacing: boolean;
  SiteForm: TCustomForm;
  OldActiveControl: TWinControl;
begin
  if AForm = nil then exit;

  try
    AForm.DisableAlign;

    NeedPlacing := not AForm.IsVisible;
    if varDockMaster.GetSite(AForm) = nil then
    begin
      varDockMaster.MakeDockable(AForm, False);
      NeedPlacing := True;
    end;

    AControl := varDockMaster.GetControl(AForm);

    if (AControl <> nil) and NeedPlacing and varDockMaster.IsFloating(AForm) then
    begin

      GetDefaultBounds(AForm, Creator, NewBounds, DockSiblingName, DockAlign);

      if Creator <> nil then
      begin
        SiteForm := GetParentForm(AForm);
        SiteForm.BoundsRect := NewBounds;
        SiteForm.UndockWidth := NewBounds.Right - NewBounds.Left;
        SiteForm.UndockHeight := NewBounds.Bottom - NewBounds.Top;
        Site := varDockMaster.GetAnchorSite(SiteForm); // 7777
        if (Site <> nil) and (DockSiblingName <> '') then
        begin
          DockSibling := Screen.FindForm(DockSiblingName);
          if DockSibling <> nil then
          begin
            NewDockSite := varDockMaster.GetSite(DockSibling);
            if NewDockSite <> nil then
            begin
              varDockMaster.ManualDock(Site, NewDockSite, DockAlign);
            end;
          end;
        end;
      end;
    end;

  finally
    OldActiveControl := AForm.ActiveControl;

    varDockMaster.MakeVisible(AForm, BringToFront);
    AForm.EnableAlign;

    if BringToFront then
    begin
      if (OldActiveControl = nil) or (not OldActiveControl.HandleAllocated) or (FindControl(GetFocus) <> OldActiveControl) then
      begin
        Parent := GetParentForm(AForm);
        Parent.ShowOnTop;
        if (OldActiveControl <> nil) and OldActiveControl.CanFocus then
        begin
          Parent.ActiveControl := OldActiveControl;
          Parent.SetFocus;
        end;
      end;
    end;
  end;

end;

procedure TIDEGlassDockMaster.DockMasterCreateControl(Sender: TObject; aName: string; var AControl: TControl; DoDisableAutoSizing: boolean);
begin
  AControl := IDEWindowCreators.GetForm(aName, True, DoDisableAutoSizing);
end;

procedure TIDEGlassDockMaster.GetDefaultBounds(AForm: TCustomForm;
                                               out Creator: TIDEWindowCreator;
                                               out NewBounds: TRect;
                                               out DockSiblingName: string;
                                               out DockAlign: TAlign);
var
  AControl: TControl;
begin
  NewBounds := Rect(0, 0, 0, 0);
  DockSiblingName := '';
  DockAlign := alNone;

  // get the embedded control
  AControl := varDockMaster.GetControl(AForm);

  if not (AControl is TCustomForm) then exit;

  AForm := TCustomForm(AControl);

  Creator := IDEWindowCreators.FindWithName(AForm.Name);

  if Creator = nil then exit;

  if Creator.OnGetLayout <> nil then
  begin
    Creator.OnGetLayout(Self, AForm.Name, NewBounds, DockSiblingName, DockAlign);
  end
  else
  begin
    Creator.GetDefaultBounds(AForm, NewBounds);
    DockSiblingName := Creator.DockSibling;
    DockAlign := Creator.DockAlign;
  end;

  NewBounds.Left := Min(10000, Max(-10000, NewBounds.Left));
  NewBounds.Top := Min(10000, Max(-10000, NewBounds.Top));
  NewBounds.Right := Max(NewBounds.Left + 100, NewBounds.Right);
  NewBounds.Bottom := Max(NewBounds.Top + 100, NewBounds.Bottom);
end;

function TIDEGlassDockMaster.GetModified: boolean;
begin
  Result := True;
  if FChangeStamp = FSavedChangeStamp then exit;
  if varDockMaster.OptionsChangeStamp = FSavedDMChangeStamp then exit;
  Result := False;
end;

procedure TIDEGlassDockMaster.SetEnabled(const AValue: boolean);
begin
  if FEnabled = AValue then exit;
  FEnabled := AValue;
  IncreaseChangeStamp;
end;

procedure TIDEGlassDockMaster.SetModified(const AValue: boolean);
begin
  if AValue then
  begin
    IncreaseChangeStamp;
  end
  else
  begin
    FSavedChangeStamp := FChangeStamp;
    FSavedDMChangeStamp := varDockMaster.OptionsChangeStamp;
  end;
end;

procedure TIDEGlassDockMaster.SetUserLayoutLoaded(const AValue: boolean);
begin
  if FUserLayoutLoaded = AValue then exit;
  FUserLayoutLoaded := AValue;
end;

// dathox begin
function TIDEGlassDockMaster.ActivateGlassDockHeader(ACtrl: TControl): Boolean;
var
  LParent: TComponent;
  c: TComponent;
  LHeader: TGlassDockHeader;

  function CheckComponent(AComponent: TComponent): Boolean;
  begin
    Result := False;
    { bingo! now we can activate header and exit }
    if AComponent is TGlassDockHeader then
    begin
      LHeader := TGlassDockHeader(AComponent);

      { Splitter also have TGlassDockHeader, we must check this }
      if (LHeader.Parent = nil) then
        Exit(True);

      TGlassDockHeader.SelectedHeader := LHeader;
      Exit(True);
    end
    else
    // fix for closing wrong tabs
    if AComponent is TExtendedNotebook then
    begin
      if AComponent.Owner is TSourceEditorWindowInterface then
        SourceEditorManagerIntf.ActiveSourceWindow := TSourceEditorWindowInterface(AComponent.Owner);
    end;
  end;

begin
    LParent := ACtrl;

  while LParent <> nil do
  begin
    for c in LParent do
      if CheckComponent(c) then
        Exit;
    if LParent.HasParent then
      LParent := LParent.GetParentComponent
    else
      Exit;
  end;
end;

procedure TIDEGlassDockMaster.ActiveFormChanged(Sender: TObject;
  Form: TCustomForm);
begin
  if (Form <> FSelectedForm) and (Form.Parent = nil) and not IsFormDesign(Form) then
  begin
    ActivateGlassDockHeader(Form.ActiveControl);
    FSelectedForm := Form;
  end;
end;

procedure TIDEGlassDockMaster.ActiveControlChanged(Sender: TObject;
  LastControl: TControl);
var
  LForm: TCustomForm;
begin
  if LastControl = nil then
    Exit;

  LForm := TCustomForm(LastControl.GetTopParent);
  ActivateGlassDockHeader(LastControl);
  FSelectedForm := LForm; // for ActiveFormChanged
end;

procedure TIDEGlassDockMaster.OnUserInputHandler(Sender: TObject; Msg: Cardinal);
var
  LCtrl: TControl;
begin
  if (Msg = LM_LBUTTONDOWN) or (Msg = LM_RBUTTONDOWN) then
  begin
    LCtrl := FindDragTarget(Mouse.CursorPos, True);
    ActivateGlassDockHeader(LCtrl);
  end;
end;
// dathox end

procedure TIDEGlassDockMaster.MakeIDEWindowDockSite(AForm: TCustomForm; ASides: TDockSides);
var
  aManager: TGlassDockManager;
begin

  varDockMaster.MakeDockSite(AForm, [akBottom], admrpChild);

  if AForm.DockManager is TGlassDockManager then
  begin
    aManager := TGlassDockManager(AForm.DockManager);
    aManager.PreferredSiteSizeAsSiteMinimum := False;
  end;
end;

procedure TIDEGlassDockMaster.MakeIDEWindowDockable(AControl: TWinControl);
begin
  varDockMaster.MakeDockable(AControl, False);
end;

function TIDEGlassDockMaster.AddableInWindowMenu(AForm: TCustomForm): boolean;
begin
  Result := False;
  if AForm is TGlassDockHostSite then exit;

  if (varDockMaster.FindControl(AForm.Name) = nil) and (AForm.Parent <> nil) then exit;
  Result := True;
end;

function TIDEGlassDockMaster.GetRuntimeFilename: string;
begin
  Result := AppendPathDelim(LazarusIDE.GetPrimaryConfigPath) + RuntimeConfigFileName;
end;

procedure TIDEGlassDockMaster.LoadDefaultLayout;
var
  BaseDir: string;
  xFilename: string;
begin
  BaseDir := '$PkgDir(pl_glassdocking)';
  IDEMacros.SubstituteMacros(BaseDir);

  if (BaseDir <> '') and DirectoryExistsUTF8(BaseDir) then
  begin
    xFilename := AppendPathDelim(BaseDir) + DefaultConfigFileName;

    if FileExistsUTF8(xFilename) then
      LoadLayoutFromFile(xFilename);

  end;
  Modified := False;
end;

procedure TIDEGlassDockMaster.SaveAsDefaultLayout;
var
  BaseDir: string;
  xFilename: string;
begin
  BaseDir := '$PkgDir(pl_glassdocking)';
  IDEMacros.SubstituteMacros(BaseDir);

  if (BaseDir <> '') and DirectoryExistsUTF8(BaseDir) then
  begin
    xFilename := AppendPathDelim(BaseDir) + DefaultConfigFileName;
    SaveLayoutToFile(xFilename);
  end;
  Modified := False;
end;

procedure TIDEGlassDockMaster.LoadRuntimeLayout;
var
  xFilename: string;
begin
  xFilename := GetRuntimeFilename;

  if FileExistsUTF8(xFilename) then
    LoadLayoutFromFile(xFilename)
  else
    LoadDefaultLayout;

  Modified := False;
end;

procedure TIDEGlassDockMaster.SaveRuntimeLayout;
var
  xFilename: string;
begin
 if varDockMaster=nil then exit;

 if varDockMaster.SaveOnClose=false then exit;
 
  xFilename := GetRuntimeFilename;
  SaveLayoutToFile(xFilename);
  Modified := False;
end;

//=======================================================================
procedure TIDEGlassDockMaster.LoadLayoutFromFile(Filename: string);
var
  Config: TConfigStorage;
begin

  try
    Config := GetIDEConfigStorage(Filename, True);
    try
      if not varDockMaster.ConfigIsEmpty(Config) then
      begin
        // loading last layout
        varDockMaster.LoadSettingsFromConfig(Config);
        varDockMaster.LoadLayoutFromConfig(Config, True);
        UserLayoutLoaded := True;
      end
      else
      begin
        LoadDefaultLayout;
      end;
    finally
      // Config.Free; 7777
    end;
  except
    on E: Exception do
    begin
      LoadDefaultLayout;
    end;
  end;
  Modified := False;
end;

procedure TIDEGlassDockMaster.SaveLayoutToFile(Filename: string);  // 7777
var
  XMLConfig: TXMLConfig;
  Config: TXMLConfigStorage;
begin
  XMLConfig := TXMLConfig.Create(nil);
  try
    XMLConfig.StartEmpty := True;
    XMLConfig.Filename := Filename;
    Config := TXMLConfigStorage.Create(XMLConfig);
    try
      VarDockMaster.SaveSettingsToConfig(Config);
      varDockMaster.SaveLayoutToConfig(Config);
    finally
      // Config.Free;   7777
    end;
    XMLConfig.Flush;
  finally
    XMLConfig.Free;
  end;
end;

//=======================================================================

procedure TIDEGlassDockMaster.CloseAll;
begin
  varDockMaster.CloseAll;
end;

function TIDEGlassDockMaster.OnProjectClose(Sender: TObject; AProject: TLazProject): TModalResult;
begin
  Result := mrOk;
  if AProject = nil then
    exit;
  SaveRuntimeLayout;
end;

procedure TIDEGlassDockMaster.OnIDERestoreWindows(Sender: TObject);
begin
  // localize menu captions
  mnuGDSaveLayoutAsDefault.Caption := adrsSaveWindowLayoutAsDefault;
  mnuGDSaveLayoutToFile.Caption := adrsSaveWindowLayoutToFile;
  mnuGDLoadLayoutFromFile.Caption := adrsLoadWindowLayoutFromFile;
  mnuGDRestoreDefaultLayout.Caption := adrsRestoreDefaultLayout;

  LoadRuntimeLayout;
end;

procedure TIDEGlassDockMaster.RestoreDefaultLayoutClicked(Sender: TObject);
begin
  LoadDefaultLayout;
end;

procedure TIDEGlassDockMaster.SaveLayoutAsDefaultClicked(Sender: TObject);
begin
  SaveAsDefaultLayout;
end;

//=========================================
procedure TIDEGlassDockMaster.LoadLayoutFromFileClicked(Sender: TObject);
var
  Dlg: TOpenDialog;
  Filename: string;
begin
  Dlg := TOpenDialog.Create(nil);
  try
    InitIDEFileDialog(Dlg);
    Dlg.Title := adrsLoadWindowLayoutFromFileXml;
    Dlg.Options := Dlg.Options + [ofFileMustExist];
    Dlg.Filter := adrsAnchorDockingLayout + '|*.xml|' + adrsAllFiles + '|' + AllFilesMask;
    if Dlg.Execute then
    begin
      Filename := CleanAndExpandFilename(Dlg.FileName);
      try
        LoadLayoutFromFile(Filename);
      except
        on E: Exception do
        begin
          IDEMessageDialog(adrsError,
            Format(adrsErrorLoadingWindowLayoutFromFile, [Filename, #13, E.Message]),
            mtError, [mbCancel]);
        end;
      end;
    end;
    StoreIDEFileDialog(Dlg);
  finally
    Dlg.Free;
  end;
end;

procedure TIDEGlassDockMaster.SaveLayoutToFileClicked(Sender: TObject);
var
  Dlg: TSaveDialog;
  Filename: string;
begin
  Dlg := TSaveDialog.Create(nil);
  try
    InitIDEFileDialog(Dlg);
    Dlg.Title := adrsSaveWindowLayoutToFileXml;
    Dlg.Options := Dlg.Options + [ofPathMustExist, ofNoReadOnlyReturn, ofOverwritePrompt];
    Dlg.Filter := adrsAnchorDockingLayout + '|*.xml|' + adrsAllFiles + '|' + AllFilesMask;
    if Dlg.Execute then
    begin
      Filename := CleanAndExpandFilename(Dlg.FileName);
      if ExtractFileExt(Filename) = '' then
        Filename := Filename + '.xml';
      try
        SaveLayoutToFile(Filename);
      except
        on E: Exception do
        begin
          IDEMessageDialog(adrsError,
            Format(adrsErrorWritingWindowLayoutToFile, [Filename, #13, E.Message]),
            mtError, [mbCancel]);
        end;
      end;
    end;
    StoreIDEFileDialog(Dlg);
  finally
    Dlg.Free;
  end;
end;

//================================

procedure TIDEGlassDockMaster.IncreaseChangeStamp;
begin
  LUIncreaseChangeStamp64(FChangeStamp);
end;

end.
