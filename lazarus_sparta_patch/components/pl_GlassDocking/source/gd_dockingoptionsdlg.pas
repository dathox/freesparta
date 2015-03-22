
{**********************************************************************
 Package pl_GlassDocking.pkg
 is a modification of AnchorDocking Library  (http://wiki.lazarus.freepascal.org/Anchor_Docking)
 for CodeTyphon Studio
 This unit is part of CodeTyphon Project (http://www.pilotlogic.com/)
***********************************************************************}

unit gd_dockingoptionsdlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  StdCtrls, ComCtrls, LCLProc, ColorBox, gd_dockingbase, gd_dockingStr;

type
  TAnchorDockOptionsFlag = (
    adofShow_ShowHeader,
    adofShow_ShowSaveOnClose
    );
  TAnchorDockOptionsFlags = set of TAnchorDockOptionsFlag;

  { TGlassDockOptionsFrame }

  TGlassDockOptionsFrame = class(TFrame)
    ButtonSetDafault: TButton;
    HeaderColorComboBox: TColorBox;
    DragThresholdLabel: TLabel;
    DragThresholdTrackBar: TTrackBar;
    HeaderAlignLeftLabel: TLabel;
    HeaderAlignLeftTrackBar: TTrackBar;
    HeaderAlignTopLabel: TLabel;
    HeaderAlignTopTrackBar: TTrackBar;
    HeaderColor: TLabel;
    HideHeaderCaptionForFloatingCheckBox: TCheckBox;
    SaveLayoutOnCloseCheckBox: TCheckBox;
    ScaleOnResizeCheckBox: TCheckBox;
    ShowHeaderCaptionCheckBox: TCheckBox;
    ShowHeaderCheckBox: TCheckBox;
    SplitterWidthLabel: TLabel;
    SplitterWidthTrackBar: TTrackBar;
    procedure ButtonSetDafaultClick(Sender: TObject);
    procedure OkClick(Sender: TObject);
    procedure DragThresholdTrackBarChange(Sender: TObject);
    procedure HeaderAlignLeftTrackBarChange(Sender: TObject);
    procedure HeaderAlignTopTrackBarChange(Sender: TObject);
    procedure SplitterWidthTrackBarChange(Sender: TObject);
  private
    FFlags: TAnchorDockOptionsFlags;
    FMaster: TGlassDockMaster;
    FSettings: TGlassDockSettings;
    procedure SetFlags(AValue: TAnchorDockOptionsFlags);
    procedure SetMaster(const AValue: TGlassDockMaster);
    procedure SetSettings(AValue: TGlassDockSettings);
    procedure UpdateDragThresholdLabel;
    procedure UpdateHeaderAlignTopLabel;
    procedure UpdateHeaderAlignLeftLabel;
    procedure UpdateSplitterWidthLabel;
    procedure ApplyFlags;
  public
    procedure SaveToMaster;
    procedure LoadFromMaster;
    procedure LoadDefault;
    procedure SaveToSettings(TheSettings: TGlassDockSettings);
    procedure LoadFromSettings(TheSettings: TGlassDockSettings);
    property Master: TGlassDockMaster read FMaster write SetMaster;
    property Settings: TGlassDockSettings read FSettings write SetSettings;
    property Flags: TAnchorDockOptionsFlags read FFlags write SetFlags;
  end;

function ShowAnchorDockOptions(ADockMaster: TGlassDockMaster): TModalResult;

implementation

function ShowAnchorDockOptions(ADockMaster: TGlassDockMaster): TModalResult;
var
  Dlg: TForm;
  OptsFrame: TGlassDockOptionsFrame;
  BtnPanel: TButtonPanel;
begin
  Dlg := TForm.Create(nil);
  try
    Dlg.DisableAutoSizing;
    Dlg.Position := poScreenCenter;
    Dlg.AutoSize := True;
    Dlg.Caption := adrsGeneralDockingOptions;

    OptsFrame := TGlassDockOptionsFrame.Create(Dlg);
    OptsFrame.Align := alClient;
    OptsFrame.Parent := Dlg;
    OptsFrame.Master := ADockMaster;

    BtnPanel := TButtonPanel.Create(Dlg);
    BtnPanel.ShowButtons := [pbOK, pbCancel];
    BtnPanel.OKButton.OnClick := @OptsFrame.OkClick;
    BtnPanel.Parent := Dlg;
    Dlg.EnableAutoSizing;
    Result := Dlg.ShowModal;
  finally
    Dlg.Free;
  end;
end;

{$R *.lfm}

procedure TGlassDockOptionsFrame.HeaderAlignLeftTrackBarChange(Sender: TObject);
begin
  UpdateHeaderAlignLeftLabel;
end;

procedure TGlassDockOptionsFrame.HeaderAlignTopTrackBarChange(Sender: TObject);
begin
  UpdateHeaderAlignTopLabel;
end;

procedure TGlassDockOptionsFrame.SplitterWidthTrackBarChange(Sender: TObject);
begin
  UpdateSplitterWidthLabel;
end;

procedure TGlassDockOptionsFrame.OkClick(Sender: TObject);
begin
  if Settings <> nil then
    SaveToSettings(Settings);
  if Master <> nil then
    SaveToMaster;
end;

procedure TGlassDockOptionsFrame.ButtonSetDafaultClick(Sender: TObject);
begin
  LoadDefault;
end;

procedure TGlassDockOptionsFrame.DragThresholdTrackBarChange(Sender: TObject);
begin
  UpdateDragThresholdLabel;
end;

procedure TGlassDockOptionsFrame.SetMaster(const AValue: TGlassDockMaster);
begin
  if FMaster = AValue then
    exit;
  FMaster := AValue;
  if Master <> nil then
    LoadFromMaster;
end;

procedure TGlassDockOptionsFrame.SetFlags(AValue: TAnchorDockOptionsFlags);
begin
  if FFlags = AValue then
    Exit;
  FFlags := AValue;
  ApplyFlags;
end;

procedure TGlassDockOptionsFrame.SetSettings(AValue: TGlassDockSettings);
begin
  if FSettings = AValue then
    Exit;
  FSettings := AValue;
  if Settings <> nil then
    LoadFromSettings(Settings);
end;

procedure TGlassDockOptionsFrame.UpdateDragThresholdLabel;
begin
  DragThresholdLabel.Caption := adrsDragThreshold + ' (' + IntToStr(DragThresholdTrackBar.Position) + ')';
end;

procedure TGlassDockOptionsFrame.UpdateHeaderAlignTopLabel;
begin
  HeaderAlignTopLabel.Caption := adrsHeaderAlignTop + ' (' + IntToStr(HeaderAlignTopTrackBar.Position) + ')';
end;

procedure TGlassDockOptionsFrame.UpdateHeaderAlignLeftLabel;
begin
  HeaderAlignLeftLabel.Caption := adrsHeaderAlignLeft + ' (' + IntToStr(HeaderAlignLeftTrackBar.Position) + ')';
end;

procedure TGlassDockOptionsFrame.UpdateSplitterWidthLabel;
begin
  SplitterWidthLabel.Caption := adrsSplitterWidth + ' (' + IntToStr(SplitterWidthTrackBar.Position) + ')';
end;

procedure TGlassDockOptionsFrame.ApplyFlags;
begin
  ShowHeaderCheckBox.Visible := adofShow_ShowHeader in Flags;
end;

procedure TGlassDockOptionsFrame.SaveToMaster;
var
  CurSettings: TGlassDockSettings;
begin
  CurSettings := TGlassDockSettings.Create;
  try
    Master.SaveSettings(CurSettings);
    SaveToSettings(CurSettings);
    Master.LoadSettings(CurSettings);
  finally
    CurSettings.Free;
  end;
end;

procedure TGlassDockOptionsFrame.LoadFromMaster;
var
  CurSettings: TGlassDockSettings;
begin
  CurSettings := TGlassDockSettings.Create;
  try
    Master.SaveSettings(CurSettings);
    LoadFromSettings(CurSettings);
  finally
    CurSettings.Free;
  end;
end;

procedure TGlassDockOptionsFrame.LoadDefault;
var
  CurSettings: TGlassDockSettings;
begin
  CurSettings := TGlassDockSettings.Create;
  try
    CurSettings.ResetToDefault;
    LoadFromSettings(CurSettings);
  finally
    CurSettings.Free;
  end;
end;

procedure TGlassDockOptionsFrame.SaveToSettings(TheSettings: TGlassDockSettings);
begin

  if (HeaderColorComboBox.Selected = clNone) or (HeaderColorComboBox.Selected = clDefault) then
    TheSettings.HeaderColor := cnHeaderColorDefault
  else
    TheSettings.HeaderColor := HeaderColorComboBox.Selected;

  TheSettings.HeaderColor := HeaderColorComboBox.Selected;
  TheSettings.DragTreshold := DragThresholdTrackBar.Position;
  TheSettings.HeaderAlignTop := HeaderAlignTopTrackBar.Position;
  TheSettings.HeaderAlignLeft := HeaderAlignLeftTrackBar.Position;
  TheSettings.SplitterWidth := SplitterWidthTrackBar.Position;
  TheSettings.ScaleOnResize := ScaleOnResizeCheckBox.Checked;
  TheSettings.SaveOnClose:=SaveLayoutOnCloseCheckBox.Checked;
  TheSettings.ShowHeader := ShowHeaderCheckBox.Checked;
  TheSettings.ShowHeaderCaption := ShowHeaderCaptionCheckBox.Checked;
  TheSettings.HideHeaderCaptionFloatingControl := HideHeaderCaptionForFloatingCheckBox.Checked;
end;

procedure TGlassDockOptionsFrame.LoadFromSettings(TheSettings: TGlassDockSettings);
begin

  HeaderColorComboBox.Selected := TheSettings.HeaderColor;

  DragThresholdTrackBar.Hint := adrsAmountOfPixelTheMouseHasToDragBeforeDragStarts;
  DragThresholdTrackBar.Position := TheSettings.DragTreshold;
  UpdateDragThresholdLabel;

  HeaderAlignTopTrackBar.Hint := adrsMoveHeaderToTopWhenWidthHeight100HeaderAlignTop;
  HeaderAlignTopTrackBar.Position := TheSettings.HeaderAlignTop;
  UpdateHeaderAlignTopLabel;

  HeaderAlignLeftTrackBar.Hint := adrsMoveHeaderToLeftWhenWidthHeight100HeaderAlignLeft;
  HeaderAlignLeftTrackBar.Position := TheSettings.HeaderAlignLeft;
  UpdateHeaderAlignLeftLabel;

  SplitterWidthTrackBar.Hint := adrsSplitterThickness;
  SplitterWidthTrackBar.Position := TheSettings.SplitterWidth;
  UpdateSplitterWidthLabel;

  SaveLayoutOnCloseCheckBox.Caption:=adrsSaveLayoutOnClose;
  SaveLayoutOnCloseCheckBox.Checked:=TheSettings.SaveOnClose;

  ScaleOnResizeCheckBox.Caption := adrsScaleOnResize;
  ScaleOnResizeCheckBox.Hint := adrsScaleSubSitesWhenASiteIsResized;
  ScaleOnResizeCheckBox.Checked := TheSettings.ScaleOnResize;

  ShowHeaderCheckBox.Caption := adrsShowHeaders;
  ShowHeaderCheckBox.Hint := adrsEachDockedWindowHasAHeaderThatAllowsDraggingHasACo;
  ShowHeaderCheckBox.Checked := TheSettings.ShowHeader;

  ShowHeaderCaptionCheckBox.Caption := adrsShowHeaderCaptions;
  ShowHeaderCaptionCheckBox.Hint := adrsShowCaptionsOfDockedControlsInTheHeader;
  ShowHeaderCaptionCheckBox.Checked := TheSettings.ShowHeaderCaption;

  HideHeaderCaptionForFloatingCheckBox.Caption := adrsNoCaptionsForFloatingSites;
  HideHeaderCaptionForFloatingCheckBox.Hint := adrsHideHeaderCaptionsForSitesWithOnlyOneDockedControl;
  HideHeaderCaptionForFloatingCheckBox.Checked := TheSettings.HideHeaderCaptionFloatingControl;
end;

end.
