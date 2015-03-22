{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Maciej Izak

  DaThoX 2004-2015
  FreeSparta.com
}

unit sparta_edtu_anchordocking_table;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, Generics.Collections, Math, LMessages, LCLType,
  ButtonPanel;

type

  { TMyPanel }

  TMyPanel = class(ExtCtrls.TPanel)
  public
    procedure MouseEnter(var Msg: TLMessage); message CM_MouseEnter;
    procedure MouseLeave(var Msg: TLMessage); message CM_MouseLeave;
  end;

  { TfedtuAnchorDockingTable }

  TfedtuAnchorDockingTable = class(TForm)
    ButtonPanel1: TButtonPanel;
    lCaption: TLabel;
    pMainEditor: TPanel;
    pbMain: TPaintBox;
    pMain: TPanel;
    pMeta: TPanel;
    pUtils: TPanel;
    sbDown: TSpeedButton;
    sbLeft: TSpeedButton;
    sbMain: TScrollBox;
    sbRight: TSpeedButton;
    sbUp: TSpeedButton;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure sbLeftClick(Sender: TObject);
  private
    { private declarations }
    FRoot: TWinControl;
    FControls: TDictionary<TWinControl, TControl>;
    FControlsIndexList: TList<Integer>;
    procedure GetColRow(AIndex: Integer; out ACol, ARow: Integer);
    procedure RecalcEditorSize;
    function CreateNewCell(ACtrl: TControl): TPanel;
  public
    { public declarations }
    procedure ShowModal(ARoot: TWinControl); overload;
  end;

var
  fedtuAnchorDockingTable: TfedtuAnchorDockingTable;

implementation

{$R *.lfm}

{ TMyPanel }

procedure TMyPanel.MouseEnter(var Msg: TLMessage);

  procedure PrepareButton(AButton: TSpeedButton);
  begin
    AButton.Parent := Self;

    // i tak determinuja to anchors...
    AButton.AnchorSideLeft.Control := Self;
    AButton.AnchorSideTop.Control := Self;
    AButton.AnchorSideRight.Control := Self;
    AButton.AnchorSideBottom.Control := Self;

    AButton.Visible := True;
  end;

begin
  with TfedtuAnchorDockingTable(Owner) do
  begin
    PrepareButton(sbLeft);
    PrepareButton(sbUp);
    PrepareButton(sbRight);
    PrepareButton(sbDown);
  end;
end;

procedure TMyPanel.MouseLeave(var Msg: TLMessage);
begin
  with TfedtuAnchorDockingTable(Owner) do
  begin
    sbLeft.Visible := False;
    sbUp.Visible := False;
    sbRight.Visible := False;
    sbDown.Visible := False;
  end;
end;

{ TfedtuAnchorDockingTable }

procedure TfedtuAnchorDockingTable.FormCreate(Sender: TObject);
begin
  FControls := TDictionary<TWinControl, TControl>.Create;
  FControlsIndexList := TList<Integer>.Create;

  sbUp.LoadGlyphFromResourceName(HINSTANCE, 'arrow_up');
  sbDown.LoadGlyphFromResourceName(HINSTANCE, 'arrow_down');
  sbLeft.LoadGlyphFromResourceName(HINSTANCE, 'arrow_left');
  sbRight.LoadGlyphFromResourceName(HINSTANCE, 'arrow_right');
end;

procedure TfedtuAnchorDockingTable.FormDestroy(Sender: TObject);
begin
  FControls.Free;
  FControlsIndexList.Free;
end;

procedure TfedtuAnchorDockingTable.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
var
  k: TWinControl;
begin
  for k in FControls.Keys do
    k.Free;
  FControls.Clear;
  FControlsIndexList.Clear;
end;

procedure TfedtuAnchorDockingTable.OKButtonClick(Sender: TObject);
var
  i: Integer;
  LCtrl: TWinControl;
begin
  for i := 0 to pMainEditor.ControlCount - 1 do
  begin
    LCtrl := pMainEditor.Controls[i] as TWinControl;
    // zmien indeksy tylko tych kontrolek ktore poprzestawialismy
    if FControlsIndexList[i] = FRoot.GetControlIndex(FControls[LCtrl]) then
      Continue;
    FRoot.SetControlIndex(FControls[LCtrl], FControlsIndexList[i]);
  end;
  FRoot.ChildSizing.Layout := cclNone;
  FRoot.ChildSizing.Layout := pMainEditor.ChildSizing.Layout;
end;

procedure TfedtuAnchorDockingTable.PaintBox1Paint(Sender: TObject);
var
  LCtrl: TControl;
  LPaintBox: TPaintBox absolute Sender;
begin
  LCtrl := FControls[LPaintBox.Parent];
  if (LCtrl is TWinControl) then
    TWinControl(LCtrl).PaintTo(LPaintBox.Canvas, 0, 0);
end;

procedure TfedtuAnchorDockingTable.sbLeftClick(Sender: TObject);
var
  LIdx, LNewIdx, LCol, LRow: Integer;
  LCtrl: TControl;
  LDelta: Integer;
  LLayout: TControlChildrenLayout;
begin
  with pMainEditor do
  begin
    LIdx := GetControlIndex(TControl(Sender).Parent);

    LDelta := ifthen((Sender = sbLeft) or (Sender = sbUp), -1, 1);
    if (Sender = sbLeft) or (Sender = sbRight) then
      LLayout := cclLeftToRightThenTopToBottom
    else
      LLayout := cclTopToBottomThenLeftToRight;

    // wszystko zalezy od layout i strony - na szczecie mozna to zrobic generycznie
    if ChildSizing.Layout = LLayout then
    begin
      LNewIdx := LIdx + (1*LDelta);
      if ((LIdx mod ChildSizing.ControlsPerLine <> 0) and (LIdx <> 0) and (LDelta = -1))
      or ((LIdx mod ChildSizing.ControlsPerLine <> Pred(ChildSizing.ControlsPerLine)) and (LIdx <> FControls.Count - 1) and (LDelta = 1)) then
        SetControlIndex(TControl(Sender).Parent, LNewIdx);
    end
    else
    begin
      LNewIdx := LIdx + (ChildSizing.ControlsPerLine*LDelta);
      if ((LNewIdx >= 0) and (LDelta = -1))
      or ((LNewIdx < FControls.Count) and (LDelta = 1)) then
      begin
        LCtrl := Controls[LNewIdx];
        SetControlIndex(TControl(Sender).Parent, LNewIdx);
        SetControlIndex(LCtrl, LIdx);
      end;
    end;

    LLayout := ChildSizing.Layout;
    ChildSizing.Layout := cclNone;
    ChildSizing.Layout := LLayout;
    RecalcEditorSize;
  end;
end;

procedure TfedtuAnchorDockingTable.GetColRow(AIndex: Integer; out ACol, ARow: Integer);
begin
  case pMainEditor.ChildSizing.Layout of
    cclLeftToRightThenTopToBottom:
      begin
        ARow := AIndex div pMainEditor.ChildSizing.ControlsPerLine;
        ACol := AIndex mod pMainEditor.ChildSizing.ControlsPerLine;
      end;
    cclTopToBottomThenLeftToRight:
      begin
        ACol := AIndex div pMainEditor.ChildSizing.ControlsPerLine;
        ARow := AIndex mod pMainEditor.ChildSizing.ControlsPerLine;
      end;
  end;
end;

procedure TfedtuAnchorDockingTable.RecalcEditorSize;
var
  LCol, LRow, LIdx, i: Integer;
  LWidth: Integer = 0;
  LHeight: Integer = 0;
  LHorizontalSize: TList<Integer>;
  LVerticalSize: TList<Integer>;
  LControl: TControl;
begin
  LHorizontalSize := TList<Integer>.Create;
  LVerticalSize := TList<Integer>.Create;

  for LIdx := 0 to pMainEditor.ControlCount - 1 do
  begin
    LControl := pMainEditor.Controls[LIdx];
    GetColRow(LIdx, LCol, LRow);

    if LCol >= LHorizontalSize.Count then
      LHorizontalSize.Count := LCol + 1;
    if LRow >= LVerticalSize.Count then
      LVerticalSize.Count := LRow + 1;

    if LControl.Constraints.MinWidth > LHorizontalSize[LCol] then
      LHorizontalSize[LCol] := LControl.Constraints.MinWidth;
    if LControl.Constraints.MinHeight > LVerticalSize[LRow] then
      LVerticalSize[LRow] := LControl.Constraints.MinHeight;
  end;

  for i in LHorizontalSize do
    Inc(LWidth, i);
  for i in LVerticalSize do
    Inc(LHeight, i);

  pMainEditor.Width := LWidth;
  pMainEditor.Height := LHeight;

  LVerticalSize.Free;
  LHorizontalSize.Free;
end;

function TfedtuAnchorDockingTable.CreateNewCell(ACtrl: TControl): TPanel;
const
  MIN_WIDTH = 69;
  MIN_HEIGHT = 44;
  WIDTH_ADD = 75;
  HEIGHT_ADD = 20;
  START_WIDTH = MIN_WIDTH + WIDTH_ADD;
  START_HEIGHT = MIN_HEIGHT + HEIGHT_ADD;
var
  LlCaption: TLabel;
  LpbMain: TPaintBox;
  LpMeta: TMyPanel;
  LsbUp: TSpeedButton;
  LsbRight: TSpeedButton;
  LsbDown: TSpeedButton;
  LsbLeft: TSpeedButton;
  LpMain: TPanel;
begin
  LlCaption := TLabel.Create(Self);
  LpbMain := TPaintBox.Create(Self);
  LpMeta := TMyPanel.Create(Self);
  LpMain := TPanel.Create(Self);

  with LpMeta do
  begin
    Left := 48;
    Top := 32;
    BevelOuter := bvNone;
    Constraints.MinHeight := START_HEIGHT;
    Constraints.MinWidth := START_WIDTH;
    TabOrder := 0;

    with LpbMain do
    begin
      Parent := LpMeta;
      AnchorSideLeft.Control := LpMeta;
      AnchorSideLeft.Side := asrCenter;
      AnchorSideTop.Control := LpMeta;
      AnchorSideTop.Side := asrCenter;
      AnchorSideBottom.Control := LpMeta;
      AnchorSideBottom.Side := asrCenter;
      Left := 35;
      Height := ACtrl.Height;
      Top := 30;
      Width := ACtrl.Width;
      ClientHeight := ACtrl.Height;
      ClientWidth := ACtrl.Width;
      Anchors := [akLeft, akBottom];
      if (ACtrl is TWinControl) then
      begin
        with LpMeta do
        begin
          if ACtrl.Width > MIN_WIDTH then
          begin
            LpMain.Width := ACtrl.Width;
            Constraints.MinWidth := ifthen(ACtrl.Width < START_WIDTH, START_WIDTH, ACtrl.Width);
          end;
          if ACtrl.Height > MIN_HEIGHT then
          begin
            LpMain.Height := ACtrl.Height;
            Constraints.MinHeight := ifthen(ACtrl.Height + HEIGHT_ADD < START_HEIGHT,  START_HEIGHT, ACtrl.Height + HEIGHT_ADD);
          end;
        end;
        OnPaint := PaintBox1Paint;
      end;
    end;
    with LlCaption do
    begin
      Parent := LpMeta;

      AnchorSideLeft.Control := LpMeta;
      AnchorSideLeft.Side := asrCenter;
      AnchorSideTop.Control := LpMeta;

      Caption := ACtrl.Name + ': ' + ACtrl.ClassName;

      Left := 0;
      Height := 42;
      Top := 0;
      Width := 106;
      Alignment := taCenter;
      Anchors := [akLeft, akTop];
      ParentColor := False;
    end;
  end;
  LpMeta.Parent := pMainEditor;

  Result := LpMeta;
end;

procedure TfedtuAnchorDockingTable.ShowModal(ARoot: TWinControl);
var
  i: Integer;
begin
  FRoot := ARoot;
  pMainEditor.ChildSizing.Layout := ARoot.ChildSizing.Layout;
  pMainEditor.ChildSizing.ControlsPerLine := ARoot.ChildSizing.ControlsPerLine;

  for i := 0 to ARoot.ControlCount - 1 do
    if (ARoot.Controls[i].Align = alNone) and (ARoot.Controls[i].Anchors = [akLeft, akTop])
    and (ARoot.Controls[i].AnchorSideLeft.Control = nil) and (ARoot.Controls[i].AnchorSideTop.Control = nil) then
    begin
      FControls.Add(
        CreateNewCell(ARoot.Controls[i]),
        ARoot.Controls[i]);
      FControlsIndexList.Add(i);
    end;

  RecalcEditorSize;

  ShowModal;
end;

finalization
  fedtuAnchorDockingTable.Free;
end.

