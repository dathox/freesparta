{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Maciej Izak

  DaThoX 2004-2015
  FreeSparta.com
}

unit sparta_EDTU_NonVisualComponents;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, Spin, Buttons, Menus,
  Types, Generics.Collections, Generics.Defaults, Math, sparta_DesignedForm,
  // IDEIntf
  PropEdits, FormEditingIntf, SpartaAPI;

const
  NonVisualCompWidth = ComponentPaletteImageWidth + 2 * 2;

type

  { TComponentHelper }
  TComponentList = TList<TComponent>;

  TComponentHelper = class helper for TComponent
  private
    function GetLeft: SmallInt;
    procedure SetLeft(AValue: SmallInt);
    function GetTop: SmallInt;
    procedure SetTop(AValue: SmallInt);
  public
    property Left: SmallInt read GetLeft write SetLeft;
    property Top: SmallInt read GetTop write SetTop;
  end;

  { TedtuNonVisualEditor }

  TedtuNonVisualEditor = class(TFrame, ISTADesignTimeUtil, ISTAExtendedDesignTimeUtil, ISTANonVisualComponentsUtil)
    bSelectAllNonVisual: TButton;
    cbShowNonVisualComponents: TCheckBox;
    cbAnchor: TComboBox;
    gbArrange: TGroupBox;
    gbSpacing: TGroupBox;
    gbLayout: TGroupBox;
    gbAnchor: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    rbPerColumn: TRadioButton;
    rbPerRow: TRadioButton;
    sbTranspose: TSpeedButton;
    seSpacingHorizontal: TSpinEdit;
    seSpacingVertical: TSpinEdit;
    sePerColumn: TSpinEdit;
    sePerRow: TSpinEdit;
    seEdgeSpacing: TSpinEdit;
    sbSortByClassName: TSpeedButton;
    sbArrange: TSpeedButton;
    procedure bSelectAllNonVisualClick(Sender: TObject);
    procedure cbAnchorEditingDone(Sender: TObject);
    procedure cbShowNonVisualComponentsEditingDone(Sender: TObject);
    procedure sbArrangeClick(Sender: TObject);
    procedure sbSortByClassNameClick(Sender: TObject);
    procedure sbTransposeClick(Sender: TObject);
    procedure seEdgeSpacingChange(Sender: TObject);
  private
    { private declarations }
    FRefreshDisabled: Boolean;
    FRoot: TPersistent;
    FDesignedForm: IDesignedForm;
    FSelectionList: TPersistentSelectionList;
    FSelectedComponents: TList<TComponent>;
    FIsSortedByRow: Boolean;
    FSortedByRow: TObjectDictionary<SmallInt, TComponentList>;
    FSortedByCol: TObjectDictionary<SmallInt, TComponentList>;
    FBestSorted: TObjectDictionary<SmallInt, TComponentList>;
    FLowLeft, FHighRight: SmallInt;
    FLowTop, FHighBottom: SmallInt;

    procedure SortComponents;
    procedure RefreshControls;

    procedure OnDesignRefreshPropertyValues;
    procedure OnDesignSetSelection(const ASelection: TPersistentSelectionList);
    procedure BeginUpdate;
    procedure EndUpdate;

    function PointCompare(constref Left, Right: PPoint): Integer;
    function TopCompare(constref Left, Right: TComponent): Integer;
    function LeftCompare(constref Left, Right: TComponent): Integer;
    function ComponentClassNameCompare(constref Left, Right: TComponent): Integer;

    function GetRoot: TPersistent;
    procedure SetRoot(AValue: TPersistent);
    function GetParent: TWinControl;
    function GetVisible: Boolean;

    function GetShowNonVisualComponents: Boolean;
  public
    { public declarations }
    //1 . TODO ARoot jako TComponent
    //2. Zamiast ARoot dać IDesignedForm <- a wnim do wyciągniecia szerokosc i wysokosc

    constructor Create(TheOwner: TComponent; ARoot: TPersistent); reintroduce;
    destructor Destroy; override;

    procedure RefreshValues;

    property Root: TPersistent read GetRoot write SetRoot;
  end;

  TedtuNonVisual = class(TEDTU)
  public
    class function AvailableForRoot(ARoot: TPersistent): Boolean; override;
    class function CreateEDTUForRoot(TheOwner: TComponent; ARoot: TPersistent): ISTAExtendedDesignTimeUtil; override;
    class function GlyphName: string; override;
  end;

implementation

{$R *.lfm}

{ TedtuNonVisual }

class function TedtuNonVisual.AvailableForRoot(ARoot: TPersistent): Boolean;
begin
  Result := (ARoot is TDataModule) or (ARoot is TWinControl);
end;

class function TedtuNonVisual.CreateEDTUForRoot(TheOwner: TComponent;
  ARoot: TPersistent): ISTAExtendedDesignTimeUtil;
begin
  if not AvailableForRoot(ARoot) then
    Exit(nil);

  Result := TedtuNonVisualEditor.Create(TheOwner, ARoot);
end;

class function TedtuNonVisual.GlyphName: string;
begin
  Result := 'EDTU_NON_VISUAL';
end;

{ TComponentHelper }

function TComponentHelper.GetLeft: SmallInt;
begin
  Result := LeftFromDesignInfo(Self.DesignInfo);
end;

procedure TComponentHelper.SetLeft(AValue: SmallInt);
begin
  Self.DesignInfo := LeftTopToDesignInfo(AValue, Top);
end;

function TComponentHelper.GetTop: SmallInt;
begin
  Result := TopFromDesignInfo(Self.DesignInfo);
end;

procedure TComponentHelper.SetTop(AValue: SmallInt);
begin
  Self.DesignInfo := LeftTopToDesignInfo(Left, AValue);
end;

{ TedtuNonVisualEditor }

procedure TedtuNonVisualEditor.cbShowNonVisualComponentsEditingDone(
  Sender: TObject);
var
  LForm: TCustomForm;
begin
  FormEditingHook.ClearSelection;

  LForm := FormEditingHook.GetDesignerForm(FRoot);
  LForm.Invalidate;
end;

procedure TedtuNonVisualEditor.sbArrangeClick(Sender: TObject);
begin
  SortComponents;
end;

procedure TedtuNonVisualEditor.bSelectAllNonVisualClick(Sender: TObject);
var
  i: Integer;

begin
  if (GlobalDesignHook.LookupRoot <> FRoot) then
    GlobalDesignHook.LookupRoot := FRoot;

  FSelectionList.Clear;
  FormEditingHook.GetDesignerForm(FRoot);
  for i := 0 to TComponent(FRoot).ComponentCount - 1 do
    if not (TComponent(FRoot).Components[i] is TControl) then
      FSelectionList.Add(TComponent(FRoot).Components[i]);

  GlobalDesignHook.SetSelection(FSelectionList);
  RefreshValues;
end;

procedure TedtuNonVisualEditor.cbAnchorEditingDone(Sender: TObject);
begin
  if FRefreshDisabled then
    Exit;
  SortComponents;
end;

procedure TedtuNonVisualEditor.sbSortByClassNameClick(Sender: TObject);
begin
  FSelectedComponents.Sort;
  SortComponents;
end;

procedure TedtuNonVisualEditor.sbTransposeClick(Sender: TObject);
var
  LTemp: string;
begin
  BeginUpdate;
  LTemp := sePerColumn.Text;
  sePerColumn.Text := sePerRow.Text;
  sePerRow.Text := LTemp;
  if rbPerColumn.Checked then
  begin
    rbPerColumn.Checked := False;
    rbPerRow.Checked := True;
  end
  else
  begin
    rbPerColumn.Checked := True;
    rbPerRow.Checked := False;
  end;
  EndUpdate;
  SortComponents;
end;

procedure TedtuNonVisualEditor.seEdgeSpacingChange(Sender: TObject);
begin
  if FRefreshDisabled or ((Sender as TSpinEdit).Text = '#')  or ((Sender as TSpinEdit).Text = '') then
    Exit;

  SortComponents;
end;

procedure TedtuNonVisualEditor.SortComponents;
var
  LComp: TComponent;
  i, x, y: Integer;
  dx: Integer = 0;
  dy: Integer = 0;
  sx: Integer = 1;
  sy: Integer = 1;
  LAnchors: TAnchors;
  LSelectedWidth, LSelectedHeight: Integer;
  LForm: TCustomForm;
begin
  // napraw wartosci '#':
  BeginUpdate;
  seEdgeSpacing.Text := IntToStr(StrToIntDef(seEdgeSpacing.Text, 8));
  sePerColumn.Text := IntToStr(StrToIntDef(sePerColumn.Text, 8));
  sePerRow.Text := IntToStr(StrToIntDef(sePerRow.Text, 8));
  seSpacingHorizontal.Text := IntToStr(StrToIntDef(seSpacingHorizontal.Text, 8));
  seSpacingVertical.Text := IntToStr(StrToIntDef(seSpacingVertical.Text, 8));
  EndUpdate;

  // wyrównanie ...
  LAnchors := TAnchors(Integer(cbAnchor.Items.Objects[cbAnchor.ItemIndex]));
  if akRight in LAnchors then
  begin
    dx := FDesignedForm.Width - NonVisualCompWidth;
    sx := -1;
  end;
  if akBottom in LAnchors then
  begin
    dy := FDesignedForm.Height - NonVisualCompWidth;
    sy := -1;
  end;

  if LAnchors = [] then
  begin
    if rbPerRow.Checked then
    begin
      LSelectedWidth := sePerRow.Value;
      LSelectedWidth := ifthen(LSelectedWidth > FSelectedComponents.Count, FSelectedComponents.Count, LSelectedWidth);

      LSelectedHeight := FSelectedComponents.Count div sePerRow.Value;
      if FSelectedComponents.Count mod sePerRow.Value <> 0 then
        Inc(LSelectedHeight);
    end
    else
    begin
      LSelectedHeight := sePerColumn.Value;
      LSelectedHeight := ifthen(LSelectedHeight > FSelectedComponents.Count, FSelectedComponents.Count, LSelectedHeight);

      LSelectedWidth := FSelectedComponents.Count div sePerColumn.Value;
      if FSelectedComponents.Count mod sePerColumn.Value <> 0 then
        Inc(LSelectedWidth);
    end;

    LSelectedWidth := LSelectedWidth * NonVisualCompWidth + (Pred(LSelectedWidth) * seSpacingHorizontal.Value);
    LSelectedHeight := LSelectedHeight * NonVisualCompWidth + (Pred(LSelectedHeight) * seSpacingVertical.Value);

    dx := (FDesignedForm.Width - LSelectedWidth) div 2;
    dy := (FDesignedForm.Height - LSelectedHeight) div 2;
    seEdgeSpacing.Value := 0;
  end;

  for i := 0 to FSelectedComponents.Count - 1 do
  begin
    LComp := FSelectedComponents[i];

    if rbPerRow.Checked then
    begin
      x := (i mod sePerRow.Value) * sx;
      y := (i div sePerRow.Value) * sy;
    end
    else
    begin
      x := (i div sePerColumn.Value) * sx;
      y := (i mod sePerColumn.Value) * sy;
    end;
    LComp.Left := dx + x * NonVisualCompWidth + x * seSpacingHorizontal.Value + seEdgeSpacing.Value * sx;
    LComp.Top := dy + y * NonVisualCompWidth + y * seSpacingVertical.Value + seEdgeSpacing.Value * sy;
  end;

  LForm := FormEditingHook.GetDesignerForm(FRoot);
  LForm.Invalidate;
  LForm.Designer.Modified;
end;

procedure TedtuNonVisualEditor.RefreshControls;
var
  LSpacing, i: Integer;
  LHCompList, LVCompList: TComponentList;
  LWidth, LHeight: Integer;
  LLeftTop, LRightTop, LLeftBottom, LRightBottom: TPoint;
  LPointList: TList<PPoint>;
  LComp: TComponent;

  procedure CreateAndTryAddToListIfSameXY(APoint: PPoint; X, Y: Integer);
  begin
    APoint^ := Point(X, Y);

    if X = Y then
      LPointList.Add(APoint);
  end;

  procedure TrySelectItemIndex;
  begin
    if LPointList.Count = 0 then
      Exit;

    if LPointList.First = @LLeftTop then
      cbAnchor.ItemIndex := 0
    else if LPointList.First = @LRightTop then
      cbAnchor.ItemIndex := 1
    else if LPointList.First = @LLeftBottom then
      cbAnchor.ItemIndex := 2
    else if LPointList.First = @LRightBottom then
      cbAnchor.ItemIndex := 3;
  end;

begin
  BeginUpdate;

  // odnajdz najblizszy róg ... troche lipny algorytm ale co tam ;)
  LPointList := TList<PPoint>.Create(TComparer<PPoint>.Construct(PointCompare));
  LWidth := FDesignedForm.Width;
  LHeight := FDesignedForm.Height;

  // sprawdź najpierw center...
  cbAnchor.ItemIndex := -1;

  if InRange(LWidth - (FHighRight + NonVisualCompWidth), Pred(FLowLeft), Succ(FLowLeft))
    and InRange(LHeight - (FHighBottom + NonVisualCompWidth), Pred(FLowTop), Succ(FLowTop)) then
    cbAnchor.ItemIndex := 4;

  // najpierw stworz liste z taka sama odlegloscia od krawedzi
  if cbAnchor.ItemIndex = -1 then
  begin
    LComp := FSortedByRow[FLowTop].First;
    CreateAndTryAddToListIfSameXY(@LLeftTop, LComp.Left, LComp.Top);
    LComp := FSortedByRow[FLowTop].Last;
    CreateAndTryAddToListIfSameXY(@LRightTop, LWidth - (LComp.Left + NonVisualCompWidth), LComp.Top);
    LComp := FSortedByCol[FLowLeft].Last;
    CreateAndTryAddToListIfSameXY(@LLeftBottom, LComp.Left, LHeight - (LComp.Top + NonVisualCompWidth));
    LComp := FSortedByRow[FHighBottom].Last;
    CreateAndTryAddToListIfSameXY(@LRightBottom, LWidth - (LComp.Left + NonVisualCompWidth), LHeight - (LComp.Top + NonVisualCompWidth));

    LPointList.Sort;
    TrySelectItemIndex;

    // jesli nie znaleziono wtedy szukaj wsród wszystkich...
    if cbAnchor.ItemIndex = -1 then
    begin
      LPointList.Clear;
      LPointList.Add(@LLeftTop);
      LPointList.Add(@LRightTop);
      LPointList.Add(@LLeftBottom);
      LPointList.Add(@LRightBottom);
      TrySelectItemIndex;
    end;
  end;
  // i może uda sie wywnioskowac odleglosc...
  if cbAnchor.ItemIndex = 4 then // dla center odleglosc od krawedzi zawsze 0
    seEdgeSpacing.Text := '0'
  else if LPointList.First.X <> LPointList.First.Y then
    seEdgeSpacing.Text := '#'
  else
    seEdgeSpacing.Text := IntToStr(LPointList.First.X);

  LPointList.Free;

  rbPerColumn.Checked := FBestSorted = FSortedByCol;
  rbPerRow.Checked := FBestSorted = FSortedByRow;

  // ilosc komponentow w kolumnie / wierszu
  case cbAnchor.ItemIndex of
    0, 4: // left top & center
      begin
        LVCompList := FSortedByCol[FLowLeft];
        sePerColumn.Text := IntToStr(LVCompList.Count);
        LHCompList := FSortedByRow[FLowTop];
        sePerRow.Text := IntToStr(LHCompList.Count);
      end;
    1: // right top
      begin
        LVCompList := FSortedByCol[FHighRight];
        sePerColumn.Text := IntToStr(LVCompList.Count);
        LHCompList := FSortedByRow[FLowTop];
        sePerRow.Text := IntToStr(LHCompList.Count);
      end;
    2: // left bottom
      begin
        LVCompList := FSortedByCol[FLowLeft];
        sePerColumn.Text := IntToStr(LVCompList.Count);
        LHCompList := FSortedByRow[FHighBottom];
        sePerRow.Text := IntToStr(LHCompList.Count);
      end;
    3: // right bottom
      begin
        LVCompList := FSortedByCol[FHighRight];
        sePerColumn.Text := IntToStr(LVCompList.Count);
        LHCompList := FSortedByRow[FHighBottom];
        sePerRow.Text := IntToStr(LHCompList.Count);
      end;
  end;

  // odstepy w pionie
  if LVCompList.Count = 1 then
    LSpacing := 0
  else
  begin
    LSpacing := LVCompList[1].Top - (LVCompList[0].Top + NonVisualCompWidth);
    for i := 2 to LVCompList.Count - 1 do
      if LSpacing <> LVCompList[i].Top - (LVCompList[i - 1].Top + NonVisualCompWidth) then
      begin
        LSpacing := -1;
        Break;
      end;
  end;
  if LSpacing = -1 then
    seSpacingVertical.Text := '#'
  else
    seSpacingVertical.Text := IntToStr(LSpacing);

  // odstepy w poziomie
  if LHCompList.Count = 1 then
    LSpacing := 0
  else
  begin
    LSpacing := LHCompList[1].Left - (LHCompList[0].Left + NonVisualCompWidth);
    for i := 2 to LHCompList.Count - 1 do
      if LSpacing <> LHCompList[i].Left - (LHCompList[i - 1].Left + NonVisualCompWidth) then
      begin
        LSpacing := -1;
        Break;
      end;
  end;
  if LSpacing = -1 then
    seSpacingHorizontal.Text := '#'
  else
    seSpacingHorizontal.Text := IntToStr(LSpacing);

  EndUpdate;
end;

procedure TedtuNonVisualEditor.OnDesignRefreshPropertyValues;
begin
  RefreshValues;
end;

procedure TedtuNonVisualEditor.OnDesignSetSelection(
  const ASelection: TPersistentSelectionList);
begin
  if FSelectionList.IsEqual(ASelection) then
    Exit;
  RefreshValues;
end;

procedure TedtuNonVisualEditor.BeginUpdate;
begin
  FRefreshDisabled := True;
end;

procedure TedtuNonVisualEditor.EndUpdate;
begin
  FRefreshDisabled := False;
end;

function TedtuNonVisualEditor.PointCompare(constref Left, Right: PPoint
  ): Integer;
begin
  Result := TCompare.Single(sqrt(sqr(Left.x) + sqr(Left.y)), sqrt(sqr(Right.x) + sqr(Right.y)));
end;

function TedtuNonVisualEditor.TopCompare(constref Left, Right: TComponent
  ): Integer;
begin
  Result := TCompare.Int32(Left.Top, Right.Top);
end;

function TedtuNonVisualEditor.LeftCompare(constref Left, Right: TComponent
  ): Integer;
begin
  Result := TCompare.Int32(Left.Left, Right.Left);
end;

function TedtuNonVisualEditor.ComponentClassNameCompare(constref Left,
  Right: TComponent): Integer;
begin
  Result := TCompare.AnsiString(Left.ClassName, Right.ClassName);
end;

function TedtuNonVisualEditor.GetRoot: TPersistent;
begin
  Result := FRoot;
end;

procedure TedtuNonVisualEditor.SetRoot(AValue: TPersistent);
begin
  if FRoot = AValue then
    Exit;

  FRoot :=  AValue;
  if FRoot = nil then
    FDesignedForm := nil
  else
  begin
    FDesignedForm := FormEditingHook.GetDesignerForm(FRoot) as IDesignedForm;
    cbShowNonVisualComponents.Visible := not (FRoot is TDataModule);
  end;
  RefreshValues;
end;

function TedtuNonVisualEditor.GetParent: TWinControl;
begin
  Result := Parent;
end;

function TedtuNonVisualEditor.GetVisible: Boolean;
begin
  Result := Visible;
end;

function TedtuNonVisualEditor.GetShowNonVisualComponents: Boolean;
begin
  Result := cbShowNonVisualComponents.Checked;
end;

constructor TedtuNonVisualEditor.Create(TheOwner: TComponent; ARoot: TPersistent);
begin
  inherited Create(TheOwner);

  FSelectionList := TPersistentSelectionList.Create;
  FSelectedComponents := TList<TComponent>.Create(TComparer<TComponent>.Construct(ComponentClassNameCompare));
  FSortedByRow := TObjectDictionary<SmallInt, TComponentList>.Create([doOwnsValues]);
  FSortedByCol := TObjectDictionary<SmallInt, TComponentList>.Create([doOwnsValues]);

  GlobalDesignHook.AddHandlerRefreshPropertyValues(OnDesignRefreshPropertyValues);
  GlobalDesignHook.AddHandlerSetSelection(OnDesignSetSelection);

  cbAnchor.AddItem('Left Top', TObject(Byte([akLeft, akTop])));
  cbAnchor.AddItem('Right Top', TObject(Byte([akRight, akTop])));
  cbAnchor.AddItem('Left Bottom', TObject(Byte([akLeft, akBottom])));
  cbAnchor.AddItem('Right Bottom', TObject(Byte([akRight, akBottom])));
  cbAnchor.AddItem('Center', TObject(Byte([])));
  Root := ARoot;
end;

destructor TedtuNonVisualEditor.Destroy;
begin
  GlobalDesignHook.RemoveHandlerRefreshPropertyValues(OnDesignRefreshPropertyValues);
  GlobalDesignHook.RemoveHandlerSetSelection(OnDesignSetSelection);

  FSelectionList.Free;
  FSelectedComponents.Free;
  FSortedByRow.Free;
  FSortedByCol.Free;

  inherited Destroy;
end;

procedure TedtuNonVisualEditor.RefreshValues;
var
  LComp: TComponent;
  i: Integer;
  l: TComponentList;
  LLeftComparer: IComparer<TComponent>;
  LTopComparer: IComparer<TComponent>;

  procedure AddToDic(ADic: TObjectDictionary<SmallInt, TComponentList>; AKey: SmallInt; AComparer: IComparer<TComponent>);
  var
    LList: TComponentList;
  begin
    if ADic.TryGetValue(AKey, LList) then
      LList.Add(LComp)
    else
    begin
      LList := TComponentList.Create(AComparer);
      LList.Add(LComp);
      ADic.Add(AKey, LList);
    end
  end;

begin
  Self.Enabled := FRoot <> nil;

  if FRefreshDisabled or (GlobalDesignHook.LookupRoot <> FRoot) or (FRoot = nil) then
    Exit;

  FSelectedComponents.Clear;
  GlobalDesignHook.GetSelection(FSelectionList);
  for i := 0 to FSelectionList.Count - 1 do
  begin
    if not (FSelectionList[i] is TControl) and not (FSelectionList[i] is TDataModule) then
    begin
      LComp := TComponent(FSelectionList[i]);
      FSelectedComponents.Add(LComp);
    end;
  end;

  gbArrange.Enabled := FSelectedComponents.Count <> 0;
  if FSelectedComponents.Count = 0 then
    gbArrange.Caption := ' Arrange none '
  else
  begin
    case FSelectedComponents.Count of
      1: gbArrange.Caption := ' Arrange for 1 component ';
    else
      gbArrange.Caption := Format(' Arrange for %d components ', [FSelectedComponents.Count]);
    end;
    FSortedByRow.Clear;
    FSortedByCol.Clear;

    FLowLeft := High(SmallInt);
    FLowTop := High(SmallInt);
    FHighRight := Low(SmallInt);
    FHighBottom := Low(SmallInt);

    LLeftComparer := TComparer<TComponent>.Construct(LeftCompare);
    LTopComparer := TComparer<TComponent>.Construct(TopCompare);

    for LComp in FSelectedComponents do
    begin
      AddToDic(FSortedByRow, LComp.Top, LLeftComparer);
      AddToDic(FSortedByCol, LComp.Left, LTopComparer);

      if LComp.Left < FLowLeft then
        FLowLeft := LComp.Left;
      if LComp.Top < FLowTop then
        FLowTop := LComp.Top;
      if LComp.Left > FHighRight then
        FHighRight := LComp.Left;
      if LComp.Top > FHighBottom then
        FHighBottom := LComp.Top;
    end;


    for l in FSortedByRow.Values do
    begin
      l.Sort;
    end;

    for l in FSortedByCol.Values do
      l.Sort;

    if FSortedByRow[FLowTop].Count >= FSortedByCol[FLowLeft].Count then
      FBestSorted := FSortedByRow
    else
      FBestSorted := FSortedByCol;

    RefreshControls;
  end;
end;

end.

