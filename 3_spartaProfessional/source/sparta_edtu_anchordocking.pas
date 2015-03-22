{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Maciej Izak

  DaThoX 2004-2015
  FreeSparta.com
}

unit sparta_EDTU_AnchorDocking;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, StdCtrls, Spin,
  Buttons, ComCtrls, PropEdits, PropEditUtils, Generics.Collections, TypInfo,
  sparta_edtu_anchordocking_table, StrUtils, Math, Dialogs, LCLIntf, LCLType, sparta_FakeForm,
  FormEditingIntf, sparta_FakeFrame, sparta_DesignedForm, SpartaAPI;

const
   CONSTRAINTS_MIN_WIDTH = 0;
   CONSTRAINTS_MAX_WIDTH = 1;
   CONSTRAINTS_MIN_HEIGHT = 2;
   CONSTRAINTS_MAX_HEIGHT = 3;
   CONSTRAINTS_LOW = CONSTRAINTS_MIN_WIDTH;
   CONSTRAINTS_HIGH = CONSTRAINTS_MAX_HEIGHT;

   BOUNDS_LEFT = 0;
   BOUNDS_TOP = 1;
   BOUNDS_WIDTH = 2;
   BOUNDS_HEIGHT = 3;
   BOUNDS_LOW = BOUNDS_LEFT;
   BOUNDS_HIGH = BOUNDS_HEIGHT;

   BORDER_SPACE_LEFT = 0;
   BORDER_SPACE_TOP = 1;
   BORDER_SPACE_RIGHT = 2;
   BORDER_SPACE_BOTTOM = 3;
   BORDER_SPACE_AROUND = 4;
   BORDER_SPACE_INNER = 5;
   BORDER_SPACE_LOW = BORDER_SPACE_LEFT;
   BORDER_SPACE_HIGH = BORDER_SPACE_INNER;

   CELL_ALIGN_HORIZONTAL = 0;
   CELL_ALIGN_VERTICAL = 1;
   CELL_ALIGN_FILL = 0;
   CELL_ALIGN_LEFT_TOP = 1;
   CELL_ALIGN_RIGHT_BOTTOM = 2;
   CELL_ALIGN_CENTER = 3;

   LAYOUT_NONE = 0;
   LAYOUT_LeftToRightThenTopToBottom = 1;
   LAYOUT_TopToBottomThenLeftToRight = 2;

   CHILD_SPACE_LEFT_RIGHT = 0;
   CHILD_SPACE_TOP_BOTTOM = 1;
   CHILD_SPACE_HORIZONTAL = 2;
   CHILD_SPACE_VERTICAL = 3;
   CHILD_SPACE_LOW = CHILD_SPACE_LEFT_RIGHT;
   CHILD_SPACE_HIGH = CHILD_SPACE_VERTICAL;

   CHILD_SIZING_ENLARGE_HORIZONTAL = 0;
   CHILD_SIZING_ENLARGE_VERTICAL = 1;
   CHILD_SIZING_SHRINK_HORIZONTAL = 2;
   CHILD_SIZING_SHRINK_VERTICAL = 3;

   CHILD_SIZING_AnchorAligning = 0;
   CHILD_SIZING_ScaleChilds = 1;
   CHILD_SIZING_HomogenousChildResize = 2;
   CHILD_SIZING_HomogenousSpaceResize = 3;

type
  TedtuAnchorEditor = class;
  TAnchorInfo = class;

  { TAnchorSideInfo }
  TAnchorSideReferenceButtonsArray = array[TAnchorSideReference] of TSpeedButton;

  TAnchorSideInfo = class
  private
    FOwner: TAnchorInfo;
    FKind: TAnchorKind;

    FButtons: TAnchorSideReferenceButtonsArray;
    FCheckBox: TCheckBox;
    FComboBox: TComboBox;

    FEnabled: boolean;
    FUnknownEnabled: boolean;
    FSide: TAnchorSideReference;
    FUnknownSide: boolean;
    FControl: TControl;
    FUnknownControl: boolean;

    function GetButtons(AIndex: TAnchorSideReference): TSpeedButton;
    function GetMainButton: TSpeedButton;
    function GetSelectedButton: TSpeedButton;
    procedure SetSide(AValue: TAnchorSideReference);
    procedure SetControl(AValue: TControl);
    procedure SetEnabled(AValue: Boolean);
  public
    property Kind: TAnchorKind read FKind;
    property Owner: TAnchorInfo read FOwner;

    constructor Create(AOwner: TAnchorInfo; AKind: TAnchorKind; AButtons: TAnchorSideReferenceButtonsArray;
      ACheckBox: TCheckBox; AComboBox: TComboBox);

    property MainButton: TSpeedButton read GetMainButton;
    property SelectedButton: TSpeedButton read GetSelectedButton;
    property CheckBox: TCheckBox read FCheckBox;
    property ComboBox: TComboBox read FComboBox;

    property Enabled: boolean read FEnabled write SetEnabled;
    property UnknownEnabled: boolean read FUnknownEnabled write FUnknownEnabled;
    property Side: TAnchorSideReference read FSide write SetSide;
    property UnknownSide: boolean read FUnknownSide write FUnknownSide;
    property Control: TControl read FControl write SetControl;
    property UnknownControl: boolean read FUnknownControl write FUnknownControl;

    property Buttons[Index: TAnchorSideReference]: TSpeedButton read GetButtons;

    procedure RefreshControls;
    procedure AllowAllUp(AValue: Boolean);
  end;

  { TAnchorInfo }

  TSetSideInfo = (ssiEnabled, ssiSide, ssiControl);
  TConstraintsSpinEditArray = array[0..3] of TEdit;
  TBoundsSpinEditArray = array[0..3] of TSpinEdit;
  TBorderSpaceSpinEditArray = array[0..5] of TSpinEdit;
  TCellAlignSpeedButtonArray = array[0..3] of TSpeedButton;
  TLayoutSpeedButtonArray = array[0..2] of TSpeedButton;
  TChildSpaceSpinEditArray = array[0..3] of TSpinEdit;
  TChildSizingSpeedButtonArray = array[0..3] of TSpeedButton;

  TAnchorInfo = class
  private
    FAnchorEditor: TedtuAnchorEditor;
    FSides: array[TAnchorKind] of TAnchorSideInfo;
    function GetSides(AIndex: TAnchorKind): TAnchorSideInfo;

    // ustaw u wszystkich dzieci
    procedure SetSideInfo(ASideInfo: TAnchorSideInfo; ASetSideInfo: TSetSideInfo);
  public
    constructor Create(AAnchorEditor: TedtuAnchorEditor);
    destructor Destroy; override;

    procedure Clear;
    procedure RefreshControls;

    property Sides[Index: TAnchorKind]: TAnchorSideInfo read GetSides; default;
  end;

  { TLayoutInfo }

  TLayoutInfo = class
  private
    FAnchorEditor: TedtuAnchorEditor;
    FAnchorInfo: TAnchorInfo;

    FVisible: Boolean;
    FUnknownVisible: Boolean;

    FConstraints: array[0..3] of Integer;
    FConstraintsSpinEdit: TConstraintsSpinEditArray;
    FAlign: TAlign;
    FBounds: array[0..3] of Integer;
    FBoundsSpinEdit: TBoundsSpinEditArray;
    FBorderSpace: array[0..5] of Integer;
    FBorderSpaceSpinEdit: TBorderSpaceSpinEditArray;
    FCellAlign: array[0..1] of TControlCellAlign;
    FCallAlignSpeedButton: array[0..1] of TCellAlignSpeedButtonArray;

    FLayout: TControlChildrenLayout;
    FLayoutSpeedButton: TLayoutSpeedButtonArray;
    FControlsPerLine: Integer;
    FChildSpace: array[0..3] of Integer;
    FChildSpaceSpinEdit: TChildSpaceSpinEditArray;
    FChildSizing: array[0..3] of TChildControlResizeStyle;
    FChildSizingSpeedButton: array[0..3] of TChildSizingSpeedButtonArray;

    FAutoSize: Boolean;

    FUnknownConstraints: array[0..3] of Boolean;
    FUnknownAlign: Boolean;
    FUnknownBounds: array[0..3] of Boolean;
    FUnknownBorderSpace: array[0..5] of Boolean;
    FUnknownCellAlign: array[0..1] of Boolean;

    FUnknownLayout: Boolean;
    FUnknownControlsPerLine: Boolean;
    FUnknownChildSpace: array[0..3] of Boolean;
    FUnknownChildSizing: array[0..3] of Boolean;

    FUnknownAutoSize: Boolean;

    function GetConstraints(Index: Integer): Integer;
    procedure SetConstraints(Index: Integer; AValue: Integer);
    procedure SetAlign(AValue: TAlign);
    function GetBounds(Index: Integer): Integer;
    procedure SetBounds(Index: Integer; AValue: Integer);
    function GetBorderSpace(Index: Integer): Integer;
    procedure SetBorderSpace(Index: Integer; AValue: Integer);
    function GetCellAlign(Index: Integer): TControlCellAlign;
    procedure SetCellAlign(Index: Integer; AValue: TControlCellAlign);

    procedure SetLayout(AValue: TControlChildrenLayout);
    procedure SetControlsPerLine(AValue: Integer);
    function GetChildSpace(Index: Integer): Integer;
    procedure SetChildSpace(Index: Integer; AValue: Integer);
    function GetChildSizing(Index: Integer): TChildControlResizeStyle;
    procedure SetChildSizing(Index: Integer; AValue: TChildControlResizeStyle);

    procedure SetAutoSize(AValue: Boolean);
    procedure SetVisible(AValue: Boolean);

    procedure AllowAllUp(AAllow: Boolean; AValue: Integer; AButtons: array of TSpeedButton);
  public
    constructor Create(AAnchorEditor: TedtuAnchorEditor);
    destructor Destroy; override;

    procedure Clear;
    procedure RefreshControls;

    property AnchorInfo: TAnchorInfo read FAnchorInfo;

    property Constraints[Index: Integer]: Integer read GetConstraints write SetConstraints;
    property ConstraintsMinWidth: Integer index CONSTRAINTS_MIN_WIDTH read GetConstraints write SetConstraints;
    property ConstraintsMaxWidth: Integer index CONSTRAINTS_MAX_WIDTH read GetConstraints write SetConstraints;
    property ConstraintsMinHeight: Integer index CONSTRAINTS_MIN_HEIGHT read GetConstraints write SetConstraints;
    property ConstraintsMaxHeight: Integer index CONSTRAINTS_MAX_HEIGHT read GetConstraints write SetConstraints;

    property UnknownConstraintsMinWidth: Boolean read FUnknownConstraints[CONSTRAINTS_MIN_WIDTH] write FUnknownConstraints[CONSTRAINTS_MIN_WIDTH];
    property UnknownConstraintsMaxWidth: Boolean read FUnknownConstraints[CONSTRAINTS_MAX_WIDTH] write FUnknownConstraints[CONSTRAINTS_MAX_WIDTH];
    property UnknownConstraintsMinHeight: Boolean read FUnknownConstraints[CONSTRAINTS_MIN_HEIGHT] write FUnknownConstraints[CONSTRAINTS_MIN_HEIGHT];
    property UnknownConstraintsMaxHeight: Boolean read FUnknownConstraints[CONSTRAINTS_MAX_HEIGHT] write FUnknownConstraints[CONSTRAINTS_MAX_HEIGHT];

    property Align: TAlign read FAlign write SetAlign;
    property UnknownAlign: Boolean read FUnknownAlign write FUnknownAlign;

    property Bounds[Index: Integer]: Integer read GetBounds write SetBounds;
    property Left: Integer index BOUNDS_LEFT read GetBounds write SetBounds;
    property Top: Integer index BOUNDS_TOP read GetBounds write SetBounds;
    property Width: Integer index BOUNDS_WIDTH read GetBounds write SetBounds;
    property Height: Integer index BOUNDS_HEIGHT read GetBounds write SetBounds;
    property UnknownLeft: Boolean read FUnknownBounds[BOUNDS_LEFT] write FUnknownBounds[BOUNDS_LEFT];
    property UnknownTop: Boolean read FUnknownBounds[BOUNDS_TOP] write FUnknownBounds[BOUNDS_TOP];
    property UnknownWidth: Boolean read FUnknownBounds[BOUNDS_WIDTH] write FUnknownBounds[BOUNDS_WIDTH];
    property UnknownHeight: Boolean read FUnknownBounds[BOUNDS_HEIGHT] write FUnknownBounds[BOUNDS_HEIGHT];

    property BorderSpace[Index: Integer]: Integer read GetBorderSpace write SetBorderSpace;
    property BorderSpaceLeft: Integer index BORDER_SPACE_LEFT read GetBorderSpace write SetBorderSpace;
    property BorderSpaceTop: Integer index BORDER_SPACE_TOP read GetBorderSpace write SetBorderSpace;
    property BorderSpaceRight: Integer index BORDER_SPACE_RIGHT read GetBorderSpace write SetBorderSpace;
    property BorderSpaceBottom: Integer index BORDER_SPACE_BOTTOM read GetBorderSpace write SetBorderSpace;
    property BorderSpaceAround: Integer index BORDER_SPACE_AROUND read GetBorderSpace write SetBorderSpace;
    property BorderSpaceInner: Integer index BORDER_SPACE_INNER read GetBorderSpace write SetBorderSpace;
    property UnknownBorderSpaceLeft: Boolean read FUnknownBorderSpace[BORDER_SPACE_LEFT] write FUnknownBorderSpace[BORDER_SPACE_LEFT];
    property UnknownBorderSpaceTop: Boolean read FUnknownBorderSpace[BORDER_SPACE_TOP] write FUnknownBorderSpace[BORDER_SPACE_TOP];
    property UnknownBorderSpaceRight: Boolean read FUnknownBorderSpace[BORDER_SPACE_RIGHT] write FUnknownBorderSpace[BORDER_SPACE_RIGHT];
    property UnknownBorderSpaceBottom: Boolean read FUnknownBorderSpace[BORDER_SPACE_BOTTOM] write FUnknownBorderSpace[BORDER_SPACE_BOTTOM];
    property UnknownBorderSpaceAround: Boolean read FUnknownBorderSpace[BORDER_SPACE_AROUND] write FUnknownBorderSpace[BORDER_SPACE_AROUND];
    property UnknownBorderSpaceInner: Boolean read FUnknownBorderSpace[BORDER_SPACE_INNER] write FUnknownBorderSpace[BORDER_SPACE_INNER];

    property CellAlignH: TControlCellAlign index CELL_ALIGN_HORIZONTAL read GetCellAlign write SetCellAlign;
    property UnknownCellAlignH: Boolean read FUnknownCellAlign[CELL_ALIGN_HORIZONTAL] write FUnknownCellAlign[CELL_ALIGN_HORIZONTAL];
    property CellAlignV: TControlCellAlign index CELL_ALIGN_VERTICAL read GetCellAlign write SetCellAlign;
    property UnknownCellAlignV: Boolean read FUnknownCellAlign[CELL_ALIGN_VERTICAL] write FUnknownCellAlign[CELL_ALIGN_VERTICAL];

    property Layout: TControlChildrenLayout read FLayout write SetLayout;
    property UnknownLayout: Boolean read FUnknownLayout write FUnknownLayout;
    property ControlsPerLine: Integer read FControlsPerLine write SetControlsPerLine;
    property UnknownControlsPerLine: Boolean read FUnknownControlsPerLine write FUnknownControlsPerLine;

    property ChildSpace[Index: Integer]: Integer read GetChildSpace write SetChildSpace;
    property ChildSpaceLeftRight: Integer index CHILD_SPACE_LEFT_RIGHT read GetChildSpace write SetChildSpace;
    property ChildSpaceTopBottom: Integer index CHILD_SPACE_TOP_BOTTOM read GetChildSpace write SetChildSpace;
    property ChildSpaceHorizontal: Integer index CHILD_SPACE_HORIZONTAL read GetChildSpace write SetChildSpace;
    property ChildSpaceVertical: Integer index CHILD_SPACE_VERTICAL read GetChildSpace write SetChildSpace;
    property UnknownChildSpaceLeftRight: Boolean read FUnknownChildSpace[CHILD_SPACE_LEFT_RIGHT] write FUnknownChildSpace[CHILD_SPACE_LEFT_RIGHT];
    property UnknownChildSpaceTopBottom: Boolean read FUnknownChildSpace[CHILD_SPACE_TOP_BOTTOM] write FUnknownChildSpace[CHILD_SPACE_TOP_BOTTOM];
    property UnknownChildSpaceHorizontal: Boolean read FUnknownChildSpace[CHILD_SPACE_HORIZONTAL] write FUnknownChildSpace[CHILD_SPACE_HORIZONTAL];
    property UnknownChildSpaceVertical: Boolean read FUnknownChildSpace[CHILD_SPACE_VERTICAL] write FUnknownChildSpace[CHILD_SPACE_VERTICAL];

    property ChildSizing[Index: Integer]: TChildControlResizeStyle read GetChildSizing write SetChildSizing;
    property ChildSizingEnlargeHorizontal : TChildControlResizeStyle index CHILD_SIZING_ENLARGE_HORIZONTAL read GetChildSizing write SetChildSizing;
    property ChildSizingEnlargeVertical   : TChildControlResizeStyle index CHILD_SIZING_ENLARGE_VERTICAL   read GetChildSizing write SetChildSizing;
    property ChildSizingShrinkHorizontal  : TChildControlResizeStyle index CHILD_SIZING_SHRINK_HORIZONTAL  read GetChildSizing write SetChildSizing;
    property ChildSizingShrinkVertical    : TChildControlResizeStyle index CHILD_SIZING_SHRINK_VERTICAL    read GetChildSizing write SetChildSizing;
    property UnknownChildSizingEnlargeHorizontal : Boolean read FUnknownChildSizing[CHILD_SIZING_ENLARGE_HORIZONTAL] write FUnknownChildSizing[CHILD_SIZING_ENLARGE_HORIZONTAL];
    property UnknownChildSizingEnlargeVertical   : Boolean read FUnknownChildSizing[CHILD_SIZING_ENLARGE_VERTICAL  ] write FUnknownChildSizing[CHILD_SIZING_ENLARGE_VERTICAL  ];
    property UnknownChildSizingShrinkHorizontal  : Boolean read FUnknownChildSizing[CHILD_SIZING_SHRINK_HORIZONTAL ] write FUnknownChildSizing[CHILD_SIZING_SHRINK_HORIZONTAL ];
    property UnknownChildSizingShrinkVertical    : Boolean read FUnknownChildSizing[CHILD_SIZING_SHRINK_VERTICAL   ] write FUnknownChildSizing[CHILD_SIZING_SHRINK_VERTICAL   ];

    property AutoSize: Boolean read FAutoSize write SetAutoSize;
    property UnknownAutoSize: Boolean read FUnknownAutoSize write FUnknownAutoSize;

    property Visible: Boolean read FVisible write SetVisible;
    property UnknownVisible: Boolean read FUnknownVisible write FUnknownVisible;
  end;

  { TedtuAnchorEditor }

  TedtuAnchorEditor = class(TFrame, ISTADesignTimeUtil, ISTAExtendedDesignTimeUtil)
    cbAnchorSideRightCtrl: TComboBox;
    cbAnchorSideBottomCtrl: TComboBox;
    cbBottomEnabled: TCheckBox;
    cbTopEnabled: TCheckBox;
    cbLeftEnabled: TCheckBox;
    cbRightEnabled: TCheckBox;
    cbAlign: TComboBox;
    cbAnchorSideTopCtrl: TComboBox;
    cbAnchorSideLeftCtrl: TComboBox;
    cbAutoSize: TCheckBox;
    cbVisible: TCheckBox;
    Label28: TLabel;
    Label29: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    lCtrlName: TLabel;
    pEnlargeShrink: TPanel;
    eConstraintsHeight: TSpinEdit;
    eConstraintsMaxHeight: TEdit;
    eConstraintsMinHeight: TEdit;
    eConstraintsMaxWidth: TEdit;
    eConstraintsMinWidth: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    lLayoutFor: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label2: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    pAutoSize: TPanel;
    pBounds: TPanel;
    pChildSizing: TPanel;
    pAlign: TPanel;
    pAnchors: TPanel;
    pConstraints: TPanel;
    eConstraintsWidth: TSpinEdit;
    sbBottomSideBottom: TSpeedButton;
    sbBottomSideCenter: TSpeedButton;
    sbBottomSideTop: TSpeedButton;
    sbEHAnchorAligning: TSpeedButton;
    sbEHHomogenousChildResize: TSpeedButton;
    sbEHHomogenousSpaceResize: TSpeedButton;
    sbEHScaleChilds: TSpeedButton;
    sbEVAnchorAligning: TSpeedButton;
    sbEVHomogenousChildResize: TSpeedButton;
    sbEVHomogenousSpaceResize: TSpeedButton;
    sbEVScaleChilds: TSpeedButton;
    sbSHAnchorAligning: TSpeedButton;
    sbSHHomogenousChildResize: TSpeedButton;
    sbSHHomogenousSpaceResize: TSpeedButton;
    sbSHScaleChilds: TSpeedButton;
    sbSVAnchorAligning: TSpeedButton;
    sbSVHomogenousChildResize: TSpeedButton;
    sbSVHomogenousSpaceResize: TSpeedButton;
    sbSVScaleChilds: TSpeedButton;
    sbVCellCenter: TSpeedButton;
    sbVCellFill: TSpeedButton;
    sbVCellLeftTop: TSpeedButton;
    sbVCellRightBottom: TSpeedButton;
    sbLeftSideCenter: TSpeedButton;
    sbLeftSideLeft: TSpeedButton;
    sbLeftSideRight: TSpeedButton;
    sbRightSideCenter: TSpeedButton;
    sbRightSideLeft: TSpeedButton;
    sbRightSideRight: TSpeedButton;
    sbTopSideBottom: TSpeedButton;
    sbTopSideCenter: TSpeedButton;
    sbTopSideTop: TSpeedButton;
    sbShowMinMax: TSpeedButton;
    sbDefaultMinMaxValues: TSpeedButton;
    eLeft: TSpinEdit;
    eTop: TSpinEdit;
    eHeight: TSpinEdit;
    eWidth: TSpinEdit;
    sbHCellCenter: TSpeedButton;
    sbHCellFill: TSpeedButton;
    sbHCellRightBottom: TSpeedButton;
    sbHCellLeftTop: TSpeedButton;
    sbLayoutLeftToRightThenTopToBottom: TSpeedButton;
    sbLayoutNone: TSpeedButton;
    sbLayoutTopToBottomThenLeftToRight: TSpeedButton;
    bEditTable: TSpeedButton;
    seBSTop: TSpinEdit;
    seCSTopBottom: TSpinEdit;
    seCSLeftRight: TSpinEdit;
    seBSAround: TSpinEdit;
    seBSRight: TSpinEdit;
    seBSLeft: TSpinEdit;
    seBSBottom: TSpinEdit;
    seBSInner: TSpinEdit;
    seControlsPerLine: TSpinEdit;
    seCSHorizontal: TSpinEdit;
    seCSVertical: TSpinEdit;
    sbShowEnlargeShrink: TSpeedButton;
    procedure cbAlignEditingDone(Sender: TObject);
    procedure cbAnchorSideLeftCtrlEditingDone(Sender: TObject);
    procedure cbAutoSizeEditingDone(Sender: TObject);
    procedure cbLeftEnabledEditingDone(Sender: TObject);
    procedure cbVisibleEditingDone(Sender: TObject);
    procedure eConstraintsMinWidthChange(Sender: TObject);
    procedure eConstraintsWidthChange(Sender: TObject);
    procedure eLeftChange(Sender: TObject);
    procedure sbDefaultMinMaxValuesClick(Sender: TObject);
    procedure sbEHAnchorAligningClick(Sender: TObject);
    procedure sbHCellFillClick(Sender: TObject);
    procedure sbLayoutNoneClick(Sender: TObject);
    procedure sbLeftSideCenterClick(Sender: TObject);
    procedure sbShowEnlargeShrinkClick(Sender: TObject);
    procedure sbShowMinMaxClick(Sender: TObject);
    procedure seBSLeftChange(Sender: TObject);
    procedure bEditTableClick(Sender: TObject);
    procedure seControlsPerLineChange(Sender: TObject);
    procedure seCSLeftRightChange(Sender: TObject);
  private
    { private declarations }
    FRefreshDisabled: Boolean;

    FSelectionList: TPersistentSelectionList;
    FSelectedControls: TList<TControl>;
    FWinControls: Boolean;

    FRoot: TPersistent;

    FLayoutInfo: TLayoutInfo;
    function GetSide(AButton: TSpeedButton): TAnchorSideInfo; overload;
    function GetSide(AComboBox: TComboBox): TAnchorSideInfo; overload;
    function GetSide(ACheckBox: TCheckBox): TAnchorSideInfo; overload;
    function GetSide(AAnchorKind: TAnchorKind): TAnchorSideInfo; overload;
    function GetConstraintIdx(ASpinEdit: TEdit): Integer;
    procedure FillSiblingsComboBoxes;

    procedure OnDesignRefreshPropertyValues;
    procedure OnDesignSetSelection(const ASelection: TPersistentSelectionList);

    procedure BeginUpdate;
    procedure EndUpdate;
    function GetRoot: TPersistent;
    procedure SetRoot(AValue: TPersistent);
    function GetParent: TWinControl;
    function GetVisible: Boolean;
  protected
    procedure ComputeScrollbars; override;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent; ARoot: TPersistent); reintroduce;
    destructor Destroy; override;

    procedure RefreshValues;
    property Root: TPersistent read GetRoot write SetRoot;
  end;

  TedtuAnchor = class(TEDTU)
  public
    class function AvailableForRoot(ARoot: TPersistent): Boolean; override;
    class function CreateEDTUForRoot(TheOwner: TComponent; ARoot: TPersistent): ISTAExtendedDesignTimeUtil; override;
    class function GlyphName: string; override;
  end;

resourcestring
  sisRightAnchoringCheckBox = 'Right anchoring';
  sisTopAnchoringCheckBox = 'Top anchoring';
  sisLeftAnchoringCheckBox = 'Left anchoring';
  sisBottomAnchoringCheckBox = 'Bottom anchoring';

implementation

{$R *.lfm}
{$R sizing.res}

type
  TControlScrollBarAccess = class(TControlScrollBar);

{ TedtuAnchor }

class function TedtuAnchor.AvailableForRoot(ARoot: TPersistent): Boolean;
begin
  Result := ARoot is TWinControl;
end;

class function TedtuAnchor.CreateEDTUForRoot(TheOwner: TComponent;
  ARoot: TPersistent): ISTAExtendedDesignTimeUtil;
begin
  if not AvailableForRoot(ARoot) then
    Exit(nil);

  Result := TedtuAnchorEditor.Create(TheOwner, ARoot);
end;

class function TedtuAnchor.GlyphName: string;
begin
  Result := 'MENU_VIEW_ANCHOR_EDITOR';
end;

{ TLayoutInfo }

procedure TLayoutInfo.SetConstraints(Index: Integer; AValue: Integer);
var
  LCtrl: TControl;
begin
  if (FConstraints[Index] = AValue) and not FUnknownConstraints[Index] then
    Exit;

  for LCtrl in FAnchorEditor.FSelectedControls do
    case Index of
      CONSTRAINTS_MIN_WIDTH:
        begin
          LCtrl.Constraints.MinWidth := AValue;
          AValue := LCtrl.Constraints.MinWidth;
        end;
      CONSTRAINTS_MAX_WIDTH:
        begin
          LCtrl.Constraints.MaxWidth := AValue;
          AValue := LCtrl.Constraints.MaxWidth;
        end;
      CONSTRAINTS_MIN_HEIGHT:
        begin
          LCtrl.Constraints.MinHeight := AValue;
          AValue := LCtrl.Constraints.MinHeight;
        end;
      CONSTRAINTS_MAX_HEIGHT:
        begin
          LCtrl.Constraints.MaxHeight := AValue;
          AValue := LCtrl.Constraints.MaxHeight;
        end;
    end;

  FConstraints[Index] := AValue;
  FUnknownConstraints[Index] := False;

  GlobalDesignHook.Modified(FAnchorEditor);
  GlobalDesignHook.RefreshPropertyValues;
end;

function TLayoutInfo.GetConstraints(Index: Integer): Integer;
begin
  Result := FConstraints[Index];
end;

procedure TLayoutInfo.SetAlign(AValue: TAlign);
var
  LCtrl: TControl;
begin
  if (FAlign = AValue) and not FUnknownAlign then
    Exit;

  FAlign := AValue;
  FUnknownAlign := False;

  for LCtrl in FAnchorEditor.FSelectedControls do
    LCtrl.Align := AValue;

  GlobalDesignHook.Modified(FAnchorEditor);
  GlobalDesignHook.RefreshPropertyValues;
end;

function TLayoutInfo.GetBounds(Index: Integer): Integer;
begin
  Result := FBounds[Index];
end;

procedure TLayoutInfo.SetBounds(Index: Integer; AValue: Integer);
var
  LCtrl: TControl;
  LForm: TCustomForm;
begin
  if (FBounds[Index] = AValue) and not FUnknownBounds[Index] then
    Exit;

  for LCtrl in FAnchorEditor.FSelectedControls do
    if (LCtrl is TCustomForm) and (LCtrl = FAnchorEditor.FRoot) then
      case Index of
        BOUNDS_LEFT  : TFakeForm(LCtrl).Left := AValue;
        BOUNDS_TOP   : TFakeForm(LCtrl).Top := AValue;
        BOUNDS_WIDTH : TFakeForm(LCtrl).Width := AValue;
        BOUNDS_HEIGHT: TFakeForm(LCtrl).Height := AValue;
      end
    else
      case Index of
        BOUNDS_LEFT  : LCtrl.Left := AValue;
        BOUNDS_TOP   : LCtrl.Top := AValue;
        BOUNDS_WIDTH : LCtrl.Width := AValue;
        BOUNDS_HEIGHT: LCtrl.Height := AValue;
      end;

  FBounds[Index] := AValue;
  FUnknownBounds[Index] := False;

  GlobalDesignHook.Modified(FAnchorEditor);
  GlobalDesignHook.RefreshPropertyValues;
end;

function TLayoutInfo.GetBorderSpace(Index: Integer): Integer;
begin
  Result := FBorderSpace[Index];
end;

procedure TLayoutInfo.SetBorderSpace(Index: Integer; AValue: Integer);
var
  LCtrl: TControl;
begin
  if (FBorderSpace[Index] = AValue) and not FUnknownBorderSpace[Index] then
    Exit;

  for LCtrl in FAnchorEditor.FSelectedControls do
    case Index of
      BORDER_SPACE_LEFT  : LCtrl.BorderSpacing.Left := AValue;
      BORDER_SPACE_TOP   : LCtrl.BorderSpacing.Top := AValue;
      BORDER_SPACE_RIGHT : LCtrl.BorderSpacing.Right := AValue;
      BORDER_SPACE_BOTTOM: LCtrl.BorderSpacing.Bottom := AValue;
      BORDER_SPACE_AROUND: LCtrl.BorderSpacing.Around := AValue;
      BORDER_SPACE_INNER : LCtrl.BorderSpacing.InnerBorder := AValue;
    end;

  FBorderSpace[Index] := AValue;
  FUnknownBorderSpace[Index] := False;

  GlobalDesignHook.Modified(FAnchorEditor);
  GlobalDesignHook.RefreshPropertyValues;
end;

function TLayoutInfo.GetCellAlign(Index: Integer): TControlCellAlign;
begin
  Result := FCellAlign[Index];
end;

procedure TLayoutInfo.SetCellAlign(Index: Integer; AValue: TControlCellAlign);
var
  LCtrl: TControl;
begin
  if (FCellAlign[Index] = AValue) and not FUnknownCellAlign[Index] then
    Exit;

  for LCtrl in FAnchorEditor.FSelectedControls do
    case Index of
      CELL_ALIGN_HORIZONTAL: LCtrl.BorderSpacing.CellAlignHorizontal := AValue;
      CELL_ALIGN_VERTICAL  : LCtrl.BorderSpacing.CellAlignVertical := AValue;
    end;

  FCellAlign[Index] := AValue;
  FUnknownCellAlign[Index] := False;

  GlobalDesignHook.Modified(FAnchorEditor);
  GlobalDesignHook.RefreshPropertyValues;
end;

procedure TLayoutInfo.SetLayout(AValue: TControlChildrenLayout);
var
  LCtrl: TControl;
  LWinCtrl: TWinControl absolute LCtrl;
begin
  if (not FAnchorEditor.FWinControls) or ((FLayout = AValue) and not FUnknownLayout) then
    Exit;

  for LCtrl in FAnchorEditor.FSelectedControls do
    LWinCtrl.ChildSizing.Layout := AValue;

  FLayout := AValue;
  FUnknownLayout := False;

  GlobalDesignHook.Modified(FAnchorEditor);
  GlobalDesignHook.RefreshPropertyValues;
end;

procedure TLayoutInfo.SetControlsPerLine(AValue: Integer);
var
  LCtrl: TControl;
  LWinCtrl: TWinControl absolute LCtrl;
begin
  if (not FAnchorEditor.FWinControls) or ((FControlsPerLine = AValue) and not FUnknownControlsPerLine) then
    Exit;

  for LCtrl in FAnchorEditor.FSelectedControls do
    LWinCtrl.ChildSizing.ControlsPerLine := AValue;

  FControlsPerLine := AValue;
  FUnknownControlsPerLine := False;

  GlobalDesignHook.Modified(FAnchorEditor);
  GlobalDesignHook.RefreshPropertyValues;
end;

function TLayoutInfo.GetChildSpace(Index: Integer): Integer;
begin
  Result := FChildSpace[Index];
end;

procedure TLayoutInfo.SetChildSpace(Index: Integer; AValue: Integer);
var
  LCtrl: TControl;
  LWinCtrl: TWinControl absolute LCtrl;
begin
  if (not FAnchorEditor.FWinControls) or ((FChildSpace[Index] = AValue) and not FUnknownChildSpace[Index]) then
    Exit;

  for LCtrl in FAnchorEditor.FSelectedControls do
    case Index of
      CHILD_SPACE_HORIZONTAL: LWinCtrl.ChildSizing.HorizontalSpacing := AValue;
      CHILD_SPACE_VERTICAL  : LWinCtrl.ChildSizing.VerticalSpacing := AValue;
      CHILD_SPACE_LEFT_RIGHT: LWinCtrl.ChildSizing.LeftRightSpacing := AValue;
      CHILD_SPACE_TOP_BOTTOM: LWinCtrl.ChildSizing.TopBottomSpacing := AValue;
    end;

  FChildSpace[Index] := AValue;
  FUnknownChildSpace[Index] := False;

  GlobalDesignHook.Modified(FAnchorEditor);
  GlobalDesignHook.RefreshPropertyValues;
end;

function TLayoutInfo.GetChildSizing(Index: Integer): TChildControlResizeStyle;
begin
  Result := FChildSizing[Index];
end;

procedure TLayoutInfo.SetChildSizing(Index: Integer;
  AValue: TChildControlResizeStyle);
var
  LCtrl: TControl;
  LWinCtrl: TWinControl absolute LCtrl;
begin
  if (not FAnchorEditor.FWinControls) or ((FChildSizing[Index] = AValue) and not FUnknownChildSizing[Index]) then
    Exit;

  for LCtrl in FAnchorEditor.FSelectedControls do
    case Index of
      CHILD_SIZING_ENLARGE_HORIZONTAL: LWinCtrl.ChildSizing.EnlargeHorizontal := AValue;
      CHILD_SIZING_ENLARGE_VERTICAL  : LWinCtrl.ChildSizing.EnlargeVertical := AValue;
      CHILD_SIZING_SHRINK_HORIZONTAL: LWinCtrl.ChildSizing.ShrinkHorizontal := AValue;
      CHILD_SIZING_SHRINK_VERTICAL: LWinCtrl.ChildSizing.ShrinkVertical := AValue;
    end;

  FChildSizing[Index] := AValue;
  FUnknownChildSizing[Index] := False;

  GlobalDesignHook.Modified(FAnchorEditor);
  GlobalDesignHook.RefreshPropertyValues;
end;

procedure TLayoutInfo.SetAutoSize(AValue: Boolean);
var
  LCtrl: TControl;
begin
  if (FAutoSize = AValue) and not FUnknownAutoSize then
    Exit;

  for LCtrl in FAnchorEditor.FSelectedControls do
    LCtrl.AutoSize := AValue;

  FAutoSize := AValue;
  FUnknownAutoSize := False;

  GlobalDesignHook.Modified(FAnchorEditor);
  GlobalDesignHook.RefreshPropertyValues;
end;

procedure TLayoutInfo.AllowAllUp(AAllow: Boolean; AValue: Integer;
  AButtons: array of TSpeedButton);
var
  i: Integer;
  b: TSpeedButton;
begin
  for i := Low(AButtons) to High(AButtons) do
  begin
    b := AButtons[i];
    b.AllowAllUp := AAllow;
    b.Down := (i = AValue) and (not AAllow);
  end;
end;

procedure TLayoutInfo.SetVisible(AValue: Boolean);
var
  LCtrl: TControl;
begin
  if (FVisible = AValue) and not FUnknownVisible then
    Exit;

  for LCtrl in FAnchorEditor.FSelectedControls do
    LCtrl.Visible := AValue;

  FVisible := AValue;
  FUnknownVisible := False;

  GlobalDesignHook.Modified(FAnchorEditor);
  GlobalDesignHook.RefreshPropertyValues;
end;

constructor TLayoutInfo.Create(AAnchorEditor: TedtuAnchorEditor);
begin
  FAnchorInfo := TAnchorInfo.Create(AAnchorEditor);
  FAnchorEditor := AAnchorEditor;

  FConstraintsSpinEdit[CONSTRAINTS_MIN_WIDTH] := AAnchorEditor.eConstraintsMinWidth;
  FConstraintsSpinEdit[CONSTRAINTS_MAX_WIDTH] := AAnchorEditor.eConstraintsMaxWidth;
  FConstraintsSpinEdit[CONSTRAINTS_MIN_HEIGHT] := AAnchorEditor.eConstraintsMinHeight;
  FConstraintsSpinEdit[CONSTRAINTS_MAX_HEIGHT] := AAnchorEditor.eConstraintsMaxHeight;

  FBoundsSpinEdit[BOUNDS_LEFT] := AAnchorEditor.eLeft;
  FBoundsSpinEdit[BOUNDS_TOP] := AAnchorEditor.eTop;
  FBoundsSpinEdit[BOUNDS_WIDTH] := AAnchorEditor.eWidth;
  FBoundsSpinEdit[BOUNDS_HEIGHT] := AAnchorEditor.eHeight;

  FBorderSpaceSpinEdit[BORDER_SPACE_LEFT  ] := AAnchorEditor.seBSLeft;
  FBorderSpaceSpinEdit[BORDER_SPACE_TOP   ] := AAnchorEditor.seBSTop;
  FBorderSpaceSpinEdit[BORDER_SPACE_RIGHT ] := AAnchorEditor.seBSRight;
  FBorderSpaceSpinEdit[BORDER_SPACE_BOTTOM] := AAnchorEditor.seBSBottom;
  FBorderSpaceSpinEdit[BORDER_SPACE_AROUND] := AAnchorEditor.seBSAround;
  FBorderSpaceSpinEdit[BORDER_SPACE_INNER ] := AAnchorEditor.seBSInner;

  FCallAlignSpeedButton[CELL_ALIGN_HORIZONTAL][CELL_ALIGN_CENTER      ] := AAnchorEditor.sbHCellCenter;
  FCallAlignSpeedButton[CELL_ALIGN_HORIZONTAL][CELL_ALIGN_FILL        ] := AAnchorEditor.sbHCellFill;
  FCallAlignSpeedButton[CELL_ALIGN_HORIZONTAL][CELL_ALIGN_LEFT_TOP    ] := AAnchorEditor.sbHCellLeftTop;
  FCallAlignSpeedButton[CELL_ALIGN_HORIZONTAL][CELL_ALIGN_RIGHT_BOTTOM] := AAnchorEditor.sbHCellRightBottom;

  FCallAlignSpeedButton[CELL_ALIGN_VERTICAL][CELL_ALIGN_CENTER      ] := AAnchorEditor.sbVCellCenter;
  FCallAlignSpeedButton[CELL_ALIGN_VERTICAL][CELL_ALIGN_FILL        ] := AAnchorEditor.sbVCellFill;
  FCallAlignSpeedButton[CELL_ALIGN_VERTICAL][CELL_ALIGN_LEFT_TOP    ] := AAnchorEditor.sbVCellLeftTop;
  FCallAlignSpeedButton[CELL_ALIGN_VERTICAL][CELL_ALIGN_RIGHT_BOTTOM] := AAnchorEditor.sbVCellRightBottom;

  FLayoutSpeedButton[LAYOUT_NONE] := AAnchorEditor.sbLayoutNone;
  FLayoutSpeedButton[LAYOUT_LeftToRightThenTopToBottom] := AAnchorEditor.sbLayoutLeftToRightThenTopToBottom;
  FLayoutSpeedButton[LAYOUT_TopToBottomThenLeftToRight] := AAnchorEditor.sbLayoutTopToBottomThenLeftToRight;

  FChildSpaceSpinEdit[CHILD_SPACE_HORIZONTAL] := AAnchorEditor.seCSHorizontal;
  FChildSpaceSpinEdit[CHILD_SPACE_VERTICAL  ] := AAnchorEditor.seCSVertical;
  FChildSpaceSpinEdit[CHILD_SPACE_LEFT_RIGHT] := AAnchorEditor.seCSLeftRight;
  FChildSpaceSpinEdit[CHILD_SPACE_TOP_BOTTOM] := AAnchorEditor.seCSTopBottom;

  FChildSizingSpeedButton[CHILD_SIZING_ENLARGE_HORIZONTAL][CHILD_SIZING_AnchorAligning       ] := AAnchorEditor.sbEHAnchorAligning       ;
  FChildSizingSpeedButton[CHILD_SIZING_ENLARGE_HORIZONTAL][CHILD_SIZING_ScaleChilds          ] := AAnchorEditor.sbEHScaleChilds          ;
  FChildSizingSpeedButton[CHILD_SIZING_ENLARGE_HORIZONTAL][CHILD_SIZING_HomogenousChildResize] := AAnchorEditor.sbEHHomogenousChildResize;
  FChildSizingSpeedButton[CHILD_SIZING_ENLARGE_HORIZONTAL][CHILD_SIZING_HomogenousSpaceResize] := AAnchorEditor.sbEHHomogenousSpaceResize;

  FChildSizingSpeedButton[CHILD_SIZING_ENLARGE_VERTICAL  ][CHILD_SIZING_AnchorAligning       ] := AAnchorEditor.sbEVAnchorAligning       ;
  FChildSizingSpeedButton[CHILD_SIZING_ENLARGE_VERTICAL  ][CHILD_SIZING_ScaleChilds          ] := AAnchorEditor.sbEVScaleChilds          ;
  FChildSizingSpeedButton[CHILD_SIZING_ENLARGE_VERTICAL  ][CHILD_SIZING_HomogenousChildResize] := AAnchorEditor.sbEVHomogenousChildResize;
  FChildSizingSpeedButton[CHILD_SIZING_ENLARGE_VERTICAL  ][CHILD_SIZING_HomogenousSpaceResize] := AAnchorEditor.sbEVHomogenousSpaceResize;

  FChildSizingSpeedButton[CHILD_SIZING_SHRINK_HORIZONTAL ][CHILD_SIZING_AnchorAligning       ] := AAnchorEditor.sbSHAnchorAligning       ;
  FChildSizingSpeedButton[CHILD_SIZING_SHRINK_HORIZONTAL ][CHILD_SIZING_ScaleChilds          ] := AAnchorEditor.sbSHScaleChilds          ;
  FChildSizingSpeedButton[CHILD_SIZING_SHRINK_HORIZONTAL ][CHILD_SIZING_HomogenousChildResize] := AAnchorEditor.sbSHHomogenousChildResize;
  FChildSizingSpeedButton[CHILD_SIZING_SHRINK_HORIZONTAL ][CHILD_SIZING_HomogenousSpaceResize] := AAnchorEditor.sbSHHomogenousSpaceResize;

  FChildSizingSpeedButton[CHILD_SIZING_SHRINK_VERTICAL   ][CHILD_SIZING_AnchorAligning       ] := AAnchorEditor.sbSVAnchorAligning       ;
  FChildSizingSpeedButton[CHILD_SIZING_SHRINK_VERTICAL   ][CHILD_SIZING_ScaleChilds          ] := AAnchorEditor.sbSVScaleChilds          ;
  FChildSizingSpeedButton[CHILD_SIZING_SHRINK_VERTICAL   ][CHILD_SIZING_HomogenousChildResize] := AAnchorEditor.sbSVHomogenousChildResize;
  FChildSizingSpeedButton[CHILD_SIZING_SHRINK_VERTICAL   ][CHILD_SIZING_HomogenousSpaceResize] := AAnchorEditor.sbSVHomogenousSpaceResize;
end;

destructor TLayoutInfo.Destroy;
begin
  FAnchorInfo.Free;
  inherited Destroy;
end;

procedure TLayoutInfo.Clear;
begin
  FAnchorInfo.Clear;

  FVisible := False;
  FUnknownVisible := False;

  FillChar(FConstraints[0], SizeOf(FConstraints), #0);
  FillChar(FUnknownConstraints[0], SizeOf(FUnknownConstraints), False);
  FillChar(FBounds[0], SizeOf(FBounds), #0);
  FillChar(FUnknownBounds[0], SizeOf(FUnknownBounds), False);
  FillChar(FBorderSpace[0], SizeOf(FBorderSpace), #0);
  FillChar(FUnknownBorderSpace[0], SizeOf(FUnknownBorderSpace), False);
  FillChar(FCellAlign[0], SizeOf(FCellAlign), #0);
  FillChar(FUnknownCellAlign[0], SizeOf(FUnknownCellAlign), False);

  FAlign := Default(TAlign);
  FUnknownAlign := False;

  FLayout := Default(TControlChildrenLayout);
  FUnknownLayout := False;
  FControlsPerLine := 0;
  FUnknownControlsPerLine := False;
  FillChar(FChildSpace[0], SizeOf(FChildSpace), #0);
  FillChar(FUnknownChildSpace[0], SizeOf(FUnknownChildSpace), False);
  FillChar(FChildSizing[0], SizeOf(FChildSizing), #0);
  FillChar(FUnknownChildSizing[0], SizeOf(FUnknownChildSizing), False);

  FAutoSize := False;
  FUnknownAutoSize := False;
end;

procedure TLayoutInfo.RefreshControls;
var
  i: Integer;

  procedure SummaryConstraints(ASpinEdit: TSpinEdit; AMin, AMAX: Integer);
  begin
    if not FUnknownConstraints[AMin] and not FUnknownConstraints[AMax]
    and (FConstraints[AMin] = FConstraints[AMax]) then
      ASpinEdit.Text := IntToStr(FConstraints[AMax])
    else
      ASpinEdit.Text := '#';
  end;

begin
  FAnchorInfo.RefreshControls;

  FAnchorEditor.BeginUpdate;

  for i := CONSTRAINTS_LOW to CONSTRAINTS_HIGH do
    FConstraintsSpinEdit[i].Text := IfThen(FUnknownConstraints[i], '#', IntToStr(FConstraints[i]));

  SummaryConstraints(FAnchorEditor.eConstraintsWidth, CONSTRAINTS_MIN_WIDTH, CONSTRAINTS_MAX_WIDTH);
  SummaryConstraints(FAnchorEditor.eConstraintsHeight, CONSTRAINTS_MIN_HEIGHT, CONSTRAINTS_MAX_HEIGHT);

  if FUnknownAlign then
  begin
    FAnchorEditor.cbAlign.Text := '(ambiguous)';
    FAnchorEditor.cbAlign.ItemIndex := -1;
  end
  else
    for i := 0 to FAnchorEditor.cbAlign.Items.Count - 1 do
      if Align = TAlign(FAnchorEditor.cbAlign.Items.Objects[i]) then
      begin
        FAnchorEditor.cbAlign.ItemIndex := i;
        Break;
      end;

  for i := BOUNDS_LOW to BOUNDS_HIGH do
    FBoundsSpinEdit[i].Text := IfThen(FUnknownBounds[i], '#', IntToStr(FBounds[i]));

  for i := BORDER_SPACE_LOW to BORDER_SPACE_HIGH do
    FBorderSpaceSpinEdit[i].Text := IfThen(FUnknownBorderSpace[i], '#', IntToStr(FBorderSpace[i]));

  AllowAllUp(FUnknownCellAlign[CELL_ALIGN_HORIZONTAL], Integer(FCellAlign[CELL_ALIGN_HORIZONTAL]), FCallAlignSpeedButton[CELL_ALIGN_HORIZONTAL]);
  AllowAllUp(FUnknownCellAlign[CELL_ALIGN_VERTICAL], Integer(FCellAlign[CELL_ALIGN_VERTICAL]), FCallAlignSpeedButton[CELL_ALIGN_VERTICAL]);

  FAnchorEditor.sbDefaultMinMaxValues.Enabled := not UnknownWidth and not UnknownHeight;

  // ChildSizing jest widoczne tylkodla TWincontrol
  FAnchorEditor.pChildSizing.Visible := FAnchorEditor.FWinControls;
  if FAnchorEditor.FWinControls then
  begin
    AllowAllUp(FUnknownLayout, Integer(FLayout), FLayoutSpeedButton);

    FAnchorEditor.pChildSizing.Height := IfThen(FLayout = cclNone, 40, 192);

    FAnchorEditor.seControlsPerLine.Text := IfThen(FUnknownControlsPerLine, '#', IntToStr(FControlsPerLine));
    FAnchorEditor.bEditTable.Enabled := (FAnchorEditor.FSelectedControls.Count = 1) and (FLayout <> cclNone) and (FAnchorEditor.seControlsPerLine.Value > 0);
    for i := CHILD_SPACE_LOW to CHILD_SPACE_HIGH do
      FChildSpaceSpinEdit[i].Text := IfThen(FUnknownChildSpace[i], '#', IntToStr(FChildSpace[i]));

    AllowAllUp(FUnknownChildSizing[CHILD_SIZING_ENLARGE_HORIZONTAL], Integer(FChildSizing[CHILD_SIZING_ENLARGE_HORIZONTAL]), FChildSizingSpeedButton[CHILD_SIZING_ENLARGE_HORIZONTAL]);
    AllowAllUp(FUnknownChildSizing[CHILD_SIZING_ENLARGE_VERTICAL], Integer(FChildSizing[CHILD_SIZING_ENLARGE_VERTICAL]), FChildSizingSpeedButton[CHILD_SIZING_ENLARGE_VERTICAL]);
    AllowAllUp(FUnknownChildSizing[CHILD_SIZING_SHRINK_HORIZONTAL], Integer(FChildSizing[CHILD_SIZING_SHRINK_HORIZONTAL]), FChildSizingSpeedButton[CHILD_SIZING_SHRINK_HORIZONTAL]);
    AllowAllUp(FUnknownChildSizing[CHILD_SIZING_SHRINK_VERTICAL], Integer(FChildSizing[CHILD_SIZING_SHRINK_VERTICAL]), FChildSizingSpeedButton[CHILD_SIZING_SHRINK_VERTICAL]);
  end;

  // Enabled
  FAnchorEditor.cbAutoSize.AllowGrayed := FUnknownAutoSize;
  if FUnknownAutoSize then
    FAnchorEditor.cbAutoSize.State := cbGrayed
  else
    FAnchorEditor.cbAutoSize.Checked := FAutoSize;

  // Enabled
  FAnchorEditor.cbVisible.AllowGrayed := FUnknownAutoSize;
  if FUnknownVisible then
    FAnchorEditor.cbVisible.State := cbGrayed
  else
    FAnchorEditor.cbVisible.Checked := FVisible;

  FAnchorEditor.EndUpdate;
end;

{ TAnchorSideInfo }

function TAnchorSideInfo.GetButtons(AIndex: TAnchorSideReference): TSpeedButton;
begin
  Result := FButtons[AIndex];
end;

function TAnchorSideInfo.GetMainButton: TSpeedButton;
begin
  Result := Buttons[asrCenter];
end;

function TAnchorSideInfo.GetSelectedButton: TSpeedButton;
begin
  Result := Buttons[Side];
end;

procedure TAnchorSideInfo.SetSide(AValue: TAnchorSideReference);
begin
  if (FSide = AValue) and not FUnknownSide then
    Exit;
  FSide := AValue;
  FOwner.SetSideInfo(Self, ssiSide);
end;

procedure TAnchorSideInfo.SetControl(AValue: TControl);
begin
  if (FControl = AValue) and not FUnknownControl then
    Exit;

  FControl := AValue;
  FOwner.SetSideInfo(Self, ssiControl);
  RefreshControls;
end;

procedure TAnchorSideInfo.SetEnabled(AValue: Boolean);
begin
  if (FEnabled = AValue) and not FUnknownEnabled then
    Exit;
  FEnabled := AValue;
  FOwner.SetSideInfo(Self, ssiEnabled);
end;

constructor TAnchorSideInfo.Create(AOwner: TAnchorInfo; AKind: TAnchorKind;
  AButtons: TAnchorSideReferenceButtonsArray; ACheckBox: TCheckBox;
  AComboBox: TComboBox);
begin
  FOwner := AOwner;
  FKind := AKind;
  FButtons := AButtons;
  FCheckBox := ACheckBox;
  FComboBox := AComboBox;
end;

procedure TAnchorSideInfo.RefreshControls;
var
  i, idx: Integer;
  j: TAnchorSideReference;
begin
  // Enabled
  CheckBox.AllowGrayed := FUnknownEnabled;
  if FUnknownEnabled then
    CheckBox.State := cbGrayed
  else
    CheckBox.Checked := FEnabled;

  // Control (Sibling)
  if FUnknownControl then
  begin
    ComboBox.Text := '(ambiguous)';
    ComboBox.ItemIndex := -1
  end
  else
  begin
    idx := -1;
    for i := 0 to ComboBox.Items.Count - 1 do
      if ComboBox.Items.Objects[i] = FControl then
      begin
        idx := i;
        ComboBox.ItemIndex := idx;
        Break;
      end;

    // nie znaleziono - to znaczy ze wybrana zostala zla kontrolka
    if idx = -1 then
    begin
      ComboBox.Text := Format('(wrong) %s:%s', [FControl.Name, FControl.ClassName]);
      ComboBox.ItemIndex := -1
    end;
  end;

  // Side
  if FUnknownSide then
    for j := Low(TAnchorSideReference) to High(TAnchorSideReference) do
      FButtons[j].Down := False
  else
    SelectedButton.Down := True;
end;

procedure TAnchorSideInfo.AllowAllUp(AValue: Boolean);
var
  i: TAnchorSideReference;
begin
  for i := Low(TAnchorSideReference) to High(TAnchorSideReference) do
    FButtons[i].AllowAllUp := AValue;
end;

{ TAnchorInfo }

function TAnchorInfo.GetSides(AIndex: TAnchorKind): TAnchorSideInfo;
begin
  Result := FSides[AIndex];
end;

procedure TAnchorInfo.SetSideInfo(ASideInfo: TAnchorSideInfo;
  ASetSideInfo: TSetSideInfo);
var
  LCtrl: TControl;
  LSide: TAnchorSide;
  LTempControl: TControl;
  LTempSide: TAnchorSideReference;
  LTempPosition: Integer;
begin
  // two same loops - first check that all is ok if no - then cancel
  for LCtrl in FAnchorEditor.FSelectedControls do
  begin
    LSide := LCtrl.AnchorSide[ASideInfo.Kind];

    if ASideInfo.Enabled and (not LSide.CheckSidePosition(
      ASideInfo.Control, ASideInfo.Side, LTempControl, LTempSide, LTempPosition)) then
      if MessageDlg('Warning','The current settings will create a circular dependency.', mtWarning, [mbIgnore, mbCancel], 0) <>
          mrIgnore then
      begin
        FAnchorEditor.RefreshValues;
        exit;
      end;
  end;

  for LCtrl in FAnchorEditor.FSelectedControls do
  begin
    LSide := LCtrl.AnchorSide[ASideInfo.Kind];

    case ASetSideInfo of
      ssiSide: LSide.Side := ASideInfo.Side;
      ssiControl: LSide.Control := ASideInfo.Control;
      ssiEnabled:
        if ASideInfo.Enabled then
          LCtrl.Anchors := LCtrl.Anchors + [ASideInfo.Kind]
        else
          LCtrl.Anchors := LCtrl.Anchors - [ASideInfo.Kind]
    end;
  end;

  case ASetSideInfo of
    ssiSide: ASideInfo.UnknownSide := False;
    ssiControl: ASideInfo.UnknownControl := False;
    ssiEnabled:
      begin
        ASideInfo.CheckBox.AllowGrayed := False;
        ASideInfo.UnknownEnabled := False;
      end;
  end;

  GlobalDesignHook.Modified(FAnchorEditor);
  GlobalDesignHook.RefreshPropertyValues;
end;

procedure TAnchorInfo.Clear;
var
  i: TAnchorKind;
  LSide: TAnchorSideInfo;
begin
  for i := Low(TAnchorKind) to High(TAnchorKind) do
  begin
    LSide := FSides[i];
    LSide.FEnabled := False;
    LSide.FSide := asrTop;
    LSide.FControl := nil;
    LSide.FUnknownSide := False;
    LSide.FUnknownControl := False;
    LSide.FUnknownEnabled := False;
  end;
end;

procedure TAnchorInfo.RefreshControls;
var
  i: TAnchorKind;
begin
  for i := Low(TAnchorKind) to High(TAnchorKind) do
    FSides[i].RefreshControls;
end;

constructor TAnchorInfo.Create(AAnchorEditor: TedtuAnchorEditor);

  function CreateArray(ALeftTop, ARightBottom, ACenter: TSpeedButton): TAnchorSideReferenceButtonsArray;
  begin
    Result[asrLeft] := ALeftTop;
    Result[asrRight] := ARightBottom;
    Result[asrCenter] := ACenter;
  end;

begin
  FAnchorEditor := AAnchorEditor;

  FSides[akLeft] := TAnchorSideInfo.Create(Self, akLeft,
    CreateArray(
      AAnchorEditor.sbLeftSideLeft, AAnchorEditor.sbLeftSideRight, AAnchorEditor.sbLeftSideCenter),
    AAnchorEditor.cbLeftEnabled, AAnchorEditor.cbAnchorSideLeftCtrl);
  FSides[akTop] := TAnchorSideInfo.Create(Self, akTop,
    CreateArray(
      AAnchorEditor.sbTopSideTop, AAnchorEditor.sbTopSideBottom, AAnchorEditor.sbTopSideCenter),
    AAnchorEditor.cbTopEnabled, AAnchorEditor.cbAnchorSideTopCtrl);
  FSides[akRight] := TAnchorSideInfo.Create(Self, akRight,
    CreateArray(
      AAnchorEditor.sbRightSideLeft, AAnchorEditor.sbRightSideRight, AAnchorEditor.sbRightSideCenter),
    AAnchorEditor.cbRightEnabled, AAnchorEditor.cbAnchorSideRightCtrl);
  FSides[akBottom] := TAnchorSideInfo.Create(Self, akBottom,
    CreateArray(
      AAnchorEditor.sbBottomSideTop, AAnchorEditor.sbBottomSideBottom, AAnchorEditor.sbBottomSideCenter),
    AAnchorEditor.cbBottomEnabled, AAnchorEditor.cbAnchorSideBottomCtrl);
end;

destructor TAnchorInfo.Destroy;
begin
  FSides[akLeft].Free;
  FSides[akTop].Free;
  FSides[akRight].Free;
  FSides[akBottom].Free;

  inherited Destroy;
end;

{ TedtuAnchorEditor }

procedure TedtuAnchorEditor.cbAnchorSideLeftCtrlEditingDone(Sender: TObject);
var
  LComboBox: TComboBox;
  LSide: TAnchorSideInfo;
  LCtrl: TControl;
  LSelectedCtrl: TControl = nil;
  LName: string;
  i: integer;
begin
  LComboBox := Sender as TComboBox;
  LSide := GetSide(LComboBox);
  LName := Trim(LComboBox.Text);

  if LName = '(ambiguous)' then
    Exit;

  // jesli '', 'nil', '(nil)' to nil LSelectedCtrl := nil :)
  if (LName <> '') and not SameText(LName, 'nil') and not SameText(LName, '(nil)') then
  begin
    // sprawdź nazwy kontrolek i nazwy wpisane w liscie...
    for i := 0 to LComboBox.Items.Count - 1 do
    begin
      LCtrl := TControl(LComboBox.Items.Objects[i]);
      if LCtrl <> nil then
        if SameText(LName, LCtrl.Name) or SameText(LName, LComboBox.Items[i]) then
        begin
          LSelectedCtrl := LCtrl;
          Break;
        end;
    end;
    // sprawdz jeszcze parenta
    if LSelectedCtrl = nil then
      if SameText(LName, '(parent)') or SameText(LName, 'parent') then
        if LComboBox.Items.Count >= 2 then
          if Pos('(parent)', LComboBox.Items[1]) = 1 then
            LSelectedCtrl := LComboBox.Items.Objects[1] as TControl;
  end;

  LSide.Control := LSelectedCtrl;
end;

procedure TedtuAnchorEditor.cbAlignEditingDone(Sender: TObject);
var
  LAlign: string;
  i: Integer;
begin
  LAlign := cbAlign.Text;
  if LAlign = '' then
  begin
    FLayoutInfo.Align := alNone;
    Exit;
  end
  else
  begin
    for i := 0 to cbAlign.Items.Count - 1 do
      // nazwy bez al (np. zamiast alNone wpisano None)
      if SameText(LAlign, cbAlign.Items[i]) or SameText(LAlign, Copy(cbAlign.Items[i], 3)) then
      begin
        FLayoutInfo.Align := TAlign(cbAlign.Items.Objects[i]);
        Exit;
      end;
  end;
end;

procedure TedtuAnchorEditor.cbAutoSizeEditingDone(Sender: TObject);
begin
  FLayoutInfo.AutoSize := cbAutoSize.Checked;
end;

procedure TedtuAnchorEditor.cbLeftEnabledEditingDone(Sender: TObject);
var
  LSide: TAnchorSideInfo;
  ACheckBox: TCheckBox;
begin
  ACheckBox := Sender as TCheckBox;
  LSide := GetSide(ACheckBox);
  LSide.Enabled := ACheckBox.Checked;
end;

procedure TedtuAnchorEditor.cbVisibleEditingDone(Sender: TObject);
begin
  FLayoutInfo.Visible := cbVisible.Checked;
end;

procedure TedtuAnchorEditor.eConstraintsMinWidthChange(Sender: TObject);
begin
  if FRefreshDisabled then
    Exit;

  if Sender = eConstraintsMinWidth then
    FLayoutInfo.ConstraintsMinWidth := StrToIntDef(eConstraintsMinWidth.Text, 0)
  else if Sender = eConstraintsMaxWidth then
    FLayoutInfo.ConstraintsMaxWidth := StrToIntDef(eConstraintsMaxWidth.Text, 0);

  if Sender = eConstraintsMinHeight then
    FLayoutInfo.ConstraintsMinHeight := StrToIntDef(eConstraintsMinHeight.Text, 0)
  else if Sender = eConstraintsMaxHeight then
    FLayoutInfo.ConstraintsMaxHeight := StrToIntDef(eConstraintsMaxHeight.Text, 0);
end;

procedure TedtuAnchorEditor.eConstraintsWidthChange(Sender: TObject);
var
  LValue: Integer;
begin
  if FRefreshDisabled then
    Exit;

  BeginUpdate;
  if (Sender = eConstraintsWidth) and (eConstraintsWidth.Text <> '#') then
  begin
    LValue := StrToIntDef(eConstraintsWidth.Text, 0);
    FLayoutInfo.ConstraintsMinWidth := LValue;
    FLayoutInfo.ConstraintsMaxWidth := LValue;
  end
  else if (Sender = eConstraintsHeight) and (eConstraintsHeight.Text <> '#') then
  begin
    LValue := StrToIntDef(eConstraintsHeight.Text, 0);
    FLayoutInfo.ConstraintsMinHeight := LValue;
    FLayoutInfo.ConstraintsMaxHeight := LValue;
  end;
  EndUpdate;
end;

procedure TedtuAnchorEditor.eLeftChange(Sender: TObject);
var
  LForm: TCustomForm;
begin
  if FRefreshDisabled then
    Exit;

  BeginUpdate;

  try
    if (Sender = eLeft) and (eLeft.Text <> '#') then
      FLayoutInfo.Left := eLeft.Value
    else if (Sender = eTop) and (eTop.Text <> '#') then
      FLayoutInfo.Top := eTop.Value
    else if (Sender = eWidth) and (eWidth.Text <> '#') then
      FLayoutInfo.Width := eWidth.Value
    else if (Sender = eHeight) and (eHeight.Text <> '#') then
      FLayoutInfo.Height := eHeight.Value;
  finally
    EndUpdate;
  end;
end;

procedure TedtuAnchorEditor.sbDefaultMinMaxValuesClick(Sender: TObject);
begin
  eConstraintsWidth.Text := eWidth.Text;
  eConstraintsWidthChange(eConstraintsWidth);
  eConstraintsHeight.Text := eHeight.Text;
  eConstraintsWidthChange(eConstraintsHeight);
end;

procedure TedtuAnchorEditor.sbEHAnchorAligningClick(Sender: TObject);
begin
  if Sender = sbEHAnchorAligning        then FLayoutInfo.ChildSizingEnlargeHorizontal := crsAnchorAligning        else
  if Sender = sbEHScaleChilds           then FLayoutInfo.ChildSizingEnlargeHorizontal := crsScaleChilds           else
  if Sender = sbEHHomogenousChildResize then FLayoutInfo.ChildSizingEnlargeHorizontal := crsHomogenousChildResize else
  if Sender = sbEHHomogenousSpaceResize then FLayoutInfo.ChildSizingEnlargeHorizontal := crsHomogenousSpaceResize else

  if Sender = sbEVAnchorAligning        then FLayoutInfo.ChildSizingEnlargeVertical := crsAnchorAligning        else
  if Sender = sbEVScaleChilds           then FLayoutInfo.ChildSizingEnlargeVertical := crsScaleChilds           else
  if Sender = sbEVHomogenousChildResize then FLayoutInfo.ChildSizingEnlargeVertical := crsHomogenousChildResize else
  if Sender = sbEVHomogenousSpaceResize then FLayoutInfo.ChildSizingEnlargeVertical := crsHomogenousSpaceResize else

  if Sender = sbSHAnchorAligning        then FLayoutInfo.ChildSizingShrinkHorizontal := crsAnchorAligning        else
  if Sender = sbSHScaleChilds           then FLayoutInfo.ChildSizingShrinkHorizontal := crsScaleChilds           else
  if Sender = sbSHHomogenousChildResize then FLayoutInfo.ChildSizingShrinkHorizontal := crsHomogenousChildResize else
  if Sender = sbSHHomogenousSpaceResize then FLayoutInfo.ChildSizingShrinkHorizontal := crsHomogenousSpaceResize else

  if Sender = sbSVAnchorAligning        then FLayoutInfo.ChildSizingShrinkVertical := crsAnchorAligning        else
  if Sender = sbSVScaleChilds           then FLayoutInfo.ChildSizingShrinkVertical := crsScaleChilds           else
  if Sender = sbSVHomogenousChildResize then FLayoutInfo.ChildSizingShrinkVertical := crsHomogenousChildResize else
  if Sender = sbSVHomogenousSpaceResize then FLayoutInfo.ChildSizingShrinkVertical := crsHomogenousSpaceResize else
end;

procedure TedtuAnchorEditor.sbHCellFillClick(Sender: TObject);
begin
  if Sender = sbHCellFill then
    FLayoutInfo.CellAlignH := ccaFill
  else if Sender = sbHCellLeftTop then
    FLayoutInfo.CellAlignH := ccaLeftTop
  else if Sender = sbHCellRightBottom then
    FLayoutInfo.CellAlignH := ccaRightBottom
  else if Sender = sbHCellCenter then
    FLayoutInfo.CellAlignH := ccaCenter

  else if Sender = sbVCellFill then
    FLayoutInfo.CellAlignV := ccaFill
  else if Sender = sbVCellLeftTop then
    FLayoutInfo.CellAlignV := ccaLeftTop
  else if Sender = sbVCellRightBottom then
    FLayoutInfo.CellAlignV := ccaRightBottom
  else if Sender = sbVCellCenter then
    FLayoutInfo.CellAlignV := ccaCenter

end;

procedure TedtuAnchorEditor.sbLayoutNoneClick(Sender: TObject);
begin
  if Sender = sbLayoutNone then
    FLayoutInfo.Layout := cclNone
  else if Sender = sbLayoutLeftToRightThenTopToBottom then
    FLayoutInfo.Layout := cclLeftToRightThenTopToBottom
  else if Sender = sbLayoutTopToBottomThenLeftToRight then
    FLayoutInfo.Layout := cclTopToBottomThenLeftToRight;
end;

procedure TedtuAnchorEditor.sbLeftSideCenterClick(Sender: TObject);
var
  LSide: TAnchorSideInfo;
  i: TAnchorSideReference;
begin
  LSide := GetSide(Sender as TSpeedButton);
  for i := Low(TAnchorSideReference) to High(TAnchorSideReference) do
    if LSide.Buttons[i] = Sender then
    begin
      // by nam nie zamykalo panelu z buttonami...
      BeginUpdate;
      LSide.Side := i;
      EndUpdate;
      Exit;
    end;
end;

procedure TedtuAnchorEditor.sbShowEnlargeShrinkClick(Sender: TObject);
begin
  if not pEnlargeShrink.Visible then
  begin
    pChildSizing.Height := 192;
    sbShowEnlargeShrink.LoadGlyphFromResourceName(HINSTANCE, 'arrow_up');
    pEnlargeShrink.Visible := True;
  end
  else
  begin
    pChildSizing.Height := 144;
    sbShowEnlargeShrink.LoadGlyphFromResourceName(HINSTANCE, 'arrow_down');
    pEnlargeShrink.Visible := False;
  end
end;

procedure TedtuAnchorEditor.sbShowMinMaxClick(Sender: TObject);
begin
  if pConstraints.Height = 40 then
  begin
    pConstraints.Height := 96;
    sbShowMinMax.LoadGlyphFromResourceName(HINSTANCE, 'arrow_up');
  end
  else
  begin
    pConstraints.Height := 40;
    sbShowMinMax.LoadGlyphFromResourceName(HINSTANCE, 'arrow_down');
  end
end;

procedure TedtuAnchorEditor.seBSLeftChange(Sender: TObject);
begin
  if FRefreshDisabled then
    Exit;

  BeginUpdate;

  if (Sender = seBSLeft) and (seBSLeft.Text <> '#') then
    FLayoutInfo.BorderSpaceLeft := StrToIntDef(seBSLeft.Text, 0)
  else if (Sender = seBSTop) and (seBSTop.Text <> '#') then
    FLayoutInfo.BorderSpaceTop := StrToIntDef(seBSTop.Text, 0)
  else if (Sender = seBSRight) and (seBSRight.Text <> '#') then
    FLayoutInfo.BorderSpaceRight := StrToIntDef(seBSRight.Text, 0)
  else if (Sender = seBSBottom) and (seBSBottom.Text <> '#') then
    FLayoutInfo.BorderSpaceBottom := StrToIntDef(seBSBottom.Text, 0)
  else if (Sender = seBSAround) and (seBSAround.Text <> '#') then
    FLayoutInfo.BorderSpaceAround := StrToIntDef(seBSAround.Text, 0)
  else if (Sender = seBSInner) and (seBSInner.Text <> '#') then
    FLayoutInfo.BorderSpaceInner := StrToIntDef(seBSInner.Text, 0);

  EndUpdate;
end;

procedure TedtuAnchorEditor.bEditTableClick(Sender: TObject);
begin
  if FSelectedControls.Count <> 1 then
    Exit;

  if fedtuAnchorDockingTable = nil then
    fedtuAnchorDockingTable := TfedtuAnchorDockingTable.Create(nil);
  fedtuAnchorDockingTable.ShowModal(FSelectedControls[0] as TWinControl);
end;

procedure TedtuAnchorEditor.seControlsPerLineChange(Sender: TObject);
begin
  if FRefreshDisabled then
    Exit;

  BeginUpdate;

  if seControlsPerLine.Text <> '#' then
    FLayoutInfo.ControlsPerLine := StrToIntDef(seControlsPerLine.Text, 0);

  EndUpdate;

  // aktywuj/deaktywuj button od przycisków
  FLayoutInfo.RefreshControls;
end;

procedure TedtuAnchorEditor.seCSLeftRightChange(Sender: TObject);
begin
  if FRefreshDisabled then
    Exit;

  BeginUpdate;

  if (Sender = seCSLeftRight) and (seCSLeftRight.Text <> '#') then
    FLayoutInfo.ChildSpaceLeftRight := StrToIntDef(seCSLeftRight.Text, 0)
  else if (Sender = seCSTopBottom) and (seCSTopBottom.Text <> '#') then
    FLayoutInfo.ChildSpaceTopBottom := StrToIntDef(seCSTopBottom.Text, 0)
  else if (Sender = seCSHorizontal) and (seCSHorizontal.Text <> '#') then
    FLayoutInfo.ChildSpaceHorizontal := StrToIntDef(seCSHorizontal.Text, 0)
  else if (Sender = seCSVertical) and (seCSVertical.Text <> '#') then
    FLayoutInfo.ChildSpaceVertical := StrToIntDef(seCSVertical.Text, 0);

  EndUpdate;
end;

function TedtuAnchorEditor.GetSide(AButton: TSpeedButton): TAnchorSideInfo;
begin
  if (AButton = sbLeftSideLeft) or (AButton = sbLeftSideRight) or (AButton = sbLeftSideCenter) then
    Exit(FLayoutInfo.AnchorInfo[akLeft])
  else if (AButton = sbTopSideTop) or (AButton = sbTopSideBottom) or (AButton = sbTopSideCenter) then
    Exit(FLayoutInfo.AnchorInfo[akTop])
  else if (AButton = sbRightSideLeft) or (AButton = sbRightSideRight) or (AButton = sbRightSideCenter) then
    Exit(FLayoutInfo.AnchorInfo[akRight])
  else if (AButton = sbBottomSideTop) or (AButton = sbBottomSideBottom) or (AButton = sbBottomSideCenter) then
    Exit(FLayoutInfo.AnchorInfo[akBottom])
  else
    Result := nil;
end;

function TedtuAnchorEditor.GetRoot: TPersistent;
begin
  Result := FRoot;
end;

function TedtuAnchorEditor.GetSide(AAnchorKind: TAnchorKind): TAnchorSideInfo;
begin
  Exit(FLayoutInfo.AnchorInfo[AAnchorKind]);
end;

function TedtuAnchorEditor.GetConstraintIdx(ASpinEdit: TEdit): Integer;
begin
  if ASpinEdit = eConstraintsMinWidth then
    Exit(CONSTRAINTS_MIN_WIDTH)
  else if ASpinEdit = eConstraintsMaxWidth then
    Exit(CONSTRAINTS_MAX_WIDTH)
  else if ASpinEdit = eConstraintsMinHeight then
    Exit(CONSTRAINTS_MIN_HEIGHT)
  else if ASpinEdit = eConstraintsMaxHeight then
    Exit(CONSTRAINTS_MAX_HEIGHT)
end;

procedure TedtuAnchorEditor.FillSiblingsComboBoxes;
var
  LCtrl: TControl;
  LParent: TWinControl = nil;
  LSiblingParent: TWinControl = nil;
  LSiblings: TStringList;
  i: Integer;
begin
  LSiblings := TStringList.Create;

  // wyelminuj root i komponenty bez wspolnego rodzica
  for LCtrl in FSelectedControls do
  begin
    if LCtrl = FRoot then
    begin
      LParent := nil;
      // zawsze dopusc wartosc (nil)
      Break;
    end;
    if LParent = nil then
      LParent := LCtrl.Parent
    // dopusc jednak parenta jesli parent jednego z nich jest rodzenstwem drugiego
    else if LParent <> LCtrl.Parent then
    begin
      // sprawdź czy rodzic rodzica jest rodzenstwem istniejacego rodzica.
      // jesli tak to znaczy ze istniejacy rodzic powstal z kontrolki ktora
      // jest rodzenstwem nowego rodzica.
      // dla poprawnego dzialania zdarzenie moze zajsc raz
      if (LCtrl.Parent <> nil) and (LCtrl.Parent.Parent = LParent) and (LSiblingParent = nil) then
      begin
        LSiblingParent := LCtrl.Parent;
        LParent := LCtrl.Parent;
      end
      else
      // sprawdź czy istniejacy rodzic jest rodzeństwem nowej kontroli.
      // zdarzenie moze zajsc wielokrotnie
      if LParent.Parent = LCtrl.Parent then
        LSiblingParent := LParent
      else
      // niestety nie mozemy wywnioskowac kontrolkido ktorej mozemy wyrownac
      begin
        LParent := nil;
        Break;
      end;
    end;
  end;

  // mozliwe sa jedynie dzieci parenta
  // ale parent musi byc wspolny... (patrz wyzej)
  if (LParent <> nil) and (LSiblingParent = nil) then
    for i := 0 to LParent.ControlCount - 1 do
    begin
      LCtrl := LParent.Controls[i];
      if not FSelectedControls.Contains(LCtrl) then
        LSiblings.AddObject(LCtrl.Name + ':' + LCtrl.ClassName, LCtrl);
    end;

  // jesli root jest zaznaczony moze byc nil;
  if LSiblings <> nil then
  begin
    LSiblings.AddObject('(nil)', nil);
    if LParent <> nil then
      if LSiblingParent = LParent then
        LSiblings.AddObject(LParent.Name + ':' + LParent.ClassName, LParent)
      else
        LSiblings.AddObject(Format('(parent) %s', [LParent.Name]), LParent);
    LSiblings.Sort;

    cbAnchorSideLeftCtrl.Items.Assign(LSiblings);
    cbAnchorSideTopCtrl.Items.Assign(LSiblings);
    cbAnchorSideRightCtrl.Items.Assign(LSiblings);
    cbAnchorSideBottomCtrl.Items.Assign(LSiblings);

    LSiblings.Free;
  end
  else
  begin
    cbAnchorSideLeftCtrl.Clear;
    cbAnchorSideTopCtrl.Clear;
    cbAnchorSideRightCtrl.Clear;
    cbAnchorSideBottomCtrl.Clear;
  end;
end;

procedure TedtuAnchorEditor.RefreshValues;
var
  i: Integer;
  k: TAnchorKind;
  LCtrl: TControl;
  LWinCtrl: TWinControl absolute LCtrl;
  LSideInfo: TAnchorSideInfo;
  LSide: TAnchorSide;

//  function GetControlPath(ACtrl: TControl): string;
//  var
//    LParent: TWinControl;
//  begin
//    LParent := ACtrl.Parent;
//    while LParent <> nil do
//    begin
//      Result := LParent.Name + ' ' + Result;
//      LParent := LParent.Parent;
//    end;
//  end;

begin
  Self.Enabled := FRoot <> nil;

  if FRefreshDisabled or (GlobalDesignHook.LookupRoot <> FRoot) or (FRoot = nil) then
    Exit;

  FSelectedControls.Clear;
  GlobalDesignHook.GetSelection(FSelectionList);
  FWinControls := FSelectionList.Count > 0;
  for i := 0 to FSelectionList.Count - 1 do
  begin
    if FSelectionList[i] is TControl then
    begin
      LCtrl := TControl(FSelectionList[i]);
      FSelectedControls.Add(LCtrl);
      if LCtrl is TWinControl then
        FWinControls := FWinControls and (IsPublishedProp(LCtrl, 'ChildSizing'))
      else
        FWinControls := False;
    end;
  end;

  case FSelectedControls.Count of
    0: lCtrlName.Caption := 'none';
    1: lCtrlName.Caption := Format('%s:%s', [FSelectedControls[0].Name, FSelectedControls[0].ClassName]);
  else
    lCtrlName.Caption := Format('%d controls', [FSelectedControls.Count]);
  end;

  FillSiblingsComboBoxes;

  if FSelectedControls.Count > 0 then
  begin
    FLayoutInfo.Clear;

    // inicjalizacja Site
    LCtrl := FSelectedControls[0];
    for k := Low(TAnchorKind) to High(TAnchorKind) do
    begin
      LSideInfo := GetSide(k);
      LSide := LCtrl.AnchorSide[k];

      LSideInfo.FSide := LSide.Side;
      LSideInfo.FControl := LSide.Control;
      LSideInfo.FEnabled := k in LCtrl.Anchors;
    end;
    // inicjalizacja pozostalych
    FLayoutInfo.FConstraints[CONSTRAINTS_MIN_WIDTH] := LCtrl.Constraints.MinWidth;
    FLayoutInfo.FConstraints[CONSTRAINTS_MAX_WIDTH] := LCtrl.Constraints.MaxWidth;
    FLayoutInfo.FConstraints[CONSTRAINTS_MIN_HEIGHT] := LCtrl.Constraints.MinHeight;
    FLayoutInfo.FConstraints[CONSTRAINTS_MAX_HEIGHT] := LCtrl.Constraints.MaxHeight;

    FLayoutInfo.FAlign := LCtrl.Align;

    if (LCtrl is TCustomForm) and (LCtrl = FRoot) then
    begin
      FLayoutInfo.FBounds[BOUNDS_LEFT]   := TFakeForm(LCtrl).Left;
      FLayoutInfo.FBounds[BOUNDS_TOP]    := TFakeForm(LCtrl).Top;
      FLayoutInfo.FBounds[BOUNDS_WIDTH]  := TFakeForm(LCtrl).Width;
      FLayoutInfo.FBounds[BOUNDS_HEIGHT] := TFakeForm(LCtrl).Height;
    end
    else
    begin
      FLayoutInfo.FBounds[BOUNDS_LEFT]   := LCtrl.Left;
      FLayoutInfo.FBounds[BOUNDS_TOP]    := LCtrl.Top;
      FLayoutInfo.FBounds[BOUNDS_WIDTH]  := LCtrl.Width;
      FLayoutInfo.FBounds[BOUNDS_HEIGHT] := LCtrl.Height;
    end;

    FLayoutInfo.FBorderSpace[BORDER_SPACE_LEFT  ] := LCtrl.BorderSpacing.Left;
    FLayoutInfo.FBorderSpace[BORDER_SPACE_TOP   ] := LCtrl.BorderSpacing.Top;
    FLayoutInfo.FBorderSpace[BORDER_SPACE_RIGHT ] := LCtrl.BorderSpacing.Right;
    FLayoutInfo.FBorderSpace[BORDER_SPACE_BOTTOM] := LCtrl.BorderSpacing.Bottom;
    FLayoutInfo.FBorderSpace[BORDER_SPACE_AROUND] := LCtrl.BorderSpacing.Around;
    FLayoutInfo.FBorderSpace[BORDER_SPACE_INNER ] := LCtrl.BorderSpacing.InnerBorder;

    FLayoutInfo.FCellAlign[CELL_ALIGN_HORIZONTAL] := LCtrl.BorderSpacing.CellAlignHorizontal;
    FLayoutInfo.FCellAlign[CELL_ALIGN_VERTICAL  ] := LCtrl.BorderSpacing.CellAlignVertical;

    if FWinControls then
    begin
      FLayoutInfo.FLayout := LWinCtrl.ChildSizing.Layout;
      FLayoutInfo.FControlsPerLine := LWinCtrl.ChildSizing.ControlsPerLine;

      FLayoutInfo.FChildSpace[CHILD_SPACE_HORIZONTAL] := LWinCtrl.ChildSizing.HorizontalSpacing;
      FLayoutInfo.FChildSpace[CHILD_SPACE_VERTICAL  ] := LWinCtrl.ChildSizing.VerticalSpacing  ;
      FLayoutInfo.FChildSpace[CHILD_SPACE_LEFT_RIGHT] := LWinCtrl.ChildSizing.LeftRightSpacing ;
      FLayoutInfo.FChildSpace[CHILD_SPACE_TOP_BOTTOM] := LWinCtrl.ChildSizing.TopBottomSpacing ;

      FLayoutInfo.FChildSizing[CHILD_SIZING_ENLARGE_HORIZONTAL] := LWinCtrl.ChildSizing.EnlargeHorizontal;
      FLayoutInfo.FChildSizing[CHILD_SIZING_ENLARGE_VERTICAL  ] := LWinCtrl.ChildSizing.EnlargeVertical  ;
      FLayoutInfo.FChildSizing[CHILD_SIZING_SHRINK_HORIZONTAL ] := LWinCtrl.ChildSizing.ShrinkHorizontal ;
      FLayoutInfo.FChildSizing[CHILD_SIZING_SHRINK_VERTICAL   ] := LWinCtrl.ChildSizing.ShrinkVertical   ;
    end;

    FLayoutInfo.FAutoSize := LCtrl.AutoSize;
    FLayoutInfo.FVisible := LCtrl.Visible;


    // sprawdz czy grupa ma te same wartosci
    for LCtrl in FSelectedControls do
    begin
      // dla stron
      for k := Low(TAnchorKind) to High(TAnchorKind) do
      begin
        LSideInfo := GetSide(k);
        LSide := LCtrl.AnchorSide[k];

        LSideInfo.UnknownSide := LSideInfo.UnknownSide or (LSideInfo.Side <> LSide.Side);
        LSideInfo.UnknownControl := LSideInfo.UnknownControl or (LSideInfo.Control <> LSide.Control);
        LSideInfo.UnknownEnabled := LSideInfo.UnknownEnabled or (LSideInfo.Enabled <> (k in LCtrl.Anchors));
      end;
      // dla pozostalych
      FLayoutInfo.UnknownConstraintsMinWidth  := FLayoutInfo.UnknownConstraintsMinWidth  or (LCtrl.Constraints.MinWidth  <> FLayoutInfo.ConstraintsMinWidth );
      FLayoutInfo.UnknownConstraintsMaxWidth  := FLayoutInfo.UnknownConstraintsMaxWidth  or (LCtrl.Constraints.MaxWidth  <> FLayoutInfo.ConstraintsMaxWidth );
      FLayoutInfo.UnknownConstraintsMinHeight := FLayoutInfo.UnknownConstraintsMinHeight or (LCtrl.Constraints.MinHeight <> FLayoutInfo.ConstraintsMinHeight);
      FLayoutInfo.UnknownConstraintsMaxHeight := FLayoutInfo.UnknownConstraintsMaxHeight or (LCtrl.Constraints.MaxHeight <> FLayoutInfo.ConstraintsMaxHeight);
      // align
      FLayoutInfo.UnknownAlign := FLayoutInfo.UnknownAlign or (FLayoutInfo.Align <> LCtrl.Align);
      // dla width/height
      if (LCtrl is TCustomForm) and (LCtrl = FRoot) then
      begin
        FLayoutInfo.UnknownLeft := FLayoutInfo.UnknownLeft or (TFakeForm(LCtrl).Left <> FLayoutInfo.Left);
        FLayoutInfo.UnknownTop := FLayoutInfo.UnknownTop or (TFakeForm(LCtrl).Top <> FLayoutInfo.Top);
        FLayoutInfo.UnknownWidth := FLayoutInfo.UnknownWidth or (TFakeForm(LCtrl).Width <> FLayoutInfo.Width);
        FLayoutInfo.UnknownHeight := FLayoutInfo.UnknownHeight or (TFakeForm(LCtrl).Height <> FLayoutInfo.Height);
      end
      else
      begin
        FLayoutInfo.UnknownLeft := FLayoutInfo.UnknownLeft or (LCtrl.Left <> FLayoutInfo.Left);
        FLayoutInfo.UnknownTop := FLayoutInfo.UnknownTop or (LCtrl.Top <> FLayoutInfo.Top);
        FLayoutInfo.UnknownWidth := FLayoutInfo.UnknownWidth or (LCtrl.Width <> FLayoutInfo.Width);
        FLayoutInfo.UnknownHeight := FLayoutInfo.UnknownHeight or (LCtrl.Height <> FLayoutInfo.Height);
      end;
      // dla border space
      FLayoutInfo.UnknownBorderSpaceLeft := FLayoutInfo.UnknownBorderSpaceLeft or (LCtrl.BorderSpacing.Left <> FLayoutInfo.BorderSpaceLeft);
      FLayoutInfo.UnknownBorderSpaceTop := FLayoutInfo.UnknownBorderSpaceTop or (LCtrl.BorderSpacing.Top <> FLayoutInfo.BorderSpaceTop);
      FLayoutInfo.UnknownBorderSpaceRight := FLayoutInfo.UnknownBorderSpaceRight or (LCtrl.BorderSpacing.Right <> FLayoutInfo.BorderSpaceRight);
      FLayoutInfo.UnknownBorderSpaceBottom := FLayoutInfo.UnknownBorderSpaceBottom or (LCtrl.BorderSpacing.Bottom <> FLayoutInfo.BorderSpaceBottom);
      FLayoutInfo.UnknownBorderSpaceAround := FLayoutInfo.UnknownBorderSpaceAround or (LCtrl.BorderSpacing.Around <> FLayoutInfo.BorderSpaceAround);
      FLayoutInfo.UnknownBorderSpaceInner := FLayoutInfo.UnknownBorderSpaceInner or (LCtrl.BorderSpacing.InnerBorder <> FLayoutInfo.BorderSpaceInner);
      // cell align
      FLayoutInfo.UnknownCellAlignH := FLayoutInfo.UnknownCellAlignH or (LCtrl.BorderSpacing.CellAlignHorizontal <> FLayoutInfo.CellAlignH);
      FLayoutInfo.UnknownCellAlignV := FLayoutInfo.UnknownCellAlignV or (LCtrl.BorderSpacing.CellAlignVertical <> FLayoutInfo.CellAlignV);

      if FWinControls then
      begin
        FLayoutInfo.UnknownLayout := FLayoutInfo.UnknownLayout or (LWinCtrl.ChildSizing.Layout <> FLayoutInfo.Layout);
        FLayoutInfo.UnknownControlsPerLine := FLayoutInfo.UnknownControlsPerLine or (LWinCtrl.ChildSizing.ControlsPerLine <> FLayoutInfo.ControlsPerLine);

        FLayoutInfo.UnknownChildSpaceHorizontal := FLayoutInfo.UnknownChildSpaceHorizontal or (LWinCtrl.ChildSizing.HorizontalSpacing <> FLayoutInfo.ChildSpaceHorizontal);
        FLayoutInfo.UnknownChildSpaceVertical := FLayoutInfo.UnknownChildSpaceVertical or (LWinCtrl.ChildSizing.VerticalSpacing <> FLayoutInfo.ChildSpaceVertical);
        FLayoutInfo.UnknownChildSpaceLeftRight := FLayoutInfo.UnknownChildSpaceLeftRight or (LWinCtrl.ChildSizing.LeftRightSpacing <> FLayoutInfo.ChildSpaceLeftRight);
        FLayoutInfo.UnknownChildSpaceTopBottom := FLayoutInfo.UnknownChildSpaceTopBottom or (LWinCtrl.ChildSizing.TopBottomSpacing <> FLayoutInfo.ChildSpaceTopBottom);

        FLayoutInfo.UnknownChildSizingEnlargeHorizontal := FLayoutInfo.UnknownChildSizingEnlargeHorizontal or (LWinCtrl.ChildSizing.EnlargeHorizontal <> FLayoutInfo.ChildSizingEnlargeHorizontal);
        FLayoutInfo.UnknownChildSizingEnlargeVertical := FLayoutInfo.UnknownChildSizingEnlargeVertical or (LWinCtrl.ChildSizing.EnlargeVertical <> FLayoutInfo.ChildSizingEnlargeVertical);
        FLayoutInfo.UnknownChildSizingShrinkHorizontal := FLayoutInfo.UnknownChildSizingShrinkHorizontal or (LWinCtrl.ChildSizing.ShrinkHorizontal <> FLayoutInfo.ChildSizingShrinkHorizontal);
        FLayoutInfo.UnknownChildSizingShrinkVertical := FLayoutInfo.UnknownChildSizingShrinkVertical or (LWinCtrl.ChildSizing.ShrinkVertical <> FLayoutInfo.ChildSizingShrinkVertical);
      end;

      FLayoutInfo.UnknownAutoSize := FLayoutInfo.UnknownAutoSize or (LCtrl.AutoSize <> FLayoutInfo.AutoSize);
      FLayoutInfo.UnknownVisible := FLayoutInfo.UnknownVisible or (LCtrl.Visible <> FLayoutInfo.Visible);
    end;

    //
    FLayoutInfo.RefreshControls;

    Enabled := True;
  end
  else
    Enabled := False;
end;

function TedtuAnchorEditor.GetSide(AComboBox: TComboBox): TAnchorSideInfo;
begin
  if AComboBox = cbAnchorSideLeftCtrl then
    Exit(FLayoutInfo.AnchorInfo[akLeft])
  else if AComboBox = cbAnchorSideTopCtrl then
    Exit(FLayoutInfo.AnchorInfo[akTop])
  else if AComboBox = cbAnchorSideRightCtrl then
    Exit(FLayoutInfo.AnchorInfo[akRight])
  else if AComboBox = cbAnchorSideBottomCtrl then
    Exit(FLayoutInfo.AnchorInfo[akBottom])
  else
    Result := nil;
end;

function TedtuAnchorEditor.GetSide(ACheckBox: TCheckBox): TAnchorSideInfo;
begin
  if ACheckBox = cbLeftEnabled then
    Exit(FLayoutInfo.AnchorInfo[akLeft])
  else if ACheckBox = cbTopEnabled then
    Exit(FLayoutInfo.AnchorInfo[akTop])
  else if ACheckBox = cbRightEnabled then
    Exit(FLayoutInfo.AnchorInfo[akRight])
  else if ACheckBox = cbBottomEnabled then
    Exit(FLayoutInfo.AnchorInfo[akBottom])
  else
    Result := nil;
end;

procedure TedtuAnchorEditor.OnDesignRefreshPropertyValues;
var
  LCtrl: TControl;
  LForm: TCustomForm;
begin
  RefreshValues;
end;

procedure TedtuAnchorEditor.OnDesignSetSelection(
  const ASelection: TPersistentSelectionList);
begin
  if FSelectionList.IsEqual(ASelection) then
    Exit;
  RefreshValues;
end;

procedure TedtuAnchorEditor.BeginUpdate;
begin
  FRefreshDisabled := True;;
end;

procedure TedtuAnchorEditor.EndUpdate;
begin
  FRefreshDisabled := False;
end;

procedure TedtuAnchorEditor.SetRoot(AValue: TPersistent);
begin
  if FRoot = AValue then
    Exit;
  FRoot := AValue;
  RefreshValues;
end;

function TedtuAnchorEditor.GetParent: TWinControl;
begin
  Result := Parent;
end;

function TedtuAnchorEditor.GetVisible: Boolean;
begin
  Result := Visible
end;

procedure TedtuAnchorEditor.ComputeScrollbars;
var
  LSize: Integer;
begin
  //Result := inherited;
//  KindID := SM_CYHSCROLL
//else
//  KindID := SM_CXVSCROLL;
  if TControlScrollBarAccess(VertScrollBar).HandleAllocated then
    LSize := LCLIntf.GetScrollBarSize(TControlScrollBarAccess(VertScrollBar).ControlHandle,SM_CYHSCROLL)
  else
    LSize := GetSystemMetrics(SM_CYHSCROLL);
  //ClientWidth:=;
  if Visible then
    if TControlScrollBarAccess(VertScrollBar).ScrollBarShouldBeVisible then
    begin
      TPanel(Owner).Width := 256 + LSize;
    end
    else
      TPanel(Owner).Width := 256;

///  WriteLn('Woooooof! ', TControlScrollBarAccess(VertScrollBar).ScrollBarShouldBeVisible, ' ', height);
end;

constructor TedtuAnchorEditor.Create(TheOwner: TComponent; ARoot: TPersistent);

  procedure FillAlignComboBox;
  var
    i: TAlign;
  begin
    for i := Low(TAlign) to High(TAlign) do
      cbAlign.AddItem(GetEnumName(TypeInfo(TAlign), Integer(i)), TObject(i));
  end;

begin
  inherited Create(TheOwner);

  FLayoutInfo := TLayoutInfo.Create(Self);

  cbLeftEnabled.Hint := sisLeftAnchoringCheckBox;
  cbTopEnabled.Hint := sisTopAnchoringCheckBox;
  cbRightEnabled.Hint := sisRightAnchoringCheckBox;
  cbBottomEnabled.Hint := sisBottomAnchoringCheckBox;

  sbLeftSideLeft.LoadGlyphFromResourceName(HINSTANCE, 'anchor_left');
  sbLeftSideRight.LoadGlyphFromResourceName(HINSTANCE, 'anchor_left_right');
  sbLeftSideCenter.LoadGlyphFromResourceName(HINSTANCE, 'anchor_center_horizontal');
//  sbLeftSideUnknown.LoadGlyphFromResourceName(HINSTANCE, 'state_unknown');
  sbTopSideTop.LoadGlyphFromResourceName(HINSTANCE, 'anchor_top');
  sbTopSideBottom.LoadGlyphFromResourceName(HINSTANCE, 'anchor_top_bottom');
  sbTopSideCenter.LoadGlyphFromResourceName(HINSTANCE, 'anchor_center_vertical');
//  sbTopSideUnknown.LoadGlyphFromResourceName(HINSTANCE, 'state_unknown');
  sbRightSideLeft.LoadGlyphFromResourceName(HINSTANCE, 'anchor_left_right');
  sbRightSideRight.LoadGlyphFromResourceName(HINSTANCE, 'anchor_right');
  sbRightSideCenter.LoadGlyphFromResourceName(HINSTANCE, 'anchor_center_horizontal');
//  sbRightSideUnknown.LoadGlyphFromResourceName(HINSTANCE, 'state_unknown');
  sbBottomSideTop.LoadGlyphFromResourceName(HINSTANCE, 'anchor_top_bottom');
  sbBottomSideBottom.LoadGlyphFromResourceName(HINSTANCE, 'anchor_bottom');
  sbBottomSideCenter.LoadGlyphFromResourceName(HINSTANCE, 'anchor_center_vertical');
//  sbBottomSideUnknown.LoadGlyphFromResourceName(HINSTANCE, 'state_unknown');

  sbHCellCenter.LoadGlyphFromResourceName(HINSTANCE, 'anchor_center_horizontal');
  sbHCellFill.LoadGlyphFromResourceName(HINSTANCE, 'align');
  sbHCellLeftTop.LoadGlyphFromResourceName(HINSTANCE, 'anchor_left');
  sbHCellRightBottom.LoadGlyphFromResourceName(HINSTANCE, 'anchor_right');
  sbVCellCenter.LoadGlyphFromResourceName(HINSTANCE, 'anchor_center_vertical');
  sbVCellFill.LoadGlyphFromResourceName(HINSTANCE, 'align');
  sbVCellLeftTop.LoadGlyphFromResourceName(HINSTANCE, 'anchor_top');
  sbVCellRightBottom.LoadGlyphFromResourceName(HINSTANCE, 'anchor_bottom');

  sbShowMinMax.LoadGlyphFromResourceName(HINSTANCE, 'arrow_down');
  sbShowEnlargeShrink.LoadGlyphFromResourceName(HINSTANCE, 'arrow_down');
  sbDefaultMinMaxValues.LoadGlyphFromResourceName(HINSTANCE, 'btn_all');

  sbEHAnchorAligning.LoadGlyphFromResourceName(HINSTANCE, 'ANCHOR_ALIGNING_HORIZONTAL');
  sbEVAnchorAligning.LoadGlyphFromResourceName(HINSTANCE, 'ANCHOR_ALIGNING_VERTICAL');
  sbSHAnchorAligning.LoadGlyphFromResourceName(HINSTANCE, 'ANCHOR_ALIGNING_HORIZONTAL');
  sbSVAnchorAligning.LoadGlyphFromResourceName(HINSTANCE, 'ANCHOR_ALIGNING_VERTICAL');
  sbEHHomogenousChildResize.LoadGlyphFromResourceName(HINSTANCE, 'HOMOGENOUS_CHILD_RESIZE_HORIZONTAL');
  sbEVHomogenousChildResize.LoadGlyphFromResourceName(HINSTANCE, 'HOMOGENOUS_CHILD_RESIZE_VERTICAL');
  sbSHHomogenousChildResize.LoadGlyphFromResourceName(HINSTANCE, 'HOMOGENOUS_CHILD_RESIZE_HORIZONTAL');
  sbSVHomogenousChildResize.LoadGlyphFromResourceName(HINSTANCE, 'HOMOGENOUS_CHILD_RESIZE_VERTICAL');
  sbEHHomogenousSpaceResize.LoadGlyphFromResourceName(HINSTANCE, 'HOMOGENOUS_SPACE_RESIZE_HORIZONTAL');
  sbEVHomogenousSpaceResize.LoadGlyphFromResourceName(HINSTANCE, 'HOMOGENOUS_SPACE_RESIZE_VERTICAL');
  sbSHHomogenousSpaceResize.LoadGlyphFromResourceName(HINSTANCE, 'HOMOGENOUS_SPACE_RESIZE_HORIZONTAL');
  sbSVHomogenousSpaceResize.LoadGlyphFromResourceName(HINSTANCE, 'HOMOGENOUS_SPACE_RESIZE_VERTICAL');
  sbEHScaleChilds.LoadGlyphFromResourceName(HINSTANCE, 'SCALE_CHILDS_HORIZONTAL');
  sbEVScaleChilds.LoadGlyphFromResourceName(HINSTANCE, 'SCALE_CHILDS_VERTICAL');
  sbSHScaleChilds.LoadGlyphFromResourceName(HINSTANCE, 'SCALE_CHILDS_HORIZONTAL');
  sbSVScaleChilds.LoadGlyphFromResourceName(HINSTANCE, 'SCALE_CHILDS_VERTICAL');

  sbLayoutLeftToRightThenTopToBottom.LoadGlyphFromResourceName(HINSTANCE, 'LAYOUT_LEFT_TO_RIGHT_THEN_TOP_TO_BOTTOM');
  sbLayoutNone.LoadGlyphFromResourceName(HINSTANCE, 'LAYOUT_NONE');
  sbLayoutTopToBottomThenLeftToRight.LoadGlyphFromResourceName(HINSTANCE, 'LAYOUT_TOP_TO_BOTTOM_THEN_LEFT_TO_RIGHT');

  FillAlignComboBox;

  FSelectionList := TPersistentSelectionList.Create;
  FSelectedControls := TList<TControl>.Create;

  // RefreshValues;

  GlobalDesignHook.AddHandlerRefreshPropertyValues(OnDesignRefreshPropertyValues);
  GlobalDesignHook.AddHandlerSetSelection(OnDesignSetSelection);

  Root := ARoot;
  //FResizer := AResizer;
end;

destructor TedtuAnchorEditor.Destroy;
begin
  GlobalDesignHook.RemoveHandlerRefreshPropertyValues(OnDesignRefreshPropertyValues);
  GlobalDesignHook.RemoveHandlerSetSelection(OnDesignSetSelection);

  FSelectionList.Free;
  FSelectedControls.Free;
  FLayoutInfo.Free;

  inherited Destroy;
end;

end.

