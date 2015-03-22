{**********************************************************************
 Package pl_ExDesign
 This unit is part of CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************}

unit ELDsgxObjectInsp;

{$mode objfpc}{$H+}

interface

uses
  // FCL
  SysUtils, Types, Classes, TypInfo,
  // LCL
  InterfaceBase, Forms, Buttons, Graphics, GraphType, LCLProc, StdCtrls,
  LCLType, LCLIntf, Controls, ComCtrls, ExtCtrls, LMessages, LResources,
  LazConfigStorage, Menus, Dialogs, Themes, TreeFilterEdit, ObjInspStrConsts,
  PropEdits, GraphPropEdits, ListViewPropEdit, ImageListEditor,
  ComponentTreeView, ComponentEditors, IDEImagesIntf, OIFavoriteProperties,
  PropEditUtils;


const
  OIOptionsFileVersion = 3;

  DefBackgroundColor = clBtnFace;
  DefReferencesColor = clMaroon;
  DefSubPropertiesColor = clGreen;
  DefNameColor = clWindowText;
  DefDefaultValueColor = clWindowText;
  DefValueColor = clMaroon;
  DefReadOnlyColor = clGrayText;
  DefHighlightColor = clHighlight;
  DefHighlightFontColor = clHighlightText;
  DefGutterColor = DefBackgroundColor;
  DefGutterEdgeColor = cl3DShadow;

type
  EObjectInspectorException = class(Exception);

  TplObjectInspector = class;
  TplOICustomPropertyGrid = class;

  // standard ObjectInspector pages

  TplObjectInspectorPage = (
    oipgpProperties,
    oipgpEvents
    );
  TplObjectInspectorPages = set of TplObjectInspectorPage;

  { TplOIOptions }
  TplOIOptions = class
  private
    FComponentTreeHeight: integer;
    FConfigStore: TConfigStorage;
    FDefaultItemHeight: integer;
    FGutterColor: TColor;
    FGutterEdgeColor: TColor;
    FShowComponentTree: boolean;
    FSaveBounds: boolean;
    FLeft: integer;
    FShowGutter: boolean;
    FShowInfoBox: boolean;
    FInfoBoxHeight: integer;
    FShowStatusBar: boolean;
    FTop: integer;
    FWidth: integer;
    FHeight: integer;
    FGridSplitterX: array[TplObjectInspectorPage] of integer;
    FPropertyNameColor: TColor;
    FDefaultValueColor: TColor;
    FSubPropertiesColor: TColor;
    FValueColor: TColor;
    FReadOnlyColor: TColor;
    FReferencesColor: TColor;
    FGridBackgroundColor: TColor;
    FHighlightColor: TColor;
    FHighlightFontColor: TColor;
    FShowHints: boolean;
    FAutoShow: Boolean;
    FBoldNonDefaultValues: Boolean;
    FDrawGridLines: Boolean;
    function FPropertyGridSplitterX(Page: TplObjectInspectorPage): integer;
    procedure FPropertyGridSplitterX(Page: TplObjectInspectorPage; const AValue: integer);
  public
    constructor Create;
    function Load: boolean;
    function Save: boolean;
    procedure Assign(AnObjInspector: TplObjectInspector);
    procedure AssignTo(AnObjInspector: TplObjectInspector); overload;
    procedure AssignTo(AGrid: TplOICustomPropertyGrid); overload;
    property ConfigStore: TConfigStorage read FConfigStore write FConfigStore;
    property SaveBounds:boolean read FSaveBounds write FSaveBounds;
    property Left:integer read FLeft write FLeft;
    property Top:integer read FTop write FTop;
    property Width:integer read FWidth write FWidth;
    property Height:integer read FHeight write FHeight;
    property GridSplitterX[Page: TplObjectInspectorPage]:integer read FPropertyGridSplitterX write FPropertyGridSplitterX;
    property DefaultItemHeight: integer read FDefaultItemHeight write FDefaultItemHeight;
    property ShowComponentTree: boolean read FShowComponentTree write FShowComponentTree;
    property ComponentTreeHeight: integer read FComponentTreeHeight write FComponentTreeHeight;
    property GridBackgroundColor: TColor read FGridBackgroundColor write FGridBackgroundColor;
    property SubPropertiesColor: TColor read FSubPropertiesColor write FSubPropertiesColor;
    property ReferencesColor: TColor read FReferencesColor write FReferencesColor;
    property ValueColor: TColor read FValueColor write FValueColor;
    property ReadOnlyColor: TColor read FReadOnlyColor write FReadOnlyColor;
    property DefaultValueColor: TColor read FDefaultValueColor write FDefaultValueColor;
    property PropertyNameColor: TColor read FPropertyNameColor write FPropertyNameColor;
    property HighlightColor: TColor read FHighlightColor write FHighlightColor;
    property HighlightFontColor: TColor read FHighlightFontColor write FHighlightFontColor;
    property GutterColor: TColor read FGutterColor write FGutterColor;
    property GutterEdgeColor: TColor read FGutterEdgeColor write FGutterEdgeColor;
    property ShowHints: boolean read FShowHints write FShowHints;
    property AutoShow: boolean read FAutoShow write FAutoShow;
    property BoldNonDefaultValues: boolean read FBoldNonDefaultValues write FBoldNonDefaultValues;
    property DrawGridLines: boolean read FDrawGridLines write FDrawGridLines;
    property ShowGutter: boolean read FShowGutter write FShowGutter;
    property ShowStatusBar: boolean read FShowStatusBar write FShowStatusBar;
    property ShowInfoBox: boolean read FShowInfoBox write FShowInfoBox;
    property InfoBoxHeight: integer read FInfoBoxHeight write FInfoBoxHeight;
  end;

  { TplOIPropertyGridRow }
  TplOIPropertyGridRow = class
  private
    FTop:integer;
    FHeight:integer;
    FLvl:integer;
    FName:string;
    FExpanded: boolean;
    FTree:TplOICustomPropertyGrid;
    FChildCount:integer;
    FPriorBrother,
    FFirstChild,
    FLastChild,
    FNextBrother,
    FParent: TplOIPropertyGridRow;
    FEditor: TPropertyEditor;
    FWidgetSets: TLCLPlatforms;
    FIndex:integer;
    LastPaintedValue: string;
    procedure GetLvl;
  public
    constructor Create(PropertyTree: TplOICustomPropertyGrid;  PropEditor:TPropertyEditor;
                       ParentNode:TplOIPropertyGridRow; WidgetSets: TLCLPlatforms);
    destructor Destroy; override;
    function ConsistencyCheck: integer;
    function HasChild(Row: TplOIPropertyGridRow): boolean;
    function GetBottom: integer;
    function IsReadOnly: boolean;
    function IsDisabled: boolean;
    procedure MeasureHeight(ACanvas: TCanvas);
    function Sort(const Compare: TListSortCompare): boolean;// true if changed
    function IsSorted(const Compare: TListSortCompare): boolean;
    function Next: TplOIPropertyGridRow;
    function NextSkipChilds: TplOIPropertyGridRow;
    property Editor:TPropertyEditor read FEditor;
    property Top:integer read FTop write FTop;
    property Height:integer read FHeight write FHeight;
    property Bottom: integer read GetBottom;
    property Lvl:integer read FLvl;
    property Name: string read FName;
    property Expanded:boolean read FExpanded;
    property Tree:TplOICustomPropertyGrid read FTree;
    property Parent:TplOIPropertyGridRow read FParent;
    property ChildCount:integer read FChildCount;
    property FirstChild:TplOIPropertyGridRow read FFirstChild;
    property LastChild:TplOIPropertyGridRow read FLastChild;
    property NextBrother:TplOIPropertyGridRow read FNextBrother;
    property PriorBrother:TplOIPropertyGridRow read FPriorBrother;
    property Index: integer read FIndex;
  end;

  //----------------------------------------------------------------------------
  TplOIPropertyGridState = (
    pgsChangingItemIndex,
    pgsApplyingValue,
    pgsUpdatingEditControl,
    pgsBuildPropertyListNeeded,
    pgsGetComboItemsCalled,
    pgsIdleEnabled
    );
  TplOIPropertyGridStates = set of TplOIPropertyGridState;

  { TplOICustomPropertyGrid }

  TplOICustomPropertyGridColumn = (
    oipgcName,
    oipgcValue
  );

  TOILayout = (
   oilHorizontal,
   oilVertical
  );

  TOIQuickEdit = (
    oiqeEdit,
    oiqeShowValue
  );

  TOIPropertyHint = function(Sender: TObject; PointedRow: TplOIPropertyGridRow;
            ScreenPos: TPoint; aHintWindow: THintWindow;
            out HintWinRect: TRect; out AHint: string
             ): boolean of object;

  TplOICustomPropertyGrid = class(TCustomControl)
  private
    FBackgroundColor: TColor;
    FColumn: TplOICustomPropertyGridColumn;
    FGutterColor: TColor;
    FGutterEdgeColor: TColor;
    FHighlightColor: TColor;
    FLayout: TOILayout;
    FOnOIKeyDown: TKeyEvent;
    FOnPropertyHint: TOIPropertyHint;
    FOnSelectionChange: TNotifyEvent;
    FReferencesColor: TColor;
    FReadOnlyColor: TColor;
    FRowSpacing: integer;
    FShowGutter: Boolean;
    FSubPropertiesColor: TColor;
    FChangeStep: integer;
    FCurrentButton: TControl; // nil or ValueButton
    FCurrentEdit: TWinControl;  // nil or ValueEdit or ValueComboBox or ValueCheckBox
    FCurrentEditorLookupRoot: TPersistent;
    FDefaultItemHeight:integer;
    FDragging: boolean;
    FExpandedProperties: TStringList;// used to restore expanded state when switching selected component(s)
    FExpandingRow: TplOIPropertyGridRow;
    FFavourites: TOIFavoriteProperties;
    FFilter: TTypeKinds;
    FIndent: integer;
    FItemIndex: integer;
    FNameFont, FDefaultValueFont, FValueFont, FHighlightFont: TFont;
    FNewComboBoxItems: TStringList;
    FOnModified: TNotifyEvent;
    FPreferredSplitterX: integer; // best splitter position
    FPropertyEditorHook: TPropertyEditorHook;
    FRows: TFPList;// list of TplOIPropertyGridRow
    FSelection: TPersistentSelectionList;
    FNotificationComponents: TFPList;
    FSplitterX: integer; // current splitter position
    FStates: TplOIPropertyGridStates;
    FTopY: integer;
    FDrawHorzGridLines: Boolean;
    FActiveRowBmp: TCustomBitmap;
    FFirstClickTime: TDateTime;
    // hint stuff
    FHintTimer: TTimer;
    FHintWindow: THintWindow;
    FHintIndex: integer;
    FShowingLongHint: boolean; // last hint was activated by the hinttimer
    ValueEdit: TEdit;
    ValueComboBox: TComboBox;
    ValueCheckBox: TCheckBox;
    ValueButton: TSpeedButton;
    procedure HintTimer(Sender: TObject);
    procedure ResetHintTimer;
    procedure HideHint;
    procedure OnUserInput(Sender: TObject; Msg: Cardinal);
    procedure IncreaseChangeStep;
    function  GridIsUpdating: boolean;
    function  GetRow(Index:integer):TplOIPropertyGridRow;
    function  GetRowCount:integer;
    procedure ClearRows;
    function  GetCurrentEditValue: string;
    procedure SetActiveControl(const AControl: TWinControl);
    procedure SetColumn(const AValue: TplOICustomPropertyGridColumn);
    procedure SetCurrentEditValue(const NewValue: string);
    procedure SetDrawHorzGridLines(const AValue: Boolean);
    procedure SetFavourites(const AValue: TOIFavoriteProperties);
    procedure SetFilter(const AValue: TTypeKinds);
    procedure SetGutterColor(const AValue: TColor);
    procedure SetGutterEdgeColor(const AValue: TColor);
    procedure SetHighlightColor(const AValue: TColor);
    procedure SetItemIndex(NewIndex:integer);
    function  IsCurrentEditorAvailable: Boolean;
    function  GetNameRowHeight: Integer; // temp solution untill TFont.height returns its actual value
    procedure SetItemsTops;
    procedure AlignEditComponents;
    procedure EndDragSplitter;
    procedure SetRowSpacing(const AValue: integer);
    procedure SetShowGutter(const AValue: Boolean);
    procedure SetSplitterX(const NewValue:integer);
    procedure SetTopY(const NewValue:integer);
    function  GetPropNameColor(ARow: TplOIPropertyGridRow):TColor;
    function  GetTreeIconX(Index: integer):integer;
    function  RowRect(ARow: integer):TRect;
    procedure PaintRow(ARow: integer);
    procedure DoPaint(PaintOnlyChangedValues: boolean);
    procedure SetSelection(const ASelection:TPersistentSelectionList);
    procedure SeTplPropertyEditorHook(NewPropertyEditorHook:TPropertyEditorHook);
    procedure UpdateSelectionNotifications;
    procedure AddPropertyEditor(PropEditor: TPropertyEditor);
    procedure AddStringToComboBox(const s: string);
    procedure ExpandRow(Index: integer);
    procedure ShrinkRow(Index: integer);
    procedure AddSubEditor(PropEditor: TPropertyEditor);
    procedure SortSubEditors(ParentRow: TplOIPropertyGridRow);
    function  CanExpandRow(Row: TplOIPropertyGridRow): boolean;
    procedure SetRowValue;
    procedure DoCallEdit(Edit: TOIQuickEdit = oiqeEdit);
    procedure RefreshValueEdit;
    procedure ToggleRow;
    procedure ValueEditDblClick(Sender : TObject);
    procedure ValueControlMouseDown(Sender: TObject; Button:TMouseButton; Shift: TShiftState; X,Y:integer);
    procedure ValueControlMouseMove(Sender: TObject; Shift: TShiftState; X,Y:integer);
    procedure ValueEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ValueEditKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ValueEditExit(Sender: TObject);
    procedure ValueEditChange(Sender: TObject);
    procedure ValueEditMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ValueCheckBoxKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ValueCheckBoxKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ValueCheckBoxExit(Sender: TObject);
    procedure ValueCheckBoxClick(Sender: TObject);
    procedure ValueComboBoxExit(Sender: TObject);
    procedure ValueComboBoxKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ValueComboBoxKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ValueComboBoxMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ValueComboBoxCloseUp(Sender: TObject);
    procedure ValueComboBoxGetItems(Sender: TObject);
    procedure ValueButtonClick(Sender: TObject);
    procedure ValueComboBoxMeasureItem(Control: TWinControl; Index: Integer; var AHeight: Integer);
    procedure ValueComboBoxDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
    procedure OnIdle(Sender: TObject; var Done: Boolean);
    procedure SetIdleEvent(Enable: boolean);
    procedure WMVScroll(var Msg: TLMScroll); message LM_VSCROLL;
    procedure WMMouseWheel(var Message: TLMMouseEvent); message LM_MOUSEWHEEL;
    procedure SetBackgroundColor(const AValue: TColor);
    procedure SetReferences(const AValue: TColor);
    procedure SetSubPropertiesColor(const AValue: TColor);
    procedure SetReadOnlyColor(const AValue: TColor);
    procedure UpdateScrollBar;
    function  FillComboboxItems: boolean; // true if something changed
    function  EditorFilter(const AEditor: TPropertyEditor): Boolean;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure MouseDown(Button:TMouseButton; Shift:TShiftState; X,Y:integer); override;
    procedure MouseMove(Shift:TShiftState; X,Y:integer);  override;
    procedure MouseUp(Button:TMouseButton; Shift:TShiftState; X,Y:integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure HandleStandardKeys(var Key: Word; Shift: TShiftState); virtual;
    procedure HandleKeyUp(var Key: Word; Shift: TShiftState); virtual;
    procedure DoTabKey; virtual;
    procedure DoSetBounds(ALeft, ATop, AWidth, AHeight: integer); override;
    procedure DoSelectionChange;
  public
    constructor Create(TheOwner: TComponent); override;
    constructor CreateWithParams(AnOwner: TComponent;
                                 APropertyEditorHook: TPropertyEditorHook;
                                 TypeFilter: TTypeKinds;
                                 DefItemHeight: integer);
    destructor Destroy;  override;
    function  InitHints: boolean;
    function  CanEditRowValue: boolean;
    procedure SaveChanges;
    function  ConsistencyCheck: integer;
    procedure EraseBackground(DC: HDC); override;
    function  GetActiveRow: TplOIPropertyGridRow;
    function  GetHintTypeAt(RowIndex: integer; X: integer): TPropEditHint;
    function  GetRowByPath(const PropPath: string): TplOIPropertyGridRow;
    function  GridHeight: integer;
    function  MouseToIndex(y: integer; MustExist: boolean):integer;
    function  PropertyPath(Index: integer):string;
    function  PropertyPath(Row: TplOIPropertyGridRow):string;
    function  TopMax: integer;
    procedure BuildPropertyList(OnlyIfNeeded: boolean = false);
    procedure Clear;
    procedure Paint; override;
    procedure PropEditLookupRootChange;
    procedure RefreshPropertyValues;
    procedure ScrollToActiveItem;
    procedure ScrollToItem(NewIndex: Integer);
    procedure SetBounds(aLeft, aTop, aWidth, aHeight: integer); override;
    procedure SetCurrentRowValue(const NewValue: string);
    procedure SetItemIndexAndFocus(NewItemIndex: integer);
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor default DefBackgroundColor;
    property GutterColor: TColor read FGutterColor write SetGutterColor default DefGutterColor;
    property GutterEdgeColor: TColor read FGutterEdgeColor write SetGutterEdgeColor default DefGutterEdgeColor;
    property HighlightColor: TColor read FHighlightColor write SetHighlightColor default DefHighlightColor;
    property ReferencesColor: TColor read FReferencesColor write SetReferences default DefReferencesColor;
    property SubPropertiesColor: TColor read FSubPropertiesColor  write SetSubPropertiesColor default DefSubPropertiesColor;
    property ReadOnlyColor: TColor read FReadOnlyColor write SetReadOnlyColor default DefReadOnlyColor;
    property NameFont: TFont read FNameFont write FNameFont;
    property DefaultValueFont: TFont read FDefaultValueFont write FDefaultValueFont;
    property ValueFont: TFont read FValueFont write FValueFont;
    property HighlightFont: TFont read FHighlightFont write FHighlightFont;
    property BorderStyle default bsSingle;
    property Column: TplOICustomPropertyGridColumn read FColumn write SetColumn;
    property CurrentEditValue: string read GetCurrentEditValue write SetCurrentEditValue;
    property DefaultItemHeight:integer read FDefaultItemHeight write FDefaultItemHeight default 25;
    property DrawHorzGridLines: Boolean read FDrawHorzGridLines write SetDrawHorzGridLines default True;
    property ExpandedProperties: TStringList read FExpandedProperties write FExpandedProperties;
    property Indent: integer read FIndent write FIndent;
    property ItemIndex: integer read FItemIndex write SetItemIndex;
    property Layout: TOILayout read FLayout write FLayout default oilHorizontal;
    property OnModified: TNotifyEvent read FOnModified write FOnModified;
    property OnOIKeyDown: TKeyEvent read FOnOIKeyDown write FOnOIKeyDown;
    property OnSelectionChange: TNotifyEvent read FOnSelectionChange write FOnSelectionChange;
    property OnPropertyHint: TOIPropertyHint read FOnPropertyHint write FOnPropertyHint;
    property PrefferedSplitterX: integer read FPreferredSplitterX write FPreferredSplitterX default 100;
    property PropertyEditorHook: TPropertyEditorHook read FPropertyEditorHook write SeTplPropertyEditorHook;
    property RowCount: integer read GetRowCount;
    property Rows[Index: integer]: TplOIPropertyGridRow read GetRow;
    property RowSpacing: integer read FRowSpacing write SetRowSpacing;
    property Selection: TPersistentSelectionList read FSelection write SetSelection;
    property ShowGutter: Boolean read FShowGutter write SetShowGutter default True;
    property SplitterX: integer read FSplitterX write SetSplitterX default 100;
    property TopY: integer read FTopY write SetTopY default 0;
    property Favorites: TOIFavoriteProperties read FFavourites write SetFavourites;
    property Filter : TTypeKinds read FFilter write SetFilter;
  end;


  { TplOIPropertyGrid }
  TplOIPropertyGrid = class(TplOICustomPropertyGrid)
  published
    property Align;
    property Anchors;
    property BackgroundColor;
    property BorderStyle;
    property Constraints;
    property DefaultItemHeight;
    property DefaultValueFont;
    property Indent;
    property NameFont;
    property OnChangeBounds;
    property OnClick;
    property OnDblClick;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnModified;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnSelectionChange;
    property PopupMenu;
    property PrefferedSplitterX;
    property SplitterX;
    property Tabstop;
    property ValueFont;
    property Visible;
  end;


  { TplCustomPropertiesGrid }
  TplCustomPropertiesGrid = class(TplOICustomPropertyGrid)
  private
    FAutoFreeHook: boolean;
    FSaveOnChangeTIObject: boolean;
    function GetTIObject: TPersistent;
    procedure SetAutoFreeHook(const AValue: boolean);
    procedure SetTIObject(const AValue: TPersistent);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property TIObject: TPersistent read GetTIObject write SetTIObject;
    property AutoFreeHook: boolean read FAutoFreeHook write SetAutoFreeHook;
    property SaveOnChangeTIObject: boolean read FSaveOnChangeTIObject write FSaveOnChangeTIObject default true;
  end;


 //============================================================================
 //============================================================================
 //============================================================================

  { TplObjectInspector }

  TOnAddAvailablePersistent = procedure(APersistent: TPersistent; var Allowed: boolean) of object;

  TOIFlag = ( oifRebuildPropListsNeeded);
  TOIFlags = set of TOIFlag;

  TplObjectInspector = class(TCustomPanel)
    MainPopupMenu: TPopupMenu;
    CopyPopupmenuItem: TMenuItem;
    CutPopupmenuItem: TMenuItem;
    DeletePopupmenuItem: TMenuItem;
    EventGrid: TplOICustomPropertyGrid;
    ComponentRestrictedLabel: TLabel;
    ComponentRestrictedBox: TPaintBox;
    FindDeclarationPopupmenuItem: TMenuItem;
    OptionsSeparatorMenuItem: TMenuItem;
    NoteBook: TPageControl;
    OptionsSeparatorMenuItem2: TMenuItem;
    PastePopupmenuItem: TMenuItem;
    PropertyGrid: TplOICustomPropertyGrid;
    SetDefaultPopupMenuItem: TMenuItem;
    ShowComponentTreePopupMenuItem: TMenuItem;
    ShowHintsPopupMenuItem: TMenuItem;
    ShowOptionsPopupMenuItem: TMenuItem;
    UndoPropertyPopupMenuItem: TMenuItem;

    procedure OnGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure OnGridKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure OnGridDblClick(Sender: TObject);
    procedure OnSetDefaultPopupmenuItemClick(Sender: TObject);
    procedure OnUndoPopupmenuItemClick(Sender: TObject);
    procedure OnFindDeclarationPopupmenuItemClick(Sender: TObject);
    procedure OnCutPopupmenuItemClick(Sender: TObject);
    procedure OnCopyPopupmenuItemClick(Sender: TObject);
    procedure OnPastePopupmenuItemClick(Sender: TObject);
    procedure OnDeletePopupmenuItemClick(Sender: TObject);
    procedure OnShowHintPopupMenuItemClick(Sender: TObject);
    procedure OnShowOptionsPopupMenuItemClick(Sender: TObject);
    procedure OnMainPopupMenuPopup(Sender: TObject);

  private
    FAutoShow: Boolean;
    FOnPropertyHint: TOIPropertyHint;
    FOnSelectionChange: TNotifyEvent;
    FOnFindDeclarationOfProperty: TNotifyEvent;
    FOnOIKeyDown: TKeyEvent;
    FOnRemainingKeyDown: TKeyEvent;
    FOnRemainingKeyUp: TKeyEvent;
    FSelections: TPersistentSelectionList;
    FDefaultItemHeight: integer;
    FFlags: TOIFlags;
    FOnShowOptions: TNotifyEvent;
    FPropertyEditorHook: TPropertyEditorHook;
    FOnAddAvailablePersistent: TOnAddAvailablePersistent;
    FOnSelectPersistentsInOI: TNotifyEvent;
    FOnModified: TNotifyEvent;
    FUpdateLock: integer;
    FComponentEditor: TBaseComponentEditor;
    function  GetGridControl(Page: TplObjectInspectorPage): TplOICustomPropertyGrid;
    procedure SetComponentEditor(const AValue: TBaseComponentEditor);
    procedure SetDefaultItemHeight(const AValue: integer);
    procedure SetOnShowOptions(const AValue: TNotifyEvent);
    procedure SetPropertyEditorHook(NewValue: TPropertyEditorHook);
    procedure SetSelections(const ASelection: TPersistentSelectionList);
    procedure ShowNextPage(Delta: integer);
    procedure DoComponentEditorVerbMenuItemClick(Sender: TObject);
    procedure DoCollectionAddItem(Sender: TObject);
    procedure DoZOrderItemClick(Sender: TObject);
  private
    FInSelection: Boolean;
    FOnAutoShow: TNotifyEvent;
    fSelfDesignHook:TPropertyEditorHook;
    fShowEventsPage:boolean;
  protected
    function  PersistentToString(APersistent: TPersistent): string;
    procedure AddPersistentToList(APersistent: TPersistent; List: TStrings);
    procedure HookLookupRootChange;
    procedure OnGridModified(Sender: TObject);
    procedure OnGridSelectionChange(Sender: TObject);
    function  OnGridPropertyHint(Sender: TObject; PointedRow: TplOIPropertyGridRow;
                                ScreenPos: TPoint; aHintWindow: THintWindow;
                                out HintWinRect: TRect; out AHint: string): boolean;

    procedure HookGetSelection(const ASelection: TPersistentSelectionList);
    procedure HookSetSelection(const ASelection: TPersistentSelectionList);
    procedure DestroyNoteBook;
    procedure CreateNoteBook;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure DoModified(Sender: TObject);
    procedure SetShowEventsPage(val:boolean);
    function  GetSelected: TPersistent;
    procedure SetSelected(val:TPersistent);
    procedure RefreshPropertyValues;
    procedure RebuildPropertyLists;
    procedure HookRefreshPropertyValues;
    procedure ActivateGrid(Grid: TplOICustomPropertyGrid);
    procedure FocusGrid(Grid: TplOICustomPropertyGrid = nil);
    function  GetComponentEditorForSelection: TBaseComponentEditor;
    procedure CreateSelfPropEditorHook;
    procedure DestroySelfPropEditorHook;
    property  ComponentEditor: TBaseComponentEditor read FComponentEditor write SetComponentEditor;
  public
    constructor Create(AnOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddPersistent(APersistent: TPersistent; const mClear:Boolean=true);
    procedure SaveChanges;

    procedure BeginUpdate;
    procedure EndUpdate;
    procedure RefreshSelections;
    function  GetActivePropertyGrid: TplOICustomPropertyGrid;
    function  GetActivePropertyRow: TplOIPropertyGridRow;
    function  GetCurRowDefaultValue(var DefaultStr: string): boolean;
    //..
    property Selected: TPersistent read GetSelected write SetSelected;
    property AutoShow: Boolean read FAutoShow write FAutoShow;
    property DefaultItemHeight: integer read FDefaultItemHeight write SetDefaultItemHeight;
    property Selections: TPersistentSelectionList read FSelections write SetSelections;
    property PropertyEditorHook: TPropertyEditorHook  read FPropertyEditorHook write SetPropertyEditorHook;
    property GridControl[Page: TplObjectInspectorPage]: TplOICustomPropertyGrid  read GetGridControl;
  published
    property ShowEventsPage:boolean read FShowEventsPage write SetShowEventsPage;
    property OnAddAvailPersistent: TOnAddAvailablePersistent read FOnAddAvailablePersistent write FOnAddAvailablePersistent;
    property OnSelectPersistentsInOI: TNotifyEvent read FOnSelectPersistentsInOI write FOnSelectPersistentsInOI;
    property OnModified: TNotifyEvent read FOnModified write FOnModified;
    property OnSelectionChange: TNotifyEvent read FOnSelectionChange write FOnSelectionChange;
    property OnPropertyHint: TOIPropertyHint read FOnPropertyHint write FOnPropertyHint;
    property OnShowOptions: TNotifyEvent read FOnShowOptions write SetOnShowOptions;
    property OnRemainingKeyUp: TKeyEvent read FOnRemainingKeyUp  write FOnRemainingKeyUp;
    property OnRemainingKeyDown: TKeyEvent read FOnRemainingKeyDown write FOnRemainingKeyDown;

    property OnOIKeyDown: TKeyEvent read FOnOIKeyDown write FOnOIKeyDown;
    property OnFindDeclarationOfProperty: TNotifyEvent   read FOnFindDeclarationOfProperty write FOnFindDeclarationOfProperty;
    property OnAutoShow: TNotifyEvent read FOnAutoShow write FOnAutoShow;
    //...
    property Align;
    property Anchors;
    property AutoSize;
    property BorderSpacing;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property BidiMode;
    property BorderWidth;
    property BorderStyle;
    property ChildSizing;
    property ClientHeight;
    property ClientWidth;
    property Color;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentBidiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property TabOrder;
    property TabStop;
    property UseDockManager default True;
    property Visible;

  end;



//-------------------Registretion Procedures -------------------
procedure plRegisterPropertyEditor(PropertyType: PTypeInfo;
                                   PersistentClass: TClass;
                                   const PropertyName: shortstring;
                                   EditorClass: TPropertyEditorClass);

//==============================================================
//==============================================================
implementation

{$R ELDsgxObjectInsp.res}

uses
  math;

const
  DefaultOIPageNames: array[TplObjectInspectorPage] of shortstring = (
    'PropertyPage',
    'EventPage'
    );
  DefaultOIGridNames: array[TplObjectInspectorPage] of shortstring = (
    'PropertyGrid',
    'EventGrid'
    );

DefaultObjectInspectorName: string = 'plObjectInspector';

function SortGridRows(Item1, Item2 : pointer) : integer;
begin
  Result:=SysUtils.CompareText(TplOIPropertyGridRow(Item1).Name,
                               TplOIPropertyGridRow(Item2).Name);
end;

//=======================Registretion Procedures =========================

procedure plRegisterPropertyEditor(PropertyType: PTypeInfo;
                                   PersistentClass: TClass;
                                   const PropertyName: shortstring;
                                   EditorClass: TPropertyEditorClass);
begin
  RegisterPropertyEditor(PropertyType,PersistentClass,PropertyName,EditorClass);
end;

//======================== TplOICustomPropertyGrid =================================

constructor TplOICustomPropertyGrid.CreateWithParams(AnOwner:TComponent;
  APropertyEditorHook:TPropertyEditorHook; TypeFilter:TTypeKinds;
  DefItemHeight: integer);
var
  Details: TThemedElementDetails;
begin
  inherited Create(AnOwner);
  FLayout := oilHorizontal;

  FSelection:=TPersistentSelectionList.Create;
  FNotificationComponents:=TFPList.Create;
  FPropertyEditorHook:=APropertyEditorHook;
  FFilter:=TypeFilter;
  FItemIndex:=-1;
  FStates:=[];
  FColumn := oipgcValue;
  FRows:=TFPList.Create;
  FExpandingRow:=nil;
  FDragging:=false;
  FExpandedProperties:=TStringList.Create;
  FCurrentEdit:=nil;
  FCurrentButton:=nil;

  // visible values
  FTopY:=0;
  FSplitterX:=100;
  FPreferredSplitterX:=FSplitterX;
  Details := ThemeServices.GetElementDetails(ttGlyphOpened);
  FIndent := ThemeServices.GetDetailSize(Details).cx;

  FBackgroundColor:=DefBackgroundColor;
  FReferencesColor:=DefReferencesColor;
  FSubPropertiesColor:=DefSubPropertiesColor;
  FReadOnlyColor:=DefReadOnlyColor;
  FHighlightColor:=DefHighlightColor;
  FGutterColor:=DefGutterColor;
  FGutterEdgeColor:=DefGutterEdgeColor;

  FNameFont:=TFont.Create;
  FNameFont.Color:=DefNameColor;
  FValueFont:=TFont.Create;
  FValueFont.Color:=DefValueColor;
  FDefaultValueFont:=TFont.Create;
  FDefaultValueFont.Color:=DefDefaultValueColor;
  FHighlightFont:=TFont.Create;
  FHighlightFont.Color:=DefHighlightFontColor;

  FDrawHorzGridLines := True;
  FShowGutter := True;

  SetInitialBounds(0,0,200,130);
  ControlStyle:=ControlStyle+[csAcceptsControls,csOpaque];
  BorderWidth:=0;
  BorderStyle := bsSingle;

  // create sub components
  ValueEdit:=TEdit.Create(Self);
  with ValueEdit do
  begin
    Name:='ValueEdit';
    Visible:=false;
    Enabled:=false;
    AutoSize:=false;
    SetBounds(0,-30,80,25); // hidden
    Parent:=Self;
    OnMouseDown := @ValueControlMouseDown;
    OnMouseMove := @ValueControlMouseMove;
    OnDblClick := @ValueEditDblClick;
    OnExit:=@ValueEditExit;
    OnChange:=@ValueEditChange;
    OnKeyDown:=@ValueEditKeyDown;
    OnKeyUp:=@ValueEditKeyUp;
    OnMouseUp:=@ValueEditMouseUp;
  end;

  ValueComboBox:=TComboBox.Create(Self);
  with ValueComboBox do
  begin
    Name:='ValueComboBox';
    Sorted:=true;
    AutoSelect:=true;
    AutoComplete:=true;
    Visible:=false;
    Enabled:=false;
    AutoSize:=false;
    SetBounds(0,-30,Width,Height); // hidden
    DropDownCount:=20;
    Parent:=Self;
    OnMouseDown := @ValueControlMouseDown;
    OnMouseMove := @ValueControlMouseMove;
    OnDblClick := @ValueEditDblClick;
    OnExit:=@ValueComboBoxExit;
    //OnChange:=@ValueComboBoxChange; the on change event is called even,
                                   // if the user is still editing
    OnKeyDown:=@ValueComboBoxKeyDown;
    OnKeyUp:=@ValueComboBoxKeyUp;
    OnMouseUp:=@ValueComboBoxMouseUp;
    OnGetItems:=@ValueComboBoxGetItems;
    OnCloseUp:=@ValueComboBoxCloseUp;
    OnMeasureItem:=@ValueComboBoxMeasureItem;
    OnDrawItem:=@ValueComboBoxDrawItem;
  end;

  ValueCheckBox:=TCheckBox.Create(Self);
  with ValueCheckBox do
  begin
    Name:='ValueCheckBox';
    Visible:=false;
    AutoSize:=false;
    Enabled:=false;
    SetBounds(0,-30,Width,Height); // hidden
    Parent:=Self;
    OnMouseDown := @ValueControlMouseDown;
    OnMouseMove := @ValueControlMouseMove;
    OnExit:=@ValueCheckBoxExit;
    OnKeyDown:=@ValueCheckBoxKeyDown;
    OnKeyUp:=@ValueCheckBoxKeyUp;
  end;

  ValueButton:=TSpeedButton.Create(Self);
  with ValueButton do
  begin
    Name:='ValueButton';
    Visible:=false;
    Enabled:=false;
    Transparent:=false;
    OnClick:=@ValueButtonClick;
    Caption := '...';
    SetBounds(0,-30,Width,Height); // hidden
    Parent:=Self;
  end;

  FActiveRowBmp := CreateBitmapFromResourceName(HInstance,'pl_pg_active_row');

  if DefItemHeight<3 then
    FDefaultItemHeight:=ValueComboBox.Height-3
  else
    FDefaultItemHeight:=DefItemHeight;

  BuildPropertyList;

  Application.AddOnUserInputHandler(@OnUserInput,true);
end;

procedure TplOICustomPropertyGrid.UpdateScrollBar;
var
  ScrollInfo: TScrollInfo;
begin
  if HandleAllocated then begin
    ScrollInfo.cbSize := SizeOf(ScrollInfo);
    ScrollInfo.fMask := SIF_ALL or SIF_DISABLENOSCROLL;
    ScrollInfo.nMin := 0;
    ScrollInfo.nTrackPos := 0;
    ScrollInfo.nMax := TopMax+ClientHeight-1;
    ScrollInfo.nPage := ClientHeight;
    if ScrollInfo.nPage<1 then ScrollInfo.nPage:=1;
    if TopY > ScrollInfo.nMax then TopY:=ScrollInfo.nMax;
    ScrollInfo.nPos := TopY;
    ShowScrollBar(Handle, SB_VERT, True);
    SetScrollInfo(Handle, SB_VERT, ScrollInfo, True);
  end;
end;

function TplOICustomPropertyGrid.FillComboboxItems: boolean;
var
  ExcludeUpdateFlag: boolean;
  CurRow: TplOIPropertyGridRow;
begin
  Result:=false;
  ExcludeUpdateFlag:=not (pgsUpdatingEditControl in FStates);
  Include(FStates,pgsUpdatingEditControl);
  ValueComboBox.Items.BeginUpdate;
  try
    CurRow:=Rows[FItemIndex];
    if FNewComboBoxItems<>nil then FNewComboBoxItems.Clear;
    CurRow.Editor.GetValues(@AddStringToComboBox);
    if FNewComboBoxItems<>nil then begin
      FNewComboBoxItems.Sorted:=paSortList in CurRow.Editor.GetAttributes;
      if ValueComboBox.Items.Equals(FNewComboBoxItems) then exit;
      ValueComboBox.Items.Assign(FNewComboBoxItems);

    end else if ValueComboBox.Items.Count=0 then begin
      exit;
    end else begin
      ValueComboBox.Items.Text:='';
      ValueComboBox.Items.Clear;

    end;
    Result:=true;

  finally
    FreeAndNil(FNewComboBoxItems);
    ValueComboBox.Items.EndUpdate;
    if ExcludeUpdateFlag then
      Exclude(FStates,pgsUpdatingEditControl);
  end;
end;

procedure TplOICustomPropertyGrid.CreateParams(var Params: TCreateParams);
const
  ClassStylesOff = CS_VREDRAW or CS_HREDRAW;
begin
  inherited CreateParams(Params);
  with Params do begin
    {$IFOPT R+}{$DEFINE RangeChecking}{$ENDIF}
    {$R-}
    WindowClass.Style := WindowClass.Style and not ClassStylesOff;
    Style := Style or WS_VSCROLL or WS_CLIPCHILDREN;
    {$IFDEF RangeChecking}{$R+}{$UNDEF RangeChecking}{$ENDIF}   
    ExStyle := ExStyle or WS_EX_CLIENTEDGE;
  end;
end;

procedure TplOICustomPropertyGrid.CreateWnd;
begin
  inherited CreateWnd;
  // handle just created, set scrollbar
  UpdateScrollBar;
end;

procedure TplOICustomPropertyGrid.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  i: LongInt;
begin
  if (Operation=opRemove) and (FNotificationComponents<>nil) then begin
    FNotificationComponents.Remove(AComponent);
    i:=FSelection.IndexOf(AComponent);
    if i>=0 then begin
      FSelection.Delete(i);
      Include(FStates,pgsBuildPropertyListNeeded);
    end;
  end;
  inherited Notification(AComponent, Operation);
end;

procedure TplOICustomPropertyGrid.WMVScroll(var Msg: TLMScroll);
begin
  case Msg.ScrollCode of
      // Scrolls to start / end of the text
    SB_TOP:        TopY := 0;
    SB_BOTTOM:     TopY := TopMax;
      // Scrolls one line up / down
    SB_LINEDOWN:   TopY := TopY + DefaultItemHeight div 2;
    SB_LINEUP:     TopY := TopY - DefaultItemHeight div 2;
      // Scrolls one page of lines up / down
    SB_PAGEDOWN:   TopY := TopY + ClientHeight - DefaultItemHeight;
    SB_PAGEUP:     TopY := TopY - ClientHeight + DefaultItemHeight;
      // Scrolls to the current scroll bar position
    SB_THUMBPOSITION,
    SB_THUMBTRACK: TopY := Msg.Pos;
      // Ends scrolling
    SB_ENDSCROLL:  SetCaptureControl(nil); // release scrollbar capture
  end;
end;

procedure TplOICustomPropertyGrid.WMMouseWheel(var Message: TLMMouseEvent);
begin
  // -1 : scroll by page
  if Mouse.WheelScrollLines=-1
    then TopY := TopY -
              (Message.WheelDelta * (ClientHeight - DefaultItemHeight)) div 120
    // scrolling one line -> scroll half an item, see SB_LINEDOWN and SB_LINEUP
    // handler in WMVScrol
    else TopY := TopY -
              (Message.WheelDelta * Mouse.WheelScrollLines*DefaultItemHeight) div 240;
  Message.Result := 1;
end;

destructor TplOICustomPropertyGrid.Destroy;
var
  a: integer;
begin
  SetIdleEvent(false);
  Application.RemoveOnUserInputHandler(@OnUserInput);
  FItemIndex := -1;
  for a := 0 to FRows.Count - 1 do
    Rows[a].Free;
  FreeAndNil(FRows);
  FreeAndNil(FSelection);
  FreeAndNil(FNotificationComponents);
  FreeAndNil(FValueFont);
  FreeAndNil(FDefaultValueFont);
  FreeAndNil(FNameFont);
  FreeAndNil(FHighlightFont);
  FreeAndNil(FExpandedProperties);
  FreeAndNil(FHintTimer);
  FreeAndNil(FHintWindow);
  FreeAndNil(FNewComboBoxItems);
  FreeAndNil(FActiveRowBmp);
  inherited Destroy;
end;

function TplOICustomPropertyGrid.InitHints: boolean;
begin
  if not ShowHint then exit(false);

  Result := true;
  if FHintTimer = nil then
  begin
    FHintIndex := -1;
    FShowingLongHint := False;
    FHintTimer := TTimer.Create(nil);
    FHintTimer.Interval := 500;
    FHintTimer.Enabled := False;
    FHintTimer.OnTimer := @HintTimer;

    FHintWindow := THintWindow.Create(Self);

    FHIntWindow.Visible := False;
    FHintWindow.Caption := 'This is a hint window'#13#10'Neat huh?';
    FHintWindow.HideInterval := 4000;
    FHintWindow.AutoHide := True;
  end
end;

function TplOICustomPropertyGrid.IsCurrentEditorAvailable: Boolean;
begin
  Result := (FCurrentEdit <> nil) and InRange(FItemIndex, 0, FRows.Count - 1);
end;

function TplOICustomPropertyGrid.ConsistencyCheck: integer;
var
  i: integer;
begin
  for i:=0 to FRows.Count-1 do begin
    if Rows[i]=nil then begin
      Result:=-1;
      exit;
    end;
    if Rows[i].Index<>i then begin
      Result:=-2;
      exit;
    end;
    Result:=Rows[i].ConsistencyCheck;
    if Result<>0 then begin
      dec(Result,100);
      exit;
    end;
  end;
  Result:=0;
end;

procedure TplOICustomPropertyGrid.SetSelection(
  const ASelection: TPersistentSelectionList);
var
  CurRow:TplOIPropertyGridRow;
  OldSelectedRowPath:string;
begin
  if ASelection=nil then exit;
  if (not ASelection.ForceUpdate) and FSelection.IsEqual(ASelection) then exit;

  OldSelectedRowPath:=PropertyPath(ItemIndex);
  ItemIndex:=-1;
  ClearRows;
  FSelection.Assign(ASelection);
  UpdateSelectionNotifications;
  BuildPropertyList;
  CurRow:=GetRowByPath(OldSelectedRowPath);
  if CurRow<>nil then
    ItemIndex:=CurRow.Index;
  Column := oipgcValue;
end;

procedure TplOICustomPropertyGrid.SeTplPropertyEditorHook(
  NewPropertyEditorHook:TPropertyEditorHook);
begin
  if FPropertyEditorHook=NewPropertyEditorHook then exit;
  FPropertyEditorHook:=NewPropertyEditorHook;
  IncreaseChangeStep;
  SetSelection(FSelection);
end;

procedure TplOICustomPropertyGrid.UpdateSelectionNotifications;
var
  i: Integer;
  AComponent: TComponent;
begin
  for i:=0 to FSelection.Count-1 do begin
    if FSelection[i] is TComponent then begin
      AComponent:=TComponent(FSelection[i]);
      if FNotificationComponents.IndexOf(AComponent)<0 then begin
        FNotificationComponents.Add(AComponent);
        AComponent.FreeNotification(Self);
      end;
    end;
  end;
  for i:=FNotificationComponents.Count-1 downto 0 do begin
    AComponent:=TComponent(FNotificationComponents[i]);
    if FSelection.IndexOf(AComponent)<0 then begin
      FNotificationComponents.Delete(i);
      AComponent.RemoveFreeNotification(Self);
    end;
  end;
 end;

function TplOICustomPropertyGrid.PropertyPath(Index:integer):string;
begin
  if (Index>=0) and (Index<FRows.Count) then begin
    Result:=PropertyPath(Rows[Index]);
  end else
    Result:='';
end;

function TplOICustomPropertyGrid.PropertyPath(Row: TplOIPropertyGridRow): string;
begin
  if Row=nil then begin
    Result:='';
    exit;
  end;
  Result:=Row.Name;
  Row:=Row.Parent;
  while Row<>nil do begin
    Result:=Row.Name+'.'+Result;
    Row:=Row.Parent;
  end;
end;

function TplOICustomPropertyGrid.GetRowByPath(
  const PropPath: string): TplOIPropertyGridRow;
// searches PropPath. Expands automatically parent rows
var CurName:string;
  s,e:integer;
  CurParentRow:TplOIPropertyGridRow;
begin
  Result:=nil;
  if FRows.Count=0 then exit;
  CurParentRow:=nil;
  s:=1;
  while (s<=length(PropPath)) do begin
    e:=s;
    while (e<=length(PropPath)) and (PropPath[e]<>'.') do inc(e);
    CurName:=uppercase(copy(PropPath,s,e-s));
    s:=e+1;
    // search name in childs
    if CurParentRow=nil then
      Result:=Rows[0]
    else
      Result:=CurParentRow.FirstChild;
    while (Result<>nil) and (uppercase(Result.Name)<>CurName) do
      Result:=Result.NextBrother;
    if Result=nil then begin
      exit;
    end else begin
      // expand row
      CurParentRow:=Result;
      ExpandRow(CurParentRow.Index);
    end;
  end;
  if s<=length(PropPath) then Result:=nil;
end;

procedure TplOICustomPropertyGrid.SetRowValue;
var
  CurRow: TplOIPropertyGridRow;
  NewValue: string;
  OldExpanded: boolean;
  OldChangeStep: integer;
begin
   if not CanEditRowValue or Rows[FItemIndex].IsReadOnly then exit;

  NewValue:=GetCurrentEditValue;

  CurRow:=Rows[FItemIndex];
  if length(NewValue)>CurRow.Editor.GetEditLimit then
    NewValue:=LeftStr(NewValue,CurRow.Editor.GetEditLimit);

  if CurRow.Editor.GetVisualValue=NewValue then exit;

  OldChangeStep:=fChangeStep;
  Include(FStates,pgsApplyingValue);
  try
    {$IFNDEF DoNotCatchOIExceptions}
    try
    {$ENDIF}
      CurRow.Editor.SetValue(NewValue);
    {$IFNDEF DoNotCatchOIExceptions}
    except
      on E: Exception do begin
        MessageDlg(oisError, E.Message, mtError, [mbOk], 0);
      end;
    end;
    {$ENDIF}
    if (OldChangeStep<>FChangeStep) then begin
      // the selection has changed
      // => CurRow does not exist any more
      exit;
    end;

    // set value in edit control
    SetCurrentEditValue(CurRow.Editor.GetVisualValue);

    // update volatile sub properties
    if (paVolatileSubProperties in CurRow.Editor.GetAttributes)
    and ((CurRow.Expanded) or (CurRow.ChildCount>0)) then begin
      OldExpanded:=CurRow.Expanded;
      ShrinkRow(FItemIndex);
      if OldExpanded then
        ExpandRow(FItemIndex);
    end;
   finally
    Exclude(FStates,pgsApplyingValue);
  end;
  if FPropertyEditorHook=nil then
    DoPaint(true)
  else
    FPropertyEditorHook.RefreshPropertyValues;
  if Assigned(FOnModified) then FOnModified(Self);
end;

procedure TplOICustomPropertyGrid.DoCallEdit(Edit: TOIQuickEdit);
var
  CurRow:TplOIPropertyGridRow;
  OldChangeStep: integer;
begin

  if not CanEditRowValue then exit;

  OldChangeStep:=fChangeStep;
  CurRow:=Rows[FItemIndex];
  if paDialog in CurRow.Editor.GetAttributes then begin
    {$IFNDEF DoNotCatchOIExceptions}
    try
    {$ENDIF}

      Include(FStates,pgsApplyingValue);
      try
        if Edit=oiqeShowValue then
          CurRow.Editor.ShowValue
        else
          CurRow.Editor.Edit;
      finally
        Exclude(FStates,pgsApplyingValue);
      end;
    {$IFNDEF DoNotCatchOIExceptions}
    except
      on E: Exception do begin
        MessageDlg(oisError, E.Message, mtError, [mbOk], 0);
      end;
    end;
    {$ENDIF}

    if (OldChangeStep<>FChangeStep) then begin
      // the selection has changed
      // => CurRow does not exist any more
      RefreshPropertyValues;
      exit;
    end;

    // update value
    RefreshValueEdit;

    //invalidate changed subproperties
    DoPaint(True);
  end;
end;

procedure TplOICustomPropertyGrid.RefreshValueEdit;
var
  CurRow: TplOIPropertyGridRow;
  NewValue: string;
begin
  if not GridIsUpdating and IsCurrentEditorAvailable then begin
    CurRow:=Rows[FItemIndex];
    NewValue:=CurRow.Editor.GetVisualValue;
    SetCurrentEditValue(NewValue);
  end;
end;

procedure TplOICustomPropertyGrid.ValueEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  ScrollToActiveItem;
  HandleStandardKeys(Key,Shift);
end;

procedure TplOICustomPropertyGrid.ValueEditKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  HandleKeyUp(Key,Shift);
end;

procedure TplOICustomPropertyGrid.ValueEditExit(Sender: TObject);
begin
  SetRowValue;
end;

procedure TplOICustomPropertyGrid.ValueEditChange(Sender: TObject);
var CurRow: TplOIPropertyGridRow;
begin
  if (pgsUpdatingEditControl in FStates) or not IsCurrentEditorAvailable then exit;
  CurRow:=Rows[FItemIndex];
  if paAutoUpdate in CurRow.Editor.GetAttributes then
    SetRowValue;
end;

procedure TplOICustomPropertyGrid.ValueEditMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button=mbLeft) and (Shift=[ssCtrl,ssLeft]) then
    DoCallEdit(oiqeShowValue);
end;

procedure TplOICustomPropertyGrid.ValueCheckBoxKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  ScrollToActiveItem;
  HandleStandardKeys(Key,Shift);
end;

procedure TplOICustomPropertyGrid.ValueCheckBoxKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  HandleKeyUp(Key,Shift);
end;

procedure TplOICustomPropertyGrid.ValueCheckBoxExit(Sender: TObject);
begin
  SetRowValue;
end;

procedure TplOICustomPropertyGrid.ValueCheckBoxClick(Sender: TObject);
var
  CurRow: TplOIPropertyGridRow;
begin
  if (pgsUpdatingEditControl in FStates) or not IsCurrentEditorAvailable then exit;
  CurRow:=Rows[FItemIndex];
  if paAutoUpdate in CurRow.Editor.GetAttributes then
    SetRowValue;
end;

procedure TplOICustomPropertyGrid.ValueComboBoxExit(Sender: TObject);
begin
  if pgsUpdatingEditControl in FStates then exit;
  SetRowValue;
end;

procedure TplOICustomPropertyGrid.ValueComboBoxKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  ScrollToActiveItem;
  HandleStandardKeys(Key,Shift);
end;

procedure TplOICustomPropertyGrid.ValueComboBoxKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  HandleKeyUp(Key,Shift);
end;

procedure TplOICustomPropertyGrid.ValueComboBoxMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button=mbLeft) then begin
    if (Shift=[ssCtrl,ssLeft]) then
      DoCallEdit(oiqeShowValue)
    else if (FFirstClickTime<>0) and (Now-FFirstClickTime<(1/86400*0.4)) then
      ValueEditDblClick(Sender);
  end;
end;

procedure TplOICustomPropertyGrid.ValueButtonClick(Sender: TObject);
begin
  ScrollToActiveItem;
  DoCallEdit;
end;

procedure TplOICustomPropertyGrid.ValueComboBoxMeasureItem(Control: TWinControl;
  Index: Integer; var AHeight: Integer);
var
  CurRow: TplOIPropertyGridRow;
begin
  if (FItemIndex >= 0) and (FItemIndex < FRows.Count) then
  begin
    CurRow := Rows[FItemIndex];
    CurRow.Editor.ListMeasureHeight('Fj', Index, ValueComboBox.Canvas, AHeight);
    AHeight := Max(AHeight, ValueComboBox.ItemHeight);
  end;
end;

procedure TplOICustomPropertyGrid.SetItemIndex(NewIndex:integer);
var
  NewRow: TplOIPropertyGridRow;
  NewValue: string;
  EditorAttributes: TPropertyAttributes;
begin
  if GridIsUpdating or (FItemIndex = NewIndex) then
    exit;

  // save old edit value
  SetRowValue;

  Include(FStates, pgsChangingItemIndex);
  if (FItemIndex >= 0) and (FItemIndex < FRows.Count) then
    Rows[FItemIndex].Editor.Deactivate;
  if CanFocus then
    SetCaptureControl(nil);

  FItemIndex := NewIndex;
  if FCurrentEdit <> nil then
  begin
    FCurrentEdit.Visible:=false;
    FCurrentEdit.Enabled:=false;
    FCurrentEdit:=nil;
  end;
  if FCurrentButton<>nil then
  begin
    FCurrentButton.Visible:=false;
    FCurrentButton.Enabled:=false;
    FCurrentButton:=nil;
  end;
  FCurrentEditorLookupRoot:=nil;
  if (NewIndex>=0) and (NewIndex<FRows.Count) then
  begin
    NewRow:=Rows[NewIndex];

    ScrollToItem(NewIndex);

    if CanFocus then
      NewRow.Editor.Activate;
    EditorAttributes:=NewRow.Editor.GetAttributes;
    if paDialog in EditorAttributes then begin
      FCurrentButton:=ValueButton;
      FCurrentButton.Visible:=true;

    end;
    NewValue:=NewRow.Editor.GetVisualValue;
    {$IFDEF UseOICheckBox}
    if (NewRow.Editor is TBoolPropertyEditor) then begin
      FCurrentEdit:=ValueCheckBox;
      ValueCheckBox.Enabled:=not NewRow.IsReadOnly;
      ValueCheckBox.Caption:=NewValue;
      ValueCheckBox.Checked:=(NewValue='True');
    end else
    {$ENDIF}
    if paValueList in EditorAttributes then begin
      FCurrentEdit:=ValueComboBox;
      if paPickList in EditorAttributes then begin
        // text field should be readonly
        if paCustomDrawn in EditorAttributes then
          ValueComboBox.Style:=csOwnerDrawVariable
        else
          ValueComboBox.Style:=csDropDownList;
      end else begin
        if paCustomDrawn in EditorAttributes then
          ValueComboBox.Style:=csOwnerDrawVariable
        else
          ValueComboBox.Style:=csDropDown;
      end;
      ValueComboBox.MaxLength:=NewRow.Editor.GetEditLimit;
      ValueComboBox.Sorted:=paSortList in NewRow.Editor.GetAttributes;
      ValueComboBox.Enabled:=not NewRow.IsReadOnly;
      // Do not fill the items here, because it can be very slow.
      // Just fill in some values and update the values, before the combobox
      // popups
      ValueComboBox.Items.Text:=NewValue;
      Exclude(FStates,pgsGetComboItemsCalled);
      SetIdleEvent(true);
      ValueComboBox.Text:=NewValue;
    end else begin
      FCurrentEdit:=ValueEdit;
      ValueEdit.ReadOnly:=NewRow.IsReadOnly;
      ValueEdit.Enabled:=true;
      ValueEdit.MaxLength:=NewRow.Editor.GetEditLimit;
      ValueEdit.Text:=NewValue;
    end;
    AlignEditComponents;
    if FCurrentEdit<>nil then begin
      if FPropertyEditorHook<>nil then
        FCurrentEditorLookupRoot:=FPropertyEditorHook.LookupRoot;
      FCurrentEdit.Visible:=true;
      if (FDragging=false) and (FCurrentEdit.Showing)
      and FCurrentEdit.Enabled
      and (not NewRow.IsReadOnly)
      and CanFocus then begin
        if (Column=oipgcValue) then
          SetActiveControl(FCurrentEdit);
      end;
    end;
    if FCurrentButton<>nil then
      FCurrentButton.Enabled:=not NewRow.IsDisabled;
  end;

  Exclude(FStates, pgsChangingItemIndex);
  DoSelectionChange;
  Invalidate;
end;

function TplOICustomPropertyGrid.GetNameRowHeight: Integer;
begin
  Result := Abs(FNameFont.Height);
  if Result = 0
  then Result := 16;
  Inc(Result, 2); // margin
end;

function TplOICustomPropertyGrid.GetRowCount:integer;
begin
  Result:=FRows.Count;
end;

procedure TplOICustomPropertyGrid.BuildPropertyList(OnlyIfNeeded: boolean);
var
  a: integer;
  CurRow: TplOIPropertyGridRow;
  OldSelectedRowPath: string;
begin
  if OnlyIfNeeded and (not (pgsBuildPropertyListNeeded in FStates)) then exit;
  Exclude(FStates,pgsBuildPropertyListNeeded);

  OldSelectedRowPath:=PropertyPath(ItemIndex);
  // unselect
  ItemIndex:=-1;
  // clear
  for a:=0 to FRows.Count-1 do Rows[a].Free;
  FRows.Clear;
  // get properties
  GetPersistentProperties(
    FSelection, FFilter + [tkClass], FPropertyEditorHook, @AddPropertyEditor,
    @EditorFilter);
  // sort
  FRows.Sort(@SortGridRows);
  for a:=0 to FRows.Count-1 do begin
    if a>0 then
      Rows[a].FPriorBrother:=Rows[a-1]
    else
      Rows[a].FPriorBrother:=nil;
    if a<FRows.Count-1 then
      Rows[a].FNextBrother:=Rows[a+1]
    else
      Rows[a].FNextBrother:=nil;
  end;
  // set indices and tops
  SetItemsTops;
  // restore expands
  for a:=FExpandedProperties.Count-1 downto 0 do begin
    CurRow:=GetRowByPath(FExpandedProperties[a]);
    if CurRow<>nil then
      ExpandRow(CurRow.Index);
  end;
  // update scrollbar
  FTopY:=0;
  UpdateScrollBar;
  // reselect
  CurRow:=GetRowByPath(OldSelectedRowPath);
  if CurRow<>nil then begin
    ItemIndex:=CurRow.Index;
  end;
  // paint
  Invalidate;
end;

procedure TplOICustomPropertyGrid.AddPropertyEditor(PropEditor: TPropertyEditor);
var
  NewRow: TplOIPropertyGridRow;
  WidgetSets: TLCLPlatforms;
begin
  WidgetSets := [];
  if Favorites<>nil then begin

    if Favorites is TOIRestrictedProperties then
    begin
      WidgetSets := (Favorites as TOIRestrictedProperties).AreRestricted(
                                                  Selection,PropEditor.GetName);
      if WidgetSets = [] then
      begin
        PropEditor.Free;
        Exit;
      end;
    end
    else
      if not Favorites.AreFavorites(Selection,PropEditor.GetName) then begin
        PropEditor.Free;
        exit;
      end;
  end;
  if PropEditor is TClassPropertyEditor then
    (PropEditor as TClassPropertyEditor).SubPropsTypeFilter := FFilter;
  NewRow := TplOIPropertyGridRow.Create(Self, PropEditor, nil, WidgetSets);
  FRows.Add(NewRow);
  if FRows.Count>1 then begin
    NewRow.FPriorBrother:=Rows[FRows.Count-2];
    NewRow.FPriorBrother.FNextBrother:=NewRow;
  end;
end;

procedure TplOICustomPropertyGrid.AddStringToComboBox(const s: string);
begin
  if FNewComboBoxItems=nil then
    FNewComboBoxItems:=TStringList.Create;
  FNewComboBoxItems.Add(s);
end;

procedure TplOICustomPropertyGrid.ExpandRow(Index:integer);
var
  a: integer;
  CurPath: string;
  AlreadyInExpandList: boolean;
  ActiveRow: TplOIPropertyGridRow;
begin
  // Save ItemIndex
  if ItemIndex <> -1 then
    ActiveRow := Rows[ItemIndex]
  else
    ActiveRow := nil;
  FExpandingRow := Rows[Index];
  if (FExpandingRow.Expanded) or (not CanExpandRow(FExpandingRow)) then
  begin
    FExpandingRow := nil;
    Exit;
  end;
  FExpandingRow.Editor.GetProperties(@AddSubEditor);
  SortSubEditors(FExpandingRow);
  SetItemsTops;
  FExpandingRow.FExpanded := True;
  a := 0;
  CurPath:=uppercase(PropertyPath(FExpandingRow.Index));
  AlreadyInExpandList:=false;
  while a < FExpandedProperties.Count do
  begin
    if FExpandedProperties[a]=copy(CurPath,1,length(FExpandedProperties[a])) then
    begin
      if Length(FExpandedProperties[a]) = Length(CurPath) then
      begin
        AlreadyInExpandList := True;
        inc(a);
      end
      else
        FExpandedProperties.Delete(a);
    end
    else
      inc(a);
  end;
  if not AlreadyInExpandList then
    FExpandedProperties.Add(CurPath);
  FExpandingRow := nil;
  // restore ItemIndex
  if ActiveRow <> nil then
    FItemIndex := ActiveRow.Index
  else
    FItemIndex := -1;
  UpdateScrollBar;
  Invalidate;
end;

procedure TplOICustomPropertyGrid.ShrinkRow(Index:integer);
var
  CurRow, ARow: TplOIPropertyGridRow;
  StartIndex, EndIndex, a: integer;
  CurPath: string;
begin
  CurRow := Rows[Index];
  if (not CurRow.Expanded) then
    Exit;
  // calculate all childs (between StartIndex..EndIndex)
  StartIndex := CurRow.Index + 1;
  EndIndex := FRows.Count - 1;
  ARow := CurRow;
  while ARow <> nil do
  begin
    if ARow.NextBrother <> nil then
    begin
      EndIndex := ARow.NextBrother.Index - 1;
      break;
    end;
    ARow := ARow.Parent;
  end;
  if (FItemIndex >= StartIndex) and (FItemIndex <= EndIndex) then
    // current row delete, set new current row
    ItemIndex:=0
  else
  if FItemIndex > EndIndex then
    // adjust current index for deleted rows
    FItemIndex := FItemIndex - (EndIndex - StartIndex + 1);
  for a := EndIndex downto StartIndex do
  begin
    Rows[a].Free;
    FRows.Delete(a);
  end;
  SetItemsTops;
  CurRow.FExpanded := False;
  CurPath := UpperCase(PropertyPath(CurRow.Index));
  a := 0;
  while a < FExpandedProperties.Count do
  begin
    if copy(FExpandedProperties[a], 1, length(CurPath)) = CurPath then
      FExpandedProperties.Delete(a)
    else
      inc(a);
  end;
  if CurRow.Parent <> nil then
    FExpandedProperties.Add(PropertyPath(CurRow.Parent.Index));
  UpdateScrollBar;
  Invalidate;
end;

procedure TplOICustomPropertyGrid.AddSubEditor(PropEditor:TPropertyEditor);
var NewRow:TplOIPropertyGridRow;
  NewIndex:integer;
begin
  if PropEditor is TClassPropertyEditor then
    (PropEditor as TClassPropertyEditor).SubPropsTypeFilter := FFilter;
  NewRow:=TplOIPropertyGridRow.Create(Self,PropEditor,FExpandingRow, []);
  NewIndex:=FExpandingRow.Index+1+FExpandingRow.ChildCount;
  NewRow.FIndex:=NewIndex;
  FRows.Insert(NewIndex,NewRow);
  if NewIndex<FItemIndex
    then inc(FItemIndex);
  if FExpandingRow.FFirstChild=nil then
    FExpandingRow.FFirstChild:=NewRow;
  NewRow.FPriorBrother:=FExpandingRow.FLastChild;
  FExpandingRow.FLastChild:=NewRow;
  if NewRow.FPriorBrother<>nil then
    NewRow.FPriorBrother.FNextBrother:=NewRow;
  inc(FExpandingRow.FChildCount);
end;

procedure TplOICustomPropertyGrid.SortSubEditors(ParentRow: TplOIPropertyGridRow);
var
  Item: TplOIPropertyGridRow;
  Index: Integer;
  Next: TplOIPropertyGridRow;
begin
  if not ParentRow.Sort(@SortGridRows) then exit;
  // update FRows
  Item:=ParentRow.FirstChild;
  Index:=ParentRow.Index+1;
  Next:=ParentRow.NextSkipChilds;
  while (Item<>nil) and (Item<>Next) do begin
    FRows[Index]:=Item;
    Item.FIndex:=Index;
    Item:=Item.Next;
    inc(Index);
  end;
end;

function TplOICustomPropertyGrid.CanExpandRow(Row: TplOIPropertyGridRow): boolean;
var
  AnObject: TPersistent;
  ParentRow: TplOIPropertyGridRow;
begin
  Result:=false;
  if (Row=nil) or (Row.Editor=nil) then exit;
  if (not (paSubProperties in Row.Editor.GetAttributes)) then exit;
  // check if circling
  if (Row.Editor is TPersistenTPropertyEditor) then begin
    AnObject:=TPersistent(Row.Editor.GetObjectValue);
    if FSelection.IndexOf(AnObject)>=0 then exit;
    ParentRow:=Row.Parent;
    while ParentRow<>nil do begin
      if (ParentRow.Editor is TPersistenTPropertyEditor)
      and (ParentRow.Editor.GetObjectValue=AnObject) then
        exit;
      ParentRow:=ParentRow.Parent;
    end;
  end;
  Result:=true;
end;

function TplOICustomPropertyGrid.MouseToIndex(y:integer;MustExist:boolean):integer;
var l,r,m:integer;
begin
  l:=0;
  r:=FRows.Count-1;
  inc(y,FTopY);
  while (l<=r) do begin
    m:=(l+r) shr 1;
    if Rows[m].Top>y then begin
      r:=m-1;
    end else if Rows[m].Bottom<=y then begin
      l:=m+1;
    end else begin
      Result:=m;  exit;
    end;
  end;
  if (MustExist=false) and (FRows.Count>0) then begin
    if y<0 then Result:=0
    else Result:=FRows.Count-1;
  end else Result:=-1;
end;

function TplOICustomPropertyGrid.GetActiveRow: TplOIPropertyGridRow;
begin
  if InRange(ItemIndex,0,FRows.Count-1) then
    Result:=Rows[ItemIndex]
  else
    Result:=nil;
end;

procedure TplOICustomPropertyGrid.SetCurrentRowValue(const NewValue: string);
begin
  if not CanEditRowValue or Rows[FItemIndex].IsReadOnly then exit;
  // SetRowValue reads the value from the current edit control and writes it
  // to the property editor
  // -> set the text in the current edit control without changing FLastEditValue
  SetCurrentEditValue(NewValue);
  SetRowValue;
end;

procedure TplOICustomPropertyGrid.SetItemIndexAndFocus(NewItemIndex: integer);
begin
  if not InRange(NewItemIndex, 0, FRows.Count - 1) then exit;
  ItemIndex:=NewItemIndex;
  if FCurrentEdit<>nil then
  begin
    SetActiveControl(FCurrentEdit);
    if (FCurrentEdit is TCustomEdit) then
      TCustomEdit(FCurrentEdit).SelectAll;
  end;
end;

function TplOICustomPropertyGrid.CanEditRowValue: boolean;
begin
  Result:=
    not GridIsUpdating and IsCurrentEditorAvailable
    and ((FCurrentEditorLookupRoot = nil)
      or (FPropertyEditorHook = nil)
      or (FPropertyEditorHook.LookupRoot = FCurrentEditorLookupRoot));
end;

procedure TplOICustomPropertyGrid.SaveChanges;
begin
  SetRowValue;
end;

function TplOICustomPropertyGrid.GetHintTypeAt(RowIndex: integer; X: integer): TPropEditHint;
var
  IconX: integer;
begin
  Result := pehNone;
  if (RowIndex < 0) or (RowIndex >= RowCount) then
    Exit;
  if SplitterX <= X then
  begin
    if (FCurrentButton <> nil) and (FCurrentButton.Left <= X) then
      Result := pehEditButton
    else
      Result := pehValue;
  end else
  begin
    IconX := GetTreeIconX(RowIndex);
    if IconX + Indent > X then
      Result := pehTree
    else
      Result := pehName;
  end;
end;

procedure TplOICustomPropertyGrid.MouseDown(Button:TMouseButton;  Shift:TShiftState;
  X,Y:integer);
var
  IconX,Index:integer;
  PointedRow:TplOIPropertyGridRow;
begin

  inherited MouseDown(Button,Shift,X,Y);

  HideHint;

  if Button=mbLeft then begin
    FFirstClickTime:=Now;
    if Cursor=crHSplit then begin
      FDragging:=true;
    end
    else
    begin
      Index:=MouseToIndex(Y,false);
      if (Index>=0) and (Index<FRows.Count) then
      begin
        PointedRow:=Rows[Index];
        if CanExpandRow(PointedRow) then
        begin
          IconX:=GetTreeIconX(Index);
          if ((X>=IconX) and (X<=IconX+FIndent)) or (ssDouble in Shift) then
          begin
            if PointedRow.Expanded then
              ShrinkRow(Index)
            else
              ExpandRow(Index);
          end;
        end;

        SetItemIndexAndFocus(Index);
        SetCaptureControl(Self);
        Column := oipgcValue;
      end;
    end;
  end;
end;

procedure TplOICustomPropertyGrid.MouseMove(Shift:TShiftState;  X,Y:integer);
var
  SplitDistance:integer;
  Index: Integer;
  fPropRow: TplOIPropertyGridRow;
  fHint: String;
  fpoint: TPoint;
  fHintRect: TRect;
begin
  inherited MouseMove(Shift,X,Y);
  SplitDistance:=X-SplitterX;
  if FDragging then begin
    if ssLeft in Shift then begin
      SplitterX:=SplitterX+SplitDistance;
    end else begin
      EndDragSplitter;
    end;
  end
  else
  begin
    if (abs(SplitDistance)<=2) then begin
      Cursor:=crHSplit;
    end else begin
      Cursor:=crDefault;
    end;

    if ssLeft in Shift then
    begin
      Index := MouseToIndex(Y, False);
      SetItemIndexAndFocus(Index);
      SetCaptureControl(Self);
    end;

    // to check if the property text fits in its box, if not show a hint
    if ShowHint then
    begin
      Index := MouseToIndex(y,false);
      if (Index > -1)
      and (not FShowingLongHint)
      and ((FHintWindow=nil) or (not FHintWindow.Visible)
           or (Index<>FHintIndex))
      then begin
        FHintIndex:=Index;
        FShowingLongHint:=false;
        fPropRow := GetRow(Index);
        if X < SplitterX then
        begin
          // Mouse is over property name...
          fHint := fPropRow.Name;
          if InitHints and ((Canvas.TextWidth(fHint) + BorderWidth + GetTreeIconX(Index) + Indent) >= SplitterX) then
          begin
            fHintRect := FHintWindow.CalcHintRect(0,fHint,nil);
            fPoint := ClientToScreen(
                                   Point(BorderWidth+GetTreeIconX(Index)+Indent,
                                   fPropRow.Top - TopY-1));
            MoveRect(fHintRect,fPoint.x,fPoint.y);
            FHintWindow.ActivateHint(fHintRect,fHint);
          end;
        end
        else
        begin
          // Mouse is over property value...
          fHint := fPropRow.LastPaintedValue;
          if length(fHint) > 100 then fHint := copy(fHint, 1, 100) + '...';
          if (Canvas.TextWidth(fHint) > (ClientWidth - BorderWidth - SplitterX)) and
             InitHints then
          begin
            fHintRect := FHintWindow.CalcHintRect(0,fHint,nil);
            fpoint := ClientToScreen(Point(SplitterX, fPropRow.Top - TopY - 1));
            MoveRect(fHintRect, fPoint.x, fPoint.y);
            FHintWindow.ActivateHint(fHintRect, fHint);
          end;
        end;
      end;
    end;
  end;
end;

procedure TplOICustomPropertyGrid.MouseUp(Button:TMouseButton;  Shift:TShiftState;
  X,Y:integer);
begin
  if FDragging then EndDragSplitter;
  SetCaptureControl(nil);
  inherited MouseUp(Button,Shift,X,Y);
end;

procedure TplOICustomPropertyGrid.KeyDown(var Key: Word; Shift: TShiftState);
begin
  HandleStandardKeys(Key,Shift);
  inherited KeyDown(Key, Shift);
end;

procedure TplOICustomPropertyGrid.HandleStandardKeys(
  var Key: Word; Shift: TShiftState);

var
  Handled: Boolean;

  procedure FindPropertyByFirstLetter;
  var
    i: Integer;
  begin
    if Column = oipgcName then
      for i := 0 to RowCount - 1 do
        if (Rows[i].Lvl = Rows[ItemIndex].Lvl) and
          (Ord(upCase(Rows[i].Name[1])) = Key) then
        begin
          SetItemIndexAndFocus(i);
          exit;
        end;
    Handled := false;
  end;

  procedure HandleUnshifted;
  const
    Page = 20;
  begin
    Handled := true;
    case Key of
      VK_UP   : SetItemIndexAndFocus(ItemIndex - 1);
      VK_DOWN : SetItemIndexAndFocus(ItemIndex + 1);
      VK_PRIOR: SetItemIndexAndFocus(Max(ItemIndex - Page, 0));
      VK_NEXT : SetItemIndexAndFocus(Min(ItemIndex + Page, FRows.Count - 1));

      VK_TAB: DoTabKey;

      VK_RETURN:
        begin
          SetRowValue;
          if FCurrentEdit is TCustomEdit then
            TCustomEdit(FCurrentEdit).SelectAll;
        end;

      VK_ESCAPE: RefreshValueEdit;

      Ord('A')..Ord('Z'): FindPropertyByFirstLetter;

      else
        Handled := false;
    end;
  end;

begin

  Handled := false;
  if (Shift = []) or (Shift = [ssShift]) then
  begin
    if not (FCurrentEdit is TCustomCombobox) or
       not TCustomCombobox(FCurrentEdit).DroppedDown then
      HandleUnshifted;
  end
  else
  if Shift = [ssCtrl] then
  begin
    case Key of
      VK_RETURN:
        begin
          ToggleRow;
          Handled := true;
        end;
    end;
  end
  else
  if Shift = [ssAlt] then
    case Key of
      VK_LEFT:
        begin
          Handled := (ItemIndex >= 0) and Rows[ItemIndex].Expanded;
          if Handled then ShrinkRow(ItemIndex);
        end;

      VK_RIGHT:
        begin
          Handled := (ItemIndex >= 0) and not Rows[ItemIndex].Expanded and
            CanExpandRow(Rows[ItemIndex]);
          if Handled then ExpandRow(ItemIndex)
        end;
    end;


  if not Handled and Assigned(OnOIKeyDown) then
  begin
    OnOIKeyDown(Self, Key, Shift);
    Handled := Key = VK_UNKNOWN;
  end;

  if Handled then
    Key := VK_UNKNOWN;
end;

procedure TplOICustomPropertyGrid.HandleKeyUp(var Key: Word; Shift: TShiftState);
begin
  if (Key<>VK_UNKNOWN) and Assigned(OnKeyUp) then OnKeyUp(Self,Key,Shift);
end;

procedure TplOICustomPropertyGrid.DoTabKey;
begin
  if Column = oipgcValue then
  begin
    Column := oipgcName;
    Self.SetFocus;
  end else
  begin
    Column := oipgcValue;
    if FCurrentEdit <> nil then
      FCurrentEdit.SetFocus;
  end;
end;

function TplOICustomPropertyGrid.EditorFilter(
  const AEditor: TPropertyEditor): Boolean;
begin
  Result := IsInteresting(AEditor, FFilter);
end;

procedure TplOICustomPropertyGrid.EraseBackground(DC: HDC);
begin
  // everything is painted, so erasing the background is not needed
end;

procedure TplOICustomPropertyGrid.DoSetBounds(ALeft, ATop, AWidth, AHeight: integer);
begin
  inherited DoSetBounds(ALeft, ATop, AWidth, AHeight);
  UpdateScrollBar;
end;

procedure TplOICustomPropertyGrid.DoSelectionChange;
begin
  if Assigned(FOnSelectionChange) then
    OnSelectionChange(Self);
end;

constructor TplOICustomPropertyGrid.Create(TheOwner: TComponent);
begin
  CreateWithParams(TheOwner,nil,AllTypeKinds,25);
end;

procedure TplOICustomPropertyGrid.OnUserInput(Sender: TObject; Msg: Cardinal);
begin
  ResetHintTimer;
end;

procedure TplOICustomPropertyGrid.EndDragSplitter;
begin
  if FDragging then begin
    Cursor:=crDefault;
    FDragging:=false;
    FPreferredSplitterX:=FSplitterX;
    if FCurrentEdit<>nil then begin
      SetCaptureControl(nil);
      if Column=oipgcValue then
        FCurrentEdit.SetFocus
      else
        Self.SetFocus;
    end;
  end;
end;

procedure TplOICustomPropertyGrid.SetReadOnlyColor(const AValue: TColor);
begin
  if FReadOnlyColor = AValue then Exit;
  FReadOnlyColor := AValue;
  Invalidate;
end;

procedure TplOICustomPropertyGrid.SetRowSpacing(const AValue: integer);
begin
  if FRowSpacing = AValue then exit;
  FRowSpacing := AValue;
  SetItemsTops;
end;

procedure TplOICustomPropertyGrid.SetShowGutter(const AValue: Boolean);
begin
  if FShowGutter=AValue then exit;
  FShowGutter:=AValue;
  invalidate;
end;

procedure TplOICustomPropertyGrid.SetSplitterX(const NewValue:integer);
var AdjustedValue:integer;
begin
  AdjustedValue:=NewValue;
  if AdjustedValue>ClientWidth then AdjustedValue:=ClientWidth;
  if AdjustedValue<1 then AdjustedValue:=1;
  if FSplitterX<>AdjustedValue then begin
    FSplitterX:=AdjustedValue;
    AlignEditComponents;
    Invalidate;
  end;
end;

procedure TplOICustomPropertyGrid.SetTopY(const NewValue:integer);
var
  NewTopY: integer;
begin
  NewTopY := TopMax;
  if NewValue < NewTopY then
    NewTopY := NewValue;
  if NewTopY < 0 then
    NewTopY := 0;
  if FTopY<>NewTopY then begin
    FTopY:=NewTopY;
    UpdateScrollBar;
    AlignEditComponents;
    Invalidate;
  end;
end;

function TplOICustomPropertyGrid.GetPropNameColor(ARow:TplOIPropertyGridRow):TColor;

 function HasWriter(APropInfo: PPropInfo): Boolean; inline;
 begin
   Result := Assigned(APropInfo) and Assigned(APropInfo^.SetProc);
 end;

var
  ParentRow:TplOIPropertyGridRow;
  IsObjectSubProperty:Boolean;
begin
  // Try to guest if ARow, or one of its parents, is a subproperty
  // of an object (and not an item of a set)
  IsObjectSubProperty:=false;
  ParentRow:=ARow.Parent;
  while Assigned(ParentRow) do
  begin
    if ParentRow.Editor is TPersistenTPropertyEditor then
      IsObjectSubProperty:=true;
    ParentRow:=ParentRow.Parent;
  end;

  if (ItemIndex <> -1) and (ItemIndex = ARow.Index) then
    Result := FHighlightFont.Color
  else
  if not HasWriter(ARow.Editor.GetPropInfo) then
    Result := FReadOnlyColor
  else
  if ARow.Editor is TPersistenTPropertyEditor then
    Result := FReferencesColor
  else
  if IsObjectSubProperty then
    Result := FSubPropertiesColor
  else
    Result := FNameFont.Color;
end;

procedure TplOICustomPropertyGrid.SetBounds(aLeft,aTop,aWidth,aHeight:integer);
begin

  inherited SetBounds(aLeft,aTop,aWidth,aHeight);
  if Visible then begin
    if not FDragging then begin
      if (SplitterX<5) and (aWidth>20) then
        SplitterX:=100
      else
        SplitterX:=FPreferredSplitterX;
    end;
    AlignEditComponents;
  end;
end;

function TplOICustomPropertyGrid.GetTreeIconX(Index:integer):integer;
begin
  Result:=Rows[Index].Lvl*Indent+2;
end;

function TplOICustomPropertyGrid.TopMax:integer;
begin
  Result:=GridHeight-ClientHeight+2*integer(BorderWidth);
  if Result<0 then Result:=0;
end;

function TplOICustomPropertyGrid.GridHeight:integer;
begin
  if FRows.Count>0 then
    Result:=Rows[FRows.Count-1].Bottom
  else
    Result:=0;
end;

procedure TplOICustomPropertyGrid.AlignEditComponents;
var RRect,EditCompRect,EditBtnRect:TRect;

  function CompareRectangles(r1,r2:TRect):boolean;
  begin
    Result:=(r1.Left=r2.Left) and (r1.Top=r2.Top) and (r1.Right=r2.Right)
       and (r1.Bottom=r2.Bottom);
  end;

// AlignEditComponents
begin
  if ItemIndex>=0
  then begin
    RRect := RowRect(ItemIndex);
    EditCompRect := RRect;
    EditCompRect.Bottom := EditCompRect.Bottom - 1;

    if Layout = oilHorizontal
    then begin
      EditCompRect.Left := RRect.Left + SplitterX;
    end
    else begin
      EditCompRect.Top := RRect.Top + GetNameRowHeight;
      EditCompRect.Left := RRect.Left + GetTreeIconX(ItemIndex) + Indent;
    end;

    if FCurrentButton<>nil then begin
      // edit dialog button
      with EditBtnRect do begin
        Top := EditCompRect.Top;
        Left := EditCompRect.Right - 20;
        Bottom := EditCompRect.Bottom;
        Right := EditCompRect.Right;
        EditCompRect.Right := Left;
      end;
      if not CompareRectangles(FCurrentButton.BoundsRect,EditBtnRect) then begin
        FCurrentButton.BoundsRect:=EditBtnRect;
      end;

    end;
    if FCurrentEdit<>nil then begin
      // resize the edit component
      Dec(EditCompRect.Left);
      Dec(EditCompRect.Top);
      Inc(EditCompRect.Bottom);

      if not CompareRectangles(FCurrentEdit.BoundsRect,EditCompRect) then begin
        FCurrentEdit.BoundsRect:=EditCompRect;
        if FCurrentEdit is TComboBox then
          TComboBox(FCurrentEdit).ItemHeight:=
                                         EditCompRect.Bottom-EditCompRect.Top-6;
        FCurrentEdit.Invalidate;
      end;
    end;
  end;
end;

procedure TplOICustomPropertyGrid.PaintRow(ARow: integer);
var
  FullRect,NameRect,NameIconRect,NameTextRect,ValueRect, ParentRect:TRect;
  IconX,IconY:integer;
  CurRow:TplOIPropertyGridRow;
  DrawState:TPropEditDrawState;
  OldFont:TFont;
  lclPlatform: TLCLPlatform;
  X, Y: Integer;
  NameBgColor: TColor;

  procedure DrawTreeIcon(X, Y: Integer; Minus: Boolean);
  const
    PlusMinusDetail: array[Boolean] of TThemedTreeview =
    (
      ttGlyphClosed,
      ttGlyphOpened
    );
  var
    Details: TThemedElementDetails;
    Size: TSize;
  begin
    Details := ThemeServices.GetElementDetails(PlusMinusDetail[Minus]);
    Size := ThemeServices.GetDetailSize(Details);
    ThemeServices.DrawElement(Canvas.Handle, Details, Rect(X, Y, X + Size.cx, Y + Size.cy), nil);
  end;

  procedure DrawActiveRow(X, Y: Integer);
  begin
    Canvas.Draw(X, Y, FActiveRowBmp);
  end;

// PaintRow
begin
  CurRow := Rows[ARow];
  FullRect := RowRect(ARow);
  NameRect := FullRect;
  ValueRect := FullRect;
  Inc(FullRect.Bottom, FRowSpacing);

  if ARow = FItemIndex then
  begin
    if Layout = oilHorizontal then
    begin
      if Assigned(FCurrentButton) and (FCurrentButton.Visible) then
        Dec(FullRect.Right, FCurrentButton.Width);

      if Assigned(FCurrentEdit) and (FCurrentEdit.Visible) then
        Dec(FullRect.Right, FCurrentEdit.Width);
    end;
  end;

  if Layout = oilHorizontal
  then begin
    NameRect.Right:=SplitterX;
    ValueRect.Left:=SplitterX;
  end
  else begin
    NameRect.Bottom := NameRect.Top + GetNameRowHeight;
    ValueRect.Top := NameRect.Bottom;
  end;

  IconX:=GetTreeIconX(ARow);
  IconY:=((NameRect.Bottom-NameRect.Top-9) div 2)+NameRect.Top;
  NameIconRect := NameRect;
  NameIconRect.Right := IconX + Indent;
  NameTextRect := NameRect;
  NameTextRect.Left := NameIconRect.Right;

  if Layout = oilVertical then
    ValueRect.Left := NameTextRect.Left
  else
  begin
    inc(NameIconRect.Right, 2 + Ord(ShowGutter));
    inc(NameTextRect.Left, 3 +  + Ord(ShowGutter));
  end;

  DrawState:=[];
  if ARow = FItemIndex then
    Include(DrawState, pedsSelected);

  with Canvas do
  begin
    // clear background in one go

    if (ARow = FItemIndex) and (FHighlightColor <> clNone) then
      NameBgColor := FHighlightColor
    else
      NameBgColor := FBackgroundColor;

    if FBackgroundColor <> clNone then
    begin
      Brush.Color := FBackgroundColor;
      FillRect(FullRect);
    end;

    if ShowGutter and (Layout = oilHorizontal) and
       (FGutterColor <> FBackgroundColor) and (FGutterColor <> clNone) then
    begin
      Brush.Color := FGutterColor;
      FillRect(NameIconRect);
    end;

    // draw icon
    if CanExpandRow(CurRow) then
      DrawTreeIcon(IconX, IconY, CurRow.Expanded)
    else
    if (ARow = FItemIndex) then
      DrawActiveRow(IconX, IconY);

    // draw name
    OldFont:=Font;
    Font:=FNameFont;
    Font.Color := GetPropNameColor(CurRow);
    // set bg color to highlight if needed
    if (NameBgColor <> FBackgroundColor) and (NameBgColor <> clNone) then
    begin
      Brush.Color := NameBgColor;
      FillRect(NameTextRect);
    end;
    CurRow.Editor.PropDrawName(Canvas, NameTextRect, DrawState);
    Font := OldFont;

    if (FBackgroundColor <> clNone) then // return color back to background
      Brush.Color := FBackgroundColor;

    // draw widgetsets
    X := NameRect.Right - 2;
    Y := (NameRect.Top + NameRect.Bottom - IDEImages.Images_16.Height) div 2;
    OldFont:=Font;
    Font:=FNameFont;
    Font.Color := clRed;
    for lclPlatform := High(TLCLPlatform) downto Low(TLCLPlatform) do
    begin
      if lclPlatform in CurRow.FWidgetSets then
      begin
        Dec(X, IDEImages.Images_16.Width);
        IDEImages.Images_16.Draw(Canvas, X, Y,
          IDEImages.LoadImage(16, 'issue_'+LCLPlatformDirNames[lclPlatform]));
      end;
    end;
    Font:=OldFont;

    // draw value
    if ARow<>ItemIndex
    then begin
      OldFont:=Font;
      if CurRow.Editor.IsNotDefaultValue then
        Font:=FValueFont
      else
        Font:=FDefaultValueFont;
      CurRow.Editor.PropDrawValue(Canvas,ValueRect,DrawState);
      Font:=OldFont;
    end;
    CurRow.LastPaintedValue:=CurRow.Editor.GetVisualValue;


    // -----------------
    // frames
    // -----------------

    if Layout = oilHorizontal then
    begin
      // Row Divider

      if DrawHorzGridLines then
      begin
        Pen.Style := psDot;
        Pen.EndCap := pecFlat;
        Pen.Cosmetic := False;
        Pen.Color := cl3DShadow;
        if FRowSpacing <> 0 then
        begin
          MoveTo(NameTextRect.Left, NameRect.Top - 1);
          LineTo(ValueRect.Right, NameRect.Top - 1);
        end;
        MoveTo(NameTextRect.Left, NameRect.Bottom - 1);
        LineTo(ValueRect.Right, NameRect.Bottom - 1);
      end;

      // Split lines between: icon and name, name and value
      Pen.Style := psSolid;
      Pen.Cosmetic := True;
      Pen.Color := cl3DHiLight;
      MoveTo(NameRect.Right - 1, NameRect.Bottom - 1);
      LineTo(NameRect.Right - 1, NameRect.Top - 1 - FRowSpacing);
      Pen.Color := cl3DShadow;
      MoveTo(NameRect.Right - 2, NameRect.Bottom - 1);
      LineTo(NameRect.Right - 2, NameRect.Top - 1 - FRowSpacing);

      // draw gutter line
      if ShowGutter then
      begin
        Pen.Color := GutterEdgeColor;
        MoveTo(NameIconRect.Right, NameRect.Bottom - 1);
        LineTo(NameIconRect.Right, NameRect.Top - 1 - FRowSpacing);

        if CurRow.Lvl > 0 then
        begin
          // draw to parent
          if ARow > 0 then
          begin
            ParentRect := RowRect(ARow - 1);
            X := ParentRect.Left + GetTreeIconX(ARow - 1) + Indent + 3;
            if X <> NameIconRect.Right then
            begin
              MoveTo(NameIconRect.Right, NameRect.Top - 1 - FRowSpacing);
              LineTo(X - 1, NameRect.Top - 1 - FRowSpacing);
            end;
          end;
          // to to parent next sibling
          if ARow < FRows.Count - 1 then
          begin
            ParentRect := RowRect(ARow + 1);
            X := ParentRect.Left + GetTreeIconX(ARow + 1) + Indent + 3;
            if X <> NameIconRect.Right then
            begin
              MoveTo(NameIconRect.Right, NameRect.Bottom - 1);
              LineTo(X - 1, NameRect.Bottom - 1);
            end;
          end;
        end;
      end;
    end
    else begin
      Pen.Style := psSolid;
      Pen.Color := cl3DLight;
      MoveTo(ValueRect.Left, ValueRect.Bottom - 1);
      LineTo(ValueRect.Left, NameTextRect.Top);
      LineTo(ValueRect.Right - 1, NameTextRect.Top);
      Pen.Color:=cl3DHiLight;
      LineTo(ValueRect.Right - 1, ValueRect.Bottom - 1);
      LineTo(ValueRect.Left, ValueRect.Bottom - 1);

      MoveTo(NameTextRect.Left + 1, NametextRect.Bottom);
      LineTo(NameTextRect.Left + 1, NameTextRect.Top + 1);
      LineTo(NameTextRect.Right - 2, NameTextRect.Top + 1);
      Pen.Color:=cl3DLight;
      LineTo(NameTextRect.Right - 2, NameTextRect.Bottom - 1);
      LineTo(NameTextRect.Left + 2, NameTextRect.Bottom - 1);
    end;
  end;
end;

procedure TplOICustomPropertyGrid.DoPaint(PaintOnlyChangedValues: boolean);
var
  a: integer;
  SpaceRect: TRect;
  GutterX: Integer;
begin
  BuildPropertyList(true);
  if not PaintOnlyChangedValues then
  begin
    with Canvas do
    begin
      // draw properties
      for a := 0 to FRows.Count - 1 do
        PaintRow(a);
      // draw unused space below rows
      SpaceRect := Rect(BorderWidth, BorderWidth,
                        ClientWidth - BorderWidth + 1, ClientHeight - BorderWidth + 1);
      if FRows.Count > 0 then
        SpaceRect.Top := Rows[FRows.Count - 1].Bottom - FTopY + BorderWidth;
      if FBackgroundColor <> clNone then
      begin
        Brush.Color := FBackgroundColor;
        FillRect(SpaceRect);
      end;

      // draw gutter if needed
      if ShowGutter and (Layout = oilHorizontal) then
      begin
        if FRows.Count > 0 then
          GutterX := RowRect(FRows.Count - 1).Left + GetTreeIconX(FRows.Count - 1)
        else
          GutterX := BorderWidth + 2;
        inc(GutterX, Indent + 3);
        SpaceRect.Right := GutterX;
        if GutterColor <> clNone then
        begin
          Brush.Color := GutterColor;
          FillRect(SpaceRect);
        end;
        MoveTo(GutterX, SpaceRect.Top);
        LineTo(GutterX, SpaceRect.Bottom);
      end;
      // don't draw border: borderstyle=bsSingle
    end;
  end else
  begin
    for a := 0 to FRows.Count-1 do
    begin
      if Rows[a].Editor.GetVisualValue <> Rows[a].LastPaintedValue then
        PaintRow(a);
    end;
  end;
end;

procedure TplOICustomPropertyGrid.Paint;
begin
  inherited Paint;
  DoPaint(false);
end;

procedure TplOICustomPropertyGrid.RefreshPropertyValues;
begin
  RefreshValueEdit;
  DoPaint(true);
end;

procedure TplOICustomPropertyGrid.ScrollToActiveItem;
begin
  ScrollToItem(FItemIndex);
end;

procedure TplOICustomPropertyGrid.ScrollToItem(NewIndex: Integer);
var
  NewRow: TplOIPropertyGridRow;
begin
  if (NewIndex >= 0) and (NewIndex < FRows.Count) then
  begin
    NewRow := Rows[NewIndex];
    if NewRow.Bottom >= TopY + (ClientHeight - 2*BorderWidth) then
      TopY := NewRow.Bottom- (ClientHeight - 2*BorderWidth) + 1
    else
      if NewRow.Top < TopY then TopY := NewRow.Top;
  end;
end;

procedure TplOICustomPropertyGrid.PropEditLookupRootChange;
begin
  // When the LookupRoot changes, no changes can be stored
  // -> undo the value editor changes
  RefreshValueEdit;
  if PropertyEditorHook<>nil then
    FCurrentEditorLookupRoot:=PropertyEditorHook.LookupRoot;
end;

function TplOICustomPropertyGrid.RowRect(ARow:integer):TRect;
const
  ScrollBarWidth=0;
begin
  Result.Left:=BorderWidth;
  Result.Top:=Rows[ARow].Top-FTopY+BorderWidth;
  Result.Right:=ClientWidth-ScrollBarWidth;
  Result.Bottom:=Rows[ARow].Bottom-FTopY+BorderWidth;
end;

procedure TplOICustomPropertyGrid.SetItemsTops;
// compute row tops from row heights
// set indices of all rows
var a:integer;
begin
  for a:=0 to FRows.Count-1 do begin
    Rows[a].FIndex:=a;
    Rows[a].MeasureHeight(Canvas);
  end;
  if FRows.Count>0 then
    Rows[0].Top:=0;
  for a:=1 to FRows.Count-1 do
    Rows[a].FTop:=Rows[a-1].Bottom + FRowSpacing;
end;

procedure TplOICustomPropertyGrid.ClearRows;
var i:integer;
begin
  IncreaseChangeStep;
  // reverse order to make sure child rows are freed before parent rows
  for i:=FRows.Count-1 downto 0 do
  begin
    Rows[i].Free;
    FRows[i]:=nil;
  end;
  FRows.Clear;
end;

function TplOICustomPropertyGrid.GetCurrentEditValue: string;
begin
  if FCurrentEdit=ValueEdit then
    Result:=ValueEdit.Text
  else if FCurrentEdit=ValueComboBox then
    Result:=ValueComboBox.Text
  else if FCurrentEdit=ValueCheckBox then
    Result:=ValueCheckBox.Caption
  else
    Result:='';
end;

procedure TplOICustomPropertyGrid.SetActiveControl(const AControl: TWinControl);
var
  F: TCustomForm;
begin
  F := GetParentForm(Self);
  if F <> nil then
    F.ActiveControl := AControl;
end;

procedure TplOICustomPropertyGrid.SetColumn(
  const AValue: TplOICustomPropertyGridColumn);
begin
  if FColumn <> AValue then
  begin
    FColumn := AValue;
    // TODO: indication
  end;
end;

procedure TplOICustomPropertyGrid.SetCurrentEditValue(const NewValue: string);
begin
  if FCurrentEdit=ValueEdit then
    ValueEdit.Text:=NewValue
  else if FCurrentEdit=ValueComboBox then
    ValueComboBox.Text:=NewValue
  else if FCurrentEdit=ValueCheckBox then begin
    ValueCheckBox.Caption:=NewValue;
    ValueCheckBox.Checked:=NewValue='True';
  end;
end;

procedure TplOICustomPropertyGrid.SetDrawHorzGridLines(const AValue: Boolean);
begin
  if FDrawHorzGridLines = AValue then Exit;
  FDrawHorzGridLines := AValue;
  Invalidate;
end;

procedure TplOICustomPropertyGrid.SetFavourites(const AValue: TOIFavoriteProperties);
begin
  if FFavourites=AValue then exit;
  FFavourites:=AValue;
  BuildPropertyList;
end;

procedure TplOICustomPropertyGrid.SetFilter(const AValue: TTypeKinds);
begin
  if (AValue<>FFilter) then
  begin
    FFilter:=AValue;
    BuildPropertyList;
  end;
end;

procedure TplOICustomPropertyGrid.SetGutterColor(const AValue: TColor);
begin
  if FGutterColor=AValue then exit;
  FGutterColor:=AValue;
  invalidate;
end;

procedure TplOICustomPropertyGrid.SetGutterEdgeColor(const AValue: TColor);
begin
  if FGutterEdgeColor=AValue then exit;
  FGutterEdgeColor:=AValue;
  invalidate;
end;

procedure TplOICustomPropertyGrid.SetHighlightColor(const AValue: TColor);
begin
  if FHighlightColor=AValue then exit;
  FHighlightColor:=AValue;
  Invalidate;
end;

procedure TplOICustomPropertyGrid.Clear;
begin
  ClearRows;
end;

function TplOICustomPropertyGrid.GetRow(Index:integer):TplOIPropertyGridRow;
begin
  Result:=TplOIPropertyGridRow(FRows[Index]);
end;

procedure TplOICustomPropertyGrid.ValueComboBoxCloseUp(Sender: TObject);
begin
  SetRowValue;
end;

procedure TplOICustomPropertyGrid.ValueComboBoxGetItems(Sender: TObject);
{ This event is called whenever the widgetset updates the list.
  On gtk the list is updated just before the user popups the list.
  Other widgetsets need the list always, which is bad, as this means collecting
  all items even if the dropdown never happens.
}
var
  CurRow: TplOIPropertyGridRow;
  MaxItemWidth, CurItemWidth, i, Cnt: integer;
  ItemValue, CurValue: string;
  NewItemIndex: LongInt;
  ExcludeUpdateFlag: boolean;
begin
  Include(FStates,pgsGetComboItemsCalled);
  if (FItemIndex>=0) and (FItemIndex<FRows.Count) then begin
    ExcludeUpdateFlag:=not (pgsUpdatingEditControl in FStates);
    Include(FStates,pgsUpdatingEditControl);
    ValueComboBox.Items.BeginUpdate;
    try
      CurRow:=Rows[FItemIndex];

      // Items
      if not FillComboboxItems then exit;

      // Text and ItemIndex
      CurValue:=CurRow.Editor.GetVisualValue;
      ValueComboBox.Text:=CurValue;
      NewItemIndex:=ValueComboBox.Items.IndexOf(CurValue);
      if NewItemIndex>=0 then
        ValueComboBox.ItemIndex:=NewItemIndex;

      // ItemWidth
      MaxItemWidth:=ValueComboBox.Width;
      Cnt:=ValueComboBox.Items.Count;
      for i:=0 to Cnt-1 do begin
        ItemValue:=ValueComboBox.Items[i];
        CurItemWidth:=ValueComboBox.Canvas.TextWidth(ItemValue);
        CurRow.Editor.ListMeasureWidth(ItemValue,i,ValueComboBox.Canvas,
                                       CurItemWidth);
        if MaxItemWidth<CurItemWidth then
          MaxItemWidth:=CurItemWidth;
      end;
      ValueComboBox.ItemWidth:=MaxItemWidth;
    finally
      ValueComboBox.Items.EndUpdate;
      if ExcludeUpdateFlag then
        Exclude(FStates,pgsUpdatingEditControl);
    end;
  end;
end;

procedure TplOICustomPropertyGrid.ValueComboBoxDrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  CurRow: TplOIPropertyGridRow;
  ItemValue: string;
  AState: TPropEditDrawState;
begin
  if (FItemIndex>=0) and (FItemIndex<FRows.Count) then begin
    CurRow:=Rows[FItemIndex];
    if (Index>=0) and (Index<ValueComboBox.Items.Count) then
      ItemValue:=ValueComboBox.Items[Index]
    else
      ItemValue:='';
    AState:=[];
    if odPainted in State then Include(AState,pedsPainted);
    if odSelected in State then Include(AState,pedsSelected);
    if odFocused in State then Include(AState,pedsFocused);
    if odComboBoxEdit in State then
      Include(AState,pedsInEdit)
    else
      Include(AState,pedsInComboList);

    // clear background
    with ValueComboBox.Canvas do begin
      Brush.Color:=clWhite;
      Pen.Color:=clBlack;
      Font.Color:=Pen.Color;
      FillRect(ARect);
    end;
    CurRow.Editor.ListDrawValue(ItemValue,Index,ValueComboBox.Canvas,ARect,
                                AState);
  end;
end;

procedure TplOICustomPropertyGrid.OnIdle(Sender: TObject; var Done: Boolean);
begin
  if (not (pgsGetComboItemsCalled in FStates))
  and (FCurrentEdit=ValueComboBox)
  and ValueComboBox.Enabled
  then begin
    ValueComboBoxGetItems(Self);
  end;
end;

procedure TplOICustomPropertyGrid.SetIdleEvent(Enable: boolean);
begin
  if (pgsIdleEnabled in FStates)=Enable then exit;
  if Enable then begin
    Application.AddOnIdleHandler(@OnIdle);
    Include(FStates,pgsIdleEnabled);
  end else begin
    Application.RemoveOnIdleHandler(@OnIdle);
    Exclude(FStates,pgsIdleEnabled);
  end;
end;

procedure TplOICustomPropertyGrid.HintTimer(sender : TObject);
var
  Rect : TRect;
  AHint : String;
  Position : TPoint;
  Index: integer;
  PointedRow:TplOIPropertyGridRow;
  Window: TWinControl;
  HintType: TPropEditHint;
  ClientPosition: TPoint;
begin
  if FHintTimer <> nil then
    FHintTimer.Enabled := False;
  if (not InitHints) then exit;

  Position := Mouse.CursorPos;

  Window := FindLCLWindow(Position);
  if not Assigned(Window) then Exit;
  If (Window <> Self) and (not IsParentOf(Window)) then exit;

  ClientPosition := ScreenToClient(Position);
  if ((ClientPosition.X <=0) or (ClientPosition.X >= Width) or
     (ClientPosition.Y <= 0) or (ClientPosition.Y >= Height)) then
    Exit;

  AHint := '';
  Index := MouseToIndex(ClientPosition.Y, False);
  if (Index >= 0) and (Index < FRows.Count) then
  begin
    //IconX:=GetTreeIconX(Index);
    PointedRow := Rows[Index];
    if Assigned(PointedRow) and Assigned(PointedRow.Editor) then
    begin
      if Index <> ItemIndex then
      begin
        HintType := GetHintTypeAt(Index,Position.X);
        if (HintType = pehName) and Assigned(OnPropertyHint) then
        begin
          if OnPropertyHint(Self, PointedRow, Position, FHintWindow, Rect, AHint) then
          begin
            FHintIndex := Index;
            FShowingLongHint := True;
            FHintWindow.ActivateHint(Rect, AHint);
          end;
          exit;
        end;
        AHint := PointedRow.Editor.GetHint(HintType, Position.X, Position.Y);
      end;
    end;
  end;

  if AHint = '' then Exit;
  FHintIndex := Index;
  FShowingLongHint := True;
  Rect := FHintWindow.CalcHintRect(0, AHint, nil);  //no maxwidth
  Rect.Left := Position.X + 10;
  Rect.Top := Position.Y + 10;
  Rect.Right := Rect.Left + Rect.Right + 3;
  Rect.Bottom := Rect.Top + Rect.Bottom + 3;

  FHintWindow.ActivateHint(Rect, AHint);
end;

Procedure TplOICustomPropertyGrid.ResetHintTimer;
begin
  if FHintWindow = nil then exit;

  HideHint;

  FHintTimer.Enabled := False;
  if RowCount > 0 then
    FHintTimer.Enabled := not FDragging;
end;

procedure TplOICustomPropertyGrid.HideHint;
begin
  if FHintWindow = nil then Exit;
  FHintWindow.Visible := False;
  FHintIndex := -1;
  FShowingLongHint := False;
  while FHintWindow.ControlCount > 0 do
    FHintWindow.Controls[0].Free;
end;

procedure TplOICustomPropertyGrid.ValueControlMouseDown(Sender : TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  HideHint;
  ScrollToActiveItem;
end;

procedure TplOICustomPropertyGrid.ValueControlMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: integer);
begin
  // when the cursor is divider change it to default
  if (Sender as TControl).Parent.Cursor <> crDefault then
    (Sender as TControl).Parent.Cursor := crDefault;
end;

procedure TplOICustomPropertyGrid.IncreaseChangeStep;
begin
  if FChangeStep<>$7fffffff then
    inc(FChangeStep)
  else
    FChangeStep:=-$7fffffff;
end;

function TplOICustomPropertyGrid.GridIsUpdating: boolean;
begin
  Result:=(FStates*[pgsChangingItemIndex,pgsApplyingValue,
                    pgsBuildPropertyListNeeded]<>[])
end;

procedure TplOICustomPropertyGrid.ToggleRow;
var
  CurRow: TplOIPropertyGridRow;
  TypeKind : TTypeKind;
begin
  if not CanEditRowValue then exit;

  if FHintTimer <> nil then
    FHintTimer.Enabled := False;

  if (FCurrentEdit = ValueComboBox) then
  begin
    //either an Event or an enumeration or Boolean
    CurRow := Rows[FItemIndex];
    TypeKind := CurRow.Editor.GetPropType^.Kind;
    if TypeKind in [tkEnumeration, tkBool, tkSet] then
    begin
      // set value to next value in list
      if ValueComboBox.Items.Count = 0 then Exit;
      if ValueComboBox.ItemIndex < (ValueComboBox.Items.Count - 1) then
        ValueComboBox.ItemIndex := ValueComboBox.ItemIndex + 1
      else
        ValueComboBox.ItemIndex := 0;
      SetRowValue;
      exit;
    end;
  end;
  DoCallEdit;
end;

procedure TplOICustomPropertyGrid.ValueEditDblClick(Sender: TObject);
begin
  FFirstClickTime:=0;
  ToggleRow;
end;

procedure TplOICustomPropertyGrid.SetBackgroundColor(const AValue: TColor);
begin
  if FBackgroundColor=AValue then exit;
  FBackgroundColor:=AValue;
  Invalidate;
end;

procedure TplOICustomPropertyGrid.SetReferences(const AValue: TColor);
begin
  if FReferencesColor=AValue then exit;
  FReferencesColor:=AValue;
  Invalidate;
end;

procedure TplOICustomPropertyGrid.SetSubPropertiesColor(const AValue: TColor);
begin
  if FSubPropertiesColor=AValue then exit;
  FSubPropertiesColor:=AValue;
  Invalidate;
end;


//========================== TplOIPropertyGridRow ====================================

constructor TplOIPropertyGridRow.Create(PropertyTree: TplOICustomPropertyGrid;
  PropEditor:TPropertyEditor; ParentNode:TplOIPropertyGridRow; WidgetSets: TLCLPlatforms);
begin
  inherited Create;
  // tree pointer
  FTree:=PropertyTree;
  FParent:=ParentNode;
  FNextBrother:=nil;
  FPriorBrother:=nil;
  FExpanded:=false;
  // child nodes
  FChildCount:=0;
  FFirstChild:=nil;
  FLastChild:=nil;
  // director
  FEditor:=PropEditor;
  GetLvl;
  FName:=FEditor.GetName;
  FTop:=0;
  FHeight:=FTree.DefaultItemHeight;
  FIndex:=-1;
  LastPaintedValue:='';
  FWidgetSets := WidgetSets;
end;

destructor TplOIPropertyGridRow.Destroy;
begin

  if FPriorBrother<>nil then FPriorBrother.FNextBrother:=FNextBrother;
  if FNextBrother<>nil then FNextBrother.FPriorBrother:=FPriorBrother;
  if FParent<>nil then begin
    if FParent.FFirstChild=Self then FParent.FFirstChild:=FNextBrother;
    if FParent.FLastChild=Self then FParent.FLastChild:=FPriorBrother;
    dec(FParent.FChildCount);
  end;
  if FEditor<>nil then FEditor.Free;
  inherited Destroy;
end;

function TplOIPropertyGridRow.ConsistencyCheck: integer;
var
  OldLvl, RealChildCount: integer;
  AChild: TplOIPropertyGridRow;
begin
  if Top<0 then begin
    Result:=-1;
    exit;
  end;
  if Height<0 then begin
    Result:=-2;
    exit;
  end;
  if Lvl<0 then begin
    Result:=-3;
    exit;
  end;
  OldLvl:=Lvl;
  GetLvl;
  if Lvl<>OldLvl then begin
    Result:=-4;
    exit;
  end;
  if Name='' then begin
    Result:=-5;
    exit;
  end;
  if NextBrother<>nil then begin
    if NextBrother.PriorBrother<>Self then begin
      Result:=-6;
      exit;
    end;
    if NextBrother.Index<Index+1 then begin
      Result:=-7;
      exit;
    end;
  end;
  if PriorBrother<>nil then begin
    if PriorBrother.NextBrother<>Self then begin
      Result:=-8;
      exit;
    end;
    if PriorBrother.Index>Index-1 then begin
      Result:=-9
    end;
  end;
  if (Parent<>nil) then begin
    // has parent
    if (not Parent.HasChild(Self)) then begin
      Result:=-10;
      exit;
    end;
  end else begin
    // no parent
  end;
  if FirstChild<>nil then begin
    if Expanded then begin
      if (FirstChild.Index<>Index+1) then begin
        Result:=-11;
        exit;
      end;
    end;
  end else begin
    if LastChild<>nil then begin
      Result:=-12;
      exit;
    end;
  end;
  RealChildCount:=0;
  AChild:=FirstChild;
  while AChild<>nil do begin
    if AChild.Parent<>Self then begin
      Result:=-13;
      exit;
    end;
    inc(RealChildCount);
    AChild:=AChild.NextBrother;
  end;
  if RealChildCount<>ChildCount then begin
    Result:=-14;
    exit;
  end;
  Result:=0;
end;

function TplOIPropertyGridRow.HasChild(Row: TplOIPropertyGridRow): boolean;
var
  ChildRow: TplOIPropertyGridRow;
begin
  ChildRow:=FirstChild;
  while ChildRow<>nil do begin
    if ChildRow=Row then begin
      Result:=true;
      exit;
    end;
  end;
  Result:=false;
end;


procedure TplOIPropertyGridRow.GetLvl;
var n:TplOIPropertyGridRow;
begin
  FLvl:=0;
  n:=FParent;
  while n<>nil do begin
    inc(FLvl);
    n:=n.FParent;
  end;
end;

function TplOIPropertyGridRow.GetBottom:integer;
begin
  Result:=FTop+FHeight;
  if FTree.Layout = oilVertical
  then Inc(Result, FTree.GetNameRowHeight);
end;

function TplOIPropertyGridRow.IsReadOnly: boolean;
begin
  Result:=Editor.IsReadOnly or IsDisabled;
end;

function TplOIPropertyGridRow.IsDisabled: boolean;
var
  ParentRow: TplOIPropertyGridRow;
begin
  Result:=false;
  ParentRow:=Parent;
  while (ParentRow<>nil) do begin
    if paDisableSubProperties in ParentRow.Editor.GetAttributes then begin
      Result:=true;
      exit;
    end;
    ParentRow:=ParentRow.Parent;
  end;
end;

procedure TplOIPropertyGridRow.MeasureHeight(ACanvas: TCanvas);
begin
  FHeight:=FTree.DefaultItemHeight;
  Editor.PropMeasureHeight(Name,ACanvas,FHeight);
end;

function TplOIPropertyGridRow.Sort(const Compare: TListSortCompare): boolean;
var
  List: TFPList;
  Item: TplOIPropertyGridRow;
  i: Integer;
begin
  if IsSorted(Compare) then exit(false);
  List:=TFPList.Create;
  try
    // create a TFPList of the childs
    List.Capacity:=ChildCount;
    Item:=FirstChild;
    while Item<>nil do begin
      List.Add(Item);
      Item:=Item.NextBrother;
    end;
    // sort the TFPList
    List.Sort(Compare);
    // sort in double linked list
    for i:=0 to List.Count-1 do begin
      Item:=TplOIPropertyGridRow(List[i]);
      if i=0 then begin
        FFirstChild:=Item;
        Item.FPriorBrother:=nil;
      end else
        Item.FPriorBrother:=TplOIPropertyGridRow(List[i-1]);
      if i=List.Count-1 then begin
        FLastChild:=Item;
        Item.FNextBrother:=nil;
      end else
        Item.FNextBrother:=TplOIPropertyGridRow(List[i+1]);
    end;
  finally
    List.Free;
  end;
  Result:=true;
end;

function TplOIPropertyGridRow.IsSorted(const Compare: TListSortCompare): boolean;
var
  Item1: TplOIPropertyGridRow;
  Item2: TplOIPropertyGridRow;
begin
  if ChildCount<2 then exit(true);
  Item1:=FirstChild;
  while true do begin
    Item2:=Item1.NextBrother;
    if Item2=nil then break;
    if Compare(Item1,Item2)>0 then exit(false);
    Item1:=Item2;
  end;
  Result:=true;
end;

function TplOIPropertyGridRow.Next: TplOIPropertyGridRow;
begin
  if fFirstChild<>nil then
    Result:=fFirstChild
  else
    Result:=NextSkipChilds;
end;

function TplOIPropertyGridRow.NextSkipChilds: TplOIPropertyGridRow;
begin
  Result:=Self;
  while (Result<>nil) do begin
    if Result.NextBrother<>nil then begin
      Result:=Result.NextBrother;
      exit;
    end;
    Result:=Result.Parent;
  end;
end;

//========================= TplOIOptions =====================================================

function TplOIOptions.FPropertyGridSplitterX(Page: TplObjectInspectorPage): integer;
begin
  Result:=FGridSplitterX[Page];
end;

procedure TplOIOptions.FPropertyGridSplitterX(Page: TplObjectInspectorPage;
  const AValue: integer);
begin
  FGridSplitterX[Page]:=AValue;
end;

constructor TplOIOptions.Create;
var
  p: TplObjectInspectorPage;
begin
  inherited Create;

  FSaveBounds:=false;
  FLeft:=0;
  FTop:=0;
  FWidth:=250;
  FHeight:=400;
  for p:=Low(TplObjectInspectorPage) to High(TplObjectInspectorPage) do
    FGridSplitterX[p]:=110;
  FDefaultItemHeight:=20;
  FShowComponentTree:=true;
  FComponentTreeHeight:=100;
  FInfoBoxHeight:=80;

  FGridBackgroundColor := DefBackgroundColor;
  FDefaultValueColor := DefDefaultValueColor;
  FSubPropertiesColor := DefSubPropertiesColor;
  FValueColor := DefValueColor;
  FReadOnlyColor := DefReadOnlyColor;
  FReferencesColor := DefReferencesColor;
  FPropertyNameColor := DefNameColor;
  FHighlightColor := DefHighlightColor;
  FHighlightFontColor := DefHighlightFontColor;
  FGutterColor := DefGutterColor;
  FGutterEdgeColor := DefGutterEdgeColor;

  FBoldNonDefaultValues := True;
  FDrawGridLines := True;
  FShowGutter := True;
  FShowStatusBar := True;
  FShowInfoBox := False;
end;

function TplOIOptions.Load: boolean;
var
  Path: String;
  FileVersion: integer;
  Page: TplObjectInspectorPage;
begin
  Result:=false;
  if ConfigStore=nil then exit;
  try
    Path:='ObjectInspectorOptions/';
    FileVersion:=ConfigStore.GetValue(Path+'Version/Value',0);

    FSaveBounds:=ConfigStore.GetValue(Path+'Bounds/Valid'
                                      ,false);
    if FSaveBounds then begin
      FLeft:=ConfigStore.GetValue(Path+'Bounds/Left',0);
      FTop:=ConfigStore.GetValue(Path+'Bounds/Top',0);
      FWidth:=ConfigStore.GetValue(Path+'Bounds/Width',250);
      FHeight:=ConfigStore.GetValue(Path+'Bounds/Height',400);
    end;

    if FileVersion>=2 then begin
      for Page:=Low(TplObjectInspectorPage) to High(TplObjectInspectorPage) do
        FGridSplitterX[Page]:=ConfigStore.GetValue(
           Path+'Bounds/'+DefaultOIPageNames[Page]+'/SplitterX',110);
    end else begin
      FGridSplitterX[oipgpProperties]:=ConfigStore.GetValue(
         Path+'Bounds/PropertyGridSplitterX',110);
      FGridSplitterX[oipgpEvents]:=ConfigStore.GetValue(
         Path+'Bounds/EventGridSplitterX',110);
    end;
    for Page:=Low(TplObjectInspectorPage) to High(TplObjectInspectorPage) do
      if FGridSplitterX[Page]<10 then FGridSplitterX[Page]:=10;

    FDefaultItemHeight:=ConfigStore.GetValue(
       Path+'Bounds/DefaultItemHeight',20);
    if FDefaultItemHeight<0 then FDefaultItemHeight:=20;
    FShowComponentTree:=ConfigStore.GetValue(
       Path+'ComponentTree/Show/Value',true);
    FComponentTreeHeight:=ConfigStore.GetValue(
       Path+'ComponentTree/Height/Value',100);

    FGridBackgroundColor:=ConfigStore.GetValue(
         Path+'Color/GridBackground',DefBackgroundColor);
    FDefaultValueColor:=ConfigStore.GetValue(
         Path+'Color/DefaultValue', DefDefaultValueColor);
    FSubPropertiesColor:=ConfigStore.GetValue(
         Path+'Color/SubProperties', DefSubPropertiesColor);
    FValueColor:=ConfigStore.GetValue(
         Path+'Color/Value', DefValueColor);
    FReadOnlyColor:=ConfigStore.GetValue(
         Path+'Color/ReadOnly', DefReadOnlyColor);
    FReferencesColor:=ConfigStore.GetValue(
         Path+'Color/References',DefReferencesColor);
    FPropertyNameColor:=ConfigStore.GetValue(
         Path+'Color/PropertyName',DefNameColor);
    FHighlightColor:=ConfigStore.GetValue(
         Path+'Color/Highlight',DefHighlightColor);
    FHighlightFontColor:=ConfigStore.GetValue(
         Path+'Color/HighlightFont',DefHighlightFontColor);
    FGutterColor:=ConfigStore.GetValue(
         Path+'Color/Gutter',DefGutterColor);
    FGutterEdgeColor:=ConfigStore.GetValue(
         Path+'Color/GutterEdge',DefGutterEdgeColor);

    FShowHints:=ConfigStore.GetValue(
         Path+'ShowHints',FileVersion>=3);
    FAutoShow := ConfigStore.GetValue(
         Path+'AutoShow',true);
    FBoldNonDefaultValues := ConfigStore.GetValue(
         Path+'BoldNonDefaultValues',true);
    FDrawGridLines := ConfigStore.GetValue(
         Path+'DrawGridLines',true);
    FShowGutter := ConfigStore.GetValue(
         Path+'ShowGutter',true);
    FShowStatusBar := ConfigStore.GetValue(
         Path+'ShowStatusBar',true);
    FShowInfoBox := ConfigStore.GetValue(
         Path+'ShowInfoBox',false);
    FInfoBoxHeight := ConfigStore.GetValue(
       Path+'InfoBoxHeight',80);
  except
    on E: Exception do begin
      DebugLn('ERROR: TplOIOptions.Load: ',E.Message);
      exit;
    end;
  end;
  Result:=true;
end;

function TplOIOptions.Save: boolean;
var
  Page: TplObjectInspectorPage;
  Path: String;
begin
  Result:=false;
  if ConfigStore=nil then exit;
  try
    Path:='ObjectInspectorOptions/';
    ConfigStore.SetValue(Path+'Version/Value',OIOptionsFileVersion);

    ConfigStore.SetDeleteValue(Path+'Bounds/Valid',FSaveBounds,
                             false);

    ConfigStore.SetDeleteValue(Path+'Bounds/Valid',FSaveBounds,
                             false);
    if FSaveBounds then begin
      ConfigStore.SetValue(Path+'Bounds/Left',FLeft);
      ConfigStore.SetValue(Path+'Bounds/Top',FTop);
      ConfigStore.SetValue(Path+'Bounds/Width',FWidth);
      ConfigStore.SetValue(Path+'Bounds/Height',FHeight);
    end;
    for Page:=Low(TplObjectInspectorPage) to High(TplObjectInspectorPage) do
      ConfigStore.SetDeleteValue(
         Path+'Bounds/'+DefaultOIPageNames[Page]+'/SplitterX',
         FGridSplitterX[Page],110);
    ConfigStore.SetDeleteValue(Path+'Bounds/DefaultItemHeight',
                             FDefaultItemHeight,20);
    ConfigStore.SetDeleteValue(Path+'ComponentTree/Show/Value',
                             FShowComponentTree,true);
    ConfigStore.SetDeleteValue(Path+'ComponentTree/Height/Value',
                             FComponentTreeHeight,100);

    ConfigStore.SetDeleteValue(Path+'Color/GridBackground',
                             FGridBackgroundColor,DefBackgroundColor);
    ConfigStore.SetDeleteValue(Path+'Color/DefaultValue',
                             FDefaultValueColor,DefDefaultValueColor);
    ConfigStore.SetDeleteValue(Path+'Color/SubProperties',
                             FSubPropertiesColor,DefSubPropertiesColor);
    ConfigStore.SetDeleteValue(Path+'Color/Value',
                             FValueColor,DefValueColor);
    ConfigStore.SetDeleteValue(Path+'Color/ReadOnly',
                             FReadOnlyColor,DefReadOnlyColor);
    ConfigStore.SetDeleteValue(Path+'Color/References',
                             FReferencesColor,DefReferencesColor);
    ConfigStore.SetDeleteValue(Path+'Color/PropertyName',
                              FPropertyNameColor,DefNameColor);
    ConfigStore.SetDeleteValue(Path+'Color/Highlight',
                              FHighlightColor,DefHighlightColor);
    ConfigStore.SetDeleteValue(Path+'Color/HighlightFont',
                              FHighlightFontColor,DefHighlightFontColor);
    ConfigStore.SetDeleteValue(Path+'Color/Gutter',
                              FGutterColor,DefGutterColor);
    ConfigStore.SetDeleteValue(Path+'Color/GutterEdge',
                              FGutterEdgeColor,DefGutterEdgeColor);

    ConfigStore.SetDeleteValue(Path+'ShowHints',FShowHints, True);
    ConfigStore.SetDeleteValue(Path+'AutoShow',FAutoShow, True);
    ConfigStore.SetDeleteValue(Path+'BoldNonDefaultValues',FBoldNonDefaultValues, True);
    ConfigStore.SetDeleteValue(Path+'DrawGridLines',FDrawGridLines, True);
    ConfigStore.SetDeleteValue(Path+'ShowGutter',FShowGutter, True);
    ConfigStore.SetDeleteValue(Path+'ShowStatusBar',FShowStatusBar, True);
    ConfigStore.SetDeleteValue(Path+'ShowInfoBox',FShowInfoBox, False);
    ConfigStore.SetDeleteValue(Path+'InfoBoxHeight',FInfoBoxHeight,80);
  except
    on E: Exception do begin
      DebugLn('ERROR: TplOIOptions.Save: ',E.Message);
      exit;
    end;
  end;
  Result:=true;
end;

procedure TplOIOptions.Assign(AnObjInspector: TplObjectInspector);
var
  Page: TplObjectInspectorPage;
begin
  FLeft:=AnObjInspector.Left;
  FTop:=AnObjInspector.Top;
  FWidth:=AnObjInspector.Width;
  FHeight:=AnObjInspector.Height;
  for Page:=Low(TplObjectInspectorPage) to High(TplObjectInspectorPage) do
    if AnObjInspector.GridControl[Page]<>nil then
      FGridSplitterX[Page]:=AnObjInspector.GridControl[Page].PrefferedSplitterX;
  FDefaultItemHeight:=AnObjInspector.DefaultItemHeight;


  FGridBackgroundColor:=AnObjInspector.PropertyGrid.BackgroundColor;
  FSubPropertiesColor:=AnObjInspector.PropertyGrid.SubPropertiesColor;
  FReferencesColor:=AnObjInspector.PropertyGrid.ReferencesColor;
  FValueColor:=AnObjInspector.PropertyGrid.ValueFont.Color;
  FDefaultValueColor:=AnObjInspector.PropertyGrid.DefaultValueFont.Color;
  FReadOnlyColor:=AnObjInspector.PropertyGrid.ReadOnlyColor;
  FPropertyNameColor:=AnObjInspector.PropertyGrid.NameFont.Color;
  FHighlightColor:=AnObjInspector.PropertyGrid.HighlightColor;
  FHighlightFontColor:=AnObjInspector.PropertyGrid.HighlightFont.Color;
  FGutterColor:=AnObjInspector.PropertyGrid.GutterColor;
  FGutterEdgeColor:=AnObjInspector.PropertyGrid.GutterEdgeColor;

  FShowHints := AnObjInspector.PropertyGrid.ShowHint;
  FAutoShow := AnObjInspector.AutoShow;
  FBoldNonDefaultValues := fsBold in AnObjInspector.PropertyGrid.ValueFont.Style;
  FDrawGridLines := AnObjInspector.PropertyGrid.DrawHorzGridLines;
  FShowGutter := AnObjInspector.PropertyGrid.ShowGutter;

end;

procedure TplOIOptions.AssignTo(AnObjInspector: TplObjectInspector);
var
  Page: TplObjectInspectorPage;
  Grid: TplOICustomPropertyGrid;
begin
  if FSaveBounds then
  begin
    AnObjInspector.SetBounds(FLeft,FTop,FWidth,FHeight);
  end;

  for Page := Low(TplObjectInspectorPage) to High(TplObjectInspectorPage) do
  begin
    Grid := AnObjInspector.GridControl[Page];
    if Grid = nil then
      Continue;
    Grid.PrefferedSplitterX := FGridSplitterX[Page];
    Grid.SplitterX := FGridSplitterX[Page];
    AssignTo(Grid);
  end;
  AnObjInspector.DefaultItemHeight := DefaultItemHeight;

  AnObjInspector.AutoShow := AutoShow;
end;

procedure TplOIOptions.AssignTo(AGrid: TplOICustomPropertyGrid);
begin
  AGrid.BackgroundColor := FGridBackgroundColor;
  AGrid.SubPropertiesColor := FSubPropertiesColor;
  AGrid.ReferencesColor := FReferencesColor;
  AGrid.ReadOnlyColor := FReadOnlyColor;
  AGrid.ValueFont.Color := FValueColor;
  if FBoldNonDefaultValues then
    AGrid.ValueFont.Style := [fsBold]
  else
    AGrid.ValueFont.Style := [];
  AGrid.DefaultValueFont.Color := FDefaultValueColor;
  AGrid.NameFont.Color := FPropertyNameColor;
  AGrid.HighlightColor := FHighlightColor;
  AGrid.HighlightFont.Color := FHighlightFontColor;
  AGrid.GutterColor := FGutterColor;
  AGrid.GutterEdgeColor := FGutterEdgeColor;
  AGrid.ShowHint := FShowHints;
  AGrid.DrawHorzGridLines := FDrawGridLines;
  AGrid.ShowGutter := FShowGutter;
end;


//============================ TplCustomPropertiesGrid =============================================

function TplCustomPropertiesGrid.GetTIObject: TPersistent;
begin
  if PropertyEditorHook<>nil then Result:=PropertyEditorHook.LookupRoot;
end;

procedure TplCustomPropertiesGrid.SetAutoFreeHook(const AValue: boolean);
begin
  if FAutoFreeHook=AValue then exit;
  FAutoFreeHook:=AValue;
end;

procedure TplCustomPropertiesGrid.SetTIObject(const AValue: TPersistent);
var
  NewSelection: TPersistentSelectionList;
begin
  if (TIObject=AValue) then begin
    if ((AValue<>nil) and (Selection.Count=1) and (Selection[0]=AValue))
    or (AValue=nil) then
      exit;
  end;
  if SaveOnChangeTIObject then
    SaveChanges;
  if PropertyEditorHook=nil then
    PropertyEditorHook:=TPropertyEditorHook.Create;
  PropertyEditorHook.LookupRoot:=AValue;
  if (AValue<>nil) and ((Selection.Count<>1) or (Selection[0]<>AValue)) then
  begin
    NewSelection:=TPersistentSelectionList.Create;
    try
      if AValue<>nil then
        NewSelection.Add(AValue);
      Selection:=NewSelection;
    finally
      NewSelection.Free;
    end;
  end;
end;

constructor TplCustomPropertiesGrid.Create(TheOwner: TComponent);
var
  Hook: TPropertyEditorHook;
begin
  Hook:=TPropertyEditorHook.Create;
  FSaveOnChangeTIObject:=true;
  FAutoFreeHook:=true;
  CreateWithParams(TheOwner,Hook,AllTypeKinds,25);
end;

destructor TplCustomPropertiesGrid.Destroy;
begin
  if FAutoFreeHook then
    FPropertyEditorHook.Free;
  inherited Destroy;
end;

//==================================================================================================
//======================= TplObjectInspector =======================================================
//==================================================================================================

constructor TplObjectInspector.Create(AnOwner: TComponent);

//------------------------------------------------------------------
  procedure AddPopupMenuItem(var NewMenuItem: TMenuItem;
    ParentMenuItem: TMenuItem; const AName, ACaption, AHint, AResourceName: string;
    AnOnClick: TNotifyEvent; CheckedFlag, EnabledFlag, VisibleFlag: boolean);
  begin
    NewMenuItem:=TMenuItem.Create(Self);
    with NewMenuItem do
    begin
      Name:=AName;
      Caption:=ACaption;
      Hint:=AHint;
      OnClick:=AnOnClick;
      Checked:=CheckedFlag;
      Enabled:=EnabledFlag;
      Visible:=VisibleFlag;
      if AResourceName <> '' then
        ImageIndex := IDEImages.LoadImage(16, AResourceName);
    end;
    if ParentMenuItem<>nil then
      ParentMenuItem.Add(NewMenuItem)
    else
      MainPopupMenu.Items.Add(NewMenuItem);
  end;

  function AddSeparatorMenuItem(ParentMenuItem: TMenuItem; const AName: string; VisibleFlag: boolean): TMenuItem;
  begin
    Result := TMenuItem.Create(Self);
    with Result do
    begin
      Name := AName;
      Caption := cLineCaption;
      Visible := VisibleFlag;
    end;
    if ParentMenuItem <> nil then
      ParentMenuItem.Add(Result)
    else
      MainPopupMenu.Items.Add(Result);
  end;
//------------------------------------------------------------------

begin
  inherited Create(AnOwner);
  //....
    self.Width:=200;
    self.Height:=300;
    self.Caption:='';
    self.BevelOuter:=bvNone;

    fShowEventsPage:=false;

    MainPopupMenu:= TPopupMenu.Create(self);
    MainPopupMenu.Parent:=self;
    MainPopupMenu.OnPopup := @OnMainPopupMenuPopup ;

  //....
  CreateSelfPropEditorHook; //create fSelfDesignHook
  FPropertyEditorHook:=fSelfDesignHook;

  FInSelection := False;
  FSelections:=TPersistentSelectionList.Create;
  FAutoShow := True;
  FDefaultItemHeight := 22;

  FComponentEditor := nil;

  Caption := oisObjectInspector;

  MainPopupMenu.Images := IDEImages.Images_16;

  AddPopupMenuItem(SetDefaultPopupmenuItem,nil,'SetDefaultPopupMenuItem',
     'Set to Default value','Set property value to Default', '',
     @OnSetDefaultPopupmenuItemClick,false,true,true);

  AddPopupMenuItem(UndoPropertyPopupMenuItem,nil,'UndoPropertyPopupMenuItem',
     oisUndo,'Set property value to last valid value', '',
     @OnUndoPopupmenuItemClick,false,true,true);

  AddPopupMenuItem(FindDeclarationPopupmenuItem,nil,'FindDeclarationPopupmenuItem',
     oisFinddeclaration,'Jump to declaration of property', '',
     @OnFindDeclarationPopupmenuItemClick,false,true,false);

  OptionsSeparatorMenuItem := AddSeparatorMenuItem(nil, 'OptionsSeparatorMenuItem', true);
  AddPopupMenuItem(CutPopupMenuItem,nil,'CutPopupMenuItem',
     oisCutComponents,'Cut selected item', '',
     @OnCutPopupmenuItemClick,false,true,true);

  AddPopupMenuItem(CopyPopupMenuItem,nil,'CopyPopupMenuItem',
     oisCopyComponents,'Copy selected item', '',
     @OnCopyPopupmenuItemClick,false,true,true);

  AddPopupMenuItem(PastePopupMenuItem,nil,'PastePopupMenuItem',
     oisPasteComponents,'Paste selected item', '',
     @OnPastePopupmenuItemClick,false,true,true);

  AddPopupMenuItem(DeletePopupMenuItem,nil,'DeletePopupMenuItem',
     oisDeleteComponents,'Delete selected item', '',
     @OnDeletePopupmenuItemClick,false,true,true);

  OptionsSeparatorMenuItem2 := AddSeparatorMenuItem(nil, 'OptionsSeparatorMenuItem2', true);
  AddPopupMenuItem(ShowHintsPopupMenuItem,nil
     ,'ShowHintPopupMenuItem',oisShowHints,'Grid hints', ''
     ,@OnShowHintPopupMenuItemClick,false,true,true);

  CreateNoteBook;
end;

destructor TplObjectInspector.Destroy;
begin
  DestroySelfPropEditorHook;
  FreeAndNil(FSelections);
  FreeAndNil(FComponentEditor);
  inherited Destroy;

end;

procedure TplObjectInspector.AddPersistent(APersistent: TPersistent; const mClear:Boolean=true);
 begin
   if mClear then Selections.Clear;

 // DaThoX begin
   if APersistent <> nil then
 // DaThoX end
     fSelections.Add(APersistent);

   RefreshSelections;
 end;

procedure TplObjectInspector.SetPropertyEditorHook(NewValue:TPropertyEditorHook);
var
  Page: TplObjectInspectorPage;
begin
  if FPropertyEditorHook=NewValue then exit;

  if FPropertyEditorHook=fSelfDesignHook then  exit;


  if FPropertyEditorHook<>nil then
  begin
    FPropertyEditorHook.RemoveAllHandlersForObject(Self);
  end;

  FPropertyEditorHook:=NewValue;

  if FPropertyEditorHook<>nil then
  begin
    FPropertyEditorHook.AddHandlerChangeLookupRoot(@HookLookupRootChange);
    FPropertyEditorHook.AddHandlerRefreshPropertyValues(
                                                @HookRefreshPropertyValues);
    FPropertyEditorHook.AddHandlerGetSelection(@HookGetSelection);
    FPropertyEditorHook.AddHandlerSetSelection(@HookSetSelection);
    // select root component
    FSelections.Clear;
    if (FPropertyEditorHook<>nil) and (FPropertyEditorHook.LookupRoot<>nil)
    and (FPropertyEditorHook.LookupRoot is TComponent) then
      FSelections.Add(TComponent(FPropertyEditorHook.LookupRoot));

    for Page:=Low(TplObjectInspectorPage) to High(TplObjectInspectorPage) do
      if GridControl[Page]<>nil then
        GridControl[Page].PropertyEditorHook:=FPropertyEditorHook;

    RefreshSelections;
  end;
end;

function TplObjectInspector.PersistentToString(APersistent: TPersistent): string;
begin
  if APersistent is TComponent then
    Result:=TComponent(APersistent).GetNamePath+': '+APersistent.ClassName
  else
    Result:=APersistent.ClassName;
end;

procedure TplObjectInspector.SetDefaultItemHeight(const AValue: integer);
var
  NewValue: Integer;
  Page: TplObjectInspectorPage;
begin
  NewValue:=AValue;
  if NewValue<0 then
    NewValue:=0
  else if NewValue=0 then
    NewValue:=22
  else if (NewValue>0) and (NewValue<10) then
    NewValue:=10
  else if NewValue>100 then NewValue:=100;
  if FDefaultItemHeight=NewValue then exit;
  FDefaultItemHeight:=NewValue;
  for Page:=Low(TplObjectInspectorPage) to High(TplObjectInspectorPage) do
    if GridControl[Page]<>nil then
      GridControl[Page].DefaultItemHeight:=FDefaultItemHeight;
  RebuildPropertyLists;
end;


procedure TplObjectInspector.SetOnShowOptions(const AValue: TNotifyEvent);
begin
  if FOnShowOptions=AValue then exit;
  FOnShowOptions:=AValue;
  ShowOptionsPopupMenuItem.Visible:=FOnShowOptions<>nil;
end;

procedure TplObjectInspector.AddPersistentToList(APersistent: TPersistent; List: TStrings);
var
  Allowed: boolean;
begin
  if (APersistent is TComponent)
  and (csDestroying in TComponent(APersistent).ComponentState) then exit;
  Allowed:=true;
  if Assigned(FOnAddAvailablePersistent) then
    FOnAddAvailablePersistent(APersistent,Allowed);
  if Allowed then
    List.AddObject(PersistentToString(APersistent),APersistent);
end;

procedure TplObjectInspector.HookLookupRootChange;
var
  Page: TplObjectInspectorPage;
begin
  for Page:=Low(TplObjectInspectorPage) to High(TplObjectInspectorPage) do
    if GridControl[Page]<>nil then
      GridControl[Page].PropEditLookupRootChange;

end;


procedure TplObjectInspector.BeginUpdate;
begin
  inc(FUpdateLock);
end;

procedure TplObjectInspector.EndUpdate;
begin
  dec(FUpdateLock);
  if FUpdateLock<0 then begin
    DebugLn('ERROR TplObjectInspector.EndUpdate');
  end;
  if FUpdateLock=0 then begin
    if oifRebuildPropListsNeeded in FFLags then
      RebuildPropertyLists;
  end;
end;

function TplObjectInspector.GetActivePropertyGrid: TplOICustomPropertyGrid;
begin
  Result:=nil;
  if NoteBook=nil then exit;

  case NoteBook.PageIndex of
  0: Result:=PropertyGrid;
  1: Result:=EventGrid;
  end;
end;

function TplObjectInspector.GetActivePropertyRow: TplOIPropertyGridRow;
var
  CurGrid: TplOICustomPropertyGrid;
begin
  Result:=nil;
  CurGrid:=GetActivePropertyGrid;
  if CurGrid=nil then exit;
  Result:=CurGrid.GetActiveRow;
end;

function TplObjectInspector.GetCurRowDefaultValue(var DefaultStr: string): boolean;
var
  CurRow: TplOIPropertyGridRow;
begin
  Result:=false;
  DefaultStr:='';
  CurRow:=GetActivePropertyRow;
  if (CurRow=nil) or (not (paHasDefaultValue in CurRow.Editor.GetAttributes))
  then exit;
  try
    DefaultStr:=CurRow.Editor.GetDefaultValue;
    Result:=true;
  except
    DefaultStr:='';
  end;
end;

procedure TplObjectInspector.SetSelections(const ASelection: TPersistentSelectionList);
begin
  if (not Assigned(ASelection)) then  exit;
  if FInSelection and FSelections.IsEqual(ASelection) then  exit; // prevent endless loops
  if (not ASelection.ForceUpdate) and FSelections.IsEqual(ASelection) then  exit; // nothing changed

  FInSelection := True;
  try
    FSelections.Assign(ASelection);
    RefreshSelections;
    if Assigned(FOnSelectPersistentsInOI) then  FOnSelectPersistentsInOI(Self);
  finally
    FInSelection := False;
  end;
end;

procedure TplObjectInspector.RefreshSelections;
var
  Page: TplObjectInspectorPage;
begin
  for Page := Low(TplObjectInspectorPage) to High(TplObjectInspectorPage) do
    if GridControl[Page] <> nil then
      GridControl[Page].Selection := FSelections;

  if (not Visible) and AutoShow and (FSelections.Count > 0) then
    if Assigned(OnAutoShow) then
      OnAutoShow(Self)
    else
      Visible := True;
end;

procedure TplObjectInspector.SaveChanges;
var
  Page: TplObjectInspectorPage;
begin
  for Page:=Low(TplObjectInspectorPage) to High(TplObjectInspectorPage) do
    if GridControl[Page]<>nil then
      GridControl[Page].SaveChanges;
end;

procedure TplObjectInspector.RefreshPropertyValues;
var
  Page: TplObjectInspectorPage;
begin
  for Page:=Low(TplObjectInspectorPage) to High(TplObjectInspectorPage) do
    if GridControl[Page]<>nil then
      GridControl[Page].RefreshPropertyValues;
end;

procedure TplObjectInspector.RebuildPropertyLists;
var
  Page: TplObjectInspectorPage;
begin
  if FUpdateLock>0 then
    Include(FFLags,oifRebuildPropListsNeeded)
  else begin
    Exclude(FFLags,oifRebuildPropListsNeeded);
    for Page:=Low(TplObjectInspectorPage) to High(TplObjectInspectorPage) do
      if GridControl[Page]<>nil then
        GridControl[Page].BuildPropertyList;
  end;
end;


function TplObjectInspector.GetComponentEditorForSelection: TBaseComponentEditor;
var
  APersistent: TPersistent;
  AComponent: TComponent absolute APersistent;
  ADesigner: TIDesigner;
begin
  APersistent := GetSelected;
  if not (APersistent is TComponent) then
    Exit(nil);
  ADesigner := FindRootDesigner(AComponent);
  if not (ADesigner is TComponentEditorDesigner) then
    Exit(nil);
  Result := GetComponentEditor(AComponent, TComponentEditorDesigner(ADesigner));
end;

procedure TplObjectInspector.OnGridKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  Handled: Boolean;
begin
  Handled := false;

  //CTRL-[Shift]-TAB will select next or previous notebook tab
  if Key=VK_TAB then
  begin
    if Shift = [ssCtrl] then
    begin
      Handled := true;
      ShowNextPage(1);
    end else if Shift = [ssCtrl, ssShift] then
    begin
      Handled := true;
      ShowNextPage(-1);
    end;
  end;

  if not Handled then
  begin
    //CTRL-ArrowDown will dropdown the component combobox
    if (Key=VK_DOWN) and (ssCtrl in Shift) then
    begin
      Handled := true;

    end;
  end;

  if not Handled then
  begin
    if Assigned(OnOIKeyDown) then
      OnOIKeyDown(Self,Key,Shift);
    if (Key<>VK_UNKNOWN) and Assigned(OnRemainingKeyDown) then
      OnRemainingKeyDown(Self,Key,Shift);
  end
  else
    Key := VK_UNKNOWN;
end;

procedure TplObjectInspector.OnGridKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Assigned(OnRemainingKeyUp) then OnRemainingKeyUp(Self,Key,Shift);
end;

procedure TplObjectInspector.OnGridDblClick(Sender: TObject);
begin
  //...
end;

procedure TplObjectInspector.OnSetDefaultPopupmenuItemClick(Sender: TObject);
var
  CurGrid: TplOICustomPropertyGrid;
  DefaultStr: string;
begin
  if not GetCurRowDefaultValue(DefaultStr) then exit;
  CurGrid:=GetActivePropertyGrid;
  if CurGrid=nil then exit;
  CurGrid.SetCurrentRowValue(DefaultStr);
  RefreshPropertyValues;
end;

procedure TplObjectInspector.OnUndoPopupmenuItemClick(Sender: TObject);
var
  CurGrid: TplOICustomPropertyGrid;
  CurRow: TplOIPropertyGridRow;
begin
  CurGrid:=GetActivePropertyGrid;
  CurRow:=GetActivePropertyRow;
  if CurRow=nil then exit;
  CurGrid.CurrentEditValue:=CurRow.Editor.GetVisualValue;
end;

procedure TplObjectInspector.OnFindDeclarationPopupmenuItemClick(Sender: TObject);
begin
  if Assigned(OnFindDeclarationOfProperty) then
    OnFindDeclarationOfProperty(Self);
end;

procedure TplObjectInspector.OnCutPopupmenuItemClick(Sender: TObject);
var
  ADesigner: TIDesigner;
begin
  if (Selections.Count > 0) and (Selections[0] is TComponent) then
  begin
    ADesigner := FindRootDesigner(Selections[0]);
    if ADesigner is TComponentEditorDesigner then
      TComponentEditorDesigner(ADesigner).CutSelection;
  end;
end;

procedure TplObjectInspector.OnCopyPopupmenuItemClick(Sender: TObject);
var
  ADesigner: TIDesigner;
begin
  if (Selections.Count > 0) and (Selections[0] is TComponent) then
  begin
    ADesigner := FindRootDesigner(Selections[0]);
    if ADesigner is TComponentEditorDesigner then
      TComponentEditorDesigner(ADesigner).CopySelection;
  end;
end;

procedure TplObjectInspector.OnPastePopupmenuItemClick(Sender: TObject);
var
  ADesigner: TIDesigner;
begin
  if Selections.Count > 0 then
  begin
    ADesigner := FindRootDesigner(Selections[0]);
    if ADesigner is TComponentEditorDesigner then
      TComponentEditorDesigner(ADesigner).PasteSelection([]);
  end;
end;

procedure TplObjectInspector.OnDeletePopupmenuItemClick(Sender: TObject);
var
  ADesigner: TIDesigner;
  ACollection: TCollection;
  i: integer;
begin
  if (Selections.Count > 0) then
  begin
    ADesigner := FindRootDesigner(Selections[0]);
    if ADesigner is TComponentEditorDesigner then
    begin
      if Selections[0] is TCollection then
      begin
        ACollection := TCollection(Selections[0]);
        Selections.BeginUpdate;
        Selections.Clear;
        for i := 0 to ACollection.Count - 1 do
          Selections.Add(ACollection.Items[i]);
        Selections.EndUpdate;
        if Assigned(FOnSelectPersistentsInOI) then
          FOnSelectPersistentsInOI(Self);
      end;
      TComponentEditorDesigner(ADesigner).DeleteSelection;
    end;
  end;
end;

procedure TplObjectInspector.OnGridModified(Sender: TObject);
begin
  DoModified(Self);
end;

procedure TplObjectInspector.OnGridSelectionChange(Sender: TObject);
begin
  if Assigned(FOnSelectionChange) then OnSelectionChange(Self);
end;

function TplObjectInspector.OnGridPropertyHint(Sender: TObject;
  PointedRow: TplOIPropertyGridRow; ScreenPos: TPoint; aHintWindow: THintWindow;
  out HintWinRect: TRect; out AHint: string): boolean;
begin
  Result := False;
  if Assigned(FOnPropertyHint) then
    Result := FOnPropertyHint(Sender, PointedRow, ScreenPos, aHintWindow, HintWinRect, AHint);
end;

procedure TplObjectInspector.HookGetSelection(const ASelection: TPersistentSelectionList);
begin
  if ASelection=nil then exit;
  ASelection.Assign(FSelections);
end;

procedure TplObjectInspector.HookSetSelection(const ASelection: TPersistentSelectionList);
begin
  Selections:= ASelection;
end;

procedure TplObjectInspector.ShowNextPage(Delta: integer);
var
  NewPageIndex: Integer;
begin
  NewPageIndex := NoteBook.PageIndex;
  repeat
    NewPageIndex := NewPageIndex + Delta;
    if NewPageIndex >= NoteBook.PageCount then
      NewPageIndex := 0;
    if NewPageIndex < 0 then
      NewPageIndex := NoteBook.PageCount - 1;
    if NoteBook.Page[NewPageIndex].TabVisible then
    begin
      NoteBook.PageIndex := NewPageIndex;
      break;
    end;
  until NewPageIndex = NoteBook.PageIndex;
end;

procedure TplObjectInspector.DestroyNoteBook;
begin
  if NoteBook<>nil then NoteBook.Visible:=false;

  FreeAndNil(PropertyGrid);
  FreeAndNil(EventGrid);
  FreeAndNil(NoteBook);
end;

procedure TplObjectInspector.CreateNoteBook;

  function CreateGrid(
    ATypeFilter: TTypeKinds; AOIPage: TplObjectInspectorPage;
    ANotebookPage: Integer): TplOICustomPropertyGrid;
  begin
    Result:=TplOICustomPropertyGrid.CreateWithParams(
      Self, PropertyEditorHook, ATypeFilter, FDefaultItemHeight);
    with Result do
    begin
      Name := DefaultOIGridNames[AOIPage];
      Selection := Self.FSelections;
      Align := alClient;
      PopupMenu := MainPopupMenu;
      OnModified := @OnGridModified;
      OnSelectionChange := @OnGridSelectionChange;
      OnPropertyHint := @OnGridPropertyHint;
      OnOIKeyDown := @OnGridKeyDown;
      OnKeyUp := @OnGridKeyUp;
      OnDblClick := @OnGridDblClick;

      Parent := NoteBook.Page[ANotebookPage];
    end;
  end;

const
  PROPS = [
    tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat, tkSet,{ tkMethod,}
    tkSString, tkLString, tkAString, tkWString, tkVariant,
    {tkArray, tkRecord, tkInterface,} tkClass, tkObject, tkWChar, tkBool,
    tkInt64, tkQWord];

  function AddPage(PageName, TabCaption: string): TTabSheet;
  begin
    Result:=TTabSheet.Create(Self);
    Result.Name:=PageName;
    Result.Caption:=TabCaption;
    Result.Parent:=NoteBook;
  end;
var
  APage: TTabSheet;
begin
  DestroyNoteBook;

  // NoteBook
  NoteBook:=TPageControl.Create(Self);
  with NoteBook do
  begin
    Name := 'NoteBook';
    Parent := Self;
    Align := alClient;
    PopupMenu := MainPopupMenu;
  end;

  AddPage(DefaultOIPageNames[oipgpProperties],oisProperties);
  AddPage(DefaultOIPageNames[oipgpEvents],oisEvents);

  NoteBook.PageIndex:=0;

  PropertyGrid := CreateGrid(PROPS, oipgpProperties, 0);
  EventGrid := CreateGrid([tkMethod], oipgpEvents, 1);

  NoteBook.Page[1].TabVisible:=fShowEventsPage;
end;

procedure TplObjectInspector.KeyDown(var Key: Word; Shift: TShiftState);
var
  CurGrid: TplOICustomPropertyGrid;
begin
  CurGrid:=GetActivePropertyGrid;

  inherited KeyDown(Key, Shift);
  if (Key<>VK_UNKNOWN) and Assigned(OnRemainingKeyDown) then
    OnRemainingKeyDown(Self,Key,Shift);
end;

procedure TplObjectInspector.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited KeyUp(Key, Shift);
  if (Key<>VK_UNKNOWN) and Assigned(OnRemainingKeyUp) then
    OnRemainingKeyUp(Self,Key,Shift);
end;

procedure TplObjectInspector.DoModified(Sender: TObject);
begin
  if Assigned(FOnModified) then
    FOnModified(Sender)
end;

function TplObjectInspector.GetSelected: TPersistent;
begin
  if fSelections.Count=1 then
    Result := fSelections[0]
  else
    Result := nil;
end;

procedure TplObjectInspector.SetSelected(val:TPersistent);
 begin
   AddPersistent(val,true);
 end;

procedure TplObjectInspector.OnShowHintPopupMenuItemClick(Sender : TObject);
var
  Page: TplObjectInspectorPage;
begin
  for Page := Low(TplObjectInspectorPage) to High(TplObjectInspectorPage) do
    if GridControl[Page] <> nil then
      GridControl[Page].ShowHint := not GridControl[Page].ShowHint;
end;

procedure TplObjectInspector.OnShowOptionsPopupMenuItemClick(Sender: TObject);
begin
  if Assigned(FOnShowOptions) then FOnShowOptions(Sender);
end;

procedure TplObjectInspector.OnMainPopupMenuPopup(Sender: TObject);
var
  EditorVerbSeparator: TMenuItem;

  procedure RemoveComponentEditorMenuItems;
  var
    I: Integer;
  begin
    for I := MainPopupMenu.Items.Count - 1 downto 0 do
      if Pos('ComponentEditorVerbMenuItem', MainPopupMenu.Items[I].Name) = 1 then
        MainPopupMenu.Items[I].Free;
  end;

  procedure AddComponentEditorMenuItems;
  var
    I, VerbCount: Integer;
    Item: TMenuItem;
  begin
    VerbCount := ComponentEditor.GetVerbCount;
    for I := 0 to VerbCount - 1 do
    begin
      Item := NewItem(ComponentEditor.GetVerb(I), 0, False, True,
        @DoComponentEditorVerbMenuItemClick, 0, 'ComponentEditorVerbMenuItem' + IntToStr(i));
      ComponentEditor.PrepareItem(I, Item);
      MainPopupMenu.Items.Insert(I, Item);
    end;
    // insert the separator
    if VerbCount > 0 then
    begin
      EditorVerbSeparator := NewLine;
      EditorVerbSeparator.Name := 'ComponentEditorVerbMenuItem' + IntToStr(VerbCount);
      MainPopupMenu.Items.Insert(VerbCount, EditorVerbSeparator);
    end;
  end;

  procedure AddCollectionEditorMenuItems(ACollection: TCollection);
  var
    Item: TMenuItem;
  begin
    Item := NewItem(oisAddCollectionItem, 0, False, True,
      @DoCollectionAddItem, 0, 'ComponentEditorVerbMenuItem0');
    MainPopupMenu.Items.Insert(0, Item);
    EditorVerbSeparator := NewLine;
    EditorVerbSeparator.Name := 'ComponentEditorVerbMenuItem1';
    MainPopupMenu.Items.Insert(1, EditorVerbSeparator);
  end;

  procedure AddZOrderMenuItems;
  var
    ZItem, Item: TMenuItem;
  begin
    ZItem := NewSubMenu(oisZOrder, 0, 'ComponentEditorVerbMenuItemZOrder', [], True);
    Item := NewItem(oisOrderMoveToFront, 0, False, True, @DoZOrderItemClick, 0, '');

    Item.Tag := 0;
    ZItem.Add(Item);
    Item := NewItem(oisOrderMoveToBack, 0, False, True, @DoZOrderItemClick, 0, '');

    Item.Tag := 1;
    ZItem.Add(Item);
    Item := NewItem(oisOrderForwardOne, 0, False, True, @DoZOrderItemClick, 0, '');

    Item.Tag := 2;
    ZItem.Add(Item);
    Item := NewItem(oisOrderBackOne, 0, False, True, @DoZOrderItemClick, 0, '');

    Item.Tag := 3;
    ZItem.Add(Item);
    if EditorVerbSeparator <> nil then
      MainPopupMenu.Items.Insert(EditorVerbSeparator.MenuIndex + 1, ZItem)
    else
      MainPopupMenu.Items.Insert(0, ZItem);
    Item := NewLine;
    Item.Name := 'ComponentEditorVerbMenuItemZOrderSeparator';
    MainPopupMenu.Items.Insert(ZItem.MenuIndex + 1, Item);
  end;

var
  DefaultStr: String;
  CurGrid: TplOICustomPropertyGrid;
  CurRow: TplOIPropertyGridRow;
  Persistent: TPersistent;
begin
  RemoveComponentEditorMenuItems;
  ShowHintsPopupMenuItem.Checked := PropertyGrid.ShowHint;

    CutPopupMenuItem.Visible := False;
    CopyPopupMenuItem.Visible := False;
    PastePopupMenuItem.Visible := False;
    DeletePopupMenuItem.Visible := False;
    OptionsSeparatorMenuItem2.Visible := False;


  if (MainPopupMenu.PopupComponent is TplOICustomPropertyGrid) then
  begin
    SetDefaultPopupMenuItem.Visible := True;

    SetDefaultPopupMenuItem.Enabled := GetCurRowDefaultValue(DefaultStr);
    if SetDefaultPopupMenuItem.Enabled then
      SetDefaultPopupMenuItem.Caption := Format(oisSetToDefault, [DefaultStr])  ;
   // else
   //   SetDefaultPopupMenuItem.Caption := oisSetToDefaultValue;


    CurGrid := GetActivePropertyGrid;
    CurRow := GetActivePropertyRow;
    UndoPropertyPopupMenuItem.Visible := True;
    UndoPropertyPopupMenuItem.Enabled := (CurRow<>nil) and (CurRow.Editor.GetVisualValue <> CurGrid.CurrentEditValue);
    if CurRow=nil then
    begin
      FindDeclarationPopupmenuItem.Visible := False;
    end else
    begin
      FindDeclarationPopupmenuItem.Visible := true;
      FindDeclarationPopupmenuItem.Caption:=Format(oisJumpToDeclarationOf, [
        CurRow.Name]);
      FindDeclarationPopupmenuItem.Hint:=Format(oisJumpToDeclarationOf, [CurRow.
        Editor.GetPropertyPath(0)]);
    end;

    OptionsSeparatorMenuItem.Visible := True;
  end
  else
  begin
    SetDefaultPopupMenuItem.Visible := False;
    UndoPropertyPopupMenuItem.Visible := False;
    FindDeclarationPopupmenuItem.Visible := False;
    OptionsSeparatorMenuItem.Visible := False;
  end;

end;

procedure TplObjectInspector.DoComponentEditorVerbMenuItemClick(Sender: TObject);
var
  Verb: integer;
  AMenuItem: TMenuItem;
begin
  if Sender is TMenuItem then
    AMenuItem := TMenuItem(Sender)
  else
    Exit;
  // component menu items start from the start of menu
  Verb := AMenuItem.MenuIndex;
  ComponentEditor.ExecuteVerb(Verb);
end;

procedure TplObjectInspector.DoCollectionAddItem(Sender: TObject);
var
  Persistent: TPersistent;
  Collection: TCollection absolute Persistent;
begin
  Persistent := GetSelected;

  if Persistent = nil then  Exit;

  if Persistent is TCollectionItem then
    Persistent := TCollectionItem(Persistent).Collection;

  if not (Persistent is TCollection) then Exit;

  Collection.Add;
  DoModified(Self);
  Selections.ForceUpdate := True;

  try
    SetSelections(Selections);
  finally
    Selections.ForceUpdate := False;
  end;
end;

procedure TplObjectInspector.DoZOrderItemClick(Sender: TObject);
var
  Control: TControl;
begin
  if not (Sender is TMenuItem) then Exit;
  if (Selections.Count <> 1) or not (Selections[0] is TControl) then Exit;

  Control := TControl(Selections[0]);

  if Control.Parent = nil then Exit;

  case TMenuItem(Sender).Tag of
    0: Control.BringToFront;
    1: Control.SendToBack;
    2: Control.Parent.SetControlIndex(Control, Control.Parent.GetControlIndex(Control) + 1);
    3: Control.Parent.SetControlIndex(Control, Control.Parent.GetControlIndex(Control) - 1);
  end;
  DoModified(Self);
end;


procedure TplObjectInspector.HookRefreshPropertyValues;
begin
  RefreshPropertyValues;
end;

procedure TplObjectInspector.ActivateGrid(Grid: TplOICustomPropertyGrid);
begin
  if Grid=PropertyGrid then NoteBook.PageIndex:=0
  else if Grid=EventGrid then NoteBook.PageIndex:=1;
end;

procedure TplObjectInspector.FocusGrid(Grid: TplOICustomPropertyGrid);
var
  Index: Integer;
begin
  if Grid=nil then
    Grid := GetActivePropertyGrid
  else
    ActivateGrid(Grid);
  if Grid <> nil then
  begin
    Index := Grid.ItemIndex;
    if Index < 0 then
      Index := 0;
    Grid.SetItemIndexAndFocus(Index);
  end;
end;

function TplObjectInspector.GetGridControl(Page: TplObjectInspectorPage): TplOICustomPropertyGrid;
begin
  case Page of
  oipgpEvents: Result:=EventGrid;
  else  Result:=PropertyGrid;
  end;
end;

procedure TplObjectInspector.SetComponentEditor(const AValue: TBaseComponentEditor);
begin
  if FComponentEditor <> AValue then
  begin
    FComponentEditor.Free;
    FComponentEditor := AValue;
  end;
end;

procedure TplObjectInspector.SetShowEventsPage(val:boolean);
 begin
   if val=fShowEventsPage then exit;

   fShowEventsPage:=val;
   NoteBook.Page[1].TabVisible:=fShowEventsPage;

 end;

procedure TplObjectInspector.CreateSelfPropEditorHook;
begin
  fSelfDesignHook:=TPropertyEditorHook.Create;

  {
   fSelfDesignHook.AddHandlerGetMethods(@OnPropHookGetMethods);
   fSelfDesignHook.GetPrivateDirectory:=AppendPathDelim(GetPrimaryConfigPath);
   fSelfDesignHook.AddHandlerGetMethodName(@OnPropHookGetMethodName);
   fSelfDesignHook.AddHandlerGetMethods(@OnPropHookGetMethods);
   fSelfDesignHook.AddHandlerMethodExists(@OnPropHookMethodExists);
   fSelfDesignHook.AddHandlerCreateMethod(@OnPropHookCreateMethod);
   fSelfDesignHook.AddHandlerShowMethod(@OnPropHookShowMethod);
   fSelfDesignHook.AddHandlerRenameMethod(@OnPropHookRenameMethod);
   fSelfDesignHook.AddHandlerBeforeAddPersistent(@OnPropHookBeforeAddPersistent);
   fSelfDesignHook.AddHandlerComponentRenamed(@OnPropHookComponentRenamed);
   fSelfDesignHook.AddHandlerModified(@OnPropHookModified);
   fSelfDesignHook.AddHandlerPersistentAdded(@OnPropHookPersistentAdded);
   fSelfDesignHook.AddHandlerPersistentDeleting(@OnPropHookPersistentDeleting);
   fSelfDesignHook.AddHandlerDeletePersistent(@OnPropHookDeletePersistent);
   fSelfDesignHook.AddHandlerObjectPropertyChanged(@OnPropHookObjectPropertyChanged);
   fSelfDesignHook.AddHandlerGetComponentNames(@OnPropHookGetComponentNames);
   fSelfDesignHook.AddHandlerGetComponent(@OnPropHookGetComponent);
  }

end;

procedure TplObjectInspector.DestroySelfPropEditorHook;
begin
  if fSelfDesignHook<>nil then
      fSelfDesignHook.Free;

end;

end.

