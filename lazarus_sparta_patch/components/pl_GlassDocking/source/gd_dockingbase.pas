
{**********************************************************************
 Package pl_GlassDocking.pkg
 is a modification of AnchorDocking Library  (http://wiki.lazarus.freepascal.org/Anchor_Docking)
 for CodeTyphon Studio
 This unit is part of CodeTyphon Project (http://www.pilotlogic.com/)
***********************************************************************}


unit gd_dockingbase;

{$mode objfpc}{$H+}

{ $DEFINE VerboseAnchorDockRestore}
{ $DEFINE VerboseADCustomSite}
{ $DEFINE VerboseAnchorDockPages}

interface

uses
  Math, Classes, SysUtils, LResources, types, LCLType, LCLIntf, LCLProc,
  Controls, Forms, ExtCtrls, ComCtrls, Graphics, Themes, Menus, Buttons,
  LazFileCache,
  BCPanel, IDEWindowIntf, BCTypes, BGRABitmapTypes, // DaThoX
  LazConfigStorage, gd_dockingstr, gd_dockingstorage;

const
  cnHeaderColorDefault = $00BBFFFF;

type

  TGlassDockHostSiteType = (
    adhstNone,              // fresh created, no control docked
    adhstOneControl,        // a control and the "Header" (TGlassDockHeader)
    adhstLayout,            // several controls/TGlassDockHostSite separated by TGlassDockSplitters
    adhstPages              // the "Pages" (TGlassDockPageControl) with several pages
    );

  TADMResizePolicy = (
    admrpNone,
    admrpChild  // resize child
    );

  TGlassDockHostSite = class;

  TGlassDockCloseButton = class(TCustomSpeedButton)
  protected
    procedure GetCloseGlyph; // increase reference count
    procedure ReleaseCloseGlyph; // decrease reference count
    function GetGlyphSize({%H-}Drawing: boolean; PaintRect: TRect): TSize; override;
    function DrawGlyph(ACanvas: TCanvas; const AClient: TRect; const AOffset: TPoint; AState: TButtonState;
      ATransparent: boolean; BiDiFlags: longint): TRect; override;
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer; WithThemeSpace: boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TGlassDockHeader = class(TBCPanel) // DaThoX
  private
    FCloseButton: TCustomSpeedButton;
    FHeaderPosition: TADLHeaderPosition;
    // DaThoX begin
    FCaption: TCaption;
    class var FSelectedHeader: TGlassDockHeader;
    // DaThoX end
    procedure CloseButtonClick(Sender: TObject);
    procedure HeaderPositionItemClick(Sender: TObject);
    procedure UndockButtonClick(Sender: TObject);
    procedure MergeButtonClick(Sender: TObject);
    procedure EnlargeSideClick(Sender: TObject);
    procedure SetHeaderPosition(const AValue: TADLHeaderPosition);
  // DaThoX begin
    class procedure SetSelectedHeader(const AVal: TGlassDockHeader); static;
  public
    class property SelectedHeader: TGlassDockHeader read FSelectedHeader write SetSelectedHeader;
  // DaThoX end
  protected
    procedure Paint; override;
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer; WithThemeSpace: boolean); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure UpdateHeaderControls;
    procedure SetAlign(Value: TAlign); override;
    procedure DoOnShowHint(HintInfo: PHintInfo); override;
    procedure PopupMenuPopup(Sender: TObject); virtual;
  public
    constructor Create(TheOwner: TComponent); override;
    property CloseButton: TCustomSpeedButton read FCloseButton;
    property HeaderPosition: TADLHeaderPosition read FHeaderPosition write SetHeaderPosition;
    property BevelOuter default bvNone;
  // DaThoX begin
  published
    property Caption read FCaption write FCaption;
  // DaThoX end
  end;

  TGlassDockHeaderClass = class of TGlassDockHeader;

  TGlassDockSplitter = class(TCustomSplitter)
  private
    FDockBounds: TRect;
    FDockParentClientSize: TSize;
    FDockRestoreBounds: TRect;
  protected
    procedure SetResizeAnchor(const AValue: TAnchorKind); override;
    procedure PopupMenuPopup(Sender: TObject); virtual;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure UpdateDockBounds;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: integer); override; // any normal movement sets the DockBounds
    procedure SetBoundsKeepDockBounds(ALeft, ATop, AWidth, AHeight: integer); // movement for scaling keeps the DockBounds
    function SideAnchoredControlCount(Side: TAnchorKind): integer;
    function HasAnchoredControls: boolean;
    procedure SaveLayout(LayoutNode: TGlassDockLayoutTreeNode);
    function HasOnlyOneSibling(Side: TAnchorKind; MinPos, MaxPos: integer): TControl;
    property DockRestoreBounds: TRect read FDockRestoreBounds write FDockRestoreBounds;
    property DockBounds: TRect read FDockBounds write FDockBounds;
    property DockParentClientSize: TSize read FDockParentClientSize write FDockParentClientSize;
  end;

  TGlassDockSplitterClass = class of TGlassDockSplitter;

  TGlassDockPageControl = class;
  TGlassDockPage = class(TCustomPage)
  public
    procedure UpdateDockCaption(Exclude: TControl = nil); override;
    procedure InsertControl(AControl: TControl; Index: integer); override;
    procedure RemoveControl(AControl: TControl); override;
    function GetSite: TGlassDockHostSite;
  end;

  TGlassDockPageClass = class of TGlassDockPage;


  TGlassDockPageControl = class(TCustomTabControl)
  private
    function GetDockPages(Index: integer): TGlassDockPage;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure PopupMenuPopup(Sender: TObject); virtual;
    procedure CloseButtonClick(Sender: TObject); virtual;
    procedure MoveLeftButtonClick(Sender: TObject); virtual;
    procedure MoveLeftMostButtonClick(Sender: TObject); virtual;
    procedure MoveRightButtonClick(Sender: TObject); virtual;
    procedure MoveRightMostButtonClick(Sender: TObject); virtual;
    procedure TabPositionClick(Sender: TObject); virtual;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure UpdateDockCaption(Exclude: TControl = nil); override;
    procedure RemoveControl(AControl: TControl); override;
    function GetActiveSite: TGlassDockHostSite;
    property DockPages[Index: integer]: TGlassDockPage read GetDockPages;
  end;

  TGlassDockPageControlClass = class of TGlassDockPageControl;

  TGlassDockHostSite = class(TCustomForm)
  private
    FCloseSite: boolean; // DaThoX
    FDockRestoreBounds: TRect;
    FHeader: TGlassDockHeader;
    FHeaderSide: TAnchorKind;
    FPages: TGlassDockPageControl;
    FSiteType: TGlassDockHostSiteType;
    FBoundSplitter: TGlassDockSplitter;
    fUpdateLayout: integer;
    procedure SetHeaderSide(const AValue: TAnchorKind);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function DoDockClientMsg(DragDockObject: TDragDockObject; aPosition: TPoint): boolean; override;
    function ExecuteDock(NewControl, DropOnControl: TControl; DockAlign: TAlign): boolean; virtual;
    function DockFirstControl(NewControl: TControl): boolean; virtual;
    function DockSecondControl(NewControl: TControl; DockAlign: TAlign; Inside: boolean): boolean; virtual;
    function DockAnotherControl(Sibling, NewControl: TControl; DockAlign: TAlign; Inside: boolean): boolean; virtual;
    procedure CreatePages; virtual;
    function DockSecondPage(NewControl: TControl): boolean; virtual;
    function DockAnotherPage(NewControl: TControl; InFrontOf: TControl): boolean; virtual;
    procedure AddCleanControl(AControl: TControl; TheAlign: TAlign = alNone);
    procedure RemoveControlFromLayout(AControl: TControl);
    procedure RemoveSpiralSplitter(AControl: TControl);
    procedure ClearChildControlAnchorSides(AControl: TControl);
    procedure Simplify;
    procedure SimplifyPages;
    procedure SimplifyOneControl;
    function GetOneControl: TControl;
    function GetSiteCount: integer;
    function IsOneSiteLayout(out Site: TGlassDockHostSite): boolean;
    function IsTwoSiteLayout(out Site1, Site2: TGlassDockHostSite): boolean;
    function GetUniqueSplitterName: string;
    function MakeSite(AControl: TControl): TGlassDockHostSite;
    procedure MoveAllControls(dx, dy: integer);
    procedure AlignControls(AControl: TControl; var ARect: TRect); override;
    function CheckIfOneControlHidden: boolean;
    procedure DoDock(NewDockSite: TWinControl; var ARect: TRect); override;
    procedure SetParent(NewParent: TWinControl); override;
    function HeaderNeedsShowing: boolean;
    // DaThoX begin
    procedure DoShow; override;
    procedure DoHide; override;
    // DaThoX end
    procedure DoClose(var CloseAction: TCloseAction); override;
    function CanUndock: boolean;
    procedure Undock;
    function CanMerge: boolean;
    procedure Merge;
    function EnlargeSide(Side: TAnchorKind; OnlyCheckIfPossible: boolean): boolean;
    function EnlargeSideResizeTwoSplitters(ShrinkSplitterSide, EnlargeSpitterSide: TAnchorKind; OnlyCheckIfPossible: boolean): boolean;
    function EnlargeSideRotateSplitter(Side: TAnchorKind; OnlyCheckIfPossible: boolean): boolean;
    procedure CreateBoundSplitter;
    procedure PositionBoundSplitter;
  public
    constructor CreateNew(AOwner: TComponent; Num: integer = 0); override;
    destructor Destroy; override;
    function  CloseQuery: boolean; override;
    function  CloseSite: boolean; virtual;
    procedure RemoveControl(AControl: TControl); override;
    procedure InsertControl(AControl: TControl; Index: integer); override;
    procedure GetSiteInfo(Client: TControl; var InfluenceRect: TRect; MousePos: TPoint; var CanDock: boolean); override;
    function  GetPageArea: TRect;
    procedure ChangeBounds(ALeft, ATop, AWidth, AHeight: integer; KeepBase: boolean); override;
    procedure UpdateDockCaption(Exclude: TControl = nil); override;
    procedure UpdateHeaderAlign;
    procedure UpdateHeaderShowing;
    procedure BeginUpdateLayout;
    procedure EndUpdateLayout;
    function  UpdatingLayout: boolean;
    procedure SaveLayout(LayoutTree: TGlassDockLayoutTree; LayoutNode: TGlassDockLayoutTreeNode);

    property DockRestoreBounds: TRect read FDockRestoreBounds write FDockRestoreBounds;
    property HeaderSide: TAnchorKind read FHeaderSide write SetHeaderSide;
    property Header: TGlassDockHeader read FHeader;
    property Pages: TGlassDockPageControl read FPages;
    property SiteType: TGlassDockHostSiteType read FSiteType;
    property BoundSplitter: TGlassDockSplitter read FBoundSplitter;
  end;

  TGlassDockHostSiteClass = class of TGlassDockHostSite;

  TGlassDockManager = class(TDockManager)
  private
    FDockableSites: TAnchors;
    FDockSite: TGlassDockHostSite;
    FInsideDockingAllowed: boolean;
    FPreferredSiteSizeAsSiteMinimum: boolean;
    FResizePolicy: TADMResizePolicy;
    FStoredConstraints: TRect;
    FSite: TWinControl;
    FSiteClientRect: TRect;
    procedure SetPreferredSiteSizeAsSiteMinimum(const AValue: boolean);
  public
    constructor Create(ADockSite: TWinControl); override;
    procedure GetControlBounds(Control: TControl; out AControlBounds: TRect); override;
    procedure InsertControl(Control: TControl; InsertAt: TAlign; DropCtl: TControl); override; overload;
    procedure InsertControl(ADockObject: TDragDockObject); override; overload;
    procedure LoadFromStream(Stream: TStream); override;
    procedure PositionDockRect(Client, DropCtl: TControl; DropAlign: TAlign; var DockRect: TRect); override; overload;
    procedure RemoveControl(Control: TControl); override;
    procedure ResetBounds(Force: boolean); override;
    procedure SaveToStream(Stream: TStream); override;
    function GetDockEdge(ADockObject: TDragDockObject): boolean; override;
    procedure RestoreSite(SplitterPos: integer);
    procedure StoreConstraints;
    function GetSitePreferredClientSize: TPoint;

    property Site: TWinControl read FSite; // the associated TControl (a TGlassDockHostSite or a custom dock site)
    property DockSite: TGlassDockHostSite read FDockSite; // if Site is a TGlassDockHostSite, this is it
    property DockableSites: TAnchors read FDockableSites write FDockableSites; // at which sides can be docked
    property InsideDockingAllowed: boolean read FInsideDockingAllowed write FInsideDockingAllowed;
    // if true allow to put a site into the custom dock site
    function GetChildSite: TWinControl; override; // DaThoX // get first child TGlassDockHostSite
    property ResizePolicy: TADMResizePolicy read FResizePolicy write FResizePolicy;
    property StoredConstraints: TRect read FStoredConstraints write FStoredConstraints;
    function StoredConstraintsValid: boolean;
    property PreferredSiteSizeAsSiteMinimum: boolean read FPreferredSiteSizeAsSiteMinimum write SetPreferredSiteSizeAsSiteMinimum;
  end;

  TGlassDockManagerClass = class of TGlassDockManager;


type
  TGlassDockSettings = class
  private
    FAllowDragging: boolean;
    FChangeStamp: integer;
    FDockOutsideMargin: integer;
    FDockParentMargin: integer;
    FDragTreshold: integer;
    FHeaderAlignLeft: integer;
    FHeaderAlignTop: integer;
    FHeaderButtonSize: integer;
    FHeaderHint: string;
    FHeaderColor: TColor;
    FHideHeaderCaptionFloatingControl: boolean;
    FPageAreaInPercent: integer;
    FSaveOnClose: boolean;
    FScaleOnResize: boolean;
    FShowHeader: boolean;
    FShowHeaderCaption: boolean;
    FSplitterWidth: integer;
    procedure SetAllowDragging(AValue: boolean);
    procedure SetDockOutsideMargin(AValue: integer);
    procedure SetDockParentMargin(AValue: integer);
    procedure SetDragTreshold(AValue: integer);
    procedure SetHeaderAlignLeft(AValue: integer);
    procedure SetHeaderAlignTop(AValue: integer);
    procedure SetHeaderButtonSize(AValue: integer);
    procedure SetHeaderHint(AValue: string);
    procedure SetHeaderColor(AValue: TColor);
    procedure SetHideHeaderCaptionFloatingControl(AValue: boolean);
    procedure SetPageAreaInPercent(AValue: integer);
    procedure SetSaveOnClose(AValue: boolean);
    procedure SetScaleOnResize(AValue: boolean);
    procedure SetShowHeader(AValue: boolean);
    procedure SetShowHeaderCaption(AValue: boolean);
    procedure SetSplitterWidth(AValue: integer);
  public
    procedure IncreaseChangeStamp; inline;
    procedure LoadFromConfig(Config: TConfigStorage);
    procedure SaveToConfig(Config: TConfigStorage);
    procedure ResetToDefault;
    function  IsEqual(Settings: TGlassDockSettings): boolean; reintroduce;
    property ChangeStamp: integer read FChangeStamp;
    property DragTreshold: integer read FDragTreshold write SetDragTreshold;
    property DockOutsideMargin: integer read FDockOutsideMargin write SetDockOutsideMargin;
    property DockParentMargin: integer read FDockParentMargin write SetDockParentMargin;
    property PageAreaInPercent: integer read FPageAreaInPercent write SetPageAreaInPercent;
    property HeaderAlignTop: integer read FHeaderAlignTop write SetHeaderAlignTop;
    property HeaderAlignLeft: integer read FHeaderAlignLeft write SetHeaderAlignLeft;
    property HeaderHint: string read FHeaderHint write SetHeaderHint;
    property SplitterWidth: integer read FSplitterWidth write SetSplitterWidth;
    property ScaleOnResize: boolean read FScaleOnResize write SetScaleOnResize;
    property SaveOnClose: boolean read FSaveOnClose write SetSaveOnClose;
    property ShowHeader: boolean read FShowHeader write SetShowHeader;
    property ShowHeaderCaption: boolean read FShowHeaderCaption write SetShowHeaderCaption;
    property HideHeaderCaptionFloatingControl: boolean read FHideHeaderCaptionFloatingControl write SetHideHeaderCaptionFloatingControl;
    property AllowDragging: boolean read FAllowDragging write SetAllowDragging;
    property HeaderButtonSize: integer read FHeaderButtonSize write SetHeaderButtonSize;
    property HeaderColor: TColor read FHeaderColor write SetHeaderColor;
  end;

  TGlassDockMaster = class;

  TADCreateControlEvent = procedure(Sender: TObject; aName: string; var AControl: TControl; DoDisableAutoSizing: boolean) of object;
  TADShowDockMasterOptionsEvent = function(aDockMaster: TGlassDockMaster): TModalResult;

  TGlassDockMaster = class(TComponent)
  private
    FAllowDragging: boolean;
    FControls: TFPList;  // list of TControl, custom host sites and docked controls, not helper controls
    FDockOutsideMargin: integer;
    FDockParentMargin: integer;
    FDragTreshold: integer;
    FHeaderAlignLeft: integer;
    FHeaderAlignTop: integer;
    FHeaderButtonSize: integer;
    FHeaderClass: TGlassDockHeaderClass;
    FHeaderHint: string;
    FHeaderColor: TColor;
    FIdleConnected: boolean;
    FManagerClass: TGlassDockManagerClass;
    FOnCreateControl: TADCreateControlEvent;
    FOnOptionsChanged: TNotifyEvent;
    FOnShowOptions: TADShowDockMasterOptionsEvent;
    FOptionsChangeStamp: int64;
    FPageAreaInPercent: integer;
    FPageClass: TGlassDockPageClass;
    FPageControlClass: TGlassDockPageControlClass;
    FQueueSimplify: boolean;
    FRestoreLayouts: TGlassDockRestoreLayouts;
    FRestoring: boolean;
    FSaveOnClose: boolean;
    FScaleOnResize: boolean;
    FShowHeader: boolean;
    FShowHeaderCaption: boolean;
    FHideHeaderCaptionFloatingControl: boolean;
    FShowMenuItemShowHeader: boolean;
    FSiteClass: TGlassDockHostSiteClass;
    FSplitterClass: TGlassDockSplitterClass;
    FSplitterWidth: integer;
    fNeedSimplify: TFPList; // list of TControl
    fNeedFree: TFPList; // list of TControl
    fSimplifying: boolean;
    fUpdateCount: integer;
    fDisabledAutosizing: TFPList; // list of TControl
    fTreeNameToDocker: TADNameToControl;  // TGlassDockHostSite, TGlassDockSplitter or custom docksite
    fPopupMenu: TPopupMenu;
    function GetControls(Index: integer): TControl;
    function CloseUnneededControls(Tree: TGlassDockLayoutTree): boolean;
    function CreateNeededControls(Tree: TGlassDockLayoutTree; DisableAutoSizing: boolean; ControlNames: TStrings): boolean;
    procedure MapTreeToControls(Tree: TGlassDockLayoutTree);
    function  RestoreLayout(Tree: TGlassDockLayoutTree; Scale: boolean): boolean;
    procedure EnableAllAutoSizing;
    procedure ClearLayoutProperties(AControl: TControl; NewAlign: TAlign = alClient);
    procedure PopupMenuPopup(Sender: TObject);
    procedure ChangeLockButtonClick(Sender: TObject);
    procedure SetAllowDragging(AValue: boolean);
    procedure SetDockOutsideMargin(AValue: integer);
    procedure SetDockParentMargin(AValue: integer);
    procedure SetDragTreshold(AValue: integer);
    procedure SetHeaderHint(AValue: string);
    procedure SetHeaderColor(AValue: TColor);
    procedure SetPageAreaInPercent(AValue: integer);
    procedure SetSaveOnClose(AValue: boolean);
    procedure SetScaleOnResize(AValue: boolean);
    procedure SetShowMenuItemShowHeader(AValue: boolean);
    procedure ShowHeadersButtonClick(Sender: TObject);
    procedure OptionsClick(Sender: TObject);
    procedure SetIdleConnected(const AValue: boolean);
    procedure SetQueueSimplify(const AValue: boolean);
    procedure SetRestoring(const AValue: boolean);
    procedure OptionsChanged;
  protected
    fCloseBtnReferenceCount: integer;
    fCloseBtnBitmap: TBitmap;
    function DoCreateControl(aName: string; DisableAutoSizing: boolean): TControl;
    procedure AutoSizeAllHeaders(EnableAutoSizing: boolean);
    procedure CreateCloseButtonBitmap; virtual;
    procedure DisableControlAutoSizing(AControl: TControl);
    procedure FreeCloseButtonBitmap; virtual;
    procedure InvalidateHeaders;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetHeaderAlignLeft(const AValue: integer);
    procedure SetHeaderAlignTop(const AValue: integer);
    procedure SetHeaderButtonSize(const AValue: integer);
    procedure SetShowHeader(AValue: boolean);
    procedure SetShowHeaderCaption(const AValue: boolean);
    procedure SetHideHeaderCaptionFloatingControl(const AValue: boolean);
    procedure SetSplitterWidth(const AValue: integer);
    procedure OnIdle(Sender: TObject; var Done: boolean);
    procedure AsyncSimplify({%H-}Data: PtrInt);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ControlCount: integer;
    property Controls[Index: integer]: TControl read GetControls;
    function IndexOfControl(const aName: string): integer;
    function FindControl(const aName: string): TControl;
    function IsSite(AControl: TControl): boolean;
    function IsAnchorSite(AControl: TControl): boolean;
    function IsCustomSite(AControl: TControl): boolean;
    function GetSite(AControl: TControl): TCustomForm;
    function GetAnchorSite(AControl: TControl): TGlassDockHostSite;
    function GetControl(Site: TControl): TControl;
    function IsFloating(AControl: TControl): boolean;
    function GetPopupMenu: TPopupMenu;
    function AddPopupMenuItem(AName, ACaption: string; const OnClickEvent: TNotifyEvent; AParent: TMenuItem = nil): TMenuItem; virtual;
    function AddRemovePopupMenuItem(Add: boolean; AName, ACaption: string; const OnClickEvent: TNotifyEvent;
      AParent: TMenuItem = nil): TMenuItem; virtual;
    // show / make a control dockable
    procedure MakeDockable(AControl: TControl; Show: boolean = True; BringToFront: boolean = False;
      AddDockHeader: boolean = True);
    procedure MakeDockSite(AForm: TCustomForm; Sites: TAnchors; ResizePolicy: TADMResizePolicy;
      AllowInside: boolean = False);
    procedure MakeVisible(AControl: TControl; SwitchPages: boolean);
    function ShowControl(ControlName: string; BringToFront: boolean = False): TControl;
    procedure CloseAll;

    // save/restore layouts
    procedure SaveLayoutToConfig(Config: TConfigStorage);
    procedure SaveMainLayoutToTree(LayoutTree: TGlassDockLayoutTree);
    procedure SaveSiteLayoutToTree(AForm: TCustomForm; LayoutTree: TGlassDockLayoutTree);
    function CreateRestoreLayout(AControl: TControl): TGlassDockRestoreLayout;
    function ConfigIsEmpty(Config: TConfigStorage): boolean;
    function LoadLayoutFromConfig(Config: TConfigStorage; Scale: boolean): boolean;
    property RestoreLayouts: TGlassDockRestoreLayouts read FRestoreLayouts; // layout information for restoring hidden forms
    property Restoring: boolean read FRestoring write SetRestoring;
    property IdleConnected: boolean read FIdleConnected write SetIdleConnected;
    procedure LoadSettingsFromConfig(Config: TConfigStorage);
    procedure SaveSettingsToConfig(Config: TConfigStorage);
    procedure LoadSettings(Settings: TGlassDockSettings);
    procedure SaveSettings(Settings: TGlassDockSettings);
    function SettingsAreEqual(Settings: TGlassDockSettings): boolean;

    // manual docking
    procedure ManualFloat(AControl: TControl);
    procedure ManualDock(SrcSite: TGlassDockHostSite; TargetSite: TCustomForm; Align: TAlign; TargetControl: TControl = nil);
    function ManualEnlarge(Site: TGlassDockHostSite; Side: TAnchorKind; OnlyCheckIfPossible: boolean): boolean;
    // simplification/garbage collection
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure NeedSimplify(AControl: TControl);
    procedure NeedFree(AControl: TControl);
    procedure SimplifyPendingLayouts;

    function AutoFreedIfControlIsRemoved(AControl, RemovedControl: TControl): boolean;
    function CreateSite(NamePrefix: string = ''; DisableAutoSizing: boolean = True): TGlassDockHostSite;
    function CreateSplitter(NamePrefix: string = ''): TGlassDockSplitter;
    property QueueSimplify: boolean read FQueueSimplify write SetQueueSimplify;
    property OnCreateControl: TADCreateControlEvent read FOnCreateControl write FOnCreateControl;

    // options
    property OnShowOptions: TADShowDockMasterOptionsEvent read FOnShowOptions write FOnShowOptions;
    property OnOptionsChanged: TNotifyEvent read FOnOptionsChanged write FOnOptionsChanged;
    property DragTreshold: integer read FDragTreshold write SetDragTreshold default 4;
    property DockOutsideMargin: integer read FDockOutsideMargin write SetDockOutsideMargin default 10; // max distance for outside mouse snapping
    property DockParentMargin: integer read FDockParentMargin write SetDockParentMargin default 10; // max distance for snap to parent
    property PageAreaInPercent: integer read FPageAreaInPercent write SetPageAreaInPercent default 40;
    // size of inner mouse snapping area for page docking
    property ShowHeader: boolean read FShowHeader write SetShowHeader default True; // set to false to hide all headers
    property ShowMenuItemShowHeader: boolean read FShowMenuItemShowHeader write SetShowMenuItemShowHeader default False;
    property ShowHeaderCaption: boolean read FShowHeaderCaption write SetShowHeaderCaption default True;
    // set to false to remove the text in the headers
    property HideHeaderCaptionFloatingControl: boolean read FHideHeaderCaptionFloatingControl
      write SetHideHeaderCaptionFloatingControl default True;
    // disables ShowHeaderCaption for floating controls
    property HeaderAlignTop: integer read FHeaderAlignTop write SetHeaderAlignTop default 80;
    // move header to top, when (width/height)*100<=HeaderAlignTop
    property HeaderAlignLeft: integer read FHeaderAlignLeft write SetHeaderAlignLeft default 120;
    // move header to left, when (width/height)*100>=HeaderAlignLeft
    property HeaderButtonSize: integer read FHeaderButtonSize write SetHeaderButtonSize default 10;
    property HeaderHint: string read FHeaderHint write SetHeaderHint;
    property HeaderColor: TColor read FHeaderColor write SetHeaderColor;
    property SplitterWidth: integer read FSplitterWidth write SetSplitterWidth default 4;
    property ScaleOnResize: boolean read FScaleOnResize write SetScaleOnResize default True; // scale children when resizing a site
    property SaveOnClose: boolean read FSaveOnClose write SetSaveOnClose default true; // you must call SaveLayoutToConfig yourself
    property AllowDragging: boolean read FAllowDragging write SetAllowDragging default True;
    property OptionsChangeStamp: int64 read FOptionsChangeStamp;
    procedure IncreaseOptionsChangeStamp; inline;
    // for descendants
    property SplitterClass: TGlassDockSplitterClass read FSplitterClass write FSplitterClass;
    property SiteClass: TGlassDockHostSiteClass read FSiteClass write FSiteClass;
    property ManagerClass: TGlassDockManagerClass read FManagerClass write FManagerClass;
    property HeaderClass: TGlassDockHeaderClass read FHeaderClass write FHeaderClass;
    property PageControlClass: TGlassDockPageControlClass read FPageControlClass write FPageControlClass;
    property PageClass: TGlassDockPageClass read FPageClass write FPageClass;
  end;

var
  VarDockMaster: TGlassDockMaster = nil;

function dbgs(SiteType: TGlassDockHostSiteType): string; overload;
procedure CopyAnchorBounds(Source, Target: TControl);
procedure AnchorAndChangeBounds(AControl: TControl; Side: TAnchorKind; Target: TControl);
function ControlsLeftTopOnScreen(AControl: TControl): TPoint;

type
  TAnchorControlsRect = array[TAnchorKind] of TControl;

function DockedControlIsVisible(Control: TControl): boolean;
function GetDockSplitter(Control: TControl; Side: TAnchorKind; out Splitter: TGlassDockSplitter): boolean;
function GetDockSplitterOrParent(Control: TControl; Side: TAnchorKind; out AnchorControl: TControl): boolean;
function CountAnchoredControls(Control: TControl; Side: TAnchorKind): integer;
function NeighbourCanBeShrinked(EnlargeControl, Neighbour: TControl; Side: TAnchorKind): boolean;
function ControlIsAnchoredIndirectly(StartControl: TControl; Side: TAnchorKind; DestControl: TControl): boolean;
function GetEnclosingControlRect(ControlList: TFPlist; out ARect: TAnchorControlsRect): boolean;
function GetEnclosedControls(const ARect: TAnchorControlsRect): TFPList;
procedure GetAnchorControlsRect(Control: TControl; out ARect: TAnchorControlsRect);

implementation

// DaThoX begin
type
  TCustomFormAccess = class(TCustomForm);
  TIDEDockMasterAccess = class(TIDEDockMaster);
// DaThoX end

function dbgs(SiteType: TGlassDockHostSiteType): string; overload;
begin
  case SiteType of
    adhstNone:       Result := 'None';
    adhstOneControl: Result := 'OneControl';
    adhstLayout:     Result := 'Layout';
    adhstPages:      Result := 'Pages';
    else
      Result := '?';
  end;
end;

procedure CopyAnchorBounds(Source, Target: TControl);
var
  a: TAnchorKind;
begin
  Target.DisableAutoSizing;
  Target.BoundsRect := Source.BoundsRect;
  Target.Anchors := Source.Anchors;
  Target.Align := Source.Align;
  for a := low(TAnchorKind) to high(TAnchorKind) do
    Target.AnchorSide[a].Assign(Source.AnchorSide[a]);
  Target.EnableAutoSizing;
end;

procedure AnchorAndChangeBounds(AControl: TControl; Side: TAnchorKind; Target: TControl);
begin
  if Target = AControl.Parent then
  begin
    AControl.AnchorParallel(Side, 0, Target);
    case Side of
      akTop: AControl.Top := 0;
      akLeft: AControl.Left := 0;
      akRight: AControl.Width := AControl.Parent.ClientWidth - AControl.Left;
      akBottom: AControl.Height := AControl.Parent.ClientHeight - AControl.Top;
    end;
  end
  else
  begin
    AControl.AnchorToNeighbour(Side, 0, Target);
    case Side of
      akTop: AControl.Top := Target.Top + Target.Height;
      akLeft: AControl.Left := Target.Left + Target.Width;
      akRight: AControl.Width := Target.Left - AControl.Width;
      akBottom: AControl.Height := Target.Top - AControl.Height;
    end;
  end;
end;

function ControlsLeftTopOnScreen(AControl: TControl): TPoint;
begin
  if AControl.Parent <> nil then
  begin
    Result := AControl.Parent.ClientOrigin;
    Inc(Result.X, AControl.Left);
    Inc(Result.Y, AControl.Top);
  end
  else
  begin
    Result := AControl.Parent.ClientOrigin;
  end;
end;

function DockedControlIsVisible(Control: TControl): boolean;
begin
  while Control<>nil do begin
    if (not Control.IsControlVisible)
    and (not (Control is TGlassDockPage)) then
      exit(false);
    Control:=Control.Parent;
  end;
  Result:=true;
end;

function GetDockSplitter(Control: TControl; Side: TAnchorKind; out Splitter: TGlassDockSplitter): boolean;
begin
  Result := False;
  Splitter := nil;
  if not (Side in Control.Anchors) then
    exit;
  Splitter := TGlassDockSplitter(Control.AnchorSide[Side].Control);
  if not (Splitter is TGlassDockSplitter) then
  begin
    Splitter := nil;
    exit;
  end;
  if Splitter.Parent <> Control.Parent then
    exit;
  Result := True;
end;

function GetDockSplitterOrParent(Control: TControl; Side: TAnchorKind; out AnchorControl: TControl): boolean;
begin
  Result := False;
  AnchorControl := nil;
  if not (Side in Control.Anchors) then
    exit;
  AnchorControl := Control.AnchorSide[Side].Control;
  if (AnchorControl is TGlassDockSplitter) and (AnchorControl.Parent = Control.Parent) then
    Result := True
  else if AnchorControl = Control.Parent then
    Result := True;
end;

function CountAnchoredControls(Control: TControl; Side: TAnchorKind): integer;
var
  i: integer;
  Neighbour: TControl;
begin
  Result := 0;
  for i := 0 to Control.AnchoredControlCount - 1 do
  begin
    Neighbour := Control.AnchoredControls[i];
    if (OppositeAnchor[Side] in Neighbour.Anchors) and (Neighbour.AnchorSide[OppositeAnchor[Side]].Control = Control) then
      Inc(Result);
  end;
end;

function NeighbourCanBeShrinked(EnlargeControl, Neighbour: TControl; Side: TAnchorKind): boolean;
const
  MinControlSize = 20;
var
  Splitter: TGlassDockSplitter;
begin
  Result := False;
  if not GetDockSplitter(EnlargeControl, OppositeAnchor[Side], Splitter) then
    exit;
  case Side of
    akLeft: // check if left side of Neighbour can be moved
      Result := Neighbour.Left + Neighbour.Width > EnlargeControl.Left + EnlargeControl.Width + Splitter.Width + MinControlSize;
    akRight: // check if right side of Neighbour can be moved
      Result := Neighbour.Left + MinControlSize + Splitter.Width < EnlargeControl.Left;
    akTop: // check if top side of Neighbour can be moved
      Result := Neighbour.Top + Neighbour.Height > EnlargeControl.Top + EnlargeControl.Height + Splitter.Height + MinControlSize;
    akBottom: // check if bottom side of Neighbour can be moved
      Result := Neighbour.Top + MinControlSize + Splitter.Height < EnlargeControl.Top;
  end;
end;

function ControlIsAnchoredIndirectly(StartControl: TControl; Side: TAnchorKind; DestControl: TControl): boolean;
var
  Checked: array of boolean;
  Parent: TWinControl;
  //------------------------------------------------------
  function Check(ControlIndex: integer): boolean;
  var
    AControl: TControl;
    SideControl: TControl;
    i: integer;
  begin
    if Checked[ControlIndex] then
      exit(False);
    Checked[ControlIndex] := True;
    AControl := Parent.Controls[ControlIndex];
    if AControl = DestControl then
      exit(True);

    if (Side in AControl.Anchors) then
    begin
      SideControl := AControl.AnchorSide[Side].Control;
      if (SideControl <> nil) and Check(Parent.GetControlIndex(SideControl)) then
        exit(True);
    end;
    for i := 0 to AControl.AnchoredControlCount - 1 do   // 4444
    begin
      if Checked[i] then
        continue;
      SideControl := AControl.AnchoredControls[i];
      if OppositeAnchor[Side] in SideControl.Anchors then
      begin
        if (SideControl.AnchorSide[OppositeAnchor[Side]].Control = AControl) and Check(i) then
          exit(True);
      end;
    end;
    Result := False;
  end;
  //------------------------------------------------------

var
  i: integer;
begin
  if (StartControl = nil) or (DestControl = nil) or (StartControl.Parent = nil) or (StartControl.Parent <> DestControl.Parent) or
    (StartControl = DestControl) then
    exit(False);

  Parent := StartControl.Parent;
  SetLength(Checked, Parent.ControlCount);
  for i := 0 to length(Checked) - 1 do
    Checked[i] := False;
  Result := Check(Parent.GetControlIndex(StartControl));
end;

procedure GetAnchorControlsRect(Control: TControl; out ARect: TAnchorControlsRect);
var
  a: TAnchorKind;
begin
  for a := Low(TAnchorKind) to High(TAnchorKind) do
    ARect[a] := Control.AnchorSide[a].Control;
end;

function GetEnclosingControlRect(ControlList: TFPlist; out ARect: TAnchorControlsRect): boolean;
var
  Parent: TWinControl;
  //------------------------------------------------------
  function ControlIsValidAnchor(Control: TControl; Side: TAnchorKind): boolean;
  var
    i: integer;
  begin
    Result := False;
    if (Control = ARect[Side]) then
      exit(True);// this allows Parent at the beginning

    if not (Control is TGlassDockSplitter) then
      exit;// not a splitter
    if (TGlassDockSplitter(Control).ResizeAnchor in [akLeft, akRight]) <> (Side in [akLeft, akRight]) then
      exit;// wrong alignment
    if ControlList.IndexOf(Control) >= 0 then
      exit;// is an inner control
    if ControlIsAnchoredIndirectly(Control, Side, ARect[Side]) then
      exit; // this anchor would be worse than the current maximum
    for i := 0 to ControlList.Count - 1 do
    begin
      if not ControlIsAnchoredIndirectly(Control, Side, TControl(ControlList[i])) then
      begin
        // this anchor is not above (below, ...) the inner controls
        exit;
      end;
    end;
    Result := True;
  end;
  //------------------------------------------------------

var
  TopIndex: integer;
  TopControl: TControl;
  RightIndex: integer;
  RightControl: TControl;
  BottomIndex: integer;
  BottomControl: TControl;
  LeftIndex: integer;
  LeftControl: TControl;
  Candidates: TFPList;
  i: integer;
  a: TAnchorKind;
begin
  Result := False;
  if (ControlList = nil) or (ControlList.Count = 0) then
    exit;

  // get Parent
  Parent := TControl(ControlList[0]).Parent;
  if Parent = nil then
    exit;
  for i := 0 to ControlList.Count - 1 do
    if TControl(ControlList[i]).Parent <> Parent then
      exit;

  // set the default rect: the Parent
  Result := True;
  for a := Low(TAnchorKind) to High(TAnchorKind) do
    ARect[a] := Parent;

  // find all possible Candidates
  Candidates := TFPList.Create;
  try
    Candidates.Add(Parent);
    for i := 0 to Parent.ControlCount - 1 do
      if Parent.Controls[i] is TGlassDockSplitter then
        Candidates.Add(Parent.Controls[i]);

    // now check every possible rectangle
    // Note: four loops seems to be dog slow, but the checks
    //       avoid most possibilities early
    for TopIndex := 0 to Candidates.Count - 1 do
    begin
      TopControl := TControl(Candidates[TopIndex]);
      if not ControlIsValidAnchor(TopControl, akTop) then
        continue;

      for RightIndex := 0 to Candidates.Count - 1 do
      begin
        RightControl := TControl(Candidates[RightIndex]);
        if (TopControl.AnchorSide[akRight].Control <> RightControl) and (RightControl.AnchorSide[akTop].Control <> TopControl) then
          continue; // not touching / not a corner
        if not ControlIsValidAnchor(RightControl, akRight) then
          continue;

        for BottomIndex := 0 to Candidates.Count - 1 do
        begin
          BottomControl := TControl(Candidates[BottomIndex]);
          if (RightControl.AnchorSide[akBottom].Control <> BottomControl) and (BottomControl.AnchorSide[akRight].Control <> RightControl) then
            continue; // not touching / not a corner
          if not ControlIsValidAnchor(BottomControl, akBottom) then
            continue;

          for LeftIndex := 0 to Candidates.Count - 1 do
          begin
            LeftControl := TControl(Candidates[LeftIndex]);
            if (BottomControl.AnchorSide[akLeft].Control <> LeftControl) and (LeftControl.AnchorSide[akBottom].Control <> BottomControl) then
              continue; // not touching / not a corner
            if (TopControl.AnchorSide[akLeft].Control <> LeftControl) and (LeftControl.AnchorSide[akTop].Control <> LeftControl) then
              continue; // not touching / not a corner
            if not ControlIsValidAnchor(LeftControl, akLeft) then
              continue;

            // found a better rectangle
            ARect[akLeft] := LeftControl;
            ARect[akRight] := RightControl;
            ARect[akTop] := TopControl;
            ARect[akBottom] := BottomControl;
          end;
        end;
      end;
    end;
  finally
    Candidates.Free;
  end;
end;

function GetEnclosedControls(const ARect: TAnchorControlsRect): TFPList;
var
  Parent: TWinControl;
  //------------------------------------------------------
  procedure Fill(AControl: TControl);
  var
    a: TAnchorKind;
    SideControl: TControl;
    i: integer;
  begin
    if AControl = nil then
      exit;
    if AControl = Parent then
      exit;// do not add Parent
    for a := Low(TAnchorKind) to High(TAnchorKind) do
      if ARect[a] = AControl then
        exit;// do not add boundary

    if Result.IndexOf(AControl) >= 0 then
      exit;// already added
    Result.Add(AControl);

    for a := Low(TAnchorKind) to High(TAnchorKind) do
      Fill(AControl.AnchorSide[a].Control);
    for i := 0 to Parent.ControlCount - 1 do
    begin
      SideControl := Parent.Controls[i];
      for a := Low(TAnchorKind) to High(TAnchorKind) do
        if SideControl.AnchorSide[a].Control = AControl then
          Fill(SideControl);
    end;
  end;
  //------------------------------------------------------
var
  i: integer;
  AControl: TControl;
  LeftTopControl: TControl;
begin
  Result := TFPList.Create;

  // find the Parent
  if (ARect[akLeft] = ARect[akRight]) and (ARect[akLeft] is TWinControl) then
    Parent := TWinControl(ARect[akLeft])
  else
    Parent := ARect[akLeft].Parent;

  // find the left, top most control
  for i := 0 to Parent.ControlCount - 1 do
  begin
    AControl := Parent.Controls[i];
    if (AControl.AnchorSide[akLeft].Control = ARect[akLeft]) and (AControl.AnchorSide[akTop].Control = ARect[akTop]) then
    begin
      LeftTopControl := AControl;
      break;
    end;
  end;
  if Result.Count = 0 then
    exit;

  // use flood fill to find the rest
  Fill(LeftTopControl);
end;

//====================== TGlassDockSettings ============================================

procedure TGlassDockSettings.SetAllowDragging(AValue: boolean);
begin
  if FAllowDragging = AValue then
    Exit;
  FAllowDragging := AValue;
  IncreaseChangeStamp;
end;

procedure TGlassDockSettings.SetDockOutsideMargin(AValue: integer);
begin
  if FDockOutsideMargin = AValue then
    Exit;
  FDockOutsideMargin := AValue;
  IncreaseChangeStamp;
end;

procedure TGlassDockSettings.SetDockParentMargin(AValue: integer);
begin
  if FDockParentMargin = AValue then
    Exit;
  FDockParentMargin := AValue;
  IncreaseChangeStamp;
end;

procedure TGlassDockSettings.SetDragTreshold(AValue: integer);
begin
  if FDragTreshold = AValue then
    Exit;
  FDragTreshold := AValue;
  IncreaseChangeStamp;
end;

procedure TGlassDockSettings.SetHeaderAlignLeft(AValue: integer);
begin
  if FHeaderAlignLeft = AValue then
    Exit;
  FHeaderAlignLeft := AValue;
  IncreaseChangeStamp;
end;

procedure TGlassDockSettings.SetHeaderAlignTop(AValue: integer);
begin
  if FHeaderAlignTop = AValue then
    Exit;
  FHeaderAlignTop := AValue;
  IncreaseChangeStamp;
end;

procedure TGlassDockSettings.SetHeaderButtonSize(AValue: integer);
begin
  if FHeaderButtonSize = AValue then
    Exit;
  FHeaderButtonSize := AValue;
  IncreaseChangeStamp;
end;

procedure TGlassDockSettings.SetHeaderHint(AValue: string);
begin
  if FHeaderHint = AValue then
    Exit;
  FHeaderHint := AValue;
  IncreaseChangeStamp;
end;

procedure TGlassDockSettings.SetHeaderColor(AValue: TColor);
begin
  if FHeaderColor = AValue then
    Exit;
  FHeaderColor := AValue;
  IncreaseChangeStamp;
end;

procedure TGlassDockSettings.SetHideHeaderCaptionFloatingControl(AValue: boolean);
begin
  if FHideHeaderCaptionFloatingControl = AValue then
    Exit;
  FHideHeaderCaptionFloatingControl := AValue;
  IncreaseChangeStamp;
end;

procedure TGlassDockSettings.SetPageAreaInPercent(AValue: integer);
begin
  if FPageAreaInPercent = AValue then
    Exit;
  FPageAreaInPercent := AValue;
  IncreaseChangeStamp;
end;

procedure TGlassDockSettings.SetSaveOnClose(AValue: boolean);
begin
  if FSaveOnClose=AValue then Exit;
  FSaveOnClose:=AValue;
  IncreaseChangeStamp;
end;

procedure TGlassDockSettings.SetScaleOnResize(AValue: boolean);
begin
  if FScaleOnResize = AValue then
    Exit;
  FScaleOnResize := AValue;
  IncreaseChangeStamp;
end;

procedure TGlassDockSettings.SetShowHeader(AValue: boolean);
begin
  if FShowHeader = AValue then
    Exit;
  FShowHeader := AValue;
  IncreaseChangeStamp;
end;

procedure TGlassDockSettings.SetShowHeaderCaption(AValue: boolean);
begin
  if FShowHeaderCaption = AValue then
    Exit;
  FShowHeaderCaption := AValue;
  IncreaseChangeStamp;
end;

procedure TGlassDockSettings.SetSplitterWidth(AValue: integer);
begin
  if FSplitterWidth = AValue then
    Exit;
  FSplitterWidth := AValue;
  IncreaseChangeStamp;
end;

procedure TGlassDockSettings.IncreaseChangeStamp;
begin
  LUIncreaseChangeStamp(fChangeStamp);
end;

procedure TGlassDockSettings.LoadFromConfig(Config: TConfigStorage);
begin
  Config.AppendBasePath('Settings/');
  DragTreshold := Config.GetValue('DragThreshold', 4);
  DockOutsideMargin := Config.GetValue('DockOutsideMargin', 10);
  DockParentMargin := Config.GetValue('DockParentMargin', 10);
  PageAreaInPercent := Config.GetValue('PageAreaInPercent', 40);
  HeaderAlignTop := Config.GetValue('HeaderAlignTop', 80);
  HeaderAlignLeft := Config.GetValue('HeaderAlignLeft', 120);
  SplitterWidth := Config.GetValue('SplitterWidth', 4);
  ScaleOnResize := Config.GetValue('ScaleOnResize', True);
  SaveOnClose:=Config.GetValue('SaveOnClose',true);
  ShowHeader := Config.GetValue('ShowHeader', True);
  ShowHeaderCaption := Config.GetValue('ShowHeaderCaption', True);
  HideHeaderCaptionFloatingControl := Config.GetValue('HideHeaderCaptionFloatingControl', True);
  AllowDragging := Config.GetValue('AllowDragging', True);
  HeaderButtonSize := Config.GetValue('HeaderButtonSize', 10);
  HeaderColor := Config.GetValue('HeaderColor', cnHeaderColorDefault);
  Config.UndoAppendBasePath;
end;

procedure TGlassDockSettings.SaveToConfig(Config: TConfigStorage);
begin
  Config.AppendBasePath('Settings/');
  Config.SetDeleteValue('DragThreshold', DragTreshold, 4);
  Config.SetDeleteValue('DockOutsideMargin', DockOutsideMargin, 10);
  Config.SetDeleteValue('DockParentMargin', DockParentMargin, 10);
  Config.SetDeleteValue('PageAreaInPercent', PageAreaInPercent, 40);
  Config.SetDeleteValue('HeaderAlignTop', HeaderAlignTop, 80);
  Config.SetDeleteValue('HeaderAlignLeft', HeaderAlignLeft, 120);
  Config.SetDeleteValue('SplitterWidth', SplitterWidth, 4);
  Config.SetDeleteValue('ScaleOnResize', ScaleOnResize, True);
  Config.SetDeleteValue('SaveOnClose',SaveOnClose,true);
  Config.SetDeleteValue('ShowHeader', ShowHeader, True);
  Config.SetDeleteValue('ShowHeaderCaption', ShowHeaderCaption, True);
  Config.SetDeleteValue('HideHeaderCaptionFloatingControl', HideHeaderCaptionFloatingControl, True);
  Config.SetDeleteValue('AllowDragging', AllowDragging, True);
  Config.SetDeleteValue('HeaderButtonSize', HeaderButtonSize, 10);
  Config.SetDeleteValue('HeaderColor', HeaderColor, cnHeaderColorDefault);
  Config.UndoAppendBasePath;
end;

function TGlassDockSettings.IsEqual(Settings: TGlassDockSettings): boolean;
begin
  Result:=(DragTreshold=Settings.DragTreshold)
      and (DockOutsideMargin=Settings.DockOutsideMargin)
      and (DockParentMargin=Settings.DockParentMargin)
      and (PageAreaInPercent=Settings.PageAreaInPercent)
      and (HeaderAlignTop=Settings.HeaderAlignTop)
      and (HeaderAlignLeft=Settings.HeaderAlignLeft)
      and (HeaderHint=Settings.HeaderHint)
      and (SplitterWidth=Settings.SplitterWidth)
      and (ScaleOnResize=Settings.ScaleOnResize)
      and (SaveOnClose=Settings.SaveOnClose)
      and (ShowHeader=Settings.ShowHeader)
      and (ShowHeaderCaption=Settings.ShowHeaderCaption)
      and (HideHeaderCaptionFloatingControl=Settings.HideHeaderCaptionFloatingControl)
      and (AllowDragging=Settings.AllowDragging)
      and (HeaderButtonSize=Settings.HeaderButtonSize)
      and (HeaderColor=Settings.HeaderColor)
      ;
end;

procedure TGlassDockSettings.ResetToDefault;
begin
  DragTreshold := 4;
  DockOutsideMargin := 10;
  DockParentMargin := 10;
  PageAreaInPercent := 40;
  HeaderAlignTop := 80;
  HeaderAlignLeft := 120;
  SplitterWidth := 4;
  ScaleOnResize := True;
  SaveOnClose := True;
  ShowHeader := True;
  ShowHeaderCaption := True;
  HideHeaderCaptionFloatingControl := True;
  AllowDragging := True;
  HeaderButtonSize := 10;
  HeaderColor := cnHeaderColorDefault;
end;

//========================= TGlassDockMaster ==================================================

constructor TGlassDockMaster.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FControls := TFPList.Create;
  FAllowDragging := True;
  FHeaderButtonSize := 10;
  FDragTreshold := 4;
  FDockOutsideMargin := 10;
  FDockParentMargin := 10;
  FPageAreaInPercent := 40;
  FHeaderAlignTop := 80;
  HeaderAlignLeft := 120;
  FHeaderHint := adrsDragAndDockC;
  FShowHeader := True;
  FShowHeaderCaption := True;
  FHideHeaderCaptionFloatingControl := True;
  FSplitterWidth := 4;
  FScaleOnResize := True;
  FSaveOnClose:=true;
  fNeedSimplify := TFPList.Create;
  fNeedFree := TFPList.Create;
  fDisabledAutosizing := TFPList.Create;
  FSplitterClass := TGlassDockSplitter;
  FSiteClass := TGlassDockHostSite;
  FManagerClass := TGlassDockManager;
  FHeaderClass := TGlassDockHeader;
  FPageControlClass := TGlassDockPageControl;
  FPageClass := TGlassDockPage;
  FRestoreLayouts := TGlassDockRestoreLayouts.Create;
end;

destructor TGlassDockMaster.Destroy;
var
  AControl: TControl;
  i: integer;
begin
  QueueSimplify := False;

  if FControls.Count > 0 then
    for i := FControls.Count - 1 downto 0 do
    begin
      AControl := nil;
      AControl := Controls[i];
      if AControl <> nil then
        AControl.Free;
    end;
  FControls.Clear;

  fCloseBtnReferenceCount := -1;
  FreeCloseButtonBitmap;

  if FRestoreLayouts <> nil then FRestoreLayouts.Free;
  if fPopupMenu <> nil then fPopupMenu.Free;

  if fTreeNameToDocker <> nil then fTreeNameToDocker.Free;
  if fNeedSimplify <> nil then fNeedSimplify.Free;
  if fNeedFree <> nil then fNeedFree.Free;
  if fDisabledAutosizing <> nil then fDisabledAutosizing.Free;
  if fTreeNameToDocker <> nil then fTreeNameToDocker.Free;
  if FControls <> nil then  FControls.Free;

  inherited Destroy;
end;

function TGlassDockMaster.GetControls(Index: integer): TControl;
begin
  Result := TControl(FControls[Index]);
end;

procedure TGlassDockMaster.SetHeaderAlignLeft(const AValue: integer);
begin
  if FHeaderAlignLeft = AValue then
    exit;
  FHeaderAlignLeft := AValue;
  FHeaderAlignTop := Min(FHeaderAlignLeft - 1, FHeaderAlignTop);
  OptionsChanged;
end;

procedure TGlassDockMaster.SetHeaderAlignTop(const AValue: integer);
begin
  if FHeaderAlignTop = AValue then
    exit;
  FHeaderAlignTop := AValue;
  FHeaderAlignLeft := Max(FHeaderAlignTop + 1, FHeaderAlignLeft);
  OptionsChanged;
end;

function TGlassDockMaster.CloseUnneededControls(Tree: TGlassDockLayoutTree): boolean;
var
  i: integer;
  AControl: TControl;
begin
  i := ControlCount - 1;

  for i := ControlCount - 1 downto 0 do
  begin
    AControl := Controls[i];
    if DockedControlIsVisible(AControl) and
       (Tree.Root.FindChildNode(AControl.Name, True) = nil) and
       (Application.MainForm <> AControl) then
    begin
      DisableControlAutoSizing(AControl);

      if AControl.HostDockSite is TGlassDockHostSite then
      begin
        if not TGlassDockHostSite(AControl.HostDockSite).CloseSite then
        begin
          if FControls.IndexOf(AControl) < 0 then
            AControl := nil;

          exit(False);
        end;
      end;

      if FControls.IndexOf(AControl) >= 0 then
      begin
        // the control is still there
        if AControl.HostDockSite <> nil then
        begin
          AControl.HostDockSite.Visible := False;
          AControl.HostDockSite.Parent := nil;
        end
        else
        begin
          AControl.Visible := False;
          AControl.Parent := nil;
        end;
      end;
    end;
  end;

  Result := True;
end;

function TGlassDockMaster.CreateNeededControls(Tree: TGlassDockLayoutTree; DisableAutoSizing: boolean; ControlNames: TStrings): boolean;
  //.........................................................
  procedure CreateControlsForNode(Node: TGlassDockLayoutTreeNode);
  var
    i: Integer;
    AControl: TControl;
  begin
    if (Node.NodeType in [adltnControl,adltnCustomSite])
    and (Node.Name<>'') then begin
      AControl:=FindControl(Node.Name);
      if AControl<>nil then
      begin
        if DisableAutoSizing then
          DisableControlAutoSizing(AControl);
      end else
      begin
        AControl:=DoCreateControl(Node.Name,true);
        if AControl<>nil then begin
          try
            if DisableAutoSizing and (fDisabledAutosizing.IndexOf(AControl)<0)
            then begin
              fDisabledAutosizing.Add(AControl);
              AControl.FreeNotification(Self);
            end;
            if Node.NodeType=adltnControl then
              MakeDockable(AControl,false)
            else if not IsCustomSite(AControl) then Exit
             
          finally
            if not DisableAutoSizing then
              AControl.EnableAutoSizing;
          end;
        end else Exit;
        
      end;
      if AControl<>nil then
        ControlNames.Add(AControl.Name);
    end;
    for i:=0 to Node.Count-1 do
      CreateControlsForNode(Node[i]);
  end;
  //.........................................................

begin
  Result := False;
  CreateControlsForNode(Tree.Root);
  Result := True;
end;

procedure TGlassDockMaster.MapTreeToControls(Tree: TGlassDockLayoutTree);
//------------------------------------------------------
  procedure MapHostDockSites(Node: TGlassDockLayoutTreeNode);
  // map in TreeNameToDocker each control name to its HostDockSite or custom dock site
  var
    i: integer;
    AControl: TControl;
  begin
    if Node.IsSplitter then
      exit;
    if (Node.NodeType = adltnControl) then
    begin
      AControl := FindControl(Node.Name);
      if (AControl <> nil) and (AControl.HostDockSite is TGlassDockHostSite) then
        fTreeNameToDocker[Node.Name] := AControl.HostDockSite;
      // ignore kids
      exit;
    end;
    if (Node.NodeType = adltnCustomSite) then
    begin
      AControl := FindControl(Node.Name);
      if IsCustomSite(AControl) then
        fTreeNameToDocker[Node.Name] := AControl;
    end;
    for i := 0 to Node.Count - 1 do
      MapHostDockSites(Node[i]); // recursive
  end;
  //-----------------------------------------------------
  procedure MapTopLevelSites(Node: TGlassDockLayoutTreeNode);
  // map in TreeNameToDocker each RootWindow node name to a site whith a
  // corresponding control
  // For example: if there is control on a complex site (SiteA), and the control
  //    has a node in the Tree, then the root node of the tree node is mapped to
  //    the SiteA. This way the corresponding root forms are kept which reduces
  //    flickering.

    function FindMappedControl(ChildNode: TGlassDockLayoutTreeNode): TCustomForm;
    var
      i: integer;
    begin
      if ChildNode.NodeType in [adltnControl, adltnCustomSite] then
        Result := TCustomForm(fTreeNameToDocker[ChildNode.Name])
      else
        for i := 0 to ChildNode.Count - 1 do
        begin
          Result := FindMappedControl(ChildNode[i]); // search recursive
          if Result <> nil then
            exit;
        end;
    end;
    //-----------------------------------------------------
  var
    i: integer;
    RootSite: TCustomForm;
    Site: TCustomForm;
  begin
    if Node.IsSplitter then
      exit;
    if Node.IsRootWindow then
    begin
      if Node.Name = '' then
        exit;
      if Node.NodeType = adltnControl then
        exit;
      // Node is a complex site
      if fTreeNameToDocker[Node.Name] <> nil then
        exit;
      // and not yet mapped to a site
      Site := FindMappedControl(Node);
      if Site = nil then
        exit;
      // and there is sub node mapped to a site (anchor or custom)
      RootSite := GetParentForm(Site);
      if not (RootSite is TGlassDockHostSite) then
        exit;
      // and the mapped site has a root site
      if fTreeNameToDocker.ControlToName(RootSite) <> '' then
        exit;
      // and the root site is not yet mapped
      // => map the root node to the root site
      fTreeNameToDocker[Node.Name] := RootSite;
    end
    else
      for i := 0 to Node.Count - 1 do
        MapTopLevelSites(Node[i]); // recursive
  end;

  //-----------------------------------------------------

  procedure MapBottomUp(Node: TGlassDockLayoutTreeNode);
  { map the other nodes to existing sites
    The heuristic works like this:
      if a child node was mapped to a site and the site has a parent site then
      map this node to this parent site.
  }
  var
    i: integer;
    BestSite: TControl;
  begin
    if Node.IsSplitter then
      exit;
    BestSite := fTreeNameToDocker[Node.Name];
    for i := 0 to Node.Count - 1 do
    begin
      MapBottomUp(Node[i]); // recursive
      if BestSite = nil then
        BestSite := fTreeNameToDocker[Node[i].Name];
    end;
    if (fTreeNameToDocker[Node.Name] = nil) and (BestSite <> nil) then
    begin
      // search the parent site of a child site
      repeat
        BestSite := BestSite.Parent;
        if BestSite is TGlassDockHostSite then
        begin
          if fTreeNameToDocker.ControlToName(BestSite) = '' then
            fTreeNameToDocker[Node.Name] := BestSite;
          break;
        end;
      until (BestSite = nil);
    end;
  end;

  //-----------------------------------------------------

  procedure MapSplitters(Node: TGlassDockLayoutTreeNode);
  { map the splitter nodes to existing splitters
    The heuristic works like this:
      If a node is mapped to a site and the node is at Side anchored to a
      splitter node and the site is anchored at Side to a splitter then
      map the the splitter node to the splitter.
  }
  var
    i: integer;
    Side: TAnchorKind;
    Site: TControl;
    SplitterNode: TGlassDockLayoutTreeNode;
    Splitter: TControl;
  begin
    if Node.IsSplitter then
      exit;
    for i := 0 to Node.Count - 1 do
      MapSplitters(Node[i]); // recursive

    if Node.Parent = nil then
      exit;
    // node is a child node
    Site := fTreeNameToDocker[Node.Name];
    if Site = nil then
      exit;
    // node is mapped to a site
    // check each side
    for Side:=Low(TAnchorKind) to high(TAnchorKind) do begin
      if Node.Anchors[Side]='' then continue;
      Splitter:=Site.AnchorSide[Side].Control;
      if (not (Splitter is TGlassDockSplitter))
      or (Splitter.Parent<>Site.Parent) then continue;
      SplitterNode:=Node.Parent.FindChildNode(Node.Anchors[Side],false);
      if (SplitterNode=nil) then continue;
      // this Side of node is anchored to a splitter node
      if fTreeNameToDocker[SplitterNode.Name]<>nil then continue;
      // the SplitterNode is not yet mapped
      if fTreeNameToDocker.ControlToName(Splitter)<>'' then continue;
      // there is an unmapped splitter anchored to the Site
      // => map the splitter to the splitter node
      // Note: Splitter.Name can be different from SplitterNode.Name !
      fTreeNameToDocker[SplitterNode.Name]:=Splitter;
    end;
  end;

  //-----------------------------------------------------

begin
  MapHostDockSites(Tree.Root);
  MapTopLevelSites(Tree.Root);
  MapBottomUp(Tree.Root);
  MapSplitters(Tree.Root);
end;

function TGlassDockMaster.RestoreLayout(Tree: TGlassDockLayoutTree; Scale: boolean): boolean;
var
  WorkArea, SrcWorkArea: TRect;

  function SrcRectValid(const r: TRect): boolean;
  begin
    Result:=(r.Left<r.Right) and (r.Top<r.Bottom);
  end;

  function ScaleTopLvlX(p: integer): integer;
  begin
    Result:=p;
    if SrcRectValid(SrcWorkArea) and SrcRectValid(WorkArea) then
      Result:=((p-SrcWorkArea.Left)*(WorkArea.Right-WorkArea.Left))
                div (SrcWorkArea.Right-SrcWorkArea.Left)
              +WorkArea.Left;
  end;

  function ScaleTopLvlY(p: integer): integer;
  begin
    Result:=p;
    if SrcRectValid(SrcWorkArea) and SrcRectValid(WorkArea) then
      Result:=((p-SrcWorkArea.Top)*(WorkArea.Bottom-WorkArea.Top))
                   div (SrcWorkArea.Bottom-SrcWorkArea.Top)
              +WorkArea.Top;
  end;

  function ScaleChildX(p: integer): integer;
  begin
    Result:=p;
    if SrcRectValid(SrcWorkArea) and SrcRectValid(WorkArea) then
      Result:=p*(WorkArea.Right-WorkArea.Left)
                div (SrcWorkArea.Right-SrcWorkArea.Left);
  end;

  function ScaleChildY(p: integer): integer;
  begin
    Result:=p;
    if SrcRectValid(SrcWorkArea) and SrcRectValid(WorkArea) then
      Result:=p*(WorkArea.Bottom-WorkArea.Top)
                div (SrcWorkArea.Bottom-SrcWorkArea.Top);
  end;

  procedure SetupSite(Site: TCustomForm; Node: TGlassDockLayoutTreeNode; Parent: TWinControl);
  var
    aManager: TGlassDockManager;
    NewBounds: TRect;
    aMonitor: TMonitor;
    LParent: TWinControl;
  begin
    if Parent=nil then begin
      if (Node.Monitor>=0) and (Node.Monitor<Screen.MonitorCount) then
        aMonitor:=Screen.Monitors[Node.Monitor]
      else
        aMonitor:=Site.Monitor;
      WorkArea:=aMonitor.WorkareaRect;     
    end;
    if IsCustomSite(Site) then begin
      aManager:=TGlassDockManager(Site.DockManager);
      if Node.Count>0 then
      begin
        // this custom dock site gets a child => store and clear constraints
        aManager.StoreConstraints;
      end;
    end;
    Site.Constraints.MaxWidth:=0;
    Site.Constraints.MaxHeight:=0;
    NewBounds:=Node.BoundsRect;
    if Parent=nil then begin
      NewBounds:=Rect(ScaleTopLvlX(NewBounds.Left),ScaleTopLvlY(NewBounds.Top),
                      ScaleTopLvlX(NewBounds.Right),ScaleTopLvlY(NewBounds.Bottom));
    end else begin
      NewBounds:=Rect(ScaleChildX(NewBounds.Left),ScaleChildY(NewBounds.Top),
                      ScaleChildX(NewBounds.Right),ScaleChildY(NewBounds.Bottom));
    end;

    Site.BoundsRect:=NewBounds;
    Site.Visible:=true;
    Site.Parent:=Parent;
    if IsCustomSite(Parent) then
    begin
      aManager:=TGlassDockManager(Parent.DockManager);
      Site.Align:=Node.Align;

      aManager.RestoreSite(Node.BoundSplitterPos);
      Site.HostDockSite:=Parent;
    end;
    if Site is TGlassDockHostSite then
    begin
      TGlassDockHostSite(Site).Header.HeaderPosition:=Node.HeaderPosition;
      TGlassDockHostSite(Site).DockRestoreBounds:=NewBounds;
    end;
    if Parent=nil then
    begin
      Site.WindowState:=Node.WindowState;
    end;
  end;

  function GetNodeSite(Node: TGlassDockLayoutTreeNode): TGlassDockHostSite;
  begin
    Result:=TGlassDockHostSite(fTreeNameToDocker[Node.Name]);
    if Result is TGlassDockHostSite then exit;
    if Result<>nil then
      exit(nil);
    Result:=CreateSite;
    fDisabledAutosizing.Add(Result);
    fTreeNameToDocker[Node.Name]:=Result;
  end;

  function Restore(Node: TGlassDockLayoutTreeNode; Parent: TWinControl): TControl;
  var
    AControl: TControl;
    Site: TGlassDockHostSite;
    Splitter: TGlassDockSplitter;
    i: Integer;
    Side: TAnchorKind;
    AnchorControl: TControl;
    ChildNode: TGlassDockLayoutTreeNode;
    NewBounds: TRect;
  begin
    Result:=nil;
    if Scale and SrcRectValid(Node.WorkAreaRect) then
      SrcWorkArea:=Node.WorkAreaRect;

    if Node.NodeType=adltnControl then begin
      // restore control
      // the control was already created
      // => dock it
      AControl:=FindControl(Node.Name);

      if AControl=nil then exit;

      DisableControlAutoSizing(AControl);
      if AControl.HostDockSite=nil then
        MakeDockable(AControl,false)
      else
        ClearLayoutProperties(AControl);
      Site:=AControl.HostDockSite as TGlassDockHostSite;

      AControl.Visible:=true;
      SetupSite(Site,Node,Parent);
      Result:=Site;
    end else if Node.NodeType=adltnCustomSite then begin
      // restore custom dock site
      // the control was already created
      // => position it
      AControl:=FindControl(Node.Name);

      if AControl=nil then  exit;

      if not IsCustomSite(AControl) then exit;

      DisableControlAutoSizing(AControl);
      SetupSite(TCustomForm(AControl),Node,nil);
      Result:=AControl;
      // restore docked site
      if Node.Count>0 then begin
        Restore(Node[0],TCustomForm(AControl));
      end;
    end else if Node.IsSplitter then begin
      // restore splitter
      Splitter:=TGlassDockSplitter(fTreeNameToDocker[Node.Name]);
      if Splitter=nil then begin
        Splitter:=CreateSplitter;
        fTreeNameToDocker[Node.Name]:=Splitter;
      end;

      Splitter.Parent:=Parent;
      NewBounds:=Node.BoundsRect;
      if SrcRectValid(SrcWorkArea) then
        {DaThoX NewBounds:=Rect(ScaleChildX(NewBounds.Left),ScaleChildY(NewBounds.Top),
          ScaleChildX(NewBounds.Right),ScaleChildY(NewBounds.Bottom));} ;
      Splitter.DockRestoreBounds:=NewBounds;
      Splitter.BoundsRect:=NewBounds;
      if Node.NodeType=adltnSplitterVertical then begin
        Splitter.ResizeAnchor:=akLeft;
        Splitter.AnchorSide[akLeft].Control:=nil;
        Splitter.AnchorSide[akRight].Control:=nil;
      end else begin
        Splitter.ResizeAnchor:=akTop;
        Splitter.AnchorSide[akTop].Control:=nil;
        Splitter.AnchorSide[akBottom].Control:=nil;
      end;
      Result:=Splitter;
    end else if Node.NodeType=adltnLayout then begin
      // restore layout
      Site:=GetNodeSite(Node);

      Site.BeginUpdateLayout;
      try
        SetupSite(Site,Node,Parent);
        Site.FSiteType:=adhstLayout;
        Site.Header.Parent:=nil;
        // create children
        for i:=0 to Node.Count-1 do
          Restore(Node[i],Site);
        // anchor children
        for i:=0 to Node.Count-1 do begin
          ChildNode:=Node[i];
          AControl:=fTreeNameToDocker[ChildNode.Name];

          if AControl=nil then continue;
          for Side:=Low(TAnchorKind) to high(TAnchorKind) do begin
            if ((ChildNode.NodeType=adltnSplitterHorizontal)
                and (Side in [akTop,akBottom]))
            or ((ChildNode.NodeType=adltnSplitterVertical)
                and (Side in [akLeft,akRight]))
            then continue;
            AnchorControl:=nil; ;
            if ChildNode.Anchors[Side]<>'' then begin
              AnchorControl:=fTreeNameToDocker[ChildNode.Anchors[Side]];

            end;
            if AnchorControl<>nil then
              AControl.AnchorToNeighbour(Side,0,AnchorControl)
            else
              AControl.AnchorParallel(Side,0,Site);
          end;
        end;
        
        //====================================== free unneeded helper controls (e.g. splitters)
        for i:=Site.ControlCount-1 downto 0 do begin
          AControl:=Site.Controls[i];
          if fTreeNameToDocker.ControlToName(AControl)<>'' then continue;
          if AControl is TGlassDockSplitter then
          begin
            AControl.Free;
          end;
        end;
      finally
        Site.EndUpdateLayout;
      end;
      Result:=Site;
    end else if Node.NodeType=adltnPages then begin
    
      //========================================= restore pages
      Site:=GetNodeSite(Node);

      Site.BeginUpdateLayout;
      try
        SetupSite(Site,Node,Parent);
        Site.FSiteType:=adhstPages;
        Site.Header.Parent:=nil;
        Site.CreatePages;
        for i:=0 to Node.Count-1 do begin
          Site.Pages.Pages.Add(Node[i].Name);
          AControl:=Restore(Node[i],Site.Pages.Page[i]);
          if AControl=nil then continue;
          AControl.Align:=alClient;
          for Side:=Low(TAnchorKind) to high(TAnchorKind) do
            AControl.AnchorSide[Side].Control:=nil;
        end;
      finally
        Site.EndUpdateLayout;
      end;
      Result:=Site;
    end else begin
      // create children
      for i:=0 to Node.Count-1 do
        Restore(Node[i],Parent);
    end;
  end;

begin
  Result:=true;
  WorkArea:=Rect(0,0,0,0);
  SrcWorkArea:=WorkArea;
  Restore(Tree.Root,nil);
  Restoring:=true;
end;


function TGlassDockMaster.DoCreateControl(aName: string; DisableAutoSizing: boolean): TControl;
begin
  Result := nil;
  OnCreateControl(Self, aName, Result, DisableAutoSizing);
end;

procedure TGlassDockMaster.DisableControlAutoSizing(AControl: TControl);
begin
  if fDisabledAutosizing.IndexOf(AControl) >= 0 then exit;

  fDisabledAutosizing.Add(AControl);
  AControl.FreeNotification(Self);
  AControl.DisableAutoSizing;
end;

procedure TGlassDockMaster.EnableAllAutoSizing;
var
  i: integer;
  AControl: TControl;
begin
  i := fDisabledAutosizing.Count - 1;

  for i := fDisabledAutosizing.Count - 1 downto 0 do
  begin
    AControl := TControl(fDisabledAutosizing[i]);
    fDisabledAutosizing.Delete(i);
    AControl.EnableAutoSizing;
  end;

end;

procedure TGlassDockMaster.ClearLayoutProperties(AControl: TControl; NewAlign: TAlign);
var
  a: TAnchorKind;
begin
  AControl.AutoSize:=false;
  AControl.Align:=NewAlign;
  AControl.BorderSpacing.Around := 0;
  AControl.BorderSpacing.Left := 0;
  AControl.BorderSpacing.Top := 0;
  AControl.BorderSpacing.Right := 0;
  AControl.BorderSpacing.Bottom := 0;
  AControl.BorderSpacing.InnerBorder := 0;
  for a := Low(TAnchorKind) to High(TAnchorKind) do
    AControl.AnchorSide[a].Control := nil;
end;

procedure TGlassDockMaster.PopupMenuPopup(Sender: TObject);
var
  Popup: TPopupMenu;
  ChangeLockItem: TMenuItem;
  ShowHeadersItem: TMenuItem;
begin
  if not (Sender is TPopupMenu) then
    exit;
  Popup := TPopupMenu(Sender);
  Popup.Items.Clear;

  // top popup menu item can be clicked by accident, so use something simple:
  // lock/unlock
  ChangeLockItem := AddPopupMenuItem('AnchorDockMasterChangeLockMenuItem', adrsLocked, @ChangeLockButtonClick);
  ChangeLockItem.Checked := not AllowDragging;
  ChangeLockItem.ShowAlwaysCheckable := True;

  if Popup.PopupComponent is TGlassDockHeader then
    TGlassDockHeader(Popup.PopupComponent).PopupMenuPopup(Sender)
  else if Popup.PopupComponent is TGlassDockPageControl then
    TGlassDockPageControl(Popup.PopupComponent).PopupMenuPopup(Sender)
  else if Popup.PopupComponent is TGlassDockSplitter then
    TGlassDockSplitter(Popup.PopupComponent).PopupMenuPopup(Sender);

  if ShowMenuItemShowHeader or (not ShowHeader) then
  begin
    ShowHeadersItem := AddPopupMenuItem('AnchorDockMasterShowHeaderMenuItem', adrsShowHeaders, @ShowHeadersButtonClick);
    ShowHeadersItem.Checked := ShowHeader;
    ShowHeadersItem.ShowAlwaysCheckable := True;
  end;

  if Assigned(OnShowOptions) then
    AddPopupMenuItem('OptionsMenuItem', adrsDockingOptions, @OptionsClick);
end;

procedure TGlassDockMaster.SetHideHeaderCaptionFloatingControl(const AValue: boolean);
var
  Site: TGlassDockHostSite;
  i: integer;
begin
  if AValue = HideHeaderCaptionFloatingControl then
    exit;
  fHideHeaderCaptionFloatingControl := AValue;
  for i := 0 to ComponentCount - 1 do
  begin
    Site := TGlassDockHostSite(Components[i]);
    if not (Site is TGlassDockHostSite) then
      continue;
    Site.UpdateDockCaption;
  end;
  OptionsChanged;
end;

procedure TGlassDockMaster.SetSplitterWidth(const AValue: integer);
var
  i: integer;
  Splitter: TGlassDockSplitter;
begin
  if (AValue < 1) or (AValue = SplitterWidth) then
    exit;
  FSplitterWidth := AValue;
  for i := 0 to ComponentCount - 1 do
  begin
    Splitter := TGlassDockSplitter(Components[i]);
    if not (Splitter is TGlassDockSplitter) then
      continue;
    if Splitter.ResizeAnchor in [akLeft, akRight] then
      Splitter.Width := SplitterWidth
    else
      Splitter.Height := SplitterWidth;
  end;
  OptionsChanged;
end;

procedure TGlassDockMaster.OnIdle(Sender: TObject; var Done: boolean);
begin
  if Done then
  ;
  IdleConnected := False;
  Restoring := False;
end;

procedure TGlassDockMaster.AsyncSimplify(Data: PtrInt);
begin
  FQueueSimplify := False;
  SimplifyPendingLayouts;
end;

procedure TGlassDockMaster.ChangeLockButtonClick(Sender: TObject);
begin
  AllowDragging := not AllowDragging;
end;

procedure TGlassDockMaster.SetAllowDragging(AValue: boolean);
begin
  if FAllowDragging = AValue then
    Exit;
  FAllowDragging := AValue;
  OptionsChanged;
end;

procedure TGlassDockMaster.SetDockOutsideMargin(AValue: integer);
begin
  if FDockOutsideMargin = AValue then
    Exit;
  FDockOutsideMargin := AValue;
  OptionsChanged;
end;

procedure TGlassDockMaster.SetDockParentMargin(AValue: integer);
begin
  if FDockParentMargin = AValue then
    Exit;
  FDockParentMargin := AValue;
  OptionsChanged;
end;

procedure TGlassDockMaster.SetDragTreshold(AValue: integer);
begin
  if FDragTreshold = AValue then
    Exit;
  FDragTreshold := AValue;
  OptionsChanged;
end;

procedure TGlassDockMaster.SetHeaderHint(AValue: string);
begin
  if FHeaderHint = AValue then
    Exit;
  FHeaderHint := AValue;
  OptionsChanged;
end;

procedure TGlassDockMaster.SetHeaderColor(AValue: TColor);
begin
  if FHeaderColor = AValue then
    Exit;
  FHeaderColor := AValue;
  OptionsChanged;
  InvalidateHeaders;
end;

procedure TGlassDockMaster.SetPageAreaInPercent(AValue: integer);
begin
  if FPageAreaInPercent = AValue then
    Exit;
  FPageAreaInPercent := AValue;
  OptionsChanged;
end;

procedure TGlassDockMaster.SetSaveOnClose(AValue: boolean);
begin
  if FSaveOnClose=AValue then Exit;
  FSaveOnClose:=AValue;
  OptionsChanged;
end;

procedure TGlassDockMaster.SetScaleOnResize(AValue: boolean);
begin
  if FScaleOnResize = AValue then
    Exit;
  FScaleOnResize := AValue;
  OptionsChanged;
end;

procedure TGlassDockMaster.SetShowMenuItemShowHeader(AValue: boolean);
begin
  if FShowMenuItemShowHeader = AValue then
    Exit;
  FShowMenuItemShowHeader := AValue;
  OptionsChanged;
end;

procedure TGlassDockMaster.ShowHeadersButtonClick(Sender: TObject);
begin
  ShowHeader := not ShowHeader;
end;

procedure TGlassDockMaster.OptionsClick(Sender: TObject);
begin
  if Assigned(OnShowOptions) then
    OnShowOptions(Self);
end;

procedure TGlassDockMaster.SetIdleConnected(const AValue: boolean);
begin
  if FIdleConnected = AValue then
    exit;
  FIdleConnected := AValue;
  if IdleConnected then
    Application.AddOnIdleHandler(@OnIdle, True)
  else
    Application.RemoveOnIdleHandler(@OnIdle);
end;

procedure TGlassDockMaster.SetQueueSimplify(const AValue: boolean);
begin
  if FQueueSimplify = AValue then
    exit;
  FQueueSimplify := AValue;
  if FQueueSimplify then
    Application.QueueAsyncCall(@AsyncSimplify, 0)
  else
    Application.RemoveAsyncCalls(Self);
end;

procedure TGlassDockMaster.SetRestoring(const AValue: boolean);
var
  AComponent: TComponent;
  i: integer;
begin
  if FRestoring = AValue then
    exit;
  FRestoring := AValue;
  if FRestoring then
  begin
    IdleConnected := True;
  end
  else
  begin
    for i := 0 to ComponentCount - 1 do
    begin
      AComponent := Components[i];
      if AComponent is TGlassDockHostSite then
        TGlassDockHostSite(AComponent).DockRestoreBounds := Rect(0, 0, 0, 0)
      else if AComponent is TGlassDockSplitter then
        TGlassDockSplitter(AComponent).DockRestoreBounds := Rect(0, 0, 0, 0);
    end;
  end;
end;

procedure TGlassDockMaster.OptionsChanged;
begin
  IncreaseOptionsChangeStamp;
  if Assigned(OnOptionsChanged) then
    OnOptionsChanged(Self);
end;

procedure TGlassDockMaster.SetHeaderButtonSize(const AValue: integer);
begin
  if FHeaderButtonSize = AValue then
    exit;
  FHeaderButtonSize := Max(1, AValue);
  FreeCloseButtonBitmap;
  if fCloseBtnReferenceCount > 0 then
    CreateCloseButtonBitmap;
  AutoSizeAllHeaders(True);
  OptionsChanged;
end;

procedure TGlassDockMaster.SetShowHeader(AValue: boolean);
var
  i: integer;
  Site: TGlassDockHostSite;
begin
  if FShowHeader = AValue then
    exit;
  FShowHeader := AValue;
  for i := 0 to ComponentCount - 1 do
  begin
    Site := TGlassDockHostSite(Components[i]);
    if not (Site is TGlassDockHostSite) then
      continue;
    if (Site.Header <> nil) then
    begin
      DisableControlAutoSizing(Site);
      Site.UpdateHeaderShowing;
    end;
  end;
  EnableAllAutoSizing;
  OptionsChanged;
end;

procedure TGlassDockMaster.SetShowHeaderCaption(const AValue: boolean);
var
  i: integer;
  Site: TGlassDockHostSite;
begin
  if FShowHeaderCaption = AValue then
    exit;
  FShowHeaderCaption := AValue;
  for i := 0 to ComponentCount - 1 do
  begin
    Site := TGlassDockHostSite(Components[i]);
    if not (Site is TGlassDockHostSite) then
      continue;
    Site.UpdateDockCaption;
  end;
  OptionsChanged;
end;

procedure TGlassDockMaster.Notification(AComponent: TComponent; Operation: TOperation);
var
  AControl: TControl;
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent is TControl then
    begin
      AControl := TControl(AComponent);
      FControls.Remove(AControl);
      fNeedSimplify.Remove(AControl);
      fNeedFree.Remove(AControl);
      fDisabledAutosizing.Remove(AControl);
      if fTreeNameToDocker <> nil then
        fTreeNameToDocker.RemoveControl(AControl);
    end;
  end;
end;

procedure TGlassDockMaster.FreeCloseButtonBitmap;
begin
  if fCloseBtnBitmap <> nil then
    fCloseBtnBitmap.Free;
  fCloseBtnBitmap := nil;
end;

procedure TGlassDockMaster.InvalidateHeaders;
var
  i: integer;
  Site: TGlassDockHostSite;
begin
  for i := 0 to ComponentCount - 1 do
  begin
    Site := TGlassDockHostSite(Components[i]);
    if not (Site is TGlassDockHostSite) then
      continue;
    if (Site.Header <> nil) and (Site.Header.Parent <> nil) then
      Site.Header.Invalidate;
  end;
end;

procedure TGlassDockMaster.AutoSizeAllHeaders(EnableAutoSizing: boolean);
var
  i: integer;
  Site: TGlassDockHostSite;
begin
  for i := 0 to ComponentCount - 1 do
  begin
    Site := TGlassDockHostSite(Components[i]);
    if not (Site is TGlassDockHostSite) then
      continue;
    if (Site.Header <> nil) and (Site.Header.Parent <> nil) then
    begin
      Site.Header.InvalidatePreferredSize;
      DisableControlAutoSizing(Site);
    end;
  end;
  if EnableAutoSizing then
    EnableAllAutoSizing;
end;

//=========================================== ct9999
procedure TGlassDockMaster.CreateCloseButtonBitmap;
var
  BitmapHandle, MaskHandle: HBITMAP;
  OrigBitmap: TCustomBitmap;
begin
  if fCloseBtnBitmap <> nil then
    exit;
  try
    OrigBitmap := CreateBitmapFromLazarusResource('glassdock_close_icon');

    if OrigBitmap = nil then
    begin
      if ThemeServices.GetStockImage(idButtonClose, BitmapHandle, MaskHandle) then
      begin
        OrigBitmap := TBitmap.Create;
        OrigBitmap.Handle := BitmapHandle;
        if MaskHandle <> 0 then
          OrigBitmap.MaskHandle := MaskHandle;
      end
      else
      begin
        OrigBitmap := GetDefaultButtonIcon(idButtonClose);
      end;
    end;

    //----------------------- ct9999
    if varDockMaster <> nil then
    begin
      varDockMaster.fCloseBtnBitmap := TBitmap.Create;
      with varDockMaster.fCloseBtnBitmap do
      begin
        SetSize(HeaderButtonSize, HeaderButtonSize);
        Canvas.Brush.Color := clWhite;
        Canvas.FillRect(Rect(0, 0, Width, Height));
        Canvas.StretchDraw(Rect(0, 0, Width, Height), OrigBitmap);
        Transparent := True;
        TransparentColor := clWhite;
      end;
    end;

  finally
    if OrigBitmap <> nil then
      OrigBitmap.Free;
  end;
end;

function TGlassDockMaster.ControlCount: integer;
begin
  Result := FControls.Count;
end;

function TGlassDockMaster.IndexOfControl(const aName: string): integer;
var
  i: integer;
begin
  Result := -9999;

  for i := 0 to ControlCount - 1 do
    if SameText(Controls[i].Name, aName) then
    begin
      Result := i;
      exit;
    end;
end;

function TGlassDockMaster.FindControl(const aName: string): TControl;
var
  i: longint;
begin
  i := IndexOfControl(aName);
  if i >= 0 then
    Result := Controls[i]
  else
    Result := nil;
end;

function TGlassDockMaster.IsSite(AControl: TControl): boolean;
begin
  Result := (AControl is TGlassDockHostSite) or IsCustomSite(AControl);
end;

function TGlassDockMaster.IsAnchorSite(AControl: TControl): boolean;
begin
  Result := AControl is TGlassDockHostSite;
end;

function TGlassDockMaster.IsCustomSite(AControl: TControl): boolean;
begin
  Result := (AControl is TCustomForm) // also checks for nil
    and (AControl.Parent = nil) and (TCustomForm(AControl).DockManager is TGlassDockManager);
end;

function TGlassDockMaster.GetSite(AControl: TControl): TCustomForm;
begin
  Result := nil;
  if AControl = nil then
    exit
  else if IsCustomSite(AControl) then
    Result := TCustomForm(AControl)
  else if AControl is TGlassDockHostSite then
    Result := TGlassDockHostSite(AControl)
  else if (AControl.HostDockSite is TGlassDockHostSite) then
    Result := TGlassDockHostSite(AControl.HostDockSite);
end;

function TGlassDockMaster.GetAnchorSite(AControl: TControl): TGlassDockHostSite;
begin
  Result := nil;
  if AControl = nil then
    Result := nil
  else if AControl is TGlassDockHostSite then
    Result := TGlassDockHostSite(AControl)
  else if (AControl.HostDockSite is TGlassDockHostSite) then
    Result := TGlassDockHostSite(AControl.HostDockSite);
end;

function TGlassDockMaster.GetControl(Site: TControl): TControl;
var
  AnchorSite: TGlassDockHostSite;
begin
  Result := nil;
  if IsCustomSite(Site) then
    Result := Site
  else if Site is TGlassDockHostSite then
  begin
    AnchorSite := TGlassDockHostSite(Site);
    if AnchorSite.SiteType = adhstOneControl then
      Result := AnchorSite.GetOneControl;
  end
  else if (Site <> nil) and (Site.HostDockSite is TGlassDockHostSite) and (TGlassDockHostSite(Site.HostDockSite).SiteType = adhstOneControl) then
    Result := Site;
end;

function TGlassDockMaster.IsFloating(AControl: TControl): boolean;
begin
  if AControl is TGlassDockHostSite then
  begin
    Result := (TGlassDockHostSite(AControl).SiteType = adhstOneControl) and (AControl.Parent = nil);
  end
  else if (AControl.HostDockSite is TGlassDockHostSite) then
  begin
    Result := (TGlassDockHostSite(AControl.HostDockSite).SiteType = adhstOneControl) and (AControl.HostDockSite.Parent = nil);
  end
  else
    Result := AControl.Parent = nil;
end;

function TGlassDockMaster.GetPopupMenu: TPopupMenu;
begin
  if fPopupMenu = nil then
  begin
    fPopupMenu := TPopupMenu.Create(Self);
    fPopupMenu.OnPopup := @PopupMenuPopup;
  end;
  Result := fPopupMenu;
end;

function TGlassDockMaster.AddPopupMenuItem(AName, ACaption: string; const OnClickEvent: TNotifyEvent; AParent: TMenuItem): TMenuItem;
begin
  Result := TMenuItem(fPopupMenu.FindComponent(AName));
  if Result = nil then
  begin
    Result := TMenuItem.Create(fPopupMenu);
    Result.Name := AName;
    if AParent = nil then
      fPopupMenu.Items.Add(Result)
    else
      AParent.Add(Result);
  end;
  Result.Caption := ACaption;
  Result.OnClick := OnClickEvent;
end;

function TGlassDockMaster.AddRemovePopupMenuItem(Add: boolean; AName, ACaption: string; const OnClickEvent: TNotifyEvent;
  AParent: TMenuItem): TMenuItem;
begin
  if Add then
    Result := AddPopupMenuItem(AName, ACaption, OnClickEvent, AParent)
  else
  begin
    Result := TMenuItem(fPopupMenu.FindComponent(AName));
    if Result <> nil then
      FreeAndNil(Result);
  end;
end;

procedure TGlassDockMaster.MakeDockable(AControl: TControl; Show: boolean; BringToFront: boolean; AddDockHeader: boolean); // 7777
var
  Site: TGlassDockHostSite;
begin
  if AControl.Name = ''     then exit;
  if (AControl is TCustomForm) and (fsModal in TCustomForm(AControl).FormState) then exit;
  if IsCustomSite(AControl) then exit;

  Site := nil;
  AControl.DisableAutoSizing;
  try
    if AControl is TGlassDockHostSite then
    begin
      // already a site
      Site := TGlassDockHostSite(AControl);
    end
    else if AControl.Parent = nil then
    begin

      if FControls.IndexOf(AControl) < 0 then
      begin
        FControls.Add(AControl);
        AControl.FreeNotification(Self);
      end;

      // create docksite
      Site := CreateSite;
      try
        try
          Site.BoundsRect := AControl.BoundsRect;
          ClearLayoutProperties(AControl);
          // dock
          AControl.ManualDock(Site);
          AControl.Visible := True;
          if not AddDockHeader then
            Site.Header.Parent := nil;
        except
          FreeAndNil(Site);
          exit; //  raise;
        end;
      finally
        if Site <> nil then
          Site.EnableAutoSizing;
      end;
    end
    else if AControl.Parent is TGlassDockHostSite then
    begin
      // AControl is already docked => show site
      Site := TGlassDockHostSite(AControl.Parent);
      AControl.Visible := True;
    end
    else
    begin
      exit;
    end;
    if (Site <> nil) and Show then
      MakeVisible(Site, BringToFront);
  finally
    AControl.EnableAutoSizing;
  end;
  // BringToFront
  if Show and BringToFront and (Site<>nil) then
  begin
    GetParentForm(Site).BringToFront;
    Site.SetFocus;
  end;
end;

procedure TGlassDockMaster.MakeDockSite(AForm: TCustomForm; Sites: TAnchors; ResizePolicy: TADMResizePolicy; AllowInside: boolean); // 7777
var
  AManager: TGlassDockManager;
begin
  if AForm.Name = ''            then exit;
  if AForm.DockManager <> nil   then exit;
  if AForm.Parent <> nil        then exit;
  if fsModal in AForm.FormState then exit;
  if Sites = []                 then exit;

  AForm.DisableAutoSizing;

  try
    if FControls.IndexOf(AForm) < 0 then
    begin
      FControls.Add(AForm);
      AForm.FreeNotification(Self);
    end;
    AManager := ManagerClass.Create(AForm);
    AManager.DockableSites := Sites;
    AManager.InsideDockingAllowed := AllowInside;
    AManager.ResizePolicy := ResizePolicy;
    AForm.DockManager := AManager;
    AForm.UseDockManager := True;
    AForm.DockSite := True;
  finally
    AForm.EnableAutoSizing;
  end;
end;

procedure TGlassDockMaster.MakeVisible(AControl: TControl; SwitchPages: boolean);
begin
  if AControl = nil then  exit;

  while AControl <> nil do
  begin
    AControl.Visible := True;
    if SwitchPages and (AControl is TGlassDockPage) then
      TGlassDockPageControl(AControl.Parent).PageIndex := TGlassDockPage(AControl).PageIndex;
    AControl := AControl.Parent;
  end;

end;

function TGlassDockMaster.ShowControl(ControlName: string; BringToFront: boolean): TControl;
begin
  Result := DoCreateControl(ControlName, False);
  if Result = nil then
    exit;
  MakeDockable(Result, True, BringToFront);
end;

procedure TGlassDockMaster.CloseAll;
var
  i: integer;
  AForm: TCustomForm;
  AControl: TWinControl;
begin
  // hide all forms
  i := Screen.CustomFormCount - 1;

  for i := Screen.CustomFormCount - 1 downto 0 do
  begin
    AForm := GetParentForm(Screen.CustomForms[i]);
    if AForm <> nil then
      AForm.Hide;
  end;

  // close all forms except the MainForm
  for i := Screen.CustomFormCount - 1 downto 0 do
  begin
    AForm := Screen.CustomForms[i];
    if AForm <> nil then
      if (AForm <> Application.MainForm) and not AForm.IsParentOf(Application.MainForm) then
      begin
        AControl := AForm;
        while (AControl.Parent <> nil) and (AControl.Parent <> Application.MainForm) do
        begin
          AControl := AControl.Parent;
          if AControl is TCustomForm then
            AForm := TCustomForm(AControl);
        end;
        AForm.Close;
      end;
  end;

end;

procedure TGlassDockMaster.SaveMainLayoutToTree(LayoutTree: TGlassDockLayoutTree);
var
  i: integer;
  AControl: TControl;
  Site: TGlassDockHostSite;
  SavedSites: TFPList;
  LayoutNode: TGlassDockLayoutTreeNode;
  AForm: TCustomForm;
  VisibleControls: TStringList;
begin
  SavedSites := TFPList.Create;
  VisibleControls := TStringList.Create;

  try
    for i := 0 to ControlCount - 1 do
    begin
      AControl := Controls[i];

      if not DockedControlIsVisible(AControl) then continue;

      VisibleControls.Add(AControl.Name);
      AForm := GetParentForm(AControl);

      if AForm = nil then continue;

      if SavedSites.IndexOf(AForm) >= 0 then continue;

      SavedSites.Add(AForm);

      DebugWriteChildAnchors(AForm, True, True);

      if (AForm is TGlassDockHostSite) then
      begin
        Site := TGlassDockHostSite(AForm);
        LayoutNode := LayoutTree.NewNode(LayoutTree.Root);
        Site.SaveLayout(LayoutTree, LayoutNode);
      end
      else
      if IsCustomSite(AForm) then
      begin
        // custom dock site
        LayoutNode := LayoutTree.NewNode(LayoutTree.Root);
        LayoutNode.NodeType := adltnCustomSite;
        LayoutNode.Assign(AForm);

        // can have one normal dock site
        Site := nil;
        if AForm.DockManager <> nil then
          Site := TGlassDockHostSite(TGlassDockHostSite(TGlassDockManager(AForm.DockManager).GetChildSite)); // DaThoX

        if Site <> nil then
        begin
          LayoutNode := LayoutTree.NewNode(LayoutNode);
          Site.SaveLayout(LayoutTree, LayoutNode);
        end;

      end
      else
        exit;

    end;
    // remove invisible controls
    LayoutTree.Root.Simplify(VisibleControls);
  finally
    VisibleControls.Free;
    SavedSites.Free;
  end;
end;

procedure TGlassDockMaster.SaveSiteLayoutToTree(AForm: TCustomForm; LayoutTree: TGlassDockLayoutTree);
var
  LayoutNode: TGlassDockLayoutTreeNode;
  Site: TGlassDockHostSite;
begin
  if (AForm is TGlassDockHostSite) then
  begin
    Site := TGlassDockHostSite(AForm);
    Site.SaveLayout(LayoutTree, LayoutTree.Root);
  end
  else if IsCustomSite(AForm) then
  begin
    LayoutTree.Root.NodeType := adltnCustomSite;
    LayoutTree.Root.Assign(AForm);
    // can have one normal dock site
    Site := TGlassDockHostSite(TGlassDockManager(AForm.DockManager).GetChildSite); // DaThoX
    if Site <> nil then
    begin
      LayoutNode := LayoutTree.NewNode(LayoutTree.Root);
      Site.SaveLayout(LayoutTree, LayoutNode);
    end;
  end
  else
    exit;
end;

function TGlassDockMaster.CreateRestoreLayout(AControl: TControl): TGlassDockRestoreLayout;
{ Create a restore layout for AControl and its child controls.
  It contains the whole parent structure so that the restore knows where to
  put AControl.
}

  procedure AddControlNames(SubControl: TControl; RestoreLayout: TGlassDockRestoreLayout);
  var
    i: integer;
  begin
    if (FControls.IndexOf(SubControl) >= 0) and not RestoreLayout.HasControlName(SubControl.Name) then
      RestoreLayout.ControlNames.Add(SubControl.Name);
    if SubControl is TWinControl then
      for i := 0 to TWinControl(SubControl).ControlCount - 1 do
        AddControlNames(TWinControl(SubControl).Controls[i], RestoreLayout);
  end;

var
  AForm: TCustomForm;
begin
  if not IsSite(AControl) then exit;

  AForm := GetParentForm(AControl);
  Result := TGlassDockRestoreLayout.Create(TGlassDockLayoutTree.Create);
  if AForm = nil then exit;

  SaveSiteLayoutToTree(AForm, Result.Layout);
  AddControlNames(AControl, Result);
end;

procedure TGlassDockMaster.SaveLayoutToConfig(Config: TConfigStorage);
var
  Tree: TGlassDockLayoutTree;
begin
  Tree := TGlassDockLayoutTree.Create;
  try
    Config.AppendBasePath('MainConfig/');
    SaveMainLayoutToTree(Tree);
    Tree.SaveToConfig(Config);
    Config.UndoAppendBasePath;
    Config.AppendBasePath('Restores/');
    RestoreLayouts.SaveToConfig(Config);
    Config.UndoAppendBasePath;

  finally
    Tree.Free;
  end;
end;

function TGlassDockMaster.ConfigIsEmpty(Config: TConfigStorage): boolean;
begin
  Result := Config.GetValue('MainConfig/Nodes/ChildCount', 0) = 0;
end;

function TGlassDockMaster.LoadLayoutFromConfig(Config: TConfigStorage; Scale: boolean): boolean; // 7777
var
  Tree: TGlassDockLayoutTree;
  ControlNames: TStringList;
begin
  Result := False;
  ControlNames := TStringList.Create;
  fTreeNameToDocker := TADNameToControl.Create;
  Tree := TGlassDockLayoutTree.Create;
  try
    // load layout
    Config.AppendBasePath('MainConfig/');
    try
      Tree.LoadFromConfig(Config);
    finally
      Config.UndoAppendBasePath;
    end;

    // load restore layouts for hidden forms
    Config.AppendBasePath('Restores/');
    try
      RestoreLayouts.LoadFromConfig(Config);
    finally
      Config.UndoAppendBasePath;
    end;

    // close all unneeded forms/controls
    if not CloseUnneededControls(Tree) then
      exit;

    BeginUpdate;
    try
      // create all needed forms/controls
      if not CreateNeededControls(Tree, True, ControlNames) then
        exit;

      // simplify layouts
      ControlNames.Sort;
      Tree.Root.Simplify(ControlNames);

      // reuse existing sites to reduce flickering
      MapTreeToControls(Tree);

      // create sites
      RestoreLayout(Tree, Scale);
    finally
      EndUpdate;
    end;
  finally
    // clean up
    FreeAndNil(fTreeNameToDocker);
    ControlNames.Free;
    Tree.Free;
    // commit (this can raise an exception)
    EnableAllAutoSizing;
  end;
  Result := True;
end;

procedure TGlassDockMaster.LoadSettingsFromConfig(Config: TConfigStorage);
var
  Settings: TGlassDockSettings;
begin
  Settings := TGlassDockSettings.Create;
  try
    Settings.LoadFromConfig(Config);
    LoadSettings(Settings);
  finally
    Settings.Free;
  end;
end;

procedure TGlassDockMaster.SaveSettingsToConfig(Config: TConfigStorage);
var
  Settings: TGlassDockSettings;
begin
  Settings := TGlassDockSettings.Create;
  try
    SaveSettings(Settings);
    Settings.SaveToConfig(Config);
  finally
    Settings.Free;
  end;
end;

procedure TGlassDockMaster.LoadSettings(Settings: TGlassDockSettings);
begin
  DragTreshold     := Settings.DragTreshold;
  DockOutsideMargin := Settings.DockOutsideMargin;
  DockParentMargin := Settings.DockParentMargin;
  PageAreaInPercent := Settings.PageAreaInPercent;
  HeaderAlignTop := Settings.HeaderAlignTop;
  HeaderAlignLeft := Settings.HeaderAlignLeft;
  SplitterWidth := Settings.SplitterWidth;
  ScaleOnResize := Settings.ScaleOnResize;
  SaveOnClose   := Settings.SaveOnClose;
  ShowHeader := Settings.ShowHeader;
  ShowHeaderCaption := Settings.ShowHeaderCaption;
  HideHeaderCaptionFloatingControl  := Settings.HideHeaderCaptionFloatingControl;
  AllowDragging := Settings.AllowDragging;
  HeaderButtonSize := Settings.HeaderButtonSize;
  HeaderColor := Settings.HeaderColor;
end;

procedure TGlassDockMaster.SaveSettings(Settings: TGlassDockSettings);
begin
  Settings.DragTreshold := DragTreshold;
  Settings.DockOutsideMargin := DockOutsideMargin;
  Settings.DockParentMargin := DockParentMargin;
  Settings.PageAreaInPercent := PageAreaInPercent;
  Settings.HeaderAlignTop := HeaderAlignTop;
  Settings.HeaderAlignLeft := HeaderAlignLeft;
  Settings.SplitterWidth := SplitterWidth;
  Settings.ScaleOnResize := ScaleOnResize;
  Settings.SaveOnClose:=SaveOnClose;
  Settings.ShowHeader := ShowHeader;
  Settings.ShowHeaderCaption := ShowHeaderCaption;
  Settings.HideHeaderCaptionFloatingControl := HideHeaderCaptionFloatingControl;
  Settings.AllowDragging := AllowDragging;
  Settings.HeaderButtonSize := HeaderButtonSize;
  Settings.HeaderColor := HeaderColor;
end;

function TGlassDockMaster.SettingsAreEqual(Settings: TGlassDockSettings): boolean;
var
  Cur: TGlassDockSettings;
begin
  Cur := TGlassDockSettings.Create;
  try
    SaveSettings(Cur);
    Result := Cur.IsEqual(Settings);
  finally
    Cur.Free;
  end;
end;

procedure TGlassDockMaster.ManualFloat(AControl: TControl);
var
  Site: TGlassDockHostSite;
begin
  Site := GetAnchorSite(AControl);
  if Site = nil then
    exit;
  Site.Undock;
end;

procedure TGlassDockMaster.ManualDock(SrcSite: TGlassDockHostSite; TargetSite: TCustomForm; Align: TAlign; TargetControl: TControl);
var
  Site: TGlassDockHostSite;
  aManager: TGlassDockManager;
  DockObject: TDragDockObject;
begin

  if SrcSite = TargetSite then
    exit;

  if SrcSite.IsParentOf(TargetSite) then exit;
  if TargetSite.IsParentOf(SrcSite) then exit;

  if IsCustomSite(TargetSite) then
  begin
    aManager := TGlassDockManager(TargetSite.DockManager);
    Site := TGlassDockHostSite(aManager.GetChildSite); // DaThoX
    if Site = nil then
    begin

      BeginUpdate;
      try
        DockObject := TDragDockObject.Create(SrcSite);
        try
          DockObject.DropAlign := Align;
          DockObject.DockRect := SrcSite.BoundsRect;
          aManager.InsertControl(DockObject);
        finally
          DockObject.Free;
        end;
      finally
        EndUpdate;
      end;
      exit;
    end;
    // else: dock into child site of custom dock site
  end
  else
  begin
    // dock to or into TargetSite
    if not (TargetSite is TGlassDockHostSite) then exit;

    Site := TGlassDockHostSite(TargetSite);
  end;
  if AutoFreedIfControlIsRemoved(Site, SrcSite) then exit;

  BeginUpdate;
  try
    Site.ExecuteDock(SrcSite, TargetControl, Align);
  finally
    EndUpdate;
  end;
end;

function TGlassDockMaster.ManualEnlarge(Site: TGlassDockHostSite; Side: TAnchorKind; OnlyCheckIfPossible: boolean): boolean;
begin
  Result := (Site <> nil) and Site.EnlargeSide(Side, OnlyCheckIfPossible);
end;

procedure TGlassDockMaster.BeginUpdate;
begin
  Inc(fUpdateCount);
end;

procedure TGlassDockMaster.EndUpdate;
begin
  if fUpdateCount <= 0 then exit;

  Dec(fUpdateCount);
  if fUpdateCount = 0 then
    SimplifyPendingLayouts;
end;

procedure TGlassDockMaster.NeedSimplify(AControl: TControl);
begin
  if Self = nil then exit;
  if csDestroying in ComponentState then  exit;
  if csDestroying in AControl.ComponentState then  exit;
  if fNeedSimplify = nil then  exit;
  if fNeedSimplify.IndexOf(AControl) >= 0 then  exit;
  if not ((AControl is TGlassDockHostSite) or (AControl is TGlassDockPage)) then exit;
  if Application.Terminated then exit;

  fNeedSimplify.Add(AControl);
  AControl.FreeNotification(Self);
  QueueSimplify := True;
end;

procedure TGlassDockMaster.NeedFree(AControl: TControl);  // 7777
begin
  if fNeedFree.IndexOf(AControl) >= 0 then exit;
  if csDestroying in AControl.ComponentState then exit;

  fNeedFree.Add(AControl);
  AControl.DisableAutoSizing;
  AControl.Parent := nil;
  AControl.Visible := False;
end;

procedure TGlassDockMaster.SimplifyPendingLayouts;
var
  AControl: TControl;
  Changed: boolean;
  i: integer;
begin
  if fSimplifying or (fUpdateCount > 0) then exit;

  fSimplifying := True;
  try
    // simplify layout (do not free controls in this step, only mark them)
    repeat
      Changed := False;

      for i := fNeedSimplify.Count - 1 downto 0 do
      begin
        AControl := TControl(fNeedSimplify[i]);
        if (csDestroying in AControl.ComponentState) or (fNeedFree.IndexOf(AControl) >= 0) then
        begin
          fNeedSimplify.Delete(i);
          Changed := True;
        end
        else if (AControl is TGlassDockHostSite) then
        begin

          if not TGlassDockHostSite(AControl).UpdatingLayout then
          begin
            fNeedSimplify.Delete(i);
            Changed := True;
            if TGlassDockHostSite(AControl).SiteType = adhstNone then
            begin
              NeedFree(AControl);
            end
            else
            begin
              TGlassDockHostSite(AControl).Simplify;
            end;
          end;
        end
        else if AControl is TGlassDockPage then
        begin
          fNeedSimplify.Delete(i);
          Changed := True;
          NeedFree(AControl);
        end
        else
          exit;
      end;
    until not Changed;

    // free unneeded controls
    while fNeedFree.Count>0 do
      if csDestroying in TControl(fNeedFree[0]).ComponentState then
        fNeedFree.Delete(0)
      else
        TControl(fNeedFree[0]).Free;

  finally
    fSimplifying:=false;
  end;
end;

function TGlassDockMaster.AutoFreedIfControlIsRemoved(AControl, RemovedControl: TControl): boolean;
var
  ParentSite: TGlassDockHostSite;
  Page: TGlassDockPage;
  PageControl: TGlassDockPageControl;
  OtherPage: TGlassDockPage;
  Site, Site1, Site2: TGlassDockHostSite;
begin
  Result := False;
  if (RemovedControl = nil) or (AControl = nil) then
    exit;
  while RemovedControl <> nil do
  begin
    if RemovedControl = AControl then
      exit(True);
    if RemovedControl is TGlassDockPage then
    begin
      // a page will be removed
      Page := TGlassDockPage(RemovedControl);

      if not (Page.Parent is TGlassDockPageControl) then exit;

      PageControl := TGlassDockPageControl(Page.Parent);
      if PageControl.PageCount > 2 then exit;

      if PageControl.PageCount = 2 then
      begin
        // this pagecontrol will be replaced by the content of the other page
        if PageControl = AControl then
          exit(True);

        if PageControl.Page[0] = Page then
          OtherPage := PageControl.DockPages[1]  else
          OtherPage := PageControl.DockPages[0];

        // the other page will be removed (its content will be moved up)
        if OtherPage = AControl then exit(True);

        if (OtherPage.ControlCount > 0) then
        begin
          if (OtherPage.Controls[0] is TGlassDockHostSite) and (OtherPage.Controls[0] = RemovedControl) then
            exit(True); // the site of the other page will be removed (its content moved up)
        end;
        exit;
      end;
      // the last page of the pagecontrol is freed => the pagecontrol will be removed too
    end
    else if RemovedControl is TGlassDockPageControl then
    begin
      // the pagecontrol will be removed
      if not (RemovedControl.Parent is TGlassDockHostSite) then exit;
      // a pagecontrol is always the only child of a site
      // => the site will be removed too
    end
    else if RemovedControl is TGlassDockHostSite then
    begin
      // a site will be removed
      Site := TGlassDockHostSite(RemovedControl);
      if Site.Parent is TGlassDockPage then
      begin
        // a page has only one site
        // => the page will be removed too
      end
      else if Site.Parent is TGlassDockHostSite then
      begin
        ParentSite := TGlassDockHostSite(Site.Parent);
        if (ParentSite.SiteType = adhstOneControl) or ParentSite.IsOneSiteLayout(Site) then
        begin
          // the control of a OneControl site is removed => the ParentSite is freed too
        end
        else if ParentSite.SiteType = adhstLayout then
        begin
          if ParentSite.IsTwoSiteLayout(Site1, Site2) then
          begin
            // when there are two sites and one of them is removed
            // the content of the other will be moved up and then both sites are
            // removed
            if (Site1 = AControl) or (Site2 = AControl) then exit(True);
          end;
          exit; // removing only site will not free the layout
        end
        else
        begin
          exit;

        end;
      end
      else
        exit; // other classes will never be auto freed
    end
    else
    begin
      // control is not a site => check if control is in a OneControl site
      if not (RemovedControl.Parent is TGlassDockHostSite) then
        exit;
      ParentSite := TGlassDockHostSite(RemovedControl.Parent);
      if (ParentSite.SiteType <> adhstOneControl) then
        exit;
      if ParentSite.GetOneControl <> RemovedControl then
        exit;
      // the control of a OneControl site is removed => the site is freed too
    end;
    RemovedControl := RemovedControl.Parent;
  end;
end;

function TGlassDockMaster.CreateSite(NamePrefix: string; DisableAutoSizing: boolean): TGlassDockHostSite;
var
  i: integer;
  NewName: string;
begin
  Result := TGlassDockHostSite(SiteClass.NewInstance);
  Result.DisableAutoSizing;
  Result.CreateNew(Self, 1);
  i := 0;

  repeat
    Inc(i);
    NewName := NamePrefix + cnGlassDockSiteName + IntToStr(i);
  until (Screen.FindForm(NewName) = nil) and (FindComponent(NewName) = nil);

  Result.Name := NewName;

  if not DisableAutoSizing then
    Result.EnableAutoSizing;
end;

function TGlassDockMaster.CreateSplitter(NamePrefix: string): TGlassDockSplitter;
var
  i: integer;
  NewName: string;
begin
  Result := SplitterClass.Create(Self);
  i := 0;
  repeat
    Inc(i);
    NewName := NamePrefix + cnGlassDockSplitterName + IntToStr(i);
  until FindComponent(NewName) = nil;

  Result.Name := NewName;
end;

procedure TGlassDockMaster.IncreaseOptionsChangeStamp;
begin
  LUIncreaseChangeStamp64(FOptionsChangeStamp);
end;

//======================== TGlassDockHostSite =============================================

procedure TGlassDockHostSite.SetHeaderSide(const AValue: TAnchorKind);
begin
  if FHeaderSide = AValue then
    exit;
  FHeaderSide := AValue;
end;

procedure TGlassDockHostSite.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = Pages then
      FPages := nil;
    if AComponent = Header then
      FHeader := nil;
    if AComponent = BoundSplitter then
      FBoundSplitter := nil;
  end;
end;

function TGlassDockHostSite.DoDockClientMsg(DragDockObject: TDragDockObject; aPosition: TPoint): boolean;
begin
  if aPosition.X = 0 then
  ;
  Result := ExecuteDock(DragDockObject.Control, DragDockObject.DropOnControl, DragDockObject.DropAlign);
end;

function TGlassDockHostSite.ExecuteDock(NewControl, DropOnControl: TControl; DockAlign: TAlign): boolean;
begin
  if UpdatingLayout then
    exit;

  DisableAutoSizing;
  try
    BeginUpdateLayout;
    try
      VarDockMaster.SimplifyPendingLayouts;
      NewControl.DisableAutoSizing;

      if (NewControl.Parent = Self) and (SiteType = adhstLayout) then
      begin
        // change of layout, one child is docked to the outer side
        RemoveControlFromLayout(NewControl);
      end
      else if (NewControl.Parent = Parent) and (Parent is TGlassDockHostSite) and (TGlassDockHostSite(Parent).SiteType = adhstLayout) then
      begin
        // change of layout, one sibling is moved
        TGlassDockHostSite(Parent).RemoveControlFromLayout(NewControl);
      end;

      if SiteType = adhstNone then
      begin
        // make a control dockable by docking it into a TGlassDockHostSite;
        Result := DockFirstControl(NewControl);
      end
      else if DockAlign = alClient then
      begin
        // page docking
        if SiteType = adhstOneControl then
        begin
          if Parent is TGlassDockPage then
          begin
            // add as sibling page
            Result := (Parent.Parent.Parent as TGlassDockHostSite).DockAnotherPage(NewControl, nil);
          end
          else
            // create pages
            Result := DockSecondPage(NewControl);
        end
        else if SiteType = adhstPages then
          // add as sibling page
          Result := DockAnotherPage(NewControl, DropOnControl);
      end
      else if DockAlign in [alLeft, alTop, alRight, alBottom] then
      begin
        // anchor docking
        if SiteType = adhstOneControl then
        begin
          if Parent is TGlassDockHostSite then
          begin
            // add site as sibling
            Result := TGlassDockHostSite(Parent).DockAnotherControl(Self, NewControl, DockAlign, DropOnControl <> nil);
          end
          else
            // create layout
            Result := DockSecondControl(NewControl, DockAlign, DropOnControl <> nil);
        end
        else if SiteType = adhstLayout then
          // add site as sibling
          Result := DockAnotherControl(nil, NewControl, DockAlign, DropOnControl <> nil);
      end;

      NewControl.EnableAutoSizing;
    finally
      EndUpdateLayout;
    end;
  finally
    EnableAutoSizing;
  end;
end;

function TGlassDockHostSite.DockFirstControl(NewControl: TControl): boolean;
var
  DestRect: TRect;
begin
  if SiteType <> adhstNone then exit;

  // create adhstOneControl
  DestRect := ClientRect;
  NewControl.Dock(Self, DestRect);
  FSiteType := adhstOneControl;
  if NewControl is TCustomForm then
  begin
    Icon.Assign(TCustomForm(NewControl).Icon);
  end;
  Result := True;
end;

function TGlassDockHostSite.DockSecondControl(NewControl: TControl; DockAlign: TAlign; Inside: boolean): boolean;
var
  OldSite: TGlassDockHostSite;
  OldControl: TControl;
begin
  Result := True;

  if SiteType <> adhstOneControl then exit;
  if not (DockAlign in [alLeft, alTop, alRight, alBottom]) then exit;

  FSiteType := adhstLayout;

  // remove header (keep it for later use)
  Header.Parent := nil;

  // put the OldControl into a site of its own (OldSite) and dock OldSite
  OldControl := GetOneControl;
  OldSite := MakeSite(OldControl);
  AddCleanControl(OldSite);
  OldSite.AnchorClient(0);
  // the LCL will compute the bounds later after EnableAutoSizing
  // but the bounds are needed now => set them manually
  OldSite.BoundsRect := Rect(0, 0, ClientWidth, ClientHeight);

  Result := DockAnotherControl(OldSite, NewControl, DockAlign, Inside);

end;

function TGlassDockHostSite.DockAnotherControl(Sibling, NewControl: TControl; DockAlign: TAlign; Inside: boolean): boolean;
var
  Splitter: TGlassDockSplitter;
  a: TAnchorKind;
  NewSite: TGlassDockHostSite;
  NewBounds: TRect;
  MainAnchor: TAnchorKind;
  i: integer;
  NewSiblingWidth: integer;
  NewSiblingHeight: integer;
  NewSize: longint;
  BoundsIncreased: boolean;
  NewParentBounds: TRect;
begin
  Result := False;
  if VarDockMaster = nil then
    exit;

  if SiteType <> adhstLayout then exit;
  if not (DockAlign in [alLeft, alTop, alRight, alBottom]) then exit;

  // add a splitter
  Splitter := VarDockMaster.CreateSplitter;
  if DockAlign in [alLeft, alRight] then
  begin
    Splitter.ResizeAnchor := akLeft;
    Splitter.Width := VarDockMaster.SplitterWidth;
  end
  else
  begin
    Splitter.ResizeAnchor := akTop;
    Splitter.Height := VarDockMaster.SplitterWidth;
  end;
  Splitter.Parent := Self;

  // dock the NewControl
  NewSite := MakeSite(NewControl);
  AddCleanControl(NewSite);

  BoundsIncreased := False;
  if (not Inside) then
  begin
    if (Parent = nil) then
    begin
      // expand Self
      NewBounds := BoundsRect;
      case DockAlign of
        alLeft:
        begin
          Dec(NewBounds.Left, NewSite.Width + Splitter.Width);
          MoveAllControls(NewSite.Width + Splitter.Width, 0);
        end;
        alRight:
          Inc(NewBounds.Right, NewSite.Width + Splitter.Width);
        alTop:
        begin
          Dec(NewBounds.Top, NewSite.Height + Splitter.Height);
          MoveAllControls(0, NewSite.Height + Splitter.Height);
        end;
        alBottom:
          Inc(NewBounds.Bottom, NewSite.Height + Splitter.Height);
      end;
      BoundsRect := NewBounds;
      BoundsIncreased := True;
    end
    else if VarDockMaster.IsCustomSite(Parent) then
    begin
      // Parent is a custom docksite
      // => expand Self and Parent
      // expand Parent (the custom docksite)
      NewParentBounds := Parent.BoundsRect;
      NewBounds := BoundsRect;
      case DockAlign of
        alLeft:
        begin
          i := NewSite.Width + Splitter.Width;
          Dec(NewParentBounds.Left, i);
          Dec(NewBounds.Left, i);
          MoveAllControls(i, 0);
        end;
        alRight:
        begin
          i := NewSite.Width + Splitter.Width;
          Inc(NewBounds.Right, i);
          Inc(NewParentBounds.Right, i);
        end;
        alTop:
        begin
          i := NewSite.Height + Splitter.Height;
          Dec(NewBounds.Top, i);
          Dec(NewParentBounds.Top, i);
          MoveAllControls(0, i);
        end;
        alBottom:
        begin
          i := NewSite.Height + Splitter.Height;
          Inc(NewParentBounds.Bottom, i);
          Inc(NewBounds.Bottom, i);
        end;
      end;
      Parent.BoundsRect := NewParentBounds;
      BoundsRect := NewBounds;
      BoundsIncreased := True;
      TGlassDockManager(Parent.DockManager).FSiteClientRect := Parent.ClientRect;
    end;

  end;

  // anchors
  MainAnchor := MainAlignAnchor[DockAlign];
  if Inside and (Sibling <> nil) then
  begin
    for a := low(TAnchorKind) to high(TAnchorKind) do
    begin
      if a in AnchorAlign[DockAlign] then
      begin
        NewSite.AnchorSide[a].Assign(Sibling.AnchorSide[a]);
      end
      else
      begin
        NewSite.AnchorToNeighbour(a, 0, Splitter);
      end;
    end;
    Sibling.AnchorToNeighbour(MainAnchor, 0, Splitter);

    if DockAlign in [alLeft, alRight] then
    begin
      Splitter.AnchorSide[akTop].Assign(Sibling.AnchorSide[akTop]);
      Splitter.AnchorSide[akBottom].Assign(Sibling.AnchorSide[akBottom]);
      // resize and move
      // the NewSite gets at maximum half the space
      // Many bounds are later set by the LCL anchoring. When docking several
      // controls at once the bounds are needed earlier.
      NewSize := Max(1, Min(NewSite.Width, Sibling.Width div 2));
      NewBounds := Rect(0, 0, NewSize, Sibling.Height);
      NewSiblingWidth := Max(1, Sibling.Width - NewSize - Splitter.Width);
      if DockAlign = alLeft then
      begin
        // alLeft: NewControl, Splitter, Sibling
        Splitter.SetBounds(Sibling.Left + NewSize, Sibling.Top,
          Splitter.Width, Sibling.Height);
        OffsetRect(NewBounds, Sibling.Left, Sibling.Top);
        Sibling.SetBounds(Splitter.Left + Splitter.Width, Sibling.Top,
          NewSiblingWidth, Sibling.Height);
      end
      else
      begin
        // alRight: Sibling, Splitter, NewControl
        Sibling.Width := NewSiblingWidth;
        Splitter.SetBounds(Sibling.Left + Sibling.Width, Sibling.Top,
          Splitter.Width, Sibling.Height);
        OffsetRect(NewBounds, Splitter.Left + Splitter.Width, Sibling.Top);
      end;
      NewSite.BoundsRect := NewBounds;
    end
    else
    begin
      Splitter.AnchorSide[akLeft].Assign(Sibling.AnchorSide[akLeft]);
      Splitter.AnchorSide[akRight].Assign(Sibling.AnchorSide[akRight]);
      // resize and move
      // the NewSite gets at maximum half the space
      // Many bounds are later set by the LCL anchoring. When docking several
      // controls at once the bounds are needed earlier.
      NewSize := Max(1, Min(NewSite.Height, Sibling.Height div 2));
      NewSiblingHeight := Max(1, Sibling.Height - NewSize - Splitter.Height);
      if DockAlign = alTop then
      begin
        // alTop: NewControl, Splitter, Sibling
        Splitter.SetBounds(Sibling.Left, Sibling.Top + NewSize,
          Sibling.Width, Splitter.Height);
        NewSite.SetBounds(Sibling.Left, Sibling.Top, Sibling.Width, NewSize);
        Sibling.SetBounds(Sibling.Left, Splitter.Top + Splitter.Height,
          Sibling.Width, NewSiblingHeight);
      end
      else
      begin
        // alBottom: Sibling, Splitter, NewControl
        Sibling.Height := NewSiblingHeight;
        Splitter.SetBounds(Sibling.Left, Sibling.Top + Sibling.Height,
          Sibling.Width, Splitter.Height);
        NewSite.SetBounds(Sibling.Left, Splitter.Top + Splitter.Height,
          Sibling.Width, NewSize);
      end;
    end;
  end
  else
  begin
    if DockAlign in [alLeft, alRight] then
      NewSize := NewSite.Width
    else
      NewSize := NewSite.Height;

    for i := 0 to ControlCount - 1 do
    begin
      Sibling := Controls[i];
      if Sibling.AnchorSide[MainAnchor].Control = Self then
      begin
        // this Sibling is anchored to the docked site
        // anchor it to the splitter
        Sibling.AnchorToNeighbour(MainAnchor, 0, Splitter);
        if not BoundsIncreased then
        begin
          // the NewSite gets at most half the space
          if DockAlign in [alLeft, alRight] then
            NewSize := Min(NewSize, Sibling.Width div 2)
          else
            NewSize := Min(NewSize, Sibling.Height div 2);
        end;
      end;
    end;
    NewSize := Max(1, NewSize);

    // anchor Splitter and NewSite
    a := ClockwiseAnchor[MainAnchor];
    Splitter.AnchorParallel(a, 0, Self);
    Splitter.AnchorParallel(OppositeAnchor[a], 0, Self);
    NewSite.AnchorParallel(a, 0, Self);
    NewSite.AnchorParallel(OppositeAnchor[a], 0, Self);
    NewSite.AnchorParallel(MainAnchor, 0, Self);
    NewSite.AnchorToNeighbour(OppositeAnchor[MainAnchor], 0, Splitter);

    // Many bounds are later set by the LCL anchoring. When docking several
    // controls at once the bounds are needed earlier.
    if DockAlign in [alLeft, alRight] then
    begin
      if DockAlign = alLeft then
      begin
        // alLeft: NewSite, Splitter, other siblings
        Splitter.SetBounds(NewSize, 0, Splitter.Width, ClientHeight);
        NewSite.SetBounds(0, 0, NewSize, ClientHeight);
      end
      else
      begin
        // alRight: other siblings, Splitter, NewSite
        NewSite.SetBounds(ClientWidth - NewSize, 0, NewSize, ClientHeight);
        Splitter.SetBounds(NewSite.Left - Splitter.Width, 0, Splitter.Width, ClientHeight);
      end;
    end
    else
    begin
      if DockAlign = alTop then
      begin
        // alTop: NewSite, Splitter, other siblings
        Splitter.SetBounds(0, NewSize, ClientWidth, Splitter.Height);
        NewSite.SetBounds(0, 0, ClientWidth, NewSize);
      end
      else
      begin
        // alBottom: other siblings, Splitter, NewSite
        NewSite.SetBounds(0, ClientHeight - NewSize, ClientWidth, NewSize);
        Splitter.SetBounds(0, NewSite.Top - Splitter.Height, ClientWidth, Splitter.Height);
      end;
    end;
    // shrink siblings
    for i := 0 to ControlCount - 1 do
    begin
      Sibling := Controls[i];
      if Sibling.AnchorSide[MainAnchor].Control = Splitter then
      begin
        NewBounds := Sibling.BoundsRect;
        case DockAlign of
          alLeft: NewBounds.Left := Splitter.Left + Splitter.Width;
          alRight: NewBounds.Right := Splitter.Left;
          alTop: NewBounds.Top := Splitter.Top + Splitter.Height;
          alBottom: NewBounds.Bottom := Splitter.Top;
        end;
        NewBounds.Right := Max(NewBounds.Left + 1, NewBounds.Right);
        NewBounds.Bottom := Max(NewBounds.Top + 1, NewBounds.Bottom);
        Sibling.BoundsRect := NewBounds;
      end;
    end;
  end;

  Result := True;
end;

procedure TGlassDockHostSite.CreatePages;
begin
  if VarDockMaster = nil then exit;

  if FPages<>nil then exit;

  FPages := VarDockMaster.PageControlClass.Create(nil);
  FPages.FreeNotification(Self);
  FPages.Parent := Self;
  FPages.Align := alClient;
end;

function TGlassDockHostSite.DockSecondPage(NewControl: TControl): boolean;
var
  OldControl: TControl;
  OldSite: TGlassDockHostSite;
begin
  if SiteType <> adhstOneControl then exit;

  FSiteType := adhstPages;
  CreatePages;

  Header.Parent := nil;

  OldControl := GetOneControl;
  OldSite := MakeSite(OldControl);
  OldSite.HostDockSite := nil;
  FPages.Pages.Add(OldSite.Caption);
  OldSite.Parent := FPages.Page[0];
  OldSite.Align := alClient;
  OldSite.Visible := True;

  Result := DockAnotherPage(NewControl, nil);
end;

function TGlassDockHostSite.DockAnotherPage(NewControl: TControl; InFrontOf: TControl): boolean;
var
  NewSite: TGlassDockHostSite;
  NewIndex: longint;
begin
  if SiteType <> adhstPages then exit;

  NewSite := MakeSite(NewControl);
  NewIndex := FPages.PageCount;
  if (InFrontOf is TGlassDockPage) and (InFrontOf.Parent = Pages) then
    NewIndex := TGlassDockPage(InFrontOf).PageIndex;
  Pages.Pages.Insert(NewIndex, NewSite.Caption);
  NewSite.Parent := FPages.Page[NewIndex];
  NewSite.Align := alClient;
  NewSite.Visible := True;
  FPages.PageIndex := NewIndex;

  Result := True;
end;

procedure TGlassDockHostSite.AddCleanControl(AControl: TControl; TheAlign: TAlign);
var
  a: TAnchorKind;
begin
  AControl.Parent := Self;
  AControl.Align := TheAlign;
  AControl.Anchors := [akLeft, akTop, akRight, akBottom];
  for a := Low(TAnchorKind) to high(TAnchorKind) do
    AControl.AnchorSide[a].Control := nil;
  AControl.Visible := True;
end;

procedure TGlassDockHostSite.RemoveControlFromLayout(AControl: TControl);

  procedure RemoveControlBoundSplitter(Splitter: TGlassDockSplitter; Side: TAnchorKind);
  var
    i: integer;
    Sibling: TControl;
    NewBounds: TRect;
  begin
    for i := Splitter.AnchoredControlCount - 1 downto 0 do
    begin
      Sibling := Splitter.AnchoredControls[i];
      if Sibling.AnchorSide[Side].Control = Splitter then
      begin
        // anchor Sibling to next
        Sibling.AnchorSide[Side].Assign(AControl.AnchorSide[Side]);
        // enlarge Sibling
        NewBounds := Sibling.BoundsRect;
        case Side of
          akTop: NewBounds.Top := AControl.Top;
          akLeft: NewBounds.Left := AControl.Left;
          akRight: NewBounds.Right := AControl.Left + AControl.Width;
          akBottom: NewBounds.Bottom := AControl.Top + AControl.Height;
        end;
        Sibling.BoundsRect := NewBounds;
      end;
    end;
    Splitter.Free;

    ClearChildControlAnchorSides(AControl);
  end;

  procedure ConvertToOneControlType(OnlySiteLeft: TGlassDockHostSite);
  var
    a: TAnchorKind;
    NewBounds: TRect;
    p: TPoint;
    i: integer;
    Sibling: TControl;
    NewParentBounds: TRect;
  begin
    BeginUpdateLayout;
    try
      // remove splitter
      for i := ControlCount - 1 downto 0 do
      begin
        Sibling := Controls[i];
        if Sibling is TGlassDockSplitter then
          Sibling.Free
        else if Sibling is TGlassDockHostSite then
          for a := low(TAnchorKind) to high(TAnchorKind) do
            Sibling.AnchorSide[a].Control := nil;
      end;
      if (Parent = nil) then
      begin
        // shrink this site
        NewBounds := OnlySiteLeft.BoundsRect;
        p := ClientOrigin;
        OffsetRect(NewBounds, p.x, p.y);
        BoundsRect := NewBounds;
      end
      else if VarDockMaster.IsCustomSite(Parent) then
      begin
        // parent is a custom dock site
        // shrink this site and the parent
        NewParentBounds := Parent.BoundsRect;
        case Align of
          alTop:
          begin
            Inc(NewParentBounds.Top, Height - OnlySiteLeft.Height);
            Width := Parent.ClientWidth;
            Height := OnlySiteLeft.Height;
          end;
          alBottom:
          begin
            Dec(NewParentBounds.Bottom, Height - OnlySiteLeft.Height);
            Width := Parent.ClientWidth;
            Height := OnlySiteLeft.Height;
          end;
          alLeft:
          begin
            Inc(NewParentBounds.Left, Width - OnlySiteLeft.Width);
            Width := OnlySiteLeft.Width;
            Height := Parent.ClientHeight;
          end;
          alRight:
          begin
            Dec(NewParentBounds.Right, Width - OnlySiteLeft.Width);
            Width := OnlySiteLeft.Width;
            Height := Parent.ClientHeight;
          end;
        end;
        Parent.BoundsRect := NewParentBounds;
      end;

      // change type
      FSiteType := adhstOneControl;
      OnlySiteLeft.Align := alClient;
      Header.Parent := Self;
      UpdateHeaderAlign;

      VarDockMaster.NeedSimplify(Self);
    finally
      EndUpdateLayout;
    end;
  end;

var
  Side: TAnchorKind;
  Splitter: TGlassDockSplitter;
  OnlySiteLeft: TGlassDockHostSite;
  Sibling: TControl;
  SplitterCount: integer;
begin
  if SiteType <> adhstLayout then exit;

  if IsOneSiteLayout(OnlySiteLeft) then
  begin
    ClearChildControlAnchorSides(AControl);
    ConvertToOneControlType(OnlySiteLeft);
    exit;
  end;

  // remove a splitter and fill the gap
  SplitterCount := 0;
  for Side := Low(TAnchorKind) to high(TAnchorKind) do
  begin
    Sibling := AControl.AnchorSide[OppositeAnchor[Side]].Control;
    if Sibling is TGlassDockSplitter then
    begin
      Inc(SplitterCount);
      Splitter := TGlassDockSplitter(Sibling);
      if Splitter.SideAnchoredControlCount(Side) = 1 then
      begin
        // Splitter is only used by AControl at Side
        RemoveControlBoundSplitter(Splitter, Side);
        exit;
      end;
    end;
  end;

  if SplitterCount = 4 then
  begin
    RemoveSpiralSplitter(AControl);
    exit;
  end;

  ClearChildControlAnchorSides(AControl);
end;

procedure TGlassDockHostSite.RemoveSpiralSplitter(AControl: TControl);
var
  Splitters: array[TAnchorKind] of TGlassDockSplitter;
  Side: TAnchorKind;
  Keep: TAnchorKind;
  DeleteSplitter: TGlassDockSplitter;
  i: integer;
  Sibling: TControl;
  NextSide: TAnchorKind;
  NewBounds: TRect;
begin
  for Side := low(TAnchorKind) to high(TAnchorKind) do
    Splitters[Side] := AControl.AnchorSide[Side].Control as TGlassDockSplitter;
  // Prefer the pair with shortest distance between
  if (Splitters[akRight].Left - Splitters[akLeft].Left) < (Splitters[akBottom].Top - Splitters[akTop].Top) then
    Keep := akLeft
  else
    Keep := akTop;
  DeleteSplitter := Splitters[OppositeAnchor[Keep]];
  // transfer anchors from the deleting splitter to the kept splitter
  for i := 0 to ControlCount - 1 do
  begin
    Sibling := Controls[i];
    for Side := low(TAnchorKind) to high(TAnchorKind) do
    begin
      if Sibling.AnchorSide[Side].Control = DeleteSplitter then
        Sibling.AnchorSide[Side].Control := Splitters[Keep];
    end;
  end;
  // longen kept splitter
  NextSide := ClockwiseAnchor[Keep];
  if Splitters[Keep].AnchorSide[NextSide].Control <> Splitters[NextSide] then
    NextSide := OppositeAnchor[NextSide];
  Splitters[Keep].AnchorSide[NextSide].Control :=
    DeleteSplitter.AnchorSide[NextSide].Control;
  case NextSide of
    akTop: Splitters[Keep].Top := DeleteSplitter.Top;
    akLeft: Splitters[Keep].Left := DeleteSplitter.Left;
    akRight: Splitters[Keep].Width := DeleteSplitter.Left + DeleteSplitter.Width - Splitters[Keep].Left;
    akBottom: Splitters[Keep].Height := DeleteSplitter.Top + DeleteSplitter.Height - Splitters[Keep].Top;
  end;

  // move splitter to the middle
  if Keep = akLeft then
    Splitters[Keep].Left := (Splitters[Keep].Left + DeleteSplitter.Left) div 2
  else
    Splitters[Keep].Top := (Splitters[Keep].Top + DeleteSplitter.Top) div 2;
  // adjust all anchored controls
  for i := 0 to ControlCount - 1 do
  begin
    Sibling := Controls[i];
    for Side := low(TAnchorKind) to high(TAnchorKind) do
    begin
      if Sibling.AnchorSide[Side].Control = Splitters[Keep] then
      begin
        NewBounds := Sibling.BoundsRect;
        case Side of
          akTop: NewBounds.Top := Splitters[Keep].Top + Splitters[Keep].Height;
          akLeft: NewBounds.Left := Splitters[Keep].Left + Splitters[Keep].Width;
          akRight: NewBounds.Right := Splitters[Keep].Left;
          akBottom: NewBounds.Bottom := Splitters[Keep].Top;
        end;
        Sibling.BoundsRect := NewBounds;
      end;
    end;
  end;

  // delete the splitter
  DeleteSplitter.Free;

  ClearChildControlAnchorSides(AControl);
end;

procedure TGlassDockHostSite.ClearChildControlAnchorSides(AControl: TControl);
var
  Side: TAnchorKind;
  Sibling: TControl;
begin
  for Side := Low(TAnchorKind) to high(TAnchorKind) do
  begin
    Sibling := AControl.AnchorSide[Side].Control;

    if (Sibling = nil) then continue;

    if (Sibling.Parent = Self) then
      AControl.AnchorSide[Side].Control := nil;
  end;
end;

procedure TGlassDockHostSite.Simplify;
var
  AControl: TControl;
begin
  if (Pages <> nil) and (Pages.PageCount = 1) then
    SimplifyPages
  else if (SiteType = adhstOneControl) then
  begin
    AControl := GetOneControl;
    if AControl is TGlassDockHostSite then
      SimplifyOneControl else
      if (AControl = nil) or (csDestroying in AControl.ComponentState) then
       VarDockMaster.NeedFree(Self);
  end;
end;

procedure TGlassDockHostSite.SimplifyPages;
var
  Page: TGlassDockPage;
  Site: TGlassDockHostSite;
begin
  if Pages = nil then
    exit;
  if Pages.PageCount = 1 then
  begin
    DisableAutoSizing;
    BeginUpdateLayout;
    try
      // move the content of the Page to the place where Pages is
      Page := Pages.DockPages[0];
      Site := Page.GetSite;
      Site.Parent := Self;
      if Site <> nil then  CopyAnchorBounds(Pages, Site);
      if SiteType = adhstPages then FSiteType := adhstOneControl;

      // free Pages
      FreeAndNil(FPages);
      if SiteType = adhstOneControl then SimplifyOneControl;
    finally
      EndUpdateLayout;
      EnableAutoSizing;
    end;
  end
  else if Pages.PageCount = 0 then
  begin
    FSiteType := adhstNone;
    FreeAndNil(FPages);
    VarDockMaster.NeedSimplify(Self);
  end;
end;

procedure TGlassDockHostSite.SimplifyOneControl;
var
  Site: TGlassDockHostSite;
  i: integer;
  Child: TControl;
  a: TAnchorKind;
begin
  if SiteType <> adhstOneControl then exit;
  if not IsOneSiteLayout(Site) then exit;

  if Site = nil then  exit;

  DisableAutoSizing;
  BeginUpdateLayout;
  try
    // move the content of Site up and free Site
    // Note: it is not possible to do it the other way round, because moving a
    // form to screen changes the z order and focus
    FSiteType := Site.SiteType;

    // header
    Header.Align := Site.Header.Align;
    Header.Caption := Site.Header.Caption;
    UpdateHeaderShowing;
    Caption := Site.Caption;

    Site.BeginUpdateLayout;

    // move controls from Site to Self

    for i := Site.ControlCount - 1 downto 0 do
    begin
      Child := Site.Controls[i];

      if Child <> nil then
        if Child.Owner <> Site then
        begin
          Child.Parent := Self;

          if Child = Site.Pages then
          begin
            FPages := Site.Pages;
            Site.FPages := nil;
          end;

          if Child.HostDockSite = Site then
            Child.HostDockSite := Self;

          for a := low(TAnchorKind) to high(TAnchorKind) do
          begin
            if Child.AnchorSide[a].Control = Site then
              Child.AnchorSide[a].Control := Self;
          end;
        end;
    end;

    Site.EndUpdateLayout;

    // delete Site
    Site.FSiteType := adhstNone;
    VarDockMaster.NeedFree(Site);
  finally
    EndUpdateLayout;
    EnableAutoSizing;
  end;

end;

function TGlassDockHostSite.GetOneControl: TControl;
var
  i: integer;
begin
  for i := 0 to ControlCount - 1 do
  begin
    Result := Controls[i];
    if Result.Owner <> Self then exit;
  end;
  Result := nil;
end;

function TGlassDockHostSite.GetSiteCount: integer;
var
  i: integer;
  Child: TControl;
begin
  Result := 0;
  for i := 0 to ControlCount - 1 do
  begin
    Child := Controls[i];
    if not (Child is TGlassDockHostSite) then continue;
    if not Child.IsControlVisible then continue;
    Inc(Result);
  end;
end;

function TGlassDockHostSite.IsOneSiteLayout(out Site: TGlassDockHostSite): boolean;
var
  i: integer;
  Child: TControl;
begin
  Site := nil;
  for i := 0 to ControlCount - 1 do
  begin
    Child := Controls[i];
    if not (Child is TGlassDockHostSite) then continue;
    if not Child.IsControlVisible then continue;
    if Site <> nil then exit(False);
    Site := TGlassDockHostSite(Child);
  end;
  Result := Site <> nil;
end;

function TGlassDockHostSite.IsTwoSiteLayout(out Site1, Site2: TGlassDockHostSite): boolean;
var
  i: integer;
  Child: TControl;
begin
  Site1 := nil;
  Site2 := nil;
  for i := 0 to ControlCount - 1 do
  begin
    Child := Controls[i];
    if not (Child is TGlassDockHostSite) then
      continue;
    if not Child.IsControlVisible then continue;

    if Site1 = nil then
      Site1 := TGlassDockHostSite(Child)
      else if Site2 = nil then
      Site2 := TGlassDockHostSite(Child)
      else
      exit(False);
  end;
  Result := Site2 <> nil;
end;

function TGlassDockHostSite.GetUniqueSplitterName: string;
var
  i: integer;
begin
  i := 0;
  repeat
    Inc(i);
    Result := cnGlassDockSplitterName + IntToStr(i);
  until FindComponent(Result) = nil;
end;

function TGlassDockHostSite.MakeSite(AControl: TControl): TGlassDockHostSite;
begin
  if AControl is TGlassDockHostSite then
    Result := TGlassDockHostSite(AControl)
  else
  begin
    Result := VarDockMaster.CreateSite;
    try
      AControl.ManualDock(Result, nil, alClient);
    finally
      Result.EnableAutoSizing;
    end;
  end;
end;

procedure TGlassDockHostSite.MoveAllControls(dx, dy: integer);
var
  i: integer;
  Child: TControl;
  NewBounds: TRect;
begin
  for i := 0 to ControlCount - 1 do
  begin
    Child := Controls[i];
    NewBounds := Child.BoundsRect;
    OffsetRect(NewBounds, dx, dy);
    if Child.AnchorSideLeft.Control = Self then  NewBounds.Left := 0;
    if Child.AnchorSideTop.Control = Self then   NewBounds.Top := 0;
    Child.BoundsRect := NewBounds;
  end;
end;

procedure TGlassDockHostSite.AlignControls(AControl: TControl; var ARect: TRect);
var
  i: integer;
  Child: TControl;
  Splitter: TGlassDockSplitter;
begin
  inherited AlignControls(AControl, ARect);
  if csDestroying in ComponentState then exit;

  if VarDockMaster.ScaleOnResize and (not UpdatingLayout) and (not VarDockMaster.Restoring) then
  begin
    // scale splitters
    for i := 0 to ControlCount - 1 do
    begin
      Child := Controls[i];

      if not Child.IsControlVisible then continue;

      if Child is TGlassDockSplitter then
      begin
        Splitter := TGlassDockSplitter(Child);
        if Splitter.ResizeAnchor in [akLeft, akRight] then
        begin
          if Splitter.DockParentClientSize.cx > 0 then
            Splitter.SetBoundsKeepDockBounds(
              (Splitter.DockBounds.Left * ClientWidth) div Splitter.DockParentClientSize.cx,
              Splitter.Top, Splitter.Width, Splitter.Height);
        end
        else
        begin
          if Splitter.DockParentClientSize.cy > 0 then
            Splitter.SetBoundsKeepDockBounds(Splitter.Left,
              (Splitter.DockBounds.Top * ClientHeight) div Splitter.DockParentClientSize.cy,
              Splitter.Width, Splitter.Height);
        end;
      end;
    end;
  end;
end;

function TGlassDockHostSite.CheckIfOneControlHidden: boolean;
var
  Child: TControl;
begin
  Result := False;

  if UpdatingLayout or
     (not IsControlVisible) or
     (csDestroying in ComponentState) or
     (SiteType <> adhstOneControl) then exit;

  Child := GetOneControl;

  if (Child = nil) then  exit;
  if Child.IsControlVisible then  exit;

  // docked child was hidden/closed
  Result := True;
  // => undock
  BeginUpdateLayout;
  DisableAutoSizing;
  Visible := False;
  Parent := nil;
  EnableAutoSizing;
  EndUpdateLayout;
  if (not (Child is TCustomForm)) or (csDestroying in Child.ComponentState) then
    Release;
end;

procedure TGlassDockHostSite.DoDock(NewDockSite: TWinControl; var ARect: TRect);
begin
  inherited DoDock(NewDockSite, ARect);
  VarDockMaster.SimplifyPendingLayouts;
end;

procedure TGlassDockHostSite.SetParent(NewParent: TWinControl);
var
  OldCaption: string;
  OldParent: TWinControl;
begin
  OldParent := Parent;
  if NewParent = OldParent then
    exit;
  inherited SetParent(NewParent);
  OldCaption := Caption;
  UpdateDockCaption;
  if OldCaption <> Caption then
  begin
    // UpdateDockCaption has not updated parents => do it now
    if Parent is TGlassDockHostSite then
      TGlassDockHostSite(Parent).UpdateDockCaption;
    if Parent is TGlassDockPage then
      TGlassDockPage(Parent).UpdateDockCaption;
  end;
  UpdateHeaderShowing;

  if (BoundSplitter <> nil) and (BoundSplitter.Parent <> Parent) then
  begin
    FreeAndNil(FBoundSplitter);
  end;
  if Parent = nil then
    BorderStyle := bsSizeable
  else
    BorderStyle := bsNone;
end;

function TGlassDockHostSite.HeaderNeedsShowing: boolean;
begin
  Result := (SiteType <> adhstLayout) and (not (Parent is TGlassDockPage)) and VarDockMaster.ShowHeader;
end;

procedure TGlassDockHostSite.DoShow;

  procedure Show(AControl: TWinControl);
  var
    i: Integer;
    Child: TControl;
  begin
    for i:=0 to AControl.ControlCount-1 do begin
      Child:=AControl.Controls[i];
      if Child is TWinControl then begin
        if Child is TCustomForm then begin
          TCustomForm(Child).Show;
        end
      end;
    end;
  end;

begin
  inherited DoShow;
  Show(Self);
end;

procedure TGlassDockHostSite.DoHide;

  procedure Hide(AControl: TWinControl);
  var
    i: Integer;
    Child: TControl;
  begin
    for i:=0 to AControl.ControlCount-1 do begin
      Child:=AControl.Controls[i];
      if Child is TWinControl then begin
        if Child is TCustomForm then begin
          TCustomForm(Child).Hide;
        end
      end;
    end;
  end;

begin
  if not (csDestroying in ComponentState) then
    Hide(Self);
  inherited DoHide;
end;

procedure TGlassDockHostSite.DoClose(var CloseAction: TCloseAction);
begin
  inherited DoClose(CloseAction);
end;

function TGlassDockHostSite.CanUndock: boolean;
begin
  Result := Parent <> nil;
end;

procedure TGlassDockHostSite.Undock;
var
  p: TPoint;
begin
  if Parent = nil then
    exit;
  DisableAutoSizing;
  p := Point(0, 0);
  p := ClientToScreen(p);
  Parent := nil;
  SetBounds(p.x, p.y, Width, Height);
  EnableAutoSizing;
end;

function TGlassDockHostSite.CanMerge: boolean;
begin
  Result := (SiteType = adhstLayout) and (Parent is TGlassDockHostSite) and (TGlassDockHostSite(Parent).SiteType = adhstLayout);
end;

procedure TGlassDockHostSite.Merge;
var
  ParentSite: TGlassDockHostSite;
  i: integer;
  Child: TControl;
  Side: TAnchorKind;
begin
  ParentSite := Parent as TGlassDockHostSite;
  if ParentSite = nil then
    exit;

  if (SiteType <> adhstLayout) or (ParentSite.SiteType <> adhstLayout) then
    exit;

  ParentSite.BeginUpdateLayout;
  DisableAutoSizing;
  try
    i:=0;
    while i<ControlCount-1 do begin
      Child:=Controls[i];
      if Child.Owner=Self then
        inc(i)
      else begin
        Child.Parent:=ParentSite;
        Child.SetBounds(Child.Left+Left,Child.Top+Top,Child.Width,Child.Height);
        for Side:=Low(TAnchorKind) to High(TAnchorKind) do begin
          if Child.AnchorSide[Side].Control=Self then
            Child.AnchorSide[Side].Assign(AnchorSide[Side]);
        end;
      end;
    end;
    Parent:=nil;
    VarDockMaster.NeedFree(Self);
  finally
    ParentSite.EndUpdateLayout;
  end;
end;

function TGlassDockHostSite.EnlargeSide(Side: TAnchorKind; OnlyCheckIfPossible: boolean): boolean;
begin
  Result := True;
  if EnlargeSideResizeTwoSplitters(Side, ClockwiseAnchor[Side], OnlyCheckIfPossible) then
    exit;
  if EnlargeSideResizeTwoSplitters(Side, OppositeAnchor[ClockwiseAnchor[Side]], OnlyCheckIfPossible) then
    exit;
  if EnlargeSideRotateSplitter(Side, OnlyCheckIfPossible) then
    exit;
  Result := False;
end;

function TGlassDockHostSite.EnlargeSideResizeTwoSplitters(ShrinkSplitterSide, EnlargeSpitterSide: TAnchorKind;
  OnlyCheckIfPossible: boolean): boolean;
var
  ParentSite: TGlassDockHostSite;
  ShrinkSplitter: TGlassDockSplitter;
  EnlargeSplitter: TGlassDockSplitter;
  KeptSide: TAnchorKind;
  KeptAnchorControl: TControl;
  Sibling: TControl;
  ShrinkControl: TControl;
  i: integer;
begin
  Result := False;
  if not (Parent is TGlassDockHostSite) then exit;
  ParentSite := TGlassDockHostSite(Parent);
  if not OnlyCheckIfPossible then
  begin
    ParentSite.BeginUpdateLayout;
    ParentSite.DisableAutoSizing;
  end;
  try
    // check ShrinkSplitter
    ShrinkSplitter := TGlassDockSplitter(AnchorSide[ShrinkSplitterSide].Control);
    if not (ShrinkSplitter is TGlassDockSplitter) then exit;
    // check if EnlargeSpitterSide is a neighbor ShrinkSplitterSide
    if (EnlargeSpitterSide <> ClockwiseAnchor[ShrinkSplitterSide]) and
      (EnlargeSpitterSide <> OppositeAnchor[ClockwiseAnchor[ShrinkSplitterSide]]) then
      exit;
    // check EnlargeSpitter
    EnlargeSplitter := TGlassDockSplitter(AnchorSide[EnlargeSpitterSide].Control);
    if not (EnlargeSplitter is TGlassDockSplitter) then exit;
    // check if KeptSide is anchored to a splitter or parent
    KeptSide := OppositeAnchor[EnlargeSpitterSide];
    KeptAnchorControl := AnchorSide[KeptSide].Control;
    if not ((KeptAnchorControl = ParentSite) or (KeptAnchorControl is TGlassDockSplitter)) then
      exit;
    // check if ShrinkSplitter is anchored/stops at KeptAnchorControl
    if ShrinkSplitter.AnchorSide[KeptSide].Control <> KeptAnchorControl then exit;

    // check if there is a control to shrink
    ShrinkControl := nil;
    for i := 0 to ShrinkSplitter.AnchoredControlCount - 1 do
    begin
      Sibling := ShrinkSplitter.AnchoredControls[i];
      if (Sibling.AnchorSide[OppositeAnchor[ShrinkSplitterSide]].Control = ShrinkSplitter) and
        (Sibling.AnchorSide[KeptSide].Control = KeptAnchorControl) then
      begin
        ShrinkControl := Sibling;
        break;
      end;
    end;
    if ShrinkControl = nil then exit;

    if OnlyCheckIfPossible then
    begin
      // check if ShrinkControl is large enough for shrinking
      case EnlargeSpitterSide of
        akTop: if ShrinkControl.Top >= EnlargeSplitter.Top then
            exit;
        akLeft: if ShrinkControl.Left >= EnlargeSplitter.Left then
            exit;
        akRight: if ShrinkControl.Left + ShrinkControl.Width <= EnlargeSplitter.Left + EnlargeSplitter.Width then
            exit;
        akBottom: if ShrinkControl.Top + ShrinkControl.Height <= EnlargeSplitter.Top + EnlargeSplitter.Height then
            exit;
      end;
    end
    else
    begin
      // do it
      // enlarge the EnlargeSplitter and Self
      AnchorAndChangeBounds(EnlargeSplitter, ShrinkSplitterSide,
        ShrinkControl.AnchorSide[ShrinkSplitterSide].Control);
      AnchorAndChangeBounds(Self, ShrinkSplitterSide,
        ShrinkControl.AnchorSide[ShrinkSplitterSide].Control);
      // shrink the ShrinkSplitter and ShrinkControl
      AnchorAndChangeBounds(ShrinkSplitter, KeptSide, EnlargeSplitter);
      AnchorAndChangeBounds(ShrinkControl, KeptSide, EnlargeSplitter);
    end;

  finally
    if not OnlyCheckIfPossible then
    begin
      ParentSite.EnableAutoSizing;
      ParentSite.EndUpdateLayout;
    end;
  end;
  Result := True;
end;

function TGlassDockHostSite.EnlargeSideRotateSplitter(Side: TAnchorKind; OnlyCheckIfPossible: boolean): boolean;
var
  Splitter: TGlassDockSplitter;
  CWSide: TAnchorKind;
  CWSplitter: TGlassDockSplitter;
  CCWSide: TAnchorKind;
  i: integer;
  Sibling: TControl;
  BehindSide: TAnchorKind;
  RotateSplitter: TGlassDockSplitter;
  CCWSplitter: TGlassDockSplitter;
begin
  Result := False;
  // check if there is a splitter at Side
  Splitter := TGlassDockSplitter(AnchorSide[Side].Control);
  if not (Splitter is TGlassDockSplitter) then exit;
  // check if there is a splitter at clockwise Side
  CWSide := ClockwiseAnchor[Side];
  CWSplitter := TGlassDockSplitter(AnchorSide[CWSide].Control);
  if not (CWSplitter is TGlassDockSplitter) then exit;
  // check if there is a splitter at counter clockwise Side
  CCWSide := OppositeAnchor[CWSide];
  CCWSplitter := TGlassDockSplitter(AnchorSide[CCWSide].Control);
  if not (CCWSplitter is TGlassDockSplitter) then exit;
  // check if neighbor splitters end at Splitter
  if CWSplitter.AnchorSide[Side].Control <> Splitter then  exit;
  if CCWSplitter.AnchorSide[Side].Control <> Splitter then exit;
  // find the rotate splitter behind Splitter
  BehindSide := OppositeAnchor[Side];
  RotateSplitter := nil;
  for i := 0 to Splitter.AnchoredControlCount - 1 do
  begin
    Sibling := Splitter.AnchoredControls[i];
    if Sibling.AnchorSide[BehindSide].Control <> Splitter then
      continue;
    if not (Sibling is TGlassDockSplitter) then
      continue;
    if Side in [akLeft, akRight] then
    begin
      if Sibling.Top < Top - VarDockMaster.SplitterWidth then
        continue;
      if Sibling.Top > Top + Height then
        continue;
    end
    else
    begin
      if Sibling.Left < Left - VarDockMaster.SplitterWidth then
        continue;
      if Sibling.Left > Left + Width then
        continue;
    end;
    if RotateSplitter = nil then
      RotateSplitter := TGlassDockSplitter(Sibling)
    else
      // there are multiple splitters behind
      exit;
  end;
  if RotateSplitter = nil then
    exit;
  // check that all siblings at RotateSplitter are large enough to shrink
  for i := 0 to RotateSplitter.AnchoredControlCount - 1 do
  begin
    Sibling := RotateSplitter.AnchoredControls[i];
    if Side in [akLeft, akRight] then
    begin
      if (Sibling.Top > Top - VarDockMaster.SplitterWidth) and (Sibling.Top + Sibling.Height < Top + Height + VarDockMaster.SplitterWidth) then
        exit;
    end
    else
    begin
      if (Sibling.Left > Left - VarDockMaster.SplitterWidth) and (Sibling.Left + Sibling.Width < Left + Width + VarDockMaster.SplitterWidth) then
        exit;
    end;
  end;
  Result := True;
  if OnlyCheckIfPossible then exit;

  DisableAutoSizing;
  try
    // enlarge the two neighbor splitters
    AnchorAndChangeBounds(CWSplitter, Side, RotateSplitter.AnchorSide[Side].Control);
    AnchorAndChangeBounds(CCWSplitter, Side, RotateSplitter.AnchorSide[Side].Control);
    // enlarge control
    AnchorAndChangeBounds(Self, Side, RotateSplitter.AnchorSide[Side].Control);
    // shrink the neighbors and anchor them to the enlarge splitters
    for i := 0 to Parent.ControlCount - 1 do
    begin
      Sibling := Parent.Controls[i];
      if Sibling.AnchorSide[CWSide].Control = RotateSplitter then
        AnchorAndChangeBounds(Sibling, CWSide, CCWSplitter)
      else if Sibling.AnchorSide[CCWSide].Control = RotateSplitter then
        AnchorAndChangeBounds(Sibling, CCWSide, CWSplitter);
    end;
    // rotate the RotateSplitter
    RotateSplitter.AnchorSide[Side].Control := nil;
    RotateSplitter.AnchorSide[BehindSide].Control := nil;
    RotateSplitter.ResizeAnchor := Side;
    AnchorAndChangeBounds(RotateSplitter, CCWSide, Splitter.AnchorSide[CCWSide].Control);
    AnchorAndChangeBounds(RotateSplitter, CWSide, CCWSplitter);
    if Side in [akLeft, akRight] then
    begin
      RotateSplitter.Left := Splitter.Left;
      RotateSplitter.Width := VarDockMaster.SplitterWidth;
    end
    else
    begin
      RotateSplitter.Top := Splitter.Top;
      RotateSplitter.Height := VarDockMaster.SplitterWidth;
    end;
    // shrink Splitter
    AnchorAndChangeBounds(Splitter, CCWSide, CWSplitter);
    // anchor some siblings of Splitter to RotateSplitter
    for i := 0 to Parent.ControlCount - 1 do
    begin
      Sibling := Parent.Controls[i];
      case Side of
        akLeft: if Sibling.Top < Top then
            continue;
        akRight: if Sibling.Top > Top then
            continue;
        akTop: if Sibling.Left > Left then
            continue;
        akBottom: if Sibling.Left < Left then
            continue;
      end;
      if Sibling.AnchorSide[BehindSide].Control = Splitter then
        Sibling.AnchorSide[BehindSide].Control := RotateSplitter
      else if Sibling.AnchorSide[Side].Control = Splitter then
        Sibling.AnchorSide[Side].Control := RotateSplitter;
    end;
  finally
    EnableAutoSizing;
  end;
end;

procedure TGlassDockHostSite.CreateBoundSplitter;
begin
  if BoundSplitter <> nil then exit;
  FBoundSplitter := VarDockMaster.CreateSplitter;
  BoundSplitter.FreeNotification(Self);
  BoundSplitter.Align := Align;
  BoundSplitter.Parent := Parent;
end;

procedure TGlassDockHostSite.PositionBoundSplitter;
begin
  case Align of
    alTop: BoundSplitter.SetBounds(0, Height, Parent.ClientWidth, BoundSplitter.Height);
    alBottom: BoundSplitter.SetBounds(0, Parent.ClientHeight - Height - BoundSplitter.Height,
        Parent.ClientWidth, BoundSplitter.Height);
    alLeft: BoundSplitter.SetBounds(Width, 0, BoundSplitter.Width, Parent.ClientHeight);
    alRight: BoundSplitter.SetBounds(Parent.ClientWidth - Width - BoundSplitter.Width, 0
        , BoundSplitter.Width, Parent.ClientHeight);
  end;
end;

function TGlassDockHostSite.CloseQuery: boolean;

  function Check(AControl: TWinControl): boolean;
  var
    i: integer;
    Child: TControl;
  begin
    for i := 0 to AControl.ControlCount - 1 do
    begin
      Child := AControl.Controls[i];
      if Child is TWinControl then
      begin
        if Child is TCustomForm then
        begin
          if not TCustomForm(Child).CloseQuery then
            exit(False);
        end
        else
        begin
          if not Check(TWinControl(Child)) then
            exit(False);
        end;
      end;
    end;
    Result := True;
  end;

begin
  Result := Check(Self);
end;

function TGlassDockHostSite.CloseSite: boolean;
var
  AControl: TControl;
  AForm: TCustomForm;
  IsMainForm: boolean;
  CloseAction: TCloseAction;
  NeedEnableAutoSizing: Boolean;
begin
  Result:=CloseQuery;
  if not Result then exit;

  case SiteType of
  adhstNone:
    begin
      Release;
      exit;
    end;
  adhstOneControl:
    begin
      DisableAutoSizing;
      NeedEnableAutoSizing:=true;
      try
        AControl:=GetOneControl;
        if AControl is TCustomForm then begin
          AForm:=TCustomForm(AControl);
          IsMainForm := (Application.MainForm = AForm)
                        or (AForm.IsParentOf(Application.MainForm));
          if IsMainForm then
            CloseAction := caFree
          else
            CloseAction := caHide;
          // ToDo: TCustomForm(AControl).DoClose(CloseAction);
          case CloseAction of
          caHide: Hide;
          caMinimize: WindowState := wsMinimized;
          caFree:
            begin
              // if form is MainForm, then terminate the application
              // the owner of the MainForm is the application,
              // so the Application will take care of free-ing the form
              // and Release is not necessary
              if IsMainForm then
                Application.Terminate
              else begin
                NeedEnableAutoSizing:=false;
                Release;
                AForm.Release;
                exit;
              end;
            end;
          end;
        end else begin
          AControl.Visible:=false;
          NeedEnableAutoSizing:=false;
          Release;
          exit;
        end;
        Visible:=false;
        Parent:=nil;
      finally
        if NeedEnableAutoSizing then
          EnableAutoSizing;
      end;
    end;
  end;
end;

procedure TGlassDockHostSite.RemoveControl(AControl: TControl);
begin
  DisableAutoSizing;
  inherited RemoveControl(AControl);
  if not (csDestroying in ComponentState) then
  begin
    if (not ((AControl is TGlassDockHeader) or (AControl is TGlassDockSplitter))) then
    begin
      if (SiteType = adhstLayout) then
        RemoveControlFromLayout(AControl) else
        VarDockMaster.NeedSimplify(Self);

      UpdateDockCaption;
    end;
  end;
  EnableAutoSizing;
end;

procedure TGlassDockHostSite.InsertControl(AControl: TControl; Index: integer);
begin
  DisableAutoSizing;
  try
    inherited InsertControl(AControl, Index);
    if not ((AControl is TGlassDockSplitter) or (AControl is TGlassDockHeader)) then
      UpdateDockCaption;
  finally
    EnableAutoSizing;
  end;
end;

procedure TGlassDockHostSite.UpdateDockCaption(Exclude: TControl);
var
  i: integer;
  Child: TControl;
  NewCaption, OldCaption: string;
begin
  if csDestroying in ComponentState then exit;
  NewCaption := '';
  for i := 0 to ControlCount - 1 do
  begin
    Child := Controls[i];
    if Child = Exclude then continue;
    if (Child.HostDockSite = Self) or (Child is TGlassDockHostSite) or (Child is TGlassDockPageControl) then
    begin
      if NewCaption <> '' then NewCaption := NewCaption + ',';
      NewCaption := NewCaption + Child.Caption;
    end;
  end;
  OldCaption := Caption;
  Caption := NewCaption;
  if ((Parent = nil) and VarDockMaster.HideHeaderCaptionFloatingControl) or (not VarDockMaster.ShowHeaderCaption) then
    Header.Caption := '' else
    Header.Caption := Caption;

  if OldCaption <> Caption then
  begin
    if Parent is TGlassDockHostSite then   TGlassDockHostSite(Parent).UpdateDockCaption;
    if Parent is TGlassDockPage then       TGlassDockPage(Parent).UpdateDockCaption;
  end;

  // do not show close button for mainform
  Header.CloseButton.Visible := not IsParentOf(Application.MainForm);
end;

procedure TGlassDockHostSite.GetSiteInfo(Client: TControl; var InfluenceRect: TRect; MousePos: TPoint; var CanDock: boolean);
var
  ADockMargin: longint;
begin
  if VarDockMaster = nil then exit;

  GetWindowRect(Handle, InfluenceRect);

  if (Parent = nil) or VarDockMaster.IsCustomSite(Parent) then
  begin
    // allow docking outside => enlarge margins
    ADockMargin := VarDockMaster.DockOutsideMargin;
    InfluenceRect.Left := InfluenceRect.Left - ADockMargin;
    InfluenceRect.Top := InfluenceRect.Top - ADockMargin;
    InfluenceRect.Right := InfluenceRect.Right + ADockMargin;
    InfluenceRect.Bottom := InfluenceRect.Bottom + ADockMargin;
  end
  else if Parent is TGlassDockHostSite then
  begin
    // do not cover parent site => shrink margins
    ADockMargin := VarDockMaster.DockParentMargin;
    ADockMargin := Min(ADockMargin, Min(ClientWidth, ClientHeight) div 10);
    ADockMargin := Max(0, ADockMargin);
    InfluenceRect.Left := InfluenceRect.Left + ADockMargin;
    InfluenceRect.Top := InfluenceRect.Top + ADockMargin;
    InfluenceRect.Right := InfluenceRect.Right - ADockMargin;
    InfluenceRect.Bottom := InfluenceRect.Bottom - ADockMargin;
  end;

  CanDock := (Client is TGlassDockHostSite) and not VarDockMaster.AutoFreedIfControlIsRemoved(Self, Client);

  if Assigned(OnGetSiteInfo) then
    OnGetSiteInfo(Self, Client, InfluenceRect, MousePos, CanDock);
end;

function TGlassDockHostSite.GetPageArea: TRect;
begin
  if VarDockMaster = nil then exit;
  Result := Rect(0, 0, Width * VarDockMaster.PageAreaInPercent div 100, Height * VarDockMaster.PageAreaInPercent div 100);
  OffsetRect(Result, (Width * (100 - VarDockMaster.PageAreaInPercent)) div 200,
    (Height * (100 - VarDockMaster.PageAreaInPercent)) div 200);
end;

procedure TGlassDockHostSite.ChangeBounds(ALeft, ATop, AWidth, AHeight: integer; KeepBase: boolean);
begin
  inherited ChangeBounds(ALeft, ATop, AWidth, AHeight, KeepBase);
  if Header <> nil then UpdateHeaderAlign;
end;

procedure TGlassDockHostSite.UpdateHeaderAlign;
begin
  if Header = nil then
    exit;
  case Header.HeaderPosition of
    adlhpAuto:
      if Header.Align in [alLeft, alRight] then
      begin
        if (ClientHeight > 0) and ((ClientWidth * 100 div ClientHeight) <= VarDockMaster.HeaderAlignTop) then
          Header.Align := alTop;
      end
      else
      begin
        if (ClientHeight > 0) and ((ClientWidth * 100 div ClientHeight) >= VarDockMaster.HeaderAlignLeft) then
        begin
          if Application.BidiMode = bdRightToLeft then
            Header.Align := alRight
          else
            Header.Align := alLeft;
        end;
      end;
    adlhpLeft: Header.Align := alLeft;
    adlhpTop: Header.Align := alTop;
    adlhpRight: Header.Align := alRight;
    adlhpBottom: Header.Align := alBottom;
  end;
end;

procedure TGlassDockHostSite.UpdateHeaderShowing;
begin
  if Header = nil then exit;
  if HeaderNeedsShowing then
    Header.Parent := Self else
    Header.Parent := nil;
end;

procedure TGlassDockHostSite.BeginUpdateLayout;
begin
  Inc(fUpdateLayout);
  if fUpdateLayout = 1 then VarDockMaster.BeginUpdate;
end;

procedure TGlassDockHostSite.EndUpdateLayout;
begin
  if fUpdateLayout = 0 then exit;
  Dec(fUpdateLayout);
  if fUpdateLayout = 0 then VarDockMaster.EndUpdate;
end;

function TGlassDockHostSite.UpdatingLayout: boolean;
begin
  Result := (fUpdateLayout > 0) or (csDestroying in ComponentState);
end;

procedure TGlassDockHostSite.SaveLayout(LayoutTree: TGlassDockLayoutTree; LayoutNode: TGlassDockLayoutTreeNode);
var
  i: integer;
  Site: TGlassDockHostSite;
  ChildNode: TGlassDockLayoutTreeNode;
  Child: TControl;
  Splitter: TGlassDockSplitter;
  OneControl: TControl;
begin
  if SiteType = adhstOneControl then
    OneControl := GetOneControl
  else
    OneControl := nil;
  if (SiteType = adhstOneControl) and (OneControl <> nil) and (not (OneControl is TGlassDockHostSite)) then
  begin
    LayoutNode.NodeType := adltnControl;
    LayoutNode.Assign(Self);
    LayoutNode.Name := OneControl.Name;
    LayoutNode.HeaderPosition := Header.HeaderPosition;
  end
  else if (SiteType in [adhstLayout, adhstOneControl]) then
  begin
    LayoutNode.NodeType := adltnLayout;
    for i := 0 to ControlCount - 1 do
    begin
      Child := Controls[i];
      if Child.Owner = Self then
        continue;
      if (Child is TGlassDockHostSite) then
      begin
        Site := TGlassDockHostSite(Child);
        ChildNode := LayoutTree.NewNode(LayoutNode);
        Site.SaveLayout(LayoutTree, ChildNode);
      end
      else if (Child is TGlassDockSplitter) then
      begin
        Splitter := TGlassDockSplitter(Child);
        ChildNode := LayoutTree.NewNode(LayoutNode);
        Splitter.SaveLayout(ChildNode);
      end;
    end;
    LayoutNode.Assign(Self);
    LayoutNode.HeaderPosition := Header.HeaderPosition;
  end
  else if SiteType = adhstPages then
  begin
    LayoutNode.NodeType := adltnPages;
    for i := 0 to Pages.PageCount - 1 do
    begin
      Site := Pages.DockPages[i].GetSite;  // 8888
      if Site <> nil then
      begin
        ChildNode := LayoutTree.NewNode(LayoutNode);
        Site.SaveLayout(LayoutTree, ChildNode);
      end;
    end;
    LayoutNode.Assign(Self);
    LayoutNode.HeaderPosition := Header.HeaderPosition;
  end
  else
    LayoutNode.NodeType := adltnNone;
  if BoundSplitter <> nil then
  begin
    if Align in [alLeft, alRight] then
      LayoutNode.BoundSplitterPos := BoundSplitter.Left  else
      LayoutNode.BoundSplitterPos := BoundSplitter.Top;
  end;
end;

constructor TGlassDockHostSite.CreateNew(AOwner: TComponent; Num: integer);
begin
  inherited CreateNew(AOwner, Num);
  Visible := False;
  FHeaderSide := akTop;
  FHeader := VarDockMaster.HeaderClass.Create(Self);
  FHeader.Align := alTop;
  FHeader.Parent := Self;
  FSiteType := adhstNone;
  UpdateHeaderAlign;
  DragKind := dkDock;
  DockManager := VarDockMaster.ManagerClass.Create(Self);
  UseDockManager := True;
  DragManager.RegisterDockSite(Self, True);
end;

destructor TGlassDockHostSite.Destroy;  // 3333
begin
  if FPages <> nil then FPages.Free;
  FPages := nil;

  inherited Destroy;
end;

//======================== TGlassDockHeader ==============================================

procedure TGlassDockHeader.PopupMenuPopup(Sender: TObject);
var
  HeaderPosItem: TMenuItem;
  ParentSite: TGlassDockHostSite;
  Side: TAnchorKind;
  SideCaptions: array[TAnchorKind] of string;
  Item: TMenuItem;
  ContainsMainForm: boolean;
  s: string;
begin
  ParentSite := TGlassDockHostSite(Parent);
  SideCaptions[akLeft] := adrsLeft;
  SideCaptions[akTop] := adrsTop;
  SideCaptions[akRight] := adrsRight;
  SideCaptions[akBottom] := adrsBottom;

  // undock, merge
  VarDockMaster.AddRemovePopupMenuItem(ParentSite.CanUndock, 'UndockMenuItem', adrsUndock, @UndockButtonClick);
  VarDockMaster.AddRemovePopupMenuItem(ParentSite.CanMerge, 'MergeMenuItem', adrsMerge, @MergeButtonClick);

  // header position
  HeaderPosItem := VarDockMaster.AddPopupMenuItem('HeaderPosMenuItem', adrsHeaderPosition, nil);

  Item := VarDockMaster.AddPopupMenuItem('HeaderPosAutoMenuItem', adrsAutomatically, @HeaderPositionItemClick, HeaderPosItem);
  if Item <> nil then
  begin
    Item.Tag := Ord(adlhpAuto);
    Item.Checked := HeaderPosition = TADLHeaderPosition(Item.Tag);
  end;

  for Side := Low(TAnchorKind) to High(TAnchorKind) do
  begin
    Item := VarDockMaster.AddPopupMenuItem('HeaderPos' + DbgS(Side) + 'MenuItem', SideCaptions[Side],
      @HeaderPositionItemClick, HeaderPosItem);
    if Item = nil then continue;
    Item.Tag := Ord(Side) + 1;
    Item.Checked := HeaderPosition = TADLHeaderPosition(Item.Tag);
  end;

  // enlarge
  for Side := Low(TAnchorKind) to High(TAnchorKind) do
  begin
    Item := VarDockMaster.AddRemovePopupMenuItem(ParentSite.EnlargeSide(Side, True), 'Enlarge' + DbgS(Side) + 'MenuItem',
      Format(adrsEnlargeSide, [SideCaptions[Side]]), @EnlargeSideClick);
    if Item <> nil then Item.Tag := Ord(Side);
  end;

  // close
  ContainsMainForm := ParentSite.IsParentOf(Application.MainForm);

  if ContainsMainForm then
    s := Format(adrsQuit, [Application.Title]) else
    s := adrsClose;

  VarDockMaster.AddRemovePopupMenuItem(CloseButton.Visible, 'CloseMenuItem', s, @CloseButtonClick);
end;

procedure TGlassDockHeader.CloseButtonClick(Sender: TObject);
begin
  if Parent is TGlassDockHostSite then
  begin
    VarDockMaster.RestoreLayouts.Add(VarDockMaster.CreateRestoreLayout(Parent), True);
    TGlassDockHostSite(Parent).CloseSite;
  end;
end;

procedure TGlassDockHeader.HeaderPositionItemClick(Sender: TObject);
var
  Item: TMenuItem;
begin
  if not (Sender is TMenuItem) then exit;
  Item := TMenuItem(Sender);
  HeaderPosition := TADLHeaderPosition(Item.Tag);
end;

procedure TGlassDockHeader.UndockButtonClick(Sender: TObject);
begin
  TGlassDockHostSite(Parent).Undock;
end;

procedure TGlassDockHeader.MergeButtonClick(Sender: TObject);
begin
  TGlassDockHostSite(Parent).Merge;
end;

procedure TGlassDockHeader.EnlargeSideClick(Sender: TObject);
var
  Side: TAnchorKind;
begin
  if not (Sender is TMenuItem) then exit;
  Side := TAnchorKind(TMenuItem(Sender).Tag);
  TGlassDockHostSite(Parent).EnlargeSide(Side, False);
end;

procedure TGlassDockHeader.SetHeaderPosition(const AValue: TADLHeaderPosition);
begin
  if FHeaderPosition = AValue then exit;
  FHeaderPosition := AValue;
  if Parent is TGlassDockHostSite then
    TGlassDockHostSite(Parent).UpdateHeaderAlign;
end;

// DaThoX begin
class procedure TGlassDockHeader.SetSelectedHeader(const AVal: TGlassDockHeader);
var
  LOldSelectedHeader: TGlassDockHeader;
begin
  LOldSelectedHeader:=FSelectedHeader;
  FSelectedHeader:=AVal;

  { deactivate old header if exist }
  if Assigned(LOldSelectedHeader) then
    LOldSelectedHeader.Repaint;

  FSelectedHeader.Repaint;
end;
// DaThoX end

procedure TGlassDockHeader.Paint;
var
  r: TRect;
  TxtH: longint;
  TxtW: longint;
  dx, dy: integer;
begin
  // DaThoX begin
  r:=ClientRect;
  Canvas.Pen.Color:=clBtnFace;
  Canvas.Brush.Color:=clBtnFace;
  Canvas.FillRect(r);

  if SelectedHeader = Self then
  begin
    Background.Color := $00D1B499;
    Canvas.Font.Bold := True;
  end
  else
  begin
    Background.Color := clBtnFace;
    Canvas.Font.Bold := False;
  end;
  inherited;

  if Caption<>'' then begin
    Canvas.Brush.Color:=clNone;
    Canvas.Brush.Style:=bsClear;
    TxtH:=Canvas.TextHeight(Caption);
    TxtW:=Canvas.TextWidth(Caption);

    if Align in [alLeft,alRight] then
    begin
      // vertical
      dx:=Max(0,(r.Right-r.Left-TxtH) div 2);
      dy:=Max(0,(r.Bottom-r.Top-TxtW) div 2);
      Canvas.Font.Orientation:=900;
      Canvas.TextOut(Pred(r.Left-dx),r.Bottom-dy,Caption);
    end else
    begin
      // horizontal
      dx:=Max(0,(r.Right-r.Left-TxtW) div 2);
      dy:=Max(0,(r.Bottom-r.Top-TxtH) div 2);
      Canvas.Font.Orientation:=0;
      Canvas.TextOut(r.Left+dx,r.Top+dy,Caption);
    end;
  end;

  exit;
  // DaThoX end
  r := ClientRect;

  Canvas.Frame3d(r, 1, bvRaised);

  Canvas.Brush.Color := VarDockMaster.HeaderColor;

  if (Canvas.Brush.Color = clNone) or (Canvas.Brush.Color = clDefault) or (Canvas.Brush.Color = 0) then
    Canvas.Brush.Color := cnHeaderColorDefault;

  Canvas.FillRect(r);

  if CloseButton.IsControlVisible and (CloseButton.Parent = Self) then
  begin
    if Align in [alLeft, alRight] then
      r.Top := CloseButton.Top + CloseButton.Height + 1 else
      r.Right := CloseButton.Left - 1;
  end;

  // caption
  if Caption <> '' then
  begin
    Canvas.Brush.Color := clNone;
    Canvas.Brush.Style := bsClear;
    TxtH := Canvas.TextHeight(Caption);
    TxtW := Canvas.TextWidth(Caption);

    if Align in [alLeft, alRight] then
    begin
      // vertical
      dx := Max(0, (r.Right - r.Left - TxtH) div 2);
      dy := Max(0, (r.Bottom - r.Top - TxtW) div 2);
      Canvas.Font.Orientation := 900;
      Canvas.TextOut(r.Left + dx, r.Bottom - dy, Caption);
    end
    else
    begin
      // horizontal
      dx := Max(0, (r.Right - r.Left - TxtW) div 2);
      dy := Max(0, (r.Bottom - r.Top - TxtH) div 2);
      Canvas.Font.Orientation := 0;
      Canvas.TextOut(r.Left + dx, r.Top + dy, Caption);
    end;
  end;
end;

procedure TGlassDockHeader.CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer; WithThemeSpace: boolean);
const
  TestTxt = 'ABCXYZ123gqj';
var
  DC: HDC;
  R: TRect;
  OldFont: HGDIOBJ;
  Flags: cardinal;
  NeededHeight: integer;
begin
  inherited CalculatePreferredSize(PreferredWidth, PreferredHeight, WithThemeSpace);
  if Caption <> '' then
  begin
    DC := GetDC(Parent.Handle);
    try
      R := Rect(0, 0, 10000, 10000);
      OldFont := SelectObject(DC, HGDIOBJ(Font.Reference.Handle));
      Flags := DT_CALCRECT or DT_EXPANDTABS or DT_SINGLELINE or DT_NOPREFIX;

      DrawText(DC, PChar(TestTxt), Length(TestTxt), R, Flags);
      SelectObject(DC, OldFont);
      NeededHeight := R.Bottom - R.Top + BevelWidth * 2;
    finally
      ReleaseDC(Parent.Handle, DC);
    end;

    if Align in [alLeft, alRight] then
    begin
      PreferredWidth := Max(NeededHeight, PreferredWidth);
    end else
    begin
      PreferredHeight := Max(NeededHeight, PreferredHeight);
    end;
  end;
end;

procedure TGlassDockHeader.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  // DaThoX begin
  if SelectedHeader <> Self then
    SelectedHeader := Self;
  // DaThoX end

  inherited MouseDown(Button, Shift, X, Y);
  if (Button = mbLeft) and VarDockMaster.AllowDragging then
    DragManager.DragStart(Parent, False, VarDockMaster.DragTreshold);
end;

procedure TGlassDockHeader.UpdateHeaderControls;
begin
  if Align in [alLeft, alRight] then
  begin
    if CloseButton <> nil then CloseButton.Align := alTop;
  end else
  begin
    if CloseButton <> nil then CloseButton.Align := alRight;
  end;

end;

procedure TGlassDockHeader.SetAlign(Value: TAlign);
begin
  if Value = Align then exit;
  DisableAutoSizing;
  inherited SetAlign(Value);
  UpdateHeaderControls;
  EnableAutoSizing;
end;

procedure TGlassDockHeader.DoOnShowHint(HintInfo: PHintInfo);
var
  s: string;
  p: longint;
  c: string;
begin
  s := VarDockMaster.HeaderHint;
  p := Pos('%s', s);
  if p > 0 then
  begin
    if Parent <> nil then
      c := Parent.Caption else
      c := '';

    s := Format(s, [c]);
  end;

  HintInfo^.HintStr := s;
  inherited DoOnShowHint(HintInfo);
end;

constructor TGlassDockHeader.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FHeaderPosition := adlhpAuto;
  FCloseButton := TGlassDockCloseButton.Create(Self);
  BevelOuter := bvNone;
  BorderWidth := 0;
  // DaThoX begin
  BorderBCStyle:=bpsBorder;
  Border.Style:=bboSolid;
  Rounding.RoundOptions:=[rrDefault];
  Rounding.RoundX:=3;
  Rounding.RoundY:=3;
  Background.Style:=bbsColor;
  Background.Color:=clBtnFace;
  Border.Color:=clActiveBorder;
  Color:=clBtnFace;
  // DaThoX end

  with FCloseButton do
  begin
    Name := 'CloseButton';
    Parent := Self;
    Flat := True;
    ShowHint := True;
    Hint := adrsClose;
    OnClick := @CloseButtonClick;
    AutoSize := True;
  end;

  SetColor(clblack);
  Align := alTop;
  AutoSize := True;
  ShowHint := True;
  PopupMenu := VarDockMaster.GetPopupMenu;
end;

//================ TGlassDockCloseButton ====================================================

constructor TGlassDockCloseButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  GetCloseGlyph;
  Glyph := VarDockMaster.fCloseBtnBitmap;
end;

destructor TGlassDockCloseButton.Destroy;
begin
  ReleaseCloseGlyph;
  inherited Destroy;
end;

procedure TGlassDockCloseButton.GetCloseGlyph;
begin
  Inc(VarDockMaster.fCloseBtnReferenceCount);
  if VarDockMaster.fCloseBtnBitmap = nil then
    VarDockMaster.CreateCloseButtonBitmap;
end;

procedure TGlassDockCloseButton.ReleaseCloseGlyph;
begin
  if VarDockMaster = nil then exit;
  Dec(VarDockMaster.fCloseBtnReferenceCount);
  if VarDockMaster.fCloseBtnReferenceCount = 0 then
    VarDockMaster.FreeCloseButtonBitmap;
end;

function TGlassDockCloseButton.GetGlyphSize(Drawing: boolean; PaintRect: TRect): TSize;
begin
  if PaintRect.Left = 0 then ;
  Result.cx := VarDockMaster.fCloseBtnBitmap.Width;
  Result.cy := VarDockMaster.fCloseBtnBitmap.Height;
end;

function TGlassDockCloseButton.DrawGlyph(ACanvas: TCanvas; const AClient: TRect; const AOffset: TPoint;
  AState: TButtonState; ATransparent: boolean; BiDiFlags: longint): TRect;
var
  closeBmp: TBitmap;
begin
  if BiDiFlags = 0 then ;
  closeBmp := VarDockMaster.fCloseBtnBitmap;
  closeBmp.Transparent := ATransparent;
  if ATransparent then closeBmp.TransparentMode := tmAuto;
  if AState = bsDisabled then ;
  Result := Rect(0, 0, closeBmp.Width, closeBmp.Height);
  OffsetRect(Result, AClient.Left + AOffset.X, AClient.Top + AOffset.Y);
  ACanvas.Draw(Result.Left, Result.Top, closeBmp);
end;

procedure TGlassDockCloseButton.CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer; WithThemeSpace: boolean);
begin
  PreferredWidth := VarDockMaster.fCloseBtnBitmap.Width + 4;
  PreferredHeight := VarDockMaster.fCloseBtnBitmap.Height + 4;
end;

//=============================== TGlassDockManager =======================================

procedure TGlassDockManager.SetPreferredSiteSizeAsSiteMinimum(const AValue: boolean);
begin
  if FPreferredSiteSizeAsSiteMinimum = AValue then exit;
  FPreferredSiteSizeAsSiteMinimum := AValue;
  if DockSite = nil then
    Site.AdjustSize;
end;

constructor TGlassDockManager.Create(ADockSite: TWinControl);
begin
  inherited Create(ADockSite);
  FSite := ADockSite;
  FDockableSites := [akLeft, akTop, akBottom, akRight];
  FInsideDockingAllowed := True;
  FPreferredSiteSizeAsSiteMinimum := True;
  if (ADockSite is TGlassDockHostSite) then
    FDockSite := TGlassDockHostSite(ADockSite);
end;

procedure TGlassDockManager.GetControlBounds(Control: TControl; out AControlBounds: TRect);
begin
  if Control = nil then
  ;
  AControlBounds := Rect(0, 0, 0, 0);
end;

procedure TGlassDockManager.InsertControl(Control: TControl; InsertAt: TAlign; DropCtl: TControl);
begin
  if Control = nil then;
  if InsertAt = alNone then
  ;
  if DropCtl = nil then
  ;
end;

procedure TGlassDockManager.InsertControl(ADockObject: TDragDockObject);
var
  NewSiteBounds: TRect;
  NewChildBounds: TRect;
  Child: TControl;
  ChildSite: TGlassDockHostSite;
  SplitterWidth: integer;
begin
  if DockSite <> nil then
  begin
  end
  else
  begin
    Site.DisableAutoSizing;
    try
      // align dragged Control
      Child := ADockObject.Control;
      Child.Parent := Site;
      Child.Align := ADockObject.DropAlign;
      Child.Width := ADockObject.DockRect.Right - ADockObject.DockRect.Left;
      Child.Height := ADockObject.DockRect.Bottom - ADockObject.DockRect.Top;

      SplitterWidth := 0;
      ChildSite := nil;
      if Child is TGlassDockHostSite then
      begin
        ChildSite := TGlassDockHostSite(Child);
        ChildSite.CreateBoundSplitter;
        SplitterWidth := VarDockMaster.SplitterWidth;
      end;

      // resize Site
      NewSiteBounds := Site.BoundsRect;
      case ADockObject.DropAlign of
        alLeft: Dec(NewSiteBounds.Left, Child.ClientWidth + SplitterWidth);
        alRight: Dec(NewSiteBounds.Right, Child.ClientWidth + SplitterWidth);
        alTop: Dec(NewSiteBounds.Top, Child.ClientHeight + SplitterWidth);
        alBottom: Inc(NewSiteBounds.Bottom, Child.ClientHeight + SplitterWidth);
      end;
      if not StoredConstraintsValid then
        StoreConstraints;
      if ADockObject.DropAlign in [alLeft, alRight] then
        Site.Constraints.MaxWidth := 0
      else
        Site.Constraints.MaxHeight := 0;
      Site.BoundsRect := NewSiteBounds;
      FSiteClientRect := Site.ClientRect;

      // resize child
      NewChildBounds := Child.BoundsRect;
      case ADockObject.DropAlign of
        alTop: NewChildBounds := Bounds(0, 0, Site.ClientWidth, Child.ClientHeight);
        alBottom: NewChildBounds := Bounds(0, Site.ClientHeight - Child.ClientHeight,
            Site.ClientWidth, Child.ClientHeight);
        alLeft: NewChildBounds := Bounds(0, 0, Child.ClientWidth, Site.ClientHeight);
        alRight: NewChildBounds := Bounds(Site.ClientWidth - Child.ClientWidth, 0, Child.ClientWidth,
            Site.ClientHeight);
      end;
      Child.BoundsRect := NewChildBounds;

      if ChildSite <> nil then
        ChildSite.PositionBoundSplitter;

      // only allow to dock one control
      DragManager.RegisterDockSite(Site, False);
    finally
      Site.EnableAutoSizing;
    end;
  end;
end;

procedure TGlassDockManager.LoadFromStream(Stream: TStream);
begin
  if Stream = nil then
  ;
end;

procedure TGlassDockManager.PositionDockRect(Client, DropCtl: TControl; DropAlign: TAlign; var DockRect: TRect);
{ Client = dragged source site (a TGlassDockHostSite)
  DropCtl is target control (the DockSite, DockSite.Pages or one of the pages)
  DropAlign: where on Client DropCtl should be placed
  DockRect: the estimated new bounds of DropCtl
}
var
  Offset: TPoint;
  Inside: boolean;
begin
  if (DropAlign = alClient) and (DockSite <> nil) and (DockSite.Pages <> nil) then
  begin
    // dock into pages
    if DropCtl = DockSite.Pages then
    begin
      // dock as last page
      DockRect := DockSite.Pages.TabRect(DockSite.Pages.PageCount - 1);
      case DockSite.Pages.TabPosition of
        tpTop, tpBottom: DockRect.Left := (DockRect.Left + DockRect.Right) div 2;
        tpLeft, tpRight: DockRect.Top := (DockRect.Top + DockRect.Bottom) div 2;
      end;
      Offset := DockSite.Pages.ClientOrigin;
      OffsetRect(DockRect, Offset.X, Offset.Y);
      exit;
    end
    else if DropCtl is TGlassDockPage then
    begin
      // dock in front of page
      DockRect := DockSite.Pages.TabRect(TGlassDockPage(DropCtl).PageIndex);
      case DockSite.Pages.TabPosition of
        tpTop, tpBottom: DockRect.Right := (DockRect.Left + DockRect.Right) div 2;
        tpLeft, tpRight: DockRect.Bottom := (DockRect.Top + DockRect.Bottom) div 2;
      end;
      Offset := DockSite.Pages.ClientOrigin;
      OffsetRect(DockRect, Offset.X, Offset.Y);
      exit;
    end;
  end;

  Inside := (DropCtl = Site);
  if (not Inside) and (Site.Parent <> nil) then
  begin
    if (Site.Parent is TGlassDockHostSite) or (not (Site.Parent.DockManager is TGlassDockManager)) or (Site.Parent.Parent <> nil) then
      Inside := True;
  end;
  case DropAlign of
    alLeft:
      if Inside then
        DockRect := Rect(0, 0, Min(Client.Width, Site.ClientWidth div 2), Site.ClientHeight)
      else
        DockRect := Rect(-Client.Width, 0, 0, Site.ClientHeight);
    alRight:
      if Inside then
      begin
        DockRect := Rect(0, 0, Min(Client.Width, Site.Width div 2), Site.ClientHeight);
        OffsetRect(DockRect, Site.ClientWidth - DockRect.Right, 0);
      end
      else
        DockRect := Bounds(Site.ClientWidth, 0, Client.Width, Site.ClientHeight);
    alTop:
      if Inside then
        DockRect := Rect(0, 0, Site.ClientWidth, Min(Client.Height, Site.ClientHeight div 2))
      else
        DockRect := Rect(0, -Client.Height, Site.ClientWidth, 0);
    alBottom:
      if Inside then
      begin
        DockRect := Rect(0, 0, Site.ClientWidth, Min(Client.Height, Site.ClientHeight div 2));
        OffsetRect(DockRect, 0, Site.ClientHeight - DockRect.Bottom);
      end
      else
        DockRect := Bounds(0, Site.ClientHeight, Site.ClientWidth, Client.Height);
    alClient:
    begin
      // paged docking => show center
      if DockSite <> nil then
        DockRect := DockSite.GetPageArea;
    end;
    else
      exit; // use default
  end;
  Offset := Site.ClientOrigin;
  OffsetRect(DockRect, Offset.X, Offset.Y);
end;

procedure TGlassDockManager.RemoveControl(Control: TControl);
var
  NewBounds: TRect;
  ChildSite: TGlassDockHostSite;
  SplitterWidth: integer;
begin
  if DockSite <> nil then
  begin
    // ....
  end
  else
  begin

    if Control is TGlassDockHostSite then
    begin
      SplitterWidth := 0;
      if Control is TGlassDockHostSite then
      begin
        ChildSite := TGlassDockHostSite(Control);
        if ChildSite.BoundSplitter <> nil then
          SplitterWidth := VarDockMaster.SplitterWidth;
      end;

      // shrink Site
      NewBounds := Site.BoundsRect;
      case Control.Align of
        alTop: Inc(NewBounds.Top, Control.Height + SplitterWidth);
        alBottom: Dec(NewBounds.Bottom, Control.Height + SplitterWidth);
        alLeft: Inc(NewBounds.Left, Control.Width + SplitterWidth);
        alRight: Dec(NewBounds.Right, Control.Width + SplitterWidth);
      end;
      if StoredConstraintsValid then
      begin
        // restore constraints
        with Site.Constraints do
        begin
          MinWidth := FStoredConstraints.Left;
          MinHeight := FStoredConstraints.Top;
          MaxWidth := FStoredConstraints.Right;
          MaxHeight := FStoredConstraints.Bottom;
        end;
        FStoredConstraints := Rect(0, 0, 0, 0);
      end;
      Site.BoundsRect := NewBounds;

      // Site can dock a control again
      DragManager.RegisterDockSite(Site, True);
    end;
  end;
end;

procedure TGlassDockManager.ResetBounds(Force: boolean);
var
  OldSiteClientRect: TRect;
  WidthDiff: integer;
  HeightDiff: integer;
  ClientRectChanged: boolean;
  //..............................................
  procedure AlignChilds;
  var
    i: integer;
    b: TRect;
    AControl: TControl;
    ChildMaxSize: TPoint;
    SiteMinSize: TPoint;
    Child: TGlassDockHostSite;
  begin
    if ClientRectChanged and VarDockMaster.Restoring then
    begin
      // ClientRect changed => restore bounds
      for i := 0 to Site.ControlCount - 1 do
      begin
        AControl := Site.Controls[i];
        if AControl = nil then continue;

        b := Rect(0, 0, 0, 0);
        if AControl is TGlassDockHostSite then
          b := TGlassDockHostSite(AControl).DockRestoreBounds
        else if AControl is TGlassDockSplitter then
          b := TGlassDockSplitter(AControl).DockRestoreBounds;
        if (b.Right <= b.Left) or (b.Bottom <= b.Top) then
          b := AControl.BoundsRect;

        if AControl is TGlassDockSplitter then
        begin
          // fit splitter into clientarea
          if AControl.AnchorSide[akLeft].Control = nil then
            b.Left := Max(0, Min(b.Left, Site.ClientWidth - 10));
          if AControl.AnchorSide[akTop].Control = nil then
            b.Top := Max(0, Min(b.Top, Site.ClientHeight - 10));
          if TGlassDockSplitter(AControl).ResizeAnchor in [akLeft, akRight] then
          begin
            b.Right := b.Left + VarDockMaster.SplitterWidth;
            b.Bottom := Max(1, Min(b.Bottom, Site.ClientHeight - b.Top));
          end
          else
          begin
            b.Right := Max(1, Min(b.Right, Site.ClientWidth - b.Left));
            b.Bottom := b.Top + VarDockMaster.SplitterWidth;
          end;
        end;

        AControl.BoundsRect := b;
        if AControl is TGlassDockSplitter then
          TGlassDockSplitter(AControl).UpdateDockBounds;
      end;
      exit;
    end;

    if DockSite <> nil then exit;
    Child := TGlassDockHostSite(GetChildSite); // DaThoX
    if Child = nil then exit;

    ChildMaxSize := Point(Site.ClientWidth - VarDockMaster.SplitterWidth, Site.ClientHeight - VarDockMaster.SplitterWidth);
    if PreferredSiteSizeAsSiteMinimum then
    begin
      SiteMinSize := GetSitePreferredClientSize;
      if Child.Align in [alLeft, alRight] then
      begin
        ChildMaxSize.X := Max(0, (ChildMaxSize.X - SiteMinSize.X));
      end
      else
      begin
        ChildMaxSize.Y := Max(0, (ChildMaxSize.Y - SiteMinSize.Y));
      end;

    end;

    case ResizePolicy of
      admrpChild:
      begin
        if Child.Align in [alLeft, alRight] then
          Child.Width := Max(1, Min(ChildMaxSize.X, Child.Width + WidthDiff))
        else
        begin
          i := Max(1, Min(ChildMaxSize.Y, Child.Height + HeightDiff));

          Child.Height := i;
        end;
      end;
    end;
  end;
  //..............................................
begin
  // if Force then ;

  OldSiteClientRect := FSiteClientRect;
  FSiteClientRect := Site.ClientRect;
  WidthDiff := FSiteClientRect.Right - OldSiteClientRect.Right;
  HeightDiff := FSiteClientRect.Bottom - OldSiteClientRect.Bottom;
  ClientRectChanged := (WidthDiff <> 0) or (HeightDiff <> 0);
  if ClientRectChanged or PreferredSiteSizeAsSiteMinimum then
    AlignChilds;
end;

procedure TGlassDockManager.SaveToStream(Stream: TStream);
begin
  if Stream = nil then
  ;
end;

function TGlassDockManager.GetDockEdge(ADockObject: TDragDockObject): boolean;
var
  BestDistance: integer;

  procedure FindMinDistance(CurAlign: TAlign; CurDistance: integer);
  begin
    if CurDistance < 0 then
      CurDistance := -CurDistance;
    if CurDistance >= BestDistance then exit;
    ADockObject.DropAlign := CurAlign;
    BestDistance := CurDistance;
  end;

var
  p: TPoint;
  LastTabRect: TRect;
  TabIndex: longint;
begin
  if DockableSites = [] then
  begin
    ADockObject.DropAlign := alNone;
    exit(False);
  end;

  p := Site.ScreenToClient(ADockObject.DragPos);
  if (DockSite <> nil) and (DockSite.Pages <> nil) then
  begin
    // page docking
    ADockObject.DropAlign := alClient;
    p := DockSite.Pages.ScreenToClient(ADockObject.DragPos);
    LastTabRect := DockSite.Pages.TabRect(DockSite.Pages.PageCount - 1);
    if (p.Y >= LastTabRect.Top) and (p.y < LastTabRect.Bottom) then
    begin
      // specific tab
      if p.X >= LastTabRect.Right then
      begin
        // insert as last
        ADockObject.DropOnControl := DockSite.Pages;
      end
      else
      begin
        TabIndex := DockSite.Pages.TabIndexAtClientPos(p);
        if TabIndex >= 0 then
        begin
          // insert in front of an existing
          ADockObject.DropOnControl := DockSite.Pages.Page[TabIndex];
        end;
      end;
    end;
  end
  else if (DockSite <> nil) and PtInRect(DockSite.GetPageArea, p) then
  begin
    // page docking
    ADockObject.DropAlign := alClient;
  end
  else
  begin
    // check side
    BestDistance := High(integer);
    if akLeft in DockableSites then   FindMinDistance(alLeft, p.X);
    if akRight in DockableSites then  FindMinDistance(alRight, Site.ClientWidth - p.X);
    if akTop in DockableSites then    FindMinDistance(alTop, p.Y);
    if akBottom in DockableSites then FindMinDistance(alBottom, Site.ClientHeight - p.Y);

    // check inside
    if InsideDockingAllowed and (((ADockObject.DropAlign = alLeft) and (p.X >= 0)) or
      ((ADockObject.DropAlign = alTop) and (p.Y >= 0)) or ((ADockObject.DropAlign = alRight) and (p.X < Site.ClientWidth)) or
      ((ADockObject.DropAlign = alBottom) and (p.Y < Site.ClientHeight))) then
      ADockObject.DropOnControl := Site
    else
      ADockObject.DropOnControl := nil;
  end;

  Result := True;
end;

procedure TGlassDockManager.RestoreSite(SplitterPos: integer);
var
  ChildSite: TGlassDockHostSite;
begin

  FSiteClientRect := Site.ClientRect;
  if DockSite <> nil then
    exit;
  ChildSite := TGlassDockHostSite(GetChildSite); // DaThoX

  if ChildSite <> nil then
  begin
    ChildSite.CreateBoundSplitter;
    ChildSite.PositionBoundSplitter;
    if ChildSite.Align in [alLeft, alRight] then
      ChildSite.BoundSplitter.Left := SplitterPos
    else
      ChildSite.BoundSplitter.Top := SplitterPos;
    case ChildSite.Align of
      alTop: ChildSite.Height := ChildSite.BoundSplitter.Top;
      // DaThoX (src: http://bugs.freepascal.org/view.php?id=18538)
      // alBottom: ChildSite.Height := Site.ClientHeight - (ChildSite.BoundSplitter.Top + ChildSite.BoundSplitter.Height);
      alLeft: ChildSite.Width := ChildSite.BoundSplitter.Left;
      alRight: ChildSite.Width := Site.ClientWidth - (ChildSite.BoundSplitter.Left + ChildSite.BoundSplitter.Width);
    end;
    // only allow to dock one control
    DragManager.RegisterDockSite(Site, False);

  end;
end;

procedure TGlassDockManager.StoreConstraints;
begin
  with Site.Constraints do
    FStoredConstraints := Rect(MinWidth, MinHeight - 10, MaxWidth, MaxHeight); // DaThoX -10 to hide components pallete
end;

function TGlassDockManager.GetSitePreferredClientSize: TPoint;
{ Compute the preferred inner size of Site without the ChildSite and without
  the splitter
}
var
  ChildSite: TGlassDockHostSite;
  Splitter:  TGlassDockSplitter;
  SplitterSize: TPoint;
  i: Integer;
  ChildControl: TControl;
  PrefWidth: Integer;
  PrefHeight: Integer;
  SplitterAnchor: TAnchorKind; // side where a child is anchored to the splitter
  ChildPrefWidth: integer;
  ChildPrefHeight: integer;
  ChildBottom: Integer;
  ChildRight: Integer;
begin
  Result:=Point(0,0);
  Site.GetPreferredSize(Result.X,Result.Y);
  // compute the bounds without the Splitter and ChildSite
  ChildSite:=TGlassDockHostSite(GetChildSite);
  if ChildSite=nil then exit;
  Splitter:=ChildSite.BoundSplitter;
  if Splitter=nil then exit;
  SplitterSize:=Point(0,0);
  Splitter.GetPreferredSize(SplitterSize.X,SplitterSize.Y);
  PrefWidth:=0;
  PrefHeight:=0;
  if ChildSite.Align in [alLeft,alRight] then
    PrefHeight:=Result.Y
  else
    PrefWidth:=Result.X;
  SplitterAnchor:=MainAlignAnchor[ChildSite.Align];
  for i:=0 to Site.ControlCount-1 do begin
    ChildControl:=Site.Controls[i];
    if (ChildControl=Splitter) or (ChildControl=ChildSite) then continue;
    if (ChildControl.AnchorSide[SplitterAnchor].Control=Splitter)
    or ((ChildControl.Align in [alLeft,alTop,alRight,alBottom,alClient])
      and (SplitterAnchor in AnchorAlign[ChildControl.Align]))
    then begin
      // this control could be resized by the splitter
      // => use its position and preferred size for a preferred size of the ChildSite
      ChildPrefWidth:=0;
      ChildPrefHeight:=0;
      ChildControl.GetPreferredSize(ChildPrefWidth,ChildPrefHeight);

      case ChildSite.Align of
      alTop:
        begin
          ChildBottom:=ChildControl.Top+ChildControl.Height;
          PrefHeight:=Max(PrefHeight,Site.ClientHeight-ChildBottom-ChildPrefHeight);
        end;
      alBottom:
        PrefHeight:=Max(PrefHeight,ChildControl.Top+ChildPrefHeight);
      alLeft:
        begin
          ChildRight:=ChildControl.Left+ChildControl.Width;
          PrefWidth:=Max(PrefWidth,Site.ClientWidth-ChildRight-ChildPrefWidth);
        end;
      alRight:
        PrefWidth:=Max(PrefWidth,ChildControl.Left+ChildPrefWidth);
      end;
    end;
  end;
  Result.X:=PrefWidth;
  Result.Y:=PrefHeight;
end;

function TGlassDockManager.GetChildSite: TWinControl; // DaThoX
var
  i: integer;
begin
  for i := 0 to Site.ControlCount - 1 do
    if Site.Controls[i] is TGlassDockHostSite then
    begin
      Result := TCustomForm(Site.Controls[i]); // DaThoX
      exit;
    end;
  Result := nil;
end;

function TGlassDockManager.StoredConstraintsValid: boolean;
begin
  with FStoredConstraints do
    Result := (Left <> 0) or (Top <> 0) or (Right <> 0) or (Bottom <> 0);
end;

//======================== TGlassDockSplitter ==============================================

procedure TGlassDockSplitter.SetResizeAnchor(const AValue: TAnchorKind);
begin
  inherited SetResizeAnchor(AValue);

  case ResizeAnchor of
    akLeft: Anchors := AnchorAlign[alLeft];
    akTop: Anchors := AnchorAlign[alTop];
    akRight: Anchors := AnchorAlign[alRight];
    akBottom: Anchors := AnchorAlign[alBottom];
  end;
end;

procedure TGlassDockSplitter.PopupMenuPopup(Sender: TObject);
begin

end;

procedure TGlassDockSplitter.UpdateDockBounds;
begin
  FDockBounds := BoundsRect;
  if Parent <> nil then
  begin
    FDockParentClientSize.cx := Parent.ClientWidth;
    FDockParentClientSize.cy := Parent.ClientHeight;
  end
  else
  begin
    FDockParentClientSize.cx := 0;
    FDockParentClientSize.cy := 0;
  end;
end;

procedure TGlassDockSplitter.SetBounds(ALeft, ATop, AWidth, AHeight: integer);
begin
  DisableAutoSizing;
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  UpdateDockBounds;
  EnableAutoSizing;
end;

procedure TGlassDockSplitter.SetBoundsKeepDockBounds(ALeft, ATop, AWidth, AHeight: integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
end;

function TGlassDockSplitter.SideAnchoredControlCount(Side: TAnchorKind): integer;
var
  Sibling: TControl;
  i: integer;
begin
  Result := 0;
  for i := 0 to AnchoredControlCount - 1 do
  begin
    Sibling := AnchoredControls[i];
    if Sibling.AnchorSide[OppositeAnchor[Side]].Control = Self then
      Inc(Result);
  end;
end;

function TGlassDockSplitter.HasAnchoredControls: boolean;
// returns true if this splitter has at least one non splitter control anchored to it
var
  i: Integer;
  Sibling: TControl;
begin
  Result:=false;
  for i:=0 to AnchoredControlCount-1 do begin
    Sibling:=AnchoredControls[i];
    if Sibling is TGlassDockSplitter then continue;
    exit(true);
  end;
end;

procedure TGlassDockSplitter.SaveLayout(LayoutNode: TGlassDockLayoutTreeNode);
begin
  if ResizeAnchor in [akLeft, akRight] then
    LayoutNode.NodeType := adltnSplitterVertical else
    LayoutNode.NodeType := adltnSplitterHorizontal;

  LayoutNode.Assign(Self);
end;

function TGlassDockSplitter.HasOnlyOneSibling(Side: TAnchorKind; MinPos, MaxPos: integer): TControl;
var
  i: integer;
  AControl: TControl;
begin
  Result := nil;
  for i := 0 to AnchoredControlCount - 1 do
  begin
    AControl := AnchoredControls[i];
    if AControl = nil then continue;

    if AControl.AnchorSide[OppositeAnchor[Side]].Control <> Self then continue;
    // AControl is anchored at Side to this splitter
    if (Side in [akLeft, akRight]) then
    begin
      if (AControl.Left > MaxPos) or (AControl.Left + AControl.Width < MinPos) then continue;
    end
    else
    begin
      if (AControl.Top > MaxPos) or (AControl.Top + AControl.Height < MinPos) then continue;
    end;
    // AControl is in range
    if Result = nil then
      Result := AControl
    else
    begin
      // there is more than one control
      Result := nil;
      exit;
    end;
  end;
end;

constructor TGlassDockSplitter.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Align := alNone;
  ResizeAnchor := akLeft;
  // make sure the splitter never vanish
  Constraints.MinWidth := 2;
  Constraints.MinHeight := 2;
  PopupMenu := VarDockMaster.GetPopupMenu;
  MinSize := 24;
end;

//===================== TGlassDockPageControl ==================================================

function TGlassDockPageControl.GetDockPages(Index: integer): TGlassDockPage;
begin
  Result := TGlassDockPage(Page[Index]);
end;

procedure TGlassDockPageControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
  ATabIndex: longint;
  APage: TCustomPage;
  Site: TGlassDockHostSite;
begin
  inherited MouseDown(Button, Shift, X, Y);
  ATabIndex := TabIndexAtClientPos(Point(X, Y));
  if (Button = mbLeft) and VarDockMaster.AllowDragging and (ATabIndex >= 0) then
  begin
    APage := Page[ATabIndex];
    if (APage.ControlCount > 0) and (APage.Controls[0] is TGlassDockHostSite) then
    begin
      Site := TGlassDockHostSite(APage.Controls[0]);
      DragManager.DragStart(Site, False, VarDockMaster.DragTreshold);
    end;
  end;
end;

procedure TGlassDockPageControl.PopupMenuPopup(Sender: TObject);
var
  ContainsMainForm: boolean;
  s: string;
  TabPositionSection: TMenuItem;
  Item: TMenuItem;
  tp: TTabPosition;
begin
  if VarDockMaster = nil then exit;

  // movement
  if PageIndex > 0 then
    VarDockMaster.AddPopupMenuItem('MoveLeftMenuItem', adrsMovePageLeft, @MoveLeftButtonClick);

  if PageIndex > 1 then
    VarDockMaster.AddPopupMenuItem('MoveLeftMostMenuItem', adrsMovePageLeftmost, @MoveLeftMostButtonClick);

  if PageIndex < PageCount - 1 then
    VarDockMaster.AddPopupMenuItem('MoveRightMenuItem', adrsMovePageRight, @MoveRightButtonClick);

  if PageIndex < PageCount - 2 then
    VarDockMaster.AddPopupMenuItem('MoveRightMostMenuItem', adrsMovePageRightmost, @MoveRightMostButtonClick);

  // tab position
  TabPositionSection := VarDockMaster.AddPopupMenuItem('TabPositionMenuItem', adrsTabPosition, nil);

  for tp := Low(TTabPosition) to high(TTabPosition) do
  begin
    case tp of
      tpTop: s := adrsTop;
      tpBottom: s := adrsBottom;
      tpLeft: s := adrsLeft;
      tpRight: s := adrsRight;
    end;
    Item := VarDockMaster.AddPopupMenuItem('TabPos' + ADLTabPostionNames[tp] + 'MenuItem', s, @TabPositionClick, TabPositionSection);
    Item.ShowAlwaysCheckable := True;
    Item.Checked := TabPosition = tp;
    Item.Tag := Ord(tp);
  end;

  // close
  ContainsMainForm := IsParentOf(Application.MainForm);
  if ContainsMainForm then
    s := Format(adrsQuit, [Application.Title]) else
    s := adrsClose;

  VarDockMaster.AddPopupMenuItem('CloseMenuItem', s, @CloseButtonClick);
end;

procedure TGlassDockPageControl.CloseButtonClick(Sender: TObject);
var
  Site: TGlassDockHostSite;
begin
  Site := GetActiveSite;
  if Site = nil then
    exit;
  VarDockMaster.RestoreLayouts.Add(VarDockMaster.CreateRestoreLayout(Site), True);
  Site.CloseSite;
  VarDockMaster.SimplifyPendingLayouts;
end;

procedure TGlassDockPageControl.MoveLeftButtonClick(Sender: TObject);
begin
  if PageIndex > 0 then
    Page[PageIndex].PageIndex := Page[PageIndex].PageIndex - 1;
end;

procedure TGlassDockPageControl.MoveLeftMostButtonClick(Sender: TObject);
begin
  if PageIndex > 0 then
    Page[PageIndex].PageIndex := 0;
end;

procedure TGlassDockPageControl.MoveRightButtonClick(Sender: TObject);
begin
  if PageIndex < PageCount - 1 then
    Page[PageIndex].PageIndex := Page[PageIndex].PageIndex + 1;
end;

procedure TGlassDockPageControl.MoveRightMostButtonClick(Sender: TObject);
begin
  if PageIndex < PageCount - 1 then
    Page[PageIndex].PageIndex := PageCount - 1;
end;

procedure TGlassDockPageControl.TabPositionClick(Sender: TObject);
var
  Item: TMenuItem;
begin
  if not (Sender is TMenuItem) then
    exit;
  Item := TMenuItem(Sender);
  TabPosition := TTabPosition(Item.Tag);
end;

procedure TGlassDockPageControl.UpdateDockCaption(Exclude: TControl);
begin
  if Exclude = nil then
  ;
end;

procedure TGlassDockPageControl.RemoveControl(AControl: TControl);
begin
  inherited RemoveControl(AControl);
  if (not (csDestroying in ComponentState)) then
  begin
    if (PageCount <= 1) and (Parent is TGlassDockHostSite) then
      VarDockMaster.NeedSimplify(Parent);
  end;
end;

function TGlassDockPageControl.GetActiveSite: TGlassDockHostSite;
var
  CurPage: TCustomPage;
  CurDockPage: TGlassDockPage;
begin
  Result := nil;
  CurPage := ActivePageComponent;
  if not (CurPage is TGlassDockPage) then
    exit;
  CurDockPage := TGlassDockPage(CurPage);
  Result := CurDockPage.GetSite;
end;

constructor TGlassDockPageControl.Create(TheOwner: TComponent);
begin
  PageClass := VarDockMaster.PageClass;
  inherited Create(TheOwner);
  PopupMenu := VarDockMaster.GetPopupMenu;
end;

//========================== TGlassDockPage ============================================

procedure TGlassDockPage.UpdateDockCaption(Exclude: TControl);
var
  i: integer;
  Child: TControl;
  NewCaption: string;
begin
  NewCaption := '';
  for i := 0 to ControlCount - 1 do
  begin
    Child := Controls[i];
    if Child = Exclude then continue;
    if not (Child is TGlassDockHostSite) then continue;
    if NewCaption <> '' then NewCaption := NewCaption + ',';
    NewCaption := NewCaption + Child.Caption;
  end;

  if Caption = NewCaption then exit;
  Caption := NewCaption;
  if Parent is TGlassDockPageControl then
    TGlassDockPageControl(Parent).UpdateDockCaption;
end;

procedure TGlassDockPage.InsertControl(AControl: TControl; Index: integer);
begin
  inherited InsertControl(AControl, Index);

  if AControl is TGlassDockHostSite then
  begin
    if TGlassDockHostSite(AControl).Header <> nil then
      TGlassDockHostSite(AControl).Header.Parent := nil;
    UpdateDockCaption;
  end;
end;

procedure TGlassDockPage.RemoveControl(AControl: TControl);
begin
  inherited RemoveControl(AControl);
  if (GetSite = nil) and
     (not (csDestroying in ComponentState)) and
     (Parent <> nil) and
     (not (csDestroying in Parent.ComponentState)) then
     VarDockMaster.NeedSimplify(Self);
end;

function TGlassDockPage.GetSite: TGlassDockHostSite;
begin
  Result := nil;
  if ControlCount = 0 then exit;
  if not (Controls[0] is TGlassDockHostSite) then exit;
  Result := TGlassDockHostSite(Controls[0]);
end;

end.
