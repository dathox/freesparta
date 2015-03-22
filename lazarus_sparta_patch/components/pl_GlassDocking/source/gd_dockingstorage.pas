
{**********************************************************************
 Package pl_GlassDocking.pkg
 is a modification of AnchorDocking Library  (http://wiki.lazarus.freepascal.org/Anchor_Docking)
 for CodeTyphon Studio
 This unit is part of CodeTyphon Project (http://www.pilotlogic.com/)
***********************************************************************}

unit gd_dockingstorage;

{$mode objfpc}{$H+}

interface

uses
  Math, Classes, SysUtils, LCLProc, AvgLvlTree, ExtCtrls, ComCtrls, Forms,
  Controls, LazConfigStorage, gd_dockingstr;

const
  cnGlassDockSplitterName = 'GlassDockSplitter';
  cnGlassDockSiteName = 'GlassDockSite';

type
  TADLTreeNodeType = (
    adltnNone,
    adltnLayout,
    adltnControl,
    adltnSplitterHorizontal,
    adltnSplitterVertical,
    adltnPages,
    adltnCustomSite
    );
  TADLTreeNodeTypes = set of TADLTreeNodeType;

  TADLHeaderPosition = (
    adlhpAuto,
    adlhpTop,
    adlhpLeft,
    adlhpRight,
    adlhpBottom
    );
  TADLHeaderPositions = set of TADLHeaderPosition;

  EAnchorDockLayoutError = class(Exception);


  TGlassDockLayoutTreeNode = class
  private
    FAlign: TAlign;
    fAnchors: array[TAnchorKind] of string;
    FBoundSplitterPos: integer;
    FBoundsRect: TRect;
    FHeaderPosition: TADLHeaderPosition;
    FMonitor: integer;
    FName: string;
    FNodes: TFPList; // list of TGlassDockLayoutTreeNode
    FNodeType: TADLTreeNodeType;
    FParent: TGlassDockLayoutTreeNode;
    FWorkAreaRect: TRect;
    FTabPosition: TTabPosition;
    FWindowState: TWindowState;
    function GetAnchors(Site: TAnchorKind): string;
    function GetBottom: integer;
    function GetHeight: integer;
    function GetLeft: integer;
    function GetNodes(Index: integer): TGlassDockLayoutTreeNode;
    function GetRight: integer;
    function GetTop: integer;
    function GetWidth: integer;
    procedure SetAlign(const AValue: TAlign);
    procedure SetAnchors(Site: TAnchorKind; const AValue: string);
    procedure SetBottom(const AValue: integer);
    procedure SetBoundSplitterPos(const AValue: integer);
    procedure SetBoundsRect(const AValue: TRect);
    procedure SetHeaderPosition(const AValue: TADLHeaderPosition);
    procedure SetHeight(const AValue: integer);
    procedure SetLeft(const AValue: integer);
    procedure SetMonitor(const AValue: integer);
    procedure SetName(const AValue: string);
    procedure SetNodeType(const AValue: TADLTreeNodeType);
    procedure SetParent(const AValue: TGlassDockLayoutTreeNode);
    procedure SetRight(const AValue: integer);
    procedure SetWorkAreaRect(const AValue: TRect);
    procedure SetTabPosition(const AValue: TTabPosition);
    procedure SetTop(const AValue: integer);
    procedure SetWidth(const AValue: integer);
    procedure SetWindowState(const AValue: TWindowState);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function IsEqual(Node: TGlassDockLayoutTreeNode): boolean;
    procedure Assign(Node: TGlassDockLayoutTreeNode);
    procedure Assign(AControl: TControl);
    procedure LoadFromConfig(Config: TConfigStorage);
    procedure SaveToConfig(Config: TConfigStorage);
    function FindChildNode(aName: string; Recursive: boolean): TGlassDockLayoutTreeNode;
    function FindControlNode: TGlassDockLayoutTreeNode;
    procedure CheckConsistency; virtual;

    // simplifying
    procedure Simplify(ExistingNames: TStrings);
    procedure DeleteNode(ChildNode: TGlassDockLayoutTreeNode);
    function FindNodeBoundSplitter(ChildNode: TGlassDockLayoutTreeNode; Side: TAnchorKind): TGlassDockLayoutTreeNode;
    procedure DeleteNodeBoundSplitter(Splitter, ChildNode: TGlassDockLayoutTreeNode; Side: TAnchorKind);
    procedure DeleteSpiralSplitter(ChildNode: TGlassDockLayoutTreeNode);
    procedure ReplaceWithChildren(ChildNode: TGlassDockLayoutTreeNode);

    // properties
    procedure IncreaseChangeStamp; virtual;
    property Name: string read FName write SetName;
    property NodeType: TADLTreeNodeType read FNodeType write SetNodeType;
    property Parent: TGlassDockLayoutTreeNode read FParent write SetParent;
    property Left: integer read GetLeft write SetLeft;
    property Top: integer read GetTop write SetTop;
    property Width: integer read GetWidth write SetWidth;
    property Height: integer read GetHeight write SetHeight;
    property Right: integer read GetRight write SetRight;
    property Bottom: integer read GetBottom write SetBottom;
    property BoundsRect: TRect read FBoundsRect write SetBoundsRect;
    property BoundSplitterPos: integer read FBoundSplitterPos write SetBoundSplitterPos;
    property WorkAreaRect: TRect read FWorkAreaRect write SetWorkAreaRect;
    property Anchors[Site: TAnchorKind]: string read GetAnchors write SetAnchors; // empty means default (parent)
    property Align: TAlign read FAlign write SetAlign;
    property WindowState: TWindowState read FWindowState write SetWindowState;
    property Monitor: integer read FMonitor write SetMonitor;
    property HeaderPosition: TADLHeaderPosition read FHeaderPosition write SetHeaderPosition;
    property TabPosition: TTabPosition read FTabPosition write SetTabPosition;
    function Count: integer;
    function IsSplitter: boolean;
    function IsRootWindow: boolean;
    property Nodes[Index: integer]: TGlassDockLayoutTreeNode read GetNodes; default;
  end;

  TGlassDockLayoutTree = class;

  TGlassDockLayoutTreeRootNode = class(TGlassDockLayoutTreeNode)
  private
    FTree: TGlassDockLayoutTree;
  public
    procedure IncreaseChangeStamp; override;
    property Tree: TGlassDockLayoutTree read FTree write FTree;
    procedure CheckConsistency; override;
  end;

  TGlassDockLayoutTree = class
  private
    FChangeStamp: int64;
    FSavedChangeStamp: int64;
    FRoot: TGlassDockLayoutTreeRootNode;
    function GetModified: boolean;
    procedure SetModified(const AValue: boolean);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure LoadFromConfig(Config: TConfigStorage);
    procedure SaveToConfig(Config: TConfigStorage);
    procedure IncreaseChangeStamp;
    property ChangeStamp: int64 read FChangeStamp;
    property Modified: boolean read GetModified write SetModified;
    property Root: TGlassDockLayoutTreeRootNode read FRoot;
    function NewNode(aParent: TGlassDockLayoutTreeNode): TGlassDockLayoutTreeNode;
    procedure Assign(Source: TObject);
  end;

  TGlassDockRestoreLayout = class
  private
    FControlNames: TStrings;
    FLayout: TGlassDockLayoutTree;
    procedure SetControlNames(const AValue: TStrings);
  public
    constructor Create; overload;
    constructor Create(aLayout: TGlassDockLayoutTree); overload;
    destructor Destroy; override;
    function IndexOfControlName(AName: string): integer;
    function HasControlName(AName: string): boolean;
    procedure RemoveControlName(AName: string);
    procedure UpdateControlNames;
    procedure LoadFromConfig(Config: TConfigStorage);
    procedure SaveToConfig(Config: TConfigStorage);
    property ControlNames: TStrings read FControlNames write SetControlNames;
    property Layout: TGlassDockLayoutTree read FLayout;
  end;

  TGlassDockRestoreLayouts = class
  private
    fItems: TFPList;
    function GetItems(Index: integer): TGlassDockRestoreLayout;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Delete(Index: integer);
    function IndexOfName(AControlName: string): integer;
    function FindByName(AControlName: string): TGlassDockRestoreLayout;
    procedure Add(Layout: TGlassDockRestoreLayout; RemoveOther: boolean);
    procedure RemoveByName(AControlName: string);
    procedure LoadFromConfig(Config: TConfigStorage);
    procedure SaveToConfig(Config: TConfigStorage);
    function ConfigIsEmpty(Config: TConfigStorage): boolean;
    function Count: integer;
    property Items[Index: integer]: TGlassDockRestoreLayout read GetItems;
  end;

  TADNameToControl = class
  private
    fItems: TStringList;
    function IndexOfName(const aName: string): integer;
    function GetControl(const aName: string): TControl;
    procedure SetControl(const aName: string; const AValue: TControl);
  public
    constructor Create;
    destructor Destroy; override;
    function ControlToName(AControl: TControl): string;
    property Control[const aName: string]: TControl read GetControl write SetControl; default;
    procedure RemoveControl(AControl: TControl);
    procedure WriteDebugReport(Msg: string);
  end;

const
  ADLTreeNodeTypeNames: array[TADLTreeNodeType] of string = (
    'None',
    'Layout',
    'Control',
    'SplitterHorizontal',
    'SplitterVertical',
    'Pages',
    'CustomSite'
    );
  ADLWindowStateNames: array[TWindowState] of string = (
    'Normal',
    'Minimized',
    'Maximized',
    'Fullscreen'
    );
  ADLHeaderPositionNames: array[TADLHeaderPosition] of string = (
    'auto',
    'left',
    'top',
    'right',
    'bottom'
    );
  ADLTabPostionNames: array[TTabPosition] of string = (
    'Top',
    'Bottom',
    'Left',
    'Right'
    );
  ADLAlignNames: array[TAlign] of string = (
    'None',
    'Top',
    'Bottom',
    'Left',
    'Right',
    'Client',
    'Custom'
    );

function NameToADLTreeNodeType(s: string): TADLTreeNodeType;
function NameToADLWindowState(s: string): TWindowState;
function NameToADLHeaderPosition(s: string): TADLHeaderPosition;
function NameToADLTabPosition(s: string): TTabPosition;
function NameToADLAlign(s: string): TAlign;
function dbgs(const NodeType: TADLTreeNodeType): string; overload;

procedure WriteDebugLayout(Title: string; RootNode: TObject);
function DebugLayoutAsString(RootNode: TObject): string;
procedure DebugWriteChildAnchors(RootNode: TGlassDockLayoutTreeNode); overload;
procedure DebugWriteChildAnchors(RootControl: TWinControl; OnlyWinControls, OnlyForms: boolean); overload;

implementation

function NameToADLTreeNodeType(s: string): TADLTreeNodeType;
begin
  for Result := low(TADLTreeNodeType) to high(TADLTreeNodeType) do
    if s = ADLTreeNodeTypeNames[Result] then
      exit;
  Result := adltnNone;
end;

function NameToADLWindowState(s: string): TWindowState;
begin
  for Result := low(TWindowState) to high(TWindowState) do
    if s = ADLWindowStateNames[Result] then
      exit;
  Result := wsNormal;
end;

function NameToADLHeaderPosition(s: string): TADLHeaderPosition;
begin
  for Result := low(TADLHeaderPosition) to high(TADLHeaderPosition) do
    if s = ADLHeaderPositionNames[Result] then
      exit;
  Result := adlhpAuto;
end;

function NameToADLTabPosition(s: string): TTabPosition;
begin
  for Result := low(TTabPosition) to high(TTabPosition) do
    if s = ADLTabPostionNames[Result] then
      exit;
  Result := tpTop;
end;

function NameToADLAlign(s: string): TAlign;
begin
  for Result := low(TAlign) to high(TAlign) do
    if s = ADLAlignNames[Result] then
      exit;
  Result := alNone;
end;

function dbgs(const NodeType: TADLTreeNodeType): string; overload;
begin
  Result := ADLTreeNodeTypeNames[NodeType];
end;

procedure WriteDebugLayout(Title: string; RootNode: TObject);
begin
  //....
end;

function DebugLayoutAsString(RootNode: TObject): string;
type
  TNodeInfo = record
    MinSize: TPoint;
    MinSizeValid, MinSizeCalculating: boolean;
    MinLeft: integer;
    MinLeftValid, MinLeftCalculating: boolean;
    MinTop: integer;
    MinTopValid, MinTopCalculating: boolean;
  end;
  PNodeInfo = ^TNodeInfo;
var
  Cols: longint;
  Rows: longint;
  LogCols: integer;
  NodeInfos: TPointerToPointerTree;// TObject to PNodeInfo

  procedure InitNodeInfos;
  begin
    NodeInfos := TPointerToPointerTree.Create;
  end;

  procedure FreeNodeInfos;
  var
    Item: PNodeInfo;
    NodePtr, InfoPtr: Pointer;
  begin
    NodeInfos.GetFirst(NodePtr, InfoPtr);
    repeat
      Item := PNodeInfo(InfoPtr);
      if Item = nil then
        break;
      Dispose(Item);
    until not NodeInfos.GetNext(NodePtr, NodePtr, InfoPtr);
    NodeInfos.Free;
  end;

  function GetNodeInfo(Node: TObject): PNodeInfo;
  begin
    Result := PNodeInfo(NodeInfos[Node]);
    if Result = nil then
    begin
      New(Result);
      FillChar(Result^, SizeOf(TNodeInfo), 0);
      NodeInfos[Node] := Result;
    end;
  end;

  procedure w(x, y: integer; const s: string; MaxX: integer = 0);
  var
    i: integer;
  begin
    for i := 1 to length(s) do
    begin
      if (MaxX > 0) and (x + i > MaxX) then
        exit;
      Result[LogCols * (y - 1) + x + i - 1] := s[i];
    end;
  end;

  procedure wfillrect(const ARect: TRect; c: char);
  var
    x: longint;
    y: longint;
  begin
    for x := ARect.Left to ARect.Right do
      for y := ARect.Top to ARect.Bottom do
        w(x, y, c);
  end;

  procedure wrectangle(const ARect: TRect);
  begin
    w(ARect.Left, ARect.Top, '+');
    w(ARect.Right, ARect.Top, '+');
    w(ARect.Left, ARect.Bottom, '+');
    w(ARect.Right, ARect.Bottom, '+');
    if ARect.Left < ARect.Right then
    begin
      if ARect.Top < ARect.Bottom then
      begin
        wfillrect(Rect(ARect.Left + 1, ARect.Top, ARect.Right - 1, ARect.Top), '-');// top line
        wfillrect(Rect(ARect.Left + 1, ARect.Bottom, ARect.Right - 1, ARect.Bottom), '-');// bottom line
        wfillrect(Rect(ARect.Left, ARect.Top + 1, ARect.Left, ARect.Bottom - 1), '|');// left line
        wfillrect(Rect(ARect.Right, ARect.Top + 1, ARect.Right, ARect.Bottom - 1), '|');// right line
      end
      else
      begin
        wfillrect(Rect(ARect.Left + 1, ARect.Top, ARect.Right - 1, ARect.Top), '=');// horizontal line
      end;
    end
    else
    begin
      wfillrect(Rect(ARect.Left, ARect.Top + 1, ARect.Left, ARect.Bottom - 1), '#');// vertical line
    end;
  end;

  function MapRect(const OriginalRect, OldBounds, NewBounds: TRect): TRect;

    function MapX(i: integer): integer;
    begin
      Result := NewBounds.Left + (((i - OldBounds.Left) * (NewBounds.Right - NewBounds.Left)) div (OldBounds.Right - OldBounds.Left));
    end;

    function MapY(i: integer): integer;
    begin
      Result := NewBounds.Top + (((i - OldBounds.Top) * (NewBounds.Bottom - NewBounds.Top)) div (OldBounds.Bottom - OldBounds.Top));
    end;

  begin
    Result.Left := MapX(OriginalRect.Left);
    Result.Top := MapY(OriginalRect.Left);
    Result.Right := MapX(OriginalRect.Left);
    Result.Bottom := MapY(OriginalRect.Left);
  end;

  function GetParentNode(Node: TObject): TObject;
  begin
    if Node is TControl then
      Result := TControl(Node).Parent
    else if Node is TGlassDockLayoutTreeNode then
      Result := TGlassDockLayoutTreeNode(Node).Parent
    else
      Result := nil;
  end;

  function GetSiblingNode(Node: TObject; Side: TAnchorKind): TObject;
  begin
    Result := nil;
    if Node = nil then
      exit;
    if Node is TControl then
    begin
      if not (Side in TControl(Node).Anchors) then
        exit;
      Result := TControl(Node).AnchorSide[Side].Control;
      if Result = TControl(Node).Parent then
        Result := nil;
    end
    else if Node is TGlassDockLayoutTreeNode then
    begin
      if TGlassDockLayoutTreeNode(Node).Parent <> nil then
        Result := TGlassDockLayoutTreeNode(Node).Parent.FindChildNode(TGlassDockLayoutTreeNode(Node).Anchors[Side], False);
    end;
  end;

  function GetAnchorNode(Node: TObject; Side: TAnchorKind): TObject;
  var
    ADLNode: TGlassDockLayoutTreeNode;
  begin
    Result := nil;
    if Node = nil then
      exit;
    if Node is TControl then
    begin
      if not (Side in TControl(Node).Anchors) then
        exit;
      Result := TControl(Node).AnchorSide[Side].Control;
    end
    else if Node is TGlassDockLayoutTreeNode then
    begin
      ADLNode := TGlassDockLayoutTreeNode(Node);
      if ((ADLNode.NodeType = adltnSplitterVertical) and (Side in [akLeft, akRight])) or
        ((ADLNode.NodeType = adltnSplitterHorizontal) and (Side in [akTop, akBottom])) then
        Result := nil
      else if (ADLNode.Anchors[Side] <> '') then
      begin
        if ADLNode.Parent <> nil then
          Result := ADLNode.Parent.FindChildNode(ADLNode.Anchors[Side], False);
      end
      else
        Result := GetParentNode(Node);
    end;
  end;

  function IsSplitter(Node: TObject): boolean;
  begin
    Result := (Node is TCustomSplitter) or ((Node is TGlassDockLayoutTreeNode) and (TGlassDockLayoutTreeNode(Node).IsSplitter));
  end;

  function IsPages(Node: TObject): boolean;
  begin
    Result := (Node is TCustomTabControl) or ((Node is TGlassDockLayoutTreeNode) and
      (TGlassDockLayoutTreeNode(Node).NodeType in [adltnPages, adltnNone]));
  end;

  function GetName(Node: TObject): string;
  begin
    if Node is TControl then
      Result := TControl(Node).Name
    else if Node is TGlassDockLayoutTreeNode then
      Result := TGlassDockLayoutTreeNode(Node).Name
    else
      Result := DbgSName(Node);
  end;

  function GetChildCount(Node: TObject): integer;
  begin
    if Node is TWinControl then
      Result := TWinControl(Node).ControlCount
    else if Node is TGlassDockLayoutTreeNode then
      Result := TGlassDockLayoutTreeNode(Node).Count
    else
      Result := 0;
  end;

  function GetChild(Node: TObject; Index: integer): TObject;
  begin
    if Node is TWinControl then
      Result := TWinControl(Node).Controls[Index]
    else if Node is TGlassDockLayoutTreeNode then
      Result := TGlassDockLayoutTreeNode(Node).Nodes[Index]
    else
      Result := nil;
  end;

  function GetMinSize(Node: TObject): TPoint; forward;

  function GetMinPos(Node: TObject; Side: TAnchorKind): integer;
    // calculates left or top position of Node

    function Compute(var MinPosValid, MinPosCalculating: boolean; var MinPos: integer): integer;

      procedure Improve(Neighbour: TObject);
      var
        NeighbourPos: longint;
        NeighbourSize: TPoint;
        NeighbourLength: longint;
      begin
        if Neighbour = nil then
          exit;
        if GetParentNode(Neighbour) <> GetParentNode(Node) then
          exit;
        NeighbourPos := GetMinPos(Neighbour, Side);
        NeighbourSize := GetMinSize(Neighbour);
        if Side = akLeft then
          NeighbourLength := NeighbourSize.X
        else
          NeighbourLength := NeighbourSize.Y;
        MinPos := Max(MinPos, NeighbourPos + NeighbourLength);
      end;

    var
      Sibling: TObject;
      i: integer;
      ParentNode: TObject;
    begin
      if MinPosCalculating then
      begin
        if RootNode is TWinControl then
          DebugWriteChildAnchors(TWinControl(RootNode), True, True)
        else if RootNode is TGlassDockLayoutTreeNode then
          DebugWriteChildAnchors(TGlassDockLayoutTreeNode(RootNode));

        exit;  // RaiseGDBException('circle detected');

      end;
      if (not MinPosValid) then
      begin
        MinPosValid := True;
        MinPosCalculating := True;
        Sibling := GetSiblingNode(Node, Side);
        if Sibling <> nil then
          Improve(Sibling);
        ParentNode := GetParentNode(Node);
        if ParentNode <> nil then
        begin
          for i := 0 to GetChildCount(ParentNode) - 1 do
          begin
            Sibling := GetChild(ParentNode, i);
            if Node = GetSiblingNode(Sibling, OppositeAnchor[Side]) then
              Improve(Sibling);
          end;
        end;
        MinPosCalculating := False;
      end;
      Result := MinPos;
    end;

  var
    Info: PNodeInfo;
  begin
    Info := GetNodeInfo(Node);

    if Side = akLeft then
      Result := Compute(Info^.MinLeftValid, Info^.MinLeftCalculating, Info^.MinLeft)
    else
      Result := Compute(Info^.MinTopValid, Info^.MinTopCalculating, Info^.MinTop);
  end;

  function GetChildsMinSize(Node: TObject): TPoint;
    // calculate the minimum size needed to draw the content of the node
  var
    i: integer;
    Child: TObject;
    ChildMinSize: TPoint;
  begin

    Result := Point(0, 0);
    if IsPages(Node) then
    begin
      // maximum size of all pages
      for i := 0 to GetChildCount(Node) - 1 do
      begin
        ChildMinSize := GetMinSize(GetChild(Node, i));
        Result.X := Max(Result.X, ChildMinSize.X);
        Result.Y := Max(Result.Y, ChildMinSize.Y);
      end;
    end
    else
    begin
      for i := 0 to GetChildCount(Node) - 1 do
      begin
        Child := GetChild(Node, i);
        ChildMinSize := GetMinSize(Child);
        Result.X := Max(Result.X, GetMinPos(Child, akLeft) + ChildMinSize.X);
        Result.Y := Max(Result.Y, GetMinPos(Child, akTop) + ChildMinSize.Y);
      end;
    end;
  end;

  function GetMinSize(Node: TObject): TPoint;
    // calculate the minimum size needed to draw the node
  var
    ChildMinSize: TPoint;
    Info: PNodeInfo;
  begin

    Info := GetNodeInfo(Node);
    if Info^.MinSizeValid then
    begin
      Result := Info^.MinSize;
      exit;
    end;
    if Info^.MinSizeCalculating then
    begin
      DumpStack;
      Result := Point(1, 1);
      exit;
    end;
    Info^.MinSizeCalculating := True;
    Result.X := 2 + length(GetName(Node));// border plus name
    Result.Y := 2;  // border
    if GetChildCount(Node) = 0 then
    begin
      if IsSplitter(Node) then
        Result := Point(1, 1); // splitters don't need captions
    end
    else
    begin
      ChildMinSize := GetChildsMinSize(Node);
      Result.X := Max(Result.X, ChildMinSize.X + 2);
      Result.Y := Max(Result.Y, ChildMinSize.Y + 2);
    end;

    Info^.MinSize := Result;
    Info^.MinSizeValid := True;
    Info^.MinSizeCalculating := False;
  end;

  procedure DrawNode(Node: TObject; ARect: TRect);
  var
    i: integer;
    Child: TObject;
    ChildSize: TPoint;
    ChildRect: TRect;
    AnchorNode: TObject;
  begin
    wrectangle(ARect);
    w(ARect.Left + 1, ARect.Top, GetName(Node), ARect.Right);

    for i := 0 to GetChildCount(Node) - 1 do
    begin
      Child := GetChild(Node, i);
      ChildRect.Left := ARect.Left + 1 + GetMinPos(Child, akLeft);
      ChildRect.Top := ARect.Top + 1 + GetMinPos(Child, akTop);
      ChildSize := GetMinSize(Child);
      ChildRect.Right := ChildRect.Left + ChildSize.X - 1;
      ChildRect.Bottom := ChildRect.Top + ChildSize.Y - 1;
      AnchorNode := GetAnchorNode(Child, akRight);
      if AnchorNode <> nil then
      begin
        if AnchorNode = Node then
          ChildRect.Right := ARect.Right - 1
        else if GetParentNode(AnchorNode) = Node then
          ChildRect.Right := ARect.Left + 1 + GetMinPos(AnchorNode, akLeft) - 1;
      end;
      AnchorNode := GetAnchorNode(Child, akBottom);
      if AnchorNode <> nil then
      begin
        if AnchorNode = Node then
          ChildRect.Bottom := ARect.Bottom - 1
        else if GetParentNode(AnchorNode) = Node then
          ChildRect.Bottom := ARect.Top + 1 + GetMinPos(AnchorNode, akTop) - 1;
      end;
      DrawNode(Child, ChildRect);

      if IsPages(Node) then
      begin
        // paint only one page
        break;
      end;

    end;
  end;

var
  e: string;
  y: integer;
begin
  Cols := StrToIntDef(Application.GetOptionValue('ldcn-colunms'), 79);
  Rows := StrToIntDef(Application.GetOptionValue('ldcn-rows'), 20);

  InitNodeInfos;
  try
    e := LineEnding;
    LogCols := Cols + length(e);
    SetLength(Result, LogCols * Rows);
    // fill space
    FillChar(Result[1], length(Result), ' ');
    // add line endings
    for y := 1 to Rows do
      w(Cols + 1, y, e);
    // draw node
    DrawNode(RootNode, Rect(1, 1, Cols, Rows));
  finally
    FreeNodeInfos;
  end;
end;

procedure DebugWriteChildAnchors(RootNode: TGlassDockLayoutTreeNode);

  procedure WriteControl(Node: TGlassDockLayoutTreeNode; Prefix: string);
  var
    i: integer;
    a: TAnchorKind;
    AnchorControl: TGlassDockLayoutTreeNode;
    AnchorName: string;
  begin
    DbgOut(Prefix);
    DbgOut('"' + Node.Name + '"');
    DbgOut(' Type=' + dbgs(Node.NodeType));
    DbgOut(' Bounds=', dbgs(Node.BoundsRect)
      , ',w=', dbgs(Node.BoundsRect.Right - Node.BoundsRect.Left)
      , ',h=', dbgs(Node.BoundsRect.Bottom - Node.BoundsRect.Top));
    if Node.WindowState <> wsNormal then
      DbgOut(' WindowState=', dbgs(Node.WindowState));
    if Node.Monitor <> 0 then
      DbgOut(' Monitor=', dbgs(Node.Monitor));
    if Node.BoundSplitterPos <> 0 then
      DbgOut(' SplitterPos=', dbgs(Node.BoundSplitterPos));
    if (Node.WorkAreaRect.Right > 0) and (Node.WorkAreaRect.Bottom > 0) then
      DbgOut(' WorkArea=', dbgs(Node.WorkAreaRect));

    for a := low(TAnchorKind) to high(TAnchorKind) do
    begin
      if Node.Anchors[a] <> '' then
        AnchorControl := Node.Parent.FindChildNode(Node.Anchors[a], False)
      else
        AnchorControl := nil;
      if AnchorControl = nil then
        AnchorName := 'Parent'
      else
        AnchorName := AnchorControl.Name;

    end;
    for i := 0 to Node.Count - 1 do
      WriteControl(Node[i], Prefix + '  ');
  end;

var
  i: integer;
begin

  for i := 0 to RootNode.Count - 1 do
    WriteControl(RootNode[i], '  ');
end;

procedure DebugWriteChildAnchors(RootControl: TWinControl; OnlyWinControls, OnlyForms: boolean); overload;

  procedure WriteControl(AControl: TControl; Prefix: string);
  var
    i: integer;
    a: TAnchorKind;
    AnchorControl: TControl;
    AnchorName: string;
  begin
    if OnlyWinControls and (not (AControl is TWinControl)) then
      exit;
    if OnlyForms and (not (AControl is TCustomForm)) then
      exit;
    if not AControl.IsControlVisible then
      exit;

    for a := low(TAnchorKind) to high(TAnchorKind) do
    begin
      AnchorControl := AControl.AnchorSide[a].Control;
      if AnchorControl = AControl.Parent then
        AnchorName := 'Parent'
      else if AnchorControl is TCustomForm then
        AnchorName := '"' + AnchorControl.Name + '"'
      else
        AnchorName := DbgSName(AnchorControl);

    end;
    if AControl is TWinControl then
    begin
      for i := 0 to TWinControl(AControl).ControlCount - 1 do
        WriteControl(TWinControl(AControl).Controls[i], Prefix + '  ');
    end;
  end;

var
  i: integer;
begin
  for i := 0 to RootControl.ControlCount - 1 do
    WriteControl(RootControl.Controls[i], '  ');
end;

//==================== TGlassDockLayoutTreeNode ========================================

function TGlassDockLayoutTreeNode.GetNodes(Index: integer): TGlassDockLayoutTreeNode;
begin
  Result := TGlassDockLayoutTreeNode(FNodes[Index]);
end;

function TGlassDockLayoutTreeNode.GetRight: integer;
begin
  Result := FBoundsRect.Right;
end;

function TGlassDockLayoutTreeNode.GetHeight: integer;
begin
  Result := FBoundsRect.Bottom - FBoundsRect.Top;
end;

function TGlassDockLayoutTreeNode.GetBottom: integer;
begin
  Result := FBoundsRect.Bottom;
end;

function TGlassDockLayoutTreeNode.GetAnchors(Site: TAnchorKind): string;
begin
  Result := fAnchors[Site];
end;

function TGlassDockLayoutTreeNode.GetLeft: integer;
begin
  Result := FBoundsRect.Left;
end;

function TGlassDockLayoutTreeNode.GetTop: integer;
begin
  Result := FBoundsRect.Top;
end;

function TGlassDockLayoutTreeNode.GetWidth: integer;
begin
  Result := FBoundsRect.Right - FBoundsRect.Left;
end;

procedure TGlassDockLayoutTreeNode.SetAlign(const AValue: TAlign);
begin
  if FAlign = AValue then
    exit;
  FAlign := AValue;
  IncreaseChangeStamp;
end;

procedure TGlassDockLayoutTreeNode.SetAnchors(Site: TAnchorKind; const AValue: string);
begin
  if Anchors[Site] = AValue then
    exit;
  fAnchors[Site] := AValue;
  IncreaseChangeStamp;
end;

procedure TGlassDockLayoutTreeNode.SetBottom(const AValue: integer);
begin
  if GetBottom = AValue then
    exit;
  FBoundsRect.Bottom := AValue;
  IncreaseChangeStamp;
end;

procedure TGlassDockLayoutTreeNode.SetBoundSplitterPos(const AValue: integer);
begin
  if FBoundSplitterPos = AValue then
    exit;
  FBoundSplitterPos := AValue;
  IncreaseChangeStamp;
end;

procedure TGlassDockLayoutTreeNode.SetBoundsRect(const AValue: TRect);
begin
  if CompareRect(@FBoundsRect, @AValue) then
    exit;
  FBoundsRect := AValue;
  IncreaseChangeStamp;
end;

procedure TGlassDockLayoutTreeNode.SetHeaderPosition(const AValue: TADLHeaderPosition);
begin
  if FHeaderPosition = AValue then
    exit;
  FHeaderPosition := AValue;
  IncreaseChangeStamp;
end;

procedure TGlassDockLayoutTreeNode.SetHeight(const AValue: integer);
begin
  if Height = AValue then
    exit;
  FBoundsRect.Bottom := FBoundsRect.Top + AValue;
  IncreaseChangeStamp;
end;

procedure TGlassDockLayoutTreeNode.SetLeft(const AValue: integer);
begin
  if Left = AValue then
    exit;
  FBoundsRect.Left := AValue;
  IncreaseChangeStamp;
end;

procedure TGlassDockLayoutTreeNode.SetMonitor(const AValue: integer);
begin
  if FMonitor = AValue then
    exit;
  FMonitor := AValue;
  IncreaseChangeStamp;
end;

procedure TGlassDockLayoutTreeNode.SetName(const AValue: string);
begin
  if FName = AValue then
    exit;
  FName := AValue;
  IncreaseChangeStamp;
end;

procedure TGlassDockLayoutTreeNode.SetNodeType(const AValue: TADLTreeNodeType);
begin
  if FNodeType = AValue then
    exit;
  FNodeType := AValue;
  IncreaseChangeStamp;
end;

procedure TGlassDockLayoutTreeNode.SetParent(const AValue: TGlassDockLayoutTreeNode);
begin
  if FParent = AValue then
    exit;
  if FParent <> nil then
  begin
    FParent.FNodes.Remove(Self);
    FParent.IncreaseChangeStamp;
  end;
  FParent := AValue;
  if FParent <> nil then
    FParent.FNodes.Add(Self);
  IncreaseChangeStamp;
end;

procedure TGlassDockLayoutTreeNode.SetRight(const AValue: integer);
begin
  if Right = AValue then
    exit;
  FBoundsRect.Right := AValue;
  IncreaseChangeStamp;
end;

procedure TGlassDockLayoutTreeNode.SetWorkAreaRect(const AValue: TRect);
begin
  if CompareRect(@FWorkAreaRect, @AValue) then
    exit;
  FWorkAreaRect := AValue;
  IncreaseChangeStamp;
end;

procedure TGlassDockLayoutTreeNode.SetTabPosition(const AValue: TTabPosition);
begin
  if FTabPosition = AValue then
    exit;
  FTabPosition := AValue;
  IncreaseChangeStamp;
end;

procedure TGlassDockLayoutTreeNode.SetTop(const AValue: integer);
begin
  if Top = AValue then
    exit;
  FBoundsRect.Top := AValue;
  IncreaseChangeStamp;
end;

procedure TGlassDockLayoutTreeNode.SetWidth(const AValue: integer);
begin
  if Width = AValue then
    exit;
  FBoundsRect.Right := FBoundsRect.Left + AValue;
  IncreaseChangeStamp;
end;

procedure TGlassDockLayoutTreeNode.SetWindowState(const AValue: TWindowState);
begin
  if FWindowState = AValue then
    exit;
  FWindowState := AValue;
  IncreaseChangeStamp;
end;

constructor TGlassDockLayoutTreeNode.Create;
begin
  FNodes := TFPList.Create;
end;

destructor TGlassDockLayoutTreeNode.Destroy;
begin
  Parent := nil;
  Clear;
  if FNodes <> nil then
    FNodes.Free;
  FNodes := nil;
  inherited Destroy;
end;

procedure TGlassDockLayoutTreeNode.Clear;
var
  a: TAnchorKind;
begin
  Name := '';
  FillByte(FBoundsRect, sizeOf(FBoundsRect), 0);
  while Count > 0 do
    Nodes[Count - 1].Free;
  NodeType := adltnNone;
  WindowState := wsNormal;
  Monitor := 0;
  Align := alNone;
  HeaderPosition := adlhpAuto;
  TabPosition := tpTop;
  BoundSplitterPos := 0;
  WorkAreaRect := Rect(0, 0, 0, 0);
  for a := low(TAnchorKind) to high(TAnchorKind) do
    Anchors[a] := '';
end;

function TGlassDockLayoutTreeNode.IsEqual(Node: TGlassDockLayoutTreeNode): boolean;
var
  i: integer;
  a: TAnchorKind;
begin
  Result := False;
  if (not CompareRect(@FBoundsRect, @Node.FBoundsRect)) or (Count <> Node.Count) or (NodeType <> Node.NodeType) or
    (Name <> Node.Name) or (Align <> Node.Align) or (WindowState <> Node.WindowState) or (HeaderPosition <> Node.HeaderPosition) or
    (TabPosition <> Node.TabPosition) or (BoundSplitterPos <> Node.BoundSplitterPos) or (not CompareRect(@FWorkAreaRect, @Node.FWorkAreaRect)) then
    exit;
  for a := low(TAnchorKind) to high(TAnchorKind) do
    if Anchors[a] <> Node.Anchors[a] then
      exit;
  for i := 0 to Count - 1 do
    if not Nodes[i].IsEqual(Node.Nodes[i]) then
      exit;
  Result := True;
end;

procedure TGlassDockLayoutTreeNode.Assign(Node: TGlassDockLayoutTreeNode);
var
  i: integer;
  Child: TGlassDockLayoutTreeNode;
  a: TAnchorKind;
begin
  Name := Node.Name;
  NodeType := Node.NodeType;
  BoundsRect := Node.BoundsRect;
  Align := Node.Align;
  WindowState := Node.WindowState;
  HeaderPosition := Node.HeaderPosition;
  TabPosition := Node.TabPosition;
  BoundSplitterPos := Node.BoundSplitterPos;
  WorkAreaRect := Node.WorkAreaRect;
  for a := low(TAnchorKind) to high(TAnchorKind) do
    Anchors[a] := Node.Anchors[a];

  while Count > Node.Count do
    Nodes[Count - 1].Free;
  for i := 0 to Node.Count - 1 do
  begin
    if i = Count then
    begin
      Child := TGlassDockLayoutTreeNode.Create;
      Child.Parent := Self;
    end
    else
    begin
      Child := Nodes[i];
    end;
    Child.Assign(Node.Nodes[i]);
  end;
end;

procedure TGlassDockLayoutTreeNode.Assign(AControl: TControl);
var
  AnchorControl: TControl;
  a: TAnchorKind;
begin
  Name := AControl.Name;
  BoundsRect := AControl.BoundsRect;
  Align := AControl.Align;
  if (AControl.Parent = nil) and (AControl is TCustomForm) then
  begin
    WindowState := TCustomForm(AControl).WindowState;
    Monitor := TCustomForm(AControl).Monitor.MonitorNum;
    WorkAreaRect := TCustomForm(AControl).Monitor.WorkareaRect;
  end
  else
    WindowState := wsNormal;
  if AControl is TCustomTabControl then
    TabPosition := TCustomTabControl(AControl).TabPosition
  else
    TabPosition := tpTop;
  for a := low(TAnchorKind) to high(TAnchorKind) do
  begin
    AnchorControl := AControl.AnchorSide[a].Control;

    if (AnchorControl = nil) or (AnchorControl = AControl.Parent) then
      Anchors[a] := ''
    else
    if AnchorControl.Parent = AControl.Parent then
      Anchors[a] := AnchorControl.Name;
  end;
end;

procedure TGlassDockLayoutTreeNode.LoadFromConfig(Config: TConfigStorage);
var
  i: integer;
  Child: TGlassDockLayoutTreeNode;
  NewCount: longint;
begin
  Clear;
  Name := Config.GetValue('Name', '');
  NodeType := NameToADLTreeNodeType(Config.GetValue('Type', ADLTreeNodeTypeNames[adltnNone]));
  Left := Config.GetValue('Bounds/Left', 0);
  Top := Config.GetValue('Bounds/Top', 0);
  Width := Config.GetValue('Bounds/Width', 0);
  Height := Config.GetValue('Bounds/Height', 0);
  BoundSplitterPos := Config.GetValue('Bounds/SplitterPos', 0);
  Config.GetValue('Bounds/WorkArea/Rect/', FWorkAreaRect, Rect(0, 0, 0, 0));
  Anchors[akLeft] := Config.GetValue('Anchors/Left', '');
  Anchors[akTop] := Config.GetValue('Anchors/Top', '');
  Anchors[akRight] := Config.GetValue('Anchors/Right', '');
  Anchors[akBottom] := Config.GetValue('Anchors/Bottom', '');
  Align := NameToADLAlign(Config.GetValue('Anchors/Align', dbgs(alNone)));
  WindowState := NameToADLWindowState(Config.GetValue('WindowState', ADLWindowStateNames[wsNormal]));
  HeaderPosition := NameToADLHeaderPosition(Config.GetValue('Header/Position', ADLHeaderPositionNames[adlhpAuto]));
  TabPosition := NameToADLTabPosition(Config.GetValue('Header/TabPosition', ADLTabPostionNames[tpTop]));
  Monitor := Config.GetValue('Monitor', 0);
  NewCount := Config.GetValue('ChildCount', 0);
  for i := 1 to NewCount do
  begin
    Config.AppendBasePath('Item' + IntToStr(i) + '/');
    Child := TGlassDockLayoutTreeNode.Create;
    Child.Parent := Self;
    Child.LoadFromConfig(Config);
    Config.UndoAppendBasePath;
  end;
end;

procedure TGlassDockLayoutTreeNode.SaveToConfig(Config: TConfigStorage);
var
  i: integer;
begin
  Config.SetDeleteValue('Name', Name, '');
  Config.SetDeleteValue('Type', ADLTreeNodeTypeNames[NodeType],
    ADLTreeNodeTypeNames[adltnNone]);
  Config.SetDeleteValue('Bounds/Left', Left, 0);
  Config.SetDeleteValue('Bounds/Top', Top, 0);
  Config.SetDeleteValue('Bounds/Width', Width, 0);
  Config.SetDeleteValue('Bounds/Height', Height, 0);
  Config.SetDeleteValue('Bounds/SplitterPos', BoundSplitterPos, 0);
  Config.SetDeleteValue('Bounds/WorkArea/Rect/', FWorkAreaRect, Rect(0, 0, 0, 0));
  Config.SetDeleteValue('Anchors/Left', Anchors[akLeft], '');
  Config.SetDeleteValue('Anchors/Top', Anchors[akTop], '');
  Config.SetDeleteValue('Anchors/Right', Anchors[akRight], '');
  Config.SetDeleteValue('Anchors/Bottom', Anchors[akBottom], '');
  Config.SetDeleteValue('Anchors/Align', ADLAlignNames[Align], ADLAlignNames[alNone]);
  Config.SetDeleteValue('WindowState', ADLWindowStateNames[WindowState],
    ADLWindowStateNames[wsNormal]);
  Config.SetDeleteValue('Header/Position', ADLHeaderPositionNames[HeaderPosition],
    ADLHeaderPositionNames[adlhpAuto]);
  Config.SetDeleteValue('Header/TabPosition', ADLTabPostionNames[TabPosition],
    ADLTabPostionNames[tpTop]);
  Config.SetDeleteValue('Monitor', Monitor, 0);
  Config.SetDeleteValue('ChildCount', Count, 0);
  for i := 1 to Count do
  begin
    Config.AppendBasePath('Item' + IntToStr(i) + '/');
    Nodes[i - 1].SaveToConfig(Config);
    Config.UndoAppendBasePath;
  end;
end;

function TGlassDockLayoutTreeNode.FindChildNode(aName: string; Recursive: boolean): TGlassDockLayoutTreeNode;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
  begin
    Result := Nodes[i];
    if CompareText(aName, Result.Name) = 0 then
      exit;
    if Recursive then
    begin
      Result := Result.FindChildNode(aName, True);
      if Result <> nil then
        exit;
    end;
  end;
  Result := nil;
end;

function TGlassDockLayoutTreeNode.FindControlNode: TGlassDockLayoutTreeNode;
var
  i: integer;
begin
  if NodeType = adltnControl then
    Result := Self
  else
    for i := 0 to Count - 1 do
    begin
      Result := Nodes[i].FindControlNode;
      if Result <> nil then
        exit;
    end;
end;

procedure TGlassDockLayoutTreeNode.CheckConsistency;
{ ToDo: check for topological sort }

  procedure CheckCornerIsUnique(Side1: TAnchorKind; Side1AnchorName: string; Side2: TAnchorKind; Side2AnchorName: string);
  var
    i: integer;
    Child, Found: TGlassDockLayoutTreeNode;
  begin
    Found := nil;
    for i := 0 to Count - 1 do
    begin
      Child := Nodes[i];
      if Child.IsSplitter then
        continue;
      if CompareText(Child.Anchors[Side1], Side1AnchorName) <> 0 then
        continue;
      if CompareText(Child.Anchors[Side2], Side2AnchorName) <> 0 then
        continue;

      if Found <> nil then
        exit;

      Found := Child;
    end;

    if Found = nil then
      exit;
  end;

var
  i: integer;
  Child: TGlassDockLayoutTreeNode;
  Side: TAnchorKind;
  Sibling: TGlassDockLayoutTreeNode;
begin
  // check parent
  if (NodeType = adltnNone) and (Parent <> nil) then
    exit;
  if (NodeType = adltnCustomSite) and (Parent.NodeType <> adltnNone) then
    exit;
  if (Parent <> nil) and IsSplitter and (Parent.NodeType <> adltnLayout) then
    exit;

  // check sides
  for Side := low(TAnchorKind) to high(TAnchorKind) do
  begin
    if Anchors[Side] <> '' then
    begin
      // anchor must be a sibling
      Sibling := nil;
      if Parent <> nil then
        Sibling := Parent.FindChildNode(Anchors[Side], False);

      if (Sibling = nil) then exit;

      // only anchor to splitter
      if not Sibling.IsSplitter then
        exit;

      // the free sides of a splitter must not be anchored
      if ((NodeType = adltnSplitterVertical) and (Side in [akLeft, akRight])) or ((NodeType = adltnSplitterHorizontal) and
        (Side in [akTop, akBottom])) then
        exit;

      // a page must not be anchored
      if (Parent.NodeType = adltnPages) then exit;

      // check if anchored to the wrong side of a splitter
      if ((Sibling.NodeType = adltnSplitterHorizontal) and (Side in [akLeft, akRight])) or
         ((Sibling.NodeType = adltnSplitterVertical) and (Side in [akTop, akBottom])) then
        exit;

    end;
  end;

  // only the root node, pages, layouts and customsite can have children
  if (Parent <> nil) and (Count > 0) and (not (NodeType in [adltnLayout, adltnPages, adltnCustomSite])) then
    exit;

  if (NodeType = adltnCustomSite) then
  begin
    if (Count > 1) then
      exit;
  end;

  // check if in each corner sits exactly one child
  if NodeType = adltnLayout then
    for Side := low(TAnchorKind) to high(TAnchorKind) do
      CheckCornerIsUnique(Side, '', ClockwiseAnchor[Side], '');

  // check grandchild
  for i := 0 to Count - 1 do
  begin
    Child := Nodes[i];
    Child.CheckConsistency;

    if (Child.NodeType = adltnSplitterHorizontal) then
    begin
      // check if splitter corners have exactly one sibling
      CheckCornerIsUnique(akLeft, Child.Anchors[akLeft], akTop, Child.Name);
      CheckCornerIsUnique(akLeft, Child.Anchors[akLeft], akBottom, Child.Name);
      CheckCornerIsUnique(akRight, Child.Anchors[akRight], akTop, Child.Name);
      CheckCornerIsUnique(akRight, Child.Anchors[akRight], akBottom, Child.Name);
    end;
    if (Child.NodeType = adltnSplitterVertical) then
    begin
      // check if splitter corners have exactly one sibling
      CheckCornerIsUnique(akTop, Child.Anchors[akTop], akLeft, Child.Name);
      CheckCornerIsUnique(akTop, Child.Anchors[akTop], akRight, Child.Name);
      CheckCornerIsUnique(akBottom, Child.Anchors[akBottom], akLeft, Child.Name);
      CheckCornerIsUnique(akBottom, Child.Anchors[akBottom], akRight, Child.Name);
    end;
  end;
end;

procedure TGlassDockLayoutTreeNode.Simplify(ExistingNames: TStrings);
{ Simplification rules:
   1. Control nodes without existing name are deleted.
   2. Empty layouts and pages are deleted
   3. pages and layouts with only one child are removed and its content moved up
}
var
  i: integer;
  ChildNode: TGlassDockLayoutTreeNode;
begin
  // simplify children
  i := Count - 1;
  while i >= 0 do
  begin
    ChildNode := Nodes[i];
    ChildNode.Simplify(ExistingNames);

    if (ChildNode.NodeType = adltnControl) then
    begin
      // leaf node => check if there is a control
      if (ChildNode.Name = '') or (ExistingNames.IndexOf(ChildNode.Name) < 0) then
        DeleteNode(ChildNode);
    end
    else if ChildNode.IsSplitter then
    begin
      // splitter
      // delete all children
      while ChildNode.Count > 0 do
        ChildNode[0].Free;
    end
    else if ChildNode.NodeType = adltnCustomSite then
    begin
      // custom dock site
    end
    else if ChildNode.Count = 0 then
    begin
      // inner node without child => delete
      DeleteNode(ChildNode);
    end
    else if (ChildNode.Count = 1) and (ChildNode.NodeType in [adltnLayout, adltnPages]) then
    begin
      // layouts and pages with only one child
      // => move grandchildren up and delete childnode
      ReplaceWithChildren(ChildNode);
    end;

    i := Min(i, Count) - 1;
  end;
end;

procedure TGlassDockLayoutTreeNode.DeleteNode(ChildNode: TGlassDockLayoutTreeNode);
var
  i: integer;
  Sibling: TGlassDockLayoutTreeNode;
  Side: TAnchorKind;
  Splitter: TGlassDockLayoutTreeNode;
begin

  ChildNode.Parent := nil;
  try
    if not ChildNode.IsSplitter then
    begin
      // delete node bound splitter (= a splitter only anchored to this node)
      for Side := low(TAnchorKind) to high(TAnchorKind) do
      begin
        Splitter := FindNodeBoundSplitter(ChildNode, Side);
        if Splitter <> nil then
        begin
          DeleteNodeBoundSplitter(Splitter, ChildNode, OppositeAnchor[Side]);
          exit;
        end;
      end;

      // delete spiral splitter
      for Side := low(TAnchorKind) to high(TAnchorKind) do
      begin
        Splitter := FindChildNode(ChildNode.Anchors[Side], False);
        if (Splitter = nil) or (not Splitter.IsSplitter) then
          break;
        if Side = High(TAnchorKind) then
        begin
          DeleteSpiralSplitter(ChildNode);
          exit;
        end;
      end;
    end;
  finally
    // remove references
    for i := 0 to Count - 1 do
    begin
      Sibling := Nodes[i];
      for Side := low(TAnchorKind) to high(TAnchorKind) do
        if Sibling.Anchors[Side] = ChildNode.Name then
          Sibling.Anchors[Side] := '';
    end;

    // free node
    ChildNode.Free;
  end;
end;

function TGlassDockLayoutTreeNode.FindNodeBoundSplitter(ChildNode: TGlassDockLayoutTreeNode; Side: TAnchorKind): TGlassDockLayoutTreeNode;
var
  AnchorNode: TGlassDockLayoutTreeNode;
  i: integer;
  AnchorName: string;
begin
  Result := nil;
  AnchorName := ChildNode.Anchors[Side];
  if AnchorName = '' then
    exit;
  AnchorNode := FindChildNode(AnchorName, False);
  if (AnchorNode = nil) or (not AnchorNode.IsSplitter) then
    exit;
  for i := 0 to Count - 1 do
    if (Nodes[i] <> ChildNode) and (Nodes[i].Anchors[Side] = AnchorName) then
      exit;
  Result := AnchorNode;
end;

procedure TGlassDockLayoutTreeNode.DeleteNodeBoundSplitter(Splitter, ChildNode: TGlassDockLayoutTreeNode; Side: TAnchorKind);
var
  i: integer;
  Sibling: TGlassDockLayoutTreeNode;
begin
  for i := 0 to Count - 1 do
  begin
    Sibling := Nodes[i];
    if Sibling.Anchors[Side] = Splitter.Name then
      Sibling.Anchors[Side] := ChildNode.Anchors[Side];
  end;
  DeleteNode(Splitter);
end;

procedure TGlassDockLayoutTreeNode.DeleteSpiralSplitter(ChildNode: TGlassDockLayoutTreeNode);
var
  Splitters: array[TAnchorKind] of TGlassDockLayoutTreeNode;
  Side: TAnchorKind;
  i: integer;
  Sibling: TGlassDockLayoutTreeNode;
  Keep: TAnchorKind;
  DeleteSplitter: TGlassDockLayoutTreeNode;
  NextSide: TAnchorKind;
begin
  // find the four splitters
  for Side := low(TAnchorKind) to high(TAnchorKind) do
  begin
    Splitters[Side] := FindChildNode(ChildNode.Anchors[Side], False);
    if (Splitters[Side] = nil) or (not Splitters[Side].IsSplitter) then
      exit;
  end;
  for Side := low(TAnchorKind) to high(TAnchorKind) do
  begin
    // spiral splitters are connected to each other
    NextSide := ClockwiseAnchor[Side];
    if Splitters[Side].Anchors[NextSide] <> Splitters[NextSide].Name then
    begin
      NextSide := OppositeAnchor[NextSide];
      if Splitters[Side].Anchors[NextSide] <> Splitters[NextSide].Name then
        exit;
    end;
  end;
  // Prefer the pair with shortest distance between
  if (Splitters[akRight].Left - Splitters[akLeft].Left) < (Splitters[akBottom].Top - Splitters[akTop].Top) then
    Keep := akLeft
  else
    Keep := akTop;
  DeleteSplitter := Splitters[OppositeAnchor[Keep]];
  // transfer anchors from the deleting splitter to the kept splitter
  for i := 0 to Count - 1 do
  begin
    Sibling := Nodes[i];
    for Side := low(TAnchorKind) to high(TAnchorKind) do
    begin
      if FindChildNode(Sibling.Anchors[Side], False) = DeleteSplitter then
        Sibling.Anchors[Side] := Splitters[Keep].Name;
    end;
  end;
  // longen kept splitter
  NextSide := ClockwiseAnchor[Keep];
  if Splitters[Keep].Anchors[NextSide] <> Splitters[NextSide].Name then
    NextSide := OppositeAnchor[NextSide];
  Splitters[Keep].Anchors[NextSide] := DeleteSplitter.Anchors[NextSide];
  // delete the splitter
  DeleteNode(DeleteSplitter);
end;

procedure TGlassDockLayoutTreeNode.ReplaceWithChildren(ChildNode: TGlassDockLayoutTreeNode);
var
  GrandChild: TGlassDockLayoutTreeNode;
  Side: TAnchorKind;
begin
  DebugWriteChildAnchors(Self);
  while ChildNode.Count > 0 do
  begin
    GrandChild := ChildNode[0];
    GrandChild.Parent := Self;
    OffsetRect(GrandChild.FBoundsRect, ChildNode.Left, ChildNode.Top);
    for Side := low(TAnchorKind) to high(TAnchorKind) do
    begin
      if GrandChild.Anchors[Side] = '' then
      begin
        if ((GrandChild.NodeType = adltnSplitterHorizontal) and (Side in [akTop, akBottom])) or
          ((GrandChild.NodeType = adltnSplitterVertical) and (Side in [akLeft, akRight])) then
          continue; // a free splitter sides => don't anchor it
        GrandChild.Anchors[Side] := ChildNode.Anchors[Side];
      end;
    end;
  end;

  ChildNode.Free;
  DebugWriteChildAnchors(Self);
end;

procedure TGlassDockLayoutTreeNode.IncreaseChangeStamp;
begin
  if Parent <> nil then
    Parent.IncreaseChangeStamp;
end;

function TGlassDockLayoutTreeNode.IsSplitter: boolean;
begin
  Result := NodeType in [adltnSplitterHorizontal, adltnSplitterVertical];
end;

function TGlassDockLayoutTreeNode.IsRootWindow: boolean;
begin
  Result := (NodeType in [adltnLayout, adltnPages, adltnControl, adltnCustomSite]) and ((Parent = nil) or (Parent.NodeType in [adltnNone]));
end;

function TGlassDockLayoutTreeNode.Count: integer;
begin
  Result := FNodes.Count;
end;

//=================== TGlassDockLayoutTreeRootNode ========================================

procedure TGlassDockLayoutTreeRootNode.IncreaseChangeStamp;
begin
  Tree.IncreaseChangeStamp;
end;

procedure TGlassDockLayoutTreeRootNode.CheckConsistency;
var
  Names: TStringList;

  procedure RaiseNodePath(const Msg: string; Node: TGlassDockLayoutTreeNode);
  var
    s: string;
  begin
    s := '';
    while Node <> nil do
    begin
      if s <> '' then
        s := '/' + s;
      s := Node.Name + s;
      Node := Node.Parent;
    end;
    s := Msg + s;
  end;

  procedure CheckNames(Node: TGlassDockLayoutTreeNode);
  var
    i: integer;
  begin
    if (Node.Name = '') and (Node <> Self) then
      RaiseNodePath(adrsEmptyName, Node);
    for i := 0 to Names.Count - 1 do
      if CompareText(Names[i], Node.Name) = 0 then
        RaiseNodePath(adrsDuplicateName, Node);
    Names.Add(Node.Name);
    for i := 0 to Node.Count - 1 do
      CheckNames(Node[i]);
  end;

begin
  // check that all names are unique
  Names := TStringList.Create;
  try
    CheckNames(Self);
  finally
    Names.Free;
  end;
  inherited CheckConsistency;
end;

//===================== TGlassDockLayoutTree ===============================================

procedure TGlassDockLayoutTree.SetModified(const AValue: boolean);
begin
  if AValue then
    IncreaseChangeStamp
  else
    FSavedChangeStamp := FChangeStamp;
end;

function TGlassDockLayoutTree.GetModified: boolean;
begin
  Result := FSavedChangeStamp <> FChangeStamp;
end;

constructor TGlassDockLayoutTree.Create;
begin
  FSavedChangeStamp := Low(FChangeStamp);
  FRoot := TGlassDockLayoutTreeRootNode.Create;
  Root.FTree := Self;
end;

destructor TGlassDockLayoutTree.Destroy;
begin
  if FRoot <> nil then
    FRoot.Free;
  FRoot := nil;
  inherited Destroy;
end;

procedure TGlassDockLayoutTree.Clear;
begin
  FRoot.Clear;
  Modified := False;
end;

procedure TGlassDockLayoutTree.LoadFromConfig(Config: TConfigStorage);
begin
  Config.AppendBasePath('Nodes/');
  FRoot.LoadFromConfig(Config);
  Config.UndoAppendBasePath;
  Root.CheckConsistency;
end;

procedure TGlassDockLayoutTree.SaveToConfig(Config: TConfigStorage);
begin
  Config.AppendBasePath('Nodes/');
  FRoot.SaveToConfig(Config);
  Config.UndoAppendBasePath;
end;

procedure TGlassDockLayoutTree.IncreaseChangeStamp;
begin
  if FChangeStamp < High(FChangeStamp) then
    Inc(FChangeStamp)
  else
    FChangeStamp := Low(FChangeStamp);
end;

function TGlassDockLayoutTree.NewNode(aParent: TGlassDockLayoutTreeNode): TGlassDockLayoutTreeNode;
begin
  Result := TGlassDockLayoutTreeNode.Create;
  Result.Parent := aParent;
end;

procedure TGlassDockLayoutTree.Assign(Source: TObject);
begin
  if Source is TGlassDockLayoutTree then
    Root.Assign(TGlassDockLayoutTree(Source).Root)
  else if Source is TGlassDockLayoutTreeNode then
    Root.Assign(TGlassDockLayoutTreeNode(Source))
  else
    exit;
end;


//===================== TADNameToControl =======================================

function TADNameToControl.IndexOfName(const aName: string): integer;
begin
  Result := fItems.Count - 1;
  while (Result >= 0) and (CompareText(aName, fItems[Result]) <> 0) do
    Dec(Result);
end;

function TADNameToControl.GetControl(const aName: string): TControl;
var
  i: longint;
begin
  i := IndexOfName(aName);
  if i >= 0 then
    Result := TControl(fItems.Objects[i])
  else
    Result := nil;
end;

procedure TADNameToControl.SetControl(const aName: string; const AValue: TControl);
var
  i: longint;
begin
  i := IndexOfName(aName);
  if i >= 0 then
  begin
    fItems[i] := aName;
    fItems.Objects[i] := AValue;
  end
  else
    fItems.AddObject(aName, AValue);
end;

constructor TADNameToControl.Create;
begin
  fItems := TStringList.Create;
end;

destructor TADNameToControl.Destroy;
begin
  if fItems <> nil then
    fItems.Free;
  fItems := nil;
  inherited Destroy;
end;

function TADNameToControl.ControlToName(AControl: TControl): string;
var
  i: integer;
begin
  i := fItems.Count - 1;
  while i >= 0 do
  begin
    if fItems.Objects[i] = AControl then
    begin
      Result := fItems[i];
      exit;
    end;
    Dec(i);
  end;
  Result := '';
end;

procedure TADNameToControl.RemoveControl(AControl: TControl);
var
  i: integer;
begin
  i := fItems.Count - 1;
  while i >= 0 do
  begin
    if fItems.Objects[i] = AControl then
      fItems.Delete(i);
    Dec(i);
  end;
end;

procedure TADNameToControl.WriteDebugReport(Msg: string);
begin
  //........
end;

//========================= TGlassDockRestoreLayout ======================================

procedure TGlassDockRestoreLayout.SetControlNames(const AValue: TStrings);
begin
  if FControlNames = AValue then
    exit;
  FControlNames.Assign(AValue);
end;

constructor TGlassDockRestoreLayout.Create;
begin
  FControlNames := TStringList.Create;
  FLayout := TGlassDockLayoutTree.Create;
end;

constructor TGlassDockRestoreLayout.Create(aLayout: TGlassDockLayoutTree);
begin
  FControlNames := TStringList.Create;
  FLayout := aLayout;
  UpdateControlNames;
end;

destructor TGlassDockRestoreLayout.Destroy;
begin
  if FLayout <> nil then
    FLayout.Free;
  if FControlNames <> nil then
    FControlNames.Free;
  FLayout := nil;
  FControlNames := nil;
  inherited Destroy;
end;

function TGlassDockRestoreLayout.IndexOfControlName(AName: string): integer;
var
  i: integer;
begin
  Result := -9999;
  for i := 0 to fControlNames.Count - 1 do
    if SameText(AName, FControlNames[i]) then
    begin
      Result := i;
      exit;
    end;
end;

function TGlassDockRestoreLayout.HasControlName(AName: string): boolean;
begin
  Result := IndexOfControlName(AName) >= 0;
end;

procedure TGlassDockRestoreLayout.RemoveControlName(AName: string);
var
  i: integer;
begin
  for i := FControlNames.Count - 1 downto 0 do
    if CompareText(AName, FControlNames[i]) = 0 then
      FControlNames.Delete(i);
end;

procedure TGlassDockRestoreLayout.UpdateControlNames;

  procedure Check(Node: TGlassDockLayoutTreeNode);
  var
    i: integer;
  begin
    if (Node.Name <> '') and (Node.NodeType in [adltnControl, adltnCustomSite]) and (not HasControlName(Node.Name)) then
      FControlNames.Add(Node.Name);
    for i := 0 to Node.Count - 1 do
      Check(Node[i]);
  end;

begin
  FControlNames.Clear;
  Check(Layout.Root);
end;

procedure TGlassDockRestoreLayout.LoadFromConfig(Config: TConfigStorage);
var
  i: integer;
  AName: string;
  Node: TGlassDockLayoutTreeNode;
begin
  FControlNames.Delimiter := ',';
  FControlNames.StrictDelimiter := True;
  FControlNames.DelimitedText := Config.GetValue('Names', '');
  Layout.LoadFromConfig(Config);
  for i := FControlNames.Count - 1 downto 0 do
  begin
    AName := FControlNames[i];
    if (AName <> '') and IsValidIdent(AName) and (Layout.Root <> nil) then
    begin
      Node := Layout.Root.FindChildNode(AName, True);
      if (Node <> nil) and (Node.NodeType in [adltnControl, adltnCustomSite]) then
        continue;
    end;
    FControlNames.Delete(i);
  end;
end;

procedure TGlassDockRestoreLayout.SaveToConfig(Config: TConfigStorage);
begin
  FControlNames.Delimiter := ',';
  FControlNames.StrictDelimiter := True;
  Config.SetDeleteValue('Names', FControlNames.DelimitedText, '');
  Layout.SaveToConfig(Config);
end;

//================== TGlassDockRestoreLayouts =========================================

function TGlassDockRestoreLayouts.GetItems(Index: integer): TGlassDockRestoreLayout;
begin
  Result := TGlassDockRestoreLayout(fItems[Index]);
end;

constructor TGlassDockRestoreLayouts.Create;
begin
  fItems := TFPList.Create;
end;

destructor TGlassDockRestoreLayouts.Destroy;
begin
  Clear;
  if fItems <> nil then
    fItems.Free;
  fItems := nil;
  inherited Destroy;
end;

procedure TGlassDockRestoreLayouts.Clear;
var
  i: integer;
begin
  for i := 0 to fItems.Count - 1 do
    TObject(fItems[i]).Free;
  fItems.Clear;
end;

procedure TGlassDockRestoreLayouts.Delete(Index: integer);
begin
  TObject(fItems[Index]).Free;
  fItems.Delete(Index);
end;

function TGlassDockRestoreLayouts.IndexOfName(AControlName: string): integer;
begin
  Result := Count - 1;
  while (Result >= 0) and not Items[Result].HasControlName(AControlName) do
    Dec(Result);
end;

function TGlassDockRestoreLayouts.FindByName(AControlName: string): TGlassDockRestoreLayout;
var
  i: longint;
begin
  i := IndexOfName(AControlName);
  if i >= 0 then
    Result := Items[i]
  else
    Result := nil;
end;

procedure TGlassDockRestoreLayouts.Add(Layout: TGlassDockRestoreLayout; RemoveOther: boolean);
var
  i: integer;
begin
  if Layout = nil then
    exit;
  if RemoveOther then
  begin
    for i := 0 to Layout.ControlNames.Count - 1 do
      RemoveByName(Layout.ControlNames[i]);
  end;
  fItems.Add(Layout);
end;

procedure TGlassDockRestoreLayouts.RemoveByName(AControlName: string);
var
  i: integer;
  Layout: TGlassDockRestoreLayout;
begin
  for i := Count - 1 downto 0 do
  begin
    Layout := Items[i];
    Layout.RemoveControlName(AControlName);
    if Layout.ControlNames.Count = 0 then
      Delete(i);
  end;
end;

procedure TGlassDockRestoreLayouts.LoadFromConfig(Config: TConfigStorage);
var
  NewCount: longint;
  NewItem: TGlassDockRestoreLayout;
  i: integer;
begin
  Clear;
  NewCount := Config.GetValue('Count', 0);
  for i := 1 to NewCount do
  begin
    NewItem := TGlassDockRestoreLayout.Create;
    Config.AppendBasePath('Item' + IntToStr(i + 1) + '/');
    try
      NewItem.LoadFromConfig(Config);
    finally
      Config.UndoAppendBasePath;
    end;
    if NewItem.ControlNames.Count > 0 then
      fItems.Add(NewItem)
    else
      NewItem.Free;
  end;
end;

procedure TGlassDockRestoreLayouts.SaveToConfig(Config: TConfigStorage);
var
  i: integer;
begin
  Config.SetDeleteValue('Count', Count, 0);
  for i := 0 to Count - 1 do
  begin
    Config.AppendBasePath('Item' + IntToStr(i + 1) + '/');
    try
      Items[i].SaveToConfig(Config);
    finally
      Config.UndoAppendBasePath;
    end;
  end;
end;

function TGlassDockRestoreLayouts.ConfigIsEmpty(Config: TConfigStorage): boolean;
begin
  Result := Config.GetValue('Count', 0) <= 0;
end;

function TGlassDockRestoreLayouts.Count: integer;
begin
  Result := fItems.Count;
end;

end.
