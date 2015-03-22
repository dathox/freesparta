unit BGRAPalette;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AvgLvlTree, BGRABitmapTypes;

const
  MaxLastAddedColors = 10;

type
  TBGRAWeightedPaletteEntry = packed record
    Color: TBGRAPixel;
    Weight: UInt32;
  end;
  PBGRAWeightedPaletteEntry = ^TBGRAWeightedPaletteEntry;
  ArrayOfWeightedColor = array of TBGRAWeightedPaletteEntry;
  TAlphaChannelPaletteOption = (acIgnore, acTransparentEntry, acFullChannel);

  { TBGRACustomPalette }

  TBGRACustomPalette = class
  protected
    function GetCount: integer; virtual; abstract;
    function GetColorByIndex(AIndex: integer): TBGRAPixel; virtual; abstract;
  public
    function ContainsColor(AValue: TBGRAPixel): boolean; virtual; abstract;
    function IndexOfColor(AValue: TBGRAPixel): integer; virtual; abstract;
    function GetAsArrayOfColor: ArrayOfTBGRAPixel; virtual; abstract;
    function GetAsArrayOfWeightedColor: ArrayOfWeightedColor; virtual; abstract;
    property Count: integer read GetCount;
    property Color[AIndex: integer]: TBGRAPixel read GetColorByIndex;
  end;

  { TBGRAAvgLvlPalette }

  TBGRAAvgLvlPalette = class(TBGRACustomPalette)
  protected
    FTree: TAvgLvlTree;
    FArray: array of PBGRAPixel;
    FLastAddedColors: packed array[0..MaxLastAddedColors-1] of PBGRAPixel;
    FLastAddedColorCount: integer;
    function GetCount: integer; override;
    function GetColorByIndex(AIndex: integer): TBGRAPixel; override;
    function OnCompareItems({%H-}Tree: TAvgLvlTree; Data1, Data2: Pointer): integer; virtual;
    procedure FreeEntry(AEntry: PBGRAPixel); virtual; abstract;
    procedure NeedArray; virtual;
    procedure ClearArray; virtual;
    procedure AddLastColor(AColor: PBGRAPixel);
    function GetLastColor(AValue: TBGRAPixel): PBGRAPixel;
    procedure ClearLastColors;
  public
    constructor Create; overload;
    function ContainsColor(AValue: TBGRAPixel): boolean; override;
    function IndexOfColor(AValue: TBGRAPixel): integer; override;
    procedure Clear; virtual;
    destructor Destroy; override;
    function GetAsArrayOfColor: ArrayOfTBGRAPixel; override;
    function GetAsArrayOfWeightedColor: ArrayOfWeightedColor; override;
  end;

  { TBGRAPalette }

  TBGRAPalette = class(TBGRAAvgLvlPalette)
  protected
    function CreateEntry(AColor: TBGRAPixel): PBGRAPixel; virtual;
    procedure FreeEntry(AEntry: PBGRAPixel); override;
    procedure IncludePixel(PPixel: PBGRAPixel); virtual;
  public
    constructor Create(ABitmap: TBGRACustomBitmap); virtual; overload;
    function AddColor(AValue: TBGRAPixel): boolean; virtual;
    function RemoveColor(AValue: TBGRAPixel): boolean; virtual;
  end;

  { TBGRAWeightedPalette }

  TBGRAWeightedPalette = class(TBGRAPalette)
  private
  protected
    function CreateEntry(AColor: TBGRAPixel): PBGRAPixel; override;
    procedure FreeEntry(AEntry: PBGRAPixel); override;
    function GetWeightByIndex(AIndex: Integer): UInt32; virtual;
    procedure IncludePixel(PPixel: PBGRAPixel); override;
  public
    function GetAsArrayOfWeightedColor: ArrayOfWeightedColor; override;
    function IncColor(AValue: TBGRAPixel; out NewWeight: UInt32): boolean;
    function DecColor(AValue: TBGRAPixel; out NewWeight: UInt32): boolean;
    property Weight[AIndex: Integer]: UInt32 read GetWeightByIndex;
  end;

  { TBGRAReferencePalette }

  TBGRAReferencePalette = class(TBGRAAvgLvlPalette)
  protected
    procedure FreeEntry({%H-}AEntry: PBGRAPixel); override;
  public
    function AddColor(AValue: PBGRAPixel): boolean;
    function RemoveColor(AValue: PBGRAPixel): boolean;
  end;

  { TBGRACustomApproxPalette }

  TBGRACustomApproxPalette = class(TBGRACustomPalette)
  private
    procedure ApplyNearestNeighborTo(ABitmap: TBGRACustomBitmap);
    procedure ApplyFloydSteinbergTo(ABitmap: TBGRACustomBitmap);
  public
    function FindNearestColor(AValue: TBGRAPixel): TBGRAPixel; virtual; abstract;
    function FindNearestColorIndex(AValue: TBGRAPixel): integer; virtual; abstract;
    procedure ApplyTo(ABitmap: TBGRACustomBitmap; AAlgorithm: TDitheringAlgorithm);
  end;


function BGRARequiredBitDepth(ABitmap: TBGRACustomBitmap; AAlpha: TAlphaChannelPaletteOption): integer;

implementation

function BGRARequiredBitDepth(ABitmap: TBGRACustomBitmap; AAlpha: TAlphaChannelPaletteOption): integer;
var
  palette: TBGRAPalette;
  p: PBGRAPixel;
  i: NativeInt;
  transparentEntry: boolean;
begin
  palette := TBGRAPalette.Create;
  p := ABitmap.Data;
  transparentEntry := false;
  if AAlpha = acIgnore then
  begin
    for i := ABitmap.NbPixels-1 downto 0 do
    begin
      palette.AddColor(BGRA(p^.red,p^.green,p^.blue));
      inc(p);
      if palette.Count > 256 then break;
    end;
  end else
  if AAlpha = acTransparentEntry then
  begin
    for i := ABitmap.NbPixels-1 downto 0 do
    begin
      if p^.alpha < 128 then
        transparentEntry:= true
      else
        palette.AddColor(BGRA(p^.red,p^.green,p^.blue));
      inc(p);
      if palette.Count > 256 then break;
    end;
  end else
  begin
    for i := ABitmap.NbPixels-1 downto 0 do
    begin
      palette.AddColor(p^);
      inc(p);
      if palette.Count > 256 then break;
    end;
  end;

  if palette.Count+byte(transparentEntry) > 256 then
  begin
    if (AAlpha = acFullChannel) and ABitmap.HasTransparentPixels then
      result := 32
    else
    if (AAlpha = acTransparentEntry) and ABitmap.HasTransparentPixels then
      result := 25
    else
      result := 24;
  end else
  begin
    result := 8;
    while (result > 0) and (1 shl (result shr 1) >= palette.Count) do result := result shr 1;
  end;
  palette.Free;
end;

{ TBGRACustomApproxPalette }

procedure TBGRACustomApproxPalette.ApplyTo(ABitmap: TBGRACustomBitmap;
  AAlgorithm: TDitheringAlgorithm);
begin
  case AAlgorithm of
    daNearestNeighbor: ApplyNearestNeighborTo(ABitmap);
    daFloydSteinberg: ApplyFloydSteinbergTo(ABitmap);
  end;
end;

procedure TBGRACustomApproxPalette.ApplyNearestNeighborTo(
  ABitmap: TBGRACustomBitmap);
var
  p: PBGRAPixel;
  n: integer;
begin
  p := ABitmap.Data;
  for n := ABitmap.NbPixels-1 downto 0 do
  begin
    p^ := FindNearestColor(p^);
    inc(p);
  end;
end;

function AbsRGBADiff(const c1, c2: TExpandedPixel): NativeInt;
begin
  result := abs(c1.alpha-c2.alpha);
  result += abs(c1.red-c2.red);
  result += abs(c1.green-c2.green);
  result += abs(c1.blue-c2.blue);
end;

procedure TBGRACustomApproxPalette.ApplyFloydSteinbergTo(
  ABitmap: TBGRACustomBitmap);
const
  ErrorPrecisionShift = 4;
  MaxColorDiffForDiffusion = 4096;
type
  TAccPixel = record
    red,green,blue,alpha: NativeInt;
  end;
  TLine = array of TAccPixel;

  procedure AddError(var dest: TAccPixel; const src: TAccPixel; factor: NativeInt);
  const maxError = 65536 shl ErrorPrecisionShift;
    minError = -(65536 shl ErrorPrecisionShift);
  begin
    dest.alpha += src.alpha * factor;
    if dest.alpha > maxError then dest.alpha := maxError;
    if dest.alpha < minError then dest.alpha := minError;
    dest.red += src.red * factor;
    if dest.red > maxError then dest.red := maxError;
    if dest.red < minError then dest.red := minError;
    dest.green += src.green * factor;
    if dest.green > maxError then dest.green := maxError;
    if dest.green < minError then dest.green := minError;
    dest.blue += src.blue * factor;
    if dest.blue > maxError then dest.blue := maxError;
    if dest.blue < minError then dest.blue := minError;
  end;

var
  w,h: NativeInt;

var
  p,pNext: PBGRAPixel;
  orig,cur,approxExp: TExpandedPixel;
  approx: TBGRAPixel;
  curPix,diff: TAccPixel;
  i: NativeInt;
  yWrite: NativeInt;
  tempLine, currentLine, nextLine: TLine;

  function ClampWordDiv(AValue: NativeInt): Word; inline;
  begin
    if AValue < 0 then AValue := -((-AValue) shr ErrorPrecisionShift) else AValue := AValue shr ErrorPrecisionShift;
    if AValue < 0 then
      result := 0
    else if AValue > 65535 then
      result := 65535
    else
      result := AValue;
  end;

  function Div16(AValue: NativeInt): NativeInt; inline;
  begin
    if AValue < 0 then
      result := -((-AValue) shr 4)
    else
      result := AValue shr 4;
  end;

begin
  if not Assigned(ABitmap) then exit;
  w := ABitmap.Width;
  h := ABitmap.Height;
  if (w = 0) or (h = 0) then exit;
  setlength(currentLine,w);
  setlength(nextLine,w);
  for yWrite := 0 to h-1 do
  begin
    p := ABitmap.ScanLine[yWrite];
    if yWrite < h-1 then
      pNext := ABitmap.ScanLine[yWrite+1]
    else
      pNext := nil;
    if odd(yWrite) then
    begin
      inc(p, w);
      if pNext<>nil then inc(pNext, w);
      for i := w-1 downto 0 do
      begin
        dec(p);
        if pNext<>nil then dec(pNext);
        if p^.alpha <> 0 then
        begin
          orig := GammaExpansion(p^);
          with currentLine[i] do
          begin
            curPix.alpha := alpha+NativeInt(orig.alpha shl ErrorPrecisionShift);
            curPix.red := red+NativeInt(orig.red shl ErrorPrecisionShift);
            curPix.green := green+NativeInt(orig.green shl ErrorPrecisionShift);
            curPix.blue := blue+NativeInt(orig.blue shl ErrorPrecisionShift);
            cur.alpha := ClampWordDiv(curPix.alpha);
            cur.red := ClampWordDiv(curPix.red);
            cur.green := ClampWordDiv(curPix.green);
            cur.blue := ClampWordDiv(curPix.blue);
          end;
          approx := FindNearestColor(GammaCompression(cur));
          approxExp := GammaExpansion(approx);
          diff.alpha := Div16(curPix.alpha - (approxExp.alpha shl ErrorPrecisionShift));
          if (approxExp.alpha = 0) or (cur.alpha = 0) then
          begin
            diff.red := 0;
            diff.green := 0;
            diff.blue := 0;
          end else
          begin
            diff.red := Div16(curPix.red - (approxExp.red shl ErrorPrecisionShift));
            diff.green := Div16(curPix.green - (approxExp.green shl ErrorPrecisionShift));
            diff.blue := Div16(curPix.blue - (approxExp.blue shl ErrorPrecisionShift));
          end;
          if i > 0 then
          begin
            if AbsRGBADiff(GammaExpansion((p-1)^),orig) < MaxColorDiffForDiffusion then
              AddError(currentLine[i-1], diff, 7);
          end;
          if nextLine <> nil then
          begin
            if i > 0 then
            begin
              if AbsRGBADiff(GammaExpansion((pNext-1)^),orig) < MaxColorDiffForDiffusion then
                AddError(nextLine[i-1], diff, 1);
            end;
            if AbsRGBADiff(GammaExpansion(pNext^),orig) < MaxColorDiffForDiffusion then
              AddError(nextLine[i], diff, 5);
            if i < w-1 then
            begin
              if AbsRGBADiff(GammaExpansion((pNext+1)^),orig) < MaxColorDiffForDiffusion then
                AddError(nextLine[i+1], diff, 3);
            end;
          end;
          p^ := approx;
        end;
      end
    end
    else
    for i := 0 to w-1 do
    begin
      if p^.alpha <> 0 then
      begin
        orig := GammaExpansion(p^);
        with currentLine[i] do
        begin
          curPix.alpha := alpha+NativeInt(orig.alpha shl ErrorPrecisionShift);
          curPix.red := red+NativeInt(orig.red shl ErrorPrecisionShift);
          curPix.green := green+NativeInt(orig.green shl ErrorPrecisionShift);
          curPix.blue := blue+NativeInt(orig.blue shl ErrorPrecisionShift);
          cur.alpha := ClampWordDiv(curPix.alpha);
          cur.red := ClampWordDiv(curPix.red);
          cur.green := ClampWordDiv(curPix.green);
          cur.blue := ClampWordDiv(curPix.blue);
        end;
        approx := FindNearestColor(GammaCompression(cur));
        approxExp := GammaExpansion(approx);
        diff.alpha := Div16(curPix.alpha - (approxExp.alpha shl ErrorPrecisionShift));
        if (approxExp.alpha = 0) or (cur.alpha = 0) then
        begin
          diff.red := 0;
          diff.green := 0;
          diff.blue := 0;
        end else
        begin
          diff.red := Div16(curPix.red - (approxExp.red shl ErrorPrecisionShift));
          diff.green := Div16(curPix.green - (approxExp.green shl ErrorPrecisionShift));
          diff.blue := Div16(curPix.blue - (approxExp.blue shl ErrorPrecisionShift));
        end;
        if i < w-1 then
        begin
          if AbsRGBADiff(GammaExpansion((p+1)^),orig) < MaxColorDiffForDiffusion then
            AddError(currentLine[i+1], diff, 7);
        end;
        if nextLine <> nil then
        begin
          if i > 0 then
          begin
            if AbsRGBADiff(GammaExpansion((pNext-1)^),orig) < MaxColorDiffForDiffusion then
              AddError(nextLine[i-1], diff, 3);
          end;
          if AbsRGBADiff(GammaExpansion(pNext^),orig) < MaxColorDiffForDiffusion then
            AddError(nextLine[i], diff, 5);
          if i < w-1 then
          begin
            if AbsRGBADiff(GammaExpansion((pNext+1)^),orig) < MaxColorDiffForDiffusion then
              AddError(nextLine[i+1], diff, 1);
          end;
        end;
        p^ := approx;
      end;
      inc(p);
      if pNext<>nil then inc(pNext);
    end;
    tempLine := currentLine;
    currentLine := nextLine;
    nextLine := tempLine;
    if yWrite = h-2 then
      nextLine := nil
    else
      for i := 0 to w-1 do
      begin
        nextLine[i].red := 0;
        nextLine[i].green := 0;
        nextLine[i].blue := 0;
        nextLine[i].alpha := 0;
      end;
  end;
end;

{ TBGRAWeightedPalette }

function TBGRAWeightedPalette.GetWeightByIndex(AIndex: integer): UInt32;
begin
  NeedArray;
  if (AIndex >= 0) and (AIndex < length(FArray)) then
    result := PBGRAWeightedPaletteEntry(FArray[AIndex])^.Weight
  else
    raise ERangeError.Create('Index out of bounds');
end;

procedure TBGRAWeightedPalette.IncludePixel(PPixel: PBGRAPixel);
var dummy: UInt32;
begin
  IncColor(PPixel^,dummy);
end;

function TBGRAWeightedPalette.GetAsArrayOfWeightedColor: ArrayOfWeightedColor;
var
  i: NativeInt;
begin
  NeedArray;
  setlength(result, length(FArray));
  for i := 0 to high(result) do
    result[i] := PBGRAWeightedPaletteEntry(FArray[i])^;
end;

function TBGRAWeightedPalette.CreateEntry(AColor: TBGRAPixel): PBGRAPixel;
begin
  result := PBGRAPixel(GetMem(sizeOf(TBGRAWeightedPaletteEntry)));
  result^ := AColor;
  PBGRAWeightedPaletteEntry(result)^.Weight := 1;
end;

procedure TBGRAWeightedPalette.FreeEntry(AEntry: PBGRAPixel);
begin
  FreeMem(AEntry);
end;

function TBGRAWeightedPalette.IncColor(AValue: TBGRAPixel; out NewWeight: UInt32
  ): boolean;
Var Node: TAvgLvlTreeNode;
  Entry: PBGRAPixel;
begin
  Entry := GetLastColor(AValue);
  if Entry <> nil then
  begin
    NewWeight := PBGRAWeightedPaletteEntry(Entry)^.Weight+1;
    PBGRAWeightedPaletteEntry(Entry)^.Weight := NewWeight;
    result := false;
    exit;
  end;
  Node := FTree.Find(@AValue);
  if Assigned(Node) then
  begin
    Entry := PBGRAPixel(Node.Data);
    NewWeight := PBGRAWeightedPaletteEntry(Entry)^.Weight+1;
    PBGRAWeightedPaletteEntry(Entry)^.Weight := NewWeight;
    AddLastColor(Entry);
    result := false;
  end
  else
  begin
    Entry := CreateEntry(AValue);
    FTree.Add(Entry);
    ClearArray;
    NewWeight := PBGRAWeightedPaletteEntry(Entry)^.Weight;
    AddLastColor(Entry);
    result := true;
  end;
end;

function TBGRAWeightedPalette.DecColor(AValue: TBGRAPixel; out NewWeight: UInt32
  ): boolean;
var
  Node : TAvgLvlTreeNode;
  Entry: PBGRAPixel;
begin
  Node := FTree.Find(@AValue);
  if Assigned(Node) then
  begin
    Entry := PBGRAPixel(Node.Data);
    NewWeight := PBGRAWeightedPaletteEntry(Entry)^.Weight;
    if NewWeight > 2 then
    begin
      dec(NewWeight);
      PBGRAWeightedPaletteEntry(Entry)^.Weight := NewWeight;
    end
    else
    begin
      NewWeight := 0;
      FTree.Delete(Node);
      ClearArray;
      ClearLastColors;
    end;
    result := true;
  end else
  begin
    result := false;
    NewWeight := 0;
  end;
end;

{ TBGRAReferencePalette }

procedure TBGRAReferencePalette.FreeEntry(AEntry: PBGRAPixel);
begin
  //nothing
end;

function TBGRAReferencePalette.AddColor(AValue: PBGRAPixel): boolean;
begin
  if Assigned(GetLastColor(AValue^)) then
  begin
    result := false;
    exit;
  end;
  AddLastColor(AValue);
  if Assigned(FTree.Find(AValue)) then
  begin
    result := false;
  end
  else
  begin
    result := true;
    FTree.Add(AValue);
    ClearArray;
  end;
end;

function TBGRAReferencePalette.RemoveColor(AValue: PBGRAPixel): boolean;
var
  Node : TAvgLvlTreeNode;
begin
  Node := FTree.Find(AValue);
  if Assigned(Node) then
  begin
    FTree.Delete(Node);
    result := true;
    ClearArray;
    ClearLastColors;
  end else
    result := false;
end;

{ TBGRAAvgLvlPalette }

constructor TBGRAAvgLvlPalette.Create;
begin
  FTree := TAvgLvlTree.Create;
  FTree.OnObjectCompare := @OnCompareItems;
end;

destructor TBGRAAvgLvlPalette.Destroy;
begin
  Clear;
  FreeAndNil(FTree);
  inherited Destroy;
end;

function TBGRAAvgLvlPalette.GetAsArrayOfColor: ArrayOfTBGRAPixel;
var i: NativeInt;
begin
  NeedArray;
  setlength(result, Length(FArray));
  for i := 0 to high(result) do
    result[i] := FArray[i]^;
end;

function TBGRAAvgLvlPalette.GetAsArrayOfWeightedColor: ArrayOfWeightedColor;
var i: NativeInt;
begin
  NeedArray;
  setlength(result, Length(FArray));
  for i := 0 to high(result) do
  with result[i] do
  begin
    Color := FArray[i]^;
    Weight:= 1;
  end;
end;

procedure TBGRAAvgLvlPalette.Clear;
var Node: TAvgLvlTreeNode;
begin
  For Node in FTree do
    FreeEntry(PBGRAPixel(Node.Data));
  FTree.Clear;
  ClearArray;
  FLastAddedColorCount := 0;
end;

function TBGRAAvgLvlPalette.GetCount: integer;
begin
  result := FTree.Count;
end;

function TBGRAAvgLvlPalette.ContainsColor(AValue: TBGRAPixel): boolean;
Var Node: TAvgLvlTreeNode;
begin
  if Assigned(GetLastColor(AValue)) then
  begin
    result := true;
    exit;
  end;
  Node := FTree.Find(@AValue);
  result := Assigned(Node);
  if result then AddLastColor(PBGRAPixel(Node.Data));
end;

function TBGRAAvgLvlPalette.IndexOfColor(AValue: TBGRAPixel): integer;
Var Node: TAvgLvlTreeNode;
begin
  Node := FTree.Find(@AValue);
  if Assigned(Node) then
  begin
    result := 0;
    Node := Node.Precessor;
    while Assigned(Node) do
    begin
      inc(result);
      Node := Node.Precessor;
    end;
  end else
    result := -1;
end;

function TBGRAAvgLvlPalette.GetColorByIndex(AIndex: integer): TBGRAPixel;
begin
  NeedArray;
  if (AIndex >= 0) and (AIndex < length(FArray)) then
    result := FArray[AIndex]^
  else
    raise ERangeError.Create('Index out of bounds');
end;

function TBGRAAvgLvlPalette.OnCompareItems(Tree: TAvgLvlTree; Data1, Data2: Pointer): integer;
var gray1, gray2: NativeUInt;
  c1, c2: TBGRAPixel;
begin
  c1 := PBGRAPixel(Data1)^;
  c2 := PBGRAPixel(Data2)^;
  if c1.alpha < c2.alpha then
    result := -1
  else if c1.alpha > c2.alpha then
    result := 1
  else
  begin
    gray1 := (GammaExpansionTab[c1.red] shl 8)+(GammaExpansionTab[c1.green] shl 9)+(GammaExpansionTab[c1.blue] shl 7);
    gray2 := (GammaExpansionTab[c2.red] shl 8)+(GammaExpansionTab[c2.green] shl 9)+(GammaExpansionTab[c2.blue] shl 7);
    if gray1<gray2 then
      result := -1
    else if gray1>gray2 then
      result := 1
    else
    begin
      if c1.green > c2.green then
        result := 1
      else if c1.green < c2.green then
        result := -1
      else if c1.red > c2.red then
        result := 1
      else if c1.red < c2.red then
        result := -1
      else if c1.blue > c2.blue then
        result := 1
      else if c1.blue < c2.blue then
        result := -1
      else
        result := 0;
    end;
  end;
end;

procedure TBGRAAvgLvlPalette.NeedArray;
var Node: TAvgLvlTreeNode;
  i,n: integer;
begin
  n := Count;
  if length(FArray) <> n then
  begin
    setLength(FArray,n);
    i := 0;
    for Node in FTree do
    begin
      if i >= n then break;
      FArray[i] := PBGRAPixel(Node.Data);
      inc(i);
    end;
  end;
end;

procedure TBGRAAvgLvlPalette.ClearArray;
begin
  FArray := nil;
end;

procedure TBGRAAvgLvlPalette.AddLastColor(AColor: PBGRAPixel);
begin
  if FLastAddedColorCount < MaxLastAddedColors then
  begin
    FLastAddedColors[FLastAddedColorCount] := AColor;
    inc(FLastAddedColorCount);
  end else
  begin
    move(FLastAddedColors[1],FLastAddedColors[0],(FLastAddedColorCount-1)*sizeof(PBGRAPixel));
    FLastAddedColors[FLastAddedColorCount-1] := AColor;
  end;
end;

function TBGRAAvgLvlPalette.GetLastColor(AValue: TBGRAPixel): PBGRAPixel;
var
  i: NativeInt;
begin
  for i := FLastAddedColorCount-1 downto 0 do
    if PDWord(FLastAddedColors[i])^ = DWord(AValue) then
    begin
      result := FLastAddedColors[i];
      exit;
    end;
  result := nil;
end;

procedure TBGRAAvgLvlPalette.ClearLastColors;
begin
  FLastAddedColorCount := 0;
end;

{ TBGRAPalette }

function TBGRAPalette.CreateEntry(AColor: TBGRAPixel): PBGRAPixel;
begin
  result := PBGRAPixel(GetMem(sizeOf(TBGRAPixel)));
  result^ := AColor;
end;

procedure TBGRAPalette.FreeEntry(AEntry: PBGRAPixel);
begin
  FreeMem(AEntry);
end;

procedure TBGRAPalette.IncludePixel(PPixel: PBGRAPixel);
begin
  AddColor(PPixel^);
end;

constructor TBGRAPalette.Create(ABitmap: TBGRACustomBitmap);
var p: PBGRAPixel;
  n: integer;
begin
  inherited Create;
  n:= ABitmap.NbPixels;
  p := ABitmap.Data;
  while n > 0 do
  begin
    IncludePixel(p);
    inc(p);
    dec(n);
  end;
end;

function TBGRAPalette.AddColor(AValue: TBGRAPixel): boolean;
Var Node: TAvgLvlTreeNode;
  Entry: PBGRAPixel;
begin
  if Assigned(GetLastColor(AValue)) then
  begin
    result := false;
    exit;
  end;
  Node := FTree.Find(@AValue);
  if Assigned(Node) then
  begin
    AddLastColor(PBGRAPixel(Node.Data));
    result := false;
  end
  else
  begin
    result := true;
    Entry := CreateEntry(AValue);
    FTree.Add(Entry);
    ClearArray;
    AddLastColor(Entry);
  end;
end;

function TBGRAPalette.RemoveColor(AValue: TBGRAPixel): boolean;
var
  Node : TAvgLvlTreeNode;
begin
  Node := FTree.Find(@AValue);
  if Assigned(Node) then
  begin
    FTree.Delete(Node);
    result := true;
    ClearArray;
    ClearLastColors;
  end else
    result := false;
end;

end.

