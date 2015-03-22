{
This unit is deprecated use BGRASliceScaling in the BGRABitmap package instead.
}

unit bgrabitmapthemeutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, Graphics,
  bgrabitmap, bgrabitmaptypes;

type
  TBGRABitmapArray = array of TBGRABitmap;
  TBGRABitmapArrays = array of TBGRABitmapArray;

{ TBGRABitmap functions }

function GetBGRABitmap(ABitmap: TBGRABitmap; Number: integer): TBGRABitmapArray;
function GetBGRABitmap(AFilename: string; Number: integer): TBGRABitmapArray;
function GetBGRABitmap(AStream: TStream; Number: integer): TBGRABitmapArray;

function GetBGRABitmapPart(Source: TBGRABitmap;
  BorderWidth, BorderHeight: integer): TBGRABitmapArray;

function GetBGRABitmapElements(ABitmap: TBGRABitmap;
  Number, BorderWidth, BorderHeight: integer): TBGRABitmapArrays;
function GetBGRABitmapElements(AFilename: string;
  Number, BorderWidth, BorderHeight: integer): TBGRABitmapArrays;
function GetBGRABitmapElements(AStream: TStream;
  Number, BorderWidth, BorderHeight: integer): TBGRABitmapArrays;

function DrawBGRABitmap(Source: TBGRABitmapArray;
  DestWidth, DestHeight, BorderWidth, BorderHeight: integer;
  FillLeft: boolean = False; FillTop: boolean = False; FillRight: boolean = False;
  FillBottom: boolean = False; FillCenter: boolean = False;
  DrawMode: TDrawMode = dmDrawWithTransparency;
  ResampleMode: TResampleMode = rmSimpleStretch;
  ResampleFilter: TResampleFilter = rfBestQuality): TBGRABitmap;

type

  { TBGRABitmapThemeUtil }

  TBGRABitmapThemeUtil = class
  private
    BorWidth: integer;
    BorHeight: integer;
  public
    BGRAbmpArrays: TBGRABitmapArrays;
    constructor Create(ABitmap: TBGRABitmap; Number, BorderWidth, BorderHeight: integer);
    constructor Create(ABitmap: TBitmap; Number, BorderWidth, BorderHeight: integer);
    constructor Create(AFilename: string; Number, BorderWidth, BorderHeight: integer);
    constructor Create(AStream: TStream; Number, BorderWidth, BorderHeight: integer);
    destructor Destroy; override;
    function Draw(AWidth, AHeight: integer; Number: integer;
      FillLeft: boolean = False; FillTop: boolean = False;
      FillRight: boolean = False; FillBottom: boolean = False;
      FillCenter: boolean = False; DrawMode: TDrawMode = dmDrawWithTransparency;
      ResampleMode: TResampleMode = rmSimpleStretch;
      ResampleFilter: TResampleFilter = rfBestQuality): TBGRABitmap;
    procedure Draw(ACanvas: TCanvas; X, Y, W, H, Number: integer;
      FillLeft: boolean = False; FillTop: boolean = False;
      FillRight: boolean = False; FillBottom: boolean = False;
      FillCenter: boolean = False; DrawMode: TDrawMode = dmDrawWithTransparency;
      ResampleMode: TResampleMode = rmSimpleStretch;
      ResampleFilter: TResampleFilter = rfBestQuality);
    procedure Draw(ABitmap: TBGRABitmap; X, Y, W, H, Number: integer;
      FillLeft: boolean = False; FillTop: boolean = False;
      FillRight: boolean = False; FillBottom: boolean = False;
      FillCenter: boolean = False; DrawMode: TDrawMode = dmDrawWithTransparency;
      ResampleMode: TResampleMode = rmSimpleStretch;
      ResampleFilter: TResampleFilter = rfBestQuality);
  end;

implementation

{ TBGRABitmap functions }

function GetBGRABitmap(ABitmap: TBGRABitmap; Number: integer): TBGRABitmapArray;
var
  i, s: integer;
begin
  s := ABitmap.Height div Number;
  SetLength(Result, Number);
  for i := Low(Result) to High(Result) do
    Result[i] := TBGRABitmap(ABitmap.GetPtrBitmap(s * i, s * (i + 1)));
end;

function GetBGRABitmap(AFilename: string; Number: integer): TBGRABitmapArray;
var
  temp: TBGRABitmap;
begin
  temp := TBGRABitmap.Create(AFilename);
  Result := GetBGRABitmap(temp, Number);
  temp.Free;
end;

function GetBGRABitmap(AStream: TStream; Number: integer): TBGRABitmapArray;
var
  temp: TBGRABitmap;
begin
  temp := TBGRABitmap.Create(AStream);
  Result := GetBGRABitmap(temp, Number);
  temp.Free;
end;

function GetBGRABitmapPart(Source: TBGRABitmap;
  BorderWidth, BorderHeight: integer): TBGRABitmapArray;
var
  bmpArray: TBGRABitmapArray;
begin
  SetLength(bmpArray, 9);
  Result := bmpArray;

  { Top Left }
  bmpArray[0] := TBGRABitmap.Create(BorderWidth, BorderHeight);
  bmpArray[0].PutImage(0, 0, Source, dmDrawWithTransparency);

  { Top Right }
  bmpArray[1] := TBGRABitmap.Create(BorderWidth, BorderHeight);
  bmpArray[1].PutImage(BorderWidth - Source.Width, 0, Source, dmDrawWithTransparency);

  { Bottom Left }
  bmpArray[2] := TBGRABitmap.Create(BorderWidth, BorderHeight);
  bmpArray[2].PutImage(0, BorderHeight - Source.Height, Source, dmDrawWithTransparency);

  { Bottom Right }
  bmpArray[3] := TBGRABitmap.Create(BorderWidth, BorderHeight);
  bmpArray[3].PutImage(BorderWidth - Source.Width, BorderHeight -
    Source.Height, Source, dmDrawWithTransparency);

  { Center }
  bmpArray[4] := TBGRABitmap.Create(Source.Width - BorderWidth * 2,
    Source.Height - BorderHeight * 2);
  bmpArray[4].PutImage(-BorderWidth, -BorderHeight, Source, dmDrawWithTransparency);

  { Top }
  bmpArray[5] := TBGRABitmap.Create(Source.Width - BorderWidth * 2, BorderHeight);
  bmpArray[5].PutImage(-BorderWidth, 0, Source, dmDrawWithTransparency);

  { Left }
  bmpArray[6] := TBGRABitmap.Create(BorderWidth, Source.Height - BorderHeight * 2);
  bmpArray[6].PutImage(0, -BorderHeight, Source, dmDrawWithTransparency);

  { Right }
  bmpArray[7] := TBGRABitmap.Create(BorderWidth, Source.Height - BorderHeight * 2);
  bmpArray[7].PutImage(BorderWidth - Source.Width, -BorderHeight,
    Source, dmDrawWithTransparency);

  { Bottom }
  bmpArray[8] := TBGRABitmap.Create(Source.Width - BorderWidth * 2, BorderHeight);
  bmpArray[8].PutImage(-BorderWidth, BorderHeight - Source.Height,
    Source, dmDrawWithTransparency);
end;

function GetBGRABitmapElements(ABitmap: TBGRABitmap;
  Number, BorderWidth, BorderHeight: integer): TBGRABitmapArrays;
var
  bmpArrayStates: TBGRABitmapArray;
  i, tempWidth, tempHeight: integer;
begin
  bmpArrayStates := GetBGRABitmap(ABitmap, Number);

  if (BorderWidth * 2 > bmpArrayStates[0].Width) or (BorderWidth < 0) then
    tempWidth := Trunc(bmpArrayStates[0].Width div 2)
  else
    tempWidth := BorderWidth;

  if (BorderHeight * 2 > bmpArrayStates[0].Height) or (BorderHeight < 0) then
    tempHeight := Trunc(bmpArrayStates[0].Height div 2)
  else
    tempHeight := BorderHeight;

  SetLength(Result, Number, 9);
  for i := Low(bmpArrayStates) to High(bmpArrayStates) do
  begin
    Result[i] := GetBGRABitmapPart(bmpArrayStates[i], tempWidth, tempHeight);
    bmpArrayStates[i].Free;
    bmpArrayStates[i] := nil;
  end;

  bmpArrayStates := nil;
end;

function GetBGRABitmapElements(AFilename: string;
  Number, BorderWidth, BorderHeight: integer): TBGRABitmapArrays;
var
  temp: TBGRABitmap;
begin
  temp := TBGRABitmap.Create(AFilename);
  Result := GetBGRABitmapElements(temp, Number, BorderWidth, BorderHeight);
  temp.Free;
end;

function GetBGRABitmapElements(AStream: TStream;
  Number, BorderWidth, BorderHeight: integer): TBGRABitmapArrays;
var
  temp: TBGRABitmap;
begin
  temp := TBGRABitmap.Create(AStream);
  Result := GetBGRABitmapElements(temp, Number, BorderWidth, BorderHeight);
  temp.Free;
end;

function DrawBGRABitmap(Source: TBGRABitmapArray;
  DestWidth, DestHeight, BorderWidth, BorderHeight: integer;
  FillLeft: boolean = False; FillTop: boolean = False; FillRight: boolean = False;
  FillBottom: boolean = False; FillCenter: boolean = False;
  DrawMode: TDrawMode = dmDrawWithTransparency;
  ResampleMode: TResampleMode = rmSimpleStretch;
  ResampleFilter: TResampleFilter = rfBestQuality): TBGRABitmap;

  procedure StretchAndDraw(Source, Dest: TBGRABitmap; x, y, w, h: integer;
    Fill: boolean = False);
  var
    temp: TBGRABitmap;
  begin
    if Fill then
    begin
      temp := TBGRABitmap.Create(w, h);
      temp.ResampleFilter := ResampleFilter;
      temp.Fill(Source);
    end
    else
    begin
      temp := TBGRABitmap.Create(0, 0);
      temp.ResampleFilter := ResampleFilter;
      BGRAReplace(temp, Source.Resample(w, h, ResampleMode));
    end;
    Dest.PutImage(x, y, temp, DrawMode);
    temp.Free;
  end;

  procedure DrawEachPart(Source: TBGRABitmapArray; dest: TBGRABitmap;
    DestWidth, DestHeight, BorderWidth, BorderHeight: integer);
  begin
    //center
    if (DestWidth > BorderWidth * 2) and (DestHeight > BorderHeight * 2) then
      StretchAndDraw(Source[4], dest, BorderWidth, BorderHeight,
        DestWidth - 2 * BorderWidth, DestHeight - 2 * BorderHeight, FillCenter);
    //top
    StretchAndDraw(Source[5], dest, BorderWidth, 0, DestWidth - 2 *
      BorderWidth, BorderHeight, FillTop);
    //left
    StretchAndDraw(Source[6], dest, 0, BorderHeight, BorderWidth,
      DestHeight - 2 * BorderHeight, FillLeft);
    //right
    StretchAndDraw(Source[7], dest, DestWidth - BorderWidth, BorderHeight,
      BorderWidth, DestHeight - 2 * BorderHeight, FillRight);
    //bottom
    StretchAndDraw(Source[8], dest, BorderWidth, DestHeight - BorderHeight,
      DestWidth - 2 * BorderWidth, BorderHeight, FillBottom);
    //top left
    StretchAndDraw(Source[0], dest, 0, 0, BorderWidth, BorderHeight);
    //top right
    StretchAndDraw(Source[1], dest, DestWidth - BorderWidth, 0,
      BorderWidth, BorderHeight);
    //bottom left
    StretchAndDraw(Source[2], dest, 0, DestHeight - BorderHeight,
      BorderWidth, BorderHeight);
    //bottom right
    StretchAndDraw(Source[3], dest, DestWidth - BorderWidth, DestHeight -
      BorderHeight, BorderWidth, BorderHeight);
  end;

var
  temp: TBGRABitmap;
  tempWidth, tempHeight: integer;
begin
  if (BorderWidth < 1) or (BorderHeight < 1) then
  begin
    Result := TBGRABitmap.Create(DestWidth, DestHeight);
    StretchAndDraw(Source[4], Result, 0, 0, DestWidth, DestHeight);
    exit;
  end;

  if DestWidth < BorderWidth * 2 then
    tempWidth := BorderWidth * 2
  else
    tempWidth := DestWidth;

  if DestHeight < BorderHeight * 2 then
    tempHeight := BorderHeight * 2
  else
    tempHeight := DestHeight;

  temp := TBGRABitmap.Create(tempWidth, tempHeight);
  DrawEachPart(Source, temp, tempWidth, tempHeight, BorderWidth, BorderHeight);

  if (tempWidth <> DestWidth) or (tempHeight <> DestHeight) then
  begin
    Result := TBGRABitmap.Create(DestWidth, DestHeight);
    StretchAndDraw(temp, Result, 0, 0, DestWidth, DestHeight);
    temp.Free;
  end
  else
    Result := temp;
end;

{ TBGRABitmapThemeUtil }

constructor TBGRABitmapThemeUtil.Create(ABitmap: TBGRABitmap;
  Number, BorderWidth, BorderHeight: integer);
begin
  BGRABmpArrays := GetBGRABitmapElements(ABitmap, Number, BorderWidth, BorderHeight);
  BorWidth := BGRABmpArrays[0, 0].Width;
  BorHeight := BGRABmpArrays[0, 0].Height;
  inherited Create;
end;

constructor TBGRABitmapThemeUtil.Create(ABitmap: TBitmap;
  Number, BorderWidth, BorderHeight: integer);
var
  temp: TBGRABitmap;
begin
  temp := TBGRABitmap.Create(ABitmap);
  BGRABmpArrays := GetBGRABitmapElements(temp, Number, BorderWidth, BorderHeight);
  temp.Free;
  BorWidth := BGRABmpArrays[0, 0].Width;
  BorHeight := BGRABmpArrays[0, 0].Height;
  inherited Create;
end;

constructor TBGRABitmapThemeUtil.Create(AFilename: string;
  Number, BorderWidth, BorderHeight: integer);
begin
  BGRABmpArrays := GetBGRABitmapElements(AFilename, Number, BorderWidth, BorderHeight);
  BorWidth := BGRABmpArrays[0, 0].Width;
  BorHeight := BGRABmpArrays[0, 0].Height;
  inherited Create;
end;

constructor TBGRABitmapThemeUtil.Create(AStream: TStream;
  Number, BorderWidth, BorderHeight: integer);
begin
  BGRABmpArrays := GetBGRABitmapElements(AStream, Number, BorderWidth, BorderHeight);
  BorWidth := BGRABmpArrays[0, 0].Width;
  BorHeight := BGRABmpArrays[0, 0].Height;
  inherited Create;
end;

destructor TBGRABitmapThemeUtil.Destroy;
var
  i, j: integer;
begin
  for i := Low(BGRAbmpArrays) to High(BGRABMPArrays) do
  begin
    for  j := Low(BGRABMPArrays[i]) to High(BGRABMPArrays[i]) do
    begin
      BGRABMPArrays[i, j].Free;
      BGRABMPArrays[i, j] := nil;
    end;
    BGRABMPArrays[i] := nil;
  end;
  inherited Destroy;
end;

function TBGRABitmapThemeUtil.Draw(AWidth, AHeight: integer; Number: integer;
  FillLeft: boolean = False; FillTop: boolean = False; FillRight: boolean = False;
  FillBottom: boolean = False; FillCenter: boolean = False;
  DrawMode: TDrawMode = dmDrawWithTransparency;
  ResampleMode: TResampleMode = rmSimpleStretch;
  ResampleFilter: TResampleFilter = rfBestQuality): TBGRABitmap;
begin
  Result := DrawBGRABitmap(BGRAbmpArrays[Number], AWidth, AHeight,
    BorWidth, BorHeight, FillLeft, FillTop, FillRight, FillBottom,
    FillCenter, DrawMode, ResampleMode, ResampleFilter);
end;

procedure TBGRABitmapThemeUtil.Draw(ACanvas: TCanvas; X, Y, W, H, Number: integer;
  FillLeft: boolean = False; FillTop: boolean = False; FillRight: boolean = False;
  FillBottom: boolean = False; FillCenter: boolean = False;
  DrawMode: TDrawMode = dmDrawWithTransparency;
  ResampleMode: TResampleMode = rmSimpleStretch;
  ResampleFilter: TResampleFilter = rfBestQuality);
var
  temp: TBGRABitmap;
begin
  temp := DrawBGRABitmap(BGRAbmpArrays[Number], W, H, BorWidth,
    BorHeight, FillLeft, FillTop, FillRight, FillBottom, FillCenter,
    DrawMode, ResampleMode, ResampleFilter);
  temp.Draw(ACanvas, X, Y, False);
  temp.Free;
end;

procedure TBGRABitmapThemeUtil.Draw(ABitmap: TBGRABitmap;
  X, Y, W, H, Number: integer; FillLeft: boolean = False; FillTop: boolean = False;
  FillRight: boolean = False; FillBottom: boolean = False;
  FillCenter: boolean = False; DrawMode: TDrawMode = dmDrawWithTransparency;
  ResampleMode: TResampleMode = rmSimpleStretch;
  ResampleFilter: TResampleFilter = rfBestQuality);
var
  temp: TBGRABitmap;
begin
  temp := DrawBGRABitmap(BGRAbmpArrays[Number], W, H, BorWidth,
    BorHeight, FillLeft, FillTop, FillRight, FillBottom, FillCenter,
    DrawMode, ResampleMode, ResampleFilter);
  ABitmap.PutImage(X, Y, temp, DrawMode);
  temp.Free;
end;

end.

