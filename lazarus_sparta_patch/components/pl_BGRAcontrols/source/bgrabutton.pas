{ !! THIS CONTROL IS DEPRECATED! USE BCBUTTON INSTEAD !!
  Customizable component which using BGRABitmap for drawing. Functionality:
  - Gradients
  - Double gradients
  - Rounding
  - Drop down list
  - Glyph
  - States (normal, hover, clicked)
  - Caption with shadow
  - Full alpha and antialias support

  Copyright (C) 2011 Krzysztof Dibowski dibowski at interia.pl

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}
unit BGRAButton;

{$mode objfpc}{$H+}

interface

uses
  Classes, LResources, Controls, Dialogs, BGRABitmap, BGRABitmapTypes,
  Buttons, Graphics, BGRAGradientScanner, LCLType, types, BCTypes,
  LMessages, Forms;

{off $DEFINE DEBUG}

type

  TBGRABorderStyle = (bsRound, bsBevel, bsSquare);
  TBGRAButtBodyStyle = (bbsClear, bbsColor, bbsGradient);
  TBGRAButtBorderStyle = (bboNone, bboSolid);
  TBGRAButtStyle = (bbtButton, bbtDropDown);
  TCustomBGRAButtonState = (
    bstPrepareNormal,
    bstPrepareHover,
    bstPrepareClick,
    // Drop down button
    bstPrepareNormalA,
    bstPrepareHoverA,
    bstPrepareClickA
    );
  TCustomBGRAButtonStates = set of TCustomBGRAButtonState;
  TOnAfterPrepareBGRAButton = procedure(Sender: TObject; const ABGRA: TBGRABitmap;
    ARect: TRect; AState: TCustomBGRAButtonState) of object;

  { TBody }

  TBody = class(TPersistent)
  private
    FBorderColor: TColor;
    FBorderColorOpacity: byte;
    FBorderStyle: TBGRAButtBorderStyle;
    FColor: TColor;
    FColorOpacity: byte;
    FFont: TFont;
    FGradient1: TBCGradient;
    FGradient1EndPercent: single;
    FGradient2: TBCGradient;
    FOwner: TControl;
    FStyle: TBGRAButtBodyStyle;
    FLightWidth: integer;
    FLightOpacity: byte;
    FLightColor: TColor;
    procedure OnChangeFont(Sender: TObject);
    procedure SetBorderColor(const AValue: TColor);
    procedure SetBorderColorOpacity(const AValue: byte);
    procedure SetBorderStyle(const AValue: TBGRAButtBorderStyle);
    procedure SetColor(const AValue: TColor);
    procedure SetColorOpacity(const AValue: byte);
    procedure SetFLightColor(AValue: TColor);
    procedure SetFont(const AValue: TFont);
    procedure SetGradient1(const AValue: TBCGradient);
    procedure SetGradient1EndPercent(const AValue: single);
    procedure SetGradient2(const AValue: TBCGradient);
    procedure SetLightOpacity(const AValue: byte);
    procedure SetLightWidth(const AValue: integer);
    procedure SetStyle(const AValue: TBGRAButtBodyStyle);
  public
    constructor Create(AOwner: TControl);
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
  published
    property BorderColor: TColor read FBorderColor write SetBorderColor default clBlack;
    property BorderColorOpacity: byte read FBorderColorOpacity
      write SetBorderColorOpacity default 255;
    property BorderStyle: TBGRAButtBorderStyle
      read FBorderStyle write SetBorderStyle default bboSolid;
    property Color: TColor read FColor write SetColor default clDefault;
    property ColorOpacity: byte read FColorOpacity write SetColorOpacity default 255;
    property Font: TFont read FFont write SetFont;
    property Gradient1: TBCGradient read FGradient1 write SetGradient1;
    property Gradient2: TBCGradient read FGradient2 write SetGradient2;
    property Gradient1EndPercent: single read FGradient1EndPercent
      write SetGradient1EndPercent default 35;
    property Style: TBGRAButtBodyStyle read FStyle write SetStyle default bbsGradient;
    property LightWidth: integer read FLightWidth write SetLightWidth;
    property LightOpacity: byte read FLightOpacity write SetLightOpacity;
    property LightColor: TColor read FLightColor write SetFLightColor default clWhite;
  end;

  { TBGRABorderStyleOptions }

  TBGRABorderStyleOptions = class(TPersistent)
  private
    FOwner: TControl;
    FTopLeft: TBGRABorderStyle;
    FTopRight: TBGRABorderStyle;
    FBottomRight: TBGRABorderStyle;
    FBottomLeft: TBGRABorderStyle;
    procedure SetFBottomLeft(AValue: TBGRABorderStyle);
    procedure SetFBottomRight(AValue: TBGRABorderStyle);
    procedure SetFTopLeft(AValue: TBGRABorderStyle);
    procedure SetFTopRight(AValue: TBGRABorderStyle);
    function UpdateOptions: TRoundRectangleOptions;
  public
    constructor Create(AOwner: TControl);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property Options: TRoundRectangleOptions read UpdateOptions;
  published
    property TopLeft: TBGRABorderStyle read FTopLeft write SetFTopLeft;
    property TopRight: TBGRABorderStyle read FTopRight write SetFTopRight;
    property BottomRight: TBGRABorderStyle read FBottomRight write SetFBottomRight;
    property BottomLeft: TBGRABorderStyle read FBottomLeft write SetFBottomLeft;
  end;

  { TCustomBGRAButton }

  TCustomBGRAButton = class(TGraphicControl)
  private
    { Private declarations }
    FBaseRect: TRect;
    FFlipArrow: boolean;
    FActiveButt: TBGRAButtStyle;
    FBGRANormal, FBGRAHover, FBGRAClick, FBGRANormalA, FBGRAHoverA,
    FBGRAClickA: TBGRABitmap;
    FBodyClicked: TBody;
    FBodyHover: TBody;
    FBodyNormal: TBody;
    FBorderStyle: TBGRABorderStyleOptions;
    FBorderStyleDropDown: TBGRABorderStyleOptions;
    FBorderWidth: integer;
    FDown: boolean;
    FGlyph: TBitmap;
    FGlyphMargin: integer;
    FMouseStage: TBCMouseState;
    FOnAfterPrepareBGRAButton: TOnAfterPrepareBGRAButton;
    FOnButtonClick: TNotifyEvent;
    FRoundX: integer;
    FRoundY: integer;
    FStaticButton: boolean;
    FTextAlign: TBGRATextAlign;
    FTextCanvas: boolean;
    FTextShadow: boolean;
    FTextShadowColor: TColor;
    FTextShadowColorOpacity: byte;
    FTextShadowOffsetX: integer;
    FTextShadowOffsetY: integer;
    FTextShadowRadius: integer;
    FStyle: TBGRAButtStyle;
    FTextVAlign: TBGRATextVAlign;
    FGlobalOpacity: byte;
    FTextApplyGlobalOpacity: boolean;
    FStates: TCustomBGRAButtonStates;
    FUpdateCount: integer;
    ARR_SIZE: integer;
    ARR_SPACE: integer;
    AutoSizeExtraY: integer;
    AutoSizeExtraX: integer;
    {$IFDEF DEBUG}
    FInvalidateCount: integer;
    {$ENDIF}
    procedure CalculateBaseRect;
    procedure CalculateTextSize(MaxWidth: integer;
      var NeededWidth, NeededHeight: integer);
    procedure CalculateGlyphSize(var NeededWidth, NeededHeight: integer);
    procedure ConvertToGrayScale(ABGRA: TBGRABitmap);
    procedure DrawArrow(ABGRA: TBGRABitmap; ABody: TBody; ARect: TRect);
    procedure DrawBasicBody(ABGRA: TBGRABitmap; ABody: TBody; ARect: TRect;
      boroptions: TRoundRectangleOptions);
    procedure DrawText(ABGRA: TBGRABitmap; AFontColor: TColor;
      AFontStyle: TFontStyles = []; AFontName: string = 'Default');
    function GetDropDownWidth(AFull: boolean = True): integer;
    procedure Prepare;
    procedure PrepareBGRA(ABGRA: TBGRABitmap; ABody: TBody;
      AState: TCustomBGRAButtonState);
    procedure PrepareBGRADropDown(ABGRA: TBGRABitmap; ABody: TBody;
      AState: TCustomBGRAButtonState);
    procedure OnChangeGlyph(Sender: TObject);
    procedure SetBodyClicked(const AValue: TBody);
    procedure SetBodyHover(const AValue: TBody);
    procedure SetBodyNormal(const AValue: TBody);
    procedure SetBorderWidth(const AValue: integer);
    procedure SetDown(AValue: boolean);
    procedure SetFBorderStyle(AValue: TBGRABorderStyleOptions);
    procedure SetFBorderStyleDropDown(AValue: TBGRABorderStyleOptions);
    procedure SetFFlipArrow(AValue: boolean);
    procedure SetFTextCanvas(AValue: boolean);
    procedure SetGlyph(const AValue: TBitmap);
    procedure SetGlyphMargin(const AValue: integer);
    procedure SetRoundX(const AValue: integer);
    procedure SetRoundY(const AValue: integer);
    procedure SetStaticButton(const AValue: boolean);
    procedure SeTBGRATextAlign(const AValue: TBGRATextAlign);
    procedure SetTextShadow(const AValue: boolean);
    procedure SetTextShadowColor(const AValue: TColor);
    procedure SetTextShadowColorOpacity(const AValue: byte);
    procedure SetTextShadowOffsetX(const AValue: integer);
    procedure SetTextShadowOffsetY(const AValue: integer);
    procedure SetTextShadowRadius(const AValue: integer);
    procedure SetStyle(const AValue: TBGRAButtStyle);
    procedure SeTBGRATextVAlign(const AValue: TBGRATextVAlign);
    procedure SetGlobalOpacity(const AValue: byte);
    procedure SetTextApplyGlobalOpacity(const AValue: boolean);
    procedure UpdateSize;
  protected
    procedure CMChanged(var Message: TLMessage); message CM_CHANGED;
  protected
    { Protected declarations }
    procedure BoundsChanged; override;
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer;
      WithThemeSpace: boolean); override;
    class function GetControlClassDefaultSize: TSize; override;
    procedure Click; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    procedure Paint; override;
    procedure SetEnabled(Value: boolean); override;
    procedure TextChanged; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;
    procedure SetSizeVariables(newArrowSize, newArrowSpace,
      newAutoSizeExtraVertical, newAutoSizeExtraHorizontal: integer);
    property ArrowSize: integer read ARR_SIZE;
    property ArrowSpace: integer read ARR_SPACE;
    property AutoSizeExtraVertical: integer read AutoSizeExtraY;
    property AutoSizeExtraHorizontal: integer read AutoSizeExtraX;
    property BodyNormal: TBody read FBodyNormal write SetBodyNormal;
    property BodyHover: TBody read FBodyHover write SetBodyHover;
    property BodyClicked: TBody read FBodyClicked write SetBodyClicked;
    property BorderWidth: integer read FBorderWidth write SetBorderWidth default 1;
    property BorderStyle: TBGRABorderStyleOptions
      read FBorderStyle write SetFBorderStyle;
    property BorderStyleDropDown: TBGRABorderStyleOptions
      read FBorderStyleDropDown write SetFBorderStyleDropDown;
    property Down: boolean read FDown write SetDown default False;
    property FlipArrow: boolean read FFlipArrow write SetFFlipArrow;
    property Glyph: TBitmap read FGlyph write SetGlyph;
    property GlyphMargin: integer read FGlyphMargin write SetGlyphMargin default 5;
    property RoundX: integer read FRoundX write SetRoundX default 1;
    property RoundY: integer read FRoundY write SetRoundY default 1;
    property TextAlign: TBGRATextAlign
      read FTextAlign write SeTBGRATextAlign default btaCenter;
    property TextCanvas: boolean read FTextCanvas write SetFTextCanvas default False;
    property TextVAlign: TBGRATextVAlign
      read FTextVAlign write SeTBGRATextVAlign default btvaCenter;
    property TextShadow: boolean read FTextShadow write SetTextShadow default True;
    property TextShadowColor: TColor read FTextShadowColor
      write SetTextShadowColor default clBlack;
    property TextShadowColorOpacity: byte read FTextShadowColorOpacity
      write SetTextShadowColorOpacity;
    property TextShadowOffsetX: integer read FTextShadowOffsetX
      write SetTextShadowOffsetX default 5;
    property TextShadowOffsetY: integer read FTextShadowOffsetY
      write SetTextShadowOffsetY default 5;
    property TextShadowRadius: integer read FTextShadowRadius
      write SetTextShadowRadius default 5;
    property Style: TBGRAButtStyle read FStyle write SetStyle default bbtButton;
    property StaticButton: boolean
      read FStaticButton write SetStaticButton default False;
    property GlobalOpacity: byte read FGlobalOpacity write SetGlobalOpacity;
    property TextApplyGlobalOpacity: boolean
      read FTextApplyGlobalOpacity write SetTextApplyGlobalOpacity;
    property OnAfterPrepareBGRAButton: TOnAfterPrepareBGRAButton
      read FOnAfterPrepareBGRAButton write FOnAfterPrepareBGRAButton;
    property OnButtonClick: TNotifyEvent read FOnButtonClick write FOnButtonClick;
  end;

  TBGRAButton = class(TCustomBGRAButton)
  published
    property Action;
    property Align;
    property Anchors;
    property AutoSize;
    property BodyClicked;
    property BodyHover;
    property BodyNormal;
    property BorderSpacing;
    property BorderStyle;
    property BorderStyleDropDown;
    property BorderWidth;
    property Caption;
    property Color;
    property Down;
    property Enabled;
    property FlipArrow;
    property GlobalOpacity;
    property Glyph;
    property GlyphMargin;
    property OnAfterPrepareBGRAButton;
    property OnButtonClick;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property ParentColor;
    property PopupMenu;
    property RoundX;
    property RoundY;
    property StaticButton;
    property Style;
    property TextAlign;
    property TextApplyGlobalOpacity;
    property TextCanvas;
    property TextShadow;
    property TextShadowColor;
    property TextShadowColorOpacity;
    property TextShadowOffsetX;
    property TextShadowOffsetY;
    property TextShadowRadius;
    property TextVAlign;
    property Visible;
  end;

implementation

uses LCLIntf, Math, LCLProc, BGRAPolygon, BGRAFillInfo, SysUtils, BCTools;

{ TBGRABorderStyleOptions }

procedure TBGRABorderStyleOptions.SetFBottomLeft(AValue: TBGRABorderStyle);
begin
  if FBottomLeft = AValue then
    Exit;
  FBottomLeft := AValue;

  FOwner.Perform(CM_CHANGED, 0, 0);
  FOwner.Invalidate;
end;

procedure TBGRABorderStyleOptions.SetFBottomRight(AValue: TBGRABorderStyle);
begin
  if FBottomRight = AValue then
    Exit;
  FBottomRight := AValue;

  FOwner.Perform(CM_CHANGED, 0, 0);
  FOwner.Invalidate;
end;

procedure TBGRABorderStyleOptions.SetFTopLeft(AValue: TBGRABorderStyle);
begin
  if FTopLeft = AValue then
    Exit;
  FTopLeft := AValue;

  FOwner.Perform(CM_CHANGED, 0, 0);
  FOwner.Invalidate;
end;

procedure TBGRABorderStyleOptions.SetFTopRight(AValue: TBGRABorderStyle);
begin
  if FTopRight = AValue then
    Exit;
  FTopRight := AValue;

  FOwner.Perform(CM_CHANGED, 0, 0);
  FOwner.Invalidate;
end;

function TBGRABorderStyleOptions.UpdateOptions: TRoundRectangleOptions;
var
  tl, tr, br, bl: TRoundRectangleOptions;
begin
  case TopLeft of
    bsBevel: tl := [rrTopLeftBevel];
    bsSquare: tl := [rrTopLeftSquare];
    bsRound: tl := [];
  end;

  case TopRight of
    bsBevel: tr := [rrTopRightBevel];
    bsSquare: tr := [rrTopRightSquare];
    bsRound: tr := [];
  end;

  case BottomRight of
    bsBevel: br := [rrBottomRightBevel];
    bsSquare: br := [rrBottomRightSquare];
    bsRound: br := [];
  end;

  case BottomLeft of
    bsBevel: bl := [rrBottomLeftBevel];
    bsSquare: bl := [rrBottomLeftSquare];
    bsRound: bl := [];
  end;

  Result := tl + tr + br + bl;
end;

constructor TBGRABorderStyleOptions.Create(AOwner: TControl);
begin
  FOwner := AOwner;
  FTopLeft := bsRound;
  FTopRight := bsRound;
  FBottomRight := bsRound;
  FBottomLeft := bsRound;
  inherited Create;
end;

destructor TBGRABorderStyleOptions.Destroy;
begin
  inherited Destroy;
end;

procedure TBGRABorderStyleOptions.Assign(Source: TPersistent);
begin
  if Source is TBGRABorderStyleOptions then
  begin
    FTopLeft := TBGRABorderStyleOptions(Source).FTopLeft;
    FTopRight := TBGRABorderStyleOptions(Source).FTopRight;
    FBottomRight := TBGRABorderStyleOptions(Source).FBottomRight;
    FBottomLeft := TBGRABorderStyleOptions(Source).FBottomLeft;

    FOwner.Invalidate;
  end
  else
    inherited Assign(Source);
end;

{ TBody }

procedure TBody.SetColor(const AValue: TColor);
begin
  if FColor = AValue then
    exit;
  FColor := AValue;

  FOwner.Perform(CM_CHANGED, 0, 0);
  FOwner.Invalidate;
end;

procedure TBody.SetColorOpacity(const AValue: byte);
begin
  if FColorOpacity = AValue then
    exit;
  FColorOpacity := AValue;

  FOwner.Perform(CM_CHANGED, 0, 0);
  FOwner.Invalidate;
end;

procedure TBody.SetFLightColor(AValue: TColor);
begin
  if FLightColor = AValue then
    Exit;
  FLightColor := AValue;

  FOwner.Perform(CM_CHANGED, 0, 0);
  FOwner.Invalidate;
end;

procedure TBody.SetFont(const AValue: TFont);
begin
  if FFont = AValue then
    exit;
  FFont.Assign(AValue);

  FOwner.Perform(CM_CHANGED, 0, 0);
  FOwner.Invalidate;
end;

procedure TBody.SetGradient1(const AValue: TBCGradient);
begin
  if FGradient1 = AValue then
    exit;
  FGradient1.Assign(AValue);

  FOwner.Perform(CM_CHANGED, 0, 0);
  FOwner.Invalidate;
end;

procedure TBody.SetGradient1EndPercent(const AValue: single);
begin
  if FGradient1EndPercent = AValue then
    exit;
  FGradient1EndPercent := AValue;

  FOwner.Perform(CM_CHANGED, 0, 0);
  FOwner.Invalidate;
end;

procedure TBody.SetGradient2(const AValue: TBCGradient);
begin
  if FGradient2 = AValue then
    exit;
  FGradient2.Assign(AValue);

  FOwner.Perform(CM_CHANGED, 0, 0);
  FOwner.Invalidate;
end;

procedure TBody.SetLightOpacity(const AValue: byte);
begin
  if FLightOpacity = AValue then
    exit;
  FLightOpacity := AValue;

  FOwner.Perform(CM_CHANGED, 0, 0);
  FOwner.Invalidate;
end;

procedure TBody.SetLightWidth(const AValue: integer);
begin
  if FLightWidth = AValue then
    exit;
  FLightWidth := AValue;

  FOwner.Perform(CM_CHANGED, 0, 0);
  FOwner.Invalidate;
end;

procedure TBody.OnChangeFont(Sender: TObject);
begin
  FOwner.Perform(CM_CHANGED, 0, 0);
  FOwner.Invalidate;
  TCustomBGRAButton(FOwner).UpdateSize;
end;

procedure TBody.SetBorderColor(const AValue: TColor);
begin
  if FBorderColor = AValue then
    exit;
  FBorderColor := AValue;

  FOwner.Perform(CM_CHANGED, 0, 0);
  FOwner.Invalidate;
end;

procedure TBody.SetBorderColorOpacity(const AValue: byte);
begin
  if FBorderColorOpacity = AValue then
    exit;
  FBorderColorOpacity := AValue;

  FOwner.Perform(CM_CHANGED, 0, 0);
  FOwner.Invalidate;
end;

procedure TBody.SetBorderStyle(const AValue: TBGRAButtBorderStyle);
begin
  if FBorderStyle = AValue then
    exit;
  FBorderStyle := AValue;

  FOwner.Perform(CM_CHANGED, 0, 0);
  FOwner.Invalidate;
end;

procedure TBody.SetStyle(const AValue: TBGRAButtBodyStyle);
begin
  if FStyle = AValue then
    exit;
  FStyle := AValue;

  FOwner.Perform(CM_CHANGED, 0, 0);
  FOwner.Invalidate;
end;

constructor TBody.Create(AOwner: TControl);
begin
  FOwner := AOwner;
  FBorderColor := clBlack;
  FBorderColorOpacity := 255;
  FBorderStyle := bboSolid;
  FColor := clBtnFace;
  FColorOpacity := 255;
  FFont := TFont.Create;
  FGradient1 := TBCGradient.Create(FOwner);
  FGradient2 := TBCGradient.Create(FOwner);
  FGradient1EndPercent := 35;
  FStyle := bbsGradient;
  FLightOpacity := 64;
  FLightWidth := 0;
  FLightColor := clWhite;

  FFont.Assign(FOwner.Font);
  FFont.OnChange := @OnChangeFont;
  inherited Create;
end;

destructor TBody.Destroy;
begin
  FGradient1.Free;
  FGradient2.Free;
  FFont.Free;
  inherited Destroy;
end;

procedure TBody.Assign(Source: TPersistent);
begin
  if Source is TBody then
  begin
    FBorderColor := TBody(Source).FBorderColor;
    FBorderStyle := TBody(Source).FBorderStyle;
    FColor := TBody(Source).FColor;
    FStyle := TBody(Source).FStyle;
    FGradient1EndPercent := TBody(Source).FGradient1EndPercent;
    FLightOpacity := TBody(Source).FLightOpacity;
    FLightWidth := TBody(Source).FLightWidth;
    FLightColor := TBody(Source).FLightColor;
    FFont.Assign(TBody(Source).FFont);
    FGradient1.Assign(TBody(Source).FGradient1);
    FGradient2.Assign(TBody(Source).FGradient2);

    FOwner.Invalidate;
    FOwner.InvalidatePreferredSize;
    FOwner.AdjustSize;
  end
  else
    inherited Assign(Source);
end;

{ TCustomBGRAButton }

procedure TCustomBGRAButton.CalculateBaseRect;
begin
  { As far as border width is bigger, BGRA drawing rectangle with offset (half border width) }
  FBaseRect := ClientRect;
  Inc(FBaseRect.Left, Round(FBorderWidth / 2));
  Inc(FBaseRect.Top, Round(FBorderWidth / 2));
  Dec(FBaseRect.Right, Round(FBorderWidth / 2) + 1);
  Dec(FBaseRect.Bottom, Round(FBorderWidth / 2) + 1);
end;

procedure TCustomBGRAButton.CalculateTextSize(MaxWidth: integer;
  var NeededWidth, NeededHeight: integer);
var
  s: TSize;
  ax, ay: integer;
begin
  if (Caption = '') or (FBGRANormal = nil) then
  begin
    NeededWidth := 0;
    NeededHeight := 0;
    Exit;
  end;

  Canvas.Font := FBodyNormal.FFont;
  FBGRANormal.FontStyle := FBodyNormal.FFont.Style;

  if FTextCanvas then
  begin
    FBGRANormal.FontHeight := FBodyNormal.FFont.Height;
    FBGRANormal.FontQuality := fqSystemClearType;
  end
  else
  begin
    FBGRANormal.FontHeight := Canvas.TextHeight(Caption);
    FBGRANormal.FontQuality := fqFineAntialiasing;
  end;

  if FBodyNormal.FFont.Name = '' then
    FBGRANormal.FontName := 'default'
  else
    FBGRANormal.FontName := FBodyNormal.FFont.Name;

  s := FBGRANormal.TextSize(Caption);

  { shadow offset }
  if FTextShadow then
  begin
    if FTextShadowOffsetX < 0 then
      ax := (FTextShadowOffsetX) - (FTextShadowOffsetX * 2)
    else
      ax := FTextShadowOffsetX;

    if FTextShadowOffsetY < 0 then
      ay := (FTextShadowOffsetY) - (FTextShadowOffsetY * 2)
    else
      ay := FTextShadowOffsetY;

    Inc(s.cx, 2 * ax + 2 * FTextShadowRadius);
    Inc(s.cy, 2 * ay + 2 * FTextShadowRadius);
  end;

  NeededWidth := s.cx;
  NeededHeight := s.cy;

  // old shadow offset
  //NeededWidth := s.cx + Round(1.2 * FTextShadowRadius) + FTextShadowOffsetX;
  //NeededHeight := s.cy + Round(1.2 * FTextShadowRadius) + FTextShadowOffsetY;
end;

procedure TCustomBGRAButton.CalculateGlyphSize(var NeededWidth, NeededHeight: integer);
begin
  if FGlyph = nil then
  begin
    NeededHeight := 0;
    NeededWidth := 0;
    Exit;
  end;

  NeededWidth := FGlyph.Width;
  NeededHeight := FGlyph.Height;
end;

procedure TCustomBGRAButton.ConvertToGrayScale(ABGRA: TBGRABitmap);
var
  bounds: TRect;
  px: PBGRAPixel;
  xb, yb: integer;
begin
  bounds := ABGRA.GetImageBounds;
  if (bounds.Right <= bounds.Left) or (bounds.Bottom <= Bounds.Top) then
    exit;

  for yb := bounds.Top to bounds.bottom - 1 do
  begin
    px := ABGRA.scanline[yb] + bounds.left;
    for xb := bounds.left to bounds.right - 1 do
    begin
      px^ := BGRAToGrayscale(px^);
      Inc(px);
    end;
  end;
  ABGRA.InvalidateBitmap;
end;

procedure TCustomBGRAButton.DrawText(ABGRA: TBGRABitmap; AFontColor: TColor;
  AFontStyle: TFontStyles; AFontName: string);
var
  s: TSize;
  x, y, gx, gy: integer;
  gly, shd: TBGRABitmap;
begin
  ABGRA.FontAntialias := True;
  shd := TBGRABitmap.Create(ClientWidth, ClientHeight, BGRAPixelTransparent);
  shd.FontAntialias := True;
  Canvas.Font := FBodyNormal.FFont;
  ABGRA.FontStyle := AFontStyle;
  ABGRA.FontName := AFontName;

  if FTextCanvas then
  begin
    ABGRA.FontHeight := FBodyNormal.FFont.Height;
    ABGRA.FontQuality := fqSystemClearType;
  end
  else
  begin
    ABGRA.FontHeight := Canvas.TextHeight(Caption);
    ABGRA.FontQuality := fqFineAntialiasing;
  end;

  x := 0;
  y := 0;
  gx := 0;
  gy := 0;

  s := ABGRA.TextSize(Caption);

  { X Position }
  case FTextAlign of
    btaLeft:
    begin
      if (FGlyph <> nil) and (not FGlyph.Empty) then
        gx := 5;
      if gx > 0 then
        x := gx + FGlyph.Width + FGlyphMargin
      else
        x := 5;
    end;
    btaCenter:
    begin
      if (FGlyph <> nil) and (not FGlyph.Empty) then
      begin
        gx := Round((ClientWidth - ifthen(FStyle = bbtDropDown, ARR_SPACE)) / 2) -
          Round((FGlyph.Width + s.cx + FGlyphMargin) / 2);
        x := gx + FGlyph.Width + FGlyphMargin;
      end
      else
        x := Round((ClientWidth - ifthen(FStyle = bbtDropDown, ARR_SPACE)) / 2) -
          Round((s.cx) / 2);
    end;
    btaRight:
    begin
      if (FGlyph <> nil) and (not FGlyph.Empty) then
      begin
        gx := ClientWidth - 5 - ifthen(FStyle = bbtDropDown, ARR_SPACE) -
          FGlyph.Width - FGlyphMargin - s.cx;
        x := gx + FGlyph.Width + FGlyphMargin;
      end
      else
        x := ClientWidth - 5 - ifthen(FStyle = bbtDropDown, ARR_SPACE) - s.cx;
    end;
  end;

  { Y position }
  case FTextVAlign of
    btvaTop:
    begin
      if (FGlyph <> nil) and (not FGlyph.Empty) then
      begin
        if FGlyph.Height > s.cy then
        begin
          gy := 2 + FBorderWidth;
          y := gy + Round(FGlyph.Height / 2);
        end
        else
        begin
          y := 2 + FBorderWidth;
          gy := y + Round((s.cy / 2) - (FGlyph.Height / 2));
        end;
      end
      else
        y := 2 + FBorderWidth;
    end;
    btvaCenter:
    begin
      if (FGlyph <> nil) and (not FGlyph.Empty) then
        gy := Round(ClientHeight / 2) - Round(FGlyph.Height / 2);
      y := Round(ClientHeight / 2) - Round(s.cy / 2);
    end;
    btvaBottom:
    begin
      if (FGlyph <> nil) and (not FGlyph.Empty) then
      begin
        if FGlyph.Height > s.cy then
        begin
          gy := ClientHeight - FBorderWidth - FGlyph.Height - 2;
          y := gy + Round(FGlyph.Height / 2);
        end
        else
        begin
          y := ClientHeight - FBorderWidth - 2 - s.cy;
          gy := y + Round((s.cy / 2) - (FGlyph.Height / 2));
        end;
      end
      else
        y := ClientHeight - FBorderWidth - 2 - s.cy;
    end;
  end;

  if (FMouseStage = msClicked) and (FActiveButt = bbtButton) and (not FStaticButton) then
  begin
    Inc(gx);
    Inc(gy);
    Inc(x);
    Inc(y);
  end;

  if FTextShadow then
  begin
    shd.SetSize(s.cx + 2 * FTextShadowRadius, s.cy + 2 * FTextShadowRadius);
    shd.Fill(BGRAPixelTransparent);
    shd.FontAntialias := True;
    shd.FontStyle := AFontStyle;
    shd.FontName := AFontName;

    if FTextCanvas then
    begin
      shd.FontHeight := FBodyNormal.FFont.Height;
      shd.FontQuality := fqSystemClearType;
    end
    else
    begin
      shd.FontHeight := Canvas.TextHeight(Caption);
      shd.FontQuality := fqFineAntialiasing;
    end;

    shd.TextOut(FTextShadowRadius, FTextShadowRadius,
      Caption, ColorToBGRA(ColorToRGB(FTextShadowColor), FTextShadowColorOpacity));
    BGRAReplace(shd, shd.FilterBlurRadial(FTextShadowRadius, rbFast));
    ABGRA.PutImage(x + FTextShadowOffsetX - FTextShadowRadius, y +
      FTextShadowOffsetY - FTextShadowRadius, shd,
      dmDrawWithTransparency);
  end;

  if (FGlyph <> nil) and (not FGlyph.Empty) then
  begin
    gly := TBGRABitmap.Create(FGlyph);
    if Caption <> '' then
      ABGRA.PutImage(gx, gy, gly, dmDrawWithTransparency)
    else
      ABGRA.PutImage(Round((ClientWidth - gly.Width) div 2), gy, gly,
        dmDrawWithTransparency);
    gly.Free;
  end;

  ABGRA.Bitmap;
  ABGRA.TextOut(x, y, Caption, ColorToBGRA(ColorToRGB(AFontColor)));

  shd.Free;
end;

function TCustomBGRAButton.GetDropDownWidth(AFull: boolean): integer;
begin
  Result := ARR_SPACE + (ifthen(AFull, 2, 1) * FBorderWidth);
end;

procedure TCustomBGRAButton.Prepare;
begin
  FStates := FStates + [bstPrepareNormal, bstPrepareHover, bstPrepareClick,
    bstPrepareNormalA, bstPrepareHoverA, bstPrepareClickA];
end;

procedure TCustomBGRAButton.PrepareBGRA(ABGRA: TBGRABitmap; ABody: TBody;
  AState: TCustomBGRAButtonState);
var
  r: TRect;
begin
  if (csCreating in FControlState) or (FUpdateCount > 0) then
    Exit;
  { Calculating rect }
  r := FBaseRect;

  if FStyle = bbtDropDown then
    Dec(r.Right, GetDropDownWidth(False));

  { Refreshing size }
  ABGRA.SetSize(Width - ifthen(FStyle = bbtDropDown, GetDropDownWidth(False)), Height);
  { Clearing previous paint }
  ABGRA.Fill(BGRAPixelTransparent);
  { Basic body }
  DrawBasicBody(ABGRA, ABody, r, BorderStyle.Options);

  if FTextApplyGlobalOpacity then
  begin
    { Drawing text }
    DrawText(ABGRA, ABody.FFont.Color, ABody.FFont.Style, ABody.FFont.Name);
    { Set global opacity }
    ABGRA.ApplyGlobalOpacity(FGlobalOpacity);
  end
  else
  begin
    { Set global opacity }
    ABGRA.ApplyGlobalOpacity(FGlobalOpacity);
    { Drawing text }
    DrawText(ABGRA, ABody.FFont.Color, ABody.FFont.Style, ABody.FFont.Name);
  end;

  { Convert to gray if not enabled }
  if not Enabled then
    ConvertToGrayScale(ABGRA);

  if Assigned(FOnAfterPrepareBGRAButton) then
    FOnAfterPrepareBGRAButton(Self, ABGRA, r, AState);
end;

procedure TCustomBGRAButton.PrepareBGRADropDown(ABGRA: TBGRABitmap;
  ABody: TBody; AState: TCustomBGRAButtonState);
var
  r: TRect;
begin
  if (ABGRA = nil) or (FUpdateCount > 0) then
    Exit;

  ABGRA.SetSize(GetDropDownWidth, ClientHeight);
  ABGRA.Fill(BGRAPixelTransparent);
  r := ABGRA.ClipRect;
  Inc(r.Left, Round(FBorderWidth / 2));
  Inc(r.Top, Round(FBorderWidth / 2));
  Dec(r.Right, Round(FBorderWidth / 2) + 1);
  Dec(r.Bottom, Round(FBorderWidth / 2) + 1);
  DrawBasicBody(ABGRA, ABody, r, BorderStyleDropDown.Options);
  { Drawing arrow }
  DrawArrow(ABGRA, ABody, r);

  { Convert to gray if not enabled }
  if not Enabled then
    ConvertToGrayScale(ABGRA);

  if Assigned(FOnAfterPrepareBGRAButton) then
    FOnAfterPrepareBGRAButton(Self, ABGRA, ABGRA.ClipRect, AState);
end;

procedure TCustomBGRAButton.DrawArrow(ABGRA: TBGRABitmap; ABody: TBody; ARect: TRect);
var
  p: ArrayOfTPointF;
  o: byte = 0;
  alpha: byte;
  n: byte;
  temp: TBGRABitmap;
begin
  { Clicked offset}
  if (FMouseStage = msClicked) and (FActiveButt = bbtDropDown) and
    (not FStaticButton) then
    o := 1;

  { Poly }
  SetLength(p, 3);

  p[0].x := ARect.Right - Round(ARR_SPACE / 2) + o;
  p[0].y := ARect.Bottom - Round((ARect.Bottom - ARect.Top) / 2) +
    Round(ARR_SIZE / 4) + o;

  p[1].x := ARect.Right - Round(ARR_SPACE / 2) + Round(ARR_SIZE / 2) + o;
  p[1].y := ARect.Bottom - Round((ARect.Bottom - ARect.Top) / 2) -
    Round(ARR_SIZE / 4) + o;

  p[2].x := ARect.Right - Round(ARR_SPACE / 2) - Round(ARR_SIZE / 2) + o;
  p[2].y := p[1].y;

  if TextApplyGlobalOpacity then
    alpha := GlobalOpacity
  else
    alpha := 255;

  temp := TBGRABitmap.Create(ABGRA.Width, ABGRA.Height);

  // Fill n times to get best quality
  for n := 1 to 6 do
    temp.FillPolyAntialias(p, ColorToBGRA(ColorToRGB(FBodyNormal.Font.Color),
      alpha));

  if FlipArrow then
    temp.VerticalFlip;

  ABGRA.PutImage(0, 0, temp, dmDrawWithTransparency);
  temp.Free;
end;

procedure TCustomBGRAButton.DrawBasicBody(ABGRA: TBGRABitmap; ABody: TBody;
  ARect: TRect; boroptions: TRoundRectangleOptions);
var
  borcolor, backcolor: TBGRAPixel;
  gra: TBGRAGradientScanner;
  back: TBGRABitmap;
  grect1, grect2: TRect;
  multi: TBGRAMultishapeFiller;
  fiLight: TFillBorderRoundRectInfo;
begin
  { Calculating border width and color }
  case ABody.FBorderStyle of
    bboNone: borcolor := BGRAPixelTransparent;
    bboSolid: borcolor := ColorToBGRA(ColorToRGB(ABody.FBorderColor),
        ABody.FBorderColorOpacity);
  end;

  { Background color }
  case ABody.FStyle of
    bbsClear: backcolor := BGRAPixelTransparent;
    bbsColor: backcolor := ColorToBGRA(ColorToRGB(ABody.FColor), ABody.FColorOpacity);
  end;

  case ABody.FStyle of
    bbsClear, bbsColor:
      { Solid background color }
      ABGRA.RoundRectAntialias(ARect.Left, ARect.Top, ARect.Right,
        ARect.Bottom, FRoundX, FRoundY,
        borcolor, FBorderWidth, backcolor, boroptions);
    bbsGradient:
    begin
      { Using multishape filler to merge background gradient and border }
      multi := TBGRAMultishapeFiller.Create;
      multi.PolygonOrder := poFirstOnTop; { Border will replace background }

      if borcolor.alpha <> 0 then
        { Let the background be wider with transparent border }
        multi.AddRoundRectangleBorder(ARect.Left, ARect.Top, ARect.Right,
          ARect.Bottom, FRoundX, FRoundY,
          FBorderWidth, borcolor, boroptions);

      { Gradients }
      back := TBGRABitmap.Create(ClientWidth, ClientHeight, BGRAPixelTransparent);
      grect1 := ARect;
      grect2 := ARect;
      { Gradient 1 }
      if ABody.FGradient1EndPercent > 0 then
      begin
        grect1.Bottom := Round((grect1.Bottom / 100) * ABody.FGradient1EndPercent);
        gra := CreateGradient(ABody.FGradient1, grect1);
        back.FillRect(grect1.Left, grect1.Top, grect1.Right, grect1.Bottom,
          gra, dmSet
          );
        gra.Free;
      end;
      { Gradient 2 }
      if ABody.FGradient1EndPercent < 100 then
      begin
        if grect1.Bottom < ARect.Bottom then
          grect2.Top := grect1.Bottom - 1;
        gra := CreateGradient(ABody.FGradient2, grect2);
        back.FillRect(grect2.Left, grect2.Top, grect2.Right, grect2.Bottom,
          gra, dmSet
          );
        gra.Free;
      end;

      multi.AddRoundRectangle(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom,
        FRoundX, FRoundY, back, boroptions);

      multi.Draw(ABGRA);
      multi.Free;
      back.Free;

      if ABody.LightWidth > 0 then
      begin
        //compute light position
        fiLight := TFillBorderRoundRectInfo.Create(
          ARect.Left, ARect.Top, ARect.Right, ARect.Bottom, FRoundX,
          FRoundY, FBorderWidth + ABody.LightWidth, boroptions);
        //check if there is an inner position
        if fiLight.InnerBorder <> nil then
          with fiLight.InnerBorder do //fill with light
            ABGRA.RoundRectAntialias(topleft.x, topleft.y, bottomright.x,
              bottomright.y, radiusx, radiusY,
              ColorToBGRA(ColorToRGB(ABody.LightColor), ABody.LightOpacity),
              ABody.LightWidth, boroptions);
        fiLight.Free;
      end;
    end;
  end;
end;

procedure TCustomBGRAButton.OnChangeGlyph(Sender: TObject);
begin
  Invalidate;
  UpdateSize;
end;

procedure TCustomBGRAButton.SetBodyClicked(const AValue: TBody);
begin
  if FBodyClicked = AValue then
    exit;
  FBodyClicked.Assign(AValue);

  Changed;
  Invalidate;
end;

procedure TCustomBGRAButton.SetBodyHover(const AValue: TBody);
begin
  if FBodyHover = AValue then
    exit;
  FBodyHover.Assign(AValue);

  Changed;
  Invalidate;
end;

procedure TCustomBGRAButton.SetBodyNormal(const AValue: TBody);
begin
  if FBodyNormal = AValue then
    exit;
  FBodyNormal.Assign(AValue);

  Changed;
  Invalidate;
end;

procedure TCustomBGRAButton.SetBorderWidth(const AValue: integer);
begin
  if FBorderWidth = AValue then
    exit;
  FBorderWidth := AValue;

  Changed;
  Invalidate;
  UpdateSize;
end;

procedure TCustomBGRAButton.SetDown(AValue: boolean);
begin
  if FDown = AValue then
    exit;
  FDown := AValue;

  Update;
end;

procedure TCustomBGRAButton.SetFBorderStyle(AValue: TBGRABorderStyleOptions);
begin
  if FBorderStyle = AValue then
    Exit;
  FBorderStyle := AValue;

  Changed;
  Invalidate;
end;

procedure TCustomBGRAButton.SetFBorderStyleDropDown(AValue: TBGRABorderStyleOptions);
begin
  if FBorderStyleDropDown = AValue then
    Exit;
  FBorderStyleDropDown := AValue;

  Changed;
  Invalidate;
end;

procedure TCustomBGRAButton.SetFFlipArrow(AValue: boolean);
begin
  if FFlipArrow = AValue then
    Exit;
  FFlipArrow := AValue;

  Changed;
  Invalidate;
end;

procedure TCustomBGRAButton.SetFTextCanvas(AValue: boolean);
begin
  if FTextCanvas = AValue then
    exit;
  FTextCanvas := AValue;

  Changed;
  Invalidate;
  UpdateSize;
end;

procedure TCustomBGRAButton.SetGlyph(const AValue: TBitmap);
begin
  if (FGlyph <> nil) and (FGlyph = AValue) then
    exit;

  FGlyph.Assign(AValue);

  Changed;
  Invalidate;
  UpdateSize;
end;

procedure TCustomBGRAButton.SetGlyphMargin(const AValue: integer);
begin
  if FGlyphMargin = AValue then
    exit;
  FGlyphMargin := AValue;

  Changed;
  Invalidate;
  UpdateSize;
end;

procedure TCustomBGRAButton.SetRoundX(const AValue: integer);
begin
  if FRoundX = AValue then
    exit;
  FRoundX := AValue;

  Changed;
  Invalidate;
end;

procedure TCustomBGRAButton.SetRoundY(const AValue: integer);
begin
  if FRoundY = AValue then
    exit;
  FRoundY := AValue;

  Changed;
  Invalidate;
end;

procedure TCustomBGRAButton.SetStaticButton(const AValue: boolean);
begin
  if FStaticButton = AValue then
    exit;
  FStaticButton := AValue;

  Changed;
  Invalidate;
end;

procedure TCustomBGRAButton.SeTBGRATextAlign(const AValue: TBGRATextAlign);
begin
  if FTextAlign = AValue then
    exit;
  FTextAlign := AValue;

  Changed;
  Invalidate;
end;

procedure TCustomBGRAButton.SetTextShadow(const AValue: boolean);
begin
  if FTextShadow = AValue then
    exit;
  FTextShadow := AValue;

  Changed;
  Invalidate;
  UpdateSize;
end;

procedure TCustomBGRAButton.SetTextShadowColor(const AValue: TColor);
begin
  if FTextShadowColor = AValue then
    exit;
  FTextShadowColor := AValue;

  Changed;
  Invalidate;
  UpdateSize;
end;

procedure TCustomBGRAButton.SetTextShadowColorOpacity(const AValue: byte);
begin
  if FTextShadowColorOpacity = AValue then
    exit;
  FTextShadowColorOpacity := AValue;

  Changed;
  Invalidate;
  UpdateSize;
end;

procedure TCustomBGRAButton.SetTextShadowOffsetX(const AValue: integer);
begin
  if FTextShadowOffsetX = AValue then
    exit;
  FTextShadowOffsetX := AValue;

  Changed;
  Invalidate;
  UpdateSize;
end;

procedure TCustomBGRAButton.SetTextShadowOffsetY(const AValue: integer);
begin
  if FTextShadowOffsetY = AValue then
    exit;
  FTextShadowOffsetY := AValue;

  Changed;
  Invalidate;
  UpdateSize;
end;

procedure TCustomBGRAButton.SetTextShadowRadius(const AValue: integer);
begin
  if FTextShadowRadius = AValue then
    exit;
  FTextShadowRadius := AValue;

  Changed;
  Invalidate;
  UpdateSize;
end;

procedure TCustomBGRAButton.SetStyle(const AValue: TBGRAButtStyle);
begin
  if FStyle = AValue then
    exit;
  FStyle := AValue;

  if (AValue = bbtDropDown) then
  begin
    if (FBGRANormalA = nil) then
    begin
      FBGRANormalA := TBGRABitmap.Create(ARR_SPACE, ClientHeight, BGRAPixelTransparent);
      FBGRAHoverA := TBGRABitmap.Create(ARR_SPACE, ClientHeight, BGRAPixelTransparent);
      FBGRAClickA := TBGRABitmap.Create(ARR_SPACE, ClientHeight, BGRAPixelTransparent);
    end;
  end
  else
  begin
    FreeThenNil(FBGRANormalA);
    FreeThenNil(FBGRAHoverA);
    FreeThenNil(FBGRAClickA);
  end;

  Changed;
  Invalidate;
  UpdateSize;
end;

procedure TCustomBGRAButton.SeTBGRATextVAlign(const AValue: TBGRATextVAlign);
begin
  if FTextVAlign = AValue then
    exit;
  FTextVAlign := AValue;

  Changed;
  Invalidate;
end;

procedure TCustomBGRAButton.UpdateSize;
begin
  CalculateBaseRect;
  FStates := FStates + [bstPrepareNormal, bstPrepareHover, bstPrepareClick,
    bstPrepareNormalA, bstPrepareHoverA, bstPrepareClickA];

  InvalidatePreferredSize;
  AdjustSize;
end;

procedure TCustomBGRAButton.CMChanged(var Message: TLMessage);
begin
  if FUpdateCount > 0 then
    Exit;
  Prepare;
end;

procedure TCustomBGRAButton.BoundsChanged;
begin
  CalculateBaseRect;
  Prepare;
  inherited BoundsChanged;
end;

procedure TCustomBGRAButton.CalculatePreferredSize(
  var PreferredWidth, PreferredHeight: integer; WithThemeSpace: boolean);
var
  AWidth: integer;
  gh: integer = 0;
  gw: integer = 0;
begin
  if (Parent = nil) or (not Parent.HandleAllocated) then
    Exit;
  if WidthIsAnchored then
    AWidth := Width
  else
    AWidth := 10000;

  CalculateTextSize(AWidth, PreferredWidth, PreferredHeight);

  // Extra pixels for DropDown
  if Style = bbtDropDown then
    Inc(PreferredWidth, GetDropDownWidth);

  CalculateGlyphSize(gw, gh);

  if (FGlyph <> nil) and (not FGlyph.Empty) then
  begin
    if Caption = '' then
    begin
      Inc(PreferredWidth, gw{ - AutoSizeExtraY * 2});
      Inc(PreferredHeight, gh);
    end
    else
    begin
      Inc(PreferredWidth, gw + FGlyphMargin);
      if gh > PreferredHeight then
        PreferredHeight := gh;
    end;
  end;

  // Extra pixels for AutoSize
  Inc(PreferredWidth, AutoSizeExtraX);
  Inc(PreferredHeight, AutoSizeExtraY);
end;

class function TCustomBGRAButton.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 123;
  Result.CY := 33;
end;

procedure TCustomBGRAButton.Click;
begin
  if (FActiveButt = bbtDropDown) and Assigned(FOnButtonClick) then
  begin
    FOnButtonClick(Self);
    Exit;
  end;
  inherited Click;
end;

procedure TCustomBGRAButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if csDesigning in ComponentState then
    exit;

  if (Button = mbLeft) and Enabled and (not (FMouseStage = msClicked)) then
  begin
    FMouseStage := msClicked;
    Invalidate;
  end;
end;

procedure TCustomBGRAButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
var
  p: TPoint;
begin
  inherited MouseUp(Button, Shift, X, Y);
  if csDesigning in ComponentState then
    exit;

  if (Button = mbLeft) and Enabled and (FMouseStage = msClicked) then
  begin
    FMouseStage := msHover;
    Invalidate;
  end;

  if (FActiveButt = bbtDropDown) and (PopupMenu <> nil) and Enabled then
  begin
    if FlipArrow then
      p := ClientToScreen(Point(Width - ARR_SPACE - (BorderWidth * 2),
        {PopupMenu.Height} -1))
    else
      p := ClientToScreen(Point(Width - ARR_SPACE - (BorderWidth * 2), Height + 1));

    PopupMenu.PopUp(p.x, p.y);
    //p := ClientToScreen(Point(X, Y));
    //PopupMenu.PopUp(p.x, p.y);
  end;
end;

procedure TCustomBGRAButton.MouseEnter;
begin
  if csDesigning in ComponentState then
    exit;
  FMouseStage := msHover;
  Invalidate;
  inherited MouseEnter;
end;

procedure TCustomBGRAButton.MouseLeave;
begin
  if csDesigning in ComponentState then
    exit;
  FMouseStage := msNone;
  Invalidate;
  inherited MouseLeave;
end;

procedure TCustomBGRAButton.MouseMove(Shift: TShiftState; X, Y: integer);
begin
  inherited MouseMove(Shift, X, Y);

  if (FStyle = bbtDropDown) and (x >= ClientRect.Right - ARR_SPACE) then
    FActiveButt := bbtDropDown
  else
    FActiveButt := bbtButton;

  Invalidate;
end;

procedure TCustomBGRAButton.Paint;
var
  s1, s2: TBCMouseState;
begin
  //inherited Paint;
  if (csCreating in FControlState) or (FUpdateCount > 0) then
    Exit;
  { TODO -oDibo : In future, separate FMouseStage to FButtonStage and FDropDownStage }
  if FStaticButton then
  begin
    s1 := msNone;
    s2 := msNone;
  end
  else
  if (FActiveButt = bbtButton) and FDown then
    s1 := msClicked
  else
    case FActiveButt of
      bbtButton:
      begin
        s1 := FMouseStage;
        s2 := msNone;
      end;
      bbtDropDown:
      begin
        s1 := msNone;
        s2 := FMouseStage;
      end;
    end;

  { Main button }
  case s1 of
    msNone:
    begin
      if bstPrepareNormal in FStates then
      begin
        PrepareBGRA(FBGRANormal, FBodyNormal, bstPrepareNormal);
        Exclude(FStates, bstPrepareNormal);
      end;
      FBGRANormal.Draw(Self.Canvas, 0, 0, False);
    end;
    msHover:
    begin
      if bstPrepareHover in FStates then
      begin
        PrepareBGRA(FBGRAHover, FBodyHover, bstPrepareHover);
        Exclude(FStates, bstPrepareHover);
      end;
      FBGRAHover.Draw(Self.Canvas, 0, 0, False);
    end;
    msClicked:
    begin
      if bstPrepareClick in FStates then
      begin
        PrepareBGRA(FBGRAClick, FBodyClicked, bstPrepareClick);
        Exclude(FStates, bstPrepareClick);
      end;
      FBGRAClick.Draw(Self.Canvas, 0, 0, False);
    end;
  end;

  { Drop down button }
  if FStyle = bbtDropDown then
    case s2 of
      msNone:
      begin
        if bstPrepareNormalA in FStates then
        begin
          PrepareBGRADropDown(FBGRANormalA, FBodyNormal, bstPrepareNormalA);
          Exclude(FStates, bstPrepareNormalA);
        end;
        FBGRANormalA.Draw(Self.Canvas, FBGRANormal.Width - FBorderWidth, 0, False);
      end;
      msHover:
      begin
        if bstPrepareHoverA in FStates then
        begin
          PrepareBGRADropDown(FBGRAHoverA, FBodyHover, bstPrepareHoverA);
          Exclude(FStates, bstPrepareHoverA);
        end;
        FBGRAHoverA.Draw(Self.Canvas, FBGRANormal.Width - FBorderWidth, 0, False);
      end;
      msClicked:
      begin
        if bstPrepareClickA in FStates then
        begin
          PrepareBGRADropDown(FBGRAClickA, FBodyClicked, bstPrepareClickA);
          Exclude(FStates, bstPrepareClickA);
        end;
        FBGRAClickA.Draw(Self.Canvas, FBGRANormal.Width - FBorderWidth, 0, False);
      end;
    end;
  {$IFDEF DEBUG}
  //Debug: display in button Invalidate Count.
  Inc(FInvalidateCount);
  Canvas.TextOut(0, 0, IntToStr(FInvalidateCount));
  {$ENDIF}
end;

procedure TCustomBGRAButton.SetEnabled(Value: boolean);
begin
  inherited SetEnabled(Value);

  Changed;
  Invalidate;
end;

procedure TCustomBGRAButton.TextChanged;
begin
  inherited TextChanged;
  Invalidate;
  UpdateSize;
end;

procedure TCustomBGRAButton.SetGlobalOpacity(const AValue: byte);
begin
  if FGlobalOpacity = AValue then
    exit;
  FGlobalOpacity := AValue;

  Changed;
  Invalidate;
end;

procedure TCustomBGRAButton.SetTextApplyGlobalOpacity(const AValue: boolean);
begin
  if FTextApplyGlobalOpacity = AValue then
    exit;
  FTextApplyGlobalOpacity := AValue;

  Changed;
  Invalidate;
end;

{procedure TCustomBGRAButton.WMLButtonDown(var Message: TLMLButtonDown);
begin

end; }

constructor TCustomBGRAButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DisableAutoSizing;
  Include(FControlState, csCreating);
  //{$IFDEF WINDOWS}
  // default sizes under different dpi settings
  //SetSizeVariables(ScaleX(8,96), ScaleX(16,96), ScaleY(8,96), ScaleX(24,96));
  //{$ELSE}
  // default sizes
  SetSizeVariables(8, 16, 8, 24);
  //{$ENDIF}
  BeginUpdate;
  try
    with GetControlClassDefaultSize do
      SetInitialBounds(0, 0, CX, CY);
    ControlStyle := ControlStyle + [csAcceptsControls];
    FBGRANormal := TBGRABitmap.Create(Width, Height, BGRAPixelTransparent);
    FBGRANormal.FontAntialias := True;
    FBGRAHover := TBGRABitmap.Create(Width, Height, BGRAPixelTransparent);
    FBGRAHover.FontAntialias := True;
    FBGRAClick := TBGRABitmap.Create(Width, Height, BGRAPixelTransparent);
    FBGRAClick.FontAntialias := True;
    FBorderStyle := TBGRABorderStyleOptions.Create(Self);
    FBorderStyleDropDown := TBGRABorderStyleOptions.Create(Self);
    ParentColor := False;
    Color := clNone;
    FBodyNormal := TBody.Create(Self);
    FBodyHover := TBody.Create(Self);
    FBodyClicked := TBody.Create(Self);
    FMouseStage := msNone;
    FTextAlign := btaCenter;
    FTextCanvas := False;
    FTextVAlign := btvaCenter;
    FTextShadow := True;
    FTextShadowRadius := 5;
    FTextShadowColor := clBlack;
    FTextShadowColorOpacity := 255;
    FTextShadowOffsetX := 5;
    FTextShadowOffsetY := 5;
    FBorderWidth := 1;
    FRoundX := 1;
    FRoundY := 1;
    FFlipArrow := False;
    FGlyph := TBitmap.Create;
    FGlyph.OnChange := @OnChangeGlyph;
    FGlyphMargin := 5;
    FStyle := bbtButton;
    FStaticButton := False;
    FActiveButt := bbtButton;
    FGlobalOpacity := 255;
    FTextApplyGlobalOpacity := False;
    FStates := [];
    FUpdateCount := 0;
    FDown := False;

    { Some default theme }
    FBodyNormal.FGradient2.StartColor := $00C87511;
    FBodyNormal.FGradient2.EndColor := $00EFE6D2;
    FBodyHover.FGradient2.StartColor := $00C87511;
    FBodyHover.FGradient2.EndColor := $00EFE6D2;
    FBodyClicked.FGradient2.StartColor := $00C87511;
    FBodyClicked.FGradient2.EndColor := $00EFE6D2;
    FBodyHover.FBorderColor := $00D7B697;
  finally
    Exclude(FControlState, csCreating);
    EnableAutoSizing;
    EndUpdate;
  end;
end;

destructor TCustomBGRAButton.Destroy;
begin
  FBodyNormal.Free;
  FBodyHover.Free;
  FBodyClicked.Free;
  FBGRANormal.Free;
  FBGRAHover.Free;
  FBGRAClick.Free;
  FBorderStyle.Free;
  FBorderStyleDropDown.Free;
  FreeThenNil(FGlyph);
  FreeThenNil(FBGRANormalA);
  FreeThenNil(FBGRAHoverA);
  FreeThenNil(FBGRAClickA);
  inherited Destroy;
end;

procedure TCustomBGRAButton.Assign(Source: TPersistent);
begin
  if Source is TCustomBGRAButton then
  begin
    FBorderWidth := TCustomBGRAButton(Source).FBorderWidth;
    Glyph := TCustomBGRAButton(Source).Glyph;
    FGlyphMargin := TCustomBGRAButton(Source).FGlyphMargin;
    FRoundX := TCustomBGRAButton(Source).FRoundX;
    FRoundY := TCustomBGRAButton(Source).FRoundY;
    FTextAlign := TCustomBGRAButton(Source).FTextAlign;
    FTextCanvas := TCustomBGRAButton(Source).FTextCanvas;
    FTextVAlign := TCustomBGRAButton(Source).FTextVAlign;
    FTextShadow := TCustomBGRAButton(Source).FTextShadow;
    FTextShadowColor := TCustomBGRAButton(Source).FTextShadowColor;
    FTextShadowColorOpacity := TCustomBGRAButton(Source).FTextShadowColorOpacity;
    FTextShadowOffsetX := TCustomBGRAButton(Source).FTextShadowOffsetX;
    FTextShadowOffsetY := TCustomBGRAButton(Source).FTextShadowOffsetY;
    FTextShadowRadius := TCustomBGRAButton(Source).FTextShadowRadius;
    FStyle := TCustomBGRAButton(Source).FStyle;
    FFlipArrow := TCustomBGRAButton(Source).FFlipArrow;
    FStaticButton := TCustomBGRAButton(Source).FStaticButton;
    FGlobalOpacity := TCustomBGRAButton(Source).FGlobalOpacity;
    FTextApplyGlobalOpacity := TCustomBGRAButton(Source).FTextApplyGlobalOpacity;
    FBorderStyle.Assign(TCustomBGRAButton(Source).FBorderStyle);
    FBorderStyleDropDown.Assign(TCustomBGRAButton(Source).FBorderStyle);
    FBodyNormal.Assign(TCustomBGRAButton(Source).FBodyNormal);
    FBodyHover.Assign(TCustomBGRAButton(Source).FBodyHover);
    FBodyClicked.Assign(TCustomBGRAButton(Source).FBodyClicked);

    Invalidate;
    UpdateSize;
  end
  else
    inherited Assign(Source);
end;

procedure TCustomBGRAButton.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TCustomBGRAButton.EndUpdate;
begin
  if FUpdateCount > 0 then
    Dec(FUpdateCount);
  if FUpdateCount = 0 then
    Prepare;
end;

procedure TCustomBGRAButton.SetSizeVariables(newArrowSize, newArrowSpace,
  newAutoSizeExtraVertical, newAutoSizeExtraHorizontal: integer);
begin
  ARR_SIZE := newArrowSize;
  ARR_SPACE := newArrowSpace;
  AutoSizeExtraY := newAutoSizeExtraVertical;
  AutoSizeExtraX := newAutoSizeExtraHorizontal;

  if csCreating in ControlState then
    Exit;

  Invalidate;
  UpdateSize;
end;

end.
