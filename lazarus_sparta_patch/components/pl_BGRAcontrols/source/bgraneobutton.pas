{
BGRANeoButton 2011 - 2012 by Lainz

This is a control that can be modified to create your own graphic button.
By default it has very limited functionality, but you can extend the code to
get the result you want.

* TGraphicButton *
TGraphicControl descendant. It handles mouse events and
ModalResult. It uses TGraphicButtonState to indicate the current button stage.

* TBGRANeoTextStyle *
BGRABitmap font style and alpha.

* TBGRANeoShadowStyle *
Usefull to general shadow purposes.

* TBGRANeoGradientStyle *
Gradient with alpha.

* TBGRANeoBorderStyle *
Round rectangle options, inner and outer border.
It uses TBGRANeoRoundRectangleOption.

* TBGRANeoButtonStyle *
A combination of TBGRANeoTextStyle, TBGRANeoShadowStyle, TBGRANeoGradientStyle
and TBGRANeoBorderStyle. Is the style of each TBGRANeoButton states.

* TCustomBGRANeoButton *
A button that can be overriden. TSampleBGRANeoButton show how
to do that. With 4 states, each of them read the style properties
from TBGRANeoButtonStyle.

* TBGRANeoButton *
TSampleBGRANeoButton descendant. It show how to publish all the properties that
are visible in the object inspector.
}

unit BGRANeoButton;

{$mode objfpc}{$H+}
{off $DEFINE DEBUG}

interface

uses
  Classes, LResources, Forms, Controls, Graphics, Dialogs, types,
  BGRABitmap, BGRABitmapTypes, BGRAPolygon, BGRAGradients;

type
  TGraphicButtonState = (gbsNormal, gbsHover, gbsActive);
  TBGRANeoRoundRectangleOption = (rroRound, rroBevel, rroSquare);

type

  { TGraphicButton }

  TGraphicButton = class(TGraphicControl)
  protected
    FState: TGraphicButtonState;
    FModalResult: TModalResult;
  protected
    procedure DoClick; virtual;
    procedure DoMouseDown; virtual;
    procedure DoMouseUp; virtual;
    procedure DoMouseEnter; virtual;
    procedure DoMouseLeave; virtual;
  protected
    procedure Click; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
  public
    property ModalResult: TModalResult
      read FModalResult write FModalResult default mrNone;
  end;

  { TBGRANeoShadowStyle }

  TBGRANeoShadowStyle = class(TPersistent)
  protected
    FOwner: TControl;
    FColor: TColor;
    FAlpha: byte;
    FOffsetX: integer;
    FOffsetY: integer;
    FRadius: integer;
  protected
    procedure SetFAlpha(AValue: byte); virtual;
    procedure SetFColor(AValue: TColor); virtual;
    procedure SetFOffsetX(AValue: integer); virtual;
    procedure SetFOffsetY(AValue: integer); virtual;
    procedure SetFRadius(AValue: integer); virtual;
  public
    constructor Create(AOwner: TControl);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Color: TColor read FColor write SetFColor default clWhite;
    property Alpha: byte read FAlpha write SetFAlpha default 255;
    property OffsetX: integer read FOffsetX write SetFOffsetX default 0;
    property OffsetY: integer read FOffsetY write SetFOffsetY default 0;
    property Radius: integer read FRadius write SetFRadius default 0;
  end;

  { TBGRANeoTextStyle }

  TBGRANeoTextStyle = class(TPersistent)
  protected
    FOwner: TControl;
    FFont: TFont;
    FAlpha: byte;
    FQuality: TBGRAFontQuality;
  protected
    procedure SetFAlpha(AValue: byte); virtual;
    procedure SetFFont(AValue: TFont); virtual;
    procedure SetFQuality(AValue: TBGRAFontQuality); virtual;
  public
    constructor Create(AOwner: TControl);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Font: TFont read FFont write SetFFont;
    property Alpha: byte read FAlpha write SetFAlpha default 255;
    property Quality: TBGRAFontQuality read FQuality write SetFQuality default
      fqSystemClearType;
  end;

  { TBGRANeoGradientStyle }

  TBGRANeoGradientStyle = class(TPersistent)
  protected
    FOwner: TControl;
    FColor1: TColor;
    FColor1Alpha: byte;
    FColor2: TColor;
    FColor2Alpha: byte;
    FStyle: TGradientType;
    FSinus: boolean;
  protected
    procedure SetFColor1(AValue: TColor); virtual;
    procedure SetFColor1Alpha(AValue: byte); virtual;
    procedure SetFColor2(AValue: TColor); virtual;
    procedure SetFColor2Alpha(AValue: byte); virtual;
    procedure SetFSinus(AValue: boolean); virtual;
    procedure SetFStyle(AValue: TGradientType); virtual;
  public
    constructor Create(AOwner: TControl);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Color1: TColor read FColor1 write SetFColor1 default clWhite;
    property Color1Alpha: byte read FColor1Alpha write SetFColor1Alpha default 255;
    property Color2: TColor read FColor2 write SetFColor2 default clWhite;
    property Color2Alpha: byte read FColor1Alpha write SetFColor2Alpha default 255;
    property Style: TGradientType read FStyle write SetFStyle default gtLinear;
    property Sinus: boolean read FSinus write SetFSinus default False;
  end;

  { TBGRANeoBorderStyle }

  TBGRANeoBorderStyle = class(TPersistent)
  protected
    FOwner: TControl;
    FInnerColor: TColor;
    FInnerAlpha: byte;
    FInnerWidth: single;
    FOuterColor: TColor;
    FOuterAlpha: byte;
    FOuterWidth: single;
    FRoundX: single;
    FRoundY: single;
    FTopLeft, FTopRight, FBottomLeft, FBottomRight: TBGRANeoRoundRectangleOption;
  protected
    procedure SetFBottomLeft(AValue: TBGRANeoRoundRectangleOption); virtual;
    procedure SetFBottomRight(AValue: TBGRANeoRoundRectangleOption); virtual;
    procedure SetFInnerAlpha(AValue: byte); virtual;
    procedure SetFInnerColor(AValue: TColor); virtual;
    procedure SetFInnerWidth(AValue: single); virtual;
    procedure SetFOuterAlpha(AValue: byte); virtual;
    procedure SetFOuterColor(AValue: TColor); virtual;
    procedure SetFOuterWidth(AValue: single); virtual;
    procedure SetFRoundX(AValue: single); virtual;
    procedure SetFRoundY(AValue: single); virtual;
    procedure SetFTopLeft(AValue: TBGRANeoRoundRectangleOption); virtual;
    procedure SetFTopRight(AValue: TBGRANeoRoundRectangleOption); virtual;
  public
    constructor Create(AOwner: TControl);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function Options: TRoundRectangleOptions;
  published
    property InnerColor: TColor read FInnerColor write SetFInnerColor default clWhite;
    property InnerColorAlpha: byte read FInnerAlpha write SetFInnerAlpha default 255;
    property InnerWidth: single read FInnerWidth write SetFInnerWidth default 0;
    property OuterColor: TColor read FOuterColor write SetFOuterColor default clWhite;
    property OuterColorAlpha: byte read FOuterAlpha write SetFOuterAlpha default 255;
    property OuterWidth: single read FOuterWidth write SetFOuterWidth default 0;
    property RoundX: single read FRoundX write SetFRoundX default 0;
    property RoundY: single read FRoundY write SetFRoundY default 0;
    property TopLeft: TBGRANeoRoundRectangleOption
      read FTopLeft write SetFTopLeft default rroRound;
    property TopRight: TBGRANeoRoundRectangleOption
      read FTopRight write SetFTopRight default rroRound;
    property BottomLeft: TBGRANeoRoundRectangleOption
      read FBottomLeft write SetFBottomLeft default rroRound;
    property BottomRight: TBGRANeoRoundRectangleOption
      read FBottomRight write SetFBottomRight default rroRound;
  end;

  { TBGRANeoButtonStyle }

  TBGRANeoButtonStyle = class(TPersistent)
  protected
    FOwner: TControl;
    FText: TBGRANeoTextStyle;
    FShadow: TBGRANeoShadowStyle;
    FGradient1: TBGRANeoGradientStyle;
    FGradient2: TBGRANeoGradientStyle;
    FGradientPosition: single;
    FBorder: TBGRANeoBorderStyle;
  protected
    procedure SetFBorder(AValue: TBGRANeoBorderStyle); virtual;
    procedure SetFGradient1(AValue: TBGRANeoGradientStyle); virtual;
    procedure SetFGradient2(AValue: TBGRANeoGradientStyle); virtual;
    procedure SetFGradientPosition(AValue: single); virtual;
    procedure SetFShadow(AValue: TBGRANeoShadowStyle); virtual;
    procedure SetFText(AValue: TBGRANeoTextStyle); virtual;
  public
    constructor Create(AOwner: TControl);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Text: TBGRANeoTextStyle read FText write SetFText;
    property Shadow: TBGRANeoShadowStyle read FShadow write SetFShadow;
    property Gradient1: TBGRANeoGradientStyle read FGradient1 write SetFGradient1;
    property Gradient2: TBGRANeoGradientStyle read FGradient2 write SetFGradient2;
    property GradientPosition: single read FGradientPosition write SetFGradientPosition;
    property Border: TBGRANeoBorderStyle read FBorder write SetFBorder;
  end;

  { TCustomBGRANeoButton }

  TCustomBGRANeoButton = class(TGraphicButton)
  protected
    FBGRA: TBGRABitmap;
    FStyleNormal, FStyleHover, FStyleActive, FStyleDisabled: TBGRANeoButtonStyle;
    {$IFDEF DEBUG}
    FPaintCount: integer;
    {$ENDIF}
  protected
    procedure SetFStyleActive(AValue: TBGRANeoButtonStyle); virtual;
    procedure SetFStyleDisabled(AValue: TBGRANeoButtonStyle); virtual;
    procedure SetFStyleHover(AValue: TBGRANeoButtonStyle); virtual;
    procedure SetFStyleNormal(AValue: TBGRANeoButtonStyle); virtual;
  protected
    procedure DrawButton; virtual;
    procedure DrawBody(AStyle: TBGRANeoButtonStyle); virtual;
    procedure DrawText(AStyle: TBGRANeoButtonStyle); virtual;
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property StyleNormal: TBGRANeoButtonStyle read FStyleNormal write SetFStyleNormal;
    property StyleHover: TBGRANeoButtonStyle read FStyleHover write SetFStyleHover;
    property StyleActive: TBGRANeoButtonStyle read FStyleActive write SetFStyleActive;
    property StyleDisabled: TBGRANeoButtonStyle
      read FStyleDisabled write SetFStyleDisabled;
  end;

  { TSampleBGRANeoButton }

  TSampleBGRANeoButton = class(TCustomBGRANeoButton)
  protected
    procedure DrawBody(AStyle: TBGRANeoButtonStyle); override;
    procedure DrawText(AStyle: TBGRANeoButtonStyle); override;
  end;

  { TBGRANeoButton }

  TBGRANeoButton = class(TSampleBGRANeoButton)
  published
    property StyleNormal;
    property StyleHover;
    property StyleActive;
    property StyleDisabled;
  published
    property Action;
    property Align;
    property Anchors;
    property AutoSize;
    property BidiMode;
    property BorderSpacing;
    property Caption;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ModalResult;
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDrag;
    property ParentBidiMode;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
  end;

implementation

uses sysutils;

{ TSampleBGRANeoButton }

procedure TSampleBGRANeoButton.DrawBody(AStyle: TBGRANeoButtonStyle);

  procedure DrawBodyStyle(bitmap: TBGRABitmap;
    cl1, cl2, cl3, cl4, border, light: TBGRAPixel; rx, ry, w, Value: single;
    dir1, dir2, dir3: TGradientDirection; options: TRoundRectangleOptions = [];
    drawlight: boolean = True);
  var
    texture: TBGRABitmap;
    w2, b2: single;
  begin
    texture := DoubleGradientAlphaFill(Rect(0, 0, bitmap.Width, bitmap.Height),
      cl1, cl2, cl3, cl4, dir1, dir2, dir3, Value);

    b2 := 1;
    w2 := w * 2 - 1;

    FillRoundRectangleAntialiasWithTexture(bitmap, Trunc(w / 2),
      Trunc(w / 2), bitmap.Width - b2 - Trunc(w / 2),
      bitmap.Height - b2 - Trunc(w / 2), rx, ry, options, texture);
    texture.Free;

    if drawlight then
      BorderRoundRectangleAntialias(bitmap, w, w,
        bitmap.Width - b2 - w,
        bitmap.Height - b2 - w, rx, ry, w2, options, light, False);

    BorderRoundRectangleAntialias(bitmap, Trunc(w / 2), Trunc(w / 2),
      bitmap.Width - b2 - Trunc(w / 2),
      bitmap.Height - b2 - Trunc(w / 2), rx, ry, w, options, border, False);
  end;

begin
  FBGRA.Fill(BGRAPixelTransparent);
  DrawBodyStyle(FBGRA,
    ColorToBGRA(AStyle.Gradient1.Color1, AStyle.Gradient1.Color1Alpha),
    ColorToBGRA(AStyle.Gradient1.Color2, AStyle.Gradient1.Color2Alpha),
    ColorToBGRA(AStyle.Gradient2.Color1, AStyle.Gradient2.Color1Alpha),
    ColorToBGRA(AStyle.Gradient2.Color2, AStyle.Gradient2.Color2Alpha),
    ColorToBGRA(AStyle.Border.OuterColor, AStyle.Border.OuterColorAlpha),
    ColorToBGRA(AStyle.Border.InnerColor, AStyle.Border.InnerColorAlpha),
    AStyle.Border.RoundX, AStyle.Border.RoundY, AStyle.Border.OuterWidth,
    AStyle.GradientPosition,
    gdVertical, gdVertical, gdVertical, AStyle.Border.Options);
end;

procedure TSampleBGRANeoButton.DrawText(AStyle: TBGRANeoButtonStyle);
var
  bmpSdw: TBGRABitmap;
  OutTxtSize: TSize;
  OutX, OutY: integer;
begin
  if (AStyle.Shadow.Alpha = 0) and (AStyle.Text.Alpha = 0) then
    exit;

  FBGRA.FontAntialias := True;
  FBGRA.FontHeight := AStyle.Text.Font.Height;
  FBGRA.FontStyle := AStyle.Text.Font.Style;
  FBGRA.FontName := AStyle.Text.Font.Name;
  FBGRA.FontQuality := AStyle.Text.Quality;

  OutTxtSize := FBGRA.TextSize(Caption);
  OutX := Round(Width / 2) - Round(OutTxtSize.cx / 2);
  OutY := Round(Height / 2) - Round(OutTxtSize.cy / 2);

  if AStyle.Shadow.Alpha > 0 then
  begin
    bmpSdw := TBGRABitmap.Create(OutTxtSize.cx + 2 * AStyle.Shadow.Radius,
      OutTxtSize.cy + 2 * AStyle.Shadow.Radius);

    bmpSdw.FontAntialias := True;
    bmpSdw.FontHeight := AStyle.Text.Font.Height;
    bmpSdw.FontStyle := AStyle.Text.Font.Style;
    bmpSdw.FontName := AStyle.Text.Font.Name;
    bmpSdw.FontQuality := AStyle.Text.Quality;

    bmpSdw.TextOut(AStyle.Shadow.Radius, AStyle.Shadow.Radius,
      Caption, ColorToBGRA(AStyle.Shadow.Color, AStyle.Shadow.Alpha));

    BGRAReplace(bmpSdw, bmpSdw.FilterBlurRadial(AStyle.Shadow.Radius, rbFast));

    FBGRA.PutImage(OutX + AStyle.Shadow.OffsetX - AStyle.Shadow.Radius,
      OutY + AStyle.Shadow.OffsetY - AStyle.Shadow.Radius, bmpSdw,
      dmDrawWithTransparency);

    bmpSdw.Free;
  end;

  if AStyle.Text.Alpha > 0 then
    FBGRA.TextOut(OutX, OutY, Caption, ColorToBGRA(AStyle.Text.Font.Color,
      AStyle.Text.Alpha));
end;

{ TBGRANeoButtonStyle }

procedure TBGRANeoButtonStyle.SetFBorder(AValue: TBGRANeoBorderStyle);
begin
  if FBorder = AValue then
    Exit;
  FBorder := AValue;
end;

procedure TBGRANeoButtonStyle.SetFGradient1(AValue: TBGRANeoGradientStyle);
begin
  if FGradient1 = AValue then
    Exit;
  FGradient1 := AValue;
end;

procedure TBGRANeoButtonStyle.SetFGradient2(AValue: TBGRANeoGradientStyle);
begin
  if FGradient2 = AValue then
    Exit;
  FGradient2 := AValue;
end;

procedure TBGRANeoButtonStyle.SetFGradientPosition(AValue: single);
begin
  if FGradientPosition = AValue then
    Exit;
  FGradientPosition := AValue;
end;

procedure TBGRANeoButtonStyle.SetFShadow(AValue: TBGRANeoShadowStyle);
begin
  if FShadow = AValue then
    Exit;
  FShadow := AValue;
end;

procedure TBGRANeoButtonStyle.SetFText(AValue: TBGRANeoTextStyle);
begin
  if FText = AValue then
    Exit;
  FText := AValue;
end;

constructor TBGRANeoButtonStyle.Create(AOwner: TControl);
begin
  inherited Create;
  FOwner := AOwner;
  FText := TBGRANeoTextStyle.Create(FOwner);
  FShadow := TBGRANeoShadowStyle.Create(FOwner);
  FGradient1 := TBGRANeoGradientStyle.Create(FOwner);
  FGradient2 := TBGRANeoGradientStyle.Create(FOwner);
  FGradientPosition := 0.5;
  FBorder := TBGRANeoBorderStyle.Create(FOwner);
end;

destructor TBGRANeoButtonStyle.Destroy;
begin
  FText.Free;
  FShadow.Free;
  FGradient1.Free;
  FGradient2.Free;
  FBorder.Free;
  inherited Destroy;
end;

procedure TBGRANeoButtonStyle.Assign(Source: TPersistent);
begin
  if Source is TBGRANeoButtonStyle then
  begin
    FText.Assign(TBGRANeoButtonStyle(Source).Text);
    FShadow.Assign(TBGRANeoButtonStyle(Source).Shadow);
    FGradient1.Assign(TBGRANeoButtonStyle(Source).Gradient1);
    FGradient2.Assign(TBGRANeoButtonStyle(Source).Gradient2);
    FGradientPosition := TBGRANeoButtonStyle(Source).GradientPosition;
    FBorder.Assign(TBGRANeoButtonStyle(Source).Border);
  end
  else
    inherited Assign(Source);
end;

{ TBGRANeoBorderStyle }

procedure TBGRANeoBorderStyle.SetFBottomLeft(AValue: TBGRANeoRoundRectangleOption);
begin
  if FBottomLeft = AValue then
    Exit;
  FBottomLeft := AValue;
end;

procedure TBGRANeoBorderStyle.SetFBottomRight(AValue: TBGRANeoRoundRectangleOption);
begin
  if FBottomRight = AValue then
    Exit;
  FBottomRight := AValue;
end;

procedure TBGRANeoBorderStyle.SetFInnerAlpha(AValue: byte);
begin
  if FInnerAlpha = AValue then
    Exit;
  FInnerAlpha := AValue;
end;

procedure TBGRANeoBorderStyle.SetFInnerColor(AValue: TColor);
begin
  if FInnerColor = AValue then
    Exit;
  FInnerColor := AValue;
end;

procedure TBGRANeoBorderStyle.SetFInnerWidth(AValue: single);
begin
  if FInnerWidth = AValue then
    Exit;
  FInnerWidth := AValue;
end;

procedure TBGRANeoBorderStyle.SetFOuterAlpha(AValue: byte);
begin
  if FOuterAlpha = AValue then
    Exit;
  FOuterAlpha := AValue;
end;

procedure TBGRANeoBorderStyle.SetFOuterColor(AValue: TColor);
begin
  if FOuterColor = AValue then
    Exit;
  FOuterColor := AValue;
end;

procedure TBGRANeoBorderStyle.SetFOuterWidth(AValue: single);
begin
  if FOuterWidth = AValue then
    Exit;
  FOuterWidth := AValue;
end;

procedure TBGRANeoBorderStyle.SetFRoundX(AValue: single);
begin
  if FRoundX = AValue then
    Exit;
  FRoundX := AValue;
end;

procedure TBGRANeoBorderStyle.SetFRoundY(AValue: single);
begin
  if FRoundY = AValue then
    Exit;
  FRoundY := AValue;
end;

procedure TBGRANeoBorderStyle.SetFTopLeft(AValue: TBGRANeoRoundRectangleOption);
begin
  if FTopLeft = AValue then
    Exit;
  FTopLeft := AValue;
end;

procedure TBGRANeoBorderStyle.SetFTopRight(AValue: TBGRANeoRoundRectangleOption);
begin
  if FTopRight = AValue then
    Exit;
  FTopRight := AValue;
end;

constructor TBGRANeoBorderStyle.Create(AOwner: TControl);
begin
  inherited Create;
  FOwner := AOwner;
  FInnerColor := clWhite;
  FInnerAlpha := 255;
  FInnerWidth := 0;
  FOuterColor := clWhite;
  FOuterAlpha := 255;
  FOuterWidth := 0;
  FRoundX := 0;
  FRoundY := 0;
  FTopLeft := rroRound;
  FTopRight := rroRound;
  FBottomLeft := rroRound;
  FBottomRight := rroRound;
end;

destructor TBGRANeoBorderStyle.Destroy;
begin
  inherited Destroy;
end;

procedure TBGRANeoBorderStyle.Assign(Source: TPersistent);
begin
  if Source is TBGRANeoBorderStyle then
  begin
    FInnerColor := TBGRANeoBorderStyle(Source).InnerColor;
    FInnerAlpha := TBGRANeoBorderStyle(Source).InnerColorAlpha;
    FInnerWidth := TBGRANeoBorderStyle(Source).InnerWidth;
    FOuterColor := TBGRANeoBorderStyle(Source).OuterColor;
    FOuterAlpha := TBGRANeoBorderStyle(Source).OuterColorAlpha;
    FOuterWidth := TBGRANeoBorderStyle(Source).OuterWidth;
    FRoundX := TBGRANeoBorderStyle(Source).RoundX;
    FRoundY := TBGRANeoBorderStyle(Source).RoundY;
    FTopLeft := TBGRANeoBorderStyle(Source).TopLeft;
    FTopRight := TBGRANeoBorderStyle(Source).TopRight;
    FBottomLeft := TBGRANeoBorderStyle(Source).BottomLeft;
    FBottomRight := TBGRANeoBorderStyle(Source).BottomRight;
  end
  else
    inherited Assign(Source);
end;

function TBGRANeoBorderStyle.Options: TRoundRectangleOptions;
var
  tl, tr, br, bl: TRoundRectangleOptions;
begin
  case TopLeft of
    rroBevel: tl := [rrTopLeftBevel];
    rroSquare: tl := [rrTopLeftSquare];
    rroRound: tl := [];
  end;

  case TopRight of
    rroBevel: tr := [rrTopRightBevel];
    rroSquare: tr := [rrTopRightSquare];
    rroRound: tr := [];
  end;

  case BottomRight of
    rroBevel: br := [rrBottomRightBevel];
    rroSquare: br := [rrBottomRightSquare];
    rroRound: br := [];
  end;

  case BottomLeft of
    rroBevel: bl := [rrBottomLeftBevel];
    rroSquare: bl := [rrBottomLeftSquare];
    rroRound: bl := [];
  end;

  Result := tl + tr + br + bl;
end;

{ TBGRANeoGradientStyle }

procedure TBGRANeoGradientStyle.SetFColor1(AValue: TColor);
begin
  if FColor1 = AValue then
    Exit;
  FColor1 := AValue;
end;

procedure TBGRANeoGradientStyle.SetFColor1Alpha(AValue: byte);
begin
  if FColor1Alpha = AValue then
    Exit;
  FColor1Alpha := AValue;
end;

procedure TBGRANeoGradientStyle.SetFColor2(AValue: TColor);
begin
  if FColor2 = AValue then
    Exit;
  FColor2 := AValue;
end;

procedure TBGRANeoGradientStyle.SetFColor2Alpha(AValue: byte);
begin
  if FColor1Alpha = AValue then
    Exit;
  FColor1Alpha := AValue;
end;

procedure TBGRANeoGradientStyle.SetFSinus(AValue: boolean);
begin
  if FSinus = AValue then
    Exit;
  FSinus := AValue;
end;

procedure TBGRANeoGradientStyle.SetFStyle(AValue: TGradientType);
begin
  if FStyle = AValue then
    Exit;
  FStyle := AValue;
end;

constructor TBGRANeoGradientStyle.Create(AOwner: TControl);
begin
  inherited Create;
  FOwner := AOwner;
  FColor1 := clWhite;
  FColor1Alpha := 255;
  FColor2 := clWhite;
  FColor2Alpha := 255;
  FStyle := gtLinear;
  FSinus := False;
end;

destructor TBGRANeoGradientStyle.Destroy;
begin
  inherited Destroy;
end;

procedure TBGRANeoGradientStyle.Assign(Source: TPersistent);
begin
  if Source is TBGRANeoGradientStyle then
  begin
    FColor1 := TBGRANeoGradientStyle(Source).Color1;
    FColor1Alpha := TBGRANeoGradientStyle(Source).Color1Alpha;
    FColor2 := TBGRANeoGradientStyle(Source).Color2;
    FColor2Alpha := TBGRANeoGradientStyle(Source).Color2Alpha;
    FStyle := TBGRANeoGradientStyle(Source).Style;
    FSinus := TBGRANeoGradientStyle(Source).Sinus;
  end
  else
    inherited Assign(Source);
end;

{ TBGRANeoTextStyle }

procedure TBGRANeoTextStyle.SetFAlpha(AValue: byte);
begin
  if FAlpha = AValue then
    Exit;
  FAlpha := AValue;
end;

procedure TBGRANeoTextStyle.SetFFont(AValue: TFont);
begin
  if FFont.IsEqual(AValue) then
    exit;
  FFont.Assign(AValue);
end;

procedure TBGRANeoTextStyle.SetFQuality(AValue: TBGRAFontQuality);
begin
  if FQuality = AValue then
    Exit;
  FQuality := AValue;
end;

constructor TBGRANeoTextStyle.Create(AOwner: TControl);
begin
  inherited Create;
  FOwner := AOwner;
  FAlpha := 255;
  FFont := TFont.Create;
  FQuality := fqSystemClearType;
end;

destructor TBGRANeoTextStyle.Destroy;
begin
  FFont.Free;
  inherited Destroy;
end;

procedure TBGRANeoTextStyle.Assign(Source: TPersistent);
begin
  if Source is TBGRANeoTextStyle then
  begin
    FAlpha := TBGRANeoTextStyle(Source).Alpha;
    FFont.Assign(TBGRANeoTextStyle(Source).Font);
    FQuality := TBGRANeoTextStyle(Source).Quality;
  end
  else
    inherited Assign(Source);
end;

{ TBGRANeoShadowStyle }

procedure TBGRANeoShadowStyle.SetFAlpha(AValue: byte);
begin
  if FAlpha = AValue then
    Exit;
  FAlpha := AValue;
end;

procedure TBGRANeoShadowStyle.SetFColor(AValue: TColor);
begin
  if FColor = AValue then
    Exit;
  FColor := AValue;
end;

procedure TBGRANeoShadowStyle.SetFOffsetX(AValue: integer);
begin
  if FOffsetX = AValue then
    Exit;
  FOffsetX := AValue;
end;

procedure TBGRANeoShadowStyle.SetFOffsetY(AValue: integer);
begin
  if FOffsetY = AValue then
    Exit;
  FOffsetY := AValue;
end;

procedure TBGRANeoShadowStyle.SetFRadius(AValue: integer);
begin
  if FRadius = AValue then
    Exit;
  FRadius := AValue;
end;

constructor TBGRANeoShadowStyle.Create(AOwner: TControl);
begin
  inherited Create;
  FOwner := AOwner;
  FColor := clWhite;
  FAlpha := 255;
  FOffsetX := 0;
  FOffsetY := 0;
  FRadius := 0;
end;

destructor TBGRANeoShadowStyle.Destroy;
begin
  inherited Destroy;
end;

procedure TBGRANeoShadowStyle.Assign(Source: TPersistent);
begin
  if Source is TBGRANeoShadowStyle then
  begin
    FColor := TBGRANeoShadowStyle(Source).Color;
    FAlpha := TBGRANeoShadowStyle(Source).Alpha;
    FOffsetX := TBGRANeoShadowStyle(Source).OffsetX;
    FOffsetY := TBGRANeoShadowStyle(Source).OffsetY;
    FRadius := TBGRANeoShadowStyle(Source).Radius;
  end
  else
    inherited Assign(Source);
end;

{ TCustomBGRANeoButton }

procedure TCustomBGRANeoButton.SetFStyleActive(AValue: TBGRANeoButtonStyle);
begin
  if FStyleActive = AValue then
    Exit;
  FStyleActive := AValue;
end;

procedure TCustomBGRANeoButton.SetFStyleDisabled(AValue: TBGRANeoButtonStyle);
begin
  if FStyleDisabled = AValue then
    Exit;
  FStyleDisabled := AValue;
end;

procedure TCustomBGRANeoButton.SetFStyleHover(AValue: TBGRANeoButtonStyle);
begin
  if FStyleHover = AValue then
    Exit;
  FStyleHover := AValue;
end;

procedure TCustomBGRANeoButton.SetFStyleNormal(AValue: TBGRANeoButtonStyle);
begin
  if FStyleNormal = AValue then
    Exit;
  FStyleNormal := AValue;
end;

procedure TCustomBGRANeoButton.DrawButton;
begin
  if Enabled then
    case FState of
      gbsNormal:
      begin
        DrawBody(StyleNormal);
        DrawText(StyleNormal);
      end;
      gbsHover:
      begin
        DrawBody(StyleHover);
        DrawText(StyleHover);
      end;
      gbsActive:
      begin
        DrawBody(StyleActive);
        DrawText(StyleActive);
      end;
    end
  else
  begin
    DrawBody(StyleDisabled);
    DrawText(StyleDisabled);
  end;
end;

procedure TCustomBGRANeoButton.DrawBody(AStyle: TBGRANeoButtonStyle);
begin
  // nothing here
end;

procedure TCustomBGRANeoButton.DrawText(AStyle: TBGRANeoButtonStyle);
begin
  // nothing here
end;

procedure TCustomBGRANeoButton.Paint;
begin
  FBGRA.SetSize(Width, Height);
  DrawButton;

  {$IFDEF DEBUG}
  Inc(FPaintCount);
  FBGRA.TextOut(0, 0, IntToStr(FPaintCount), BGRAWhite);
  FBGRA.TextOut(1, 1, IntToStr(FPaintCount), BGRABlack);
  {$ENDIF}

  FBGRA.Draw(Canvas, 0, 0, False);
end;

constructor TCustomBGRANeoButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBGRA := TBGRABitmap.Create;
  FStyleNormal := TBGRANeoButtonStyle.Create(Self);
  FStyleHover := TBGRANeoButtonStyle.Create(Self);
  FStyleActive := TBGRANeoButtonStyle.Create(Self);
  FStyleDisabled := TBGRANeoButtonStyle.Create(Self);
end;

destructor TCustomBGRANeoButton.Destroy;
begin
  FBGRA.Free;
  FStyleNormal.Free;
  FStyleHover.Free;
  FStyleActive.Free;
  FStyleDisabled.Free;
  inherited Destroy;
end;

procedure TCustomBGRANeoButton.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
end;

{ TGraphicButton }

procedure TGraphicButton.DoClick;
var
  Form: TCustomForm;
begin
  if ModalResult <> mrNone then
  begin
    Form := GetParentForm(Self);
    if Form <> nil then
      Form.ModalResult := ModalResult;
  end;
end;

procedure TGraphicButton.DoMouseDown;
var
  NewState: TGraphicButtonState;
begin
  NewState := gbsActive;

  if NewState <> FState then
  begin
    FState := NewState;
    Invalidate;
  end;
end;

procedure TGraphicButton.DoMouseUp;
var
  NewState: TGraphicButtonState;
  p: TPoint;
begin
  p := ScreenToClient(Mouse.CursorPos);

  if (p.x >= 0) and (p.x <= Width) and (p.y >= 0) and (p.y <= Height) then
    NewState := gbsHover
  else
    NewState := gbsNormal;

  if NewState <> FState then
  begin
    FState := NewState;
    Invalidate;
  end;
end;

procedure TGraphicButton.DoMouseEnter;
var
  NewState: TGraphicButtonState;
begin
  if Enabled then
    NewState := gbsHover
  else
  begin
    FState := gbsNormal;
    NewState := FState;
  end;

  if NewState <> FState then
  begin
    FState := NewState;
    Invalidate;
  end;
end;

procedure TGraphicButton.DoMouseLeave;
var
  NewState: TGraphicButtonState;
begin
  if Enabled then
    NewState := gbsNormal
  else
  begin
    FState := gbsNormal;
    NewState := FState;
  end;

  if NewState <> FState then
  begin
    FState := NewState;
    Invalidate;
  end;
end;

procedure TGraphicButton.Click;
begin
  DoClick;
  inherited Click;
end;

procedure TGraphicButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Button = mbLeft then
    DoMouseDown;
end;

procedure TGraphicButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  DoMouseUp;
end;

procedure TGraphicButton.MouseEnter;
begin
  inherited MouseEnter;
  DoMouseEnter;
end;

procedure TGraphicButton.MouseLeave;
begin
  inherited MouseLeave;
  DoMouseLeave;
end;

end.

