{ !! THIS CONTROL IS DEPRECATED! USE BCLABEL INSTEAD !!
  TBGRALabel 1.01v
  Description: Component TBGRALabel simulate TLabel
  This component is based on TBGRAButton of Dibo
  Create by Lucas Martín in 2011. codedeep at hotmail.com

  Functionality:
  - Caption with shadow or not
  - Full alpha and antialias support or not
  - WordWarp or not
  - Caption with Property Editor Multiline

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
unit BGRALabel;

{$mode objfpc}{$H+}

interface

uses
  Classes, LResources, Forms, Controls, Graphics, Dialogs, BGRABitmap, BGRABitmapTypes,
  Buttons, BGRAGradientScanner, LCLType, types, BCTypes;

type

  TBGRABackgroundStyle = (bbsClear, bbsColor, bbsGradient);

  { TBGRABackground }

  TBGRABackground = class(TPersistent)
  private
    FColor:        TColor;
    FColorOpacity: byte;
    FGradient1:    TBCGradient;
    FGradient1EndPercent: single;
    FGradient2:    TBCGradient;
    FOwner:        TControl;
    FStyle:        TBGRABackgroundStyle;
    FLightWidth:   integer;
    FLightOpacity: byte;
    procedure SetColor(const AValue: TColor);
    procedure SetColorOpacity(const AValue: byte);
    procedure SetGradient1(const AValue: TBCGradient);
    procedure SetGradient1EndPercent(const AValue: single);
    procedure SetGradient2(const AValue: TBCGradient);
    procedure SetLightOpacity(const AValue: byte);
    procedure SetLightWidth(const AValue: integer);
    procedure SetStyle(const AValue: TBGRABackgroundStyle);
  public
    constructor Create(AOwner: TControl);
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
  published
    //property Action;
    property Color: TColor Read FColor Write SetColor default clDefault;
    property ColorOpacity: byte Read FColorOpacity Write SetColorOpacity default 255;
    property Gradient1: TBCGradient Read FGradient1 Write SetGradient1;
    property Gradient2: TBCGradient Read FGradient2 Write SetGradient2;
    property Gradient1EndPercent: single Read FGradient1EndPercent
      Write SetGradient1EndPercent default 50;
    property Style: TBGRABackgroundStyle Read FStyle Write SetStyle default bbsClear;
    property LightWidth: integer Read FLightWidth Write SetLightWidth;
    property LightOpacity: byte Read FLightOpacity Write SetLightOpacity;
  end;

  { TBGRALabel }

  TBGRALabel = class(TGraphicControl)
  private
    { Private declarations }
    FBGRA, FBGRAText, FBGRATextShadow: TBGRABitmap;
    FBackground: TBGRABackground;
    FBorderWidth: integer;
    FMouseStage: TBCMouseState;
    FRoundX: integer;
    FRoundY: integer;
    FTextAlign: TBGRATextAlign;
    FTextAntialiasing: boolean;
    FTextShadow: boolean;
    FTextShadowAntialiasing: boolean;
    FTextShadowColor: TColor;
    FTextShadowColorOpacity: byte;
    FTextShadowOffsetX: integer;
    FTextShadowOffsetY: integer;
    FTextShadowRadius: integer;
    FTextVAlign: TBGRATextVAlign;
    FGlobalOpacity: byte;
    FTextApplyGlobalOpacity: boolean;
    FWordWrap: boolean;
    FFont: TFont;
    procedure OnChangeFont(Sender: TObject);
    procedure CalculateTextSize(MaxWidth: integer;
      var NeededWidth, NeededHeight: integer);
    procedure ConvertToGrayScale;
    procedure DrawBackground(ABackground: TBGRABackground; ARect: TRect);
    procedure DrawText(AFontColor: TColor; AFontStyle: TFontStyles = [];
      AFontName: string = 'Default');
    procedure SetBackground(const AValue: TBGRABackground);
    procedure SetBorderWidth(const AValue: integer);
    procedure SetRoundX(const AValue: integer);
    procedure SetRoundY(const AValue: integer);
    procedure SeTBGRATextAlign(const AValue: TBGRATextAlign);
    procedure SetTextShadow(const AValue: boolean);
    procedure SetTextAntialiasing(const AValue: boolean);
    procedure SetTextShadowAntialiasing(const AValue: boolean);
    procedure SetTextShadowColor(const AValue: TColor);
    procedure SetTextShadowColorOpacity(const AValue: byte);
    procedure SetTextShadowOffsetX(const AValue: integer);
    procedure SetTextShadowOffsetY(const AValue: integer);
    procedure SetTextShadowRadius(const AValue: integer);
    procedure SeTBGRATextVAlign(const AValue: TBGRATextVAlign);
    procedure SetGlobalOpacity(const AValue: byte);
    procedure SetTextApplyGlobalOpacity(const AValue: boolean);
    procedure UpdateSize;
    procedure SetFont(const AValue: TFont);
  protected
    { Protected declarations }
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer;
      WithThemeSpace: boolean); override;
    class function GetControlClassDefaultSize: TSize; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    procedure Paint; override;
    procedure Resize; override;
    procedure SetEnabled(Value: boolean); override;
    procedure TextChanged; override;
    procedure SetWordWrap(Value: boolean);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
  published
    { Published declarations }
    property Action;
    property AutoSize default True;
    property Align;
    property Anchors;
    property Background: TBGRABackground Read FBackground Write SetBackground;
    property BorderWidth: integer Read FBorderWidth Write SetBorderWidth default 1;
    property BorderSpacing;
    property Caption;
    property ParentColor;
    property Enabled;
    property PopupMenu;
    property RoundX: integer Read FRoundX Write SetRoundX default 1;
    property RoundY: integer Read FRoundY Write SetRoundY default 1;
    property TextAlign: TBGRATextAlign
      Read FTextAlign Write SeTBGRATextAlign default btaLeft;
    property TextVAlign: TBGRATextVAlign
      Read FTextVAlign Write SeTBGRATextVAlign default btvaTop;
    property TextAntialiasing: boolean Read FTextAntialiasing
      Write SetTextAntialiasing default True;
    property TextShadowAntialiasing: boolean
      Read FTextShadowAntialiasing Write SetTextShadowAntialiasing default True;
    property TextShadow: boolean Read FTextShadow Write SetTextShadow default True;
    property TextShadowColor: TColor Read FTextShadowColor
      Write SetTextShadowColor default clBlack;
    property TextShadowColorOpacity: byte Read FTextShadowColorOpacity
      Write SetTextShadowColorOpacity;
    property TextShadowOffsetX: integer Read FTextShadowOffsetX
      Write SetTextShadowOffsetX default 5;
    property TextShadowOffsetY: integer Read FTextShadowOffsetY
      Write SetTextShadowOffsetY default 5;
    property TextShadowRadius: integer Read FTextShadowRadius
      Write SetTextShadowRadius default 5;
    property GlobalOpacity: byte Read FGlobalOpacity Write SetGlobalOpacity;
    property TextApplyGlobalOpacity: boolean
      Read FTextApplyGlobalOpacity Write SetTextApplyGlobalOpacity;
    property Visible;
    property WordWrap: boolean Read FWordWrap Write SetWordWrap default False;
    property Font: TFont Read FFont Write SetFont;
  published
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;

  end;

implementation

uses LCLIntf, LCLProc, BGRAPolygon, BGRAFillInfo, propedits, BCTools;

 //const
 //ARR_SIZE = 8;
 //ARR_SPACE = ARR_SIZE + 8;


{ TBGRABackground }

procedure TBGRABackground.SetColor(const AValue: TColor);
begin
  if FColor = AValue then
    exit;
  FColor := AValue;

  FOwner.Invalidate;
end;

procedure TBGRABackground.SetColorOpacity(const AValue: byte);
begin
  if FColorOpacity = AValue then
    exit;
  FColorOpacity := AValue;

  FOwner.Invalidate;
end;

procedure TBGRABackground.SetGradient1(const AValue: TBCGradient);
begin
  if FGradient1 = AValue then
    exit;
  FGradient1.Assign(AValue);

  FOwner.Invalidate;
end;

procedure TBGRABackground.SetGradient1EndPercent(const AValue: single);
begin
  if FGradient1EndPercent = AValue then
    exit;
  FGradient1EndPercent := AValue;

  FOwner.Invalidate;
end;

procedure TBGRABackground.SetGradient2(const AValue: TBCGradient);
begin
  if FGradient2 = AValue then
    exit;
  FGradient2.Assign(AValue);

  FOwner.Invalidate;
end;

procedure TBGRABackground.SetLightOpacity(const AValue: byte);
begin
  if FLightOpacity = AValue then
    exit;
  FLightOpacity := AValue;

  FOwner.Invalidate;
end;

procedure TBGRABackground.SetLightWidth(const AValue: integer);
begin
  if FLightWidth = AValue then
    exit;
  FLightWidth := AValue;

  FOwner.Invalidate;
end;

procedure TBGRABackground.SetStyle(const AValue: TBGRABackgroundStyle);
begin
  if FStyle = AValue then
    exit;
  FStyle := AValue;

  FOwner.Invalidate;
end;

constructor TBGRABackground.Create(AOwner: TControl);
begin
  FOwner := AOwner;
  FColor := clBtnFace;
  FColorOpacity := 255;
  FGradient1 := TBCGradient.Create(FOwner);
  FGradient2 := TBCGradient.Create(FOwner);
  FGradient1EndPercent := 35;
  FStyle := bbsClear;
  FLightOpacity := 64;
  FLightWidth := 0;

  inherited Create;
end;

destructor TBGRABackground.Destroy;
begin
  FGradient1.Free;
  FGradient2.Free;
  inherited Destroy;
end;

procedure TBGRABackground.Assign(Source: TPersistent);
begin
  if Source is TBGRABackground then
  begin
    FColor := TBGRABackground(Source).FColor;
    FStyle := TBGRABackground(Source).FStyle;
    FGradient1EndPercent := TBGRABackground(Source).FGradient1EndPercent;
    FLightOpacity := TBGRABackground(Source).FLightOpacity;
    FLightWidth := TBGRABackground(Source).FLightWidth;
    FGradient1.Assign(TBGRABackground(Source).FGradient1);
    FGradient2.Assign(TBGRABackground(Source).FGradient2);

    FOwner.Invalidate;
    FOwner.InvalidatePreferredSize;
    FOwner.AdjustSize;
  end
  else
    inherited Assign(Source);
end;

{ TBGRALabel }

procedure TBGRALabel.SetFont(const AValue: TFont);
begin
  if FFont = AValue then
    exit;
  FFont.Assign(AValue);

  Invalidate;
  UpdateSize;
end;

procedure TBGRALabel.OnChangeFont(Sender: TObject);
begin
  Invalidate;
  UpdateSize;
end;

procedure TBGRALabel.CalculateTextSize(MaxWidth: integer;
  var NeededWidth, NeededHeight: integer);
var
  s: TSize;
begin
  if (Caption = '') or (FBGRA = nil) then
  begin
    NeededWidth  := 1;
    NeededHeight := 1;
    Exit;
  end;

  Canvas.Font := FFont;
  FBGRA.FontHeight := Canvas.TextHeight(Caption);
  FBGRA.FontStyle := FFont.Style;
  if FFont.Name = '' then
    FBGRA.FontName := 'default'
  else
    FBGRA.FontName := FFont.Name;
  s := FBGRA.TextSize(Caption);
  if FTextShadow then
  begin
    NeededWidth  := s.cx + Round(1.2 * FTextShadowRadius) + FTextShadowOffsetX;
    NeededHeight := s.cy + Round(1.2 * FTextShadowRadius) + FTextShadowOffsetY;
  end
  else
  begin
    NeededWidth  := s.cx;
    NeededHeight := s.cy;
  end;
end;

procedure TBGRALabel.ConvertToGrayScale;
var
  bounds: TRect;
  px: PBGRAPixel;
  xb, yb: integer;
begin
  bounds := FBGRA.GetImageBounds;
  if (bounds.Right <= bounds.Left) or (bounds.Bottom <= Bounds.Top) then
    exit;

  for yb := bounds.Top to bounds.bottom - 1 do
  begin
    px := FBGRA.scanline[yb] + bounds.left;
    for xb := bounds.left to bounds.right - 1 do
    begin
      px^ := BGRAToGrayscale(px^);
      Inc(px);
    end;
  end;
  FBGRA.InvalidateBitmap;
end;

procedure TBGRALabel.DrawText(AFontColor: TColor; AFontStyle: TFontStyles;
  AFontName: string);
var
  //s: TSize;
  //x, y, gx, gy: integer;
  //gly: TBGRABitmap;
  ATextRect{, ATextRectShadow}: TRect;
  style: TTextStyle;

begin
  FBGRAText.SetSize(ClientWidth + FTextShadowOffsetX + 2 * FTextShadowRadius,
    ClientHeight + FTextShadowOffsetY + 2 * FTextShadowRadius);
  FBGRAText.Fill(BGRAPixelTransparent);
  FBGRAText.FontAntialias := FTextAntialiasing;
  Canvas.Font := FFont;
  FBGRAText.FontHeight := Canvas.TextHeight(Caption);
  FBGRAText.FontStyle := AFontStyle;
  FBGRAText.FontName := AFontName;

  //s := FBGRAText.TextSize(Caption);
  //gx := 0;
  //gy := 0;

  ATextRect := ClientRect;
  //ATextRectShadow := ATextRect;
  //  ATextRectShadow.
  {$hints off}
  FillChar(style, sizeof(style), 0);
  {$hints on}

  style.Wordbreak := FWordWrap;
  style.ShowPrefix := False;
  style.Clipping := True;

  { X Position }
  case FTextAlign of
    btaLeft:
    begin
      //x := 5 + gx;
      style.Alignment := taLeftJustify;
    end;
    btaCenter:
    begin
      //x := Round((ClientWidth)/2)-Round((s.cx)/2);
      style.Alignment := taCenter;
    end;
    btaRight:
    begin
      //x := ClientWidth-5-s.cx;
      style.Alignment := taRightJustify;
    end;
  end;

  { Y position }
  case FTextVAlign of
    btvaTop:
    begin
      //y := 2+FBorderWidth;
      style.Layout := tlTop;
    end;
    btvaCenter:
    begin
      //y := Round(ClientHeight/2) - Round(s.cy/2);
      style.Layout := tlCenter;
    end;
    btvaBottom:
    begin
      //y := ClientHeight-FBorderWidth-2-s.cy;
      style.Layout := tlBottom;
    end;
  end;

  if FTextShadow then
  begin
    //FBGRATextShadow.SetSize(s.cx+2*FTextShadowRadius,s.cy+2*FTextShadowRadius);
    FBGRATextShadow.SetSize(ClientWidth + FTextShadowOffsetX + 2 * FTextShadowRadius,
      ClientHeight + FTextShadowOffsetY + 2 * FTextShadowRadius);
    FBGRATextShadow.Fill(BGRAPixelTransparent);
    FBGRATextShadow.FontAntialias := FTextShadowAntialiasing;
    FBGRATextShadow.FontHeight := FBGRAText.FontHeight;
    FBGRATextShadow.FontStyle := AFontStyle;
    FBGRATextShadow.FontName := AFontName;

    //FBGRATextShadow.TextOut(FTextShadowRadius,FTextShadowRadius,Caption,ColorToBGRA(ColorToRGB(FTextShadowColor),FTextShadowColorOpacity));
    FBGRATextShadow.TextRect(ATextRect, FTextShadowRadius, FTextShadowRadius,
      Caption, style, ColorToBGRA(ColorToRGB(FTextShadowColor),
      FTextShadowColorOpacity));
    BGRAReplace(FBGRATextShadow, FBGRATextShadow.FilterBlurRadial(
      FTextShadowRadius, rbFast));
    FBGRA.PutImage({x + }FTextShadowOffsetX - FTextShadowRadius,{ y +}
      FTextShadowOffsetY - FTextShadowRadius, FBGRATextShadow,
      dmDrawWithTransparency);
  end;

  FBGRA.Bitmap;
  //FBGRAText.TextOut(x,y,Caption,ColorToBGRA(ColorToRGB(AFontColor)));
  FBGRAText.TextRect(ATextRect, {x}0, {y}0, Caption, style,
    ColorToBGRA(ColorToRGB(AFontColor)));

  FBGRA.PutImage(0, 0, FBGRAText, dmDrawWithTransparency);
end;

procedure TBGRALabel.DrawBackground(ABackground: TBGRABackground; ARect: TRect);
var
  {borcolor,} backcolor: TBGRAPixel;
  gra:  TBGRAGradientScanner;
  back: TBGRABitmap;
  grect1, grect2: TRect;
  multi: TBGRAMultishapeFiller;
  fiLight: TFillBorderRoundRectInfo;
begin
  { Background color }
  case ABackground.FStyle of
    bbsClear: backcolor := BGRAPixelTransparent;
    bbsColor: backcolor := ColorToBGRA(ColorToRGB(ABackground.FColor),
        ABackground.FColorOpacity);
  end;

  case ABackground.FStyle of
    bbsClear, bbsColor:
      { Solid background color }
      FBGRA.RoundRectAntialias(ARect.Left, ARect.Top, ARect.Right,
        ARect.Bottom, FRoundX, FRoundY,
        {borcolor}BGRAPixelTransparent, FBorderWidth, backcolor);
    bbsGradient:
    begin
      { Using multishape filler to merge background gradient and border }
      multi := TBGRAMultishapeFiller.Create;
      multi.PolygonOrder := poFirstOnTop; { Border will replace background }

      {if borcolor.alpha <> 0 then
         Let the background be wider with transparent border
        multi.AddRoundRectangleBorder(ARect.Left, ARect.Top, ARect.Right,
          ARect.Bottom, FRoundX, FRoundY,
          FBorderWidth, borcolor);}

      { Gradients }
      back := TBGRABitmap.Create(ClientWidth, ClientHeight, BGRAPixelTransparent);
      grect1 := ARect;
      grect2 := ARect;
      { Gradient 1 }
      if ABackground.FGradient1EndPercent > 0 then
      begin
        grect1.Bottom := Round((grect1.Bottom / 100) * ABackground.FGradient1EndPercent);
        gra := CreateGradient(ABackground.FGradient1, grect1);
        back.FillRect(grect1.Left, grect1.Top, grect1.Right, grect1.Bottom,
          gra, dmSet
          );
        gra.Free;
      end;
      { Gradient 2 }
      if ABackground.FGradient1EndPercent < 100 then
      begin
        if grect1.Bottom < ARect.Bottom then
          grect2.Top := grect1.Bottom - 1;
        gra := CreateGradient(ABackground.FGradient2, grect2);
        back.FillRect(grect2.Left, grect2.Top, grect2.Right, grect2.Bottom,
          gra, dmSet
          );
        gra.Free;
      end;

      multi.AddRoundRectangle(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom,
        FRoundX, FRoundY, back);

      multi.Draw(FBGRA);
      multi.Free;
      back.Free;

      if ABackground.LightWidth > 0 then
      begin
        //compute light position
        fiLight := TFillBorderRoundRectInfo.Create(
          ARect.Left, ARect.Top, ARect.Right, ARect.Bottom, FRoundX,
          FRoundY, FBorderWidth + ABackground.LightWidth, []);
        //check if there is an inner position
        if fiLight.InnerBorder <> nil then
          with fiLight.InnerBorder do //fill with light
            FBGRA.RoundRectAntialias(topleft.x, topleft.y, bottomright.x,
              bottomright.y, radiusx, radiusY, BGRA(255, 255, 255,
              ABackground.LightOpacity),
              ABackground.LightWidth);
        fiLight.Free;
      end;
    end;
  end;
end;

procedure TBGRALabel.SetBackground(const AValue: TBGRABackground);
begin
  if FBackground = AValue then
    exit;
  FBackground.Assign(AValue);

  Invalidate;
end;

procedure TBGRALabel.SetBorderWidth(const AValue: integer);
begin
  if FBorderWidth = AValue then
    exit;
  FBorderWidth := AValue;

  Invalidate;
  UpdateSize;
end;

procedure TBGRALabel.SetRoundX(const AValue: integer);
begin
  if FRoundX = AValue then
    exit;
  FRoundX := AValue;

  Invalidate;
end;

procedure TBGRALabel.SetRoundY(const AValue: integer);
begin
  if FRoundY = AValue then
    exit;
  FRoundY := AValue;

  Invalidate;
end;

procedure TBGRALabel.SeTBGRATextAlign(const AValue: TBGRATextAlign);
begin
  if FTextAlign = AValue then
    exit;
  FTextAlign := AValue;

  Invalidate;
end;

procedure TBGRALabel.SetTextShadow(const AValue: boolean);
begin
  if FTextShadow = AValue then
    exit;
  FTextShadow := AValue;

  Invalidate;
  UpdateSize;
end;

procedure TBGRALabel.SetTextAntialiasing(const AValue: boolean);
begin
  if FTextAntialiasing = AValue then
    exit;
  FTextAntialiasing := AValue;

  Invalidate;
  UpdateSize;
end;

procedure TBGRALabel.SetTextShadowAntialiasing(const AValue: boolean);
begin
  if FTextShadowAntialiasing = AValue then
    exit;
  FTextShadowAntialiasing := AValue;

  Invalidate;
  UpdateSize;
end;

procedure TBGRALabel.SetTextShadowColor(const AValue: TColor);
begin
  if FTextShadowColor = AValue then
    exit;
  FTextShadowColor := AValue;

  Invalidate;
  UpdateSize;
end;

procedure TBGRALabel.SetTextShadowColorOpacity(const AValue: byte);
begin
  if FTextShadowColorOpacity = AValue then
    exit;
  FTextShadowColorOpacity := AValue;

  Invalidate;
  UpdateSize;
end;

procedure TBGRALabel.SetTextShadowOffsetX(const AValue: integer);
begin
  if FTextShadowOffsetX = AValue then
    exit;
  FTextShadowOffsetX := AValue;

  Invalidate;
  UpdateSize;
end;

procedure TBGRALabel.SetTextShadowOffsetY(const AValue: integer);
begin
  if FTextShadowOffsetY = AValue then
    exit;
  FTextShadowOffsetY := AValue;

  Invalidate;
  UpdateSize;
end;

procedure TBGRALabel.SetTextShadowRadius(const AValue: integer);
begin
  if FTextShadowRadius = AValue then
    exit;
  FTextShadowRadius := AValue;

  Invalidate;
  UpdateSize;
end;

procedure TBGRALabel.SeTBGRATextVAlign(const AValue: TBGRATextVAlign);
begin
  if FTextVAlign = AValue then
    exit;
  FTextVAlign := AValue;

  Invalidate;
end;

procedure TBGRALabel.UpdateSize;
begin
  InvalidatePreferredSize;
  AdjustSize;
end;

procedure TBGRALabel.SetWordWrap(Value: boolean);
begin
  if FWordWrap <> Value then
  begin
    FWordWrap := Value;
    Invalidate;
    UpdateSize;
  end;
end;

procedure TBGRALabel.CalculatePreferredSize(
  var PreferredWidth, PreferredHeight: integer;
  WithThemeSpace: boolean);
var
  AWidth: integer;
  //gh: integer = 0;
  //gw: integer = 0;
begin
  {inherited CalculatePreferredSize(PreferredWidth, PreferredHeight,
    WithThemeSpace);  }

  if (Parent = nil) or (not Parent.HandleAllocated) then
    Exit;
  if WidthIsAnchored and WordWrap then
    AWidth := Width
  else
    AWidth := 10000;
  CalculateTextSize(AWidth, PreferredWidth, PreferredHeight);

  Inc(PreferredWidth, (FBorderWidth * 2) div 2);
  Inc(PreferredHeight, (FBorderWidth * 2) div 2);

end;

class function TBGRALabel.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 123;
  Result.CY := 33;
end;

procedure TBGRALabel.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
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

procedure TBGRALabel.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
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

  if (PopupMenu <> nil) and Enabled then
  begin
    p := ClientToScreen(Point(X, Y));
    PopupMenu.PopUp(p.x, p.y);
  end;
end;

procedure TBGRALabel.MouseEnter;
begin
  if csDesigning in ComponentState then
    exit;
  FMouseStage := msHover;
  Invalidate;
  inherited MouseEnter;
end;

procedure TBGRALabel.MouseLeave;
begin
  if csDesigning in ComponentState then
    exit;
  FMouseStage := msNone;
  Invalidate;
  inherited MouseLeave;
end;

procedure TBGRALabel.MouseMove(Shift: TShiftState; X, Y: integer);
begin
  inherited MouseMove(Shift, X, Y);

  Invalidate;
end;

procedure TBGRALabel.Paint;
var
  {bodya,} bodyn: TBGRABackground;
  baserect: TRect;
begin
  //inherited Paint;
  { Mouse stage }
  {case FMouseStage of
    msNormal: bodya := FBackground;
  end;}

  { Calculating base rect }
  baserect := ClientRect;
  Inc(baserect.Left, Round(FBorderWidth / 2));
  Inc(baserect.Top, Round(FBorderWidth / 2));
  Dec(baserect.Right, Round(FBorderWidth / 2) + 1);
  Dec(baserect.Bottom, Round(FBorderWidth / 2) + 1);

  { Clearing previous paint }
  FBGRA.Fill(BGRAPixelTransparent);

  { Drawing background label }
  bodyn := FBackground;

  DrawBackground(bodyn, Rect(baserect.Left, baserect.Top, baserect.Right,
    baserect.Bottom));

  if FTextApplyGlobalOpacity then
  begin
    { Drawing text }
    DrawText(FFont.Color, FFont.Style, FFont.Name);
    { Set global opacity }
    FBGRA.ApplyGlobalOpacity(FGlobalOpacity);
  end
  else
  begin
    { Set global opacity }
    FBGRA.ApplyGlobalOpacity(FGlobalOpacity);
    { Drawing text }
    DrawText(FFont.Color, FFont.Style, FFont.Name);
  end;

  { Convert to gray if not enabled }
  if not Enabled then
    ConvertToGrayScale;

  { Finally drawing on canvas }
  FBGRA.Draw(Self.Canvas, 0, 0, False);
end;

procedure TBGRALabel.Resize;
begin
  inherited Resize;
  if FBGRA <> nil then
    FBGRA.SetSize(Width, Height);
end;

procedure TBGRALabel.SetEnabled(Value: boolean);
begin
  inherited SetEnabled(Value);
  Invalidate;
end;

procedure TBGRALabel.TextChanged;
begin
  inherited TextChanged;
  Invalidate;
  UpdateSize;
end;

procedure TBGRALabel.SetGlobalOpacity(const AValue: byte);
begin
  if FGlobalOpacity = AValue then
    exit;
  FGlobalOpacity := AValue;
  Invalidate;
end;

procedure TBGRALabel.SetTextApplyGlobalOpacity(const AValue: boolean);
begin
  if FTextApplyGlobalOpacity = AValue then
    exit;
  FTextApplyGlobalOpacity := AValue;
  Invalidate;
end;

{procedure TBGRALabel.WMLButtonDown(var Message: TLMLButtonDown);
begin

end; }

constructor TBGRALabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);
  ControlStyle := ControlStyle + [csAcceptsControls];
  FBGRA := TBGRABitmap.Create(Width, Height, BGRAPixelTransparent);
  FBGRA.FontAntialias := True;
  FBGRAText := TBGRABitmap.Create(Width, Height, BGRAPixelTransparent);
  FTextAntialiasing := True;
  FBGRAText.FontAntialias := FTextAntialiasing;
  FBGRATextShadow := TBGRABitmap.Create(Width, Height, BGRAPixelTransparent);
  FTextShadowAntialiasing := True;
  FBGRATextShadow.FontAntialias := FTextShadowAntialiasing;
  ParentColor := False;
  FBackground := TBGRABackground.Create(Self);
  FMouseStage := msNone;
  FTextAlign := btaLeft;
  FTextVAlign := btvaTop;
  FTextShadow := True;
  FTextShadowRadius := 5;
  FTextShadowColor := clBlack;
  FTextShadowColorOpacity := 255;
  FTextShadowOffsetX := 5;
  FTextShadowOffsetY := 5;
  FBorderWidth := 1;
  FRoundX := 1;
  FRoundY := 1;
  FGlobalOpacity := 255;
  FTextApplyGlobalOpacity := False;
  FWordWrap := False;
  FFont := TFont.Create;
  FFont.OnChange := @OnChangeFont;
end;

destructor TBGRALabel.Destroy;
begin
  FBGRA.Free;
  FBGRAText.Free;
  FBGRATextShadow.Free;
  FBackground.Free;
  FFont.Free;
  inherited Destroy;
end;

procedure TBGRALabel.Assign(Source: TPersistent);
begin
  if Source is TBGRALabel then
  begin
    FBorderWidth := TBGRALabel(Source).FBorderWidth;
    FRoundX := TBGRALabel(Source).FRoundX;
    FRoundY := TBGRALabel(Source).FRoundY;
    FTextAlign := TBGRALabel(Source).FTextAlign;
    FTextAntialiasing := TBGRALabel(Source).FTextAntialiasing;
    FTextVAlign := TBGRALabel(Source).FTextVAlign;
    FTextShadow := TBGRALabel(Source).FTextShadow;
    FTextShadowAntialiasing := TBGRALabel(Source).FTextShadowAntialiasing;
    FTextShadowColor := TBGRALabel(Source).FTextShadowColor;
    FTextShadowColorOpacity := TBGRALabel(Source).FTextShadowColorOpacity;
    FTextShadowOffsetX := TBGRALabel(Source).FTextShadowOffsetX;
    FTextShadowOffsetY := TBGRALabel(Source).FTextShadowOffsetY;
    FTextShadowRadius := TBGRALabel(Source).FTextShadowRadius;
    FGlobalOpacity := TBGRALabel(Source).FGlobalOpacity;
    FTextApplyGlobalOpacity := TBGRALabel(Source).FTextApplyGlobalOpacity;
    FWordWrap := TBGRALabel(Source).FWordWrap;

    FFont.Assign(TBGRALabel(Source).FFont);
    FBackground.Assign(TBGRALabel(Source).FBackground);

    Invalidate;
    UpdateSize;
  end
  else
    inherited Assign(Source);
end;

end.
