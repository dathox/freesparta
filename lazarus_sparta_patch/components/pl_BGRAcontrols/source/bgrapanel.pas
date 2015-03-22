{ !! THIS CONTROL IS DEPRECATED! USE BCBUTTON INSTEAD !!
  Panel with BGRAGradient functionality

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
unit BGRAPanel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, BGRABitmap, LMessages,
  BGRABitmapTypes, BGRAGradientScanner, BCTypes, Types;

type
  TBGRAPanelBackStyle      = (bpsColor, bpsGradient);
  TOnAfterPrepareBGRAPanel = procedure(Sender: TObject; const ABGRA: TBGRABitmap;
    ARect: TRect) of object;

  { TBGRAPanel }

  TBGRAPanel = class(TPanel)
  private
    { Private declarations }
    FBGRA:           TBGRABitmap;
    FBackgroundStyle: TBGRAPanelBackStyle;
    FCaptionAlignment: TAlignment;
    FCaptionLayout:  TTextLayout;
    FCaptionOffsetX: integer;
    FCaptionOffsetY: integer;
    FCaptionShadow:  boolean;
    FCaptionShadowColor: TColor;
    FCaptionShadowRadius: integer;
    FCaptionShadowX: integer;
    FCaptionShadowY: integer;
    FGradient:       TBCGradient;
    FOnAfterPrepareBGRAPanel: TOnAfterPrepareBGRAPanel;
    FUpdateCount:    integer;
    procedure DrawGradient;
    procedure PrepareBGRA;
    procedure SetBackgroundStyle(const AValue: TBGRAPanelBackStyle);
    procedure SetCaptionAlignment(AValue: TAlignment);
    procedure SetCaptionLayout(AValue: TTextLayout);
    procedure SetCaptionOffsetX(AValue: integer);
    procedure SetCaptionOffsetY(AValue: integer);
    procedure SetCaptionShadow(AValue: boolean);
    procedure SetCaptionShadowColor(AValue: TColor);
    procedure SetCaptionShadowRadius(AValue: integer);
    procedure SetCaptionShadowX(AValue: integer);
    procedure SetCaptionShadowY(AValue: integer);
    procedure SetGradient(const AValue: TBCGradient);
  protected
    procedure CMChanged(var Message: TLMessage); message CM_CHANGED;
    procedure FontChanged(Sender: TObject); override;
  protected
    { Protected declarations }
    procedure Paint; override;
    procedure Resize; override;
    procedure SetEnabled(Value: boolean); override;
  public
    { Public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;
  published
    { Published declarations }
    property BackgroundStyle: TBGRAPanelBackStyle
      Read FBackgroundStyle Write SetBackgroundStyle default bpsGradient;
    property CaptionAlignment: TAlignment Read FCaptionAlignment
      Write SetCaptionAlignment default taCenter;
    property CaptionLayout: TTextLayout Read FCaptionLayout
      Write SetCaptionLayout default tlCenter;
    property CaptionOffsetX: integer Read FCaptionOffsetX
      Write SetCaptionOffsetX default 0;
    property CaptionOffsetY: integer Read FCaptionOffsetY
      Write SetCaptionOffsetY default 0;
    property CaptionShadow: boolean
      Read FCaptionShadow Write SetCaptionShadow default False;
    property CaptionShadowColor: TColor Read FCaptionShadowColor
      Write SetCaptionShadowColor default clBlack;
    property CaptionShadowX: integer Read FCaptionShadowX
      Write SetCaptionShadowX default 1;
    property CaptionShadowY: integer Read FCaptionShadowY
      Write SetCaptionShadowY default 1;
    property CaptionShadowRadius: integer Read FCaptionShadowRadius
      Write SetCaptionShadowRadius default 3;
    property Gradient: TBCGradient Read FGradient Write SetGradient;
  published
    { Events }
    property OnAfterPrepareBGRAPanel: TOnAfterPrepareBGRAPanel
      Read FOnAfterPrepareBGRAPanel Write FOnAfterPrepareBGRAPanel;
  end;

implementation

uses BCTools;

{ TBGRAPanel }

procedure TBGRAPanel.SetBackgroundStyle(const AValue: TBGRAPanelBackStyle);
begin
  if FBackgroundStyle = AValue then
    exit;
  FBackgroundStyle := AValue;

  Changed;
  Invalidate;
end;

procedure TBGRAPanel.SetCaptionAlignment(AValue: TAlignment);
begin
  if FCaptionAlignment = AValue then
    Exit;
  FCaptionAlignment := AValue;
  Changed;
  Invalidate;
end;

procedure TBGRAPanel.SetCaptionLayout(AValue: TTextLayout);
begin
  if FCaptionLayout = AValue then
    Exit;
  FCaptionLayout := AValue;
  Changed;
  Invalidate;
end;

procedure TBGRAPanel.SetCaptionOffsetX(AValue: integer);
begin
  if FCaptionOffsetX = AValue then
    Exit;
  FCaptionOffsetX := AValue;
  Changed;
  Invalidate;
end;

procedure TBGRAPanel.SetCaptionOffsetY(AValue: integer);
begin
  if FCaptionOffsetY = AValue then
    Exit;
  FCaptionOffsetY := AValue;
  Changed;
  Invalidate;
end;

procedure TBGRAPanel.SetCaptionShadow(AValue: boolean);
begin
  if FCaptionShadow = AValue then
    Exit;
  FCaptionShadow := AValue;
  Changed;
  Invalidate;
end;

procedure TBGRAPanel.SetCaptionShadowColor(AValue: TColor);
begin
  if FCaptionShadowColor = AValue then
    Exit;
  FCaptionShadowColor := AValue;
  Changed;
  Invalidate;
end;

procedure TBGRAPanel.SetCaptionShadowRadius(AValue: integer);
begin
  if FCaptionShadowRadius = AValue then
    Exit;
  FCaptionShadowRadius := AValue;
  Changed;
  Invalidate;
end;

procedure TBGRAPanel.SetCaptionShadowX(AValue: integer);
begin
  if FCaptionShadowX = AValue then
    Exit;
  FCaptionShadowX := AValue;
  Changed;
  Invalidate;
end;

procedure TBGRAPanel.SetCaptionShadowY(AValue: integer);
begin
  if FCaptionShadowY = AValue then
    Exit;
  FCaptionShadowY := AValue;
  Changed;
  Invalidate;
end;

procedure TBGRAPanel.SetGradient(const AValue: TBCGradient);
begin
  if FGradient = AValue then
    exit;
  FGradient.Assign(AValue);
  Changed;
  Invalidate;
end;

procedure TBGRAPanel.CMChanged(var Message: TLMessage);
begin
  if FUpdateCount > 0 then
    Exit;
  PrepareBGRA;
end;

procedure TBGRAPanel.FontChanged(Sender: TObject);
begin
  inherited FontChanged(Sender);
  Changed;
  Invalidate;
end;

procedure TBGRAPanel.DrawGradient;
var
  sc: TBGRAGradientScanner;
begin
  sc := CreateGradient(FGradient, ClientRect);
  FBGRA.Fill(sc);
  sc.Free;
end;

procedure TBGRAPanel.PrepareBGRA;
var
  ARect: TRect;
  TS:  TTextStyle;
  txt: TBGRABitmap;
begin
  if (csCreating in FControlState) or (FUpdateCount > 0) then
    Exit;

  FBGRA.SetSize(Width, Height);

  if FBackgroundStyle = bpsGradient then
    DrawGradient
  else
    FBGRA.Fill(ColorToRGB(Color));

  ARect := GetClientRect;

  // if BevelOuter is set then draw a frame with BevelWidth
  if (BevelOuter <> bvNone) and (BevelWidth > 0) then
    FBGRA.CanvasBGRA.Frame3d(ARect, BevelWidth, BevelOuter,
      BGRA(255, 255, 255, 180), BGRA(0, 0, 0, 160)); // Note: Frame3D inflates ARect

  InflateRect(ARect, -BorderWidth, -BorderWidth);

  // if BevelInner is set then skip the BorderWidth and draw a frame with BevelWidth
  if (BevelInner <> bvNone) and (BevelWidth > 0) then
    FBGRA.CanvasBGRA.Frame3d(ARect, BevelWidth, BevelInner,
      BGRA(255, 255, 255, 160), BGRA(0, 0, 0, 160)); // Note: Frame3D inflates ARect

  if Caption <> '' then
  begin
    txt := TBGRABitmap.Create(FBGRA.Width, FBGRA.Height, BGRAPixelTransparent);
    try
      txt.CanvasBGRA.Font.Assign(Font);
      txt.Canvas.Font.Assign(Font);
      //FBGRA.CanvasBGRA.Font.Assign(Canvas.Font);
      TS := Canvas.TextStyle;
      TS.Alignment := FCaptionAlignment;
      TS.Layout := FCaptionLayout;
      TS.Opaque := False;
      TS.Clipping := False;
      TS.SystemFont := Canvas.Font.IsDefault;
      OffsetRect(ARect, FCaptionOffsetX, FCaptionOffsetY);
      if FCaptionShadow then
      begin
        OffsetRect(ARect, FCaptionShadowX, FCaptionShadowY);
        txt.CanvasBGRA.Font.BGRAColor := ColorToBGRA(FCaptionShadowColor);
        txt.CanvasBGRA.TextRect(ARect, ARect.Left, ARect.Top, Caption, TS);
        BGRAReplace(txt, txt.FilterBlurRadial(FCaptionShadowRadius, rbFast));
        txt.CanvasBGRA.Font.Color := Font.Color;
        OffsetRect(ARect, -FCaptionShadowX, -FCaptionShadowY);
        txt.CanvasBGRA.Font.Assign(Font);
        txt.Canvas.Font.Assign(Font);
      end;
      if not Enabled then
      begin
        txt.CanvasBGRA.Font.BGRAColor := BGRA(255, 255, 255, 160);
        OffsetRect(ARect, 1, 1);
        txt.CanvasBGRA.TextRect(ARect, ARect.Left, ARect.Top, Caption, TS);
        txt.CanvasBGRA.Font.BGRAColor := BGRA(0, 0, 0, 160);
        OffsetRect(ARect, -1, -1);
      end
      else
        txt.CanvasBGRA.Font.Color := Font.Color;

      txt.CanvasBGRA.TextRect(ARect, ARect.Left, ARect.Top, Caption, TS);
      FBGRA.PutImage(0, 0, txt, dmDrawWithTransparency);
      OffsetRect(ARect, -FCaptionOffsetX, -FCaptionOffsetY);
    finally
      txt.Free;
    end;
  end;
  if Assigned(FOnAfterPrepareBGRAPanel) then
    FOnAfterPrepareBGRAPanel(Self, FBGRA, ARect);
end;

procedure TBGRAPanel.Paint;
begin
  if (csCreating in FControlState) or (FUpdateCount > 0) then
    Exit;  

  {$IFDEF WINDOWS}   // ct9999 for CodeTyphon
    FBGRA.Draw(Canvas,0,0,true);
  {$ELSE}
    FBGRA.Draw(Canvas,0,0,false);
  {$ENDIF}
end;

procedure TBGRAPanel.Resize;
begin
  inherited Resize;
  if FBGRA <> nil then
    PrepareBGRA;
end;

procedure TBGRAPanel.SetEnabled(Value: boolean);
begin
  inherited SetEnabled(Value);
  Changed;
end;

constructor TBGRAPanel.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  DisableAutoSizing;
  Include(FControlState, csCreating);
  BeginUpdate;
  try
    Self.DoubleBuffered := True;
    FBGRA := TBGRABitmap.Create;
    FGradient := TBCGradient.Create(Self);
    FBackgroundStyle := bpsGradient;
    FUpdateCount := 0;
    FCaptionAlignment := taCenter;
    FCaptionLayout := tlCenter;
    FCaptionOffsetX := 0;
    FCaptionOffsetY := 0;
    FCaptionShadow := False;
    FCaptionShadowColor := clBlack;
    FCaptionShadowX := 1;
    FCaptionShadowY := 1;
    FCaptionShadowRadius := 3;
  finally
    EnableAutoSizing;
    EndUpdate;
    Exclude(FControlState, csCreating);
  end;
end;

destructor TBGRAPanel.Destroy;
begin
  FBGRA.Free;
  FGradient.Free;
  inherited Destroy;
end;

procedure TBGRAPanel.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TBGRAPanel.EndUpdate;
begin
  if FUpdateCount > 0 then
    Dec(FUpdateCount);
  if FUpdateCount = 0 then
    PrepareBGRA;
end;

end.
