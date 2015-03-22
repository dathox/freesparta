unit BGRALabelFX;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics,
  BGRABitmap, BGRABitmapTypes, BGRATextFXTypes, BGRATextFX, types;

type

  { TBGRALabelFX }

  TBGRALabelFX = class(TGraphicControl)
  private
    FBGRA:    TBGRABitmap;
    FTextEffect: TBGRATextEffect;
    FOutline: TBGRATextEffectOutline;
    FShadow:  TBGRATextEffectShadow;
    FPreviousCaption: string;
    FPreviousFont: TFont;
  protected
    procedure Paint; override;
    procedure RealSetText(const Value: TCaption); override;
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer;
      WithThemeSpace: boolean); override;
    procedure UpdateTextEffect;
  public
    function Draw: TBGRABitmap;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Action;
    property Align;
    property AutoSize;
    property Caption;
    property Font;
    property PopupMenu;
    property Outline: TBGRATextEffectOutline Read FOutline Write FOutline;
    property Shadow: TBGRATextEffectShadow Read FShadow Write FShadow;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
  end;

implementation

uses
  LResources;

{ TBGRALabelFX }

procedure TBGRALabelFX.RealSetText(const Value: TCaption);
begin
  inherited RealSetText(Value);

  if Value <> FPreviousCaption then Invalidate;
  InvalidatePreferredSize;
  AdjustSize;
end;

procedure TBGRALabelFX.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: integer; WithThemeSpace: boolean);
var
  s: TSize;
  ax, ay: integer;
begin
  UpdateTextEffect;
  s.cx := FTextEffect.Width;
  s.cy := FTextEffect.Height;

  if FShadow.Visible then
  begin
    if FShadow.OffsetX < 0 then
      ax := (FShadow.OffsetX) - (FShadow.OffsetX * 2)
    else
      ax := FShadow.OffsetX;

    if FShadow.OffsetY < 0 then
      ay := (FShadow.OffsetY) - (FShadow.OffsetY * 2)
    else
      ay := FShadow.OffsetY;

    Inc(s.cx, 2 * ax + 2 * FShadow.Radius);
    Inc(s.cy, 2 * ay + 2 * FShadow.Radius);
  end;

  PreferredWidth := s.cx;
  PreferredHeight := s.cy;
end;

procedure TBGRALabelFX.UpdateTextEffect;
var NewCaption: string;
begin
  if FTextEffect = nil then exit;
  NewCaption := Caption;
  if (NewCaption <> FPreviousCaption) or
    (Font.Name <> FPreviousFont.Name) or
    (Font.Style <> FPreviousFont.Style) or
    (Font.Quality <> FPreviousFont.Quality) or
    (Font.Orientation <> FPreviousFont.Orientation) or
    (Font.Pitch <> FPreviousFont.Pitch) or
    (Font.Height <> FPreviousFont.Height) or
    (Font.CharSet <> FPreviousFont.CharSet) then
  begin
    FreeAndNil(FTextEffect);
    FTextEffect := TBGRATextEffect.Create(NewCaption, Font, True);
    FPreviousFont.Assign(Font);
    FPreviousCaption := NewCaption;
  end;
end;

procedure TBGRALabelFX.Paint;
var
  cx, cy, px, py: integer;
  ax, ay: integer;
  shx, shy: integer;
begin
  ax := 0;
  ay := 0;

  if FShadow.Visible then
  begin
    if FShadow.OffsetX < 0 then
      ax := (FShadow.OffsetX) - (FShadow.OffsetX * 2)
    else
      ax := FShadow.OffsetX;

    if FShadow.OffsetY < 0 then
      ay := (FShadow.OffsetY) - (FShadow.OffsetY * 2)
    else
      ay := FShadow.OffsetY;

    ax := 2 * ax + 2 * FShadow.Radius;
    ay := 2 * ay + 2 * FShadow.Radius;
  end;

  UpdateTextEffect;

  InvalidatePreferredSize;
  AdjustSize;

  FBGRA.Free;
  FBGRA := TBGRABitmap.Create(FTextEffect.Width + ax,
    FTextEffect.Height + ay);

  { taCenter }
  cx := trunc((FBGRA.Width - FTextEffect.Width) / 2);
  cy := trunc((FBGRA.Height - FTextEffect.Height) / 2);
  px := trunc((Width - FBGRA.Width) / 2);
  py := trunc((Height - FBGRA.Height) / 2);

  if FShadow.OffsetX < 0 then
    shx := - FShadow.OffsetX + FShadow.Radius
  else
    shx := 2 * FShadow.OffsetX + integer(FSHadow.Radius);

  if FShadow.OffsetY < 0 then
    shy := - FShadow.OffsetY + FShadow.Radius
  else
    shy := 2 * FShadow.OffsetY + integer(FShadow.Radius);

  if FShadow.Visible then
    FTextEffect.DrawShadow(FBGRA, shx,
      shy,
      FShadow.Radius, ColorToBGRA(FShadow.Color, FShadow.Alpha));

  if FOutline.Visible then
    FTextEffect.DrawOutline(FBGRA, cx + FOutline.OffsetX, cy + FOutline.OffsetY,
      ColorToBGRA(FOutline.Color, FOutline.Alpha));

  FTextEffect.Draw(FBGRA, cx, cy, ColorToBGRA(Font.Color, 255));

  FBGRA.Draw(Self.Canvas, px, py, False);
end;

function TBGRALabelFX.Draw: TBGRABitmap;
begin
  Result := FBGRA.Duplicate as TBGRABitmap;
end;

constructor TBGRALabelFX.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetInitialBounds(0, 0, 24, 24);
  Font.Height := 24;
  FBGRA := TBGRABitmap.Create;
  FTextEffect := TBGRATextEffect.Create(Caption, Font, True);
  FPreviousCaption:= Caption;
  FPreviousFont := TFont.Create;
  FPreviousFont.Assign(Font);
  FOutline := TBGRATextEffectOutline.Create(Self);
  FShadow := TBGRATextEffectShadow.Create(Self);
  //
  AutoSize := True;
  FShadow.OffsetX := 0;
  FShadow.OffsetY := 0;
  FShadow.Radius := 5;
end;

destructor TBGRALabelFX.Destroy;
begin
  FBGRA.Free;
  FTextEffect.Free;
  FPreviousFont.Free;
  FOutline.Free;
  FShadow.Free;
  inherited Destroy;
end;

end.
