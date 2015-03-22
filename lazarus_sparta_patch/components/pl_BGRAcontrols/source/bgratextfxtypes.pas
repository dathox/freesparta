unit BGRATextFXTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics,
  BGRABitmap, BGRABitmapTypes, LMessages;

type

  { TBGRATextEffectBase }

  TBGRATextEffectBase = class(TPersistent)
  private
    FAlpha:   byte;
    FColor:   TColor;
    FOffsetX: integer;
    FOffsetY: integer;
    FOwner:   TControl;
    FVisible: boolean;
    procedure SetFAlpha(const AValue: byte);
    procedure SetFColor(const AValue: TColor);
    procedure SetFOffsetX(const AValue: integer);
    procedure SetFOffsetY(const AValue: integer);
    procedure SetFVisible(const AValue: boolean);
  public
    constructor Create(AOwner: TControl);
    destructor Destroy; override;
  published
    property Alpha: byte Read FAlpha Write SetFAlpha;
    property Color: TColor Read FColor Write SetFColor;
    property OffsetX: integer Read FOffsetX Write SetFOffsetX;
    property OffsetY: integer Read FOffsetY Write SetFOffsetY;
    property Visible: boolean Read FVisible Write SetFVisible;
  end;

  { TBGRATextEffectShadow }

  TBGRATextEffectShadow = class(TBGRATextEffectBase)
  private
    FRadius: integer;
    procedure SetFRadius(AValue: integer);
  public
    constructor Create(AOwner: TControl);
    destructor Destroy; override;
  published
    property Radius: integer Read FRadius Write SetFRadius;
  end;

  { TBGRATextEffectBaseTex }

  TBGRATextEffectBaseTex = class(TBGRATextEffectBase)
  private
    FTexture: IBGRAScanner;
  public
    constructor Create(AOwner: TControl);
    destructor Destroy; override;
    procedure Texture(ATexture: IBGRAScanner);
  published
  end;

  { TBGRATextEffectOutline }

  TBGRATextEffectOutline = class(TBGRATextEffectBaseTex)
  private
  public
    constructor Create(AOwner: TControl);
    destructor Destroy; override;
  published
  end;

implementation

{ TBGRATextEffectOutline }

constructor TBGRATextEffectOutline.Create(AOwner: TControl);
begin
  inherited Create(AOwner);
  FColor := clWhite;
end;

destructor TBGRATextEffectOutline.Destroy;
begin
  inherited Destroy;
end;

{ TBGRATextEffectBaseTex }

constructor TBGRATextEffectBaseTex.Create(AOwner: TControl);
begin
  inherited Create(AOwner);
end;

destructor TBGRATextEffectBaseTex.Destroy;
begin
  FreeAndNil(FTexture);
  inherited Destroy;
end;

procedure TBGRATextEffectBaseTex.Texture(ATexture: IBGRAScanner);
begin
  if FTexture = ATexture then
    exit;
  FTexture := ATexture;

  FOwner.Perform(CM_FONTCHANGED, 0, 0);
  FOwner.Invalidate;
end;

{ TBGRATextEffectShadow }

procedure TBGRATextEffectShadow.SetFRadius(AValue: integer);
begin
  if FRadius = AValue then
    exit;
  FRadius := AValue;

  FOwner.Perform(CM_FONTCHANGED, 0, 0);
  FOwner.Invalidate;
end;

constructor TBGRATextEffectShadow.Create(AOwner: TControl);
begin
  inherited Create(AOwner);
  FColor  := clBlack;
  FOffsetX := 1;
  FOffsetY := 1;
  FRadius := 1;
end;

destructor TBGRATextEffectShadow.Destroy;
begin
  inherited Destroy;
end;

{ TBGRATextEffectBase }

procedure TBGRATextEffectBase.SetFAlpha(const AValue: byte);
begin
  if FAlpha = AValue then
    exit;
  FAlpha := AValue;

  FOwner.Perform(CM_FONTCHANGED, 0, 0);
  FOwner.Invalidate;
end;

procedure TBGRATextEffectBase.SetFColor(const AValue: TColor);
begin
  if FColor = AValue then
    exit;
  FColor := AValue;

  FOwner.Perform(CM_FONTCHANGED, 0, 0);
  FOwner.Invalidate;
end;

procedure TBGRATextEffectBase.SetFOffsetX(const AValue: integer);
begin
  if FOffsetX = AValue then
    exit;
  FOffsetX := AValue;

  FOwner.Perform(CM_FONTCHANGED, 0, 0);
  FOwner.Invalidate;
end;

procedure TBGRATextEffectBase.SetFOffsetY(const AValue: integer);
begin
  if FOffsetY = AValue then
    exit;
  FOffsetY := AValue;

  FOwner.Perform(CM_FONTCHANGED, 0, 0);
  FOwner.Invalidate;
end;

procedure TBGRATextEffectBase.SetFVisible(const AValue: boolean);
begin
  if FVisible = AValue then
    exit;
  FVisible := AValue;

  FOwner.Perform(CM_FONTCHANGED, 0, 0);
  FOwner.Invalidate;
end;

constructor TBGRATextEffectBase.Create(AOwner: TControl);
begin
  FOwner := AOwner;
  FAlpha := 255;
  FColor := clNone;
  FOffsetX := 0;
  FOffsetY := 0;
  FVisible := True;
  inherited Create;
end;

destructor TBGRATextEffectBase.Destroy;
begin
  inherited Destroy;
end;

end.
