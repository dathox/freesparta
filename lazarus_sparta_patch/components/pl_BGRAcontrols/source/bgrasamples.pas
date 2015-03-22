unit bgrasamples;

{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, Controls, Graphics,
  BGRABitmap, BGRABitmapTypes, BGRAGradients, BGRAButton, BGRAVirtualScreen,
  BGRAImageButton, BGRAPolygon, LCLProc;

const
  // accent color
  acMagenta = TColor($009700FF);
  acPurple = TColor($00FF00A2);
  acTeal = TColor($00A9AB00);
  acLime = TColor($0026BF8C);
  acBrown = TColor($000050A0);
  acPink = TColor($00B871E6);
  acOrange = TColor($000996F0);
  acBlue = TColor($00E2A11B);
  acRed = TColor($000014E5);
  acGreen = TColor($00339933);
  // facebook button color
  fbBlue = TColor($00AA785F);
  fbGreen = TColor($004BA567);
  fbGray = TColor($00F8F8F8);

type
  TBGRASampleStyle = (ssDefault, ssFlashPlayer, ssWin7ToolBar,
    ssWin7ToolBarSmooth, ssWin7, ssOffice2010,
    ssMacOSXLion, ssWin7Cristal, ssiOSBar, ssiOSToolBar, ssiOSBackground,
    {ssFacebookBlue, ssFacebookGreen, ssFacebookGray,} ssBlack,
    ssSilverSquared, ssSilver, ssGreen, ssBlue);

const
  TBGRASampleStyleStr: array [TBGRASampleStyle] of string =
    ('Default', 'Flash Player', 'Windows 7 ToolBar', 'Windows 7 ToolBar Smooth',
    'Windows 7', 'Office 2010',
    'Mac OSX Lion', 'Windows 7 Cristal', 'iOS Bar', 'iOS ToolBar',
    'iOS Background',{ 'Facebook Blue', 'Facebook Green', 'Facebook Gray',}
    'Black', 'Silver Squared', 'Silver', 'Green', 'Blue');

function StrToTBGRASampleStyle(const s: ansistring): TBGRASampleStyle;

{ Glass }
procedure Glass(AVirtualScreen: TBGRAVirtualScreen; ABackground: TBGRABitmap;
  ABlur: integer; btnShowBkg: boolean);

{ Metro UI }
procedure AccentColorButton(AButton: TBGRAButton; AColor: TColor);
procedure AccentColorButtonAll(AControl: TControl);
procedure AccentColorImageButton(Sender: TObject);
procedure AccentColorImageButton(AButton: TBGRAImageButton; AColor: TColor);
procedure AccentColorImageButtonAll(AControl: TControl);

{ Misc }
procedure SetAllFormsDoubleBuffered;
procedure SetAllBGRAButtonFont(Control: TControl; AFont: TFont);
procedure SetBGRAButtonFont(AButton: TBGRAButton; AFont: TFont);

{ Drawings }
procedure DrawButton(bitmap: TBGRABitmap; cl1, cl2, cl3, cl4, border, light: TBGRAPixel;
  rx, ry, w, Value: single; dir1, dir2, dir3: TGradientDirection;
  options: TRoundRectangleOptions = []; drawlight: boolean = True);
procedure DrawFlashPlayerBody(ABitmap: TBGRABitmap);
procedure DrawFlashPlayerButtonPanel(ABitmap: TBGRABitmap);
procedure DrawWin7ToolBar(ABitmap: TBGRABitmap; ADir: TAlign; Smooth: boolean = False);

{ StyleButtons }
procedure StyleButtons(AControl: TControl; AButton: TBGRAButton);
procedure StyleButtonsSample(AControl: TControl; AStyle: TBGRASampleStyle);

{ Buttons }
procedure FlashPlayerButton(AButton: TBGRAButton);
procedure Win7ToolBarButton(AButton: TBGRAButton; Smooth: boolean = False);
procedure Win7Button(AButton: TBGRAButton);
procedure Office2010Button(AButton: TBGRAButton);
procedure MacOSXLionButton(AButton: TBGRAButton);
procedure Win7CristalButton(AButton: TBGRAButton);

{ Image Buttons }
procedure SingleColorImageButton(AButton: TBGRAImageButton;
  cl1, cl2, cl3, cl4: TBGRAPixel);
procedure SingleColorImageButton(AButton: TBGRAImageButton; cl1, cl2, cl3, cl4: TColor);

{ iOS }
type
  TiOSElement = (iOSBar, iOSToolBar, iOSBackground);

procedure DrawiOSElement(ABitmap: TBGRABitmap; Element: TiOSElement);
function DrawiOSElement(AWidth, AHeight: integer; Element: TiOSElement): TBGRABitmap;
procedure DrawiOSBar(ABitmap: TBGRABitmap);
procedure DrawiOSToolBar(ABitmap: TBGRABitmap; Shadow: boolean = True);
procedure DrawiOSBackground(ABitmap: TBGRABitmap);

{ Facebook Button }

procedure DrawFacebookImageButton(Sender: TObject);
procedure DrawFacebookImageButton(AButton: TBGRAImageButton; AclBody: TBGRAPixel);
procedure DrawFacebookButton(ABitmap: TBGRABitmap; AclBody, AclBackground: TBGRAPixel;
  Pressed: boolean);
procedure DrawFacebookButtonNormal(ABitmap: TBGRABitmap;
  AclBody, AclBorder, AclBorderDark, AclLight, AclShadow: TBGRAPixel);
procedure DrawFacebookButtonPressed(ABitmap: TBGRABitmap;
  AclBody, AclBorder, AclShadow: TBGRAPixel);

{ Silver Style }

procedure DrawBlackToolBarButton(AButton: TBGRAButton);
procedure DrawBlueBodyButton(AButton: TBGRAButton);
procedure DrawGreenBodyButton(AButton: TBGRAButton);
procedure DrawSilverBodyButton(AButton: TBGRAButton);
procedure DrawSilverBodyButtonDropDown(AButton: TBGRAButton);
procedure DrawSilverBodyButtonSquared(AButton: TBGRAButton);
procedure DrawSilverToolBar(ABitmap: TBGRABitmap);
procedure DrawSilverStatusBar(ABitmap: TBGRABitmap);

implementation

function StrToTBGRASampleStyle(const s: ansistring): TBGRASampleStyle;
var
  ss: TBGRASampleStyle;
  ls: ansistring;
begin
  Result := ssDefault;
  ls := UTF8LowerCase(s);
  for ss := low(TBGRASampleStyle) to high(TBGRASampleStyle) do
    if ls = UTF8LowerCase(TBGRASampleStyleStr[ss]) then
    begin
      Result := ss;
      break;
    end;
end;

{ Glass }

procedure Glass(AVirtualScreen: TBGRAVirtualScreen; ABackground: TBGRABitmap;
  ABlur: integer; btnShowBkg: boolean);
var
  bmpMask, blurBkg, copyBkg: TBGRABitmap;
  i: integer;
begin
  bmpMask := TBGRABitmap.Create(AVirtualScreen.Width, AVirtualScreen.Height);

  for i := 0 to AVirtualScreen.ControlCount - 1 do
  begin
    if AVirtualScreen.Controls[i] is TBGRAButton then
    begin
      AVirtualScreen.Controls[i].UpdateBaseBounds(True, True, False);
      bmpMask.Rectangle(AVirtualScreen.Controls[i].BaseBounds, BGRAWhite,
        BGRAWhite, dmSet);
    end;
  end;

  copyBkg := TBGRABitmap.Create;
  BGRAReplace(copyBkg, ABackground.Resample(AVirtualScreen.Width,
    AVirtualScreen.Height));

  blurBkg := copyBkg.Duplicate(False) as TBGRABitmap;
  blurBkg.ApplyMask(bmpMask);
  bmpMask.Free;

  BGRAReplace(blurBkg, blurBkg.FilterBlurRadial(ABlur, rbFast));

  if btnShowBkg then
    AVirtualScreen.Bitmap.PutImage(0, 0, copyBkg, dmDrawWithTransparency);
  copyBkg.Free;

  AVirtualScreen.Bitmap.PutImage(0, 0, blurBkg, dmDrawWithTransparency);
  blurBkg.Free;
end;

{ Metro UI }

procedure AccentColorButton(AButton: TBGRAButton; AColor: TColor);
begin
  with AButton do
  begin
    TextShadow := True;
    TextShadowOffsetX := 1;
    TextShadowOffsetY := 1;
    TextShadowRadius := 1;
    with BodyNormal do
    begin
      BorderColor := AColor;
      BorderColorOpacity := 255;
      BorderStyle := bboSolid;
      Color := AColor;
      ColorOpacity := 255;
      Font.Color := clWhite;
      Font.Name := 'Segoe WP Light';
      Style := bbsColor;
    end;
    with BodyHover do
    begin
      BorderColor := clWhite;
      BorderColorOpacity := 255;
      BorderStyle := bboSolid;
      Color := clWhite;
      ColorOpacity := 255;
      Font.Color := clWhite;
      Font.Name := 'Segoe WP Light';
      Style := bbsColor;
    end;
    with BodyClicked do
    begin
      BorderColor := AColor;
      BorderColorOpacity := 255;
      BorderStyle := bboSolid;
      Color := AColor;
      ColorOpacity := 255;
      Font.Color := clWhite;
      Font.Name := 'Segoe WP Light';
      Style := bbsColor;
    end;
  end;
end;

procedure AccentColorButtonAll(AControl: TControl);
var
  i: integer;
  WinControl: TWinControl;
begin
  if AControl is TBGRAButton then
    AccentColorButton(AControl as TBGRAButton, AControl.Color);

  if AControl is TWinControl then
  begin
    WinControl := TWinControl(AControl);
    if WinControl.ControlCount = 0 then
      exit;
    for i := 0 to WinControl.ControlCount - 1 do
      AccentColorButtonAll(WinControl.Controls[i]);
  end;
end;

procedure AccentColorImageButton(Sender: TObject);
begin
  if Sender is TBGRAImageButton then
    AccentColorImageButton(TBGRAImageButton(Sender), TBGRAImageButton(Sender).Color);
end;

procedure AccentColorImageButton(AButton: TBGRAImageButton; AColor: TColor);
begin
  AButton.Font.Name := 'Segoe WP Light';
  AButton.Font.Color := clWhite;
  SingleColorImageButton(AButton, AColor, $00FAFAFA, AColor, clGray);
end;

procedure AccentColorImageButtonAll(AControl: TControl);
var
  i: integer;
  WinControl: TWinControl;
begin
  if AControl is TBGRAImageButton then
    AccentColorImageButton(AControl as TBGRAImageButton, AControl.Color);

  if AControl is TWinControl then
  begin
    WinControl := TWinControl(AControl);
    if WinControl.ControlCount = 0 then
      exit;
    for i := 0 to WinControl.ControlCount - 1 do
      AccentColorImageButtonAll(WinControl.Controls[i]);
  end;
end;

{ Misc }

procedure SetAllFormsDoubleBuffered;
var
  i: integer;
begin
  for i := 0 to Screen.FormCount - 1 do
    Screen.Forms[i].DoubleBuffered := True;
end;

procedure SetAllBGRAButtonFont(Control: TControl; AFont: TFont);
var
  i: integer;
  WinControl: TWinControl;
  tempButton: TBGRAButton;
begin
  if Control is TBGRAButton then
  begin
    tempButton := TBGRAButton(Control);
    SetBGRAButtonFont(tempButton, AFont);
  end;
  if Control is TWinControl then
  begin
    WinControl := TWinControl(Control);
    if WinControl.ControlCount = 0 then
      exit;
    for i := 0 to WinControl.ControlCount - 1 do
      if WinControl.Controls[i] is TBGRAButton then
      begin
        tempButton := TBGRAButton(WinControl.Controls[i]);
        SetBGRAButtonFont(tempButton, AFont);
      end;
  end;
end;

procedure SetBGRAButtonFont(AButton: TBGRAButton; AFont: TFont);
begin
  with AButton do
  begin
    BodyClicked.Font := AFont;
    BodyHover.Font := AFont;
    BodyNormal.Font := AFont;
  end;
end;

{ Drawings }

procedure DrawButton(bitmap: TBGRABitmap; cl1, cl2, cl3, cl4, border, light: TBGRAPixel;
  rx, ry, w, Value: single; dir1, dir2, dir3: TGradientDirection;
  options: TRoundRectangleOptions = []; drawlight: boolean = True);
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

procedure DrawFlashPlayerBody(ABitmap: TBGRABitmap);
begin
  with ABitmap do
  begin
    GradientFill(0, 0, Width, Height, BGRA(203, 19, 23, 255), BGRA(110, 3, 20, 255),
      gtLinear, PointF(0, 0), PointF(0, Height), dmSet);
    Rectangle(0, 0, Width, Height + 1, BGRA(0, 0, 0, 215), dmDrawWithTransparency);
  end;
end;

procedure DrawFlashPlayerButtonPanel(ABitmap: TBGRABitmap);
begin
  with ABitmap do
  begin
    DrawHorizLine(0, 0, Width, BGRA(30, 30, 30, 255));
    DrawHorizLine(0, Height - 1, Width, BGRA(62, 62, 62, 255));
    Rectangle(0, 1, Width, Height - 1, BGRA(91, 91, 91, 255),
      BGRA(76, 76, 76, 255), dmSet);
  end;
end;

procedure DrawWin7ToolBar(ABitmap: TBGRABitmap; ADir: TAlign; Smooth: boolean = False);
var
  tempBmp: TBGRABitmap;
begin
  if Smooth then
    tempBmp := DoubleGradientAlphaFill(Rect(0, 0, ABitmap.Width, ABitmap.Height),
      BGRA(245, 249, 255, 255), BGRA(222, 232, 245, 255), BGRA(220, 230, 245, 255),
      BGRA(222, 230, 245, 255), gdVertical, gdVertical, gdVertical, 0.50)
  else
    tempBmp := DoubleGradientAlphaFill(Rect(0, 0, ABitmap.Width, ABitmap.Height),
      BGRA(245, 250, 255, 255), BGRA(230, 240, 250, 255), BGRA(220, 230, 244, 255),
      BGRA(221, 233, 247, 255), gdVertical, gdVertical, gdVertical, 0.50);

  ABitmap.PutImage(0, 0, tempBmp, dmSet);
  tempBmp.Free;

  case ADir of
    alLeft: with ABitmap do
      begin
        if not Smooth then
          Rectangle(0, 0, Width - 2, Height, BGRA(255, 255, 255, 100),
            dmDrawWithTransparency);
        SetVertLine(Width - 1, 0, Height - 1, BGRA(160, 175, 195, 255));
        SetVertLine(Width - 2, 0, Height - 1, BGRA(205, 218, 234, 255));
      end;
    alTop: with ABitmap do
      begin
        if not Smooth then
          Rectangle(0, 0, Width, Height - 2, BGRA(255, 255, 255, 100),
            dmDrawWithTransparency);
        SetHorizLine(0, Height - 1, Width - 1, BGRA(160, 175, 195, 255));
        SetHorizLine(0, Height - 2, Width - 1, BGRA(205, 218, 234, 255));
      end;
    alRight: with ABitmap do
      begin
        if not Smooth then
          Rectangle(2, 0, Width, Height, BGRA(255, 255, 255, 100),
            dmDrawWithTransparency);
        SetVertLine(0, 0, Height, BGRA(160, 175, 195, 255));
        SetVertLine(1, 0, Height, BGRA(205, 218, 234, 255));
      end;
    alBottom: with ABitmap do
      begin
        if not Smooth then
          Rectangle(0, 2, Width, Height, BGRA(255, 255, 255, 100),
            dmDrawWithTransparency);
        SetHorizLine(0, 0, Width - 1, BGRA(160, 175, 195, 255));
        SetHorizLine(0, 1, Width - 1, BGRA(205, 218, 234, 255));
      end;
    alClient, alCustom, alNone: with ABitmap do
      begin
        if not Smooth then
          Rectangle(0, 0, Width, Height, BGRA(255, 255, 255, 100),
            dmDrawWithTransparency);
      end;
  end;
end;

{ StyleButtons }

procedure StyleButtons(AControl: TControl; AButton: TBGRAButton);
var
  i: integer;
  WinControl: TWinControl;
begin
  if AControl is TBGRAButton then
    with AControl as TBGRAButton do
    begin
      // Those are recomended properties to keep unchanged
      AButton.Glyph := Glyph;
      AButton.GlyphMargin := GlyphMargin;
      AButton.StaticButton := StaticButton;
      AButton.Down := Down;
      AButton.Style := Style;
      AButton.TextAlign := TextAlign;
      AButton.TextVAlign := TextVAlign;

      Assign(AButton);
    end;
  if AControl is TWinControl then
  begin
    WinControl := TWinControl(AControl);
    if WinControl.ControlCount = 0 then
      exit;
    for i := 0 to WinControl.ControlCount - 1 do
      StyleButtons(WinControl.Controls[i], AButton);
  end;
end;

procedure StyleButtonsSample(AControl: TControl; AStyle: TBGRASampleStyle);
var
  tempBGRAButton: TBGRAButton;
begin
  tempBGRAButton := TBGRAButton.Create(nil);
  case AStyle of
    ssFlashPlayer: FlashPlayerButton(tempBGRAButton);
    ssWin7Toolbar: Win7ToolBarButton(tempBGRAButton);
    ssWin7ToolbarSmooth: Win7ToolBarButton(tempBGRAButton, True);
    ssWin7: Win7Button(tempBGRAButton);
    ssOffice2010: Office2010Button(tempBGRAButton);
    ssMacOSXLion: MacOSXLionButton(tempBGRAButton);
    ssWin7Cristal: Win7CristalButton(tempBGRAButton);
    ssiOSBar, ssiOSToolBar, ssiOSBackground: MacOSXLionButton(tempBGRAButton);
    // ToDO: add iOS TBGRAButton
    //ssFacebookBlue, ssFacebookGreen, ssFacebookGray: MacOSXLionButton(tempBGRAButton);
    // ToDO: add Facebook TBGRAButton
    ssBlack: DrawBlackToolBarButton(tempBGRAButton);
    ssSilver: DrawSilverBodyButton(tempBGRAButton);
    ssSilverSquared: DrawSilverBodyButtonSquared(tempBGRAButton);
    ssGreen: DrawGreenBodyButton(tempBGRAButton);
    ssBlue: DrawBlueBodyButton(tempBGRAButton);
  end;
  StyleButtons(AControl, tempBGRAButton);
  tempBGRAButton.Free;
end;

{ Buttons }

procedure FlashPlayerButton(AButton: TBGRAButton);
begin
  with AButton do
  begin
    TextCanvas := True;
    TextShadowOffSetX := 1;
    TextShadowOffSetY := 1;
    TextShadowRadius := 1;
    with BorderStyle do
    begin
      BottomLeft := bsSquare;
      BottomRight := bsSquare;
      TopLeft := bsSquare;
      TopRight := bsSquare;
    end;
    with BodyNormal do
    begin
      BorderColor := RGBToColor(24, 24, 24);
      Font.Color := clWhite;
      LightOpacity := 20;
      LightWidth := 1;
      Gradient1.StartColor := RGBToColor(104, 104, 104);
      Gradient1.EndColor := RGBToColor(104, 104, 104);
      Gradient2.StartColor := RGBToColor(103, 103, 103);
      Gradient2.EndColor := RGBToColor(71, 71, 71);
    end;
    with BodyHover do
    begin
      BorderColor := RGBToColor(24, 24, 24);
      Font.Color := clWhite;
      LightOpacity := 20;
      LightWidth := 1;
      Gradient1.StartColor := RGBToColor(118, 118, 118);
      Gradient1.EndColor := RGBToColor(118, 118, 118);
      Gradient2.StartColor := RGBToColor(117, 117, 117);
      Gradient2.EndColor := RGBToColor(81, 81, 81);
    end;
    with BodyClicked do
    begin
      BorderColor := RGBToColor(24, 24, 24);
      Font.Color := clWhite;
      LightOpacity := 20;
      LightWidth := 1;
      Gradient1.StartColor := RGBToColor(92, 92, 92);
      Gradient1.EndColor := RGBToColor(92, 92, 92);
      Gradient2.StartColor := RGBToColor(91, 91, 91);
      Gradient2.EndColor := RGBToColor(62, 62, 62);
    end;
  end;
end;

procedure Win7ToolBarButton(AButton: TBGRAButton; Smooth: boolean = False);
begin
  with AButton do
  begin
    TextCanvas := True;
    TextShadow := False;
    TextShadowOffsetX := 0;
    TextShadowOffsetY := 0;
    TextShadowRadius := 0;
    RoundX := 2;
    RoundY := 2;
    with BodyNormal do
    begin
      Font.Color := RGBToColor(30, 57, 91); //clBlack;
      Gradient1EndPercent := 55;
      BorderStyle := bboNone;
      // smooth
      if Smooth then
      begin
        Gradient1.StartColor := RGBToColor(245, 249, 255);
        Gradient1.EndColor := RGBToColor(222, 232, 245);
        Gradient2.StartColor := RGBToColor(220, 230, 245);
        Gradient2.EndColor := RGBToColor(222, 230, 245);
      end
      // normal
      else
      begin
        Gradient1.StartColor := RGBToColor(245, 250, 255);
        Gradient1.EndColor := RGBToColor(230, 240, 250);
        Gradient2.StartColor := RGBToColor(220, 230, 244);
        Gradient2.EndColor := RGBToColor(221, 233, 247);
      end;
    end;
    with BodyHover do
    begin
      Font.Color := RGBToColor(30, 57, 91); //clBlack;
      Gradient1EndPercent := 55;
      BorderColor := RGBToColor(187, 202, 219);
      Gradient1.StartColor := RGBToColor(248, 251, 254);
      Gradient1.EndColor := RGBToColor(237, 242, 250);
      Gradient2.StartColor := RGBToColor(215, 228, 244);
      Gradient2.EndColor := RGBToColor(193, 210, 232);
      LightWidth := 1;
      LightOpacity := 200;
    end;
    with BodyClicked do
    begin
      Font.Color := RGBToColor(30, 57, 91); //clBlack;
      Gradient1EndPercent := 55;
      BorderColor := RGBToColor(187, 202, 219);
      Gradient1.StartColor := RGBToColor(226, 236, 245);
      Gradient1.EndColor := RGBToColor(216, 228, 241);
      Gradient2.StartColor := RGBToColor(207, 219, 236);
      Gradient2.EndColor := RGBToColor(207, 220, 237);
    end;
  end;
end;

procedure Win7Button(AButton: TBGRAButton);
begin
  with AButton do
  begin
    TextCanvas := True;
    TextShadow := False;
    TextShadowOffsetX := 0;
    TextShadowOffsetY := 0;
    TextShadowRadius := 0;
    RoundX := 3;
    RoundY := 3;
    with BodyNormal do
    begin
      Font.Color := clBlack;
      Gradient1EndPercent := 50;
      BorderColor := RGBToColor(112, 112, 112);
      Gradient1.StartColor := RGBToColor(242, 242, 242);
      Gradient1.EndColor := RGBToColor(235, 235, 235);
      Gradient2.StartColor := RGBToColor(221, 221, 221);
      Gradient2.EndColor := RGBToColor(207, 207, 207);
      LightOpacity := 175;
      LightWidth := 1;
    end;
    with BodyHover do
    begin
      Font.Color := clBlack;
      Gradient1EndPercent := 50;
      BorderColor := RGBToColor(60, 127, 177);
      Gradient1.StartColor := RGBToColor(234, 246, 253);
      Gradient1.EndColor := RGBToColor(217, 240, 252);
      Gradient2.StartColor := RGBToColor(190, 230, 253);
      Gradient2.EndColor := RGBToColor(167, 217, 245);
      LightOpacity := 175;
      LightWidth := 1;
    end;
    with BodyClicked do
    begin
      Font.Color := clBlack;
      Gradient1EndPercent := 55;
      BorderColor := RGBToColor(44, 98, 139);
      Gradient1.StartColor := RGBToColor(229, 244, 252);
      Gradient1.EndColor := RGBToColor(196, 229, 246);
      Gradient2.StartColor := RGBToColor(152, 209, 239);
      Gradient2.EndColor := RGBToColor(104, 179, 219);
      LightWidth := 0;
    end;
  end;
end;

procedure Office2010Button(AButton: TBGRAButton);
begin
  with AButton do
  begin
    TextCanvas := True;
    RoundX := 3;
    RoundY := 3;
    TextShadow := False;
    TextShadowOffsetX := 0;
    TextShadowOffsetY := 0;
    TextShadowRadius := 0;
    with BodyNormal do
    begin
      BorderColor := RGBToColor(207, 208, 210);
      Font.Color := clBlack;
      Gradient1.StartColor := RGBToColor(255, 255, 255);
      Gradient1.EndColor := RGBToColor(237, 239, 241);
      Gradient1EndPercent := 100;
      LightOpacity := 175;
      LightWidth := 1;
    end;
    with BodyHover do
    begin
      BorderColor := RGBToColor(244, 210, 81);
      Font.Color := clBlack;
      Gradient1.StartColor := RGBToColor(254, 241, 189);
      Gradient1.EndColor := RGBToColor(254, 228, 134);
      Gradient1EndPercent := 50;
      Gradient2.StartColor := RGBToColor(254, 228, 134);
      Gradient2.EndColor := RGBToColor(254, 248, 196);
      LightOpacity := 175;
      LightWidth := 1;
    end;
    with BodyClicked do
    begin
      BorderColor := RGBToColor(194, 161, 63);
      Font.Color := clBlack;
      Gradient1.StartColor := RGBToColor(255, 229, 117);
      Gradient1.EndColor := RGBToColor(255, 216, 107);
      Gradient1EndPercent := 50;
      Gradient2.StartColor := RGBToColor(255, 216, 107);
      Gradient2.EndColor := RGBToColor(255, 239, 129);
      LightWidth := 0;
    end;
  end;
end;

procedure MacOSXLionButton(AButton: TBGRAButton);
begin
  with AButton do
  begin
    TextCanvas := True;
    TextShadow := False;
    TextShadowOffsetX := 0;
    TextShadowOffsetY := 0;
    TextShadowRadius := 0;
    RoundX := 3;
    RoundY := 3;
    with BodyNormal do
    begin
      Font.Color := clBlack;
      Gradient1EndPercent := 50;
      BorderColor := RGBToColor(154, 154, 154);
      Gradient1.StartColor := RGBToColor(255, 255, 255);
      Gradient1.EndColor := RGBToColor(243, 243, 243);
      Gradient2.StartColor := RGBToColor(236, 236, 236);
      Gradient2.EndColor := RGBToColor(235, 235, 235);
      LightOpacity := 175;
      LightWidth := 1;
    end;
    with BodyHover do
    begin
      Font.Color := clBlack;
      Gradient1EndPercent := 50;
      BorderColor := RGBToColor(86, 87, 143);
      Gradient1.StartColor := RGBToColor(204, 229, 252);
      Gradient1.EndColor := RGBToColor(161, 209, 249);
      Gradient2.StartColor := RGBToColor(143, 202, 251);
      Gradient2.EndColor := RGBToColor(207, 245, 253);
      LightOpacity := 175;
      LightWidth := 1;
    end;
    with BodyClicked do
    begin
      Font.Color := clBlack;
      Gradient1EndPercent := 55;
      BorderColor := RGBToColor(86, 87, 143);
      Gradient1.StartColor := RGBToColor(144, 195, 241);
      Gradient1.EndColor := RGBToColor(113, 180, 239);
      Gradient2.StartColor := RGBToColor(97, 173, 240);
      Gradient2.EndColor := RGBToColor(147, 206, 241);
      LightWidth := 0;
    end;
  end;
end;

procedure Win7CristalButton(AButton: TBGRAButton);
begin
  with AButton do
  begin
    TextCanvas := True;
    TextShadowOffsetX := 1;
    TextShadowOffsetY := 1;
    TextShadowRadius := 1;
    with BorderStyle do
    begin
      BottomLeft := bsSquare;
      BottomRight := bsSquare;
      TopLeft := bsSquare;
      TopRight := bsSquare;
    end;
    with BodyNormal do
    begin
      BorderColor := RGBToColor(93, 111, 136);
      Font.Color := clWhite;
      LightOpacity := 150;
      LightWidth := 1;
      Gradient1EndPercent := 40;
      Gradient1.StartColor := RGBToColor(195, 212, 231);
      Gradient1.EndColor := RGBToColor(190, 211, 232);
      Gradient2.StartColor := RGBToColor(156, 181, 207);
      Gradient2.EndColor := RGBToColor(183, 208, 233);
      Gradient2.Point1YPercent := 70;
    end;
    with BodyHover do
    begin
      BorderColor := RGBToColor(81, 94, 108);
      Font.Color := clWhite;
      LightOpacity := 150;
      LightWidth := 1;
      Gradient1EndPercent := 40;
      Gradient1.StartColor := RGBToColor(170, 213, 243);
      Gradient1.EndColor := RGBToColor(134, 194, 235);
      Gradient1.Point1YPercent := 5;
      Gradient2.StartColor := RGBToColor(45, 117, 167);
      Gradient2.EndColor := RGBToColor(36, 198, 235);
      Gradient2.Point1YPercent := 70;
    end;
    with BodyClicked do
    begin
      BorderColor := RGBToColor(81, 94, 108);
      Font.Color := clWhite;
      LightOpacity := 75;
      LightWidth := 1;
      Gradient1EndPercent := 40;
      Gradient1.StartColor := RGBToColor(127, 154, 172);
      Gradient1.EndColor := RGBToColor(97, 122, 144);
      Gradient1.Point1YPercent := 5;
      Gradient2.StartColor := RGBToColor(32, 62, 84);
      Gradient2.EndColor := RGBToColor(39, 201, 200);
      Gradient2.Point1YPercent := 70;
    end;
  end;
end;

procedure SingleColorImageButton(AButton: TBGRAImageButton;
  cl1, cl2, cl3, cl4: TBGRAPixel);
var
  temp: TBGRABitmap;
begin
  temp := TBGRABitmap.Create(1, 4);
  temp.SetPixel(0, 0, cl1);
  temp.SetPixel(0, 1, cl2);
  temp.SetPixel(0, 2, cl3);
  temp.SetPixel(0, 3, cl4);
  AButton.Bitmap := temp.Bitmap;
  temp.Free;
end;

procedure SingleColorImageButton(AButton: TBGRAImageButton; cl1, cl2, cl3, cl4: TColor);
begin
  SingleColorImageButton(AButton, ColorToBGRA(ColorToRGB(cl1), 255),
    ColorToBGRA(ColorToRGB(cl2), 255),
    ColorToBGRA(ColorToRGB(cl3), 255),
    ColorToBGRA(ColorToRGB(cl4), 255));
end;

{ iOS }

procedure DrawiOSElement(ABitmap: TBGRABitmap; Element: TiOSElement);
begin
  case Element of
    iOSBar: DrawiOSBar(ABitmap);
    iOSToolBar: DrawiOSToolBar(ABitmap);
    iOSBackground: DrawiOSBackground(ABitmap);
  end;
end;

function DrawiOSElement(AWidth, AHeight: integer; Element: TiOSElement): TBGRABitmap;
var
  ABitmap: TBGRABitmap;
begin
  ABitmap := TBGRABitmap.Create(AWidth, AHeight);
  DrawiOSElement(ABitmap, Element);
  Result := ABitmap;
end;

procedure DrawiOSBar(ABitmap: TBGRABitmap);
begin
  ABitmap.GradientFill(0, 0, ABitmap.Width, ABitmap.Height - 4,
    BGRA(238, 245, 248, 255), BGRA(196, 204, 208, 255), gtLinear,
    PointF(0, 0), PointF(0, ABitmap.Height - 4), dmSet);
  with ABitmap do
  begin
    // Bottom Bevel
    SetHorizLine(0, Height - 4, Width - 1, BGRA(190, 198, 202, 255));
    SetHorizLine(0, Height - 3, Width - 1, BGRA(201, 209, 213, 255));
    SetHorizLine(0, Height - 2, Width - 1, BGRA(134, 142, 147, 255));
    SetHorizLine(0, Height - 1, Width - 1, BGRA(177, 180, 182, 255));
  end;
end;

procedure DrawiOSToolBar(ABitmap: TBGRABitmap; Shadow: boolean = True);
begin
  if Shadow then
  begin
    DoubleGradientAlphaFill(ABitmap, Rect(0, 3, ABitmap.Width, ABitmap.Height - 6),
      BGRA(172, 185, 201), BGRA(134, 153, 178, 255),
      BGRA(125, 147, 175, 255), BGRA(110, 132, 162, 255),
      gdVertical, gdVertical, gdVertical, 0.5);
    with ABitmap do
    begin
      // Top Bevel
      SetHorizLine(0, 0, Width - 1, BGRA(201, 210, 221, 255));
      SetHorizLine(0, 1, Width - 1, BGRA(173, 184, 200, 255));
      SetHorizLine(0, 2, Width - 1, BGRA(179, 190, 205, 255));
      // Bottom Bevel
      SetHorizLine(0, Height - 6, Width - 1, BGRA(107, 129, 158, 255));
      SetHorizLine(0, Height - 5, Width - 1, BGRA(116, 139, 170, 255));
      SetHorizLine(0, Height - 4, Width - 1, BGRA(48, 54, 62, 255));
      // Bottom Shadow
      SetHorizLine(0, Height - 3, Width - 1, BGRA(0, 0, 0, 75));
      SetHorizLine(0, Height - 2, Width - 1, BGRA(255, 255, 255, 50));
      SetHorizLine(0, Height - 1, Width - 1, BGRA(0, 0, 0, 10));
    end;
  end
  else
  begin
    DoubleGradientAlphaFill(ABitmap, Rect(0, 3, ABitmap.Width, ABitmap.Height - 3),
      BGRA(172, 185, 201), BGRA(134, 153, 178, 255),
      BGRA(125, 147, 175, 255), BGRA(110, 132, 162, 255),
      gdVertical, gdVertical, gdVertical, 0.5);
    with ABitmap do
    begin
      // Top Bevel
      SetHorizLine(0, 0, Width - 1, BGRA(201, 210, 221, 255));
      SetHorizLine(0, 1, Width - 1, BGRA(173, 184, 200, 255));
      SetHorizLine(0, 2, Width - 1, BGRA(179, 190, 205, 255));
      // Bottom Bevel
      SetHorizLine(0, Height - 3, Width - 1, BGRA(107, 129, 158, 255));
      SetHorizLine(0, Height - 2, Width - 1, BGRA(116, 139, 170, 255));
      SetHorizLine(0, Height - 1, Width - 1, BGRA(48, 54, 62, 255));
    end;
  end;
end;

procedure DrawiOSBackground(ABitmap: TBGRABitmap);
var
  temp: TBGRABitmap;
begin
  temp := TBGRABitmap.Create(16, 1, BGRA(197, 204, 211));
  with temp do
  begin
    SetPixel(7, 0, BGRA(203, 210, 217));
    SetPixel(14, 0, BGRA(204, 211, 218));
    BGRAReplace(temp, temp.FilterBlurRadial(1, rbFast));
    BGRAReplace(temp, temp.FilterSharpen);
  end;
  ABitmap.Fill(temp);
  temp.Free;
end;

{ Facebook Button }

procedure DrawFacebookImageButton(Sender: TObject);
begin
  if Sender is TBGRAImageButton then
    DrawFacebookImageButton(TBGRAImageButton(Sender),
      ColorToBGRA(ColorToRGB(TBGRAImageButton(Sender).Color), 255));
end;

procedure DrawFacebookImageButton(AButton: TBGRAImageButton; AclBody: TBGRAPixel);
var
  temp1, temp2, temp: TBGRABitmap;
begin
  AButton.Font.Color := clWhite;
  temp1 := TBGRABitmap.Create(AButton.Width, AButton.Height);
  DrawFacebookButton(temp1, AclBody, BGRAPixelTransparent, False);
  temp2 := TBGRABitmap.Create(AButton.Width, AButton.Height);
  DrawFacebookButton(temp2, AclBody, BGRAPixelTransparent, True);
  temp := TBGRABitmap.Create(AButton.Width, AButton.Height * 4);
  temp.PutImage(0, 0, temp1, dmDrawWithTransparency);
  temp.PutImage(0, AButton.Height, temp1, dmDrawWithTransparency);
  BGRAReplace(temp1, temp1.FilterGrayScale);
  temp.PutImage(0, AButton.Height * 3, temp1, dmDrawWithTransparency);
  temp2.BlendImage(0, 0, temp1, boMultiply);
  temp.PutImage(0, AButton.Height * 2, temp2, dmDrawWithTransparency);
  AButton.Bitmap := temp.Bitmap;
  temp.Free;
  temp1.Free;
  temp2.Free;
end;

procedure DrawFacebookButton(ABitmap: TBGRABitmap; AclBody, AclBackground: TBGRAPixel;
  Pressed: boolean);
begin
  ABitmap.Fill(AclBackground);
  if not Pressed then
    DrawFacebookButtonNormal(ABitmap, AclBody, BGRA(0, 0, 0, 150),
      BGRA(0, 0, 0, 100), BGRA(255, 255, 255, 50), BGRA(0, 0, 0, 50))
  else
    DrawFacebookButtonPressed(ABitmap, AclBody, BGRA(0, 0, 0, 150), BGRA(0, 0, 0, 100));
end;

procedure DrawFacebookButtonNormal(ABitmap: TBGRABitmap;
  AclBody, AclBorder, AclBorderDark, AclLight, AclShadow: TBGRAPixel);
begin
  with ABitmap do
  begin
    Rectangle(0, 0, Width, Height - 1, AclBody, AclBody, dmDrawWithTransparency);
    Rectangle(0, 0, Width, Height - 1, AclBorder, BGRAPixelTransparent,
      dmDrawWithTransparency);
    DrawHorizLine(1, 1, Width - 2, AclLight);
    DrawHorizLine(1, Height - 2, Width, AclBorderDark);
    DrawHorizLine(0, Height - 1, Width, AclShadow);
  end;
end;

procedure DrawFacebookButtonPressed(ABitmap: TBGRABitmap;
  AclBody, AclBorder, AclShadow: TBGRAPixel);
begin
  with ABitmap do
  begin
    Rectangle(0, 0, Width, Height - 1, AclBody, AclBody, dmDrawWithTransparency);
    Rectangle(0, 0, Width, Height - 1, AclBorder, BGRAPixelTransparent,
      dmDrawWithTransparency);
    DrawHorizLine(0, Height - 1, Width, AclShadow);
  end;
end;

{ Silver Style }

procedure DrawBlackToolBarButton(AButton: TBGRAButton);
begin
  with AButton do
  begin
    TextCanvas := True;
    TextShadow := False;
    TextShadowOffsetX := 0;
    TextShadowOffsetY := 0;
    TextShadowRadius := 0;
    with BorderStyle do
    begin
      BottomLeft := bsSquare;
      BottomRight := bsSquare;
      TopLeft := bsSquare;
      TopRight := bsSquare;
    end;
    with BorderStyleDropDown do
    begin
      BottomLeft := bsSquare;
      BottomRight := bsSquare;
      TopLeft := bsSquare;
      TopRight := bsSquare;
    end;
    with BodyNormal do
    begin
      Font.Color := clSilver;
      BorderColor := RGBToColor(23, 23, 23);
      Color := RGBToColor(23, 23, 23);
      Style := bbsColor;
    end;
    with BodyHover do
    begin
      Font.Color := clWhite;
      BorderColor := RGBToColor(23, 23, 23);
      Color := RGBToColor(23, 23, 23);
      Style := bbsColor;
    end;
    with BodyClicked do
    begin
      Font.Color := clWhite;
      BorderColor := clBlack;
      Color := clBlack;
      Style := bbsColor;
    end;
  end;
end;

procedure DrawBlueBodyButton(AButton: TBGRAButton);
begin
  with AButton do
  begin
    //TextCanvas := True;
    TextShadowColor := clBlack;
    TextShadowOffsetX := 0;
    TextShadowOffsetY := -1;
    TextShadowRadius := 0;
    RoundX := 3;
    RoundY := 3;
    with BodyNormal do
    begin
      Font.Color := clWhite;
      Font.Style := [fsBold];
      BorderColor := RGBToColor(0, 68, 135);
      Gradient1.StartColor := RGBToColor(82, 136, 189);
      Gradient1.EndColor := RGBToColor(47, 101, 154);
      Gradient1EndPercent := 100;
      LightWidth := 1;
    end;
    with BodyHover do
    begin
      Font.Color := clWhite;
      Font.Style := [fsBold];
      BorderColor := RGBToColor(0, 68, 135);
      Gradient1.StartColor := RGBToColor(82, 136, 189);
      Gradient1.EndColor := RGBToColor(47, 101, 154);
      Gradient1EndPercent := 100;
      LightWidth := 1;
    end;
    with BodyClicked do
    begin
      Font.Color := clWhite;
      Font.Style := [fsBold];
      BorderColor := RGBToColor(0, 68, 135);
      Gradient1.StartColor := RGBToColor(82, 136, 189);
      Gradient1.EndColor := RGBToColor(47, 101, 154);
      Gradient1EndPercent := 100;
      LightWidth := 1;
      LightColor := clBlack;
    end;
  end;
end;

procedure DrawGreenBodyButton(AButton: TBGRAButton);
begin
  with AButton do
  begin
    //TextCanvas := True;
    TextShadowColor := clBlack;
    TextShadowOffsetX := 0;
    TextShadowOffsetY := -1;
    TextShadowRadius := 0;
    RoundX := 3;
    RoundY := 3;
    with BodyNormal do
    begin
      Font.Color := clWhite;
      Font.Style := [fsBold];
      BorderColor := RGBToColor(45, 82, 0);
      Gradient1.StartColor := RGBToColor(131, 184, 66);
      Gradient1.EndColor := RGBToColor(98, 151, 33);
      Gradient1EndPercent := 100;
      LightWidth := 1;
    end;
    with BodyHover do
    begin
      Font.Color := clWhite;
      Font.Style := [fsBold];
      BorderColor := RGBToColor(45, 82, 0);
      Gradient1.StartColor := RGBToColor(131, 184, 66);
      Gradient1.EndColor := RGBToColor(98, 151, 33);
      Gradient1EndPercent := 100;
      LightWidth := 1;
    end;
    with BodyClicked do
    begin
      Font.Color := clWhite;
      Font.Style := [fsBold];
      BorderColor := RGBToColor(45, 82, 0);
      Gradient1.StartColor := RGBToColor(131, 184, 66);
      Gradient1.EndColor := RGBToColor(98, 151, 33);
      Gradient1EndPercent := 100;
      LightWidth := 1;
      LightColor := clBlack;
    end;
  end;
end;

procedure DrawSilverBodyButton(AButton: TBGRAButton);
begin
  with AButton do
  begin
    //TextCanvas := True;
    TextShadowColor := clWhite;
    TextShadowOffsetX := 0;
    TextShadowOffsetY := 1;
    TextShadowRadius := 0;
    RoundX := 3;
    RoundY := 3;
    with BodyNormal do
    begin
      Font.Color := RGBToColor(91, 99, 106);
      Font.Style := [fsBold];
      BorderColor := RGBToColor(163, 166, 168);
      Gradient1.StartColor := RGBToColor(235, 239, 242);
      Gradient1.EndColor := RGBToColor(213, 217, 220);
      Gradient1EndPercent := 100;
      LightWidth := 1;
      LightOpacity := 175;
    end;
    with BodyHover do
    begin
      Font.Color := RGBToColor(91, 99, 106);
      Font.Style := [fsBold];
      BorderColor := RGBToColor(163, 166, 168);
      Gradient1.StartColor := RGBToColor(235, 239, 242);
      Gradient1.EndColor := RGBToColor(213, 217, 220);
      Gradient1EndPercent := 100;
      LightWidth := 1;
      LightOpacity := 175;
    end;
    with BodyClicked do
    begin
      Font.Color := RGBToColor(91, 99, 106);
      Font.Style := [fsBold];
      BorderColor := RGBToColor(163, 166, 168);
      Gradient1.StartColor := RGBToColor(235, 239, 242);
      Gradient1.EndColor := RGBToColor(213, 217, 220);
      Gradient1EndPercent := 100;
      LightWidth := 1;
      LightColor := clBlack;
    end;
  end;
end;

procedure DrawSilverBodyButtonDropDown(AButton: TBGRAButton);
begin
  with AButton do
  begin
    //TextCanvas := True;
    TextShadowColor := clWhite;
    TextShadowOffsetX := 0;
    TextShadowOffsetY := 1;
    TextShadowRadius := 0;
    RoundX := 3;
    RoundY := 3;
    with BorderStyle do
    begin
      BottomRight := bsSquare;
      TopRight := bsSquare;
    end;
    with BorderStyleDropDown do
    begin
      BottomLeft := bsSquare;
      TopLeft := bsSquare;
    end;
    with BodyNormal do
    begin
      Font.Color := RGBToColor(91, 99, 106);
      Font.Style := [fsBold];
      BorderColor := RGBToColor(163, 166, 168);
      Gradient1.StartColor := RGBToColor(235, 239, 242);
      Gradient1.EndColor := RGBToColor(213, 217, 220);
      Gradient1EndPercent := 100;
      LightWidth := 1;
      LightOpacity := 175;
    end;
    with BodyHover do
    begin
      Font.Color := RGBToColor(91, 99, 106);
      Font.Style := [fsBold];
      BorderColor := RGBToColor(163, 166, 168);
      Gradient1.StartColor := RGBToColor(235, 239, 242);
      Gradient1.EndColor := RGBToColor(213, 217, 220);
      Gradient1EndPercent := 100;
      LightWidth := 1;
      LightOpacity := 175;
    end;
    with BodyClicked do
    begin
      Font.Color := RGBToColor(91, 99, 106);
      Font.Style := [fsBold];
      BorderColor := RGBToColor(163, 166, 168);
      Gradient1.StartColor := RGBToColor(235, 239, 242);
      Gradient1.EndColor := RGBToColor(213, 217, 220);
      Gradient1EndPercent := 100;
      LightWidth := 1;
      LightColor := clBlack;
    end;
  end;
end;

procedure DrawSilverBodyButtonSquared(AButton: TBGRAButton);
begin
  with AButton do
  begin
    //TextCanvas := True;
    TextShadowColor := clWhite;
    TextShadowOffsetX := 0;
    TextShadowOffsetY := 1;
    TextShadowRadius := 0;
    RoundX := 3;
    RoundY := 3;
    with BorderStyle do
    begin
      BottomLeft := bsSquare;
      BottomRight := bsSquare;
      TopLeft := bsSquare;
      TopRight := bsSquare;
    end;
    with BorderStyleDropDown do
    begin
      BottomLeft := bsSquare;
      BottomRight := bsSquare;
      TopLeft := bsSquare;
      TopRight := bsSquare;
    end;
    with BodyNormal do
    begin
      Font.Color := RGBToColor(91, 99, 106);
      Font.Style := [fsBold];
      BorderColor := RGBToColor(163, 166, 168);
      Gradient1.StartColor := RGBToColor(235, 239, 242);
      Gradient1.EndColor := RGBToColor(213, 217, 220);
      Gradient1EndPercent := 100;
      LightWidth := 1;
      LightOpacity := 175;
    end;
    with BodyHover do
    begin
      Font.Color := RGBToColor(91, 99, 106);
      Font.Style := [fsBold];
      BorderColor := RGBToColor(163, 166, 168);
      Gradient1.StartColor := RGBToColor(235, 239, 242);
      Gradient1.EndColor := RGBToColor(213, 217, 220);
      Gradient1EndPercent := 100;
      LightWidth := 1;
      LightOpacity := 175;
    end;
    with BodyClicked do
    begin
      Font.Color := RGBToColor(91, 99, 106);
      Font.Style := [fsBold];
      BorderColor := RGBToColor(163, 166, 168);
      Gradient1.StartColor := RGBToColor(235, 239, 242);
      Gradient1.EndColor := RGBToColor(213, 217, 220);
      Gradient1EndPercent := 100;
      LightWidth := 1;
      LightColor := clBlack;
    end;
  end;
end;

procedure DrawSilverToolBar(ABitmap: TBGRABitmap);
begin
  with ABitmap do
  begin
    GradientFill(0, 0, Width, Height,
      BGRA(233, 236, 240, 255), BGRA(218, 221, 225, 255), gtLinear,
      PointF(0, 0), PointF(0, Height), dmSet);
    SetHorizLine(0, 0, Width - 1, BGRA(191, 195, 199, 255));
    SetHorizLine(0, 1, Width - 1, BGRA(253, 254, 254, 255));
    SetHorizLine(0, Height - 1, Width - 1, BGRA(191, 195, 199, 255));
  end;
end;

procedure DrawSilverStatusBar(ABitmap: TBGRABitmap);
begin
  with ABitmap do
  begin
    Fill(BGRA(228, 232, 236, 255));
    SetHorizLine(0, 0, Width - 1, BGRA(211, 215, 220, 255));
  end;
end;

end.

