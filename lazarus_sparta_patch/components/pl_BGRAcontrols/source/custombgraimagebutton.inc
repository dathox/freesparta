{%MainUnit ../custombgraimagebutton.pas}

uses
  SysUtils, BGRASliceScaling, FileUtil, IniFiles {$IFDEF WINDOWS}, mmsystem{$ENDIF};

{ TCustomResizeBitmap }

procedure TCustomResizeBitmap.SetFBorderHeight(AValue: integer);
begin
  if FBorderHeight=AValue then Exit;
  FBorderHeight:=AValue;

  if Enable then
  begin
    FOwner.Perform(CM_CHANGED, 0, 0);
    FOwner.Invalidate;
  end;
end;

procedure TCustomResizeBitmap.SetFBorderWidth(AValue: integer);
begin
  if FBorderWidth=AValue then Exit;
  FBorderWidth:=AValue;

  if Enable then
  begin
    FOwner.Perform(CM_CHANGED, 0, 0);
    FOwner.Invalidate;
  end;
end;

procedure TCustomResizeBitmap.SetFDrawMode(AValue: TDrawMode);
begin
  if FDrawMode=AValue then Exit;
  FDrawMode:=AValue;

  if Enable then
  begin
    FOwner.Perform(CM_CHANGED, 0, 0);
    FOwner.Invalidate;
  end;
end;

procedure TCustomResizeBitmap.SetFEnable(AValue: boolean);
begin
  Exit;
  {
  if FEnable=AValue then Exit;
  FEnable:=AValue;

  FOwner.Perform(CM_CHANGED, 0, 0);
  FOwner.Invalidate;
  }
end;

procedure TCustomResizeBitmap.SetFFillBottom(AValue: boolean);
begin
  if FFillBottom=AValue then Exit;
  FFillBottom:=AValue;

  if Enable then
  begin
    FOwner.Perform(CM_CHANGED, 0, 0);
    FOwner.Invalidate;
  end;
end;

procedure TCustomResizeBitmap.SetFFillCenter(AValue: boolean);
begin
  if FFillCenter=AValue then Exit;
  FFillCenter:=AValue;

  if Enable then
  begin
    FOwner.Perform(CM_CHANGED, 0, 0);
    FOwner.Invalidate;
  end;
end;

procedure TCustomResizeBitmap.SetFFillLeft(AValue: boolean);
begin
  if FFillLeft=AValue then Exit;
  FFillLeft:=AValue;

  if Enable then
  begin
    FOwner.Perform(CM_CHANGED, 0, 0);
    FOwner.Invalidate;
  end;
end;

procedure TCustomResizeBitmap.SetFFillRight(AValue: boolean);
begin
  if FFillRight=AValue then Exit;
  FFillRight:=AValue;

  if Enable then
  begin
    FOwner.Perform(CM_CHANGED, 0, 0);
    FOwner.Invalidate;
  end;
end;

procedure TCustomResizeBitmap.SetFFillTop(AValue: boolean);
begin
  if FFillTop=AValue then Exit;
  FFillTop:=AValue;

  if Enable then
  begin
    FOwner.Perform(CM_CHANGED, 0, 0);
    FOwner.Invalidate;
  end;
end;

procedure TCustomResizeBitmap.SetFResampleFilter(AValue: TResampleFilter);
begin
  if FResampleFilter=AValue then Exit;
  FResampleFilter:=AValue;

  if Enable then
  begin
    FOwner.Perform(CM_CHANGED, 0, 0);
    FOwner.Invalidate;
  end;
end;

procedure TCustomResizeBitmap.SetFResampleMode(AValue: TResampleMode);
begin
  if FResampleMode=AValue then Exit;
  FResampleMode:=AValue;

  if Enable then
  begin
    FOwner.Perform(CM_CHANGED, 0, 0);
    FOwner.Invalidate;
  end;
end;

constructor TCustomResizeBitmap.Create(AOwner: TControl);
begin
  FOwner := AOwner;
  FBorderHeight := 0;
  FBorderWidth := 0;
  FDrawMode := dmDrawWithTransparency;
  FResampleFilter := rfBestQuality;
  FResampleMode := rmFineResample;
  FEnable := True;
  FFillLeft:= False;
  FFillTop:= False;
  FFillRight:= False;
  FFillBottom:= False;
  FFillCenter:= False;
  inherited Create;
end;

destructor TCustomResizeBitmap.Destroy;
begin
  inherited Destroy;
end;

{ TCustomBGRAImageButton }

procedure TCustomBGRAImageButton.SetFBitmapFile(AValue: string);
begin
  if FBitmapFile=AValue then Exit;
  FBitmapFile:=AValue;
end;

procedure TCustomBGRAImageButton.SetFAnimation(AValue: boolean);
begin
  if FAnimation=AValue then Exit;
  FAnimation:=AValue;

  FTimerStep := 0;
end;

procedure TCustomBGRAImageButton.SetFAutoSizeExtraX(AValue: integer);
begin
  if FAutoSizeExtraX=AValue then Exit;
  FAutoSizeExtraX:=AValue;

  InvalidatePreferredSize;
  AdjustSize;
end;

procedure TCustomBGRAImageButton.SetFAutoSizeExtraY(AValue: integer);
begin
  if FAutoSizeExtraY=AValue then Exit;
  FAutoSizeExtraY:=AValue;

  InvalidatePreferredSize;
  AdjustSize;
end;

procedure TCustomBGRAImageButton.SetFBitmapOptions(AValue: TCustomResizeBitmap
  );
begin
  if FBitmapOptions=AValue then Exit;
  FBitmapOptions:=AValue;
  UpdateBmp(nil);
  Invalidate;
end;

procedure TCustomBGRAImageButton.SetFTextVisible(AValue: boolean);
begin
  if FTextVisible=AValue then Exit;
  FTextVisible:=AValue;

  InvalidatePreferredSize;
  AdjustSize;

  Invalidate;
end;

procedure TCustomBGRAImageButton.SetFBmp(const AValue: TBitmap);
begin
  if FBmp = AValue then
    exit;
  FBmp.Assign(AValue);

  InvalidatePreferredSize;
  AdjustSize;
  Changed;

  Invalidate;
end;

procedure TCustomBGRAImageButton.SetFChecked(AValue: boolean);
begin
  if FChecked = AValue then
    Exit;
  FChecked := AValue;
  if FChecked = False then
  begin
    FBmpState := bsUp;
    Invalidate;
  end
  else
  begin
    if FBmpState <> FCheckedState then
    begin
      FBmpState := FCheckedState;
      Invalidate;
    end;
  end;
end;

{------------------------------------------------------------------------------}

procedure TCustomBGRAImageButton.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  inherited MouseDown(Button, Shift, X, Y);

  if Button = mbLeft then
  begin
    if Enabled and Sound then
      DoPlaySound(SoundClick);

    if Animation then
      ATimerStep := 0;

    DoButtonDown;
  end;
end;

procedure TCustomBGRAImageButton.MouseEnter;
begin
  inherited MouseEnter;

  if Enabled and Animation then
  begin
    ATimer.Enabled := True;
    ATimerStep := 0;
  end;

  if Enabled and Sound then
    DoPlaySound(SoundEnter);
  DoButtonEnter;
end;

procedure TCustomBGRAImageButton.MouseLeave;
begin
  inherited MouseLeave;

  if Animation then
  begin
    ATimer.Enabled := False;
    ATimerStep := 0;
  end;

  if Animation and Toggle and Checked then
  begin
    ATimerStep := 0;
    Invalidate;
  end;

  DoButtonLeave;
end;

procedure TCustomBGRAImageButton.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  inherited MouseUp(Button, Shift, X, Y);

  DoButtonUp;
end;

procedure TCustomBGRAImageButton.Click;
var
  Form: TCustomForm;
begin
  if ModalResult <> mrNone then
  begin
    Form := GetParentForm(Self);
    if Form <> nil then
      Form.ModalResult := ModalResult;
  end;

  if Toggle then
    Checked := not Checked;

  inherited Click;
end;

procedure TCustomBGRAImageButton.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: integer; WithThemeSpace: boolean);
var
  s: TSize;
  ax, ay: integer;
begin
  if TextVisible then
  begin
  s := ABGRAText.TextSize(Caption);

  if Shadow.Visible then
  begin
    if Shadow.OffsetX < 0 then
      ax := (Shadow.OffsetX) - (Shadow.OffsetX * 2)
    else
      ax := Shadow.OffsetX;

    if Shadow.OffsetY < 0 then
      ay := (Shadow.OffsetY) - (Shadow.OffsetY * 2)
    else
      ay := Shadow.OffsetY;

    Inc(s.cx, AutoSizeExtraHorizontal + 2 * ax + 2 * Shadow.Radius);
    Inc(s.cy, AutoSizeExtraVertical + 2 * ay + 2 * Shadow.Radius);
  end
  else
  begin
    Inc(s.cx, AutoSizeExtraHorizontal);
    Inc(s.cy, AutoSizeExtraVertical);
  end
  end
  else
  begin
    s.cx := AutoSizeExtraHorizontal;
    s.cy := AutoSizeExtraVertical;
  end;

  if (Bitmap <> nil) or (Bitmap.Width > 1) and (Bitmap.Height > 1) then
  begin
    if s.cx > Bitmap.Width then
     PreferredWidth := s.cx
    else
     PreferredWidth := Bitmap.Width;

    if s.cy > (Bitmap.Height div 4) then
      PreferredHeight := s.cy
    else
      PreferredHeight := (Bitmap.Height div 4);
  end;
end;

procedure TCustomBGRAImageButton.CMChanged(var Message: TLMessage);
begin
  UpdateBmp(nil);
end;

procedure TCustomBGRAImageButton.DoButtonDown;
var
  NewState: TButtonState;
begin
  NewState := bsDown;

  if Checked then
  begin
    ABmpState := ACheckedState;
    NewState  := ABmpState;
  end;

  if NewState <> ABmpState then
  begin
    ABmpState := NewState;
    Invalidate;
  end;
end;

procedure TCustomBGRAImageButton.DoButtonEnter;
var
  NewState: TButtonState;
begin
  if Enabled then
    NewState := bsHot
  else
  begin
    ABmpState := bsUp;
    NewState  := ABmpState;
  end;

  if Checked then
  begin
    ABmpState := ACheckedState;
    NewState  := ABmpState;
  end;

  if NewState <> ABmpState then
  begin
    ABmpState := NewState;
    Invalidate;
  end;
end;

procedure TCustomBGRAImageButton.DoButtonLeave;
var
  NewState: TButtonState;
begin
  if Enabled then
    NewState := bsUp
  else
  begin
    ABmpState := bsUp;
    NewState  := ABmpState;
  end;

  if Checked then
  begin
    ABmpState := ACheckedState;
    NewState  := ABmpState;
  end;

  if NewState <> ABmpState then
  begin
    ABmpState := NewState;
    Invalidate;
  end;
end;

procedure TCustomBGRAImageButton.DoButtonUp;
var
  NewState: TButtonState;
  p: TPoint;
begin
  p := ScreenToClient(Mouse.CursorPos);

  if (p.x >= 0) and (p.x <= Width) and (p.y >= 0) and (p.y <= Height) then
    NewState := bsHot
  else
  begin
    NewState := bsUp;
    if Animation then
      ATimer.Enabled := False;
  end;

  if Checked then
  begin
    ABmpState := ACheckedState;
    NewState  := ABmpState;
  end;

  if NewState <> ABmpState then
  begin
    ABmpState := NewState;
    Invalidate;
  end;
end;

{------------------------------------------------------------------------------}

procedure TCustomBGRAImageButton.RealSetText(const Value: TCaption);
begin
  inherited RealSetText(Value);
  if TextVisible then
  begin
    UpdateTxt;
    Invalidate;
  end;
end;

procedure TCustomBGRAImageButton.CMFontChanged(var Message: TLMessage);
begin
  if TextVisible then
    UpdateTxt;
end;

{------------------------------------------------------------------------------}

procedure TCustomBGRAImageButton.DoPlaySound(AFileName: string);
begin
  if FileExistsUTF8(AFileName) then
  begin
  {$IFDEF MSWINDOWS}
    if Assigned(FOnPlaySound) then
      FOnPlaySound(Self, AFileName, ABmpState);
    PlaySound(PChar(AFileName), 0, SND_ASYNC);
  {$ELSE}
    exit;
  {$ENDIF}
  end;
end;

{------------------------------------------------------------------------------}

procedure TCustomBGRAImageButton.Paint;

  procedure DrawFrame;
  begin
    with inherited Canvas do
    begin
      Pen.Color := clBlack;
      Pen.Style := psDash;
      MoveTo(0, 0);
      LineTo(Self.Width - 1, 0);
      LineTo(Self.Width - 1, Self.Height - 1);
      LineTo(0, Self.Height - 1);
      LineTo(0, 0);
    end;
  end;

var
  NewState: TButtonState;
begin
  if csDesigning in ComponentState then
    DrawFrame;

  if (Bitmap = nil) or (Bitmap.Width < 1) and (Bitmap.Height < 1) then
    exit;

  if (ABGRA.Width <> Width) or (ABGRA.Height <> Height) then
    UpdateBmp(nil);

  if TextVisible then
    if (ABGRAText.Width <> Width) or (ABGRAText.Height <> Height) then
      UpdateTxt;

  ABGRA.Fill(BGRAPixelTransparent);

  if Checked then
    NewState := ACheckedState
  else
    NewState := ABmpState;

  if Enabled and not ATimer.Enabled then
    case NewState of
      bsUp: ABGRA.PutImage(0, 0, ABGRAUp, BitmapOptions.DrawMode);
      bsDown: ABGRA.PutImage(0, 0, ABGRADown, BitmapOptions.DrawMode);
      bsHot: ABGRA.PutImage(0, 0, ABGRAHot, BitmapOptions.DrawMode);
      bsDisabled: ABGRA.PutImage(0, 0, ABGRADisabled, BitmapOptions.DrawMode);
    end
  else if not Enabled and not ATimer.Enabled then
    ABGRA.PutImage(0, 0, ABGRADisabled, BitmapOptions.DrawMode);

  if Enabled and ATimer.Enabled then
    case NewState of
      bsUp: ABGRA.PutImage(0, 0, ABGRAUp, BitmapOptions.DrawMode);
      bsDown: begin
        ABGRA.PutImage(0, 0, ABGRADown, BitmapOptions.DrawMode);
        ABGRA.PutImage(0, 0, ABGRAHot, dmDrawWithTransparency, ATimerStep); // always transparent
      end;
      bsHot: begin
        ABGRA.PutImage(0, 0, ABGRAUp, BitmapOptions.DrawMode);
        ABGRA.PutImage(0, 0, ABGRAHot, dmDrawWithTransparency, ATimerStep); // always transparent
      end;
      bsDisabled: ABGRA.PutImage(0, 0, ABGRADisabled, BitmapOptions.DrawMode);
    end
  else if not Enabled and ATimer.Enabled then
    ABGRA.PutImage(0, 0, ABGRADisabled, BitmapOptions.DrawMode);

  if TextVisible then
    FBGRA.PutImage(0, 0, ABGRAText, dmDrawWithTransparency); // always transparent

  if Assigned(FOnRedraw) then
    FOnRedraw(Self, ABGRA, NewState);

  {$IFDEF DEBUG}
  //Debug: display in button Invalidate Count, Update Bitmap Count, Update Text Count.
  Inc(FInvalidateCount);
  ABGRA.Textout(0, 0, IntToStr(FInvalidateCount) + ',' + IntToStr(FUpdateCount) +
    ',' + IntToStr(FUpdateCountTxt), BGRAWhite);
  ABGRA.Textout(1, 1, IntToStr(FInvalidateCount) + ',' + IntToStr(FUpdateCount) +
    ',' + IntToStr(FUpdateCountTxt), BGRABlack);
  {$ENDIF}

  ABGRA.Draw(Canvas, Rect(0, 0, Width, Height), False); // always transparent
end;

procedure TCustomBGRAImageButton.Animate(Sender: TObject);
begin
  if ATimerStep = 0 then
    ATimerIncreasing := true;

  if ATimerStep = 255 then
    ATimerIncreasing := false;

  if ATimerIncreasing then
    Inc(FTimerStep, +17)
  else
    Inc(FTimerStep, -17);

  Invalidate;
end;

procedure TCustomBGRAImageButton.UpdateBmp(Sender: TObject);
var
  themeutil: TBGRAMultiSliceScaling;
  debug: boolean;
  i: integer;
begin
  if csLoading in ComponentState then
    exit;

  if (Bitmap = nil) or (Bitmap.Width < 1) and (Bitmap.Height < 1) then
    exit;

  ABGRA.SetSize(Width, Height);

  themeutil := TBGRAMultiSliceScaling.Create(FBmp, BitmapOptions.BorderHeight, BitmapOptions.BorderWidth, BitmapOptions.BorderHeight, BitmapOptions.BorderWidth, 4, sdVertical);

  for i:=0 to high(themeutil.SliceScalingArray) do
  begin
  themeutil.SliceScalingArray[i].ResampleMode:= BitmapOptions.ResampleMode;
  themeutil.SliceScalingArray[i].DrawMode := BitmapOptions.DrawMode;
  themeutil.SliceScalingArray[i].ResampleFilter := BitmapOptions.ResampleFilter;
  themeutil.SliceScalingArray[i].SliceRepeat[srpTop] := BitmapOptions.FillTop;
  themeutil.SliceScalingArray[i].SliceRepeat[srpLeft] := BitmapOptions.FillLeft;
  themeutil.SliceScalingArray[i].SliceRepeat[srpMiddleHorizontal] := BitmapOptions.FillCenter;
  themeutil.SliceScalingArray[i].SliceRepeat[srpMiddleVertical] := BitmapOptions.FillCenter;
  themeutil.SliceScalingArray[i].SliceRepeat[srpRight] := BitmapOptions.FillRight;
  themeutil.SliceScalingArray[i].SliceRepeat[srpBottom] := BitmapOptions.FillBottom;
  end;

  {$IFDEF DEBUG}
  debug := true;
  {$ELSE}
  debug := false;
  {$ENDIF}

  FBGRAUp.Free;
  FBGRAUp := TBGRABitmap.Create(Width, Height);
  themeutil.Draw(0, FBGRAUp, 0, 0, Width, Height, debug);

  FBGRAHot.Free;
  FBGRAHot := TBGRABitmap.Create(Width, Height);
  themeutil.Draw(1, FBGRAHot, 0, 0, Width, Height, debug);

  FBGRADown.Free;
  FBGRADown := TBGRABitmap.Create(Width, Height);
  themeutil.Draw(2, FBGRADown, 0, 0, Width, Height, debug);

  FBGRADisabled.Free;
  FBGRADisabled := TBGRABitmap.Create(Width, Height);
  themeutil.Draw(3, FBGRADisabled, 0, 0, Width, Height, debug);

  FreeAndNil(themeutil);

  InvalidatePreferredSize;
  AdjustSize;

  if Sender is TBitmap then
    Invalidate;

  {$IFDEF DEBUG}
  Inc(FUpdateCount);
  {$ENDIF}
end;

procedure TCustomBGRAImageButton.UpdateTxt;

function TextShadow(AWidth,AHeight: Integer; AText: String; AFontHeight: Integer;
  ATextColor,AShadowColor: TBGRAPixel; AOffSetX,AOffSetY: Integer; ARadius: Integer = 0;
  AFontStyle: TFontStyles = []; AFontName: String = 'Default'; AShowText: Boolean = True;
  AShowShadow: Boolean = True; AFontQuality: TBGRAFontQuality = fqFineAntialiasing): TBGRACustomBitmap;
var
  bmpOut,bmpSdw: TBGRABitmap; OutTxtSize: TSize; OutX,OutY: Integer;
begin
  bmpOut:= TBGRABitmap.Create(AWidth,AHeight);
  bmpOut.FontAntialias:= True;
  bmpOut.FontHeight:= AFontHeight;
  bmpOut.FontStyle:= AFontStyle;
  bmpOut.FontName:= AFontName;
  bmpOut.FontQuality:= AFontQuality;

  OutTxtSize:= bmpOut.TextSize(AText);
  OutX:= Round(AWidth/2) - Round(OutTxtSize.cx/2);
  OutY:= Round(AHeight/2) - Round(OutTxtSize.cy/2);

  if AShowShadow then
  begin
  bmpSdw:= TBGRABitmap.Create(OutTxtSize.cx+2*ARadius,OutTxtSize.cy+2*ARadius);
  bmpSdw.FontAntialias:= True;
  bmpSdw.FontHeight:= AFontHeight;
  bmpSdw.FontStyle:= AFontStyle;
  bmpSdw.FontName:= AFontName;
  bmpSdw.FontQuality:= AFontQuality;

  bmpSdw.TextOut(ARadius,ARadius,AText,AShadowColor);
  BGRAReplace(bmpSdw,bmpSdw.FilterBlurRadial(ARadius,rbFast));
  bmpOut.PutImage(OutX+AOffSetX-ARadius,OutY+AOffSetY-ARadius,bmpSdw,dmDrawWithTransparency);
  bmpSdw.Free;
  end;

  if AShowText = True then bmpOut.TextOut(OutX,OutY,AText,ATextColor);

  Result:= bmpOut;
end;

begin
  if csLoading in ComponentState then
    exit;

  if (Bitmap = nil) or (FBGRA.Width < 1) and (FBGRA.Height < 1) then
    exit;

  ABGRAText.Fill(BGRAPixelTransparent);

  BGRAReplace(FBGRAText, TextShadow(Width, Height, Caption, Font.Height,
    ColorToBGRA(ColorToRGB(Font.Color), 255),
    ColorToBGRA(ColorToRGB(FShadow.Color), Shadow.Alpha), Shadow.OffsetX,
    Shadow.OffsetY, Shadow.Radius, Font.Style, Font.Name, True, Shadow.Visible) as TBGRABitmap);

  InvalidatePreferredSize;
  AdjustSize;

  {$IFDEF DEBUG}
  Inc(FUpdateCountTxt);
  {$ENDIF}
end;

{------------------------------------------------------------------------------}

constructor TCustomBGRAImageButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);

  AutoSizeExtraHorizontal := 24;
  AutoSizeExtraVertical := 8;
  Font.Color := clBlack;
  Font.Height := 18;

  FBGRA := TBGRABitmap.Create;
  FBGRAText := TBGRABitmap.Create;

  FBmp := TBitmap.Create;
  FBmp.OnChange := @UpdateBmp;

  FBGRAUp := nil;
  FBGRADown := nil;
  FBGRADisabled := nil;
  FBGRAHot := nil;

  FTimer := TTimer.Create(Self);
  FTimer.Enabled := False;
  FTimer.OnTimer := @Animate;
  FTimer.Interval := 30;
  FTimerStep := 0;

  Shadow := TBGRATextEffectShadow.Create(Self);
  BitmapOptions := TCustomResizeBitmap.Create(Self);

  FCheckedState := bsDown;
  FTextVisible := True;
end;

destructor TCustomBGRAImageButton.Destroy;
begin
  if Bitmap <> nil then
    FBmp.Free;

  FBGRA.Free;
  FBGRAText.Free;

  FBGRAUp.Free;
  FBGRADown.Free;
  FBGRADisabled.Free;
  FBGRAHot.Free;

  FTimer.OnTimer := nil;
  FTimer.Free;

  Shadow.Free;
  BitmapOptions.Free;

  inherited Destroy;
end;

procedure TCustomBGRAImageButton.Assign(Source: TPersistent);
begin
  if Source is TCustomBGRAImageButton then
  begin
    Loading;
    FBmp.Assign(TCustomBGRAImageButton(Source).FBmp);
    FBitmapFile := TCustomBGRAImageButton(Source).FBitmapFile;
    FAnimation := TCustomBGRAImageButton(Source).FAnimation;
    FChecked := TCustomBGRAImageButton(Source).FChecked;
    FSound := TCustomBGRAImageButton(Source).FSound;
    FSoundEnter := TCustomBGRAImageButton(Source).FSoundEnter;
    FSoundClick := TCustomBGRAImageButton(Source).FSoundClick;
    FModalResult := TCustomBGRAImageButton(Source).FModalResult;
    FToggle := TCustomBGRAImageButton(Source).FToggle;
    FAutoSizeExtraY := TCustomBGRAImageButton(Source).FAutoSizeExtraY;
    FAutoSizeExtraX := TCustomBGRAImageButton(Source).FAutoSizeExtraX;
    FTextVisible := TCustomBGRAImageButton(Source).FTextVisible;
    { Assign Shadow }
    FShadow.Alpha := TCustomBGRAImageButton(Source).FShadow.Alpha;
    FShadow.Color := TCustomBGRAImageButton(Source).FShadow.Color;
    FShadow.OffsetX := TCustomBGRAImageButton(Source).FShadow.OffsetX;
    FShadow.OffsetY := TCustomBGRAImageButton(Source).FShadow.OffsetY;
    FShadow.Radius := TCustomBGRAImageButton(Source).FShadow.Radius;
    FShadow.Visible := TCustomBGRAImageButton(Source).FShadow.Visible;
    { Assign BitmapOptions}
    FBitmapOptions.BorderHeight := TCustomBGRAImageButton(Source).FBitmapOptions.BorderHeight;
    FBitmapOptions.BorderWidth := TCustomBGRAImageButton(Source).FBitmapOptions.BorderWidth;
    FBitmapOptions.DrawMode := TCustomBGRAImageButton(Source).FBitmapOptions.DrawMode;
    FBitmapOptions.Enable := TCustomBGRAImageButton(Source).FBitmapOptions.Enable;
    FBitmapOptions.FillBottom := TCustomBGRAImageButton(Source).FBitmapOptions.FillBottom;
    FBitmapOptions.FillCenter := TCustomBGRAImageButton(Source).FBitmapOptions.FillCenter;
    FBitmapOptions.FillLeft := TCustomBGRAImageButton(Source).FBitmapOptions.FillLeft;
    FBitmapOptions.FillRight := TCustomBGRAImageButton(Source).FBitmapOptions.FillRight;
    FBitmapOptions.FillTop := TCustomBGRAImageButton(Source).FBitmapOptions.FillTop;
    FBitmapOptions.ResampleFilter := TCustomBGRAImageButton(Source).FBitmapOptions.ResampleFilter;
    FBitmapOptions.ResampleMode := TCustomBGRAImageButton(Source).FBitmapOptions.ResampleMode;
    { Assign Font }
    Font.Assign(TCustomBGRAImageButton(Source).Font);
    Loaded;

    UpdateBmp(nil);
    if TextVisible then
      UpdateTxt;
    Invalidate;
  end
  else
    inherited Assign(Source);
end;

function TCustomBGRAImageButton.BitmapLoadFromFile(AFileName: String): Boolean;
{
* Usage *
  button.BitmapLoadFromFile(some_path + button.BitmapFile);
}
var
  temp: TBGRABitmap;
begin
  if FileExistsUTF8(AFileName) then
  begin
    temp := TBGRABitmap.Create(AFileName);
    Bitmap := temp.Bitmap;
    temp.Free;
    Result := True;
  end
  else
    Result := False;
end;

function TCustomBGRAImageButton.LoadFromFile(AFileName: string): Boolean;

{
* Usage *
  button.LoadFromFile(some_path + 'button.ini');
  button.BitmapLoadFromFile(some_path + button.BitmapFile);
}

var
  temp: TMemIniFile;
begin
  if FileExistsUTF8(AFileName) then
  begin
    temp := TMemIniFile.Create(AFileName);
    with Self do
    begin
      // Image
      Animation := temp.ReadBool('default', 'Animation', False);
      BitmapFile := temp.ReadString('default', 'BitmapFile', 'button.png');
      // BitmapOptions
      with BitmapOptions do
      begin
        BorderHeight := temp.ReadInteger('default', 'BorderHeight', 5);
        BorderWidth := temp.ReadInteger('default', 'BorderWidth', 5);
        Enable := temp.ReadBool('default', 'Enable', False);
        FillBottom := temp.ReadBool('default', 'FillBottom', False);
        FillCenter := temp.ReadBool('default', 'FillCenter', False);
        FillRight := temp.ReadBool('default', 'FillRight', False);
        FillTop := temp.ReadBool('default', 'FillTop', False);
      end;
      // Sound
      Sound := temp.ReadBool('default', 'Sound', False);
      SoundClick := temp.ReadString('default', 'SoundClick', 'click.wav');
      SoundEnter := temp.ReadString('default', 'SoundEnter', 'enter.wav');
      // Text
      Caption := temp.ReadString('default', 'Caption', Caption);
      TextVisible := temp.ReadBool('default', 'TextVisible', True);
      Font.Color := BGRAToColor(StrToBGRA(temp.ReadString('default', 'FontColor', 'black')));
      Font.Name := temp.ReadString('default', 'FontName', 'default');
      Font.Height := temp.ReadInteger('default', 'FontHeight', 18);
      // Size
      Left := temp.ReadInteger('default', 'Left', Left);
      Top := temp.ReadInteger('default', 'Top', Top);
      Width := temp.ReadInteger('default', 'Width', Width);
      Height := temp.ReadInteger('default', 'Height', Height);
      // Shadow
      with Shadow do
      begin
        Visible := temp.ReadBool('default', 'ShadowVisible', True);
        Alpha := temp.ReadInteger('default', 'ShadowAlpha', 255);
        Color := BGRAToColor(StrToBGRA(temp.ReadString('default', 'ShadowColor', 'black')));
        OffsetX := temp.ReadInteger('default', 'ShadowOffsetX', 1);
        OffsetY := temp.ReadInteger('default', 'ShadowOffsetY', 1);
        Radius := temp.ReadInteger('default', 'ShadowRadius', 1);
      end;
      // Misc
      AutoSizeExtraHorizontal := temp.ReadInteger('default', 'AutoSizeExtraHorizontal', 24);
      AutoSizeExtraVertical := temp.ReadInteger('default', 'AutoSizeExtraVertical', 8);
      Checked := temp.ReadBool('default', 'Checked', False);
      Toggle := temp.ReadBool('default', 'Toggle', Toggle);
    end;
    temp.Free;
    Result := True;
  end
  else
    Result := False;
end;

function TCustomBGRAImageButton.SaveToFile(AFileName: string): boolean;

function BGRAToStrRGB(AColor: TBGRAPixel): string;
begin
  result := 'rgb(' + IntToStr(AColor.red) + ',' + IntToStr(AColor.green) + ',' + IntToStr(AColor.blue) + ')'
end;

var
  temp: TMemIniFile;
begin
  temp := TMemIniFile.Create(AFileName);
  // Image
  temp.WriteBool('default', 'Animation', Animation);
  temp.WriteString('default', 'BitmapFile', BitmapFile);
  // BitmapOptions
  temp.WriteInteger('default', 'BorderHeight', BitmapOptions.BorderHeight);
  temp.WriteInteger('default', 'BorderWidth', BitmapOptions.BorderWidth);
  temp.WriteBool('default', 'Enable', BitmapOptions.Enable);
  temp.WriteBool('default', 'FillBottom', BitmapOptions.FillBottom);
  temp.WriteBool('default', 'FillCenter', BitmapOptions.FillCenter);
  temp.WriteBool('default', 'FillRight', BitmapOptions.FillRight);
  temp.WriteBool('default', 'FillTop', BitmapOptions.FillTop);
  // Sound
  temp.WriteBool('default', 'Sound', Sound);
  temp.WriteString('default', 'SoundClick', SoundClick);
  temp.WriteString('default', 'SoundEnter', SoundEnter);
  // Text
  temp.WriteString('default', 'Caption', Caption);
  temp.WriteBool('default', 'TextVisible', TextVisible);
  temp.WriteString('default', 'FontColor', BGRAToStrRGB(ColorToBGRA(Font.Color)));
  temp.WriteString('default', 'FontName', Font.Name);
  temp.WriteInteger('default', 'FontHeight', Font.Height);
  // Size
  temp.WriteInteger('default', 'Left', Left);
  temp.WriteInteger('default', 'Top', Top);
  temp.WriteInteger('default', 'Width', Width);
  temp.WriteInteger('default', 'Height', Height);
  // Shadow
  temp.WriteBool('default', 'ShadowVisible', Shadow.Visible);
  temp.WriteInteger('default', 'ShadowAlpha', Shadow.Alpha);
  temp.WriteString('default', 'ShadowColor', BGRAToStrRGB(ColorToBGRA(Shadow.Color)));
  temp.WriteInteger('default', 'ShadowOffsetX', Shadow.OffsetX);
  temp.WriteInteger('default', 'ShadowOffsetY', Shadow.OffsetY);
  temp.WriteInteger('default', 'ShadowRadius', Shadow.Radius);
  // Misc
  temp.WriteInteger('default', 'AutoSizeExtraHorizontal', AutoSizeExtraHorizontal);
  temp.WriteInteger('default', 'AutoSizeExtraVertical', AutoSizeExtraVertical);
  temp.WriteBool('default', 'Checked', Checked);
  temp.WriteBool('default', 'Toggle', Toggle);
  // Save and Free
  temp.UpdateFile;
  temp.Free;

  if FileExistsUTF8(AFileName) then
    Result := True
  else
    Result := False;
end;
