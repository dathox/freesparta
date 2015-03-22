{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Maciej Izak

  DaThoX 2004-2015
  FreeSparta.com
}

unit sparta_EDTU_Form;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, StdCtrls, PropEdits, Generics.Collections,
  Graphics, LCLIntf, LCLType, Spin, Math, sparta_FakeForm, SrcEditorIntf, LazIDEIntf, SpartaAPI;

type
  //poDesigned,        // use bounds from the designer (read from stream)
  //poDefault,         // LCL decision (normally window manager decides)
  //poDefaultPosOnly,  // designed size and LCL position
  //poDefaultSizeOnly, // designed position and LCL size
  //poScreenCenter,    // center form on screen (depends on DefaultMonitor)
  //poDesktopCenter,   // center form on desktop (total of all screens)
  //poMainFormCenter,  // center form on main form (depends on DefaultMonitor)
  //poOwnerFormCenter  // center form on owner form (depends on DefaultMonitor)

  { TedtuFormEditor }

  TedtuFormEditor = class(TFrame, ISTADesignTimeUtil, ISTAExtendedDesignTimeUtil)
    cbShowDesigned: TCheckBox;
    cbResolution: TComboBox;
    eLeft: TSpinEdit;
    eTop: TSpinEdit;
    iDesktop: TImage;
    iForm: TImage;
    iPosition: TImage;
    lLeft: TLabel;
    lTop: TLabel;
    lDesignedPosition: TLabel;
    pDesktop: TPanel;
    pForm: TPanel;
    rgPosition: TRadioGroup;
    procedure cbResolutionSelect(Sender: TObject);
    procedure cbShowDesignedEditingDone(Sender: TObject);
    procedure eLeftChange(Sender: TObject);
    procedure rgPositionSelectionChanged(Sender: TObject);
  private
    FRefreshDisabled: Boolean;
    FSelectionList: TPersistentSelectionList;
    FRoot: TPersistent;
    FScreenWidth, FScreenHeight, FScaleScreenWidth, FScaleScreenHeight: Integer;
    inReposition : boolean;
    oldPos : TPoint;
    class var FResolutions: TDictionary<string, Integer>;

    function RootIsSelected: Boolean;
    procedure OnDesignRefreshPropertyValues;
    procedure OnDesignSetSelection(const ASelection: TPersistentSelectionList);

    function GetRoot: TPersistent;
    procedure SetRoot(AValue: TPersistent);
    function GetParent: TWinControl;
    function GetVisible: Boolean;

    procedure BeginUpdate;
    procedure EndUpdate;

    procedure ControlMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ControlMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ControlMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure OnFormPaint(Sender: TObject);
    procedure OnFormDblClick(Sender: TObject);
    procedure OnFormClick(Sender: TObject);
    function FormsStartPoint: TPoint;

    procedure AddForm(Sender: TObject);
    procedure DrawForm(AImage: TImage; AForm: TFakeForm; APenColor, ABrushColor: TColor);
    procedure RepaintForms;

    procedure RepaintDesktop;

    class constructor Init;
    class destructor Finit;
  public
    constructor Create(TheOwner: TComponent; ARoot: TPersistent); reintroduce;
    destructor Destroy; override;

    procedure RefreshValues;
    property Root: TPersistent read GetRoot write SetRoot;
  end;

  TedtuForm = class(TEDTU)
  public
    class function AvailableForRoot(ARoot: TPersistent): Boolean; override;
    class function CreateEDTUForRoot(TheOwner: TComponent; ARoot: TPersistent): ISTAExtendedDesignTimeUtil; override;
    class function GlyphName: string; override;
  end;

implementation

uses
  sparta_MainIDE;

{$R *.lfm}

{ TedtuForm }

class function TedtuForm.AvailableForRoot(ARoot: TPersistent): Boolean;
begin
  Result := ARoot is TForm;
end;

class function TedtuForm.CreateEDTUForRoot(TheOwner: TComponent;
  ARoot: TPersistent): ISTAExtendedDesignTimeUtil;
begin
  if not AvailableForRoot(ARoot) then
    Exit(nil);
  Result := TedtuFormEditor.Create(TheOwner, ARoot);
end;

class function TedtuForm.GlyphName: string;
begin
  Result := 'EDTU_FORM';
end;

{$R position_images.res}

procedure TedtuFormEditor.DrawForm(AImage: TImage; AForm: TFakeForm; APenColor, ABrushColor: TColor);
var
  i: integer;
begin
  with AImage do
  begin
    Visible := (AForm.Position in [poDesigned, poDefaultPosOnly, poDefaultSizeOnly]) or not cbShowDesigned.Checked;
    if not Visible then
      Exit;

    Left := FormsStartPoint.x + Round(AForm.Left/7.5);
    Top := FormsStartPoint.y + Round(AForm.Top/7.5);
    Width := Round(AForm.Width/7.5);
    Height:= Round(AForm.Height/7.5);
    Picture.Bitmap.SetSize(Width, Height);
    Picture.Bitmap.Canvas.Pen.Color:=APenColor;
    Picture.Bitmap.Canvas.Brush.Color:=ABrushColor;

    if (AForm.Width >= 135) and (AForm.Height >= 45) then
    begin
      Picture.Bitmap.Canvas.Rectangle(Rect(0,0,Width,6));
      Picture.Bitmap.Canvas.Rectangle(Rect(0,5,Width,Height));

      Picture.Bitmap.Canvas.Pen.Color:=APenColor;
      Picture.Bitmap.Canvas.Brush.Color:=APenColor;
      for i := 0 to 2 do
        Picture.Bitmap.Canvas.Ellipse(Rect(Width - (5+i*5),2,Width - (2+i*5),4));
    end
    else
      Picture.Bitmap.Canvas.Rectangle(Rect(0,0,Width,Height));
  end;
end;

procedure TedtuFormEditor.RepaintForms;
var
  i: Integer;
  LCtrl: TControl;
begin
  for i := 0 to pDesktop.ControlCount - 1 do
  begin
    LCtrl := pDesktop.Controls[i];
    if LCtrl.Tag <> 0 then
      DrawForm(TImage(LCtrl), TFakeForm(TDesignFormData(LCtrl.Tag).Form.Form), clBlack, clGray);
  end;
end;

procedure TedtuFormEditor.RepaintDesktop;
begin
  FScaleScreenWidth := Round(FScreenWidth/7.5);
  FScaleScreenHeight := Round(FScreenHeight/7.5);
  iDesktop.Picture.LoadFromResourceName(HINSTANCE, 'SPARTA_DESKTOP_256');
  iDesktop.Picture.Bitmap.Canvas.Pen.Color:=clWhite;
  iDesktop.Picture.Bitmap.Canvas.Brush.Style:=bsClear;
  iDesktop.Picture.Bitmap.Canvas.Rectangle(Rect(0,0,FScaleScreenWidth,FScaleScreenHeight));
end;

class constructor TedtuFormEditor.Init;
begin
  FResolutions := TDictionary<string, Integer>.Create;

  FResolutions.Add('640×480', 0);
  FResolutions.Add('800×480', 1);
  FResolutions.Add('800×600', 2);
  FResolutions.Add('1024×768', 3);
  FResolutions.Add('1280×720', 4);
  FResolutions.Add('1366×768', 5);
  FResolutions.Add('1280×800', 6);
  FResolutions.Add('1440×900', 7);
  FResolutions.Add('1280×1024', 8);
  FResolutions.Add('1600×1024', 9);
  FResolutions.Add('1400×1050', 10);
  FResolutions.Add('1024×600', 11);
  FResolutions.Add('1680×1050', 12);
  FResolutions.Add('1600×900', 13);
  FResolutions.Add('1920×1080', 14);
end;

class destructor TedtuFormEditor.Finit;
begin
  FResolutions.Free;
end;

{ TedtuFormEditor }

procedure TedtuFormEditor.cbShowDesignedEditingDone(Sender: TObject);
begin
  RepaintForms;
  RefreshValues;
end;

procedure TedtuFormEditor.eLeftChange(Sender: TObject);
begin
  if FRefreshDisabled then
    Exit;

  BeginUpdate;

  if (Sender = eLeft) and (eLeft.Text <> '#') then
    TFakeForm(FRoot).Left := eLeft.Value
  else if (Sender = eTop) and (eTop.Text <> '#') then
    TFakeForm(FRoot).Top := eTop.Value;
  //else if (Sender = eWidth) and (eWidth.Text <> '#') then
  //  FLayoutInfo.Width := eWidth.Value
  //else if (Sender = eHeight) and (eHeight.Text <> '#') then
  //  FLayoutInfo.Height := eHeight.Value;

  GlobalDesignHook.Modified(Self);
  GlobalDesignHook.RefreshPropertyValues;

  EndUpdate;
end;

procedure TedtuFormEditor.cbResolutionSelect(Sender: TObject);
var
  p: TPair<string, integer>;
  LPos: Integer;
begin
  for p in FResolutions do
    if p.Value = cbResolution.ItemIndex then
    begin
      LPos := Pos('×', p.Key);
      FScreenWidth := StrToInt(Copy(p.Key, 1, LPos - 1));
      FScreenHeight := StrToInt(Copy(p.Key, LPos + 2));
      RepaintDesktop;
      Exit;
    end;
end;

procedure TedtuFormEditor.rgPositionSelectionChanged(Sender: TObject);

  procedure LoadResImg(const AName: string);
  begin
    iPosition.Picture.LoadFromResourceName(HINSTANCE, AName);
  end;

begin
  case TPosition(rgPosition.ItemIndex) of
    poDesigned:        LoadResImg('POSITION_DESIGNED');
    poDefault:         LoadResImg('POSITION_DEFAULT');
    poDefaultPosOnly:  LoadResImg('POSITION_DEFAULT_POS');
    poDefaultSizeOnly: LoadResImg('POSITION_DEFAULT_SIZE');
    poScreenCenter:    LoadResImg('POSITION_SCREEN_CENTER');
    poDesktopCenter:   LoadResImg('POSITION_DESKTOP_CENTER');
    poMainFormCenter:  LoadResImg('POSITION_MAIN_FORM_CENTER');
    poOwnerFormCenter: LoadResImg('POSITION_OWNER_FORM_CENTER');
  end;

  (FRoot as TFakeForm).Position := TPosition(rgPosition.ItemIndex);
  GlobalDesignHook.Modified(Self);
  GlobalDesignHook.RefreshPropertyValues;
end;

function TedtuFormEditor.RootIsSelected: Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to FSelectionList.Count - 1 do
    if FSelectionList[i] = FRoot then
      Exit(True);
end;

procedure TedtuFormEditor.OnDesignRefreshPropertyValues;
begin
  if RootIsSelected then
    RefreshValues;
end;

procedure TedtuFormEditor.OnDesignSetSelection(
  const ASelection: TPersistentSelectionList);
begin
  if FSelectionList.IsEqual(ASelection) or (GlobalDesignHook.LookupRoot <> FRoot) or (FRoot = nil) then
    Exit;

  GlobalDesignHook.GetSelection(FSelectionList);

  if RootIsSelected then
    RefreshValues;
end;

function TedtuFormEditor.GetRoot: TPersistent;
begin
  Result := FRoot;
end;

procedure TedtuFormEditor.SetRoot(AValue: TPersistent);
begin
  if FRoot = AValue then
    Exit;
  FRoot := AValue;
  RefreshValues;
end;

function TedtuFormEditor.GetParent: TWinControl;
begin
  Result := Parent;
end;

function TedtuFormEditor.GetVisible: Boolean;
begin
  Result := Visible;
end;

procedure TedtuFormEditor.BeginUpdate;
begin
  FRefreshDisabled := True;
end;

procedure TedtuFormEditor.EndUpdate;
begin
  FRefreshDisabled := False;
end;

procedure TedtuFormEditor.ControlMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inReposition:=True;
  GetCursorPos(oldPos);
end;

procedure TedtuFormEditor.ControlMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  newPos: TPoint;
begin
  if inReposition then
  begin
    with TImage(Sender) do
    begin
      GetCursorPos(newPos);

      //move
      begin
        Screen.Cursor := crSize;
        Left := Left - oldPos.X + newPos.X;
        Top := Top - oldPos.Y + newPos.Y;
        // nie wychodź za bitmapę
        if Left < iDesktop.Left then
          Left := iDesktop.Left;
        if Top < iDesktop.Top then
          Top := iDesktop.Top;

        if Left + Width > iDesktop.Left + FScaleScreenWidth then
          Left := iDesktop.Left + (FScaleScreenWidth - Width);
        if Top + Height > iDesktop.Top + FScaleScreenHeight then
          Top := iDesktop.Top + (FScaleScreenHeight - Height);

        Left := ifthen(Left < iDesktop.Left, iDesktop.Left, Left);
        Top := ifthen(Top < iDesktop.Top, iDesktop.Top, Top);

        oldPos := newPos;
      end;
    end;
  end;
end;

procedure TedtuFormEditor.ControlMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if inReposition then
  begin
    Screen.Cursor := crDefault;
    inReposition := False;

    TFakeForm(FRoot).Left := Round((iForm.Left - iDesktop.Left) * 7.5);
    TFakeForm(FRoot).Top := Round((iForm.Top - iDesktop.Top) * 7.5);
    BeginUpdate;
    eLeft.Text := IntToStr(TFakeForm(FRoot).Left);
    eTop.Text  := IntToStr(TFakeForm(FRoot).Top);
    EndUpdate;

    GlobalDesignHook.Modified(Self);
    GlobalDesignHook.RefreshPropertyValues;
  end;
end;

procedure TedtuFormEditor.OnFormPaint(Sender: TObject);
var
  LImage: TImage absolute Sender;
  LResize: TNotifyEvent;
begin
  if LImage.Tag <> 0 then
  begin
    LResize:=LImage.OnResize;
    LImage.OnResize:=nil;
    DrawForm(LImage, TFakeForm(TDesignFormData(LImage.Tag).Form.Form), clBlack, clGray);
    LImage.OnResize:=LResize;
  end;
end;

procedure TedtuFormEditor.OnFormDblClick(Sender: TObject);
var
  LImage: TImage absolute Sender;
  LSourceEditor: TSourceEditorInterface;
  LDesigner: TIDesigner;
  LFormData: TDesignFormData;
begin
  LFormData := TDesignFormData(LImage.Tag);

  LDesigner := LFormData.Form.Form.Designer;

  LSourceEditor := FindSourceEditorForDesigner(LDesigner);
  if LSourceEditor = nil then
    Exit;

  SourceEditorManagerIntf.ActiveEditor := LSourceEditor;
  IDETabMaster.ShowDesigner(LSourceEditor);
end;

procedure TedtuFormEditor.OnFormClick(Sender: TObject);
begin
  TImage(Sender).BringToFront;
  iForm.BringToFront;
end;

function TedtuFormEditor.FormsStartPoint: TPoint;
begin
  Result := Point(iDesktop.Left, iDesktop.Top);
end;

procedure TedtuFormEditor.AddForm(Sender: TObject);
var
  LImage: TImage;
  LFormData: TDesignFormData absolute Sender;
begin
  LImage := TImage.Create(Self);
  LImage.Parent := pDesktop;
  LImage.Hint := LFormData.Form.Form.ClassName;
  LImage.Tag := PtrInt(LFormData);
  DrawForm(LImage,TFakeForm(LFormData.Form.Form), clBlack, clGray);
  LFormData.AddFormImage(LImage);
  LImage.OnResize:=OnFormPaint;
  LImage.OnDblClick:=OnFormDblClick;
  LImage.OnClick:=OnFormClick;
  // przy tworzeniu EDTU Form wystarczy, że wywolamy to po dodaniu form, nie ma potrzeby wywolywac BringToFront po
  // dodaniu kazdej formy z osobna
  if Assigned(iForm.OnMouseDown) then
    iForm.BringToFront;
end;

constructor TedtuFormEditor.Create(TheOwner: TComponent; ARoot: TPersistent);
var
  p: Pointer;
  LFormData: TDesignFormData absolute p;
  LResolution: string;
  LResIndex: Integer;
begin
  inherited Create(TheOwner);

  FSelectionList := TPersistentSelectionList.Create;

  with Screen.MonitorFromWindow(Application.MainForm.Handle) do
  begin
    FScreenWidth := ifthen(Width > 1920, 1920, Width);
    FScreenHeight := ifthen(Height > 1080, 1080, Height);

    LResolution := Format('%d×%d', [FScreenWidth, FScreenHeight]);
    if not FResolutions.TryGetValue(LResolution, LResIndex) then
      LResIndex := -1
    else
      cbResolution.Items[LResIndex] := cbResolution.Items[LResIndex] + ' MAIN';

    cbResolution.ItemIndex := LResIndex;
  end;

  RepaintDesktop;

  TDesignFormData.AddFormEvents.Add(AddForm);

  for p in dsgForms do
    if LFormData.Form.Form is TFakeForm then
    begin
      if LFormData.Form.Form = ARoot then
        Continue;
      AddForm(LFormData);
    end;

  iForm.OnMouseDown:=ControlMouseDown;
  iForm.OnMouseUp:=ControlMouseUp;
  iForm.OnMouseMove:=ControlMouseMove;
  iForm.BringToFront;

  GlobalDesignHook.AddHandlerRefreshPropertyValues(OnDesignRefreshPropertyValues);
  GlobalDesignHook.AddHandlerSetSelection(OnDesignSetSelection);

  Root := ARoot;
end;

destructor TedtuFormEditor.Destroy;
var
  i: Integer;
  LCtrl: TControl;
begin
  TDesignFormData.AddFormEvents.Remove(AddForm);

  GlobalDesignHook.RemoveHandlerRefreshPropertyValues(OnDesignRefreshPropertyValues);
  GlobalDesignHook.RemoveHandlerSetSelection(OnDesignSetSelection);

  for i := 0 to pDesktop.ControlCount - 1 do
  begin
    LCtrl := pDesktop.Controls[i];
    if LCtrl.Tag <> 0 then
      TDesignFormData(LCtrl.Tag).RemoveFormImage(TImage(LCtrl));
  end;

  FSelectionList.Free;

  inherited Destroy;
end;

procedure TedtuFormEditor.RefreshValues;
begin
  Self.Enabled := (FRoot <> nil) and (FRoot is TCustomForm);

  if (GlobalDesignHook.LookupRoot <> FRoot) or (FRoot = nil) or not Self.Enabled then
    Exit;

  BeginUpdate;

  GlobalDesignHook.GetSelection(FSelectionList);
  rgPosition.ItemIndex := Integer(TCustomForm(FRoot).Position);
  DrawForm(iForm, TFakeForm(FRoot), clBlack, clSilver);
  iForm.Hint:=Format('THIS - %s', [FRoot.ClassName]);
  eLeft.Value := TFakeForm(FRoot).Left;
  eTop.Value := TFakeForm(FRoot).Top;

  EndUpdate;
end;

end.

