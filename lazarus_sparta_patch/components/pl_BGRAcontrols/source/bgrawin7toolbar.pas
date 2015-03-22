unit BGRAWin7ToolBar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRABitmap, BGRABitmapTypes, Controls, Graphics,
  BGRAVirtualScreen, BGRASamples, LResources, Dialogs, Forms;

type

  { TCustomBGRAWin7ToolBar }

  TCustomBGRAWin7ToolBar = class(TCustomBGRAVirtualScreen)
  private
    procedure SetFStyle(AValue: TBGRASampleStyle);
  protected
    FSmooth: boolean;
    FStyle: TBGRASampleStyle;
    procedure RedrawBitmapContent; override;
  public
    procedure StyleButtons;
    procedure SwitchAlign;
    constructor Create(TheOwner: TComponent); override;
  public
    property Style: TBGRASampleStyle Read FStyle Write SetFStyle;
  end;

  TBGRAWin7ToolBar = class(TCustomBGRAWin7ToolBar)
  published
    property Style;
    // TBGRAVirtualScreen
    property OnRedraw;
    property Bitmap;
    // TPanel
    property Align;
    //property Alignment;
    property Anchors;
    property AutoSize;
    property BorderSpacing;
    //property BevelInner;
    //property BevelOuter;
    //property BevelWidth;
    property BidiMode;
    property BorderWidth;
    property BorderStyle;
    property Caption;
    //property ChildSizing;
    property ClientHeight;
    property ClientWidth;
    //property Color;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    //property Font;
    property FullRepaint;
    property ParentBidiMode;
    //property ParentColor;
    //property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property UseDockManager default True;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnGetDockCaption;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

implementation


{ TBGRAWin7ToolBar }

procedure TCustomBGRAWin7ToolBar.SetFStyle(AValue: TBGRASampleStyle);
begin
  if FStyle = AValue then
    Exit;
  FStyle := AValue;

  StyleButtons;

  RedrawBitmap;
end;

procedure TCustomBGRAWin7ToolBar.RedrawBitmapContent;
begin
  if (Bitmap <> nil) and (Bitmap.NbPixels <> 0) then
  begin

    case Style of
      ssWin7ToolBar: DrawWin7ToolBar(Bitmap, Align);
      ssWin7ToolBarSmooth: DrawWin7ToolBar(Bitmap, Align, True);
      ssFlashPlayer: DrawFlashPlayerButtonPanel(Bitmap);
      ssiOSBar: DrawiOSBar(Bitmap);
      ssiOSToolBar: DrawiOSToolBar(Bitmap, False);
      ssiOSBackground: DrawiOSBackground(Bitmap);
      //ssFacebookBlue: DrawFacebookButton(Bitmap, ColorToBGRA(fbBlue), ColorToBGRA(ColorToRGB(Parent.Color), 255), False);
      //ssFacebookGreen: DrawFacebookButton(Bitmap, ColorToBGRA(fbGreen), ColorToBGRA(ColorToRGB(Parent.Color), 255), False);
      //ssFacebookGray: DrawFacebookButton(Bitmap, ColorToBGRA(fbGray), ColorToBGRA(ColorToRGB(Parent.Color), 255), False);
      ssBlack: Bitmap.Fill(BGRA(23, 23, 23, 255));
      ssSilverSquared: Bitmap.Fill(BGRA(223, 226, 230, 255));
      ssSilver, ssGreen, ssBlue: DrawSilverToolBar(Bitmap);
    else
      DrawWin7ToolBar(Bitmap, Align); // ToDO: add a toolbar for each style
    end;

    if Assigned(OnRedraw) then
      OnRedraw(self, Bitmap);
  end;
end;

procedure TCustomBGRAWin7ToolBar.StyleButtons;
begin
  StyleButtonsSample(Self, Style);
end;

procedure TCustomBGRAWin7ToolBar.SwitchAlign;
begin
  case Align of
    alCustom, alNone, alClient, alLeft: Align := alTop;
    alTop: Align := alRight;
    alRight: Align := alBottom;
    alBottom: Align := alLeft;
  end;

  case Align of
    // if horizontal
    alCustom, alNone, alClient, alTop, alBottom: ChildSizing.Layout :=
        cclTopToBottomThenLeftToRight;
    // if vertical
    alLeft, alRight: ChildSizing.Layout := cclLeftToRightThenTopToBottom;
  end;
end;

constructor TCustomBGRAWin7ToolBar.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  // align top default sizing
  Align := alTop;
  ChildSizing.Layout := cclTopToBottomThenLeftToRight;
  //{$IFDEF WINDOWS}
    // default spacing under different dpi settings
    //ChildSizing.LeftRightSpacing := ScaleX(4,96);
    //ChildSizing.TopBottomSpacing := ScaleY(4,96);
  //{$ELSE}
    // default spacing
    ChildSizing.LeftRightSpacing := 4;
    ChildSizing.TopBottomSpacing := 4;
  //{$ENDIF}
end;

end.
