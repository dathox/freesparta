unit BGRARibbon;

{$mode objfpc}{$H+}

interface

uses
  Classes, LResources, Forms, Controls, Graphics, Dialogs,
  BGRAVirtualScreen, BGRABitmapTypes, BGRARibbonGroup;

type

  { TBGRARibbon }

  TBGRARibbon = class(TCustomBGRAVirtualScreen)
  private
    { Private declarations }
  protected
    { Protected declarations }
    procedure RedrawBitmapContent; override;
  public
    { Public declarations }
    CurrentGroup: TComponentName;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure DiscardStage;
    constructor Create(TheOwner: TComponent); override;
  published
    { Published declarations }
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


{ TBGRARibbon }

procedure TBGRARibbon.RedrawBitmapContent;
begin
  if (Bitmap <> nil) and (Bitmap.NbPixels <> 0) then
  begin
    // normal bkg
    bitmap.GradientFill(0, 0, bitmap.Width, bitmap.Height, BGRAWhite,
      BGRA(229, 233, 238, 255), gtLinear, PointF(0, trunc(bitmap.Height * 0.40)),
      PointF(0, bitmap.Height), dmSet);

    // top line
    bitmap.DrawHorizLine(0, 0, bitmap.Width - 1, BGRA(182, 186, 191, 255));
    // bottom line
    bitmap.DrawHorizLine(0, bitmap.Height - 4, bitmap.Width - 1,
      BGRA(139, 144, 151, 255));
    bitmap.DrawHorizLine(0, bitmap.Height - 3, bitmap.Width - 1,
      BGRA(174, 177, 182, 255));
    bitmap.DrawHorizLine(0, bitmap.Height - 2, bitmap.Width - 1,
      BGRA(187, 191, 195, 255));
    bitmap.DrawHorizLine(0, bitmap.Height - 1, bitmap.Width - 1,
      BGRA(196, 200, 205, 255));
  end;
end;

procedure TBGRARibbon.MouseEnter;
begin
  CurrentGroup := '';
  DiscardStage;
  inherited MouseEnter;
end;

procedure TBGRARibbon.MouseLeave;
begin
  CurrentGroup := '';
  DiscardStage;
  inherited MouseLeave;
end;

procedure TBGRARibbon.DiscardStage;
var
  i: integer;
begin
  for i := 0 to self.ControlCount - 1 do
    if self.Controls[i] is TBGRARibbonGroup then
      TBGRARibbonGroup(self.Controls[i]).DiscardBitmap;
end;

constructor TBGRARibbon.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Align := alTop;
  ChildSizing.Layout := cclTopToBottomThenLeftToRight;
  ChildSizing.EnlargeVertical := crsHomogenousChildResize;
end;

end.
