unit BGRARibbonGroup;

{$mode objfpc}{$H+}

interface

uses
  Classes, LResources, Forms, Controls, Graphics, Dialogs,
  BGRAVirtualScreen, BGRABitmapTypes;

type

  { TBGRARibbonGroup }

  TBGRARibbonGroup = class(TCustomBGRAVirtualScreen)
  private
    { Private declarations }
  protected
    { Protected declarations }
    procedure RedrawBitmapContent; override;
  public
    { Public declarations }
    procedure MouseEnter; override;
    procedure MouseLeave; override;
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
    property ChildSizing;
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

uses
  BGRARibbon;


{ TBGRARibbonGroup }

procedure TBGRARibbonGroup.RedrawBitmapContent;
var
  IsHover: boolean;
begin
  if Parent is TBGRARibbon then
    if TBGRARibbon(Parent).CurrentGroup = Self.Name then
      IsHover := False;

  if (Bitmap <> nil) and (Bitmap.NbPixels <> 0) then
  begin

    if IsHover then
      // normal group
      bitmap.GradientFill(0, 0, bitmap.Width, bitmap.Height, BGRAWhite,
        BGRA(229, 233, 238, 255), gtLinear, PointF(0, trunc(bitmap.Height * 0.40)),
        PointF(0, bitmap.Height), dmSet)
    else
      // hover group
      bitmap.GradientFill(0, 0, bitmap.Width, bitmap.Height, BGRAWhite,
        BGRA(224, 230, 236, 255),
        gtRadial,
        PointF(trunc(bitmap.Width div 2), bitmap.Height),
        PointF(0, 0),
        dmSet);

    bitmap.DrawVertLine(bitmap.Width - 1, 1, bitmap.Height - 5, BGRAWhite);
    bitmap.DrawVertLine(bitmap.Width - 2, 1, bitmap.Height - 5,
      BGRA(176, 182, 188, 255));
    bitmap.DrawVertLine(bitmap.Width - 3, 1, bitmap.Height - 5, BGRAWhite);

    if not IsHover then
      bitmap.DrawHorizLine(0, bitmap.Height - 5, bitmap.Width - 4, BGRAWhite);

    // light
    //bitmap.GradientFill(0, 0, bitmap.Width, bitmap.Height, BGRAWhite,
    //  BGRA(255, 255, 255, 0), gtLinear, PointF(0, 0),
    //  PointF(0, bitmap.Height), dmDrawWithTransparency);

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

procedure TBGRARibbonGroup.MouseEnter;
begin
  if Parent is TBGRARibbon then
  begin
    TBGRARibbon(Parent).CurrentGroup := Self.Name;
    TBGRARibbon(Parent).DiscardStage;
  end;
  inherited MouseEnter;
end;

procedure TBGRARibbonGroup.MouseLeave;
begin
  if Parent is TBGRARibbon then
  begin
    TBGRARibbon(Parent).CurrentGroup := '';
    TBGRARibbon(Parent).DiscardStage;
  end;
  inherited MouseLeave;
end;

constructor TBGRARibbonGroup.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  ChildSizing.Layout := cclTopToBottomThenLeftToRight;
  ChildSizing.EnlargeVertical := crsHomogenousChildResize;
  ChildSizing.EnlargeHorizontal := crsHomogenousChildResize;
  ChildSizing.ShrinkHorizontal := crsScaleChilds;
  ChildSizing.ShrinkVertical := crsScaleChilds;
  ChildSizing.HorizontalSpacing := 6;
  ChildSizing.LeftRightSpacing := 6;
  ChildSizing.TopBottomSpacing := 6;
  ChildSizing.VerticalSpacing := 6;
end;

end.
