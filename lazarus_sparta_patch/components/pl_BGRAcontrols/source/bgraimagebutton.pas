unit BGRAImageButton;

{$mode objfpc}{$H+}

interface

uses
  Classes, LResources, CustomBGRAImageButton;

type

  { TBGRAImageButton }

  TBGRAImageButton = class(TCustomBGRAImageButton)
  published
    property Action;
    property Align;
    property Anchors;
    property Animation;
    property AutoSize;
    property AutoSizeExtraHorizontal;
    property AutoSizeExtraVertical;
    property BidiMode;
    property Bitmap;
    property BitmapFile;
    property BitmapOptions;
    property BorderSpacing;
    property Caption;
    property Checked;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ModalResult;
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnPlaySound;
    property OnRedraw;
    property OnResize;
    property OnStartDrag;
    property ParentBidiMode;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Shadow;
    property ShowHint;
    property Sound;
    property SoundClick;
    property SoundEnter;
    property TextVisible;
    property Toggle;
    property Visible;
  end;


implementation

end.

