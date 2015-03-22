unit CustomBGRAImageButton;

{$mode objfpc}{$H+}

interface

uses
  Classes, Controls, Graphics{, Buttons}, Forms, LMessages, BGRABitmap, BGRABitmapTypes,
  BGRATextFXTypes, types, ExtCtrls;

{off $DEFINE DEBUG}

type

  TButtonState =
    (
    bsUp,       // button is up
    bsDisabled, // button disabled (grayed)
    bsDown,     // button is down
    bsExclusive,// button is the only down in his group
    bsHot       // button is under mouse
    );

  TCustomBGRAImageButtonRedraw = procedure(Sender: TObject;
    const ABGRA: TBGRABitmap; AState: TButtonState) of object;

  TCustomBGRAImageButtonPlaySound = procedure(Sender: TObject;
    const AFileName: string; AState: TButtonState) of object;

  { TCustomResizeBitmap }

  TCustomResizeBitmap = class(TPersistent)
  protected
    FOwner: TControl;
    FBorderWidth: integer;
    FBorderHeight: integer;
    FDrawMode: TDrawMode;
    FResampleMode: TResampleMode;
    FResampleFilter: TResampleFilter;
    FEnable: boolean deprecated;
    FFillLeft: boolean;
    FFillTop: boolean;
    FFillRight: boolean;
    FFillBottom: boolean;
    FFillCenter: boolean;
    procedure SetFBorderHeight(AValue: integer);
    procedure SetFBorderWidth(AValue: integer);
    procedure SetFDrawMode(AValue: TDrawMode);
    procedure SetFEnable(AValue: boolean);
    procedure SetFFillBottom(AValue: boolean);
    procedure SetFFillCenter(AValue: boolean);
    procedure SetFFillLeft(AValue: boolean);
    procedure SetFFillRight(AValue: boolean);
    procedure SetFFillTop(AValue: boolean);
    procedure SetFResampleFilter(AValue: TResampleFilter);
    procedure SetFResampleMode(AValue: TResampleMode);
  public
    constructor Create(AOwner: TControl);
    destructor Destroy; override;
  published
    property BorderWidth: integer read FBorderWidth write SetFBorderWidth;
    property BorderHeight: integer read FBorderHeight write SetFBorderHeight;
    property DrawMode: TDrawMode read FDrawMode write SetFDrawMode;
    property ResampleMode: TResampleMode read FResampleMode write SetFResampleMode;
    property ResampleFilter: TResampleFilter read FResampleFilter
      write SetFResampleFilter;
    property Enable: boolean read FEnable write SetFEnable;
    property FillLeft: boolean read FFillLeft write SetFFillLeft;
    property FillTop: boolean read FFillTop write SetFFillTop;
    property FillRight: boolean read FFillRight write SetFFillRight;
    property FillBottom: boolean read FFillBottom write SetFFillBottom;
    property FillCenter: boolean read FFillCenter write SetFFillCenter;
  end;

  { TCustomBGRAImageButton }

  TCustomBGRAImageButton = class(TGraphicControl)
  protected
    FAnimation: boolean;
    FAutoSizeExtraX: integer;
    FAutoSizeExtraY: integer;
    FBGRA: TBGRABitmap;
    FBGRADisabled: TBGRABitmap;
    FBGRADown: TBGRABitmap;
    FBGRAHot: TBGRABitmap;
    FBGRAText: TBGRABitmap;
    FBGRAUp: TBGRABitmap;
    FBitmapFile: string;
    FBitmapOptions: TCustomResizeBitmap;
    FBmp: TBitmap;
    FBmpState: TButtonState;
    FChecked: boolean;
    FCheckedState: TButtonState;
    FModalResult: TModalResult;
    FOnPlaySound: TCustomBGRAImageButtonPlaySound;
    FOnRedraw: TCustomBGRAImageButtonRedraw;
    FShadow: TBGRATextEffectShadow;
    FSound: boolean;
    FSoundEnter, FSoundClick: string;
    FTextVisible: boolean;
    FTimer: TTimer;
    FTimerIncreasing: boolean;
    FTimerStep: integer;
    FToggle: boolean;
    {$IFDEF DEBUG}
    FInvalidateCount, FUpdateCount, FUpdateCountTxt: integer;
    {$ENDIF}
  protected
    procedure SetFAnimation(AValue: boolean); virtual;
    procedure SetFAutoSizeExtraX(AValue: integer); virtual;
    procedure SetFAutoSizeExtraY(AValue: integer); virtual;
    procedure SetFBitmapFile(AValue: string); virtual;
    procedure SetFBitmapOptions(AValue: TCustomResizeBitmap); virtual;
    procedure SetFBmp(const AValue: TBitmap); virtual;
    procedure SetFChecked(AValue: boolean); virtual;
    procedure SetFTextVisible(AValue: boolean); virtual;
  protected
    procedure DoButtonDown; virtual;
    procedure DoButtonEnter; virtual;
    procedure DoButtonLeave; virtual;
    procedure DoButtonUp; virtual;
  protected
    procedure Click; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
  protected
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer;
      WithThemeSpace: boolean); override;
  protected
    procedure Animate(Sender: TObject); virtual;
    procedure CMChanged(var Message: TLMessage); message CM_CHANGED; virtual;
    procedure CMFontChanged(var Message: TLMessage); message CM_FONTCHANGED; virtual;
    procedure DoPlaySound(AFileName: string); virtual;
    procedure Paint; override;
    procedure RealSetText(const Value: TCaption); override;
    procedure UpdateBmp(Sender: TObject); virtual;
    procedure UpdateTxt; virtual;
  protected
    property ABGRA: TBGRABitmap read FBGRA write FBGRA;
    property ABGRADisabled: TBGRABitmap read FBGRADisabled write FBGRADisabled;
    property ABGRADown: TBGRABitmap read FBGRADown write FBGRADown;
    property ABGRAHot: TBGRABitmap read FBGRAHot write FBGRAHot;
    property ABGRAText: TBGRABitmap read FBGRAText write FBGRA;
    property ABGRAUp: TBGRABitmap read FBGRAUp write FBGRAUp;
    property ABmpState: TButtonState read FBmpState write FBmpState;
    property ACheckedState: TButtonState read FCheckedState write FCheckedState;
    property ATimer: TTimer read FTimer write FTimer;
    property ATimerIncreasing: boolean read FTimerIncreasing write FTimerIncreasing;
    property ATimerStep: integer read FTimerStep write FTimerStep;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function BitmapLoadFromFile(AFileName: string): boolean; virtual;
    function LoadFromFile(AFileName: string): boolean; virtual;
    function SaveToFile(AFileName: string): boolean; virtual;
    procedure Assign(Source: TPersistent); override;
  public
    property Animation: boolean read FAnimation write SetFAnimation default False;
    property AutoSizeExtraHorizontal: integer
      read FAutoSizeExtraX write SetFAutoSizeExtraX default 24;
    property AutoSizeExtraVertical: integer read FAutoSizeExtraY
      write SetFAutoSizeExtraY default 8;
    property Bitmap: TBitmap read FBmp write SetFBmp;
    property BitmapFile: string read FBitmapFile write SetFBitmapFile;
    property BitmapOptions: TCustomResizeBitmap
      read FBitmapOptions write SetFBitmapOptions;
    property Checked: boolean read FChecked write SetFChecked default False;
    property ModalResult: TModalResult
      read FModalResult write FModalResult default mrNone;
    property OnPlaySound: TCustomBGRAImageButtonPlaySound
      read FOnPlaySound write FOnPlaySound;
    property OnRedraw: TCustomBGRAImageButtonRedraw read FOnRedraw write FOnRedraw;
    property Shadow: TBGRATextEffectShadow read FShadow write FShadow;
    property Sound: boolean read FSound write FSound default False;
    property SoundClick: string read FSoundClick write FSoundClick;
    property SoundEnter: string read FSoundEnter write FSoundEnter;
    property TextVisible: boolean read FTextVisible write SetFTextVisible default True;
    property Toggle: boolean read FToggle write FToggle default False;
  end;

implementation

{$I custombgraimagebutton.inc}

end.

