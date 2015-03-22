{**********************************************************************
                PilotLogic Software House.
    
 Package pl_BGRAcontrols.pkg
 This unit is part of CodeTyphon Project (http://www.pilotlogic.com/)
***********************************************************************}
    
unit AllBGRAControlsRegister;

{$mode objfpc}{$H+}

interface

uses
  Classes,SysUtils,TypInfo,lresources,PropEdits,ComponentEditors,LCLType, GraphPropEdits, ImgList,
  BGRAButton,
  BGRAFlashProgressBar,
  BGRAImageList,
  BGRAKnob,
  BGRARKnob,
  BGRAPanel,
  BGRASpeedButton,
  BGRAImageButton,
  BGRAImageManipulation,
  BGRALabel,
  BGRALabelFX,
  BGRAShape,
  BGRAGraphicControl,
  BGRASpriteAnimation,
  BGRAWin7ToolBar,
  BGRANeoButton,
  BGRARibbon,
  BGRARibbonGroup,
  BGRAVirtualScreen,
  BGRAResizeSpeedButton,
  BGRALED,
  uPSI_BGRAPascalScript,

  BCLabel,
  BCPanel,
  BCButton,
  BCImageButton,
  BCGameGrid,
  bcstylesform,

  DTAnalogCommon,
  DTAnalogClock,
  dtthemedclock,
  DTAnalogGauge,
  dtthemedgauge,

  uEKnob,
  ueled,
  uERotImage,
  uESelector,
  uEMultiTurn;

type
  TBCButtonImageIndexPropertyEditor = class(TImageIndexPropertyEditor)
  protected
    function GetImageList: TCustomImageList; override;
  end;

procedure Register;

implementation

  {$R AllBGRAControlsRegister.res}

function TBCButtonImageIndexPropertyEditor.GetImageList: TCustomImageList;
var
  Component: TPersistent;
begin
  Component := GetComponent(0);
  if Component is TCustomBCButton then
    Result := TCustomBCButton(Component).Images
  else
    Result := nil
end;


//==========================================================
procedure Register;
begin

  RegisterComponents ('BGRA Controls',[
                                  TBCButton,
                                  TBCXButton,
                                  TBCImageButton,
                                  TBCLabel,
                                  TBCPanel,
                                  TBCGameGrid,

                                  TBGRAButton,
                                  TBGRANeoButton,
                                  TBGRASpeedButton,
                                  TBGRAResizeSpeedButton,
                                  TBGRAPanel,
                                  TBGRAKnob,
                                  TBGRARKnob,
                                  TBGRAImageList,
                                  TBGRAFlashProgressBar,
                                  TBGRAImageButton,
                                  TBGRAWin7ToolBar,
                                  TBGRARibbon,
                                  TBGRARibbonGroup,
                                  TBGRALabel,
                                  TBGRALabelFX,
                                  TBGRAShape,
                                  TBGRALED,
                                  TBGRAImageManipulation,
                                  TBGRAGraphicControl,
                                  TBGRASpriteAnimation,
                                  TBGRAVirtualScreen,

                                  TDTAnalogClock,
                                  TDTAnalogGauge,
                                  TDTThemedClock,
                                  TDTThemedGauge,

                                  TuEKnob,
                                  TuESelector,
                                  TuEMultiTurn,
                                  TuELED,
                                  TuERotImage,

                                  TPSImport_bgrapascalscript
                                   ]);

RegisterPropertyEditor(TypeInfo(TTranslateString), TBGRALabel,'Caption', TStringMultilinePropertyEditor);
RegisterPropertyEditor(TypeInfo(Integer), TBCButton, 'ImageIndex', TBCButtonImageIndexPropertyEditor);

end;

end.

