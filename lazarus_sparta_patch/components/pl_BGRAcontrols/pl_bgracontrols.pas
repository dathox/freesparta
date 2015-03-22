{ This file was automatically created by Typhon. Do not edit!
  This source is only used to compile and install the package.
 }

unit pl_bgracontrols;

interface

uses
  AllBGRAControlsRegister, BCBaseCtrls, BCButton, BCEffect, bcfilters, 
  BCGameGrid, BCImageButton, BCLabel, BCPanel, BCRTTI, BCStylesForm, 
  BCTileMap, BCTools, BCTypes, bgrabitmapthemeutils, BGRAButton, 
  BGRAFlashProgressBar, BGRAGraphicControl, BGRAImageButton, BGRAImageList, 
  BGRAImageManipulation, BGRAKnob, BGRALabel, BGRALabelFX, BGRALED, 
  BGRANeoButton, BGRAPanel, BGRAPascalScript, BGRAResizeSpeedButton, 
  BGRARibbon, BGRARibbonGroup, BGRARKnob, bgrasamples, BGRAScript, BGRAShape, 
  BGRASpeedButton, BGRASpriteAnimation, BGRATextFXTypes, BGRAVirtualScreen, 
  BGRAWin7ToolBar, CustomBGRAImageButton, DTAnalogClock, DTAnalogCommon, 
  DTAnalogGauge, dtthemedclock, dtthemedgauge, uEKnob, ueled, uEMultiTurn, 
  uERotImage, uESelector, uPSI_BGRAPascalScript, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('AllBGRAControlsRegister', @AllBGRAControlsRegister.Register);
  RegisterUnit('BCBaseCtrls', @BCBaseCtrls.Register);
end;

initialization
  RegisterPackage('pl_bgracontrols', @Register);
end.
