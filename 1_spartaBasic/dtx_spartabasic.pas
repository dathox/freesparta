{ This file was automatically created by Typhon. Do not edit!
  This source is only used to compile and install the package.
 }

unit dtx_SpartaBasic;

interface

uses
  dtx_sparta_BasicRegister, sparta_DesignedForm, sparta_Resizer, 
  sparta_ResizerFrame, SpartaAPI, sparta_ComponentPalette, sparta_FakeCustom, 
  sparta_FakeForm, sparta_FakeFrame, sparta_FakeNonControl, sparta_MainIDE, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('dtx_sparta_BasicRegister', @dtx_sparta_BasicRegister.Register);
end;

initialization
  RegisterPackage('dtx_SpartaBasic', @Register);
end.
