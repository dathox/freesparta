{ This file was automatically created by Typhon. Do not edit!
  This source is only used to compile and install the package.
 }

unit dtx_SpartaStarter;

interface

uses
  dtx_sparta_StarterRegister, sparta_FakeFormBackground, sparta_FakeFormBG, 
  sparta_EDTU_Main, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('dtx_sparta_StarterRegister', 
    @dtx_sparta_StarterRegister.Register);
end;

initialization
  RegisterPackage('dtx_SpartaStarter', @Register);
end.
