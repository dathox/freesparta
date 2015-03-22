{ This file was automatically created by Typhon. Do not edit!
  This source is only used to compile and install the package.
 }

unit dtx_SpartaProfessional;

interface

uses
  dtx_sparta_ProfessionalRegister, sparta_EDTU_AnchorDocking, 
  sparta_edtu_anchordocking_table, sparta_EDTU_Form, 
  sparta_EDTU_NonVisualComponents, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('dtx_sparta_ProfessionalRegister', 
    @dtx_sparta_ProfessionalRegister.Register);
end;

initialization
  RegisterPackage('dtx_SpartaProfessional', @Register);
end.
