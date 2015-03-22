{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit pl_glassdocking;

interface

uses
  gd_dockingbase, gd_dockingideboss, gd_dockingideframe, 
  gd_dockingideregister, gd_dockingoptionsdlg, gd_dockingstorage, 
  gd_dockingstr, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('gd_dockingideregister', @gd_dockingideregister.Register);
end;

initialization
  RegisterPackage('pl_glassdocking', @Register);
end.
