{**********************************************************************
          Copyright (c) PilotLogic Software House.
                   All rights reserved.
                   
 This unit is part of CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************}

unit ct_popupcomppages;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, Buttons, MainBar, 
  LazIDEIntf // DaThoX
  ;

type

  { TDlgPopupCompPages }

  TDlgPopupCompPages = class(TForm)
    cBtnClose: TSpeedButton;
    ImageList1: TImageList;
    Panel1: TPanel;
    Panel2: TPanel;
    TreeView1: TTreeView;
    procedure cBtnCloseClick(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TreeView1Click(Sender: TObject);
  private
    fDirForPkgs:string;
    //----------------------------------
     fPopupItemIndy,
     fPopupItemGLScene,
     fPopupItemCindy,
     fPopupItemExtra,
     fPopupItemDCP,
     fPopupItemACS,
     fPopupItemASIOVST,
     fPopupItemVirtual,
     fPopupItemSCADA,
     fPopupItemFZControls,
     fPopupItemLuiControls,
     fPopupItemJujibo,
     fPopupItemShapes,
     fPopupItemGraphics32,
     fPopupItemBrook,
     fPopupItemRX     :TTreeNode;

  public
    procedure FixBounds;
    procedure ClearList;
    procedure BuildList;
  end;

var
  DlgPopupCompPages: TDlgPopupCompPages;

procedure DlgPopupCompPages_Execute(const X,Y:integer);

implementation

{$R *.lfm}

type // DaThoX
  TLazIDEInterfaceAccess = class(TLazIDEInterface);

procedure DlgPopupCompPages_Execute(const X,Y:integer);
 var zPos:Tpoint;
begin
  if DlgPopupCompPages=nil then
    Application.CreateForm(TDlgPopupCompPages, DlgPopupCompPages);

  if NOT DlgPopupCompPages.Visible then
   begin
    zPos:=point(X,Y);
    DlgPopupCompPages.Left:=zPos.x-(DlgPopupCompPages.Width div 2);
    DlgPopupCompPages.Top:=zPos.y-5;
    DlgPopupCompPages.FixBounds;
    DlgPopupCompPages.Show;
   end else
   begin
    DlgPopupCompPages.Close;
   end;
end;

//=================== TDlgPopupCompPages ===================================

procedure TDlgPopupCompPages.FormShow(Sender: TObject);
begin
  BuildList;
end;

procedure TDlgPopupCompPages.FormDeactivate(Sender: TObject);
begin
  close;
end;

procedure TDlgPopupCompPages.cBtnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TDlgPopupCompPages.FixBounds;
begin
  if (self.Height+100)>screen.Height then
     self.Height:=screen.Height-self.Top-100 else
     self.Height:=Round(8*screen.Height/10) - self.Top;

  if (self.Left+self.Width+50)>screen.Width then
      self.Left:=self.Left-(self.Width div 2)+10;

  if self.Height<400 then self.Height:=400;
end;

//---------------------------------------------------------------
procedure TDlgPopupCompPages.ClearList;
 begin
  TreeView1.Items.Clear;

  fPopupItemIndy:=nil;
  fPopupItemGLScene:=nil;
  fPopupItemCindy:=nil;
  fPopupItemExtra:=nil;
  fPopupItemDCP:=nil;
  fPopupItemACS:=nil;
  fPopupItemASIOVST:=nil;
  fPopupItemVirtual:=nil;
  fPopupItemSCADA:=nil;
  fPopupItemRX:=nil;
  fPopupItemFZControls:=nil;
  fPopupItemLuiControls:=nil;
  fPopupItemJujibo:=nil;
  fPopupItemShapes:=nil;
  fPopupItemGraphics32:=nil;
  fPopupItemBrook:=nil;

 end;

//---------------------------------------------------------------

procedure TDlgPopupCompPages.TreeView1Click(Sender: TObject);
 var id:integer;
    //..................................................
    procedure _findPage(const aname:string);
     var ix:integer;
    begin
     id:=-1;
     if MainIDEBar.ComponentPageControl=nil then exit;
     if MainIDEBar.ComponentPageControl.PageCount=0 then exit;

     for ix:=0 to MainIDEBar.ComponentPageControl.PageCount-1 do
       if SameText(aname,MainIDEBar.ComponentPageControl.Page[ix].Caption) then
         begin
           id:=ix;
           exit;
         end;
    end;
    //..................................................
begin
  if TreeView1.Selected=nil then exit;
  if TreeView1.Selected.ImageIndex=1 then exit;
  //.......................
  _findPage(TreeView1.Selected.Text);
  if id>-1 then
  MainIDEBar.ComponentPageControl.PageIndex:=id;
  TLazIDEInterfaceAccess(LazarusIDE).DoCallNotifyHandler(lihtUpdateComponentPageControl, MainIDEBar.ComponentPageControl); // DaThoX;

  Close;
end;

procedure TDlgPopupCompPages.BuildList;
 var i,xPageIndex:integer;
     dummy:TTreeNode;
     ss0,ss1:string;
     isSubItem:boolean;

//....................................
    procedure _AddFirstMenu(var amenu:TTreeNode;aname:string);
     begin
        if amenu=nil then
          begin
             amenu:=TreeView1.Items.AddChild(nil,aName);
             amenu.ImageIndex:=1;
             amenu.SelectedIndex:=1;
          end;
     end;
//....................................

     procedure _AddMenu(var apatent:TTreeNode;apage:string);
       begin
        if apatent=nil then exit;

         dummy:=TreeView1.Items.AddChild(apatent,apage);
         dummy.ImageIndex:=0;
         dummy.SelectedIndex:=0;
         isSubItem:=true;
       end;
 //....................................
     Function _IsSameStr(var outstr:string;Const astr:string):boolean;
      var ic:integer;
          istr:string;
       begin
         result:=false;
         if astr='' then exit;
         ic:=Length(astr);
         istr:=leftStr(outstr,ic);
         result:=SameText(astr,istr);
       end;
 //....................................
begin

 ClearList;
 TreeView1.BeginUpdate;

 if MainIDEBar.ComponentPageControl=nil then
  begin
   dummy:=TreeView1.Items.AddChild(nil,'Sorry, NO Pages');
   Exit;
  end;

 for i:=0 to MainIDEBar.ComponentPageControl.PageCount-1 do
  begin
      isSubItem:=false;
      xPageIndex:=MainIDEBar.ComponentPageControl.Page[i].PageIndex;
      ss0:=MainIDEBar.ComponentPageControl.Page[i].Caption;

 //====================================================================
        if _IsSameStr(ss0,'Indy') then
         begin
           _AddFirstMenu(fPopupItemIndy,'Indy pages');
           _AddMenu(fPopupItemIndy,ss0);  continue;
         end;
        if _IsSameStr(ss0,'GLScene') then
        begin
           _AddFirstMenu(fPopupItemGLScene,'GLScene pages');
           _AddMenu(fPopupItemGLScene,ss0);  continue;
        end;
        if _IsSameStr(ss0,'Cindy') then
        begin
           _AddFirstMenu(fPopupItemCindy,'Cindy pages');
           _AddMenu(fPopupItemCindy,ss0); continue;
        end;
        if _IsSameStr(ss0,'Extra') then
        begin
           _AddFirstMenu(fPopupItemExtra,'Extra pages');
           _AddMenu(fPopupItemExtra,ss0);  continue;
        end;
        if _IsSameStr(ss0,'DCP') then
        begin
           _AddFirstMenu(fPopupItemDCP,'DCP pages');
           _AddMenu(fPopupItemDCP,ss0);  continue;
        end;
        if _IsSameStr(ss0,'RX') then
        begin
           _AddFirstMenu(fPopupItemRX,'RX pages');
           _AddMenu(fPopupItemRX,ss0);  continue;
        end;
        if _IsSameStr(ss0,'ACS') then
        begin
           _AddFirstMenu(fPopupItemACS,'Audio ACS pages');
           _AddMenu(fPopupItemACS,ss0);  continue;
        end;
        if _IsSameStr(ss0,'ASIO/VST') then
        begin
           _AddFirstMenu(fPopupItemASIOVST,'Audio ASIO-VST pages');
           _AddMenu(fPopupItemASIOVST,ss0);  continue;
        end;
        if _IsSameStr(ss0,'Virtual Controls') then
        begin
           _AddFirstMenu(fPopupItemVirtual,'Virtual Controls pages');
           _AddMenu(fPopupItemVirtual,ss0); continue;
        end;
        if _IsSameStr(ss0,'PascalSCADA') then
        begin
           _AddFirstMenu(fPopupItemSCADA,'PascalSCADA pages');
           _AddMenu(fPopupItemSCADA,ss0);  continue;
        end;
        if _IsSameStr(ss0,'FZControls') then
        begin
           _AddFirstMenu(fPopupItemFZControls,'FZControls pages');
           _AddMenu(fPopupItemFZControls,ss0); continue;
        end;
        if _IsSameStr(ss0,'LuiControls') then
        begin
           _AddFirstMenu(fPopupItemLuiControls,'LuiControls pages');
           _AddMenu(fPopupItemLuiControls,ss0); continue;
        end;
        if _IsSameStr(ss0,'Jujibo') then
        begin
           _AddFirstMenu(fPopupItemJujibo,'Jujibo pages');
           _AddMenu(fPopupItemJujibo,ss0); continue;
        end;
        if _IsSameStr(ss0,'Shapes') then
        begin
           _AddFirstMenu(fPopupItemShapes,'Shapes pages');
           _AddMenu(fPopupItemShapes,ss0); continue;
        end;
        if _IsSameStr(ss0,'Brook') then
        begin
           _AddFirstMenu(fPopupItemBrook,'Brook pages');
           _AddMenu(fPopupItemBrook,ss0); continue;
        end;
        if _IsSameStr(ss0,'Graphics32') then
        begin
           _AddFirstMenu(fPopupItemGraphics32,'Graphics32 pages');
           _AddMenu(fPopupItemGraphics32,ss0); continue;
        end;
 //====================================================================

        if isSubItem=false then
         begin
          dummy:=TreeView1.Items.AddChild(nil,ss0);
          dummy.ImageIndex:=0;
          dummy.SelectedIndex:=0;

         end;
  end;

 TreeView1.EndUpdate;

 TreeView1.FullExpand;

  //......
  Panel2.Caption:='Total Pages: '+IntToStr(MainIDEBar.ComponentPageControl.PageCount);
end;


end.

