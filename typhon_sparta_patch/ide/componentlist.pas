{                    
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************
}

unit ComponentList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons, FormEditingIntf, LazarusIDEStrConsts, ExtCtrls, ComCtrls,
  ComponentPalette, ComponentReg, PackageDefs, ExtDlgs, FormEditor, PropEdits,
  LCLType, Menus, ButtonPanel, IDEWindowIntf, types, Themes;


const
  ComponentListFormName = 'ComponentList';
type
  { TComponentListForm }

  TComponentListForm = class(TForm)
    btExpand: TSpeedButton;
    btUpdate: TSpeedButton;
    btSettings: TSpeedButton;
    ImageList1: TImageList;
    LabelSearch: TLabel;
    ListboxComponents: TListBox;
    cSmallImages: TMenuItem;
    cMediumImages: TMenuItem;
    cNormalImages: TMenuItem;
    MenuItem1: TMenuItem;
    cTabList: TMenuItem;
    cTabInher: TMenuItem;
    PageControl: TPageControl;
    cTextPanel: TPanel;
    Panel3: TPanel;
    Panel6: TPanel;
    PatternEdit: TEdit;
    btAdd: TSpeedButton;
    cMainMenu: TPopupMenu;
    TabSheetInheritance: TTabSheet;
    TabSheetListBox: TTabSheet;
    TabSheetPaletteTree: TTabSheet;
    TreeInheritance: TTreeView;
    TreePallette: TTreeView;
    procedure btAddClick(Sender: TObject);
    procedure btExpandClick(Sender: TObject);
    procedure btSettingsClick(Sender: TObject);
    procedure cMainMenuPopup(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListboxComponentsClick(Sender: TObject);
    procedure ListboxComponentsDblClick(Sender: TObject);
    procedure ListboxComponentsDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure ListboxComponentsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure PageControlChange(Sender: TObject);
    procedure PatternEditKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure btUpdateClick(Sender: TObject);
    procedure TreeInheritanceClick(Sender: TObject);
    procedure TreeInheritanceDblClick ( Sender: TObject ) ;
    procedure TreeInheritanceKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure TreePalletteClick(Sender: TObject);
    procedure TreePalletteCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure TreePalletteDblClick(Sender: TObject);
    procedure TreePalletteKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure UpdateComponentSelection(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure PatternEditChange(Sender: TObject);
    //.....
    procedure ZCommonProc(Sender: TObject);
  private
    fProcessing: boolean;
    fFullExpand: boolean;
    FComponentList: TFPList;
    FComSelected:TRegisteredComponent;
    FLastFormActivated: TCustomForm;
    FImagesSize:integer;
    fHideListTab:boolean;
    fHideInherTab:boolean;
    procedure FindAllIDEComponents;
    procedure AddSelectedComponent(AComponent: TRegisteredComponent);
    Procedure SendSelectedComponent(AComponent: TRegisteredComponent; AMulti: Boolean); // DaThoX
    procedure SetFocusToDataList;
    procedure SetImagesSize(const aValue: Integer);
    procedure SetHideListTab(const aValue: boolean);
    procedure SetHideInherTab(const aValue: boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure  UpdateVisible;
    procedure  UpdateVisibleEx(aform:TCustomForm);
    //....
    property ImagesSize: Integer read FImagesSize write SetImagesSize;
    property HideListTab:boolean read FHideListTab write SetHideListTab;
    property HideInherTab:boolean read FHideInherTab write SetHideInherTab;
  end;
  
var
  ComponentListForm: TComponentListForm;

implementation

{$R *.lfm}

uses MainBase;

{ TComponentListForm }

constructor TComponentListForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FLastFormActivated:=nil;
  fFullExpand:=true;
  FImagesSize:=3;
  fHideListTab:=true;
  fHideInherTab:=true;

  FComponentList := TFPList.Create;

  //Translations..
  LabelSearch.Caption := lisMenuFind;
  Caption := lisCmpLstComponents;
  TabSheetListBox.Caption := lisCmpLstList;
  TabSheetPaletteTree.Caption := lisCmpLstPalette;
  TabSheetInheritance.Caption := lisCmpLstInheritance;
  
  //PLEASE add a defaultpage property in TPagecontrol
  PageControl.ActivePage := TabSheetPaletteTree;
end;

destructor TComponentListForm.Destroy;
begin
  IDEDialogLayoutList.SaveLayout(Self);
  ComponentListForm := nil;
  FComponentList.Free;
  inherited Destroy;
end;

procedure TComponentListForm.FindAllIDEComponents;
//Collect all available components (excluding hidden)
var
  AComponent: TRegisteredComponent;
  APage: TBaseComponentPage;
  i, j: Integer;
begin
  FComponentList.Clear;
  
  if Assigned(IDEComponentPalette) then
  begin
    for i := 0 to IDEComponentPalette.Pages.Count-1 do
    begin
      APage := IDEComponentPalette.Pages[i];
      if APage.Visible then
        for j := 0 to IDEComponentPalette.Comps.Count-1 do
        begin
          AComponent := IDEComponentPalette.Comps[j];
          if (AComponent.RealPage = APage)
          and AComponent.Visible then
            FComponentList.Add(AComponent);
        end;
    end;
  end;
end;

 //=====================================================
procedure TComponentListForm.UpdateComponentSelection(Sender: TObject);
//Apply the filter and fill the three tabsheets
var
  AComponent: TRegisteredComponent;
  AFilter, AClassName: string;
  AClassList, List: TStringlist;
  i, j, AIndex: Integer;
  ANode,ANode2: TTreeNode;
  AClass: TClass;
  FvisiblePages: Integer;
begin
  if fProcessing then exit;

  case FImagesSize of
   1:begin
       TreePallette.DefaultItemHeight:=18;
       ListboxComponents.ItemHeight:=18;
     end;
   2:begin
       TreePallette.DefaultItemHeight:=22;
       ListboxComponents.ItemHeight:=22;
     end;
   3:begin
       TreePallette.DefaultItemHeight:=26;
       ListboxComponents.ItemHeight:=26;
     end;
   end;

  FvisiblePages:=0;

  fProcessing := true;
  Screen.Cursor := crHourGlass;
  try
    AFilter := UpperCase(PatternEdit.Text);

   //------------------------------------------
    //First tabsheet (palette layout)
    TreePallette.BeginUpdate;
    try

      TreePallette.Items.Clear;

      for i := 0 to FComponentList.Count-1 do
      begin
        AComponent := TRegisteredComponent(FComponentList[i]);
        AClassName := AComponent.ComponentClass.ClassName;

        if (AFilter='') or (Pos(AFilter, UpperCase(AClassName))>0) then
         if AComponent.Visible then // ct9999
          begin
          //find out parent node
          ANode := TreePallette.Items.FindTopLvlNode(AComponent.RealPage.PageName);

          if ANode = nil then
           begin
            ANode := TreePallette.Items.AddChild(nil,AComponent.RealPage.PageName);
            inc(FvisiblePages);
           end;

          //add the item
         ANode2:=TreePallette.Items.AddChildObject(ANode, AClassName, AComponent);

        end;
      end;

      if fFullExpand then TreePallette.FullExpand;

    finally
      TreePallette.EndUpdate;
    end;

   //------------------------------------------
    //Second tabsheet (Listbox Components)

    ListboxComponents.Items.BeginUpdate;
    try
      ListboxComponents.Items.Clear;
      for i := 0 to FComponentList.Count-1 do
      begin
        AComponent := TRegisteredComponent(FComponentList[i]);
        AClassName := AComponent.ComponentClass.ClassName;
        if (AFilter='') or (Pos(AFilter, UpperCase(AClassName))>0) then
          if AComponent.Visible then // ct9999
            ListboxComponents.Items.AddObject(AClassName, AComponent);
      end;
    finally
      ListboxComponents.Items.EndUpdate;
    end;

    //------------------------------------------
    //Third tabsheet (component inheritence)
    List := TStringlist.Create;
    AClassList:= TStringlist.Create;
    TreeInheritance.Items.BeginUpdate;
    try
      TreeInheritance.Items.Clear;
      AClassList.Sorted := true;
      AClassList.CaseSensitive := false;
      AClassList.Duplicates := dupIgnore;

      for i := 0 to FComponentList.Count-1 do
      begin
        AComponent := TRegisteredComponent(FComponentList[i]);
        AClassName := AComponent.ComponentClass.ClassName;
        if (AFilter='') or (Pos(AFilter, UpperCase(AClassName))>0)then
        if AComponent.Visible then // ct9999
        begin

          // walk down to parent, stop on tcomponent, since components are at least
          // a tcomponent descendant
          List.Clear;
          AClass := AComponent.ComponentClass;
          while (AClass.ClassInfo <> nil) and (AClass.ClassType <> TComponent.ClassType) do
          begin
            List.AddObject(AClass.ClassName, TObject(AClass));
            AClass := AClass.ClassParent;
          end;

          //build the tree
          for j := List.Count - 1 downto 0 do
          begin
            AClass := TClass(List.Objects[j]);
            AClassName := List[j];

            if not AClassList.Find(AClassName, AIndex)
            then begin
              //find out parent position
              if Assigned(AClass.ClassParent) and AClassList.Find(AClass.ClassParent.ClassName, AIndex)
              then ANode := TTreeNode(AClassList.Objects[AIndex])
              else ANode := nil;

              //add the item
              if AClassName <> AComponent.ComponentClass.ClassName
              then ANode := TreeInheritance.Items.AddChild(ANode, AClassName)
              else ANode := TreeInheritance.Items.AddChildObject(ANode, AClassName, AComponent);
              AClassList.AddObject(AClassName, ANode);
            end;
          end;
        end;
      end;


      TreeInheritance.AlphaSort;
      TreeInheritance.FullExpand;
    finally
      List.Free;
      AClassList.Free;
      TreeInheritance.Items.EndUpdate;
    end;
    
  finally
    Screen.Cursor := crDefault;
    fProcessing := false;
  end;

cTextPanel.Caption:='TP:'+IntToStr(IDEComponentPalette.Pages.Count-1)+
                    ' TC:'+IntToStr(FComponentList.Count)+
                    ' - VP:'+IntToStr(FvisiblePages)+
                    ' VC:'+IntToStr(ListboxComponents.Items.Count)+'  ';

cTextPanel.Hint:='Total IDE Pages:'+IntToStr(IDEComponentPalette.Pages.Count-1)+' '+
                 'Total IDE Components:'+IntToStr(FComponentList.Count)+ ' '+
                 'Visible Pages:'+IntToStr(FvisiblePages)+' '+
                 'Visible Components:'+IntToStr(ListboxComponents.Items.Count);
end;

// Add the DblClicked component to the current designed form
procedure TComponentListForm.AddSelectedComponent(AComponent: TRegisteredComponent);
var
  TypeClass: TComponentClass;
  X, Y: integer;
  DisableAutoSize: Boolean;
  ParentComponent: TComponent;
  NewComponent: TComponent;
begin
  if not Assigned(AComponent) then Exit;
  if not Assigned(FormEditingHook) then Exit;

  TypeClass:=AComponent.ComponentClass;
  ParentComponent:=FormEditingHook.GetDefaultComponentParent(TypeClass);
  if ParentComponent=nil then exit;

  //Would be lovely if it would only select the component as if it was
  //clicked in the palette bar so you can drop it on your own position.
  if not FormEditingHook.GetDefaultComponentPosition(TypeClass,ParentComponent,X,Y) then exit;

  DisableAutoSize:=true;
  NewComponent:=FormEditingHook.CreateComponent(ParentComponent,TypeClass,'', X,Y,0,0,DisableAutoSize);

  if Assigned(NewComponent) then
  begin
    if DisableAutoSize and (NewComponent is TControl) then
      TControl(NewComponent).EnableAutoSizing;
    GlobalDesignHook.PersistentAdded(NewComponent,true);
  end;
end;

//Send selected Component to IDEComponentPalette
procedure TComponentListForm.SendSelectedComponent(
  AComponent: TRegisteredComponent; AMulti: Boolean);
begin
  if visible=false then exit;

  TComponentPalette(IDEComponentPalette).SetSelected(AComponent, AMulti); // DaThoX

  if Assigned(AComponent) then
    GetMainIde.DoShowDesignerFormOfCurrentSrc(True); // 77777
end;

procedure TComponentListForm.ListboxComponentsDblClick(Sender: TObject);
begin
  AddSelectedComponent(TRegisteredComponent(ListboxComponents.Items.Objects[ListboxComponents.ItemIndex]));
end;

procedure TComponentListForm.ListboxComponentsKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
    if (ListboxComponents.ItemIndex >= 0) then
      ListboxComponentsDblClick(Sender);
end;

procedure TComponentListForm.SetFocusToDataList;
var
  S: String;
  i: Integer;
  Node: TTreeNode;
begin
  S := PatternEdit.Text;
  case PageControl.PageIndex of
     0:
    begin
      TreePallette.SetFocus;
      with TreePallette do
      begin
        if Items.Count > 0 then
        begin
          Node := Items.FindNodeWithText(S);
          if Node <> nil then
            Selected := Node
          else
            Selected := Items[0];
        end;
      end;
    end;
    1:
    begin
      ListBoxComponents.SetFocus;
      if ListBoxComponents.Items.Count > 0 then
      begin
        i := ListBoxComponents.Items.IndexOf(S);
        if i < 0 then
          i := 0;
        ListBoxComponents.ItemIndex := i;
      end;
    end;
    2:
    begin
      TreeInheritance.SetFocus;
      with TreeInheritance do
      begin
        if Items.Count > 0 then
        begin
          Node := Items.FindNodeWithText(S);
          if Node <> nil then
            Selected := Node
          else
            Selected := Items[0];
        end;
      end;
    end;
  end;
end;

procedure TComponentListForm.PatternEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
  begin
    Key := 0;
    SetFocusToDataList;
  end;
end;

procedure TComponentListForm.TreePalletteDblClick(Sender: TObject);
var
  AComponent: TRegisteredComponent;
begin
  if not Assigned(TreePallette.Selected) then exit;
  AComponent := TRegisteredComponent(TreePallette.Selected.Data);
  if not Assigned(AComponent) then exit;

  AddSelectedComponent(AComponent);
end;

procedure TComponentListForm.TreePalletteKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then TreePalletteDblClick(Sender);
end;

procedure TComponentListForm.TreeInheritanceDblClick(Sender:TObject);
var
  AComponent: TRegisteredComponent;
begin
  if not Assigned(TreeInheritance.Selected)
  then exit;
  AComponent := TRegisteredComponent(TreeInheritance.Selected.Data);
  if not Assigned(AComponent)
  then exit;
  AddSelectedComponent(AComponent);
end;

procedure TComponentListForm.TreeInheritanceKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
    TreeInheritanceDblClick(Sender);
end;

procedure TComponentListForm.FormShow(Sender: TObject);
begin
  if PatternEdit.Canfocus
  then PatternEdit.SetFocus;

  FindAllIDEComponents;
  UpdateComponentSelection(nil);
  IDEDialogLayoutList.ApplyLayout(Self);
end;

procedure TComponentListForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
//Close the form on escape key like every other IDE dialog does
begin
  if (Key=VK_ESCAPE) and (Parent=nil) then
    Close;
end;

procedure TComponentListForm.PatternEditChange(Sender: TObject);
begin
  UpdateComponentSelection(nil);
end;

 //===================================================  8888

procedure TComponentListForm.TreePalletteCustomDrawItem(
  Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState;
  var DefaultDraw: Boolean);

var
  Comp: TRegisteredComponent;
  ARect: TRect;
  CurIcon: TCustomBitmap;
  Indent, IconWidth, IconHeight,TxtH,VertMid,LeftOffset: Integer;
  R: TRect;
  //.....................................
  Procedure _drawArrow ;
   var Points: PPoint;
       MidX,MidY: Integer;
  begin
          MidX:=(R.Left+R.Right)DIV 2;
          MidY:=(R.Bottom+R.Top)DIV 2;
          GetMem(Points, SizeOf(TPoint) * 3);
          if Node.Expanded then
          begin
            // draw an arrow down
            Points[0] := Point(R.Left, MidY);
            Points[1] := Point(R.Right - 1, MidY);
            Points[2] := Point(MidX, R.Bottom - 1);
          end else
          begin
            // draw an arrow right
            Points[0] := Point(MidX - 1, R.Top);
            Points[1] := Point(R.Right - 2, MidY);
            Points[2] := Point(MidX - 1, R.Bottom - 1);
          end;
          Sender.Canvas.Polygon(Points, 3, False);
          FreeMem(Points);
   end;
  //.....................................
begin
  DefaultDraw := False;
  Indent := (Sender as TTreeView).Indent;
  Comp:=TRegisteredComponent(Node.Data);

  with Sender.Canvas do
  begin

    if cdsSelected in State then
    begin
      Brush.Color := Sender.SelectionColor;
    end
    else begin
      Brush.Color := Sender.Color;
    end;

    ARect := Node.DisplayRect(False);
    FillRect(ARect);

    ARect.Left := ARect.Left + (Node.Level * Indent);

    CurIcon:=nil;

    if Comp is TPkgComponent then
      CurIcon:=TPkgComponent(Comp).Icon;

    if CurIcon<>nil then
    begin
      IconWidth:=CurIcon.Width;
      IconHeight:=CurIcon.Height;
      ARect.Left := ARect.Left + Indent;

      R := Rect(ARect.Left,ARect.Top, ARect.Left + Sender.DefaultItemHeight, ARect.Top+ Sender.DefaultItemHeight);
      InflateRect(R,-1,-1);
      StretchDraw(R,CurIcon);

      ARect.Left := ARect.Left + IconWidth + 2;
      font.Bold:=false;
      LeftOffset:=5;
    end else
    begin
      Brush.Color := Sender.ExpandSignColor;
      R := Rect(ARect.Left+2, ARect.Top+4, ARect.Left + 15, ARect.Top+17);

      _drawArrow;

      if cdsSelected in State then
       begin
         Brush.Color := Sender.SelectionColor;
       end
       else begin
         Brush.Color := Sender.Color;
      end;

      font.Bold:=true;
      LeftOffset:=20;
    end;

    //.............. Text ................
    TxtH:=TextHeight(Node.Text);
    TextOut(ARect.Left+LeftOffset, ARect.Top+(ARect.Bottom-ARect.Top-TxtH) div 2, Node.Text);
  end;
end;

procedure TComponentListForm.ListboxComponentsDrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  Comp: TRegisteredComponent;
  CurStr: string;
  CurIcon: TCustomBitmap;
  TxtH, IconWidth, IconHeight,LeftOffset: Integer;
  R: TRect;
begin
  if (Index<0) or (Index>=ListboxComponents.Items.Count) then exit;

  LeftOffset:=7;

  Comp:=TRegisteredComponent(ListboxComponents.Items.Objects[Index]);

  with ListboxComponents.Canvas do
  begin
    CurStr:=Comp.ComponentClass.ClassName;

    FillRect(ARect);
    CurIcon:=nil;

    if Comp is TPkgComponent then
      CurIcon:=TPkgComponent(Comp).Icon;

    if CurIcon<>nil then
    begin
      IconWidth:=CurIcon.Width;
      IconHeight:=CurIcon.Height;

      R := Rect(ARect.Left+2, ARect.Top, ARect.Left+2+ListboxComponents.ItemHeight, ARect.Top+ListboxComponents.ItemHeight);
      InflateRect(R,-1,-1);
      StretchDraw(R,CurIcon);
    end;

     //.............. Text ................
    TxtH:=TextHeight(CurStr);
    TextOut(LeftOffset+ARect.Left+25, ARect.Top+(ARect.Bottom-ARect.Top-TxtH) div 2, CurStr);
  end;
end;

//========================================================

procedure TComponentListForm.TreePalletteClick(Sender: TObject);
begin
  if not Assigned(TreePallette.Selected) then exit;
  FComSelected:= TRegisteredComponent(TreePallette.Selected.Data);
  SendSelectedComponent(FComSelected, ssShift in GetKeyShiftState); // DaThoX
end;


procedure TComponentListForm.ListboxComponentsClick(Sender: TObject);
begin
  if ListboxComponents.ItemIndex<0 then exit;
  FComSelected:=TRegisteredComponent(ListboxComponents.Items.Objects[ListboxComponents.ItemIndex]);
  SendSelectedComponent(FComSelected, ssShift in GetKeyShiftState); // DaThoX
end;

procedure TComponentListForm.TreeInheritanceClick(Sender: TObject);
begin
  if not Assigned(TreeInheritance.Selected) then exit;
  FComSelected:= TRegisteredComponent(TreeInheritance.Selected.Data);
  SendSelectedComponent(FComSelected, ssShift in GetKeyShiftState); // DaThoX
end;

procedure TComponentListForm.PageControlChange(Sender: TObject);
var
  S: String;
  i: Integer;
  Node: TTreeNode;
begin

  if FComSelected=nil then exit;

  S := FComSelected.ComponentClass.ClassName;

  case PageControl.PageIndex of
     0:
    begin
      TreePallette.SetFocus;
      with TreePallette do
      begin
        if Items.Count > 0 then
        begin
          Node := Items.FindNodeWithText(S);
          if Node <> nil then
            Selected := Node
          else
            Selected := Items[0];

          MakeSelectionVisible;
        end;
      end;
    end;
    1:
    begin
      ListBoxComponents.SetFocus;
      if ListBoxComponents.Items.Count > 0 then
      begin
        i := ListBoxComponents.Items.IndexOf(S);
        if i < 0 then
          i := 0;
        ListBoxComponents.ItemIndex := i;
      end;
    end;
    2:
    begin
      TreeInheritance.SetFocus;
      with TreeInheritance do
      begin
        if Items.Count > 0 then
        begin
          Node := Items.FindNodeWithText(S);
          if Node <> nil then
            Selected := Node
          else
            Selected := Items[0];

          MakeSelectionVisible
        end;
      end;
    end;
  end;
end;

//---------- Properties funtions ------------

procedure TComponentListForm.SetImagesSize(const aValue: Integer);
begin
  if aValue=fImagesSize then exit;
  FImagesSize:=aValue;
  UpdateVisible;
end;

procedure TComponentListForm.SetHideListTab(const aValue: boolean);
begin
  if aValue=fHideListTab then exit;
  FHideListTab:=aValue;
  TabSheetListBox.TabVisible:=FHideListTab;
end;

procedure TComponentListForm.SetHideInherTab(const aValue: boolean);
begin
  if aValue=fHideInherTab then exit;
  FHideInherTab:=aValue;
  TabSheetInheritance.TabVisible:=FHideInherTab;
end;

//----------------------

procedure  TComponentListForm.UpdateVisible;
 begin
   if visible=false then exit;
   UpdateComponentSelection(nil);
 end;

procedure  TComponentListForm.UpdateVisibleEx(aform:TCustomForm);
 begin
   if visible=false then exit;
   if FLastFormActivated=aform then exit;
   FLastFormActivated:=aform;
   UpdateComponentSelection(nil);
 end;

procedure TComponentListForm.btUpdateClick(Sender: TObject);
begin
  PatternEdit.Text:='';
  UpdateVisible;
end;

procedure TComponentListForm.btAddClick(Sender: TObject);
begin
  //if FComSelected=nil then exit;
 // AddSelectedComponent(FComSelected);
    SendSelectedComponent(nil, false);
end;

procedure TComponentListForm.btExpandClick(Sender: TObject);
begin
  fFullExpand := not fFullExpand;

  if fFullExpand then
     ImageList1.GetBitmap(1, btExpand.Glyph)  else
     ImageList1.GetBitmap(0, btExpand.Glyph);

  UpdateVisible;
end;

procedure TComponentListForm.btSettingsClick(Sender: TObject);
begin
  btSettings.PopupMenu.PopUp;
end;

procedure TComponentListForm.cMainMenuPopup(Sender: TObject);
begin
    case FImagesSize of
   1:begin
       cSmallImages.Checked:=true;
       cMediumImages.Checked:=false;
       cNormalImages.Checked:=false;
     end;
   2:begin
       cSmallImages.Checked:=false;
       cMediumImages.Checked:=true;
       cNormalImages.Checked:=false;
     end;
   3:begin
       cSmallImages.Checked:=false;
       cMediumImages.Checked:=false;
       cNormalImages.Checked:=true;
     end;
   end;

   cTabList.Checked:=NOT (fHideListTab);
   cTabInher.Checked:=NOT (fHideInherTab);
end;


procedure TComponentListForm.ZCommonProc(Sender: TObject);
 var i:integer;
 begin
  i:=-1;

  if Sender is TComponent then
    i:=TComponent(Sender).tag;

   case i of
   101:begin
        ImagesSize:=1;
       end;
   102:begin
        ImagesSize:=2;
       end;
   103:begin
        ImagesSize:=3;
       end;
   201:begin
        HideListTab:=NOT (fHideListTab);
       end;
   202:begin
        HideInherTab:=NOT (fHideInherTab);
       end;


   end;
 end;

end.

