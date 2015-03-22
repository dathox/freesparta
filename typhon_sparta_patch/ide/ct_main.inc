{%MainUnit main.pp}

{**********************************************************************
       This file is part of
       CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************}


var
     fPkgsPopUpMenu: TPopupMenu;

     fSelPageButton: TSpeedButton;
     fSelLayerButton: TSpeedButton;
     fPkgOpenButton: TToolButton;


procedure TMainIDE.exMainIDEOpenPackageFileClick(Sender: TObject);
 begin
  TPkgManager(PkgBoss).MainIDEitmPkgOpenPackageFileClick(self);
 end;

procedure TMainIDE.exMainIDEPkgGraphClick(Sender: TObject);
 begin
  TPkgManager(PkgBoss).MainIDEitmPkgPkgGraphClick(self);
 end;
//..........
procedure TMainIDE.exMainIDESearchFindClick(Sender: TObject);
 begin
  SourceEditorManager.FindClicked(self);
 end;

procedure TMainIDE.exMainIDESearchReplaceClick(Sender: TObject);
 begin
  SourceEditorManager.ReplaceClicked(self);
 end;

//..........

procedure TMainIDE.exPkgDDOpenButtonClick(Sender: TObject);
var zPos: TPoint;
begin
  zPos:=point(fPkgOpenButton.Width,fPkgOpenButton.Height);
  zPos:=fPkgOpenButton.ClientToScreen(zPos);
  DlgPopupPkgs_Execute(zPos.x,zPos.y);
end;

procedure TMainIDE.SetupSpeedButtons;

  //-----------------------------------------------
  function CreateButton(AToolBar: TToolBar; const AName, APixName: String;
                        const AOnClick: TNotifyEvent; const AHint: String): TToolButton;
  begin
    Result := TToolButton.Create(OwningComponent);
    with Result do
    begin
      Name := AName;
      Parent := AToolBar;
      Enabled := True;
      OnClick := AOnClick;
      ImageIndex := IDEImages.LoadImage(16, APixName);
      Hint := AHint;
      ShowHint:=true;
    end;
  end;
  //-----------------------------------------------
  function CreateDivider(AToolBar: TToolBar): TToolButton;
  begin
    Result := TToolButton.Create(OwningComponent);
    with Result do
    begin
      Style := tbsDivider;
      AutoSize := True;
      Parent := AToolBar;
    end;
  end;
  //-----------------------------------------------
  function CreateToolBar(AName: String): TToolBar;
  begin
    Result := TToolBar.Create(OwningComponent);
    Result.Name := AName;
    Result.Parent := MainIDEBar.pnlToolButtons;// MainIDEBar; // DaThoX
    Result.Images := IDEImages.Images_16;
  end;
  //----------------------------------------------
begin

  // DaThoX  begin
  MainIDEBar.pnlToolButtons := TPanel.Create(OwningComponent);
  with MainIDEBar.pnlToolButtons do
  begin
    Name := 'pnlToolButtons';
    Parent := MainIDEBar;
    Align := alClient;
    Caption := '';
    BevelOuter := bvNone;
    Height:=28;
    Visible := EnvironmentOptions.ComponentPaletteVisible;
    MainIDEBar.ComponentPageControl.Parent := MainIDEBar.pnlToolButtons;
  end;
  // DaThoX end

  MainIDEBar.pnlSpeedButtons := TPanel.Create(OwningComponent);
  with MainIDEBar.pnlSpeedButtons do
  begin
    Name := 'pnlSpeedButtons';
    Parent := MainIDEBar.pnlToolButtons; // DaThoX
    Align := alRight;//alLeft;
    Caption := '';
    BevelOuter := bvNone;
    Width:=28;
    Visible := EnvironmentOptions.ComponentPaletteVisible;
  end;

  MainIDEBar.tbStandard := CreateToolBar('tbStandard');
  MainIDEBar.tbStandard.AutoSize:=true;
  MainIDEBar.tbStandard.Visible := EnvironmentOptions.IDESpeedButtonsVisible;

  MainIDEBar.tbViewDebug := CreateToolBar('tbViewDebug');
  MainIDEBar.tbViewDebug.Visible := false;

//...................................  mnuNewOtherClicked

 //New
  CreateButton(MainIDEBar.tbStandard , 'NewSpeedBtn'    , 'menu_new' , @mnuNewOtherClicked, lisMenuNewOther);
  MainIDEBar.NewUnitSpeedBtn     := CreateButton(MainIDEBar.tbStandard , 'NewUnitSpeedBtn'    , 'item_unit'                 , @mnuNewUnitClicked, lisMenuNewUnit);
  MainIDEBar.NewFormSpeedBtn     := CreateButton(MainIDEBar.tbStandard , 'NewFormSpeedBtn'    , 'item_form'                 , @mnuNewFormClicked, lisMenuNewForm);

  // Project
  MainIDEBar.tbDivider1          := CreateDivider(MainIDEBar.tbStandard);
  CreateButton(MainIDEBar.tbStandard , 'NewProjectSpeedBtn'      , 'menu_project_new' ,      @mnuNewProjectClicked,         lisMenuNewProject);
  CreateButton(MainIDEBar.tbStandard , 'OpenProjectSpeedBtn'     , 'menu_project_open' ,     @mnuOpenProjectClicked,        lisOpenProject2);
  CreateButton(MainIDEBar.tbStandard , 'SaveProjectSpeedBtn'     , 'menu_project_save' ,     @mnuSaveProjectClicked,        lisKMSaveProject);
  CreateButton(MainIDEBar.tbStandard , 'SaveProjectAsSpeedBtn'   , 'menu_project_saveas',    @mnuSaveProjectAsClicked,      lisKMSaveProjectAs);
  CreateButton(MainIDEBar.tbStandard , 'ProjectInspectorSpeedBtn', 'menu_project_inspector', @mnuProjectInspectorClicked,   lisMenuProjectInspector);
  CreateButton(MainIDEBar.tbStandard , 'ProjectOptionsSpeedBtn'  , 'menu_project_options',   @mnuProjectOptionsClicked,     lisMenuProjectOptions);

  //File
  CreateDivider(MainIDEBar.tbStandard);
  MainIDEBar.OpenFileSpeedBtn    := CreateButton(MainIDEBar.tbStandard , 'OpenFileSpeedBtn'   , 'laz_open'                  , @mnuOpenClicked, lisOpen);
  MainIDEBar.SaveSpeedBtn        := CreateButton(MainIDEBar.tbStandard , 'SaveSpeedBtn'       , 'laz_save'                  , @mnuSaveClicked, lisSave);
  CreateButton(MainIDEBar.tbStandard , 'SaveAsSpeedBtn'  , 'menu_saveas' , @mnuSaveAsClicked, lisSaveAs);
  MainIDEBar.SaveAllSpeedBtn     := CreateButton(MainIDEBar.tbStandard , 'SaveAllSpeedBtn'    , 'menu_save_all'             , @mnuSaveAllClicked, lisSaveAll);

  //View
  MainIDEBar.tbDivider2          := CreateDivider(MainIDEBar.tbStandard);

  MainIDEBar.ViewUnitsSpeedBtn   := CreateButton(MainIDEBar.tbStandard, 'ViewSrcEditorSpeedBtn', 'menu_view_source_editor'   , @mnuViewSourceEditorClicked,srkmecToggleSourceEditor);
  MainIDEBar.ViewUnitsSpeedBtn   := CreateButton(MainIDEBar.tbStandard, 'ViewInspectorSpeedBtn', 'menu_view_Inspector'       , @mnuViewInspectorClicked,srkmecToggleObjectInsp);
  MainIDEBar.ViewUnitsSpeedBtn   := CreateButton(MainIDEBar.tbStandard, 'ViewcomponetsSpeedBtn', 'menu_view_componets'       , @mnuViewComponentsClick, srkmecViewComponents);
  MainIDEBar.ViewUnitsSpeedBtn   := CreateButton(MainIDEBar.tbStandard, 'ViewUnitsSpeedBtn'    , 'menu_view_units'           , @mnuViewUnitsClicked, lisHintViewUnits);
  MainIDEBar.ViewFormsSpeedBtn   := CreateButton(MainIDEBar.tbStandard, 'ViewFormsSpeedBtn'    , 'menu_view_forms'           , @mnuViewFormsClicked, lisHintViewForms);
  MainIDEBar.ToggleFormSpeedBtn  := CreateButton(MainIDEBar.tbStandard, 'ToggleFormSpeedBtn'   , 'menu_view_toggle_form_unit', @mnuToggleFormUnitCLicked, lisHintToggleFormUnit);

  //Find
  CreateDivider(MainIDEBar.tbStandard);
  CreateButton(MainIDEBar.tbStandard , 'SearchFindSpeedBtn'    , 'menu_search_find'    , @exMainIDESearchFindClick, lisMenuFind);
  CreateButton(MainIDEBar.tbStandard , 'SearchReplaceSpeedBtn' , 'menu_search_replace' , @exMainIDESearchReplaceClick, lisReplace);

 //Package
  CreateDivider(MainIDEBar.tbStandard);
  fPkgOpenButton:=
  CreateButton(MainIDEBar.tbStandard , 'OpenPackageSpeedBtn'  , 'pkg_open'             , @exMainIDEOpenPackageFileClick, lisMenuOpenPackageFile);
  CreateButton(MainIDEBar.tbStandard , 'PkgGraphSpeedBtn'     , 'pkg_graph'            , @exMainIDEPkgGraphClick, lisMenuPackageGraph);

 //Tools
  MainIDEBar.tbDivider1          := CreateDivider(MainIDEBar.tbStandard);
  CreateButton(MainIDEBar.tbStandard , 'OpenExamplesManagerBtn', 'camera' ,           @mnuToolManageExamplesClicked, lisPWViewExampleProjects);
  CreateButton(MainIDEBar.tbStandard , 'IDEOptionsBtn', 'menu_environment_options' ,  @mnuEnvGeneralOptionsClicked,  lisMenuGeneralOptions);

 //Run
  MainIDEBar.tbDivider3          := CreateDivider(MainIDEBar.tbStandard);
  MainIDEBar.BuildModeSpeedButton:= CreateButton(MainIDEBar.tbStandard, 'BuildModeSpeedButton', 'menu_compiler_options'    , @mnuChgBuildModeClicked, lisChangeBuildMode);

  // CreateMenuItem(ParentMI,itmRunMenuCompile,'itmRunMenuCompile',lisMenuCompile,'menu_build');
  // CreateMenuItem(ParentMI,itmRunMenuBuild,'itmRunMenuBuild',lisMenuBuild,'menu_build_all');

  CreateButton(MainIDEBar.tbStandard , 'CompileSpeedButton'    , 'menu_build'        , @mnuCompileProjectClicked  , lisCompile);
  CreateButton(MainIDEBar.tbStandard , 'BuildSpeedButton'      , 'menu_build_all'    , @mnuBuildProjectClicked    , lisBuild);


  MainIDEBar.RunSpeedButton      := CreateButton(MainIDEBar.tbStandard, 'RunSpeedButton'     , 'menu_run'                  , @mnuRunProjectClicked, lisRun);
  MainIDEBar.PauseSpeedButton    := CreateButton(MainIDEBar.tbStandard, 'PauseSpeedButton'   , 'menu_pause'                , @mnuPauseProjectClicked, lisPause);
  MainIDEBar.StopSpeedButton     := CreateButton(MainIDEBar.tbStandard, 'StopSpeedButton'    , 'menu_stop'                 , @mnuStopProjectClicked, lisStop);
  MainIDEBar.StepIntoSpeedButton := CreateButton(MainIDEBar.tbStandard, 'StepIntoSpeedButton', 'menu_stepinto'             , @mnuStepIntoProjectClicked, lisHintStepInto);
  MainIDEBar.StepOverSpeedButton := CreateButton(MainIDEBar.tbStandard, 'StepOverpeedButton' , 'menu_stepover'             , @mnuStepOverProjectClicked, lisHintStepOver);
  MainIDEBar.StepOutSpeedButton  := CreateButton(MainIDEBar.tbStandard, 'StepOutSpeedButton' , 'menu_stepout'              , @mnuStepOutProjectClicked, lisHintStepOut);

//------------------------------------------

  MainIDEBar.CreatePopupMenus(OwningComponent);

  MainIDEBar.OpenFileSpeedBtn.Style := tbsDropDown;
  MainIDEBar.OpenFileSpeedBtn.DropDownMenu := MainIDEBar.OpenFilePopUpMenu;
  MainIDEBar.OpenFilePopupMenu.OnPopup := @OpenFilePopupMenuPopup;

  MainIDEBar.BuildModeSpeedButton.Style:=tbsDropDown;
  MainIDEBar.BuildModeSpeedButton.DropdownMenu:=MainIDEBar.SetBuildModePopupMenu;
  MainIDEBar.SetBuildModePopupMenu.OnPopup := @SetBuildModePopupMenuPopup;

  MainIDEBar.PauseSpeedButton.Enabled := False;
  MainIDEBar.StopSpeedButton.Enabled := False;

  //-------------------------------------------------------------------
  fPkgsPopUpMenu:=TPopupMenu.Create(MainIDEBar);

  if fPkgOpenButton<>nil then
   begin
    fPkgOpenButton.Style := tbsDropDown;
    fPkgOpenButton.DropDownMenu := fPkgsPopUpMenu;
    fPkgsPopUpMenu.OnPopup := @exPkgDDOpenButtonClick;
   end;

  //-------------------------------------------------------------------
  fSelPageButton:=TSpeedButton.Create(OwningComponent); // DaThoX
  with fSelPageButton do
        begin
          Name:='PalettePageSelectBtn';
          Parent:=MainIDEBar.pnlSpeedButtons;
          LoadGlyphFromResourceName(HInstance,'SelCompPage');
          Flat := True;
          Hint := 'Click to Select Palette Page';
          SetBounds(1,27,26,26);
          OnClick := @SelComponentPageButtonClick;
        end;
  //-------------------------------------------------------------------
  fSelLayerButton:=TSpeedButton.Create(OwningComponent); // DaThoX
  with fSelLayerButton do
        begin
          Name:='LayerSelectBtn';
          Parent:=MainIDEBar.pnlSpeedButtons;
          {$IFDEF SUNOS}
             {$IFDEF CPU64}
               LoadGlyphFromResourceName(HInstance,'layer_sol64');
             {$ELSE}
               LoadGlyphFromResourceName(HInstance,'layer_sol32');
             {$ENDIF}
          {$ENDIF}
          {$IFDEF FREEBSD}
             {$IFDEF CPU64}
               LoadGlyphFromResourceName(HInstance,'layer_freebsd64');
             {$ELSE}
               LoadGlyphFromResourceName(HInstance,'layer_freebsd32');
             {$ENDIF}
          {$ENDIF}
          {$IFDEF OPENBSD}
             {$IFDEF CPU64}
               LoadGlyphFromResourceName(HInstance,'layer_openbsd64');
             {$ELSE}
               LoadGlyphFromResourceName(HInstance,'layer_openbsd32');
             {$ENDIF}
          {$ENDIF}
          {$IFDEF NETBSD}
             {$IFDEF CPU64}
               LoadGlyphFromResourceName(HInstance,'layer_netbsd64');
             {$ELSE}
               LoadGlyphFromResourceName(HInstance,'layer_netbsd32');
             {$ENDIF}
          {$ENDIF}
          {$IFDEF DRAGONFLY}
             {$IFDEF CPU64}
               LoadGlyphFromResourceName(HInstance,'layer_dragonfly64');
             {$ELSE}
               LoadGlyphFromResourceName(HInstance,'layer_dragonfly32');
             {$ENDIF}
          {$ENDIF}
          {$IFDEF LINUX}
             {$IFDEF CPU64}
               LoadGlyphFromResourceName(HInstance,'layer_linux64');
             {$ELSE}
               LoadGlyphFromResourceName(HInstance,'layer_linux32');
             {$ENDIF}
          {$ENDIF}
          {$IFDEF WINDOWS}
             {$IFDEF CPU64}
               LoadGlyphFromResourceName(HInstance,'layer_win64');
             {$ELSE}
               LoadGlyphFromResourceName(HInstance,'layer_win32');
             {$ENDIF}
          {$ENDIF}
          {$IFDEF DARWIN}
             {$IFDEF CPU64}
               LoadGlyphFromResourceName(HInstance,'layer_mac64');
             {$ELSE}
               LoadGlyphFromResourceName(HInstance,'layer_mac32');
             {$ENDIF}
          {$ENDIF}
          Flat := True;
          Hint := 'Click to Select IDE Target Layer';
          SetBounds(1,1,26,26);
        //  PopupMenu:=fSelPagePopup;
        //  OnClick := @SelPageButtonClick;
        end;
end;
