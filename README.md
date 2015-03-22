# freesparta
FreeSparta - is a distribution/patch of the mature Lazarus/Typhon environment

# WARNING

* **Sparta was tested only on Windows 7 32/64!**
* **pl_GlassDocking** don't work correctly in pure Lazarus distribution (for docking/undocking modules with designers). In Typhon IDE all works fine. Lazarus works fine with Sparta packages without **pl_GlassDocking / AnchorDocking**.
* You need to use [Generics.Collections library](https://github.com/dathox/generics.collections).

# How to install (Windows)

##For CodeTyphon 5.2

1. Delete old CodeTyphon directory (C:\codetyphon) even if it is CodeTyphon 5.2
2. [Download CodeTyphon 5.2](http://www.filefactory.com/file/1w2p5ebn5h97/CodeTyphonIns.zip)
3. Extract CodeTyphonIns.zip in C:\ 
4. Run windows console with administrator privileges
5. In windows console goto C:\CodeTyphonIns directory
6. Run from console install.bat file
7. Select action 0 ( Install CodeTyphon Studio (remove old first) )
8. Select action 1 ( Run CodeTyphon Center (CTC) )
9. Select from CodeTyphon Center window "CodeTyphon → Update Manager"
10. Change "Latest Released Version (Stable)" to "LAB Development Version (Experimental)"
11. Select "Actions → Check for Updates"
12. Select "Actions → Start Update..."
13. Please wait... :)
14. Now we have console window again (but new version -  "CodeTyphon Studio 5.30 Setup"). 
15. Select action 1 "Run CodeTyphon Center (CTC)"
16. Click "Typhon-IDE → Typhon32 IDE, Build SmallIDE"
17. Please wait... :|
18. Click "Typhon-IDE → Run Typhon32 IDE"
19. Now you can close "CodeTyphon Center"
20. In Typhon IDE click "Tools → Options ..."
21. Select "Desktop"
22. Uncheck "Use Embedded Form Designer"
23. Click OK
24. Restart Typhon IDE
25. Click "Package → Install/Uninstall Packages ..."
26. Remove all possible Packages from "Installed" list (all below "LCLBase 1.5")
27. Click button "Save and rebuld IDE"
28. Click "Continue"
29. Copy all files from "typhon_sparta_patch" into "C:\codetyphon\typhon"
30. Delete all files from:
   *"C:\codetyphon\typhon\units"
   *"C:\codetyphon\typhon\components\BaseIdeintf\units"
   *"C:\codetyphon\typhon\lcl\units"
31. Click "Tools → Build Typhon with Profile: Optimized IDE"
32. Open and install package "1_spartaBasic\dtx_spartabasic.lpk"
33. Open and install package "2_spartaStarter\dtx_spartastarter.lpk"
34. Open and install package "3_spartaProfessional\dtx_spartaprofessional.lpk"
35. If you like Delphi integrated UI then please install package pl_glassdocking "C:\codetyphon\typhon\components\pl_GlassDocking\pl_glassdocking.lpk"
36. GL & HF :D

##For Lazarus 1.5 rev. 48023

1. You can follow the steps 1-17 of "For CodeTyphon 5.2" guide to get FPC compiler with Generics.Collections library
2. Export SVN repo to C:\codetyphon\lazarus
3. Copy all files from "lazarus_sparta_patch" into "C:\codetyphon\lazarus"
4. Run in command line this script:
   ```bat
   START "" /D"C:\codetyphon\lazarus\" /B /WAIT "C:\codetyphon\fpc\fpc32\bin\i386-win32\make.exe" all OPT="-g-" INSTALL_PREFIX="C:\codetyphon\lazarus\"
   ```
5. Run Lazarus "C:\codetyphon\lazarus\lazarus.exe"
6. Open and install package "1_spartaBasic\dtx_spartabasic.lpk"
7. Open and install package "2_spartaStarter\dtx_spartastarter.lpk"
8. Open and install package "3_spartaProfessional\dtx_spartaprofessional.lpk"
9. If you like Delphi integrated UI then please install package pl_glassdocking
10. GL & HF :D

#What was improved

**components\customdrawn\source\customdrawnextras.pas**
* PersistentDeleted event

**components\pl_GlassDocking\** (modification of AnchorDocking library)
* Delphi like active docked form header
* New X icon ;)
* Fix for message window (http://bugs.freepascal.org/view.php?id=18538)
* Fix for closing wrong tab (for clones of module)

**components\ideintf\laz_images.res**
* New icons :)

**components\ideintf\componenteditors.pas**
* New class TIDEComponentsMaster. Utils for hiding non visual components.

**components\ideintf\componentreg.pas**
* Extended "Component Added Event"  for new components palette in Sparta
* "Multi Select" option for components palette (for new components palette in Sparta and for existing Delphi like components palette)

**components\ideintf\formeditingintf.pas**
* Designer Base Class system (now any "Designer Base Class" like TForm, TFrame, TDataModule can be replaced by plugin)
* Utils functions IsFormDesign and LookupRoot

**components\ideintf\idewindowintf.pas**
* Changes for "IDETabMaster" system (Code/Designer pages for modules)

**components\ideintf\lazideintf.pas**
* Changes for "IDETabMaster" system (Code/Designer pages for modules)
* Infrastructure for other (user defined) tab assigned to module (like History Tab from Delphi)

**components\ideintf\objectinspector.pp**
* Fix for copy/paste problem for integrated IDE with GlassDocking/AnchorDocking package

**components\ideintf\propedits.pp**
* New event "Persistent Deleted". Useful for statistics for designed form (for example count of object, or for refresing state of sparta design time utils)

**components\ideintf\srceditorintf.pas**
* New events called after a Window is shown/hidden (semWindowShow/semWindowHide)

**components\ideintf\unitresources.pas**
* Designer Base Class system (now any "Designer Base Class" like TForm, TFrame, TDataModule can be replaced by plugin)

**designer\controlselection.pp**
* Designer Base Class system (now any "Designer Base Class" like TForm, TFrame, TDataModule can be replaced by plugin)
* Show/Hide non visual components (by DEComponentsMaster)

**designer\customnonformdesigner.pas**
* Many changes for "Designer Base Class system" (now any "Designer Base Class" like TForm, TFrame, TDataModule can be replaced by plugin)

**designer\designer.pp**
* Extended "Component Added Event"  for new components palette in Sparta
* Show/Hide non visual components (by DEComponentsMaster)
* Changes for "Designer Base Class system" (now any "Designer Base Class" like TForm, TFrame, TDataModule can be replaced by plugin)
* New event "Persistent Deleted". Useful for statistics for designed form (for example count of object, or for refresing state of sparta design time utils)
* Fix for painting points (for Design tab of Module)

**designer\framedesigner.pas**
* Many changes for "Designer Base Class system" (now any "Designer Base Class" like TForm, TFrame, TDataModule can be replaced by plugin)

**designer\noncontroldesigner.pas**
* Many changes for "Designer Base Class system" (now any "Designer Base Class" like TForm, TFrame, TDataModule can be replaced by plugin)

**ide\codeexplorer.pas**
* Fix for switching Code/Designer (use KeyDown insted of KeyUp)

**ide\componentlist.pas**
* Extended "Component Added Event"  for new components palette in Sparta
* "Multi Select" option for components palette (for new components palette in Sparta and for existing Delphi like components palette)

**ide\componentpalette.pas**
* Extended "Component Added Event"  for new components palette in Sparta
* "Multi Select" option for components palette (for new components palette in Sparta and for existing Delphi like components palette)

**ide\customformeditor.pp**
* Changes for "Designer Base Class system" (now any "Designer Base Class" like TForm, TFrame, TDataModule can be replaced by plugin)

**ide\main.pp**
* Fix for copy/paste problem for integrated IDE with GlassDocking/AnchorDocking package
* Show/Hide components palette button for integrated IDE with GlassDocking/AnchorDocking package
* CodeTyphon buttons layout
* Extended "Component Added Event"  for new components palette in Sparta
* Fix for switching Code/Designer
* Changes for "IDETabMaster" system (Code/Designer pages for modules)
* JumpToCompilerMessage event

**ide\mainbar.pas**
* Changes for Show/Hide components palette button

**ide\mainbase.pas**
* Changes for "IDETabMaster" system (Code/Designer pages for modules)

**ide\sourceeditor.pp**
* New notifications semWindowShow and semWindowHide
* Bug fix for current ActiveEditor in docked IDE
* Modification for plugable PageControl (fix for problem with finding SourceEditor), changes for "IDETabMaster" system (Code/Designer pages for modules)

**ide\sourcefilemanager.pas**
* Changes for "Designer Base Class system" (now any "Designer Base Class" like TForm, TFrame, TDataModule can be replaced by plugin)
* Changes for "IDETabMaster" system (Code/Designer pages for modules)

**lcl\include\wincontrol.inc**
* Bug fix for method TWinControl.AlignControls (conditional expression "if NeedAlignWork then" is commented). Sometimes when we add only one control, and we will use scroolbars properties in ObjectInspector on the form, then the control is scrolled in wrong way (vertically instead of horizontal).

**lcl\include\win32\win32callback.inc**
* Bug fix for OverlayWindowProc function (window used by GetDesignerDC is moved when scroolbars are modified at designtime)

**lcl\controls.pp**
* TDockManager. GetChildSite new method (used to hide/show in docked version of IDE some parts by resizing window, for example to show/hide components palette by using dedicated button)

**pl_Cindy, pl_ExDesign**
* Small bug fix for CodeTyphon