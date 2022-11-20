unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls, Clipbrd,
  ExtCtrls, ComCtrls, SynEdit, lclintf, Registry, ShlObj, Process, pawnhighlighter,
  FileUtil, runoptions, LazFileUtils, SynEditTypes, IniFiles;

function IsUserAnAdmin(): Integer; external 'shell32.dll';

type

  { TMainForm }

  TMainForm = class(TForm)
    FindDialog: TFindDialog;
    FontDialog: TFontDialog;
    ilToolbar: TImageList;
    lbFunction: TListBox;
    lbCompiler: TListBox;
    MainMenu: TMainMenu;
    MenuItem1: TMenuItem;
    miOutputCopyAll: TMenuItem;
    miOutputClear: TMenuItem;
    Separator1: TMenuItem;
    miShowCompOutput: TMenuItem;
    mFile: TMenuItem;
    miNew: TMenuItem;
    miOpen: TMenuItem;
    miClose: TMenuItem;
    OpenDialog: TOpenDialog;
    pmOutput: TPopupMenu;
    ReplaceDialog: TReplaceDialog;
    SaveDialog: TSaveDialog;
    SEP1: TMenuItem; // seperator
    miSave: TMenuItem;
    miSaveAs: TMenuItem;
    SEP2: TMenuItem; // seperator
    miExit: TMenuItem;
    mEdit: TMenuItem;
    miUndo: TMenuItem;
    miRedo: TMenuItem;
    SEP3: TMenuItem; // seperator
    miCut: TMenuItem;
    miCopy: TMenuItem;
    miPaste: TMenuItem;
    miDelete: TMenuItem;
    SEP4: TMenuItem; // seperator
    miFind: TMenuItem;
    miFindNext: TMenuItem;
    miFindPrev: TMenuItem;
    miReplace: TMenuItem;
    miGoTo: TMenuItem;
    SEP5: TMenuItem; // seperator
    miSelectAll: TMenuItem;
    mBuild: TMenuItem;
    miCompile: TMenuItem;
    miCompileRun: TMenuItem;
    SEP6: TMenuItem; // seperator
    miRunOptions: TMenuItem;
    mOptions: TMenuItem;
    miFont: TMenuItem;
    miShowFuncList: TMenuItem;
    miAssocFiles: TMenuItem;
    mHelp: TMenuItem;
    miLangGuide: TMenuItem;
    SEP7: TMenuItem; // seperator
    miAbout: TMenuItem;
    EditPopupMenu: TPopupMenu;
    popUndo: TMenuItem;
    popRedo: TMenuItem;
    SEP8: TMenuItem; // seperator
    popCut: TMenuItem;
    popCopy: TMenuItem;
    popPaste: TMenuItem;
    popDelete: TMenuItem;
    SEP9: TMenuItem; // seperator
    popSelectAll: TMenuItem;
    RightSplitter: TSplitter;
    BottomSplitter: TSplitter;
    StatusBar: TStatusBar;
    SynEdit: TSynEdit;
    PawnHighlighter: TSynPawnSyn;
    ToolBar1: TToolBar;
    tbNew: TToolButton;
    tbCompile: TToolButton;
    tbCompileRun: TToolButton;
    tbSpace2: TToolButton;
    tbHelpTopics: TToolButton;
    tbOpen: TToolButton;
    tbSave: TToolButton;
    tbSpace1: TToolButton;
    tbFind: TToolButton;
    tbFindNext: TToolButton;
    tbFindPrev: TToolButton;
    tbReplace: TToolButton;
    ToolButton5: TToolButton;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lbFunctionDblClick(Sender: TObject);
    procedure miOutputCopyClick(Sender: TObject);
    procedure miOutputClearClick(Sender: TObject);
    procedure miOutputCopyAllClick(Sender: TObject);
    procedure miShowCompOutputClick(Sender: TObject);
    procedure miAboutClick(Sender: TObject);
    procedure miAssocFilesClick(Sender: TObject);
    procedure MakeAssoc(Assoc: Boolean);
    procedure miCloseClick(Sender: TObject);
    procedure miCompileClick(Sender: TObject);
    procedure miCompileRunClick(Sender: TObject);
    procedure miCopyClick(Sender: TObject);
    procedure miCutClick(Sender: TObject);
    procedure miDeleteClick(Sender: TObject);
    procedure miExitClick(Sender: TObject);
    procedure miFindClick(Sender: TObject);
    procedure miFindNextClick(Sender: TObject);
    procedure miFindPrevClick(Sender: TObject);
    procedure miFontClick(Sender: TObject);
    procedure miGoToClick(Sender: TObject);
    procedure miLangGuideClick(Sender: TObject);
    procedure miNewClick(Sender: TObject);
    procedure miOpenClick(Sender: TObject);
    procedure miPasteClick(Sender: TObject);
    procedure miRedoClick(Sender: TObject);
    procedure miReplaceClick(Sender: TObject);
    procedure miRunOptionsClick(Sender: TObject);
    procedure miSaveAsClick(Sender: TObject);
    procedure miSaveClick(Sender: TObject);
    procedure miSelectAllClick(Sender: TObject);
    procedure miShowFuncListClick(Sender: TObject);
    procedure miUndoClick(Sender: TObject);
    procedure OutputMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ReplaceDialogReplace(Sender: TObject);
    procedure SynEditChange(Sender: TObject);
    procedure SynEditStatusChange(Sender: TObject; {%H-}Changes: TSynStatusChanges);
  private
    FileName: String;
    ReplacePos: TPoint;
    Replacing: Boolean;
    procedure SetTitle(Title: String);
    procedure InitHighlighter;
    procedure SetControls(Enable: Boolean);
  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.SetTitle(Title: String);
begin
  Application.Title := Title;
  MainForm.Caption := Title;
end;

procedure TMainForm.InitHighlighter;
begin
  PawnHighlighter := TSynPawnSyn.Create(Self);
  PawnHighlighter.AsmAttri.Style := [fsBold];
  PawnHighlighter.CommentAttri.Foreground := clGreen;
  PawnHighlighter.DirecAttri.Foreground := clBlue;
  PawnHighlighter.IdentifierAttri.Foreground := clBlack;
  PawnHighlighter.KeyAttri.Foreground := clBlue;
  PawnHighlighter.KeyAttri.Style := [];
  PawnHighlighter.NumberAttri.Foreground := clNavy;
  PawnHighlighter.StringAttri.Foreground := clNavy;
  SynEdit.Highlighter := PawnHighlighter;
end;

procedure TMainForm.SetControls(Enable: Boolean);
begin
  // MainMenu
  miClose.Enabled := Enable;
  miSave.Enabled := Enable;
  miSaveAs.Enabled := Enable;
  miUndo.Enabled := Enable;
  miRedo.Enabled := Enable;
  miCut.Enabled := Enable;
  miCopy.Enabled := Enable;
  miPaste.Enabled := Enable;
  miDelete.Enabled := Enable;
  miFind.Enabled := Enable;
  miFindNext.Enabled := Enable;
  miFindPrev.Enabled := Enable;
  miReplace.Enabled := Enable;
  miGoTo.Enabled := Enable;
  miSelectAll.Enabled := Enable;
  miCompile.Enabled := Enable;
  miCompileRun.Enabled := Enable;

  // EditPopupMenu
  popUndo.Enabled := Enable;
  popRedo.Enabled := Enable;
  popCut.Enabled := Enable;
  popCopy.Enabled := Enable;
  popPaste.Enabled := Enable;
  popDelete.Enabled := Enable;
  popSelectAll.Enabled := Enable;

  SynEdit.Enabled := Enable;

  lbFunction.Enabled := Enable;

  // ToolBar
  tbSave.Enabled := Enable;
  tbFind.Enabled := Enable;
  tbFindNext.Enabled := Enable;
  tbFindPrev.Enabled := Enable;
  tbReplace.Enabled := Enable;
  tbCompile.Enabled := Enable;
  tbCompileRun.Enabled := Enable;
end;

procedure TMainForm.miAboutClick(Sender: TObject);
begin
  MessageDlg('About Pawno', 'Pawno - The PAWN Compiler GUI'#10'Version 1.2'#10#10 +
  'This program uses the amazing SynEdit!'#10'https://synedit.sourceforge.net/'#10#10 +
  'Coded by:'#10'RD42 - github.com/dashr9230', mtInformation, [mbOK], 0);
end;

procedure TMainForm.miLangGuideClick(Sender: TObject);
begin
  if FileExists('pawn-lang.pdf') then
    OpenDocument('pawn-lang.pdf');
end;

procedure TMainForm.miAssocFilesClick(Sender: TObject);
begin
  miAssocFiles.Checked := not miAssocFiles.Checked;
  MakeAssoc(miAssocFiles.Checked);
end;

procedure TMainForm.MakeAssoc(Assoc: Boolean);
var
  Registry: TRegistry;
  Settings: TIniFile;
begin
  if IsUserAnAdmin() = 0 then begin
    StatusBar.Panels[0].Text := 'Failed file association. Try run Pawno in administrator mode.';
    miAssocFiles.Checked := False;
    Exit;
  end;

  Settings := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'settings.ini');
  Settings.WriteBool('General', 'FileAssoc', Assoc);
  Settings.Free;

  Registry := TRegistry.Create;
  try
    if Assoc then begin
      Registry.RootKey := HKEY_CLASSES_ROOT;
      Registry.OpenKey('.p', True);
      Registry.WriteString('', 'PAWN.Script');
      Registry.CloseKey;
      Registry.OpenKey('.pwn', True);
      Registry.WriteString('', 'PAWN.Script');
      Registry.CloseKey;
      Registry.OpenKey('.pawn', True);
      Registry.WriteString('', 'PAWN.Script');
      Registry.CloseKey;
      Registry.CreateKey('PAWN.Script');
      Registry.OpenKey('PAWN.Script\DefaultIcon', True);
      Registry.WriteString('', Application.ExeName + ',1');
      Registry.CloseKey;
      Registry.OpenKey('PAWN.Script\shell\open\command', True);
      Registry.WriteString('', Application.ExeName + ' "%1"');
      Registry.CloseKey;
    end else begin
      Registry.RootKey := HKEY_CLASSES_ROOT;
      Registry.DeleteKey('.p');
      Registry.DeleteKey('.pwn');
      Registry.DeleteKey('.pawn');
      Registry.DeleteKey('PAWN.Script');
    end;
  finally
    Registry.Free;
  end;

  SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil);
  miAssocFiles.Checked := Assoc;
end;

procedure TMainForm.miShowCompOutputClick(Sender: TObject);
begin
  miShowCompOutput.Checked := not miShowCompOutput.Checked;
  lbCompiler.Visible := miShowCompOutput.Checked;
  BottomSplitter.Visible := miShowCompOutput.Checked;
end;

procedure TMainForm.miShowFuncListClick(Sender: TObject);
var
  Settings: TIniFile;
begin
  miShowFuncList.Checked := not miShowFuncList.Checked;
  lbFunction.Visible := miShowFuncList.Checked;
  RightSplitter.Visible := miShowFuncList.Checked;

  Settings := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'settings.ini');
  Settings.WriteBool('Display', 'ShowFuncList', miShowFuncList.Checked);
  Settings.Free;
end;

procedure TMainForm.miFontClick(Sender: TObject);
var
  SettingsIni: TIniFile;
  FontDialog.Font := SynEdit.Font; 
begin
  if FontDialog.Execute then
  begin
    SynEdit.Font := FontDialog.Font;
  end;

  SettingsIni := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'settings.ini');
  SettingsIni.WriteString('Display', 'Font_Name', SynEdit.Font.Name);
  SettingsIni.WriteInteger('Display', 'Font_Size', SynEdit.Font.Size);
  SettingsIni.Free;
end;

procedure TMainForm.miRunOptionsClick(Sender: TObject);
var
  Form: TRunOptionsForm;
begin
  //RunOptionsForm.ShowModal;

  Form := TRunOptionsForm.Create(Application);
  Form.ShowModal;
  Form.Free;
end;

procedure TMainForm.miCompileRunClick(Sender: TObject);
var
  p: TProcess;
  s, d: String;
begin
  miCompileClick(Self);

  s := ExtractFileNameWithoutExt(FileName);
  if not FileExists(s + '.amx') then Exit;

  if CopyToStr <> '' then
  begin
    d := ExtractFileNameWithoutExt(ExtractFileName(FileName));
    CopyFile(s +'.amx', CopyToStr +'\'+ d +'.amx');
  end;

  if ExecuteThisStr <> '' then
  begin
    p := TProcess.Create(nil);
    try
      p.Executable := ExecuteThisStr;

      if ParametersStr <> '' then
        p.Parameters.Add(ParametersStr);

      p.Execute;
    finally
      p.Free;
    end;
  end;
end;

procedure TMainForm.miCompileClick(Sender: TObject);
var
  p: TProcess;
  sl: TStringList;
  i: Integer;
  SavedPath: string;
begin
  miSaveClick(Self);

  lbCompiler.Visible := True;
  BottomSplitter.Visible := True;
  miShowCompOutput.Checked := True;

  lbCompiler.Clear;

  SavedPath := GetCurrentDir;
  SetCurrentDir(ExtractFilePath(FileName));

  try
    p := TProcess.Create(nil);
    p.Executable := '"'+ ExtractFilePath(ParamStr(0)) +'\pawncc.exe"';
    p.Parameters.Add('"'+ FileName +'"');
    p.Parameters.Add('-;+ -(+');
    p.Options := p.Options + [poWaitOnExit, poUsePipes, poNoConsole];
    p.Execute;

    sl := TStringList.Create;

    sl.LoadFromStream(p.Stderr);
    for i := 0 to pred(sl.Count) do
        lbCompiler.AddItem(sl[i], nil);

    sl.LoadFromStream(p.Output);
    for i := 0 to pred(sl.Count) do
        lbCompiler.AddItem(sl[i], nil);

    sl.Free;
    p.Free;
  except
    MessageDlg('Unable to execute compiler...', mtError, [mbOk], 0);
  end;

  SetCurrentDir(SavedPath);

end;

procedure TMainForm.miSelectAllClick(Sender: TObject);
begin
  SynEdit.SelectAll;
end;

procedure TMainForm.miGoToClick(Sender: TObject);
var
  s: String;
begin
  s := '';
  if InputQuery('Go to...', 'Enter line number:', s) then
  begin
    SynEdit.CaretXY := Point(1, StrToInt(s));
  end;
end;

procedure TMainForm.ReplaceDialogReplace(Sender: TObject);
var
  o: TSynSearchOptions;
begin
  if ReplaceDialog.ReplaceText = SynEdit.SelText then Exit;

  o := [];
  if frReplace in ReplaceDialog.Options then
    o := o + [ssoReplace];
  if frReplaceAll in ReplaceDialog.Options then
    o := o + [ssoReplaceAll];
  if frWholeWord in ReplaceDialog.Options then
    o := o + [ssoWholeWord];
  if frMatchCase in ReplaceDialog.Options then
    o := o + [ssoMatchCase];
  if frEntireScope in ReplaceDialog.Options then
    o := o + [ssoEntireScope];
  if not (frDown in ReplaceDialog.Options) then
    o := o + [ssoBackwards];

  SynEdit.SearchReplaceEx(ReplaceDialog.FindText, ReplaceDialog.ReplaceText, o, ReplacePos);
end;

procedure TMainForm.miReplaceClick(Sender: TObject);
begin
  ReplaceDialog.Execute;
  Replacing := True;
end;

procedure TMainForm.miFindPrevClick(Sender: TObject);
var
  o: TSynSearchOptions;
  c: TFindOptions;
begin
  o := [];

  if Replacing then
    c := ReplaceDialog.Options
  else
    c := FindDialog.Options;

  if frWholeWord in c then
    o := o + [ssoWholeWord];
  if frMatchCase in c then
    o := o + [ssoMatchCase];
  if frEntireScope in c then
    o := o + [ssoEntireScope];
  if frDown in c then
    o := o + [ssoBackwards];

  if Replacing then
    SynEdit.SearchReplaceEx(ReplaceDialog.FindText, '', o, ReplacePos)
  else
    SynEdit.SearchReplace(FindDialog.FindText, '', o);
end;

procedure TMainForm.miFindNextClick(Sender: TObject);
var
  o: TSynSearchOptions;
  c: TFindOptions;
begin
  o := [];

  if Replacing then
    c := ReplaceDialog.Options
  else
    c := FindDialog.Options;

  if frWholeWord in c then
    o := o + [ssoWholeWord];
  if frMatchCase in c then
    o := o + [ssoMatchCase];
  if frEntireScope in c then
    o := o + [ssoEntireScope];
  //if frDown in c then
    //o := o - [ssoBackwards];

  if Replacing then
    SynEdit.SearchReplaceEx(ReplaceDialog.FindText, '', o, ReplacePos)
  else
    SynEdit.SearchReplace(FindDialog.FindText, '', o);
end;

procedure TMainForm.miFindClick(Sender: TObject);
begin
  FindDialog.Execute;
  Replacing := False;
end;

procedure TMainForm.miDeleteClick(Sender: TObject);
begin
  SynEdit.ClearSelection;
end;

procedure TMainForm.miPasteClick(Sender: TObject);
begin
  SynEdit.PasteFromClipboard;
end;

procedure TMainForm.miCopyClick(Sender: TObject);
begin
  SynEdit.CopyToClipboard;
end;

procedure TMainForm.miCutClick(Sender: TObject);
begin
  SynEdit.CutToClipboard;
end;

procedure TMainForm.miRedoClick(Sender: TObject);
begin
  SynEdit.Redo;
end;

procedure TMainForm.miUndoClick(Sender: TObject);
begin
  SynEdit.Undo;
end;

procedure TMainForm.miExitClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.miSaveAsClick(Sender: TObject);
begin
  if FileName = '' then
    SaveDialog.FileName := 'Untitled'
  else
    SaveDialog.FileName := FileName;

  if SaveDialog.Execute then begin
    FileName := SaveDialog.FileName;
    SynEdit.Lines.SaveToFile(FileName);
    SynEdit.Modified := False;
    SetTitle(ExtractFileName(FileName) +' - Pawno');
  end;
end;

procedure TMainForm.miSaveClick(Sender: TObject);
begin
  if FileName = '' then begin
    SaveDialog.FileName := 'Untitled';
    if SaveDialog.Execute then begin
      FileName := SaveDialog.FileName;
      SynEdit.Lines.SaveToFile(FileName);
      SynEdit.Modified := False;
      SetTitle(ExtractFileName(FileName) +' - Pawno');
    end;
  end else begin
    SynEdit.Lines.SaveToFile(FileName);
    SynEdit.Modified := False;
    SetTitle(ExtractFileName(FileName) +' - Pawno');
  end;
end;

procedure TMainForm.miCloseClick(Sender: TObject);
var
  r: TModalResult;
begin
  if SynEdit.Modified then begin
    r := MessageDlg('File was modified. Save changes?', mtConfirmation,
      [mbYes, mbNo, mbCancel], 0);
    if r = mrCancel then
      Exit
    else if r = mrYes then
      miSaveClick(Self);
  end;

  SynEdit.Lines.Clear;
  SetTitle('Pawno');
  SetControls(False);
end;

procedure TMainForm.miOpenClick(Sender: TObject);
var
  r: TModalResult;
begin
  if SynEdit.Modified then begin
    r := MessageDlg('File was modified. Save changes?', mtConfirmation,
      [mbYes, mbNo, mbCancel], 0);
    if r = mrCancel then
      Exit
    else if r = mrYes then
      miSaveClick(Self);
  end;

  if OpenDialog.Execute then begin
    SynEdit.Lines.Clear;
    FileName := OpenDialog.FileName;
    SynEdit.Lines.LoadFromFile(FileName);
    SynEdit.Modified := False;
    SetTitle(ExtractFileName(FileName) +' - Pawno');
    SetControls(True);
    InitHighlighter;
  end;
end;

procedure TMainForm.miNewClick(Sender: TObject);
var
  r: TModalResult;
begin
  if SynEdit.Modified then
  begin
    r := MessageDlg('File was modified. Save changes?', mtConfirmation,
      [mbYes, mbNo, mbCancel], 0);
    if r = mrCancel then
      Exit
    else if r = mrYes then
      miSaveClick(Self);
  end;

  SynEdit.Lines.Clear;

  if FileExists('new.pwn') then
  begin
    SynEdit.Lines.LoadFromFile('new.pwn');
    SynEdit.Modified := False;
  end;
  FileName := '';
  SetTitle('Untitled - Pawno');
  InitHighlighter;
  SetControls(True);
end;

procedure TMainForm.SynEditStatusChange(Sender: TObject;
  Changes: TSynStatusChanges);
var
  l, c: String;
begin
  if SynEdit.IsEnabled then begin
    c := IntToStr(SynEdit.CaretX);
    l := IntToStr(SynEdit.CaretY);
    StatusBar.Panels[1].Text:='Ln: '+ l +'  Col: '+ c;
  end;
end;

procedure TMainForm.SynEditChange(Sender: TObject);
begin
  if FileName = '' then
    SetTitle('Untitled* - Pawno')
  else
    SetTitle(ExtractFileName(FileName) +'* - Pawno');
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  if 0 < ParamCount then
  begin
    FileName := ParamStr(1);
    SynEdit.Lines.LoadFromFile(FileName);
    SetTitle(ExtractFileName(FileName) +' - Pawno');
    SetControls(True);
    InitHighlighter;
  end;
end;

procedure TMainForm.lbFunctionDblClick(Sender: TObject);
begin
  if (lbFunction.Items[lbFunction.ItemIndex] <> '') and
  (Pos('//', lbFunction.Items[lbFunction.ItemIndex]) = 0) then
  begin
    SynEdit.InsertTextAtCaret(lbFunction.Items[lbFunction.ItemIndex] +'()');
    SynEdit.CaretX := SynEdit.CaretX - 1;
  end;
end;

procedure TMainForm.OutputMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  i: LongInt;
  CursorPoint: TPoint;
begin
  CursorPoint.X := X;
  CursorPoint.Y := Y;

  //lbCompiler.ClearSelection;

  for i := 0 to pred(lbCompiler.Items.Count) do
    if PtInRect(lbCompiler.ItemRect(i), CursorPoint) then
      lbCompiler.Selected[i] := True
    else
      lbCompiler.Selected[i] := False;
end;

procedure TMainForm.miOutputCopyClick(Sender: TObject);
var
  i: LongInt;
  Str: String;
begin
  Str := '';
  for i := 0 to pred(lbCompiler.Items.Count) do
    if lbCompiler.Selected[i] then
       Str := Str + lbCompiler.Items[i] + #13#10;
  Clipboard.AsText := Str;

  //Clipboard.AsText := lbCompiler.GetSelectedText;
end;

procedure TMainForm.miOutputCopyAllClick(Sender: TObject);
var
  Str: String;
  i: LongInt;
begin;
  Str := '';
  for i := 0 to pred(lbCompiler.Items.Count) do
      Str:= Str + lbCompiler.Items[i] + #13#10;
  Clipboard.AsText := Str;
end;

procedure TMainForm.miOutputClearClick(Sender: TObject);
begin
  lbCompiler.Clear;
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  r: TModalResult;
  Settings: TIniFile;
begin
  Settings := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'settings.ini');

  Settings.WriteBool('Display', 'WindowMax', wsMaximized = MainForm.WindowState);
  MainForm.WindowState := wsNormal;
  Application.ProcessMessages;

  Settings.WriteInteger('Display', 'WindowX', MainForm.Left);
  Settings.WriteInteger('Display', 'WindowY', MainForm.Top);
  Settings.WriteInteger('Display', 'WindowW', MainForm.ClientWidth);
  Settings.WriteInteger('Display', 'WindowH', MainForm.ClientHeight);
  Settings.WriteInteger('Display', 'Splitter', lbFunction.Width);

  Settings.Free;

  if SynEdit.Modified then begin
    r := MessageDlg('File was modified. Save changes?', mtConfirmation,
      [mbYes, mbNo, mbCancel], 0);
    if r = mrCancel then
      CloseAction := caNone
    else if r = mrYes then
      miSaveClick(Self);
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  Settings: TIniFile;
  sl: TStringList;
  tf: TextFile;
  i,t: Integer;
  l: String;
begin
  SynEdit.Font.Style := [];

  Settings := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'settings.ini');

  CopyToStr := Settings.ReadString('RunOpts', 'CopyDir', '');
  ExecuteThisStr := Settings.ReadString('RunOpts', 'ExeFile', '');
  ParametersStr := Settings.ReadString('RunOpts', 'Params', '');

  SynEdit.Font.Name := Settings.ReadString('Display', 'Font_Name', 'Courier New');
  SynEdit.Font.Size := Settings.ReadInteger('Display', 'Font_Size', 10);
  lbFunction.Font.Name := SynEdit.Font.Name;
  lbFunction.Font.Size := SynEdit.Font.Size;

  miAssocFiles.Checked := Settings.ReadBool('General', 'FileAssoc', True);
  miShowFuncList.Checked := Settings.ReadBool('General', 'ShowFuncList', True);
  RightSplitter.Visible := miShowFuncList.Checked;
  lbFunction.Visible:= miShowFuncList.Checked;

  MainForm.Left := Settings.ReadInteger('Display', 'WindowX', MainForm.Left);
  MainForm.Top := Settings.ReadInteger('Display', 'WindowY', MainForm.Top);
  MainForm.ClientWidth := Settings.ReadInteger('Display', 'WindowW', MainForm.ClientWidth);
  MainForm.ClientHeight := Settings.ReadInteger('Display', 'WindowH', MainForm.ClientHeight);
  lbFunction.Width := Settings.ReadInteger('Display', 'Splitter', lbFunction.Width);

  if Settings.ReadBool('Display', 'WindowMax', False) then
     MainForm.WindowState := wsMaximized;

  Settings.Free;

  MakeAssoc(miAssocFiles.Checked);

  FileName := '';
  SetTitle('Pawno');

  sl := TStringList.Create;
  try
    FindAllFiles(sl, 'include/', '*.inc', False);
    for i := 0 to pred(sl.Count) do
    begin
      AssignFile(tf, sl[i]);
      Reset(tf);

      lbFunction.AddItem('// '+ ExtractFileName(sl[i]), nil);
      lbFunction.AddItem('', nil);

      while not EOF(tf) do
      begin
        ReadLn(tf, l);
        l := TrimLeft(l);
        if CompareStr('native', Copy(l, 1, 6)) = 0 then
        begin
          if Pos('operator', l) <> 0 then continue; // Ignore operators
          Delete(l, 1, 6); // Delete 'native' from start
          //l := TrimLeft(l); // Removes spaces/tabs from the beginning
          l := Copy(l, 1, Pos('(', l) - 1); // Copy function name until hits '('
          t := Pos(':', l); // For checking tags like 'Float:','DB:', etc...
          if t <> 0  then
            lbFunction.AddItem(Trim(Copy(l, t + 1)), nil)
          else
            lbFunction.AddItem(Trim(Copy(l, 1)), nil);
        end;
      end;

      lbFunction.AddItem('', nil);
      CloseFile(tf);
    end;
    sl.Free;

    StatusBar.Panels[0].Text := 'Pawno is ready.';
  except
    StatusBar.Panels[0].Text := 'Failed to read includes. Try run Pawno in administrator mode.';
    //lbFunction.Visible := False;
    //miShowFuncList.Checked := False;
  end;
end;

end.

