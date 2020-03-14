unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls,
  ExtCtrls, ComCtrls, SynEdit, lclintf, Registry, ShlObj, Process, pawnhighlighter;

type

  { TMainForm }

  TMainForm = class(TForm)
    FontDialog: TFontDialog;
    lbFunction: TListBox;
    lbCompiler: TListBox;
    MainMenu: TMainMenu;
    mFile: TMenuItem;
    miNew: TMenuItem;
    miOpen: TMenuItem;
    miClose: TMenuItem;
    OpenDialog: TOpenDialog;
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
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure miAboutClick(Sender: TObject);
    procedure miAssocFilesClick(Sender: TObject);
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
    procedure SynEditChange(Sender: TObject);
    procedure SynEditStatusChange(Sender: TObject; Changes: TSynStatusChanges);
  private
    FileName: String;
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

  SynEdit.Enabled := Enable;
end;

procedure TMainForm.miAboutClick(Sender: TObject);
begin
  MessageDlg('About Pawno', 'Pawno - The PAWN Compiler GUI'#10'Version 1.1'#10#10 +
  'This program uses the amazing SynEdit!'#10'https://synedit.sourceforge.net/'#10#10 +
  'Coded by:'#10'RD42 - github.com/dashr9230'#10'spookie - sa-mp.com', mtInformation, [mbOK], 0);
end;

procedure TMainForm.miLangGuideClick(Sender: TObject);
begin
  if FileExists('pawn-lang.pdf') then
    OpenDocument('pawn-lang.pdf');
end;

procedure TMainForm.miAssocFilesClick(Sender: TObject);
var
  p: String;
  r: TRegistry;
begin
  p := ExtractFilePath(Application.ExeName);
  r := TRegistry.Create;
  try
    try
      if not miAssocFiles.Checked then begin
        r.RootKey := HKEY_CLASSES_ROOT;
        r.OpenKey('.p', True);
        r.WriteString('', 'PAWN.Script');
        r.CloseKey;
        r.OpenKey('.pwn', True);
        r.WriteString('', 'PAWN.Script');
        r.CloseKey;
        r.OpenKey('.pawn', True);
        r.WriteString('', 'PAWN.Script');
        r.CloseKey;
        r.CreateKey('PAWN.Script');
        r.OpenKey('PAWN.Script\DefaultIcon', True);
        r.WriteString('', p +'pawno.exe,0');
        r.CloseKey;
        r.OpenKey('PAWN.Script\shell\open\command', True);
        r.WriteString('', p +'pawno.exe "%1"');
        r.CloseKey;
        miAssocFiles.Checked := True;
      end else begin
        r.RootKey := HKEY_CLASSES_ROOT;
        r.DeleteKey('.p');
        r.DeleteKey('.pwn');
        r.DeleteKey('.pawn');
        r.DeleteKey('PAWN.Script');
        miAssocFiles.Checked := False;
      end;
    except
      // Do nothing?
    end;
  finally
    r.Free;
  end;
  SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil);
end;

procedure TMainForm.miShowFuncListClick(Sender: TObject);
begin
  if miShowFuncList.Checked then
  begin
    miShowFuncList.Checked := False;
    lbFunction.Visible := False;
    RightSplitter.Visible := False;
  end
  else
  begin
    miShowFuncList.Checked := True;
    lbFunction.Visible := True;
    RightSplitter.Visible := False;
  end;
end;

procedure TMainForm.miFontClick(Sender: TObject);
begin
  if FontDialog.Execute then
  begin
    SynEdit.Font := FontDialog.Font;
  end;
end;

procedure TMainForm.miRunOptionsClick(Sender: TObject);
begin
  // TODO
end;

procedure TMainForm.miCompileRunClick(Sender: TObject);
begin
  miCompileClick(Self);

  // TODO: Run samp-server.exe after
end;

procedure TMainForm.miCompileClick(Sender: TObject);
var
  p: TProcess;
  sl: TStringList;
  i: Integer;
begin
  miSaveClick(Self);

  lbCompiler.Visible := True;
  BottomSplitter.Visible := True;

  try
    p := TProcess.Create(nil);
    p.Executable := 'pawncc';
    p.Parameters.Add(FileName);
    p.Parameters.Add('-;+ -(+');
    p.Options := p.Options + [poWaitOnExit];
    p.Execute;

    sl := TStringList.Create;
    sl.LoadFromStream(p.Output);

    for i := 0 to pred(sl.Count) do
        lbCompiler.AddItem(sl[i], nil);

    sl.Free;
    p.Free;
  except
    MessageDlg('Unable to execute compiler...', mtError, [mbOk], 0);
  end;
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

procedure TMainForm.miReplaceClick(Sender: TObject);
begin
  // TODO
end;

procedure TMainForm.miFindPrevClick(Sender: TObject);
begin
  // TODO
end;

procedure TMainForm.miFindNextClick(Sender: TObject);
begin
  // TODO
end;

procedure TMainForm.miFindClick(Sender: TObject);
begin
  // TODO
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
    SaveDialog.FileName := 'Untitled.pwn';
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

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  r: TModalResult;
begin
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
begin
  FileName := '';
  SetTitle('Pawno');
end;

end.

