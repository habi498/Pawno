unit runoptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, IniFiles;

type

  { TRunOptionsForm }

  TRunOptionsForm = class(TForm)
    Save: TButton;
    Cancel: TButton;
    CopyTo: TEdit;
    ExecuteFile: TEdit;
    Parameters: TEdit;
    Label1: TLabel; // 'Copy script to directory:'
    Label2: TLabel; // 'Execute this file:'
    Label3: TLabel; // 'With these parameters:'
    procedure CancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SaveClick(Sender: TObject);
  private

  public

  end;

var
  RunOptionsForm: TRunOptionsForm;
  CopyToStr, ExecuteThisStr, ParametersStr: String;

implementation

{$R *.lfm}

{ TRunOptionsForm }

procedure TRunOptionsForm.SaveClick(Sender: TObject);
var
  Settings: TIniFile;
begin
  CopyToStr := CopyTo.Text;
  ExecuteThisStr := ExecuteFile.Text;
  ParametersStr := Parameters.Text;

  Settings := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'settings.ini');
  Settings.WriteString('RunOpts', 'CopyDir', CopyTo.Text);
  Settings.WriteString('RunOpts', 'ExeFile', ExecuteFile.Text);
  Settings.WriteString('RunOpts', 'Params', Parameters.Text);
  Settings.Free;

  Close;
end;

procedure TRunOptionsForm.CancelClick(Sender: TObject);
begin
  Close;
end;

procedure TRunOptionsForm.FormCreate(Sender: TObject);
begin
  CopyTo.Text := CopyToStr;
  ExecuteFile.Text := ExecuteThisStr;
  Parameters.Text := ParametersStr;
end;

end.

