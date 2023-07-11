unit IPMView_unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes , SysUtils , Forms , Controls , Graphics , Dialogs , StdCtrls , LCLTYPE,
  ComCtrls , ExtCtrls , SynEdit , globals, IPM_TXT_MAN , Torzs_MAN
  ;

type

  { TIlManFrm }

  TIlManFrm = class(TForm)
    CancelBtn : TButton ;
    FindFirstBtn : TButton ;
    GenOutFileNameCb : TCheckBox ;
    ConvertBtn: TButton;
    FindStringEd : TEdit ;
    Label2 : TLabel ;
    Label3 : TLabel ;
    FoundLb : TLabel ;
    OutputFnameEd : TEdit ;
    Label1 : TLabel ;
    LogMm: TMemo;
    ProgressBar1 : TProgressBar ;
    OpenSaveDlg : TSaveDialog ;
    InpEncRg : TRadioGroup ;
    SelectInputBtn: TButton;
    InputFnameEd: TEdit;
    OpenInputDlg: TOpenDialog;
    ChangeOutputBtn : TButton ;
    SynEdit1 : TSynEdit ;
    procedure CancelBtnClick (Sender : TObject );
    procedure ChangeOutputBtnClick (Sender : TObject );
    procedure ConvertBtnClick(Sender: TObject);
    procedure FindFirstBtnClick (Sender : TObject );
    procedure FindStringEdKeyUp (Sender : TObject ; var Key : Word ;
      Shift : TShiftState );
    procedure FormCreate (Sender : TObject );
    procedure SelectInputBtnClick(Sender: TObject);
    procedure SetEnabledStartup;
    procedure SetEnabledProcessing;
  private

  public

  end;


var
  IlManFrm: TIlManFrm;
Var
  FileNev1, FileNev2 : String;
  rrc : Trrc;
  //PleaseCancel : Boolean;
  VersionStr   : string = '1.0';
  aFile : integer;

implementation

{$R *.lfm}

{ TIlManFrm }

procedure WriteMsgMemo(aMsg : string);
begin
  IlManFrm.LogMm.Lines.Add(aMsg);
end;

procedure SetProgressBar(aPos : integer);
begin
  if aPos = High(Integer) then begin
    IlManFrm.ProgressBar1.position := IlManFrm.ProgressBar1.max;
  end else begin
    IlManFrm.ProgressBar1.position := Progress;
  end;
  Application.ProcessMessages;
end;

procedure TIlManFrm.SetEnabledStartup;
begin
  self.InputFnameEd.Enabled   := true;
  self.SelectInputBtn.Enabled := true;
  self.ConvertBtn.Enabled     := true;
  self.OutputFnameEd.Enabled  := true;
  self.ChangeOutputBtn.Enabled:= true;
  self.CancelBtn.Enabled      := false;
end;

procedure TIlManFrm.SetEnabledProcessing;
begin
  self.InputFnameEd.Enabled   := false;
  self.SelectInputBtn.Enabled := false;
  self.ConvertBtn.Enabled     := false;
  self.OutputFnameEd.Enabled  := false;
  self.ChangeOutputBtn.Enabled:= false;
  self.CancelBtn.Enabled      := true;
end;

procedure TIlManFrm.SelectInputBtnClick(Sender: TObject);
begin
  if OpenInputDlg.Execute then begin
    InputFNameEd.Text := OpenInputDlg.FileName;
    if GenOutFileNameCb.Checked then begin
      st := ExtractFileName(trim(InputFNameEd.Text));
      FileNev2 := ExtractFilePath(trim(InputFNameEd.Text)) + '_' + st + '.IL';
      OutputFNameEd.Text := FileNev2;
    end;
  end;
end;

procedure TIlManFrm.ConvertBtnClick(Sender: TObject);
//Var
  //st : string;
begin
  if (trim(InputFNameEd.Text)='') or (trim(OutputFNameEd.Text)='') then begin
    MessageDlg('Please fill input/output filenames first', mtWarning, [mbOK], 0);
    exit;
  end;
  SynEdit1.Lines.Clear;
  LogMm.Lines.Clear;
  IPM_TXT_MAN.PleaseCancel := false;
  ProgressBar1.Position := 0;
  If InpEncRg.ItemIndex = 0 then NativeAscii := true else NativeAscii := False;
  FileNev1 := trim(InputFNameEd.Text);
  FileNev2 := trim(OutputFNameEd.Text);
  SetEnabledProcessing;
  rrc := IPM_TO_TXT(FileNev1, FileNev2);
  case rrc.rc of
  0 : begin
      LogMM.Lines.Add(IntToStr(RecCounter) + ' record(s) converted successfully. ');

      aFile  := FileOpen(FileNev2,fmOpenRead);
      If aFile < 0 Then Begin
        MessageDlg('Failed to get FileSize of Output File', mtError, [mbOK], 0);
      End else begin
        FS_Out := FileSeek(aFile,int64(0),2);
        FileClose(aFile);
        if FS_Out <= 200*1024*1024 then begin
          SynEdit1.Lines.LoadFromFile(FileNev2,TEncoding.ANSI);
        end else begin
            SynEdit1.Lines.Add('Output file is too large (>200MByte), it is not loaded here');
            SynEdit1.Lines.Add('Please use an external editor (Notepad++, Far Manager, ...) to see it');
        end;
      end;
  end;
  2 : begin  // user cancellation
      SynEdit1.Lines.Add(rrc.Note);
      ProgressBar1.Position := ProgressBar1.Min;
  end;
  else begin
      LogMM.Lines.Add('Conversion Failed');
      MessageDlg('Conversion Failed', mtError, [mbOK], 0);
  end;
  end;

  SetEnabledStartup;
end;

procedure TIlManFrm .FindFirstBtnClick (Sender : TObject );
Var
  i : LongWord;
  st : string;
  //Found : boolean;
  //aPoint : TPoint;
begin
  st := AnsiUpperCase(FindStringEd.Text);
  if st = '' then exit;
  FoundLb.Visible := false;

  i := SynEdit1.SearchReplace(st,'',[]);

  if i > 0 then begin
    FoundLb.Visible := false;
    SynEdit1.SetFocus;
  end else begin
    FoundLb.Visible := true;
  end;
  exit;
  {
  Line := 0; Found := false;
  for i := 0 to Memo1.Lines.Count - 1 do begin
    j := Pos(st, AnsiUpperCase(Memo1.Lines[i]));
    if j > 0 then begin
      Found := true;
      Line := i;
      break;
    end;
  end;
  if Found then begin
    //aPoint := TPoint.Create(j, Line);
    //Memo1.ScrollBy(0,Line);
    Memo1.VertScrollBar.Position := Line;
    //Memo1.CaretPos := TPoint.Create(j, Line);
    //Memo1.SelStart := j;
    //Memo1.SelLength := length(st);
    //Memo1.HideSelection := false;
    Memo1.SetFocus;

  end;
  }
end;

procedure TIlManFrm .FindStringEdKeyUp (Sender : TObject ; var Key : Word ;
  Shift : TShiftState );
begin
  if Key = VK_RETURN then begin
    FindFirstBtnClick(Sender);
  end;
end;

procedure TIlManFrm .FormCreate (Sender : TObject );
Var
  rrc : Trrc;
begin
  Caption := 'IPM File Viewer  ' + VersionStr;
  SetEnabledStartup;
  rrc := Ertekadas;
  if rrc.rc <> 0 then begin
    MessageDlg('Error during ''IPM File Viewer'' startup' + CRLF + rrc.Note + CRLF + 'Program is terminating', mtError, [mbOK], 0);
    Application.Terminate;
  end;
  IPM_TXT_MAN.WriteMsgProc    := @WriteMsgMemo;
  IPM_TXT_MAN.SetProgressProc := @SetProgressBar;
  Width  := integer(Trunc(0.8*Screen.Width));
  Height := integer(Trunc(0.8*Screen.Height));
end;

procedure TIlManFrm.CancelBtnClick (Sender : TObject );
begin
  IPM_TXT_MAN.PleaseCancel := true;
end;

procedure TIlManFrm.ChangeOutputBtnClick (Sender : TObject );
begin
  if OpenSaveDlg.Execute then begin
    OutputFNameEd.Text := OpenSaveDlg.FileName;
  end;
end;

end.

