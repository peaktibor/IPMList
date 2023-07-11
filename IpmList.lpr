program IpmList;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes
  ,SysUtils
  ,IPM_TXT_MAN
  ,Torzs_MAN
  ,globals

  { you can add units after this };

Const
  Prog_ID = 'IPMLIST';

Var
  FileNev1, FileNev2 : String;
  rrc : Trrc;

procedure SetProgressCmd(aPos : integer);
Var
  st : string;
begin
  if aPos = High(integer) then aPos := 100;
  if (aPos < 0) or (aPos > 100) then exit;
  st := formatFloat('000',aPos) + '%';
  write(st + #13 );
end;

procedure WriteMsgStdOut(aMsg : string);
begin
  WriteLn(aMsg);
end;


begin
  try
    { TODO -oUser -cConsole Main : Insert code here }

  rrc := Ertekadas;
  if rrc.rc <> 0 then begin
    WriteLn('Error during ''IPM File Viewer'' startup' + CRLF + rrc.Note + CRLF + 'Program is terminating');
    Halt(1);
  end;

  IPM_TXT_MAN.SetProgressProc := addr(SetProgressCmd);
  IPM_TXT_MAN.WriteMsgProc    := addr(WriteMsgStdOut);

  NativeAscii := False;
  Case ParamCount of
    0 :
      Begin
        WriteLn('Hasznalat: ',Prog_ID,'.EXE <input fajl> <outputfajl|*> <ASCII=Y/N>');
        halt;
      End;
    1 :
      Begin
        FileNev1 := ParamStr(1);
        FileNev2 := '_'+FileNev1+'.IL';
        //FileNev2[Length(FileNev2)] := '~';
      End;
   2.. 3 :
      Begin
        FileNev1 := ParamStr(1);
        If ParamStr(2) = '*' Then
        begin
          FileNev2 := '_'+FileNev1+'.IL';
          //FileNev2[Length(FileNev2)] := '~';
        end
        Else FileNev2 := ParamStr(2);
        If ParamCount = 3 Then
        Begin
          If (ParamStr(3) = 'Y') or (ParamStr(3) = 'y') Then NativeAscii := True;
        End;
      End;
    Else
      Begin
        WriteLn('Hasznalat: ',Prog_ID,'.EXE <input fajl> <outputfajl|*> <ASCII=Y/N>');
        halt;
      End;
  End;

  rrc := IPM_TO_TXT(FileNev1, FileNev2);
  case rrc.rc of
  0 : begin
      WriteLn;
      WriteLn(IntToStr(RecCounter) + ' record(s) converted successfully. ');
  end;
  2 : begin  // user cancellation
      WriteLn(rrc.Note);
  end;
  else begin
      WriteLn('Conversion Failed: ' + rrc.Note);
  end;
  end;


  except
    on E: Exception do begin
      Writeln(E.ClassName, ': ', E.Message);

      WriteLn(FilvarKi,Copy(Sor_C[2],1,Pos_Header));
      WriteLn(FilvarKi,Copy(Sor_C[1],1,Pos_Header));
      WriteLn(FilvarKi,Copy(Sor_B,1,Pos_Header));

      CloseFile(Filvarki);
    end;
  end;
end.

