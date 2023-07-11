
Unit Torzs_MAN;

Interface
  Uses
  Classes
  , SysUtils
  , globals
  ;


Const
  Prog_ID  = 'IPMLIST';
  cnvNo    = 0;
  cnvToHex = 1;


Type

T_Field_Descriptor = record     (* REKORDTÕPUS ISO MEZ’K LEÕR¡S¡RA *)
  Kod         : Byte;
  Tipus       : String[2];
  Hossz_Tipus : AnsiChar;
  Hossz       : integer;
  R_Nev       : String [80];
  H_Nev       : String[80];
  Conv        : word;
  ForceBlank  : boolean;
  DefLen      : integer;  // for ForceBlank, when variable len
  DefLenStr   : string[3];  // for ForceBlank, when variable len
  NoShow      : boolean;
  Calc_Len    : integer;
end;

T_Sub_Field_Descriptor = record     (* REKORDTÕPUS ISO MEZ’K-et AL¡BONT” *)
  Kod         : String[16];          (* ¡LTAL¡BAN EUROPAY MEZ’K LEÕR¡S¡RA*)
  Tipus       : String[2];
  Hossz       : integer;
  R_Nev       : String [12];
  H_Nev       : String[35];
end;

T_PDS_Field_Descriptor = record     (* PDS le›rﬂsok *)
  Tipus       : String[2];
  Hossz_Tipus : AnsiChar;
  Min_hossz   : integer;
  Max_Hossz   : integer;
  R_Nev       : String [80];
  H_Nev       : String[80];
  Conv        : word;
  ForceBlank  : boolean;
  DefLen      : integer;  // for ForceBlank, when variable len
  DefLenStr   : string[3];  // for ForceBlank, when variable len
  NoShow      : boolean;
  Calc_Len    : integer;
end;

  PCollection   = Class (TStringlist);


T_AlaBont = record          (* REKORDTÕPUS AZ AL¡BONTHAT” MEZ’KR’L *)
  Mezo        : String[6];  (* AL¡BONTHAT” MEZ’ AZONOSÕT”JA *)
  Rekord      : String[6];  (* '*', HA MINDEN REKORDTÕPUS ESET…N AZONOS AZ AL¡BONT¡S*)
  Plus        : String[6];  (* EGY TOV¡BBI SZ€R…SI FELT…TEL *)
  P_SRD       : TStringList;

end;


T_Record_Descriptor = Record  (* REKORDTÕPUS KLÕRING REKORDOK LEÕR¡S¡RA *)
  Kod         : String[6];
  MTID        : String[4];
  BitMapCh    : String[16];
  FUC         : String[3];    (* FUNCTION CODE, 024-es MEZ’*)
  FOC         : String[2];    (* FORMAT CODE, 072-es, vagy 123-as MEZ’ *)
  FOC_MEZ     : Byte;         (* MELYIK MEZ’ ELEJ…N VAN A FOC (072,123,... *)
  Verzio      : String[2];
  Nev         : String[50];
  DbPerFile   : integer;      (* ADOTT F¡JLBAN MENNYI VAN BEL’LE *)
End;


{!!!!}
{ T_SRD_Collection SRD: Sub_Record_Descriptor }
  P_SRD_Collection = ^T_SRD_Collection;
  T_SRD_Collection = class(TStringList);


Var
    FilvarBe : Text;
    Field_Descriptor : Array [1..128] of T_Field_Descriptor;
    Sub_Field_Descr  : Array [1..200] Of T_Sub_Field_Descriptor;
    PDS_Field_Descr  : Array [1..1100] Of T_PDS_Field_Descriptor;
    AlaBont          : Array [1..50] Of T_AlaBont;
    Rec_Descr: Array [0..80]  of T_Record_Descriptor;
    Rec_Tipus_Db : integer; (* H¡NY REKORD-TÕPUST ISMER‹NK /REKORDOK.TXT F¡JLB”L/ *)
    Sub_Field_Db,
    Sub_Rec_Db
                 : Integer;
    BITMAP1      : String[32]; (* REKORDOK.TXT-bıl VAL” BEOLVAS¡SHOZ*)
    AktSubField  : String[6];

    ASCII : PAnsiChar;(* EBCDIC K”DNAK MEGFELEL’ ASCII KARAKTEREK *)
    ASCII_A : AnsiString;(* EBCDIC K”DNAK MEGFELEL’ ASCII KARAKTEREK *)
    ASCII_E : PAnsiChar;(* EBCDIC K”DNAK MEGFELEL’ ASCII KARAKTEREK *)
    Spaces  : AnsiString;
    FS      : Int64; // FileSize
    FS_Out  : Int64; // FileSize for output file
    FilePos : Int64; // Actual File position, for progressbar
    Progress: word;

procedure EbcdicToAscii(Var Value : AnsiString);
Procedure SliceStrToStringList(str: string; SL: TStringList; separator: string = ';');
function Ertekadas : Trrc;


Implementation

//Uses IPMView_unit1;


Var
  FileNev3 : String;
  i,j : integer;
  //R,S : String;
  //PS  : PString;
  TxtPref  : String;  (* A BEOLVASANDO TXT F¡JLOK EL…R…SI ⁄TVONALA *)

procedure EbcdicToAscii(Var Value : AnsiString);
Var i : integer;
Begin
  For i := 1 to Length(Value) Do Value[i] := ASCII[Byte(Value[i])];
End;

Procedure SliceStrToStringList(str: string; SL: TStringList; separator: string);
Var i  : integer;
    //st : string;
begin
  if not Assigned(SL) then exit;

  i := Pos(separator, Str);
  while i > 0 do begin
    SL.Add(Copy(Str,1,i - 1));
    Str := Copy(Str, i + Length(separator), Length(Str));
    i := Pos(separator, Str);
  end;
  if Str <> '' then SL.Add(Str);
end;

function Ertekadas : Trrc;

(* KEZD’…RT…KEK BE¡LLÕT¡SA, T÷RZS¡LLOM¡NYOK BEOLVAS¡SA *)

var i : integer;
    st : AnsiString;
    aSL: TStringList;
BEGIN

    aSL := TStringList.Create;
    Result.rc := 0;
    Result.Note := '';
  try
    aSL.Sorted := false;
    aSL.Duplicates := dupAccept;
    i := 0;

    TxtPref := ExtractFilePath(ParamStr(0));

    try

      FileNev3 := TxtPref + Prog_ID + '.MEZ';  (* ISO MEZOK LEIR¡SA *)
      AssignFile(FilvarBe, FileNev3);
      Reset(FilvarBe);
      // Read headers
      ReadLn(FilvarBe);
      ReadLn(FilvarBe);

      While Not Eof(FilvarBe) Do
      BEGIN
        If Not SeekEoln(FilvarBe) then begin
          Read(FilvarBe,st);
          aSL.Clear;
          SliceStrToStringList(st, aSL, ';');
          if aSL.Count < 1 then begin
            // too few fields
            WriteLn('too few fields in MEZ file:' + st);
            continue;
          end;
          for i := 0 to aSL.Count-1 do aSL[i] := Trim(aSL[i]);

          i := StrToInt(Trim(aSL[0]));

          Field_Descriptor[i].R_Nev      := '';
          Field_Descriptor[i].Conv       := cnvNo;
          Field_Descriptor[i].ForceBlank := false;
          Field_Descriptor[i].DefLen     := 0;
          Field_Descriptor[i].DefLenStr  := '';
          Field_Descriptor[i].NoShow     := false;

          Field_Descriptor[i].Tipus      := ShortString(aSL[1]);
          Field_Descriptor[i].Hossz_Tipus:= AnsiChar(UpperCase(aSL[2])[1]);
          Field_Descriptor[i].Hossz      := StrToInt(aSL[3]);
          Field_Descriptor[i].H_Nev      := ShortString(aSL[4]);

          if aSL.Count >=6 then begin
            Field_Descriptor[i].R_Nev      := ShortString(aSL[5]);
          end;

          if aSL.Count >=7 then begin
            if Uppercase(aSL[6]) = 'TOHEX' then Field_Descriptor[i].Conv   := cnvToHex;
          end;
          if aSL.Count >=8 then begin
            if Uppercase(aSL[7]) = 'Y' then Field_Descriptor[i].ForceBlank := true;
          end;
          if aSL.Count >=9 then begin
            if Uppercase(aSL[8]) = 'Y' then Field_Descriptor[i].NoShow     := true;
          end;
          if aSL.Count >=10 then begin
            Field_Descriptor[i].DefLenStr := ShortString(aSL[9]);
            Field_Descriptor[i].DefLen    := StrToInt(aSL[9]);
            if length(Field_Descriptor[i].DefLenStr) < 2
              then Field_Descriptor[i].DefLenStr := '0' + Field_Descriptor[i].DefLenStr;

          end;
        end;

        if (Field_Descriptor[i].R_Nev = '') or (Field_Descriptor[i].R_Nev = '*')
          then Field_Descriptor[i].R_Nev := Field_Descriptor[i].H_Nev;

        ReadLn(FilvarBe);

      END;
      CloseFile(FilvarBe);
      //Raise Exception.Create('Test Exception msg');
    Except
      On E:Exception Do Begin
        Result.rc := 1;
        Result.Note := 'Failed to load the "' + Prog_ID + '.MEZ" file!' + CRLF + Shortstring(E.Message);
        exit;
      End;
    end;

    try
      FileNev3 := TxtPref + Prog_ID +'.REK';    i := 1; Rec_Tipus_Db := 0;
      AssignFile(FilvarBe, FileNev3);

      Reset(FilvarBe);
      While Not Eof(FilvarBe) Do
      BEGIN
        If Not SeekEoln(FilvarBe) then Read(FilvarBe,Rec_Descr[i].MTID);
        Rec_Descr[i].Kod := Rec_Descr[i].MTID;
        If Not SeekEoln(FilvarBe) then Read(FilvarBe,Rec_Descr[i].FUC);
        If Not SeekEoln(FilvarBe) then Read(FilvarBe,Rec_Descr[i].Nev);

        ReadLn(FilvarBe);

        i := i + 1;
      END;
      Rec_Tipus_Db := i - 1; {WriteLn(Rec_Tipus_Db);}
      CloseFile(FilvarBe);

  Except
    On E:Exception Do Begin
      Result.rc := 1;
      Result.Note := 'Failed to load the "' + Prog_ID + '.REK" file!' + CRLF + Shortstring(E.Message);
      exit;
    End;
  end;

  try
    FileNev3 := TxtPref + Prog_ID + '.PDS';  (* ISO MEZOK LEIR¡SA *)
    AssignFile(FilvarBe, FileNev3);
    Reset(FilvarBe);
    // Read headers
    ReadLn(FilvarBe);
    ReadLn(FilvarBe);
    While Not Eof(FilvarBe) Do
    BEGIN
      Read(FilvarBe,st);
      aSL.Clear;
      SliceStrToStringList(st, aSL, ';');
      if aSL.Count < 6 then begin
        // too few fields
        continue;
      end;
      for i := 0 to aSL.Count-1 do aSL[i] := Trim(aSL[i]);

      i := StrToInt(Trim(aSL[0]));

      PDS_Field_Descr[i].R_Nev      := '';
      PDS_Field_Descr[i].Conv       := cnvNo;
      PDS_Field_Descr[i].ForceBlank := false;
      PDS_Field_Descr[i].DefLen     := 0;
      PDS_Field_Descr[i].DefLenStr  := '';
      PDS_Field_Descr[i].NoShow     := false;

      PDS_Field_Descr[i].Tipus      := ShortString(aSL[1]);
      PDS_Field_Descr[i].Hossz_Tipus:= AnsiChar(UpperCase(aSL[2])[1]);
      PDS_Field_Descr[i].Min_Hossz  := StrToInt(aSL[3]);
      if aSL[4] <> ''
        then PDS_Field_Descr[i].Max_Hossz  := StrToInt(aSL[4])
        else PDS_Field_Descr[i].Max_Hossz  := 0;
      PDS_Field_Descr[i].H_Nev      := ShortString(aSL[5]);

      if aSL.Count >=7 then begin
        PDS_Field_Descr[i].r_Nev      := ShortString(aSL[6]);
      end;

      if aSL.Count >=8 then begin
        if Uppercase(aSL[7]) = 'TOHEX' then PDS_Field_Descr[i].Conv   := cnvToHex;
      end;
      if aSL.Count >=9 then begin
        if Uppercase(aSL[8]) = 'Y' then PDS_Field_Descr[i].ForceBlank := true;
      end;
      if aSL.Count >=10 then begin
        if Uppercase(aSL[9]) = 'Y' then PDS_Field_Descr[i].NoShow     := true;
      end;
      if aSL.Count >=11 then begin
        PDS_Field_Descr[i].DefLenStr := ShortString(aSL[10]);
        PDS_Field_Descr[i].DefLen    := StrToInt(aSL[10]);
        if length(PDS_Field_Descr[i].DefLenStr) < 2
          then PDS_Field_Descr[i].DefLenStr := '0' + PDS_Field_Descr[i].DefLenStr;

      end;
      if (PDS_Field_Descr[i].R_Nev = '') or (PDS_Field_Descr[i].R_Nev = '*')
        then PDS_Field_Descr[i].R_Nev := PDS_Field_Descr[i].H_Nev;

      ReadLn(FilvarBe);

    END;
    CloseFile(FilvarBe);

  Except
    On E:Exception Do Begin
      Result.rc := 1;
      Result.Note := 'Failed to load the "' + Prog_ID + '.PDS" file!' + CRLF + Shortstring(E.Message);
      exit;
    End;
  end;

  try
    FileNev3 := TxtPref + Prog_ID +'.ALM';  (* ISO MEZOK-et ALABONTO MEZOK LEIRASA *)
    AssignFile(FilvarBe, FileNev3); I := 1;

    Reset(FilvarBe);
    While Not Eof(FilvarBe) Do
    BEGIN
      If Not SeekEoln(FilvarBe) then Read(FilvarBe, Sub_Field_Descr[i].Kod);
      If Not SeekEoln(FilvarBe) then Read(FilvarBe, Sub_Field_Descr[i].Tipus);
      If Not SeekEoln(FilvarBe) then Read(FilvarBe, Sub_Field_Descr[i].Hossz);
      If Not SeekEoln(FilvarBe) then Read(FilvarBe, Sub_Field_Descr[i].H_Nev);
      If Not SeekEoln(FilvarBe) then Read(FilvarBe, Sub_Field_Descr[i].R_Nev)
                                else Sub_Field_Descr[i].R_Nev := Sub_Field_Descr[i].H_Nev;

      ReadLn(FilvarBe);
      Sub_Field_Descr[i].H_Nev := ShortString(TrimRight(Sub_Field_Descr[i].H_Nev));
      Sub_Field_Descr[i].R_Nev := ShortString(TrimRight(Sub_Field_Descr[i].R_Nev));
      Sub_Field_Descr[i].Kod   := ShortString(TrimRight(Sub_Field_Descr[i].Kod));

     i := i + 1;
    END;
    Sub_Field_Db := i - 1;
    CloseFile(FilvarBe);

  Except
    On E:Exception Do Begin
      Result.rc := 1;
      Result.Note := 'Failed to load the "' + Prog_ID + '.ALM" file!' + CRLF + Shortstring(E.Message);
      exit;
    End;
  end;

  try
    FileNev3 := TxtPref + Prog_ID +'.ALR';  (* MEZOK-et ALABONTO ALREKORDOK LEIRASA *)
    AssignFile(FilvarBe, FileNev3); I := 1;

    Reset(FilvarBe);
    While Not Eof(FilvarBe) Do
    BEGIN
      If Not SeekEoln(FilvarBe) then Read(FilvarBe, Alabont[i].Mezo);
      Alabont[i].Mezo := ShortString(TrimRight(Alabont[i].Mezo));
      If Not SeekEoln(FilvarBe) then Read(FilvarBe, Alabont[i].Rekord);
      Alabont[i].Rekord := ShortString(TrimRight(Alabont[i].Rekord));
      If Not SeekEoln(FilvarBe) then Read(FilvarBe, Alabont[i].Plus);
      Alabont[i].Plus := ShortString(TrimRight(Alabont[i].Plus));
      J := 1;
      Alabont[i].P_SRD := TStringList.Create;


      While Not SeekEoln(FilvarBe) Do
      Begin
        Read(FilvarBe, AktSubField);
        AktSubField := ShortString(TrimRight(AktSubField));
        Alabont[i].P_SRD.Add(AktSubField);
       { PS :=  Alabont[i].P_SRD.Strings(J - 1);
        WriteLn(i,'.Alrec, ',j,'. mezß: ', PS);}

        j := j + 1;
      End;
      ReadLn(FilvarBe);

      i := i + 1;
    END;
    Sub_Rec_Db := i - 1;
    CloseFile(FilvarBe);

  Except
    On E:Exception Do Begin
      Result.rc := 1;
      Result.Note := 'Failed to load the "' + Prog_ID + '.ALR" file!' + CRLF + Shortstring(E.Message);
      exit;
    End;
  end;


  Finally
    aSL.Free;
  end;

END;

Initialization

  For i := 1 To 128 Do
  Begin
    Field_Descriptor[i].Hossz := -1;
  End;
  For i := 1 To 1100 Do
  Begin
    PDS_Field_Descr[i].Min_hossz := -1;
  End;



  ASCII_E  := #$00#$01#$02#$03#$9C#$09#$86#$7F#$97#$8D#$8E#$0B#$0C#$0D#$0E#$0F +
              #$10#$11#$12#$13#$9D#$85#$08#$87#$18#$19#$92#$8F#$1C#$1D#$1E#$1F +
              #$80#$81#$82#$83#$84#$0A#$17#$1B#$88#$89#$8A#$8B#$8C#$05#$06#$07 +
              #$90#$91#$16#$93#$94#$95#$96#$04#$98#$99#$9A#$9B#$14#$15#$9E#$1A +
              #$20#$A0#$A1#$A2#$A3#$A4#$A5#$A6#$A7#$A8#$5B#$2E#$3C#$28#$2B#$21 +
              #$26#$A9#$AA#$AB#$AC#$AD#$AE#$AF#$B0#$B1#$5D#$24#$2A#$29#$3B#$5E +
              #$2D#$2F#$B2#$B3#$B4#$B5#$B6#$B7#$B8#$B9#$7C#$2C#$25#$5F#$3E#$3F +
              #$BA#$BB#$BC#$BD#$BE#$BF#$C0#$C1#$C2#$60#$3A#$23#$40#$27#$3D#$22 +
              #$C3#$61#$62#$63#$64#$65#$66#$67#$68#$69#$C4#$C5#$C6#$C7#$C8#$C9 +
              #$CA#$6A#$6B#$6C#$6D#$6E#$6F#$70#$71#$72#$CB#$CC#$CD#$CE#$CF#$D0 +
              #$D1#$7E#$73#$74#$75#$76#$77#$78#$79#$7A#$D2#$D3#$D4#$D5#$D6#$D7 +
              #$D8#$D9#$DA#$DB#$DC#$DD#$DE#$DF#$E0#$E1#$E2#$E3#$E4#$E5#$E6#$E7 +
              #$7B#$41#$42#$43#$44#$45#$46#$47#$48#$49#$E8#$E9#$EA#$EB#$EC#$ED +
              #$7D#$4A#$4B#$4C#$4D#$4E#$4F#$50#$51#$52#$EE#$EF#$F0#$F1#$F2#$F3 +
              #$5C#$9F#$53#$54#$55#$56#$57#$58#$59#$5A#$F4#$F5#$F6#$F7#$F8#$F9 +
              #$30#$31#$32#$33#$34#$35#$36#$37#$38#$39#$FA#$FB#$FC#$FD#$FE#$FF +
              #$0D#$0A;
  SetLength(ASCII_A,258);
  For i := 1 To 256 Do
  Begin
    ASCII_A[i] := AnsiChar(i-1);
  End;
  // 120 spaces
  Spaces := '                                                                                                                        ';
  //Ertekadas;

Finalization

  For i := 1 To Sub_Rec_Db Do Begin

    if assigned(AlaBont[i].P_SRD) then AlaBont[i].P_SRD.Free;

  End;

End.

