Unit IPM_TXT_MAN;

Interface

Uses globals, Torzs_MAN;

Const max_header_len = 3200;

Type
T_Act_Rec_Tart = Record  (* AZ AKTUÁLIS REKORD TARTALMA/SZERKEZETE *)
  BitMap      : array[1..16] of Byte;
  Mezo_Db     : byte;
  Mezok       : array[1..128] of Byte;
  ForceBlank  : array[1..128] of boolean;
End;

Var   NativeAscii : Boolean;
    WriteMsgProc    : TWriteMsg = nil;
    SetProgressProc : TSetProgress = nil;
    PleaseCancel    : Boolean = false;


Function IPM_TO_TXT(FileNev1, FileNev2 : String):Trrc;
Procedure MezoFormazas(Szelesseg : integer; Hatarolo : Boolean;
                       Cnv: word=cnvNo; ForceBlank: boolean=false; NoShow: boolean=false);
Procedure FejRovatFormazas(Szelesseg : integer; FejRovat : AnsiString;
                           Sor : integer; Hatarolo : AnsiChar; NoShow: boolean=false);
//procedure WriteMsg(aMsg : string);

Var
    FilvarKi: Text;
    Sor_A, (* BEOLVASOTT KLÍRING REKORD KARAKTERENKÉNT *)
    Sor_B  (* SOR_A-ból ÉPÍTETT FORMÁZOTT ADATSOR *)
           : Array[1..max_header_len] of AnsiChar;
    Sor_C  (* SOR_B SZÁMÁRA FORMÁZOTT FEJROVAT *)
           : Array[1..2] Of Array[1..max_header_len] of AnsiChar;
    Sor_star : Array[1..max_header_len] of AnsiChar;
    Prev_SorC_1, Prev_SorC_2 : AnsiString;

    AlaBont_Szint    : Integer;
    Act_Rec_Tart     : T_Act_Rec_Tart;
    Al_Rec_Id    : Integer; (* HANYADIK ALREKORDRÓL VAN SZÓ *)
//    i,j,k,k1,l,m,m1,n,Hossz : integer;
    i,k,k1,n,Hossz : integer;
    ii,jj : Longint;
    //ib, jb : Longint;
    s,r,st :AnsiString;
    Char_Db, (* AZ ÉPPEN BEOLVASOTT KLÍRING REKORD KARATERMENNYISÉGE  *)
    Pos_Data_Raw,  (* POINTER A NYERS ADASORHOZ *)
    Pos_Data_Form, (* POINTER A FORMÁZOTT ADASORHOZ *)
    Pos_Header     (* POINTER A FORMÁZOTT FEJROVATHOZ *)
              : integer;
    //kodstring : string[3];
    RecCounter   : LongWord;

Implementation


Uses SysUtils,Math;

Type

T_Stat_1 = Record  (* STATISZTIKA A PRESENTMENTEK TARTALMAROL*)
      TxType      : String[20];
      RevStat     : AnsiChar;
      SumTxDb     : Longint;
      SumTxVol    : Longint;
      SumTxFee    : Longint;
End;

Var {FilvarBe,} //FilvarKi: Text;
    FejRovat : AnsiString;
    MTID  : String[4];   (* AZ AKTUÁLISAN          *)
    BITMAP: String[16];  (* BEOLVASOTT             *)
    FUC : String[3];     (* REKORD                 *)
    FOC : String[2];     (* Fõ JELLEMZÕI,          *)
    FOC_MEZ : integer;   (* AMIT A BEAZONOSÍTÁSHOZ *)
    Proc_Class:String[4];
    Verzio : String[2];  (* HASZNÁLOK              *)
    Akt_rec, Elozo_Rec   (* AZ AKTUÁLIS ÉS AZ ELÕZÕ REKORD SORSZÁMA*)
                         (* A REC_DESCR TÖMBBEN*)
                       : integer;
    Rec_Id             : String[6];
    Ismert  : Boolean; (* Ismert-e /sikerül-e azonosítani/ a beolvasott rekord*)
    Pr1240Db: integer; (* HÁNY DB 1240-ES ÜZENET VAN *)
    UnKnown : integer; (* HÁNY DARAB ISMERETLEN VAN *)
    Stat_1  : Array [1..5,1..2] of T_Stat_1;
    TxVol   : Longint;
    //TxFee   : Longint;
    TxVolSt : String[12];
    //TxFeeSt : String[10];
    TxType  : String[2];
    //RevStat : AnsiChar;
    act_field_len : integer;

    RDW_Str : AnsiString;
    RDW     : integer;
    Act_Pos_1014 : integer;
    RestLen      : integer;
    block_1014   : boolean;
    Act_Line     : AnsiString;

Var ReadLen,DataLen,startindex : integer;
    InFile : integer;

    Buff_1014       : array[1..4056] of AnsiChar;
    BuffContentLen  : integer;
    rc           : integer;
    Block1014Cnt : integer;
    BytesReadSum : Int64;
    RDW_Sum      : Int64;
    InFilePos    : Int64;
    Act_Pos_1012 : integer;
    Prev_Field   : integer;
    ActBit       : Byte;

procedure WriteMsg(aMsg : string);
begin
  if Assigned(WriteMsgProc) then begin
    WriteMsgProc(aMsg);
  end;
end;

procedure SetProgress(aPos : integer);
begin
  if Assigned(SetProgressProc) then begin
    SetProgressProc(aPos);
  end;
end;


Function B_Power(i:Byte; j: Byte):Byte; (* I J-edik HATVÁNYÁT ADJA*)
var k,l : integer;                      (* CSAK BYTE TÍPUSOKRA!!  *)
Begin
  If j = 0 Then B_Power := 1 Else
  If j = 1 Then B_Power := i Else
  Begin
    l := i;
    For k := 1 To j - 1 Do l := l * i;
    B_Power := l;
  End;
End;

Function Rec_Find_2(MTID, BitMap, FUC, FOC,Verzio : AnsiString) : integer;

(* AZ INPUT ALAPJÁN AZ ELSÕ /ÉS JÓ ESETBEN EGYETLEN/ MEGFELELÕT ADJA*)

Var i : integer;
Begin
  Rec_Find_2 := 0;
  For i := 1 To Rec_Tipus_db Do
  Begin
{
    If (Rec_Descr[i].MTID = MTID) and
       (Rec_Descr[i].BitMapCh = BitMap) and
       ((Rec_Descr[i].FUC = FUC) or (Rec_Descr[i].FUC = 'X  ')) and
       ((Rec_Descr[i].FOC = FOC) or (Rec_Descr[i].FOC = 'X '))  and
       (Rec_Descr[i].Verzio = Verzio) Then
}
    If (Rec_Descr[i].MTID = MTID) and
       (Rec_Descr[i].FUC = FUC) Then
    Begin
      Rec_Find_2 := i;
      Exit;
    End;
  End;
End;

Function Field_Find(Mezo: AnsiString) : integer;

(* AZ INPUT ALAPJÁN AZ ELSÕ /ÉS JÓ ESETBEN EGYETLEN/ MEGFELELÕT ADJA*)

Var i : integer;
Begin
  Field_Find := 0;
  For i := 1 To Sub_Field_Db Do
  Begin
    If Sub_Field_Descr[i].Kod = Mezo Then
    Begin
      Field_Find := i;
      Exit;
    End;
  End;
End;


Function AlaBonthato(Mezo,Rec_Id, Plus : AnsiString) : boolean;

(* ALÁBONTHATÓ-E AZ ADOTT MEZÕ? *)

Var i : integer;

Function Conform(Mi, Mivel : AnsiString) : Boolean;
Var i : integer;
Begin
  Conform := True;
  If Length(Mi) < Length(Mivel) Then
  Begin
    Conform := False;
    Exit;
  End;
  For i := 1 To Length(Mivel) Do
  Begin
    If Mi[i] <> Mivel[i] Then
    Begin
      Conform := False;
      Exit;
    End;
  End;
End;


Begin
  AlaBonthato := false;
  For i := 1 To Sub_Rec_Db Do
  Begin
    If (AlaBont[i].Mezo = Mezo) and
       ((Conform(Rec_Id, AlaBont[i].Rekord)) or (AlaBont[i].Rekord = '*')) and
       ((AlaBont[i].Plus = Plus) or (AlaBont[i].Plus = '*')) Then
    Begin
      AlaBonthato := True;
      Al_Rec_Id   := i;
{      WriteLn(Mezo,'Alabonthato');}
      Exit;
    End;
{    WriteLn(Mezo,'Nem bonthato');   }

  End;
End;

Procedure FejRovatFormazas(Szelesseg : integer; FejRovat : AnsiString;
                           Sor : integer; Hatarolo : AnsiChar; NoShow: boolean);
Var {m,} m1, m2 : integer;
Begin
  if NoShow then begin
    exit;
  end;
  If Hatarolo <> '-' Then Begin
    If Hatarolo <> ' ' Then Sor_C[Sor][Pos_Header + 1] := Hatarolo;
    m2 := 1;
  End Else m2 := 0;
  m1 := Min(Szelesseg, Length(FejRovat));
  Move(FejRovat[1],Sor_C[Sor][Pos_Header + m2 + 1],m1);
  Pos_Header := Pos_Header + Szelesseg + m2;
  // Header may be wider then data
  if Sor = 1 then Pos_Data_Form := Pos_Header;
End;

//Procedure MezoFormazas(Szelesseg : integer; Hatarolo : Boolean);
Procedure MezoFormazas(Szelesseg : integer; Hatarolo : Boolean; Cnv: word; ForceBlank: boolean; NoShow: boolean);

(* A PARAMÉTERKÉNT MEGADOTT SZÉLESSÉGBEN ÁTMÁSOL SOR_A-ból SOR_B-be,
   'NoShow' not used currently
*)

Var {m,}  m2 : integer;
Begin
  if ForceBlank then begin
    // no data, corresponding 'FejRovatFormazas' must be called
    If Hatarolo Then Begin
      Sor_B[Pos_Data_Form + 1] := '|';
      m2 := 1;
    End Else m2 := 0;
    Move(Sor_star[1],Sor_B[Pos_Data_Form + m2 + 1], Szelesseg);
    Pos_Data_Form := Pos_Data_Form + Szelesseg + m2;
    exit;
  end;

  If Hatarolo Then Begin
    Sor_B[Pos_Data_Form + 1] := '|';
    m2 := 1;
  End Else m2 := 0;
  Move(Sor_A[Pos_Data_Raw + 1],Sor_B[Pos_Data_Form + m2 + 1], Szelesseg);
  Pos_Data_Raw  := Pos_Data_Raw + Szelesseg;
  Pos_Data_Form := Pos_Data_Form + Szelesseg + m2;
End;

Function MezoFormazas3(Al_Rec_Id_ : integer) : Trrc;
Var i, j,m,n : integer;
    hossz: integer;
    s    : AnsiString;
    PS   : AnsiString;
    act_poz,end_poz : integer;
    act_PDS : AnsiString;
    act_PDS_len : integer;
    act_PDS_name: AnsiString;
    AlaBonthatoe : Boolean;
    PdsId        : integer;

Begin
  Result.rc := 1;
  Result.Note := '';
  AlaBont_Szint := 2;
{
  Sor_B[Char_Db_B + m2 + m] := Sor_A[Char_Db_A + m];
  Char_Db_A := Char_Db_A + Szelesseg;
}
  act_poz := Pos_Data_Raw + 1;
  end_poz := act_poz + act_field_len;
  while act_poz < end_poz Do
//  For i := 1 To Alabont[Al_Rec_Id].P_SRD.Count  Do

  Begin
    act_PDS     := Copy(Sor_A,act_poz,4);
    PdsId       := StrToInt(act_PDS);
    act_PDS_len := StrToInt(Copy(Sor_A,act_poz+4,3));
    act_PDS_name:= '';

    if (PDS_Field_Descr[PdsId].Min_hossz <> -1) and PDS_Field_Descr[PdsId].NoShow then begin
      // skip this PDS
      act_poz := act_poz + act_pds_len + 7;
      Pos_Data_Raw  := Pos_Data_Raw + act_pds_len + 7;
      continue;
    end;

    m := Pos_Header;

    // Format the constans beginning of the PDS
    MezoFormazas(4, True);
    FejRovatFormazas(4, 'PDS ', 1, '|');
    MezoFormazas(3, True);
    FejRovatFormazas(3, 'Len', 1, '|');

    If Length(PDS_Field_Descr[PdsId].H_Nev) <= max(act_PDS_len,4) + 9
      Then act_PDS_name :=  PDS_Field_Descr[PdsId].H_Nev
      Else act_PDS_name :=  PDS_Field_Descr[PdsId].R_Nev;

    AlaBonthatoe := AlaBonthato(act_PDS, Rec_Id, Proc_Class);
    If AlaBonthatoe Then
    Begin
      // Format its subfields
      For i := 1 To Alabont[Al_Rec_Id].P_SRD.Count  Do
      Begin

        PS := AnsiString(act_PDS + '_' + Alabont[Al_Rec_Id].P_SRD.Strings[i - 1]);
        j := Field_Find(PS);

        If j = 0 Then
        Begin
          PS := AnsiString(Alabont[Al_Rec_Id].P_SRD.Strings[i - 1]);
          Result.Note := 'Cannot find subfield: ' + PS;
          WriteMsg(Result.Note);
          Exit;
        End;
        MezoFormazas(Sub_Field_Descr[j].Hossz, True);
        If Length(Sub_Field_Descr[j].H_Nev) <= Sub_Field_Descr[j].Hossz
        Then S := Sub_Field_Descr[j].H_Nev
        Else S := Sub_Field_Descr[j].R_Nev;
        If Length(S) > Sub_Field_Descr[j].Hossz
        Then Hossz := Length(S)
        Else Hossz := Sub_Field_Descr[j].Hossz;

        FejRovatFormazas(Hossz, S, 1, '|');


      End;
    End Else
    Begin

      MezoFormazas(act_PDS_len, True);
      i := max(act_PDS_len,4);
      if (PDS_Field_Descr[PdsId].Min_hossz <> -1)
        then i := max(i, PDS_Field_Descr[PdsId].Min_hossz);
      FejRovatFormazas(i, 'Data', 1, '|');

    End;

    n := Pos_Header - m - 1;
    Pos_Header := m;

    If act_PDS_name = ''
      Then FejRovatFormazas(n, Copy('PDS ' + act_PDS ,1,n), 2, '|')
      Else FejRovatFormazas(n, Copy(act_PDS_name,1,n), 2, '|');
    FejRovatFormazas(2, ' ', 2, '|');
    Pos_Header := Pos_Header - 3;

    act_poz := act_poz + act_pds_len + 7;
  End;
  Result.rc := 0;
End;

Function MezoFormazas2(Al_Rec_Id : integer) : Trrc;
Var i, j : integer;
    hossz: integer;
    s    : AnsiString;
    PS   : AnsiString;


Begin
  Result.rc := 1;
  Result.Note := '';
{  WriteLn(Al_Rec_Id); ReadLn;}
  AlaBont_Szint := 2;
 {For i := 1 To Sub_Rec_Descr[Al_Rec_Id].Mezo_Db Do}
  For i := 1 To Alabont[Al_Rec_Id].P_SRD.Count  Do


  Begin
   {j := Field_Find(Sub_Rec_Descr[Al_Rec_Id].Mezok[i]);}
    PS := AnsiString(Alabont[Al_Rec_Id].P_SRD.Strings[i - 1]);
    j := Field_Find(PS);


    If j = 0 Then
    Begin
     {WriteMsg('Nem találom a következõ almezõt: ',Sub_Rec_Descr[Al_Rec_Id].Mezok[i]:10);}
      PS := AnsiString(Alabont[Al_Rec_Id].P_SRD.Strings[i - 1]);
      Result.Note := 'Cannot find subfield: ' + PS;
      WriteMsg(Result.Note);
      Exit;
    End;
    MezoFormazas(Sub_Field_Descr[j].Hossz, True);
    If Length(Sub_Field_Descr[j].H_Nev) <= Sub_Field_Descr[j].Hossz
    Then S := Sub_Field_Descr[j].H_Nev
    Else S := Sub_Field_Descr[j].R_Nev;
    If Length(S) > Sub_Field_Descr[j].Hossz
    Then Hossz := Length(S)
    Else Hossz := Sub_Field_Descr[j].Hossz;

    FejRovatFormazas(Hossz, S, 1, '|');

  End;
  Result.rc := 0;
End;

Function Azonositok : Trrc; (* AZONOSÍTÓK BEOLVASÁSA A REKORDBÓL *)
                      (* ELÕTTE A FOC_MEZ-t KI KELL DERÍTENI (REC_FIND_1)*)
Var i,j,m,n,x : integer;
    loc_Pos_Data_Raw : integer;
    st : AnsiString;
Begin
  Result.rc := 1;
  Result.Note := '';
    loc_Pos_Data_Raw := 20 ;
    For i := 2 To Act_Rec_Tart.Mezo_db Do
    Begin

      if Act_Rec_Tart.ForceBlank[i] then continue;

      j := Act_Rec_Tart.Mezok[i];
      k1 := Field_Descriptor[j].Hossz;
      If k1 = -1 Then
      Begin
        Result.Note := 'Unknown field, specified by ' + IntToStr(i) + '. bit of BITMAP: ' + IntToStr(j);
        WriteMsg(Result.Note);
        Exit;
      End;
      If Field_Descriptor[j].Hossz_Tipus = 'F'
      Then k := k1
      Else
      Begin
        For m := loc_Pos_Data_Raw + 1 To loc_Pos_Data_Raw + 1 + k1 - 1 Do Sor_A[m] := ASCII[Ord(Sor_A[m])];
        st := Copy(PAnsiChar(addr(Sor_A[loc_Pos_Data_Raw + 1])),1,k1);
        {
        s := ''; n := 1; r := '';
        For m := loc_Pos_Data_Raw + 1 To loc_Pos_Data_Raw + 1 + k1 - 1 Do
        Begin
          s := s + ' ';  r := r + 'L';
          Sor_A[m] := ASCII[Ord(Sor_A[m])];
          s[n] := Sor_A[m];
          n := n + 1;
        End;
        }
        val(st, k, n);
        If n > 0 Then Begin
          Result.Note := 'Invalid field length: ' + st;
          WriteMsg(Result.Note);
          Exit;
        End;
        loc_Pos_Data_Raw := loc_Pos_Data_Raw + k1;
      End;
      Field_Descriptor[j].Calc_Len := k;

      If j <> 55 Then For x := loc_Pos_Data_Raw + 1 To loc_Pos_Data_Raw  + k Do Sor_A[x] := ASCII[Ord(Sor_A[x])];

      If j = 24 Then FUC := Sor_A[loc_Pos_Data_Raw + 1] + Sor_A[loc_Pos_Data_Raw + 2] + Sor_A[loc_Pos_Data_Raw + 3];
      If j in [72,123] {FOC_MEZ} Then
      Begin
        FOC := Sor_A[loc_Pos_Data_Raw + 1] + Sor_A[loc_Pos_Data_Raw + 2];
        Verzio := Sor_A[loc_Pos_Data_Raw + 3] + Sor_A[loc_Pos_Data_Raw + 4];
      End;
      If j = 104 Then
      Begin
        Proc_Class := Sor_A[loc_Pos_Data_Raw + 1] + Sor_A[loc_Pos_Data_Raw + 2] +
                      Sor_A[loc_Pos_Data_Raw + 3] + Sor_A[loc_Pos_Data_Raw + 4];
      End;
      If j = 5 Then
      Begin
        TxVolSt := '';
        For n := 1 To 12 Do
          TxVolSt := TxVolSt + Sor_A[loc_Pos_Data_Raw  + n];
        Val(TxVolSt,TxVol,m);
      End;
      If j = 123 Then
      Begin
        TxType := Sor_A[loc_Pos_Data_Raw + 9] + Sor_A[loc_Pos_Data_Raw + 10];
      End;

      loc_Pos_Data_Raw := loc_Pos_Data_Raw + k;
    End;
  Result.rc := 0;
End;
{
%MACRO READ_1014_BYTES(DEST,COUNT);

IF (ACT_POS_1014 EQ 1013) THEN
DO;
  II = 2;
  INPUT &DEST $VARYING4. II  @;
  ACT_POS_1014 = 1;
END;

RESTLEN = 1013-ACT_POS_1014;

IF RESTLEN < &COUNT THEN
DO;
  II = &COUNT + 2;
  INPUT &DEST $VARYING4002. II  @;
  &DEST = SUBSTR(&DEST,1,RESTLEN)||SUBSTR(&DEST,RESTLEN+3,&COUNT - RESTLEN);
  ACT_POS_1014 = &COUNT - RESTLEN + 1;

END; ELSE
DO;
  II = &COUNT;
  INPUT &DEST $VARYING4002. II  @;
  ACT_POS_1014 + &COUNT;
END;

%MEND;

}

function FillBuff : integer;
Var
  ReadCount   : integer;
begin
  Result := 0;
  while (Length(Buff_1014) - BuffContentLen) >= 1014 do begin
    ReadCount := FileRead(InFile,Buff_1014[BuffContentLen+1],1014);
    if ReadCount <= 0 then begin
      // Eof or error
      break;
    end;
    BytesReadSum := BytesReadSum + ReadCount;
    if ReadCount = 1014 then begin
      BuffContentLen := BuffContentLen + 1012;
      Inc(Block1014Cnt);
    end else begin
      // this is an error condition !!!
      BuffContentLen := ReadCount;
    end;
  end;
end;

Function Read_1014_Bytes(Var Dest:AnsiString; Count:integer):integer;
Begin
    Result := 0;
    FillBuff;
    if BuffContentLen < Count then begin
      Result := 1;
      exit;
    end;
    SetLength(Dest, Count);
    Move(Buff_1014[1],Dest[1],COUNT);
    BuffContentLen := BuffContentLen - Count;
    InFilePos      := InFilePos + Count;
    Move(Buff_1014[count+1],Buff_1014[1],BuffContentLen);
End;

Function Read_1014_Bytes_old(Var Dest:AnsiString; Count:integer):integer;
Var j : integer;
Begin
  Result := 0;
  IF (ACT_POS_1014 = 1013) THEN
  Begin
    //INPUT &DEST $VARYING4. II  @;
    j  := FileRead(InFile,Dest[1],2);
    if j <> 2 Then Begin Result := 1; exit; End;
    //SetLength(Dest,j);
    ACT_POS_1014 := 1;
  END;

  RESTLEN := 1013-ACT_POS_1014;

  IF RESTLEN < COUNT THEN
  Begin
    //II := COUNT + 2;
    //INPUT &DEST $VARYING4002. II  @;
    SetLength(Dest,Count+2);
    j  := FileRead(InFile,Dest[1],Count+2);
    if j <> Count+2 Then Begin Result := 1; exit; End;
    //DEST := Copy(DEST,1,RESTLEN) + Copy(DEST,RESTLEN+3,COUNT - RESTLEN);
    Move(Dest[RestLen+3],Dest[RestLen+1],COUNT - RESTLEN);
    ACT_POS_1014 := COUNT - RESTLEN + 1;

  END ELSE
  Begin
    //INPUT &DEST $VARYING4002. II  @;
    SetLength(Dest,Count);
    j  := FileRead(InFile,Dest[1],Count);
    if j <> Count Then Begin Result := 1; exit; End;
    ACT_POS_1014 := ACT_POS_1014 + COUNT;
  END;
End;

Function IPM_TO_TXT(FileNev1, FileNev2 : String) : Trrc;
//Label nonewline;
Var i,J,M : integer;
    Alabonthatoe : boolean;
    rrc : Trrc;
Begin
 Try
  Result.rc := 1;
  Result.Note := '';
  BuffContentLen := 0;
  BytesReadSum   := 0;
  InFilePos      := 0;
  Block1014Cnt   := 0;
  RDW_Sum        := 0;
  RecCounter     := 0;
  If NativeAscii Then ASCII := addr(ASCII_A[1])
                 Else ASCII := ASCII_E;

  AssignFile(Filvarki, FileNev2);
  Rewrite (FilVarki);
  //WriteMsg('Input File: ' + FileNev1);
  InFile  := FileOpen(FileNev1,fmOpenRead);
  If InFile < 0 Then
  Begin
    Result.Note := 'FileOpen failed : ' + FileNev1;
    WriteMsg(Result.Note);
    Raise Exception.Create(Result.Note);
  End;
  //AssignFile(FilvarBe, FileNev1);
  //Reset(FilvarBe);



  Elozo_Rec := 0; Akt_Rec := -1;
  Prev_SorC_1 := '';
  Prev_SorC_2 := '';
  Pos_Header := max_header_len;

  Stat_1[1,1].TxType := 'Retail';
  Stat_1[1,1].RevStat:= 'N';
  Stat_1[1,2].TxType := 'Retail';
  Stat_1[1,2].RevStat:= 'R';
  Stat_1[2,1].TxType := 'Man. Cash Advance';
  Stat_1[2,1].RevStat:= 'N';
  Stat_1[2,2].TxType := 'Man. Cash Advance';
  Stat_1[2,2].RevStat:= 'R';
  Stat_1[3,1].TxType := 'ATM';
  Stat_1[3,1].RevStat:= 'N';
  Stat_1[3,2].TxType := 'ATM';
  Stat_1[3,2].RevStat:= 'R';
  Stat_1[4,1].TxType := 'Unique';
  Stat_1[4,1].RevStat:= 'N';
  Stat_1[4,2].TxType := 'Unique';
  Stat_1[4,2].RevStat:= 'R';
  Stat_1[5,1].TxType := 'Refund/Credit';
  Stat_1[5,1].RevStat:= 'N';
  Stat_1[5,2].TxType := 'Refund/Credit';
  Stat_1[5,2].RevStat:= 'R';

  For I := 1 To 5 Do For J := 1 To 2 Do
  Begin
    Stat_1[i,j].SumTxDb := 0;
    Stat_1[i,j].SumTxVol := 0;
    Stat_1[i,j].SumTxFee := 0;
  End;

  Act_Pos_1014 := 1;
  RestLen      := 1012;
  SetLength(Act_Line,max_header_len+2);
  SetLength(RDW_Str,6);
  ReadLen  := FileRead(InFile,RDW_Str[1],4);
  If ReadLen <> 4 Then
  Begin
    Result.Note := 'Failed to read input file';
    WriteMsg(Result.Note);
    Raise Exception.Create(Result.Note);
  End;
  If RDW_Str[1] = #0 Then block_1014 := True Else block_1014 := False;

  // Get File size;
  FS := FileSeek(InFile,int64(0),2);
  // Reset infile position
  FileSeek(InFile,0,0);


  If block_1014 Then
  Begin
    WriteMsg('Layout: RDW with 1014 blocking');
    rc := Read_1014_Bytes(RDW_Str,4);
    If rc <> 0 Then Begin
      Result.Note := 'Failed to read input file';
      WriteMsg(Result.Note);
      Raise Exception.Create(Result.Note);
    End;
    RDW := Byte(RDW_Str[3])*256 + Byte(RDW_Str[4]);
    ACT_POS_1014 := 5;
    rc := Read_1014_Bytes(Act_Line,RDW);
    If rc <> 0 Then Begin
      Result.Note := 'Failed to read input file';
      WriteMsg(Result.Note);
      Raise Exception.Create(Result.Note);
    End;
    Move(Act_Line[1],Sor_A[1],RDW);
    DataLen := RDW;
    startindex := 1;
    RDW_Sum := RDW_Sum + RDW + 4;
  End Else
  Begin
    WriteMsg('Layout: Without RDW');
    If (not NativeAscii) and (RDW_Str[1] = '1') Then
    Begin
      WriteMsg('It seems ASCII...');
      NativeAscii := True;
      ASCII := addr(ASCII_A[1]);
    End;

    //move(RDW_Str[1],Sor_A[1],4);
    //startindex := 5;
    //ReadLen  := FileRead(InFile,Sor_A[startindex],max_header_len-4);
    //DataLen := ReadLen+4;
    startindex := 1;
    ReadLen  := FileRead(InFile,Sor_A[startindex],max_header_len-4);
    DataLen := ReadLen;
  End;

  //While Not Eof(FilvarBe) Do
  While DataLen >= 20 Do

  (* KEZDEM OLVASNI EGYENKÉNT AZ INPUT FÁJL REKORDJAIT *)

  BEGIN
    FUC := ''; FOC :=''; FOC_MEZ := 0; Verzio :='';

    //FillChar(Sor_A[1],Char_Db_B,' ');
    FillChar(Sor_C[1][1],Pos_Header,' ');
    FillChar(Sor_C[2][1],Pos_Header,' ');
    FillChar(Sor_B[1],Pos_Header,'_');

    Char_Db := DataLen;

    (* ITT BONTOM KI A BITMAP-et: *)

    For i := 1 To 16 Do
    Begin
      Act_Rec_Tart.Bitmap[i] := Ord(Sor_A[i + 4]);
    End;
    for i := 1 to 128 do Act_Rec_Tart.ForceBlank[i]  := false;
    k := 0;
    For i := 1 To 16 Do For j := 1 To 8 Do
    Begin
      ActBit := (i - 1) * 8 + j;
      If Act_Rec_Tart.Bitmap[i] and
         B_Power(2,(8 - j)) = B_Power(2,(8 - j)) Then
      Begin
        k := k + 1;
        //Act_Rec_Tart.Mezok[k] := (i - 1) * 8 + j;
        Act_Rec_Tart.Mezok[k]      := ActBit;
        Act_Rec_Tart.ForceBlank[k] := false;
      End else begin
        if (Field_Descriptor[ActBit].Hossz <> -1) and (Field_Descriptor[ActBit].ForceBlank) then begin
          k := k + 1;
          Act_Rec_Tart.Mezok[k]      := ActBit;
          Act_Rec_Tart.ForceBlank[k] := true;
        end;
      end;
    End;
    Act_Rec_Tart.Mezo_Db := k;
{
    S := Sor_A[1]+Sor_A[2]+Sor_A[3]+Sor_A[4];
    WriteMsg('ID:',EbcdicToAscii(S),'Mezo Db:',Act_Rec_Tart.Mezo_Db);
    For i := 1 To Act_Rec_Tart.Mezo_Db Do
    Begin
      Write(Act_Rec_Tart.Mezok[i]:4);
    End;
    WriteLn;
}
    (* ÁTKONVERTÁLOM Az MTID-et ASCII-ra *)
    For i := 1 To 4 Do Sor_A[i] := ASCII[Ord(Sor_A[i])];
    // az alábbi nem jó, mert bináris EMV tageket elrontana
    // For i := 21 To Char_Db Do Sor_A[i] := ASCII[Ord(Sor_A[i])];
    MTID := Sor_A[1] + Sor_A[2] + Sor_A[3] + Sor_A[4];
    BITMAP := '';
    For i := 5 To 5 + 16 Do
    Begin
      BITMAP := BITMAP + Sor_A[i];
    End;

    Pos_Header    := 0;
    Pos_Data_Form := 0;
    Pos_Data_Raw  := 0;

    MezoFormazas(4, False);
    FejRovatFormazas(4, 'MTID', 1, '-');
    if Field_Descriptor[1].NoShow = false then begin
      MezoFormazas(Field_Descriptor[1].Hossz, True,Field_Descriptor[1].Conv);
      FejRovatFormazas(Field_Descriptor[1].Hossz, 'BITMAP          ', 1, '|');
    end else begin
      Pos_Data_Raw := Pos_Data_Raw + Field_Descriptor[1].Hossz;
    end;

    //Char_Db_B := Char_Db_B - 17;
    //FejRovatFormazas(16, ' ', 2, ' ');

    rrc := Azonositok;  (* AZONOSÍTÓK BEOLVASÁSA A REKORDBÓL *)
    if rrc.rc <> 0 then begin
      Result := rrc;
      Raise Exception.Create(Result.Note);
    end;
    i := 1;
    If MTID = '1240' Then Pr1240Db := Pr1240Db + 1;
    If (MTID = '1240') or (MTID ='1440') Then
    Begin
      If MTID = '1240' Then j := 1 Else j := 2;
      If TxType =  '01' Then i := 1;
      If TxType =  '03' Then i := 2;
      If TxType =  '04' Then i := 3;
      If TxType =  '05' Then i := 4;
      If TxType =  '09' Then i := 5;
      Stat_1[i,j].SumTxDb := Stat_1[i,j].SumTxDb + 1;
      Stat_1[i,j].SumTxVol:= Stat_1[i,j].SumTxVol + TxVol div 100;
    End;

    Akt_Rec := Rec_Find_2(MTID,BITMAP,FUC,FOC,Verzio);
    If Akt_Rec = 0 Then
    Begin
      //WriteMsg('Nem Ismert!', MTID,'  ',BITMAP,'  ',FUC,'  ',FOC);
      Ismert := False;
      UnKnown := UnKnown + 1;
    End
    Else Begin
      Ismert := True;
      Rec_Descr[Akt_Rec].DbPerFile := Rec_Descr[Akt_Rec].DbPerFile + 1;
     { WriteMsg('Ismert, neve: ',Rec_Descr[Akt_Rec].Nev); ReadLn;}
    End;

    If Ismert Then Rec_Id := Rec_Descr[Akt_Rec].Kod Else Rec_Id := MTID;
    (* ITT KEZDÕDIK A BITMAP-en TÚLI MEZÕK FORMÁZÁSA *)
    {
    if Field_Descriptor[1].NoShow then begin
      Pos_Header := 5 ; Pos_Data_Form := 4 ;
    end else begin
      Pos_Header := 21 ; Pos_Data_Form := 20 ;
    end;
    Pos_Header := 21 ; Pos_Data_Form := 20 ;
    }
    Prev_Field := 2;
    For i := 2 To Act_Rec_Tart.Mezo_db Do
    Begin
      j := Act_Rec_Tart.Mezok[i];

      if (Act_Rec_Tart.ForceBlank[i]) then begin
        if MTID = '1240' then begin
          If Field_Descriptor[j].Hossz_Tipus = 'F' then begin
            k := Field_Descriptor[j].Hossz;
            FejRovat := Field_Descriptor[j].R_Nev;
            MezoFormazas(k, True,cnvNo,true,false);
            FejRovatFormazas(max(k,length(FejRovat)), FejRovat, 1, '|');
          end else begin
            if Field_Descriptor[j].DefLen > 0 then begin
             k := Field_Descriptor[j].DefLen + Length(Field_Descriptor[j].DefLenStr) + 1;
             //FejRovat := Copy(AnsiString('LLL'),1,Length(Field_Descriptor[j].DefLenStr));
            end else begin
             k := Length(Field_Descriptor[j].R_Nev);
             //FejRovat := 'LLL';
            end;
            FejRovat := Field_Descriptor[j].R_Nev;
            MezoFormazas(k, True,cnvNo,true,false);
            FejRovatFormazas(max(k,length(FejRovat)), FejRovat, 1, '|');
          end;
          //
          //
          continue;
        end else begin
          continue;
        end;
      end;


      k1 := Field_Descriptor[j].Hossz;
      If Field_Descriptor[j].Hossz_Tipus = 'F'
      Then k := k1
      Else
      Begin
        {
        s := ''; n := 1; r := '';
        For m := Pos_Data_Raw + 1 To Pos_Data_Raw + 1 + k1 - 1 Do
        Begin
          s := s + ' ';  r := r + 'L';
          s[n] := Sor_A[m];
          n := n + 1;
        End;
        val(s, k, n);
        FejRovat := r;
        }
        k := Field_Descriptor[j].Calc_Len;
        {
        FejRovat := Copy(AnsiString('LLL'),1,k1);
        MezoFormazas(k1, True);
        FejRovatFormazas(k1, FejRovat, 1, '|');
        Pos_Header := Pos_Header - k1 - 1;
        if k1 = 2
          Then r := AnsiString(FormatFloat('00',j))
          else r := AnsiString(FormatFloat('000',j));
        If Length(r) > k1 Then r := ' ';
        FejRovatFormazas(k1, r, 2, '|');
        }
      End;
      act_field_len := k;

      if Field_Descriptor[j].NoShow then begin
        If Field_Descriptor[j].Hossz_Tipus = 'F'
          then Pos_Data_Raw := Pos_Data_Raw + act_field_len
          else Pos_Data_Raw := Pos_Data_Raw + Field_Descriptor[j].Hossz + act_field_len;
        continue;
      end;

      If Field_Descriptor[j].Hossz_Tipus <> 'F' then begin
        FejRovat := Copy(AnsiString('LLL'),1,Field_Descriptor[j].Hossz);

        MezoFormazas(Field_Descriptor[j].Hossz, True);
        FejRovatFormazas(Field_Descriptor[j].Hossz, FejRovat, 1, '|');
        Pos_Header := Pos_Header - Field_Descriptor[j].Hossz - 1;
        if Field_Descriptor[j].Hossz = 2
          Then r := AnsiString(FormatFloat('00',j))
          else r := AnsiString(FormatFloat('000',j));
        If Length(r) > Field_Descriptor[j].Hossz Then r := ' ';
        FejRovatFormazas(Field_Descriptor[j].Hossz, r, 2, '|');

      end;

      If Length(Field_Descriptor[j].H_Nev) <= k
      Then FejRovat := Field_Descriptor[j].H_Nev {+ st}
      Else FejRovat := Field_Descriptor[j].R_Nev {+ st};
{     WriteLn(FejRovat);}
      Str(j,s);
      AlaBonthatoe := AlaBonthato(S, Rec_Id, Proc_Class);
      If AlaBonthatoe Then
      Begin
        If (AlaBont[Al_Rec_Id].P_SRD [0] = 'PDSs') Then
        Begin
          rrc := MezoFormazas3(Al_Rec_Id);
          if rrc.rc <> 0 then begin
            Result := rrc;
            Raise Exception.Create(Result.Note);
          end;
        End Else
        Begin
          m := Pos_Header;
          rrc := MezoFormazas2(Al_Rec_Id);
          if rrc.rc <> 0 then begin
            Result := rrc;
            Raise Exception.Create(Result.Note);
          end;
          n := Pos_Header - m - 1;
          Pos_Header := m;
          FejRovatFormazas(n, FejRovat, 2, '|');
          FejRovatFormazas(2, ' ', 2, '|');
          Pos_Header := Pos_Header - 3;
        End;
      End Else
      Begin
        m := Pos_Header;
        MezoFormazas(k, True,Field_Descriptor[j].Conv);
        If Length(FejRovat) > k
        Then Hossz := Length(Fejrovat)
        Else Hossz := k;

        FejRovatFormazas(Hossz, FejRovat, 1, '|');
        n := Pos_Header - m - 1;
        Pos_Header := m;
        FejRovatFormazas(n, ' ', 2, ' ')
      End;


{     WriteMsg('Fejrovat pozíció: ',Char_Db_B:5,'Adatsor pozíció: ',Char_Db_A:5);}
    End;

    If (Akt_Rec <> Elozo_Rec) or (Akt_Rec = 0) Then
    Begin
      WriteLn(Filvarki);
      If Ismert Then WriteLn(Filvarki,'     ',Rec_Descr[Akt_Rec].Nev)
                Else WriteLn(FilvarKi,'Ismeretlen Rekord!!!');
    End;

    If (Prev_SorC_1 <> Copy(Sor_C[1],1,Pos_Header)) or
       (Prev_SorC_2 <> Copy(Sor_C[2],1,Pos_Header)) Then
    Begin
      WriteLn(Filvarki);
      If AlaBont_Szint = 2 Then
      Begin
        WriteLn(FilvarKi,Copy(Sor_C[2],1,Pos_Header));
      End;

      WriteLn(FilvarKi,Copy(Sor_C[1],1,Pos_Header));
    End;
{
    For i := 1 To Char_Db_B Do
    Begin
      Write(Filvarki,Sor_B[i]);
    End;
    WriteLn(Filvarki);
}
    WriteLn(FilvarKi,Copy(Sor_B,1,Pos_Header));

    Elozo_Rec := Akt_Rec;
    Prev_SorC_1 := Copy(Sor_C[1],1,Pos_Header);
    Prev_SorC_2 := Copy(Sor_C[2],1,Pos_Header);

    (* EGY REKORDDAL VÉGEZTÜNK, JÖHET A KÖVETKEZÕ, AMÍG VAN*)
    Inc(RecCounter);
    if RecCounter mod 1000 = 0 then begin
      FilePos := FileSeek(InFile,Int64(0),fsFromCurrent);
      Progress := Word(Trunc((FilePos / FS) * 100));
      SetProgress(Progress);
      //IlManFrm.ProgressBar1.position := Progress;
      //Application.ProcessMessages;
      if PleaseCancel then begin
        Result.Note := 'Processing cancelled by User';
        WriteMsg(Result.Note);
        Result.rc   := 2;
        raise Exception.Create('User cancellation');
      end;
    end;
    If block_1014 Then
    Begin

      rc := Read_1014_Bytes(RDW_Str,4);
      If rc <> 0 Then Begin
        Result.Note := 'Failed to read input file';
        WriteMsg(Result.Note);
        Raise Exception.Create(Result.Note);
      End;
      RDW := Byte(RDW_Str[3])*256 + Byte(RDW_Str[4]);
      RDW_Sum := RDW_Sum + RDW + 4;

      If RDW = 0 Then // means no more record, but fillers to fill up last 1014 block
      Begin

        Act_Pos_1012 := (RDW_Sum mod 1012);

        if Act_Pos_1012 > 0 then begin
          rc := Read_1014_Bytes(Act_Line,(1012 - Act_Pos_1012));
          If rc <> 0 Then Begin
            Result.Note := 'Failed to read input file';
            WriteMsg(Result.Note);
            Raise Exception.Create(Result.Note);
          End;
        end;
        DataLen := 0;
        // HA ÖSSZEFÛZTÜNK TÖBB INCOMING FÁJLT, LEHET MÉG FOLYTATÁS
        rc := Read_1014_Bytes(RDW_Str,4);
        If rc = 0 Then
        Begin
          ACT_POS_1014 := 5;
          RDW_Sum      := 0;
          RDW := Byte(RDW_Str[3])*256 + Byte(RDW_Str[4]);
          RDW_Sum := RDW_Sum + RDW + 4;
          rc := Read_1014_Bytes(Act_Line,RDW);
          If rc <> 0 Then Begin
            Result.Note := 'Failed to read input file';
            WriteMsg(Result.Note);
            Raise Exception.Create(Result.Note);
          End;
          Move(Act_Line[1],Sor_A[1],RDW);
          DataLen := RDW;
          startindex := 1;
        End;
      End Else
      Begin
        rc := Read_1014_Bytes(Act_Line,RDW);
        If rc <> 0 Then Begin
          Result.Note := 'Failed to read input file';
          WriteMsg(Result.Note);
          Raise Exception.Create(Result.Note);
        End;
        Move(Act_Line[1],Sor_A[1],RDW);
        DataLen := RDW;
        startindex := 1;
      End;
    End Else
    Begin

      startindex := (DataLen-Pos_Data_Raw) + 1;
      Move(Sor_A[Pos_Data_Raw+1],Sor_A[1],DataLen-Pos_Data_Raw);
      ReadLen  := FileRead(InFile,Sor_A[startindex],max_header_len-startindex+1);
      DataLen := ReadLen + startindex - 1;
      If (DataLen > 22) and (Sor_A[1] = #13) and (Sor_A[2] = #10) Then
      Begin
        Move(Sor_A[3],Sor_A[1],DataLen-2);
        DataLen := DataLen - 2;
      End;
    End;

  END;
  // setting progress to 'Max'
  SetProgress(High(Integer));
  (* VÉGE A FÁJLNAK, VÉGEZTÜNK AZ ÖSSZES REKORDDAL*)
  WriteLn(FilvarKi);
  WriteLn(FilvarKi,'SUMMARY BY RECORD TYPE:');
  WriteLn(FilvarKi);
  j := 0;
  st := '............................................';
  For i := 1 To 80 Do
  Begin
    If Rec_Descr[i].DbPerFile <> 0 Then
    Begin
      WriteLn(FilvarKi,Trim(Rec_Descr[i].Nev),
                       //'(',
                       //Copy(Rec_Descr[i].Verzio,1,1),'.',
                       //Copy(Rec_Descr[i].Verzio,2,1),
                       //')',
                       Copy(st,1,51-Length(Trim(Rec_Descr[i].Nev))),':'
                       ,Rec_Descr[i].DbPerFile:10);
      j := j + Rec_Descr[i].DbPerFile;
    End;
  End;
  WriteLn(FilvarKi);
  If UnKnown <> 0 Then
  begin
    WriteLn(FilvarKi,'Count of Unknown Records:':52,UnKnown:10);
    j := j + UnKnown;
  End;
  WriteLn(FilvarKi,'Count of records with MTID=1240:':52,Pr1240Db:10);
  WriteLn(FilvarKi,'Total count of records:':52, j:10);
  WriteLn(FilvarKi);
  {
  WriteLn(FilvarKi,'PRESENTMENTEK TÍPUSAI, TARTALMUK:');
  WriteLn(FilvarKi);
  WriteLn(FilvarKi,'Tranzakció Típusa N/R Darabszám   Tx.Összeg  Átl.Tx.Ö    Tx.Fee  Átl.Tx.F');
  WriteLn(FilvarKi,'----------------- --- ---------  ----------  --------  --------  --------');
  For I := 1 To 5 Do For J := 1 To 2 Do
  Begin
    If Stat_1[i,j].SumTxDb = 0 Then
    Begin
      ii := 0; jj := 0;
    End
    Else
    Begin
      ii := Stat_1[i,j].SumTxVol div Stat_1[i,j].SumTxDb;
      jj := Stat_1[i,j].SumTxFee div Stat_1[i,j].SumTxDb;
    End;
    WriteLn(FilvarKi,Stat_1[i,j].TxType:17,Stat_1[i,j].RevStat:4,
                     Stat_1[i,j].SumTxDb:10,Stat_1[i,j].SumTxVol:12,
                     ii:10,Stat_1[i,j].SumTxFee:10,jj:10);
  End;
  }
  CloseFile(Filvarki);
  FileClose(InFile);

  Result.rc := 0; // no error
 Except
   On E:Exception Do
   Begin
     if Result.rc = 0 then Result.rc := 1;
     Result.Note := Shortstring(E.Message);
     CloseFile(Filvarki);
     FileClose(InFile);
   End;
 end;
End;


Initialization
  st := '                                                               ';
  For i := 1 To 2 Do st := st + st;
  UnKnown := 0;
  AlaBont_Szint := 1;
  Pr1240Db := 0;
  For i := 1 To 80 Do
  Begin
    Rec_Descr[i].DbPerFile := 0;
  End;
  FillChar(Sor_star[1],max_header_len,'*');

Finalization



End.
