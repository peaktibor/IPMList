unit globals ;

{$mode ObjFPC}{$H+}

interface


uses
  Classes , SysUtils ;

Type

Trrc = record // Rich Return Code
  rc : integer;
  Note : ShortString;
End;

TWriteMsg = procedure  (Msg : string);
TSetProgress = procedure  (Position : integer);


Var
  CRLF : string = #13#10;

implementation

end.

