{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterCpp.pas, released 2000-04-10.
The Original Code is based on the dcjCppSyn.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Michael Trier.
All Rights Reserved.

Contributors to the SynEdit and mwEdit projects are listed in the
Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id: synhighlightercpp.pp 57288 2018-02-12 14:55:20Z mattias $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
  - strings on multiple lines are not supported
-------------------------------------------------------------------------------}
{
@abstract(Provides a C++ syntax highlighter for SynEdit)
@author(Michael Trier)
@created(1998)
@lastmod(2000-05-05)
The SynHighlighterCpp unit provides SynEdit with a C++ syntax highlighter.
Thanks to Martin Waldenburg.
}

{
Modified by RD42 for Pawn language highlighting
https://github.com/dashr9230/Pawno
}

unit pawnhighlighter;

interface

uses
  SysUtils, LCLIntf, Classes, Controls, Graphics,
  SynEditTypes, SynEditHighlighter;

type
  TtkTokenKind = (tkAsm, tkComment, tkDirective, tkIdentifier, tkKey, tkNull,
    tkNumber, tkSpace, tkString, tkSymbol, tkUnknown);

  TxtkTokenKind = (
    xtkAdd, xtkAddAssign, xtkAnd, xtkAndAssign, xtkArrow, xtkAssign,
    xtkBitComplement, xtkBraceClose, xtkBraceOpen, xtkColon, xtkComma,
    xtkDecrement, xtkDivide, xtkDivideAssign, xtkEllipse, xtkGreaterThan,
    xtkGreaterThanEqual, xtkIncOr, xtkIncOrAssign, xtkIncrement, xtkLessThan,
    xtkLessThanEqual, xtkLogAnd, xtkLogComplement, xtkLogEqual, xtkLogOr,
    xtkMod, xtkModAssign, xtkMultiplyAssign, xtkNotEqual, xtkPoint, xtkQuestion,
    xtkRoundClose, xtkRoundOpen, xtkScopeResolution, xtkSemiColon, xtkShiftLeft,
    xtkShiftLeftAssign, xtkShiftRight, xtkShiftRightAssign, xtkSquareClose,
    xtkSquareOpen, xtkStar, xtkSubtract, xtkSubtractAssign, xtkXor,
    xtkXorAssign);

  TRangeState = (rsUnknown, rsAnsiC, rsAnsiCAsm, rsAnsiCAsmBlock, rsAsm,
                 rsAsmBlock, rsDirective, rsDirectiveComment, {rsString34, rsString39,}
                 rsAsmBlockString, rsAsmString, rsDirectiveString, rsString
                );

const
  // map the range into a stringtype range (comments are never mapped into string)
  // keep the range, if it already is a string-range
  SynCppRangeToStringRange: Array [TRangeState] of TRangeState
              = (rsString, rsString, rsString, rsString, rsAsmString,
                 rsAsmBlockString, rsDirectiveString, rsString,
                 rsAsmBlockString, rsAsmString, rsDirectiveString, rsString
                );
  // map the string-range back into the correct range
  // non string ranges are kept as they were
  SynCppStringRangeToRange: Array [TRangeState] of TRangeState
              = (rsUnknown, rsAnsiC, rsAnsiCAsm, rsAnsiCAsmBlock, rsAsm,
                 rsAsmBlock, rsDirective, rsDirectiveComment,
                 rsAsmBlock, rsAsm, rsDirective, rsUnknown
                );

type

  TProcTableProc = procedure of object;

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function: TtkTokenKind of object;

  TSynPawnSyn = class(TSynCustomHighlighter)
  private
    fAsmStart: Boolean;
    fRange: TRangeState;
    fLine: PChar;
    fProcTable: array[#0..#255] of TProcTableProc;
    Run: LongInt;
    fStringLen: Integer;
    fToIdent: PChar;
    fTokenPos: Integer;
    FTokenID: TtkTokenKind;
    FExtTokenID: TxtkTokenKind;
    fLineNumber: Integer;
    fIdentFuncTable: array[0..206] of TIdentFuncTableFunc;
    fAsmAttri: TSynHighlighterAttributes;
    fCommentAttri: TSynHighlighterAttributes;
    fDirecAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fInvalidAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    function KeyHash(ToHash: PChar): Integer;
    function KeyComp(const aKey: String): Boolean;
    function Func17: TtkTokenKind;
    function Func21: TtkTokenKind;
    function Func32: TtkTokenKind;
    function Func34: TtkTokenKind;
    function Func36: TtkTokenKind;
    function Func42: TtkTokenKind;
    function Func45: TtkTokenKind;
    function Func48: TtkTokenKind;
    function Func54: TtkTokenKind;
    function Func57: TtkTokenKind;
    function Func61: TtkTokenKind;
    function Func62: TtkTokenKind;
    function Func68: TtkTokenKind;
    function Func69: TtkTokenKind;
    function Func70: TtkTokenKind;
    function Func73: TtkTokenKind;
    function Func76: TtkTokenKind;
    function Func77: TtkTokenKind;
    function Func78: TtkTokenKind;
    function Func86: TtkTokenKind;
    function Func88: TtkTokenKind;
    function Func92: TtkTokenKind;
    function Func102: TtkTokenKind;
    function Func109: TtkTokenKind;
    function Func116: TtkTokenKind;
    procedure AnsiCProc;
    procedure AndSymbolProc;
    procedure AsciiCharProc;
    procedure AtSymbolProc;
    procedure BraceCloseProc;
    procedure BraceOpenProc;
    procedure CRProc;
    procedure ColonProc;
    procedure CommaProc;
    procedure DirectiveProc;
    procedure EqualProc;
    procedure GreaterProc;
    procedure IdentProc;
    procedure LFProc;
    procedure LowerProc;
    procedure MinusProc;
    procedure ModSymbolProc;
    procedure NotSymbolProc;
    procedure NullProc;
    procedure NumberProc;
    procedure OrSymbolProc;
    procedure PlusProc;
    procedure PointProc;
    procedure QuestionProc;
    procedure RoundCloseProc;
    procedure RoundOpenProc;
    procedure SemiColonProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure SquareCloseProc;
    procedure SquareOpenProc;
    procedure StarProc;
    procedure StringProc;
    procedure TildeProc;
    procedure XOrSymbolProc;
    procedure UnknownProc;
    function AltFunc: TtkTokenKind;
    procedure InitIdent;
    function IdentKind(MayBe: PChar): TtkTokenKind;
    procedure MakeMethodTables;
  protected
    function GetIdentChars: TSynIdentChars; override;
    function GetExtTokenID: TxtkTokenKind;
  public
    class function GetLanguageName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
      override;
    function GetEol: Boolean; override;
    function GetRange: Pointer; override;
    function GetTokenID: TtkTokenKind;
    procedure SetLine(const NewValue: String; LineNumber:Integer); override;
    function GetToken: String; override;
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: integer); override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: integer; override;
    function GetTokenPos: Integer; override;
    procedure Next; override;
    procedure SetRange(Value: Pointer); override;
    procedure ResetRange; override;
    property ExtTokenID: TxtkTokenKind read GetExtTokenID;
  published
    property AsmAttri: TSynHighlighterAttributes read fAsmAttri write fAsmAttri;
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri
      write fCommentAttri;
    property DirecAttri: TSynHighlighterAttributes read fDirecAttri
      write fDirecAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri
      write fIdentifierAttri;
    property InvalidAttri: TSynHighlighterAttributes read fInvalidAttri
      write fInvalidAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property NumberAttri: TSynHighlighterAttributes read fNumberAttri
      write fNumberAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri
      write fSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read fStringAttri
      write fStringAttri;
    property SymbolAttri: TSynHighlighterAttributes read fSymbolAttri
      write fSymbolAttri;
  end;

implementation

uses
  SynEditStrConst;

var
  Identifiers: array[#0..#255] of ByteBool;
  mHashTable: array[#0..#255] of Integer;

procedure MakeIdentTable;
var
  I: Char;
begin
  for I := #0 to #255 do
  begin
    Case I of
      '@', '_', '0'..'9', 'a'..'z', 'A'..'Z': Identifiers[I] := True;
    else Identifiers[I] := False;
    end;
    Case I in['a'..'z', 'A'..'Z'] of { Bug fix: mHashTable['_'] was uninitialised. [Kit] }
      True:
        begin
          if (I > #64) and (I < #91) then mHashTable[I] := Ord(I) - 64 else
            mHashTable[I] := Ord(I) - 95;
        end;
    else mHashTable[I] := 0;
    end;
  end;
end;

procedure TSynPawnSyn.InitIdent;
var
  I: Integer;
  pF: PIdentFuncTableFunc;
begin
  pF := PIdentFuncTableFunc(@fIdentFuncTable);
  for I := Low(fIdentFuncTable) to High(fIdentFuncTable) do begin
    pF^ := @AltFunc;
    Inc(pF);
  end;
  fIdentFuncTable[17] := @Func17;
  fIdentFuncTable[21] := @Func21;
  fIdentFuncTable[32] := @Func32;
  fIdentFuncTable[34] := @Func34;
  fIdentFuncTable[36] := @Func36;
  fIdentFuncTable[42] := @Func42;
  fIdentFuncTable[45] := @Func45;
  fIdentFuncTable[48] := @Func48;
  fIdentFuncTable[54] := @Func54;
  fIdentFuncTable[57] := @Func57;
  fIdentFuncTable[61] := @Func61;
  fIdentFuncTable[62] := @Func62;
  fIdentFuncTable[68] := @Func68;
  fIdentFuncTable[69] := @Func69;
  fIdentFuncTable[70] := @Func70;
  fIdentFuncTable[73] := @Func73;
  fIdentFuncTable[77] := @Func77;
  fIdentFuncTable[76] := @Func76;
  fIdentFuncTable[78] := @Func78;
  fIdentFuncTable[86] := @Func86;
  fIdentFuncTable[88] := @Func88;
  fIdentFuncTable[92] := @Func92;
  fIdentFuncTable[102] := @Func102;
  fIdentFuncTable[109] := @Func109;
  fIdentFuncTable[116] := @Func116;
end;

function TSynPawnSyn.KeyHash(ToHash: PChar): Integer;
begin
  Result := 0;
  while ToHash^ in ['_', '0'..'9', 'a'..'z', 'A'..'Z'] do
  begin
    inc(Result, mHashTable[ToHash^]);
    inc(ToHash);
  end;
  fStringLen := ToHash - fToIdent;
end; { KeyHash }

function TSynPawnSyn.KeyComp(const aKey: String): Boolean;
var
  I: Integer;
  Temp: PChar;
begin
  Temp := fToIdent;
  if Length(aKey) = fStringLen then
  begin
    Result := True;
    for i := 1 to fStringLen do
    begin
      if Temp^ <> aKey[i] then
      begin
        Result := False;
        break;
      end;
      inc(Temp);
    end;
  end else Result := False;
end; { KeyComp }

function TSynPawnSyn.Func17: TtkTokenKind;
begin
  if KeyComp('if') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPawnSyn.Func21: TtkTokenKind;
begin
  if KeyComp('do') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPawnSyn.Func32: TtkTokenKind;
begin
  if KeyComp('case') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPawnSyn.Func34: TtkTokenKind;
begin
  if KeyComp('char') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPawnSyn.Func36: TtkTokenKind;
begin
  if KeyComp('asm') or KeyComp('_asm') or KeyComp('__asm') then
  begin
    Result := tkKey;
    fRange := rsAsm;
    fAsmStart := True;
  end else
    Result := tkIdentifier;
end;

function TSynPawnSyn.Func42: TtkTokenKind;
begin
  if KeyComp('for') then Result := tkKey else
    if KeyComp('break') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPawnSyn.Func45: TtkTokenKind;
begin
  if KeyComp('else') then Result := tkKey else
    if KeyComp('new') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPawnSyn.Func48: TtkTokenKind;
begin
  if KeyComp('false') then Result := tkKey else
    if KeyComp('bool') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPawnSyn.Func54: TtkTokenKind;
begin
  if KeyComp('defined') then Result := tkKey else
    if KeyComp('tagof') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPawnSyn.Func57: TtkTokenKind;
begin
  if KeyComp('enum') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPawnSyn.Func61: TtkTokenKind;
begin
  if KeyComp('goto') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPawnSyn.Func62: TtkTokenKind;
begin
  if KeyComp('while') then Result := tkKey else
    if KeyComp('exit') then Result := tkKey else
      if KeyComp('sleep') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPawnSyn.Func68: TtkTokenKind;
begin
  if KeyComp('true') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPawnSyn.Func69: TtkTokenKind;
begin
  if KeyComp('public') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPawnSyn.Func70: TtkTokenKind;
begin
  if KeyComp('state') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPawnSyn.Func73: TtkTokenKind;
begin
  if KeyComp('stock') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPawnSyn.Func76: TtkTokenKind;
begin
  if KeyComp('const') then Result := tkKey else
    if KeyComp('default') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPawnSyn.Func77: TtkTokenKind;
begin
  if KeyComp('native') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPawnSyn.Func78: TtkTokenKind;
begin
  if KeyComp('static') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPawnSyn.Func86: TtkTokenKind;
begin
  if KeyComp('sizeof') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPawnSyn.Func88: TtkTokenKind;
begin
  if KeyComp('switch') then Result := tkKey else
    if KeyComp('assert') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPawnSyn.Func92: TtkTokenKind;
begin
  if KeyComp('forward') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPawnSyn.Func102: TtkTokenKind;
begin
  if KeyComp('return') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPawnSyn.Func109: TtkTokenKind;
begin
  if KeyComp('continue') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPawnSyn.Func116: TtkTokenKind;
begin
  if KeyComp('operator') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPawnSyn.AltFunc: TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynPawnSyn.IdentKind(MayBe: PChar): TtkTokenKind;
var
  HashKey: Integer;
begin
  fToIdent := MayBe;
  HashKey := KeyHash(MayBe);
  if HashKey < 207 then
    Result := fIdentFuncTable[HashKey]()
  else
    Result := tkIdentifier;
end;

procedure TSynPawnSyn.MakeMethodTables;
var
  I: Char;
begin
  for I := #0 to #255 do
    case I of
      '&': fProcTable[I] := @AndSymbolProc;
      #39: fProcTable[I] := @AsciiCharProc;
      '@': fProcTable[I] := @AtSymbolProc;
      '}': fProcTable[I] := @BraceCloseProc;
      '{': fProcTable[I] := @BraceOpenProc;
      #13: fProcTable[I] := @CRProc;
      ':': fProcTable[I] := @ColonProc;
      ',': fProcTable[I] := @CommaProc;
      '#': fProcTable[I] := @DirectiveProc;
      '=': fProcTable[I] := @EqualProc;
      '>': fProcTable[I] := @GreaterProc;
      '?': fProcTable[I] := @QuestionProc;
      'A'..'Z', 'a'..'z', '_': fProcTable[I] := @IdentProc;
      #10: fProcTable[I] := @LFProc;
      '<': fProcTable[I] := @LowerProc;
      '-': fProcTable[I] := @MinusProc;
      '%': fProcTable[I] := @ModSymbolProc;
      '!': fProcTable[I] := @NotSymbolProc;
      #0: fProcTable[I] := @NullProc;
      '0'..'9': fProcTable[I] := @NumberProc;
      '|': fProcTable[I] := @OrSymbolProc;
      '+': fProcTable[I] := @PlusProc;
      '.': fProcTable[I] := @PointProc;
      ')': fProcTable[I] := @RoundCloseProc;
      '(': fProcTable[I] := @RoundOpenProc;
      ';': fProcTable[I] := @SemiColonProc;
      '/': fProcTable[I] := @SlashProc;
      #1..#9, #11, #12, #14..#32: fProcTable[I] := @SpaceProc;
      ']': fProcTable[I] := @SquareCloseProc;
      '[': fProcTable[I] := @SquareOpenProc;
      '*': fProcTable[I] := @StarProc;
      #34: fProcTable[I] := @StringProc;
      '~': fProcTable[I] := @TildeProc;
      '^': fProcTable[I] := @XOrSymbolProc;
      else fProcTable[I] := @UnknownProc;
    end;
end;

constructor TSynPawnSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fAsmAttri := TSynHighlighterAttributes.Create(@SYNS_AttrAssembler, SYNS_XML_AttrAssembler);
  AddAttribute(fAsmAttri);
  fCommentAttri := TSynHighlighterAttributes.Create(@SYNS_AttrComment, SYNS_XML_AttrComment);
  fCommentAttri.Style:= [fsItalic];
  AddAttribute(fCommentAttri);
  fIdentifierAttri := TSynHighlighterAttributes.Create(@SYNS_AttrIdentifier, SYNS_XML_AttrIdentifier);
  AddAttribute(fIdentifierAttri);
  fInvalidAttri := TSynHighlighterAttributes.Create(@SYNS_AttrIllegalChar, SYNS_XML_AttrIllegalChar);
  AddAttribute(fInvalidAttri);
  fKeyAttri := TSynHighlighterAttributes.Create(@SYNS_AttrReservedWord, SYNS_XML_AttrReservedWord);
  fKeyAttri.Style:= [fsBold];
  AddAttribute(fKeyAttri);
  fNumberAttri := TSynHighlighterAttributes.Create(@SYNS_AttrNumber, SYNS_XML_AttrNumber);
  AddAttribute(fNumberAttri);
  fSpaceAttri := TSynHighlighterAttributes.Create(@SYNS_AttrSpace, SYNS_XML_AttrSpace);
  fSpaceAttri.Foreground := clWindow;
  AddAttribute(fSpaceAttri);
  fStringAttri := TSynHighlighterAttributes.Create(@SYNS_AttrString, SYNS_XML_AttrString);
  AddAttribute(fStringAttri);
  fSymbolAttri := TSynHighlighterAttributes.Create(@SYNS_AttrSymbol, SYNS_XML_AttrSymbol);
  AddAttribute(fSymbolAttri);
  fDirecAttri := TSynHighlighterAttributes.Create(@SYNS_AttrPreprocessor, SYNS_XML_AttrPreprocessor);
  AddAttribute(fDirecAttri);
  SetAttributesOnChange(@DefHighlightChange);
  InitIdent;
  MakeMethodTables;
  fRange := rsUnknown;
  fAsmStart := False;
  fDefaultFilter := 'Pawn Files (*.p,*.pwn,*.pawn)|*.p;*.pwn;*.pawn';
end; { Create }

procedure TSynPawnSyn.SetLine(const NewValue: String; LineNumber:Integer);
begin
  inherited;
  fLine := PChar(NewValue);
  Run := 0;
  fLineNumber := LineNumber;
  Next;
end; { SetLine }

procedure TSynPawnSyn.AnsiCProc;
begin
  fTokenID := tkComment;
  case FLine[Run] of
    #0:
      begin
        NullProc;
        exit;
      end;
    #10:
      begin
        LFProc;
        exit;
      end;
    #13:
      begin
        CRProc;
        exit;
      end;
  end;

  while FLine[Run] <> #0 do
    case FLine[Run] of
      '*':
        if fLine[Run + 1] = '/' then
        begin
          inc(Run, 2);
          if fRange = rsAnsiCAsm then
            fRange := rsAsm
          else if fRange = rsAnsiCAsmBlock then
            fRange := rsAsmBlock
          else if fRange = rsDirectiveComment then
            fRange := rsDirective
          else
            fRange := rsUnKnown;
          break;
        end else
          inc(Run);
      #10: break;
      #13: break;
    else inc(Run);
    end;
end;

procedure TSynPawnSyn.AndSymbolProc;
begin
  fTokenID := tkSymbol;
  case FLine[Run + 1] of
    '=':                               {and assign}
      begin
        inc(Run, 2);
        FExtTokenID := xtkAndAssign;
      end;
    '&':                               {logical and}
      begin
        inc(Run, 2);
        FExtTokenID := xtkLogAnd;
      end;
  else                                 {and}
    begin
      inc(Run);
      FExtTokenID := xtkAnd;
    end;
  end;
end;

procedure TSynPawnSyn.AsciiCharProc;
begin
  fTokenID := tkString;
  repeat
    if fLine[Run] = '\' then begin
      if fLine[Run + 1] in [#39, '\'] then                                      //ek 2000-04-26
        inc(Run);
    end;
    inc(Run);
  until fLine[Run] in [#0, #10, #13, #39];
  if fLine[Run] = #39 then
    inc(Run);
end;

procedure TSynPawnSyn.AtSymbolProc;
begin
  fTokenID := tkUnknown;
  inc(Run);
end;

procedure TSynPawnSyn.BraceCloseProc;
begin
  inc(Run);
  fTokenId := tkSymbol;
  FExtTokenID := xtkBraceClose;
  if fRange = rsAsmBlock then fRange := rsUnknown;
end;

procedure TSynPawnSyn.BraceOpenProc;
begin
  inc(Run);
  fTokenId := tkSymbol;
  FExtTokenID := xtkBraceOpen;
  if fRange = rsAsm then
  begin
    fRange := rsAsmBlock;
    fAsmStart := True;
  end;
end;

procedure TSynPawnSyn.CRProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
  if fLine[Run + 1] = #10 then Inc(Run);
end;

procedure TSynPawnSyn.ColonProc;
begin
  fTokenID := tkSymbol;
  Case FLine[Run + 1] of
    ':':                               {scope resolution operator}
      begin
        inc(Run, 2);
        FExtTokenID := xtkScopeResolution;
      end;
  else                                 {colon}
    begin
      inc(Run);
      FExtTokenID := xtkColon;
    end;
  end;
end;

procedure TSynPawnSyn.CommaProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
  FExtTokenID := xtkComma;
end;

procedure TSynPawnSyn.DirectiveProc;
begin
  if fLine[Run] in [#0, #10, #13] then begin
    if (Run <= 0) or (fLine[Run - 1] <> '\') then
      fRange := rsUnknown;
    fProcTable[fLine[Run]];
  end else begin
    fTokenID := tkDirective;
    while TRUE do
      case fLine[Run] of
        '/': // comment?
          begin
            if fLine[Run + 1] = '/' then begin // is end of directive as well
              fRange := rsUnknown;                                              //ek 2000-04-25
              break;
            end else if fLine[Run + 1] = '*' then begin // might be embedded only
              fRange := rsDirectiveComment;
              break;
            end else
              Inc(Run);
          end;
        '\': // directive continued on next line?
          begin
            Inc(Run);
            if fLine[Run] in [#0, #10, #13] then begin
              fRange := rsDirective;
              break;
            end;
          end;
        #0, #10, #13:
          begin
            fRange := rsUnknown;
            break;
          end;
        else Inc(Run);
      end;
  end;
end;

procedure TSynPawnSyn.EqualProc;
begin
  fTokenID := tkSymbol;
  case FLine[Run + 1] of
    '=':                               {logical equal}
      begin
        inc(Run, 2);
        FExtTokenID := xtkLogEqual;
      end;
  else                                 {assign}
    begin
      inc(Run);
      FExtTokenID := xtkAssign;
    end;
  end;
end;

procedure TSynPawnSyn.GreaterProc;
begin
  fTokenID := tkSymbol;
  Case FLine[Run + 1] of
    '=':                               {greater than or equal to}
      begin
        inc(Run, 2);
        FExtTokenID := xtkGreaterThanEqual;
      end;
    '>':
      begin
        if FLine[Run + 2] = '=' then   {shift right assign}
        begin
          inc(Run, 3);
          FExtTokenID := xtkShiftRightAssign;
        end
        else                           {shift right}
        begin
          inc(Run, 2);
          FExtTokenID := xtkShiftRight;
        end;
      end;
  else                                 {greater than}
    begin
      inc(Run);
      FExtTokenID := xtkGreaterThan;
    end;
  end;
end;

procedure TSynPawnSyn.QuestionProc;
begin
  fTokenID := tkSymbol;                {conditional}
  FExtTokenID := xtkQuestion;
  inc(Run);
end;

procedure TSynPawnSyn.IdentProc;
begin
  fTokenID := IdentKind((fLine + Run));
  inc(Run, fStringLen);
  while Identifiers[fLine[Run]] do inc(Run);
end;

procedure TSynPawnSyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TSynPawnSyn.LowerProc;
begin
  fTokenID := tkSymbol;
  case FLine[Run + 1] of
    '=':                               {less than or equal to}
      begin
        inc(Run, 2);
        FExtTokenID := xtkLessThanEqual;
      end;
    '<':
      begin
        if FLine[Run + 2] = '=' then   {shift left assign}
        begin
          inc(Run, 3);
          FExtTokenID := xtkShiftLeftAssign;
        end
        else                           {shift left}
        begin
          inc(Run, 2);
          FExtTokenID := xtkShiftLeft;
        end;
      end;
  else                                 {less than}
    begin
      inc(Run);
      FExtTokenID := xtkLessThan;
    end;
  end;
end;

procedure TSynPawnSyn.MinusProc;
begin
  fTokenID := tkSymbol;
  case FLine[Run + 1] of
    '=':                               {subtract assign}
      begin
        inc(Run, 2);
        FExtTokenID := xtkSubtractAssign;
      end;
    '-':                               {decrement}
      begin
        inc(Run, 2);
        FExtTokenID := xtkDecrement;
      end;
    '>':                               {arrow}
      begin
        inc(Run, 2);
        FExtTokenID := xtkArrow;
      end;
  else                                 {subtract}
    begin
      inc(Run);
      FExtTokenID := xtkSubtract;
    end;
  end;
end;

procedure TSynPawnSyn.ModSymbolProc;
begin
  fTokenID := tkSymbol;
  case FLine[Run + 1] of
    '=':                               {mod assign}
      begin
        inc(Run, 2);
        FExtTokenID := xtkModAssign;
      end;
  else                                 {mod}
    begin
      inc(Run);
      FExtTokenID := xtkMod;
    end;
  end;
end;

procedure TSynPawnSyn.NotSymbolProc;
begin
  fTokenID := tkSymbol;
  case FLine[Run + 1] of
    '=':                               {not equal}
      begin
        inc(Run, 2);
        FExtTokenID := xtkNotEqual;
      end;
  else                                 {not}
    begin
      inc(Run);
      FExtTokenID := xtkLogComplement;
    end;
  end;
end;

procedure TSynPawnSyn.NullProc;
begin
  fTokenID := tkNull;
end;

procedure TSynPawnSyn.NumberProc;
begin
  fTokenID := tkNumber;

  if (FLine[Run] = '0') and (FLine[Run+1] in ['x', 'X'])then
  begin
    inc(Run, 2);
    while FLine[Run] in ['0'..'9', 'A'..'F', 'a'..'f'] do inc(Run);
    if FLine[Run] in ['u', 'U', 'l', 'L'] then inc(Run);
    exit;
  end;

  inc(Run);
  while FLine[Run] in ['0'..'9'] do inc(Run);
  if (FLine[Run]='.') and not(fLine[Run+1]='.')  then begin
    inc(Run);
    while FLine[Run] in ['0'..'9'] do inc(Run);
  end;
  if (FLine[Run]='e') or (fLine[Run]='E')  then begin
    inc(Run);
    if (FLine[Run]='+') or (fLine[Run]='-')  then inc(Run);
    while FLine[Run] in ['0'..'9'] do inc(Run);
  end;
  if FLine[Run] in ['u', 'U', 'l', 'L'] then inc(Run);
end;

procedure TSynPawnSyn.OrSymbolProc;
begin
  fTokenID := tkSymbol;
  case FLine[Run + 1] of
    '=':                               {or assign}
      begin
        inc(Run, 2);
        FExtTokenID := xtkIncOrAssign;
      end;
    '|':                               {logical or}
      begin
        inc(Run, 2);
        FExtTokenID := xtkLogOr;
      end;
  else                                 {or}
    begin
      inc(Run);
      FExtTokenID := xtkIncOr;
    end;
  end;
end;

procedure TSynPawnSyn.PlusProc;
begin
  fTokenID := tkSymbol;
  case FLine[Run + 1] of
    '=':                               {add assign}
      begin
        inc(Run, 2);
        FExtTokenID := xtkAddAssign;
      end;
    '+':                               {increment}
      begin
        inc(Run, 2);
        FExtTokenID := xtkIncrement;
      end;
  else                                 {add}
    begin
      inc(Run);
      FExtTokenID := xtkAdd;
    end;
  end;
end;

procedure TSynPawnSyn.PointProc;
begin
  fTokenID := tkSymbol;
  if (FLine[Run + 1] = '.') and (FLine[Run + 2] = '.') then
    begin                              {ellipse}
      inc(Run, 3);
      FExtTokenID := xtkEllipse;
    end
  else                                 {point}
    begin
      inc(Run);
      FExtTokenID := xtkPoint;
    end;
end;

procedure TSynPawnSyn.RoundCloseProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
  FExtTokenID := xtkRoundClose;
end;

procedure TSynPawnSyn.RoundOpenProc;
begin
  inc(Run);
  FTokenID := tkSymbol;
  FExtTokenID := xtkRoundOpen;
end;

procedure TSynPawnSyn.SemiColonProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
  FExtTokenID := xtkSemiColon;
  if fRange = rsAsm then fRange := rsUnknown;
end;

procedure TSynPawnSyn.SlashProc;
begin
  case FLine[Run + 1] of
    '/':                               {c++ style comments}
      begin
        fTokenID := tkComment;
        inc(Run, 2);
        while not (fLine[Run] in [#0, #10, #13]) do Inc(Run);
      end;
    '*':                               {c style comments}
      begin
        fTokenID := tkComment;
        if fRange = rsAsm then
          fRange := rsAnsiCAsm
        else if fRange = rsAsmBlock then
          fRange := rsAnsiCAsmBlock
        else if fRange <> rsDirectiveComment then
          fRange := rsAnsiC;
        inc(Run, 2);
        while fLine[Run] <> #0 do
          case fLine[Run] of
            '*':
              if fLine[Run + 1] = '/' then
              begin
                inc(Run, 2);
                if fRange = rsDirectiveComment then
                  fRange := rsDirective
                else if fRange = rsAnsiCAsm then
                  fRange := rsAsm
                else
                  begin
                  if fRange = rsAnsiCAsmBlock then
                    fRange := rsAsmBlock
                  else
                    fRange := rsUnKnown;
                  end;
                break;
              end else inc(Run);
            #10, #13:
              begin
                if fRange = rsDirectiveComment then
                  fRange := rsAnsiC;
                break;
              end;
          else inc(Run);
          end;
      end;
    '=':                               {divide assign}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
        FExtTokenID := xtkDivideAssign;
      end;
  else                                 {divide}
    begin
      inc(Run);
      fTokenID := tkSymbol;
      FExtTokenID := xtkDivide;
    end;
  end;
end;

procedure TSynPawnSyn.SpaceProc;
begin
  inc(Run);
  fTokenID := tkSpace;
  while FLine[Run] in [#1..#9, #11, #12, #14..#32] do inc(Run);
end;

procedure TSynPawnSyn.SquareCloseProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
  FExtTokenID := xtkSquareClose;
end;

procedure TSynPawnSyn.SquareOpenProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
  FExtTokenID := xtkSquareOpen;
end;

procedure TSynPawnSyn.StarProc;
begin
  fTokenID := tkSymbol;
  case FLine[Run + 1] of
    '=':                               {multiply assign}
      begin
        inc(Run, 2);
        FExtTokenID := xtkMultiplyAssign;
      end;
  else                                 {star}
    begin
      inc(Run);
      FExtTokenID := xtkStar;
    end;
  end;
end;

procedure TSynPawnSyn.StringProc;
begin
  case FLine[Run] of
    #0:
      begin
        NullProc;
        exit;
      end;
    #10:
      begin
        LFProc;
        exit;
      end;
    #13:
      begin
        CRProc;
        exit;
      end;
  end;
  fTokenID := tkString;
  if not((fRange in [rsAsmBlockString,rsAsmString,rsDirectiveString,rsString])
         and (Run = 0) and (FLine[Run] = #34 ))
  then
    repeat
      if fLine[Run] = '\' then begin
        if fLine[Run + 1] in [#34, '\'] then                                      //ek 2000-04-26
          Inc(Run);
      end;
      inc(Run);
    until fLine[Run] in [#0, #10, #13, #34];
  if FLine[Run] = #34 then begin
    inc(Run);
    fRange := SynCppStringRangeToRange[fRange];
  end else begin
    // string continues in next line
    fRange := SynCppRangeToStringRange[fRange];
  end;
end;

procedure TSynPawnSyn.TildeProc;
begin
  inc(Run);                            {bitwise complement}
  fTokenId := tkSymbol;
  FExtTokenID := xtkBitComplement;
end;

procedure TSynPawnSyn.XOrSymbolProc;
begin
  fTokenID := tkSymbol;
  Case FLine[Run + 1] of
  	'=':                               {xor assign}
      begin
        inc(Run, 2);
        FExtTokenID := xtkXorAssign;
      end;
  else                                 {xor}
    begin
      inc(Run);
      FExtTokenID := xtkXor;
    end;
  end;
end;

procedure TSynPawnSyn.UnknownProc;
begin
  inc(Run);
  while (fLine[Run] in [#128..#191]) OR // continued utf8 subcode
   ((fLine[Run]<>#0) and (fProcTable[fLine[Run]] = @UnknownProc)) do inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynPawnSyn.Next;
begin
  fAsmStart := False;
  fTokenPos := Run;
  case fRange of
    rsAnsiC, rsAnsiCAsm, rsAnsiCAsmBlock, rsDirectiveComment:
      AnsiCProc;
    rsAsmBlockString,rsAsmString,rsDirectiveString,rsString:
      StringProc;
    rsDirective:
      DirectiveProc;
  else
    begin
      fRange := rsUnknown;
      fProcTable[fLine[Run]];
    end;
  end;
end;

function TSynPawnSyn.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := fCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := fIdentifierAttri;
    SYN_ATTR_KEYWORD: Result := fKeyAttri;
    SYN_ATTR_STRING: Result := fStringAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
    SYN_ATTR_NUMBER: Result := fNumberAttri;
    SYN_ATTR_DIRECTIVE: Result := fDirecAttri;
    SYN_ATTR_ASM: Result := fAsmAttri;
    else Result := nil;
  end;
end;

function TSynPawnSyn.GetEol: Boolean;
begin
  Result := fTokenID = tkNull;
end;

function TSynPawnSyn.GetRange: Pointer;
begin
  Result := Pointer(PtrInt(fRange));
end;

function TSynPawnSyn.GetToken: String;
var
  Len: LongInt;
begin
  Result := '';
  Len := Run - fTokenPos;
  SetString(Result, (FLine + fTokenPos), Len);
end;

procedure TSynPawnSyn.GetTokenEx(out TokenStart: PChar;
  out TokenLength: integer);
begin
  TokenLength:=Run-fTokenPos;
  TokenStart:=FLine + fTokenPos;
end;

function TSynPawnSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
  if ((fRange = rsAsm) or (fRange = rsAsmBlock)) and not fAsmStart
    and not (fTokenId in [tkComment, tkSpace, tkNull])
  then
    Result := tkAsm;
end;

function TSynPawnSyn.GetExtTokenID: TxtkTokenKind;
begin
  Result := FExtTokenID;
end;

function TSynPawnSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case fTokenID of
    tkAsm: Result := fAsmAttri;
    tkComment: Result := fCommentAttri;
    tkDirective: Result := fDirecAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkNumber: Result := fNumberAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkSymbol: Result := fSymbolAttri;
    tkUnknown: Result := fInvalidAttri;
    else Result := nil;
  end;
end;

function TSynPawnSyn.GetTokenKind: integer;
begin
  Result := Ord(GetTokenID);
end;

function TSynPawnSyn.GetTokenPos: Integer;
begin
  Result := fTokenPos;
end;

procedure TSynPawnSyn.ReSetRange;
begin
  fRange:= rsUnknown;
end;

procedure TSynPawnSyn.SetRange(Value: Pointer);
begin
  fRange := TRangeState(PtrUInt(Value));
end;

function TSynPawnSyn.GetIdentChars: TSynIdentChars;
begin
  Result := ['_', '0'..'9', 'a'..'z', 'A'..'Z'];
end;

class function TSynPawnSyn.GetLanguageName: string;
begin
  Result := 'Pawn';
end;

initialization
  MakeIdentTable;
  RegisterPlaceableHighlighter(TSynPawnSyn);

end.


