unit uLBCircularBufferTests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, uLBCircularBuffer;

type
  TLBCircularBufferTests = class(TTestCase)
  private
    Buffer: TLBCircularBuffer;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestWriteAndRead;
    procedure TestPeekAndSkip;
    procedure TestFindPattern_1Byte;
    procedure TestFindPattern_4Byte;
    procedure TestFindPattern_8Byte;
    procedure TestFindPattern_16Byte;
    procedure TestFindPattern_WrapAround;
    procedure TestResizePreserve;
    procedure TestResizeClear;
  end;

implementation

procedure TLBCircularBufferTests.SetUp;
begin
  Buffer := TLBCircularBuffer.Create(64);
end;

procedure TLBCircularBufferTests.TearDown;
begin
  Buffer.Free;
end;

procedure TLBCircularBufferTests.TestWriteAndRead;
var
  DataIn, DataOut: array[0..15] of Byte;
  i : Integer;

begin
  for i := 0 to 15 do DataIn[i] := i;

  AssertTrue('Writing', Buffer.Write(@DataIn[0], Length(DataIn)));
  AssertEquals('Verify available data', Length(DataIn), Buffer.AvailableForRead);

  AssertTrue('Reading', Buffer.Read(@DataOut[0], Length(DataOut)));
  for i := 0 to Length(DataOut) - 1 do
    AssertEquals('Value ' + IntToStr(i) + ':', DataIn[i], DataOut[i]);

  AssertEquals(0, Buffer.AvailableForRead);
end;

procedure TLBCircularBufferTests.TestPeekAndSkip;
var
  DataIn: array[0..7] of Byte = (10, 20, 30, 40, 50, 60, 70, 80);
  Peeked: Byte;
begin
  Buffer.Write(@DataIn, Length(DataIn));
  Peeked := Buffer.PeekByte(3);
  AssertEquals(40, Peeked);

  AssertTrue(Buffer.Skip(4));
  AssertEquals(4, Buffer.AvailableForRead);
  Peeked := Buffer.PeekByte(0);
  AssertEquals(50, Peeked);
end;

procedure TLBCircularBufferTests.TestFindPattern_1Byte;
var
  Data: array[0..7] of Byte = (1, 2, 3, 4, 5, 6, 7, 8);
begin
  Buffer.Write(@Data, Length(Data));
  AssertEquals(3, Buffer.FindByte(4, 0));
  AssertEquals(-1, Buffer.FindByte(99, 0));
end;

procedure TLBCircularBufferTests.TestFindPattern_4Byte;
var
  Data: array[0..15] of Byte;
  Pattern: array[0..3] of Byte = (100, 101, 102, 103);
  i : Integer;

begin
  for i := 0 to 15 do Data[i] := i;
  Move(Pattern, Data[8], 4); // Inject pattern at position 8

  Buffer.Write(@Data, 16);
  AssertEquals(8, Buffer.FindPattern(@Pattern, 4, 0));
end;

procedure TLBCircularBufferTests.TestFindPattern_8Byte;
var
  Data: array[0..31] of Byte;
  Pattern: array[0..7] of Byte = (200,201,202,203,204,205,206,207);
begin
  FillChar(Data, SizeOf(Data), 0);
  Move(Pattern, Data[24], 8); // Inject pattern at position 24

  Buffer.Write(@Data, 32);
  AssertEquals(24, Buffer.FindPattern(@Pattern, 8, 0));
end;

procedure TLBCircularBufferTests.TestFindPattern_16Byte;
var
  Data: array[0..63] of Byte;
  Pattern: array[0..15] of Byte;
  i : Integer;

begin
  for i := 0 to 15 do Pattern[i] := i + 50;
  FillChar(Data, SizeOf(Data), 0);
  Move(Pattern, Data[40], 16); // Inject pattern at position 40

  Buffer.Write(@Data, 64);
  AssertEquals(40, Buffer.FindPattern(@Pattern, 16, 0));
end;

procedure TLBCircularBufferTests.TestFindPattern_WrapAround;
var
  Data: array[0..63] of Byte;
  Pattern: array[0..7] of Byte = (11,12,13,14,15,16,17,18);
begin
  FillChar(Data[0], SizeOf(Data), 0);

  Buffer.Write(@Data[0], 60);
  Buffer.Read(@Data[0], 56);
  Buffer.Write(@Pattern[0], Length(Pattern));
  AssertEquals(4, Buffer.FindPattern(@Pattern[0], Length(Pattern), 0));
end;

procedure TLBCircularBufferTests.TestResizePreserve;
var
  Data: array[0..7] of Byte = (1,2,3,4,5,6,7,8);
  OutData: array[0..7] of Byte;
  i : Integer;

begin
  Buffer.Write(@Data, 8);
  AssertTrue(Buffer.ResizeBuffer(128, True));
  AssertEquals(8, Buffer.AvailableForRead);

  AssertTrue(Buffer.Read(@OutData, 8));
  for i := 0 to 7 do
    AssertEquals(Data[i], OutData[i]);
end;

procedure TLBCircularBufferTests.TestResizeClear;
var
  _Byte : Byte;

begin
  _Byte := 123;
  Buffer.Write(@_Byte, 1);
  AssertTrue(Buffer.ResizeBuffer(128, False));
  AssertEquals(0, Buffer.AvailableForRead);
end;

initialization
  RegisterTest(TLBCircularBufferTests);
end.

