unit uPlot_ResultWriter;
{
*****************************************************************************
*  This file is part of Multiple Acronym Math and Audio Plot - MAAPlot      *
*                                                                           *
*  See the file COPYING.                                                    *
*  for details about the copyright.                                         *
*                                                                           *
*  This program is distributed in the hope that it will be useful,          *
*  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
*                                                                           *
*  MAAplot for Lazarus/fpc                                                  *
*  (C) 2014 Stefan Junghans                                                 *
*****************************************************************************
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dateutils, uPlotDataTypes;

type

  TExportResultType = (ertMAAResult);

  PExportHeader = ^TExportHeader;
  TExportHeader = record
    dwNumResults: Integer;
    dwDate:       TDateTime;
    dwDescription: ShortString;
    dwReserved: ShortString;
    dwVersion: Integer;
    dwResultType: TExportResultType;
  end;

  PExportResult_MaaResult = ^TExportResult_MAAResult;
  TExportResult_MAAResult = record
    dwHeader: TExportHeader;
    dwResultArray: array of TMeasResult;
  end;

  { TPlot_ResultWriter }

  TPlot_ResultWriter = class
  private
    function GetDataDateTime: TDateTime;
    function GetDataDescription: ShortString;
    procedure SetDataDateTime(AValue: TDateTime);
    procedure SetDataDescription(AValue: ShortString);
  protected
    FDataType: TExportResultType;
    FDataHeader: TExportHeader;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function WriteToFile(AFileName: TFilename): Integer; virtual;abstract;
    procedure ReSet;virtual;

    property DataDescription: ShortString read GetDataDescription write SetDataDescription;
    property DataDateTime: TDateTime read GetDataDateTime write SetDataDateTime;
  end;

  { TPlot_ResultWriterMAAResult }

  TPlot_ResultWriterMAAResult = class (TPlot_ResultWriter)
  private
    FData: TExportResult_MAAResult;
  protected
    function PrepareWrite: Integer;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure ReSet;override;

    function WriteToFile(AFileName: TFilename): Integer; override;
    function AddResultRecord(AResultRecord: TMeasResult): Integer;
  end;

  { TPlot_ResultReader }

  TPlot_ResultReader = class
  private
    function GetDataDateTime: TDateTime;
    function GetDataDescription: ShortString;
    function GetNumResults: Integer;
    function GetResultType: TExportResultType;
  protected
    FDataHeader: TExportHeader;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function LoadFromFile(AFileName: TFilename): Integer; virtual;abstract;
    procedure ReSet;virtual;

    property DataDescription: ShortString read GetDataDescription;
    property DataDateTime: TDateTime read GetDataDateTime;
    property ResultType: TExportResultType read GetResultType;
    property NumResults: Integer read GetNumResults;

  end;

  { TPlot_ResultReaderMAAResult }

  TPlot_ResultReaderMAAResult = class (TPlot_ResultReader)
  private
    FData : TExportResult_MAAResult;
  protected
  public
    constructor Create; override;
    destructor Destroy; override;

    function LoadFromFile(AFileName: TFilename): Integer; override;
    procedure ReSet;override;

    // not dll safe, use only within plot (there is no data handshake. datapointer in handeled inside this funtion)
    function GetResultRecord(AResultIndex: Integer; var AResultRecord: TMeasResult): Integer;

  end;




implementation

{ TPlot_ResultReaderMAAResult }

constructor TPlot_ResultReaderMAAResult.Create;
begin
  inherited Create;
end;

destructor TPlot_ResultReaderMAAResult.Destroy;
var
  vLoop: Integer;
begin
  IF length(FData.dwResultArray) = 0 then exit;

  for vLoop := 0 to length(FData.dwResultArray)-1 do begin
    setlength(FData.dwResultArray[vLoop].lpMeasData^, 0, 0);
    Dispose(FData.dwResultArray[vLoop].lpMeasData);
  end;
  setlength(FData.dwResultArray, 0);

  inherited Destroy;
end;

function TPlot_ResultReaderMAAResult.LoadFromFile(AFileName: TFilename
  ): Integer;
var
  vStream: TFileStream;
  vFileName: TFilename;
  vLoop, vLoopRow, vLoopDim: Integer;

  vHeaderItem: TExportHeader;
  vDataItem: PMeasArray;
  vIdentItem: TMeasResultIdent;
  //vHeaderSize, vItemSize: Integer;
  //vValueCount: Integer;
  //vDataSize: Integer;
  //vWriteCount: Integer;
  vReadCount: Integer;

  vNumResults: Integer;
begin
  Result := 0;
  vFileName := AFileName;
  vReadCount:=0;

  vStream := nil;

  with vHeaderItem do begin // init for safety only
    dwNumResults := 0;
    dwDate := Today;
    dwDescription := 'empty';
    dwReserved :=  '';
    dwVersion := 0;
    dwResultType := ertMAAResult;
  end;

  with vIdentItem do begin
    dwResultType:= mrtGenResult;
    diDimensions:= 0;
    diPoints:= 0;
    diMeasurement:= 0;
    dwTimeStamp:= DateTimeToTimeStamp(Now);
  end;


  // clear Data
  for vLoop := 0 to length(FData.dwResultArray)-1 do begin
    setlength(FData.dwResultArray[vLoop].lpMeasData^, 0, 0);
    Dispose(FData.dwResultArray[vLoop].lpMeasData);
  end;
  setlength(FData.dwResultArray, 0);


  try
    if FileExists(vFileName) then
      vStream := TFileStream.Create(vFileName, fmOpenRead)
    else vStream := nil;
  except vStream := nil; end;

  if vStream = nil then exit;

  //vHeaderSize := SizeOf(TExportHeader);

  try
    // cannot check this as items have different sizes ;-)
    //try
    //  if (vStream.Size MOD vItemSize) <>0 then
    //    raise Exception.Create('stored plotdata is corrupted ..');
    //except
    //  Application.HandleException(Self);
    //  vStream.Free;
    //  vStream := TFileStream.Create(vFileName, fmCreate or fmOpenWrite);
    //  vStream.Size := 0;
    //end;


    // load items
    vStream.Position := 0;

    // load header
    vStream.Read(vHeaderItem, SizeOf(TExportHeader));
    vReadCount := vReadCount + SizeOf(TExportHeader);
    FDataHeader := vHeaderItem;


    vNumResults := vHeaderItem.dwNumResults;
    // prepare FData strructire
    setlength(FData.dwResultArray, vNumResults);
    for vLoop := 0 to vNumResults-1 do begin

      // read ident
       vStream.Read(vIdentItem, SizeOf(TMeasResultIdent));
       vReadCount := vReadCount + SizeOf(TMeasResultIdent);
       // prepare FData structire
       New(vDataItem);
       try
       SetLength(vDataItem^, vIdentItem.diPoints, vIdentItem.diDimensions);
       // read data
       for vLoopRow :=0 to vIdentItem.diPoints-1 do
         for vLoopDim :=0 to vIdentItem.diDimensions-1 do
           vStream.Read(vDataItem^[vLoopRow, vLoopDim], SizeOf(TValue));;

       // store
       FData.dwResultArray[vLoop].dwResultIdent := vIdentItem;
       FData.dwResultArray[vLoop].lpMeasData := vDataItem;

       FData.dwHeader := FDataHeader;
       except
         SetLength(vDataItem^, 0, 0);
         Dispose(vDataItem);
       end;
    end;
  finally
    {$IFDEF DEBUG}OutputDebugString(PChar('Headers loaded bytes/elements: ' + IntToStr(vStream.Size) + ' / ' + IntToStr(Result) ));{$ENDIF}
    FreeAndNil(vStream);
  end;

  Result := vReadCount;
end;

procedure TPlot_ResultReaderMAAResult.ReSet;
begin
  inherited ReSet;
end;

function TPlot_ResultReaderMAAResult.GetResultRecord(AResultIndex: Integer;
  var AResultRecord: TMeasResult): Integer;
var
  vLoopDim, vLoopRow: Integer; // vLoop
  //vMeasResult: TMeasResult;
  //vData: PMeasArray;
  //vValueCount, vItemSize: Integer;
begin
  Result := 0;
  IF AResultIndex > (length(FData.dwResultArray) -1) THEN begin
    Result := -1;
    exit;
  END;
  //New(vMeasResult.lpMeasData); // already created in caller
  try
    SetLength(AResultRecord.lpMeasData^, FData.dwResultArray[AResultIndex].dwResultIdent.diPoints, FData.dwResultArray[AResultIndex].dwResultIdent.diDimensions);
    AResultRecord.dwResultIdent := FData.dwResultArray[AResultIndex].dwResultIdent;
    for vLoopRow :=0 to FData.dwResultArray[AResultIndex].dwResultIdent.diPoints-1 do
      for vLoopDim :=0 to FData.dwResultArray[AResultIndex].dwResultIdent.diDimensions-1 do
        AResultRecord.lpMeasData^[vLoopRow, vLoopDim] := FData.dwResultArray[AResultIndex].lpMeasData^[vLoopRow, vLoopDim];

    AResultRecord.dwResultIdent := FData.dwResultArray[AResultIndex].dwResultIdent;
  except
    setlength(AResultRecord.lpMeasData^, 0, 0);
    Dispose(AResultRecord.lpMeasData);
    Result := -1;
  end;

end;

{ TPlot_ResultReader } //=======================================================

function TPlot_ResultReader.GetDataDateTime: TDateTime;
begin
  Result := FDataHeader.dwDate;
end;

function TPlot_ResultReader.GetDataDescription: ShortString;
begin
  Result := FDataHeader.dwDescription;
end;

function TPlot_ResultReader.GetNumResults: Integer;
begin
  Result := FDataHeader.dwNumResults;
end;

function TPlot_ResultReader.GetResultType: TExportResultType;
begin
  Result := FDataHeader.dwResultType;
end;

constructor TPlot_ResultReader.Create;
begin
  inherited Create;
  ReSet;
end;

destructor TPlot_ResultReader.Destroy;
begin
  ReSet;
  inherited Destroy;
end;

procedure TPlot_ResultReader.ReSet;
begin
  with FDataHeader do begin
    dwNumResults := 0;
    dwDescription := 'nothing loaded';
    dwDate := Now;
    dwReserved := '';
    dwVersion := -1;
    dwResultType := ertMAAResult;
  end;
end;

{ TPlot_ResultWriterMAAResult }  //=============================================

function TPlot_ResultWriterMAAResult.PrepareWrite: Integer;
begin
  FDataHeader.dwNumResults := length(FData.dwResultArray);
  FData.dwHeader := FDataHeader;
  Result := FDataHeader.dwNumResults;
end;

constructor TPlot_ResultWriterMAAResult.Create;
begin
  inherited Create;
end;

destructor TPlot_ResultWriterMAAResult.Destroy;
begin
  inherited Destroy;
end;

procedure TPlot_ResultWriterMAAResult.ReSet;
var
  vLoop: Integer;
begin
  for vLoop := 0 to length(FData.dwResultArray)-1 do begin
    setlength(FData.dwResultArray[vLoop].lpMeasData^, 0, 0);
    Dispose(FData.dwResultArray[vLoop].lpMeasData);
  end;
  setlength(FData.dwResultArray, 0);
  //
  with FDataHeader do begin
    dwNumResults := 0;
    dwDescription := 'MAA export';
    dwDate := Now;
    dwReserved := '2secnutw3tchndagckluehlasti87l339p6czn93vnrelc83o52p0z7'; // write a sync structure, costs nothing, might help in the future
    dwVersion := 0;
    dwResultType := ertMAAResult;
  end;
end;

function TPlot_ResultWriterMAAResult.WriteToFile(AFileName: TFilename): Integer;
var
  vStream: TFileStream;
  vLoop, vLoopDim, vLoopRow: Integer;
  vFileName: TFilename;
  //vItem: TMeasResult;
  vHeaderSize: Integer;
  vIdentSize, vRowSize: Integer;
  //vValueCount: Integer;
  vWriteCount: Integer;
begin
  // writes as follows:
  // <GlobalHeader><ResultIdent><ResultData><ResultIdent><ResultData><ResultIdent><ResultData>  ....
  vFileName:=AFileName;
  Result := 0;
  vWriteCount:=0;
  //vFileName := FMsgPath + cPATHDELIMITER + cHEADERFILENAME + cCONTENTEXTENSION;

  PrepareWrite;

  vStream := TFileStream.Create(vFileName, fmCreate);
  vStream.Position := 0;    // TODO: not needed
  try
    // write Header
    vHeaderSize := sizeof(TExportHeader);
    vStream.Write(FData.dwHeader, vHeaderSize);
    vWriteCount := vWriteCount + vHeaderSize;

    vIdentSize := SizeOf(TMeasResultIdent);

    for vLoop := 0 to FData.dwHeader.dwNumResults-1 do begin
      //vValueCount := FData.dwResultArray[vLoop].dwResultIdent.diPoints * FData.dwResultArray[vLoop].dwResultIdent.diDimensions;
      vRowSize := FData.dwResultArray[vLoop].dwResultIdent.diPoints;
      // write ResultIdent
      vStream.Write(FData.dwResultArray[vLoop].dwResultIdent, vIdentSize);
      // write Data
      for vLoopRow := 0 to vRowSize-1 do
        for vLoopDim := 0 to FData.dwResultArray[vLoop].dwResultIdent.diDimensions-1 do
          vStream.Write(FData.dwResultArray[vLoop].lpMeasData^[vLoopRow, vLoopDim], SizeOf(TValue));

      vWriteCount := vWriteCount + vIdentSize + vRowSize * FData.dwResultArray[vLoop].dwResultIdent.diDimensions * SizeOf(TValue);
    end;
    Result := vWriteCount;
  finally
    FreeAndNil(vStream);
  end;
  ReSet;
end;

function TPlot_ResultWriterMAAResult.AddResultRecord(AResultRecord: TMeasResult
  ): Integer;
var
  vLoopDim, vLoopRow: Integer;  // vLoop
  vMeasResult: TMeasResult;
  //vValueCount, vItemSize: Integer;
begin
  Result := 0;

  IF (AResultRecord.dwResultIdent.diPoints < 1) OR (AResultRecord.dwResultIdent.diDimensions < 1) THEN begin
    Result := -1;
    exit;
  END;

  SetLength(FData.dwResultArray, length(FData.dwResultArray) + 1);

  vMeasResult.dwResultIdent := AResultRecord.dwResultIdent;
  New(vMeasResult.lpMeasData);
  setlength(vMeasResult.lpMeasData^, AResultRecord.dwResultIdent.diPoints, AResultRecord.dwResultIdent.diDimensions);
  try
    vMeasResult.dwResultIdent := AResultRecord.dwResultIdent;
    for vLoopRow :=0 to vMeasResult.dwResultIdent.diPoints-1 do
      for vLoopDim :=0 to vMeasResult.dwResultIdent.diDimensions-1 do
        vMeasResult.lpMeasData^[vLoopRow, vLoopDim] := AResultRecord.lpMeasData^[vLoopRow, vLoopDim];


    //vValueCount := vMeasResult.dwResultIdent.diPoints * vMeasResult.dwResultIdent.diDimensions;
    //vItemSize := SizeOf(TMeasResultIdent) + vValueCount * SizeOf(TValue);

    FData.dwResultArray[length(FData.dwResultArray)-1] := vMeasResult;
  except
    setlength(vMeasResult.lpMeasData^, 0, 0);
    Dispose(vMeasResult.lpMeasData);
  end;

end;

{ TPlot_ResultWriter } //=======================================================

function TPlot_ResultWriter.GetDataDateTime: TDateTime;
begin
  Result := FDataHeader.dwDate;
end;

function TPlot_ResultWriter.GetDataDescription: ShortString;
begin
  Result := FDataHeader.dwDescription;
end;

procedure TPlot_ResultWriter.SetDataDateTime(AValue: TDateTime);
begin
  FDataHeader.dwDate := AValue;
end;

procedure TPlot_ResultWriter.SetDataDescription(AValue: ShortString);
begin
  FDataHeader.dwDescription := AValue;
end;

constructor TPlot_ResultWriter.Create;
begin
  inherited Create;
  ReSet;
end;

destructor TPlot_ResultWriter.Destroy;
begin
  ReSet;
  inherited Destroy;
end;

procedure TPlot_ResultWriter.ReSet;
begin
  with FDataHeader do begin
    dwNumResults := 0;
    dwDescription := 'MAA export';
    dwDate := Now;
    dwReserved := '2secnutw3tchndagckluehlasti87l339p6czn93vnrelc83o52p0z7'; // write a sync structure, costs nothing, might help in the future
    dwVersion := 0;
    dwResultType := ertMAAResult;
  end;
end;

end.

