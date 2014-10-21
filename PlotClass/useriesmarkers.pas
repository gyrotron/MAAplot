unit useriesmarkers;
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
  Classes, SysUtils, uPlotClass, uPlotDataTypes, math, Graphics, uPlotAxis, uPlotUtils;

type
  TMarkerShape = (msLine, msCircle, msTriangle);
  TMarkerType = (mtBorder, mtValue, mtAxis);
  TMarkerMode = (mmMaxPeak, mmMinPeak, mmFixedXValue, mmFixedXIndex, mmFixedXYZ, mmFixedYValue);
  TMarkerReadoutTarget = (mtNone, mtPlotRect, mtExternal);
  TMarkerReadoutItems = (mroValues, mroUnits);


  TMarkerStyleParams = record
    MarkShapes: set of TMarkerShape;
    Diameter: Integer;
    LineWidth: Integer;
    Color: TColor;
    Alpha: Word;
    BackGndColor: TColor;
    BackGndAlpha: Word;
    DrawAngle: Extended;
  end;

  TMarkerVisualParams = record
    XReadout, YReadout, ZReadout: Boolean;
    ReadoutItems: set of TMarkerReadoutItems;
    ShowIdent: Boolean;
    ShowMode: Boolean;
    ShowIndex: Boolean;
    StyleParams: TMarkerStyleParams;
  end;


  TCoeffArray = array of extended;

  TMarkerContainer = class;
  TMarker = class;

  { TMarkerDrawer }

  TMarkerDrawer = class
  private
    FOwnerContainer: TMarkerContainer;
  protected
  public
    constructor Create(AOwnerContainer: TMarkerContainer);virtual;
    destructor Destroy; override;

    procedure DrawMarker(AMarker: TMarker; ABitMap: TBitmap; IsGlobalImage: Boolean);virtual;abstract;
  end;

  { TPeakList }

{ Notes:
  - properties  PeakDetectThreshold, SmoothWidthNear, AutoThresholds and the Savitzky-Golay smoother
    is currently unused (although data is still smoothed which could be canceled actually)
  - The former peak algorithm was not stable so we default to a easy algoritm below
    which has no further parameters except the number of peaks to keep
}

  TPeakList = class
  private
    FAutoThresholds: Boolean;
    FOwnerContainer: TMarkerContainer;
    FAbsMaxIndex: Integer;
    FAbsMinIndex: Integer;
    FNoiseEstimate: Extended;
    FXYLine: TXYLine;  // TODO: use containers line
    FZValue: Extended; // markers only XY compatible !
    FMinPeakIndices: array of Integer;
    FMaxPeakIndices: array of Integer;
    FPeakDetectXHoldoff: Extended;
    FPeakDetectThreshold: Extended;
    FSmoothWidth: Integer;
    function GetSmoothWidthNear: Integer;
    procedure SmoothData(AXYLine:TXYLine; out ASmoothedXYLine: TXYLine);
    procedure SetSmoothWidthNear(AValue: Integer);
  protected
    procedure RefreshAbsMinMax;
  public
    constructor Create(AOwnerContainer: TMarkerContainer);
    destructor Destroy; override;
    procedure _ClearLists;

    function ReNewData(AXYLine: TXYLine; AZValue: Extended; ANeedsSorting: Boolean = false):Integer;
    procedure RefreshPeakList;

    function GetPeakValue(out AXYValue: TXYValue; out AZValue: Extended; AMode: TMarkerMode; AIndex: Integer): Integer;
    property OwnerContainer: TMarkerContainer read FOwnerContainer;

    property PeakDetectThreshold: Extended read FPeakDetectThreshold write FPeakDetectThreshold;
    property SmoothWidthNear: Integer read GetSmoothWidthNear write SetSmoothWidthNear;  // savitzky-golay
    property AutoThresholds: Boolean read FAutoThresholds write FAutoThresholds;
    // need properties MinPeaks, MaxPeaks !  ? see GUI index outside available
  end;


  { TMarkerContainer }

  TMarkerContainer = class
  private
    FHasPeakMarkers: Boolean; // decision if we need to build the peaklist
    FIndependentAxis: Integer;
    FValueAxis: Integer;
    FMarkerDrawer : TMarkerDrawer;
    FPeakList: TPeakList;
    FMarkers: TFPList;
    FXYLine: TXYLine;
    FZValue: Extended; // markers only XY compatible !
    function GetIndependentAxis: Integer;
    function GetMarkerIndex(AMarker: TMarker): Integer;
    function GetMarker(AIndex: Integer): TMarker;
    function GetMarkerCount: Integer;
    function GetValueAxis: Integer;
    procedure SetIndependentAxis(AValue: Integer);                              // deprecated, use always X axis from series
    procedure SetValueAxis(AValue: Integer);                                    // deprecated, use always Y axis
  protected
    FOwnerSeries: TPlotSeriesBase;
    function GetPeakMarkerCount: Integer;
  public
    constructor Create(AOwnerSeries: TPlotSeriesBase);virtual;
    destructor Destroy;override;

    procedure DrawMarkers(ABackBitmap: TBitmap; IsGlobalImage: Boolean); // Target is GlobalImage or FrameRect
    procedure ReNewData(AXYLine: TXYLine; AZValue: Extended; ANeedsSorting: Boolean = false);

    function AddMarker: Integer;
    function RemoveMarker(AMarker: TMarker): Integer;

    function GetValueFromXIndex(out AXYValue: TXYValue;  out AZValue: Extended; AXIndex: Integer): Integer;
    function GetValueFromXValue(out AXYValue: TXYValue; out AZValue: Extended; AXValue: Extended): Integer; // no ipol ! take nearest index

    property PeakList: TPeakList read FPeakList;
    procedure UpdateMarker(AIndex: Integer);

    property MarkerCount: Integer read GetMarkerCount;
    property Marker[AIndex: Integer]: TMarker read GetMarker;
    property MarkerIndex[AMarker: TMarker]: Integer read GetMarkerIndex;

    property OwnerSeries: TPlotSeriesBase read FOwnerSeries;
    property IndependentAxis: Integer read GetIndependentAxis write SetIndependentAxis;  //(i.e. X independent)
    property ValueAxis: Integer read GetValueAxis write SetValueAxis;                    //(i.e. Y(X) values )
  end;


  { TMarker }
  TMarker = class
  private
    FMarkermode: TMarkerMode;
    FMarkerType: TMarkerType;
    FIndex: Integer;
    FFixedValueXYZ: TXYZValue;
    FMarkerVisualParams: TMarkerVisualParams;
    FMarkerShape: TMarkerShape;

    function GetFixedValueXYZ: TXYZValue;
    function GetIndexValue: Integer;
    function GetMarkerMode: TMarkerMode;
    function GetMarkerType: TMarkerType;virtual;
    function GetVisualParams: TMarkerVisualParams;
    procedure SetFixedValueXYZ(AValue: TXYZValue);
    procedure SetIndexValue(AValue: Integer);
    procedure SetMarkerMode(AValue: TMarkerMode);
    procedure SetMarkerType(AValue: TMarkerType);
    procedure SetVisualParams(AValue: TMarkerVisualParams);
  protected
    FOwnerContainer: TMarkerContainer;
    procedure ParamsChanged;
  public
    constructor Create(AOwnerContainer: TMarkerContainer);virtual;
    destructor Destroy;override;

    function GetValue(out AXYValue: TXYValue; out AZValue: Extended): Integer;virtual;
    function GetLineEndValue(out AXYValue: TXYValue; out AZValue: Extended): Integer;virtual;
    function GetFlip: Boolean; // flip for bordermarkers drawn at ViewRange.min


    property VisualParams: TMarkerVisualParams read GetVisualParams write SetVisualParams;

    property OwnerContainer: TMarkerContainer read FOwnerContainer;
    property MarkerType: TMarkerType read GetMarkerType write SetMarkerType;
    property MarkerMode: TMarkerMode read GetMarkerMode write SetMarkerMode;

    property Index: Integer read GetIndexValue write SetIndexValue;
    property FixedValueXYZ: TXYZValue read GetFixedValueXYZ write SetFixedValueXYZ;
  end;


const
  // first entry is Norm Value, then c0...cn
  cSavitzkyGolayCoeff : array[0..8, 0..11] of Integer = (
                                                        (35,    17,  12,  -3,   0,   0,   0,   0,   0,   0,    0,    0),  // width5, index0
                                                        (21,     7,   6,   3,  -2,   0,   0,   0,   0,   0,    0,    0),
                                                        (231,   59,  54,  39,  14, -21,   0,   0,   0,   0,    0,    0),
                                                        (429,   89,  84,  69,  44,   9, -36,   0,   0,   0,    0,    0),
                                                        (143,   25,  24,  21,  16,   9,   0, -11,   0,   0,    0,    0),
                                                        (1105, 167, 162, 147, 122,  87,  42, -13, -78,   0,    0,    0),
                                                        (323,   43,  42,  39,  34,  27,  18,   7,  -6, -21,    0,    0),
                                                        (2261, 269, 264, 249, 224, 189, 144,  89,  24, -51, -136,    0),
                                                        (3059, 329, 324, 309, 284, 249, 204, 149,  84,   9,  -76, -171)
                                                        );
  cAUTOTHREASHOLD_RELY = 0.001;
  cAUTOTHRESHOLD_RELX = 0.025;
  cAUTOTHRESHOLD_SMOOTHN = 7;
  cMAXPEAKS = 20;                // number of peaks to keep

  cMARKER_MODE_NAMES: array[mmMaxPeak..mmFixedYValue] of string = ('MaxPeak','MinPeak','fix X value', 'fix X index', 'fixed XYZ', 'fix Y value');
  cMARKER_TYPE_NAMES: array[mtBorder..mtAxis] of string = ('Border', 'Value', 'Axis');
  cMARKER_SHAPE_NAMES: array[msLine..msTriangle] of string = ('Line', 'Circle', 'Triangle');

implementation

uses uMarkerDrawer, uPlotSeries, uPlotStyles, uPlotRect;  // series for debug only

{
resourcestring
  S_Valuecalc_NotImplemented  = 'No suitable ValueCalculator availabe.'#13#10+
                        'Marking possible if all axes fixed or '#13#10+
                        'for one independent + one marked axis';
  S_RefreshMarker_Incompatible  = 'RefreshMarker:'#13#10+
                        'Incompatible ValueCalculator ';
  S_Markertype_Unknown  = 'Cannot remove marker:'#13#10+
                        'Markertype is unknown ';
}

{ TMarkerDrawer }

constructor TMarkerDrawer.Create(AOwnerContainer: TMarkerContainer);
begin
  inherited Create;

  FOwnerContainer := AOwnerContainer;
end;

destructor TMarkerDrawer.Destroy;
begin
  inherited Destroy;
end;

{ TPeakList }

function TPeakList.GetSmoothWidthNear: Integer;
begin
  Result := FSmoothWidth;
end;

procedure TPeakList.SmoothData(AXYLine: TXYLine; out ASmoothedXYLine: TXYLine);
var
  vLoop: Integer;
  vLoopX: Integer;
  vCoeffRow: Integer;
  vSmoothStart, vSmoothStop: Integer;
begin
  if FSmoothWidth < 5 then exit;
  vSmoothStart := (FSmoothWidth DIV 2); vSmoothStop := length(AXYLine)-1-(FSmoothWidth DIV 2);
  SetLength(ASmoothedXYLine, length(AXYLine));
  vCoeffRow := (FSmoothWidth-5) DIV 2;
  for vLoop := 0 to length(AXYLine)-1 do begin
    if (vLoop < vSmoothStart) OR (vLoop > vSmoothStop) then begin
      ASmoothedXYLine[vLoop].Y := AXYLine[vLoop].Y;
      continue;
    end;
    ASmoothedXYLine[vLoop].Y := 0;
    for vLoopX := - ((FSmoothWidth-1) DIV 2) to ((FSmoothWidth-1) DIV 2) do begin
      if ((vLoopX+vLoop) < 0) or ((vLoopX+vLoop) > length(AXYLine)-1) then continue;
      ASmoothedXYLine[vLoop].Y := ASmoothedXYLine[vLoop].Y +
         ( AXYLine[vLoopX+vLoop].Y * cSavitzkyGolayCoeff[vCoeffRow, abs(vLoopX)+1]);
    end;
    ASmoothedXYLine[vLoop].Y := ASmoothedXYLine[vLoop].Y / cSavitzkyGolayCoeff[vCoeffRow ,0];
    ASmoothedXYLine[vLoop].X := AXYLine[vLoop].X;
  end;

end;

procedure TPeakList.SetSmoothWidthNear(AValue: Integer);
begin
  if AValue < 4 then FSmoothWidth:=1 else
  if AValue > 21 then FSmoothWidth:=21 else
  if odd(AValue) then FSmoothWidth:=AValue else FSmoothWidth:=AValue+1;
end;

procedure TPeakList.RefreshAbsMinMax;
var
  vLoop: Integer;
  vAbsMax, vAbsMin: Extended;
  vSum: Extended;
begin
  vSum:=0;
  vAbsMin:=c_GLOBALMAX;
  vAbsMax:=-c_GLOBALMAX;
  for vLoop := 0 to length(FXYLine)-1 do begin
    vSum := vSum + FXYLine[vLoop].Y;
    if FXYLine[vLoop].Y > vAbsMax then begin
       vAbsMax := FXYLine[vLoop].Y;
       FAbsMaxIndex:=vLoop;
    end else
    if FXYLine[vLoop].Y < vAbsMin then begin
       vAbsMin := FXYLine[vLoop].Y;
       FAbsMinIndex:=vLoop;
    end;
  end;
  FNoiseEstimate:=vSum / length(FXYLine);
  //writeln('NoiseEstimate: ', FloatToStrF(FNoiseEstimate, ffFixed, 4, 4));
end;


constructor TPeakList.Create(AOwnerContainer: TMarkerContainer);
begin
  FOwnerContainer := AOwnerContainer;
  FAutoThresholds:=true;
  FPeakDetectThreshold:=0;
  FPeakDetectXHoldoff:=0;
  FSmoothWidth:=1;
  setlength(FMaxPeakIndices, 0);
  setlength(FMinPeakIndices, 0);
  FAbsMaxIndex := 0;
  FAbsMinIndex := 0;
end;

destructor TPeakList.Destroy;
begin
  _ClearLists;
  setlength(FXYLine, 0);
  inherited Destroy;
end;

procedure TPeakList._ClearLists;
begin
  setlength(FMaxPeakIndices, 0);
  setlength(FMinPeakIndices, 0);
end;

function TPeakList.ReNewData(AXYLine: TXYLine; AZValue: Extended;
  ANeedsSorting: Boolean=false): Integer;
begin
  FZValue:=AZValue;
  // copy data
  setlength(FXYLine, length(AXYLine));
  if length(AXYLine) > 0 then
    Move(AXYLine[0], FXYLine[0], length(AXYLine)*SizeOf(TXYValue));
  // sort according to X value
  if ANeedsSorting then QuickSort_XYLine(FXYLine, 0, length(AXYLine)-1);

  RefreshPeakList;
  Result := 0;
end;


procedure TPeakList.RefreshPeakList;
var
  vLoop: Integer;
  vLastValue, vValue, vDelta: Extended;
  vLastSign, vSign: TValueSign;
  vSmoothedLine, vSortLine: TXYLine;
  vFineAdjust: Integer;
begin
  _ClearLists;
  IF length(FXYLine) < 1 then exit;

  IF OwnerContainer.MarkerCount < 1 then exit;

  RefreshAbsMinMax;          // first calc abs min/max for autothreshold

  IF FAutoThresholds THEN begin
     FPeakDetectThreshold := (FXYLine[FAbsMaxIndex].Y - FXYLine[FAbsMinIndex].Y) * cAUTOTHREASHOLD_RELY;
     //writeln('Threshold: ', FloatToStrF(FPeakDetectThreshold, ffFixed, 4, 4));
     FPeakDetectXHoldoff := length(FXYLine) * cAUTOTHRESHOLD_RELX;    // deprecated
     FSmoothWidth := cAUTOTHRESHOLD_SMOOTHN;
  end;

  SmoothData(FXYLine, vSmoothedLine);

  IF length(vSmoothedLine) < 2 THEN exit;
  vValue := vSmoothedLine[1].Y;
  vLastValue:=vValue;
  vSign := Sign(vValue - vSmoothedLine[0].Y);
   FOR vLoop := 2 to length(vSmoothedLine)-1 DO BEGIN  // no peaks possible at end and begin....
     vLastValue := vValue;
     vLastSign := vSign;
     vValue := vSmoothedLine[vLoop].Y;
     vDelta := vValue - vLastValue;
     vSign := sign(vDelta);
     // we take ALL peaks, problems with threshold algorithm:
     // when two values are the same at smoothedline peak, we do not exceed the threashold !
     IF (vSign <> vLastSign) THEN BEGIN // AND (abs(vDelta) >= PeakDetectThreshold) THEN BEGIN  // (vLoop >= (vLastX + vXIndexHoldoff))  AND (vLast2Value > FNoiseEstimate)
       IF (vSign < 0) THEN BEGIN
                           setlength(FMaxPeakIndices, length(FMaxPeakIndices)+1);
                           FMaxPeakIndices[length(FMaxPeakIndices)-1] := vLoop-1;
                         END
                         ELSE BEGIN
                           setlength(FMinPeakIndices, length(FMinPeakIndices)+1);
                           FMinPeakIndices[length(FMinPeakIndices)-1] := vLoop-1;
                         END;
     END;
   END;

  // sort out MaxPeaks .......................................
  // put indices as X, Y values as Y into vSortline to sort
  setlength(vSortLine, length(FMaxPeakIndices));
  for vLoop := 0 to length(FMaxPeakIndices)-1 do begin
    // adjust real maximum +/- 1 index
    vFineAdjust:=0;
    if (vLoop > 0) and (vLoop < length(FMaxPeakIndices)-1) then begin
       if FXYLine[FMaxPeakIndices[vLoop]-1].Y > FXYLine[FMaxPeakIndices[vLoop]].Y then vFineAdjust:=-1 else
       if FXYLine[FMaxPeakIndices[vLoop]+1].Y > FXYLine[FMaxPeakIndices[vLoop]].Y then vFineAdjust:=+1;
    end;
    vSortLine[vLoop].X := FMaxPeakIndices[vLoop] + vFineAdjust;
    vSortLine[vLoop].Y := FXYLine[FMaxPeakIndices[vLoop] + vFineAdjust].Y;
  end;
    QuickSort_XYLine(vSortLine, 0, length(vSortLine)-1, true);
    setlength(FMaxPeakIndices, min(cMAXPEAKS, length(vSortLine)) );
  for vLoop := 0 to length(FMaxPeakIndices)-1 do begin
    FMaxPeakIndices[vLoop] := round(vSortLine[length(vSortLine)-1-vLoop].X);    // sort is ascending, max at the end
  end;

  // sort out MinPeaks .......................................
  // put indices as X, Y values as Y into vSortline to sort
  setlength(vSortLine, length(FMInPeakIndices));
  for vLoop := 0 to length(FMinPeakIndices)-1 do begin
    // adjust real maximum +/- 1 index
    vFineAdjust:=0;
    if (vLoop > 0) and (vLoop < length(FMinPeakIndices)-1) then begin
       if FXYLine[FMinPeakIndices[vLoop]-1].Y < FXYLine[FMinPeakIndices[vLoop]].Y then vFineAdjust:=-1 else
       if FXYLine[FMinPeakIndices[vLoop]+1].Y < FXYLine[FMinPeakIndices[vLoop]].Y then vFineAdjust:=+1;
    end;
    vSortLine[vLoop].X := FMinPeakIndices[vLoop] + vFineAdjust;
    vSortLine[vLoop].Y := FXYLine[FMinPeakIndices[vLoop] + vFineAdjust].Y;
  end;
    QuickSort_XYLine(vSortLine, 0, length(vSortLine)-1, true);
    setlength(FMinPeakIndices, min(cMAXPEAKS, length(vSortLine)) );
  for vLoop := 0 to length(FMinPeakIndices)-1 do begin
    FMinPeakIndices[vLoop] := round(vSortLine[vLoop].X);    // sort is ascending, min at the beginning
  end;

  setlength(vSmoothedLine, 0);
  setlength(vSortLine, 0);
  //writeln('first index: ', IntToStr(FMaxPeakIndices[0]));
  //writeln('########### final maxPeaks: ', length(FMaxPeakIndices));
  //writeln('########### final minPeaks: ', length(FMinPeakIndices));
end;

function TPeakList.GetPeakValue(out AXYValue: TXYValue; out AZValue: Extended; AMode: TMarkerMode;
  AIndex: Integer): Integer;
begin
  Result := 0;
  AZValue:=FZValue;

  begin
    CASE AMode OF
      mmMaxPeak: begin
                   if (AIndex < 0) or (AIndex > length(FMaxPeakIndices)-1) then begin
                     Result := -1;
                     AXYValue.X := 0; AXYValue.Y := 0;
                   end else begin
                     AXYValue := FXYLine[FMaxPeakIndices[AIndex]];
                   end;
                 end;
      mmMinPeak: begin
                   if (AIndex < 0) or (AIndex > length(FMinPeakIndices)-1) then begin
                     Result := -1;
                     AXYValue.X := 0; AXYValue.Y := 0;
                   end else begin
                     AXYValue := FXYLine[FMinPeakIndices[AIndex]];
                   end;
                 end;
    END;
  end;
end;


 {TMarker}

function TMarker.GetFixedValueXYZ: TXYZValue;
begin
  Result := FFixedValueXYZ;
end;

function TMarker.GetIndexValue: Integer;
begin
  Result := FIndex;
end;

function TMarker.GetMarkerMode: TMarkerMode;
begin
  Result := FMarkermode;
end;

function TMarker.GetMarkerType: TMarkerType;
begin
  Result := FMarkerType;
end;

function TMarker.GetVisualParams: TMarkerVisualParams;
begin
  CASE MarkerMode OF
    mmFixedYValue: FMarkerVisualParams.StyleParams.DrawAngle := TPlotAxis(OwnerContainer.OwnerSeries.OwnerPlot.Axis[OwnerContainer.ValueAxis]).Drawangle;
    otherwise FMarkerVisualParams.StyleParams.DrawAngle := TPlotAxis(OwnerContainer.OwnerSeries.OwnerPlot.Axis[OwnerContainer.IndependentAxis]).Drawangle;
  END;
  // we automagically deliver color = color of series
  // and backgroundcolor = backgroundcolor of plotrect
  FMarkerVisualParams.StyleParams.Color := TPlotStyle(OwnerContainer.OwnerSeries.Style).Color;
  //FMarkerVisualParams.StyleParams.Alpha := ;
  FMarkerVisualParams.StyleParams.BackGndColor := TPlotRect(OwnerContainer.OwnerSeries.OwnerPlot.Axis[OwnerContainer.OwnerSeries.OwnerAxis].OwnerPlotRect).Style.Brush.Color;
  //FMarkerVisualParams.StyleParams.BackGndAlpha := ;

  Result := FMarkerVisualParams;
end;

procedure TMarker.SetFixedValueXYZ(AValue: TXYZValue);
begin
  FFixedValueXYZ := AValue;
  ParamsChanged;
end;

procedure TMarker.SetIndexValue(AValue: Integer);
begin
  FIndex := AValue;
  ParamsChanged;
end;

procedure TMarker.SetMarkerMode(AValue: TMarkerMode);
begin
  FMarkermode := AValue;
  ParamsChanged;
end;

procedure TMarker.SetMarkerType(AValue: TMarkerType);
begin
  FMarkerType:=AValue;
  ParamsChanged;
end;

procedure TMarker.SetVisualParams(AValue: TMarkerVisualParams);
begin
  FMarkerVisualParams := AValue;
  ParamsChanged;
end;

procedure TMarker.ParamsChanged;
begin
  OwnerContainer.UpdateMarker(OwnerContainer.MarkerIndex[self]);
end;

constructor TMarker.Create(AOwnerContainer: TMarkerContainer);
begin
  FOwnerContainer := AOwnerContainer;
  FMarkerShape := msTriangle;
  with FMarkerVisualParams do begin
    XReadout := TRUE;
    YReadout := TRUE;
    ZReadout := TRUE;
    ReadoutItems := [mroValues, mroUnits];
    ShowIdent := TRUE;
    ShowMode := TRUE;
    ShowIndex := TRUE;
    StyleParams.MarkShapes := [msTriangle];
    StyleParams.Diameter:=13;
    StyleParams.LineWidth:=3;
    StyleParams.Color:=clRed;
    StyleParams.Alpha:=$7FFF;
    StyleParams.BackGndColor:= TPlotRect(TPLotAxis(OwnerContainer.OwnerSeries.OwnerPlot.Axis[OwnerContainer.OwnerSeries.OwnerAxis]).OwnerPlotRect).Style.Brush.Color;
    StyleParams.BackGndAlpha:=$DFFF; // $D000;
    // windows does not correctly handle the text on the background if alpha >0 (text gets completely transparent)
    {$IFDEF WINDOWS}
    StyleParams.BackGndAlpha:=$0; // $D000;
    {$ENDIF}
  end;

  FMarkerType := mtValue;
  FMarkermode := mmMaxPeak;
  //FValueAxis := -1;
  //FIndependentAxis := -1;
  FFixedValueXYZ.X := 0;
  FFixedValueXYZ.Y := 0;
  FFixedValueXYZ.Z := 0;
  FIndex := -1;
end;

destructor TMarker.Destroy;
begin
  inherited Destroy;
end;

function TMarker.GetValue(out AXYValue: TXYValue; out AZValue: Extended): Integer;
begin
  AXYValue.X:=0; AXYValue.Y:=0; AZValue:=0;
  Result := -1; // generic marker only

  IF MarkerMode in [mmMaxPeak, mmMinPeak] then begin
    Result := OwnerContainer.PeakList.GetPeakValue(AXYValue, AZValue, MarkerMode, Index);
    if (MarkerType = mtBorder) or (MarkerType = mtAxis) then AXYValue.Y := OwnerContainer.OwnerSeries.OwnerPlot.Axis[OwnerContainer.ValueAxis].ViewRange.min;
    //if result < 0 then exit;
    exit;
  end;

  // check if series axes are set in ownercontainer
  if (OwnerContainer.IndependentAxis < 0)
      or (OwnerContainer.ValueAxis < 0) then exit;

  CASE MarkerMode OF
    mmFixedXIndex: begin
                       Result := OwnerContainer.GetValueFromXIndex(AXYValue, AZValue, Index);
                       if (MarkerType = mtBorder) or (MarkerType = mtAxis) then AXYValue.Y := OwnerContainer.OwnerSeries.OwnerPlot.Axis[OwnerContainer.ValueAxis].ViewRange.min;
                   end;
    mmFixedXValue: begin
                     Result := OwnerContainer.GetValueFromXValue(AXYValue, AZValue, FFixedValueXYZ.X);
                       if (MarkerType = mtBorder) or (MarkerType = mtAxis) then AXYValue.Y := OwnerContainer.OwnerSeries.OwnerPlot.Axis[OwnerContainer.ValueAxis].ViewRange.min;
                   end;
    mmFixedXYZ:    begin
                     AXYValue.X := FFixedValueXYZ.X;
                     AXYValue.Y := FFixedValueXYZ.Y;
                     AZValue    := FFixedValueXYZ.Z;
                     Result := 0;
                   end;
    mmFixedYValue: begin
                     AXYValue.X := OwnerContainer.OwnerSeries.OwnerPlot.Axis[OwnerContainer.IndependentAxis].ViewRange.min;
                     AXYValue.Y := FFixedValueXYZ.Y;
                     AZValue := FFixedValueXYZ.Z;
                     Result := 0;
                   end;
  END;
end;

function TMarker.GetLineEndValue(out AXYValue: TXYValue; out AZValue: Extended
  ): Integer;
begin
  AXYValue.X:=0; AXYValue.Y:=0; AZValue:=0;
  Result := -1; // generic marker only

  IF MarkerMode in [mmMaxPeak, mmMinPeak] then begin
    Result := OwnerContainer.PeakList.GetPeakValue(AXYValue, AZValue, MarkerMode, Index);
    if (MarkerType = mtValue) then AXYValue.Y := OwnerContainer.OwnerSeries.OwnerPlot.Axis[OwnerContainer.ValueAxis].ViewRange.min else
    if (MarkerType = mtAxis) then AXYValue.Y := OwnerContainer.OwnerSeries.OwnerPlot.Axis[OwnerContainer.ValueAxis].ViewRange.max;
    exit;
  end;

  // check if series axes are set in ownercontainer
  if (OwnerContainer.IndependentAxis < 0)
      or (OwnerContainer.ValueAxis < 0) then exit;

  //Result := GetValue(AXYValue, AZValue);

  CASE MarkerMode OF
    mmFixedXIndex: begin
                     Result := OwnerContainer.GetValueFromXIndex(AXYValue, AZValue, Index);
                     if (MarkerType = mtValue) then AXYValue.Y := OwnerContainer.OwnerSeries.OwnerPlot.Axis[OwnerContainer.ValueAxis].ViewRange.min else
                     if (MarkerType = mtAxis) then AXYValue.Y := OwnerContainer.OwnerSeries.OwnerPlot.Axis[OwnerContainer.ValueAxis].ViewRange.max;
                   end;
    mmFixedXValue: begin
                     Result := OwnerContainer.GetValueFromXValue(AXYValue, AZValue, FFixedValueXYZ.X);
                     if (MarkerType = mtValue) then AXYValue.Y := OwnerContainer.OwnerSeries.OwnerPlot.Axis[OwnerContainer.ValueAxis].ViewRange.min else
                     if (MarkerType = mtAxis) then AXYValue.Y := OwnerContainer.OwnerSeries.OwnerPlot.Axis[OwnerContainer.ValueAxis].ViewRange.max;
                   end;
    mmFixedXYZ:    begin
                     AXYValue.X := FFixedValueXYZ.X;
                     AXYValue.Y := FFixedValueXYZ.Y;
                     AZValue    := FFixedValueXYZ.Z;
                     Result := 0;
                   end;
    mmFixedYValue: begin
                     AXYValue.X := OwnerContainer.OwnerSeries.OwnerPlot.Axis[OwnerContainer.IndependentAxis].ViewRange.max;
                     AXYValue.Y := FFixedValueXYZ.Y;
                     AZValue := FFixedValueXYZ.Z;
                     Result := 0;
                   end;
  END;
end;

function TMarker.GetFlip: Boolean;
begin
  if (MarkerType = mtValue) then begin
    case MarkerMode of
      mmMinPeak:    Result := true;
      otherwise     Result := false;
    end;
  end else
  if (MarkerType = mtBorder) then begin
    Result := true;
    case MarkerMode of
      mmFixedYValue: Result := false;
      otherwise      Result := true;
    end;
  end else
  if (MarkerType = mtAxis) then begin
    case MarkerMode of
      mmFixedYValue: Result := false;
      otherwise      Result := true;
    end;
  end;
end;

{ TMarkerContainer }

function TMarkerContainer.GetMarkerCount: Integer;
begin
  IF FMarkers = nil THEN Result := 0 ELSE
  Result := FMarkers.Count;
end;

function TMarkerContainer.GetValueAxis: Integer;
begin
  //Result := FValueAxis;
  Result := TPlotSeries(OwnerSeries).YAxis;
end;

procedure TMarkerContainer.SetIndependentAxis(AValue: Integer);
begin
  FIndependentAxis := AValue;
end;

procedure TMarkerContainer.SetValueAxis(AValue: Integer);
begin
  FValueAxis := AValue;
end;

function TMarkerContainer.GetPeakMarkerCount: Integer;
var
  vLoop: Integer;
begin
  Result := 0;
  for vLoop := 0 to MarkerCount-1 do begin
    if Marker[vLoop].MarkerMode in [mmMaxPeak, mmMinPeak] then begin
      Result := Result +1;
      exit;
    end;
  end;
end;

function TMarkerContainer.GetMarker(AIndex: Integer): TMarker;
begin
  Result := nil;
  IF (AIndex > MarkerCount-1)  OR (AIndex < 0) THEN exit;
  Result := TMarker(FMarkers.Items[AIndex]);
end;

function TMarkerContainer.GetMarkerIndex(AMarker: TMarker): Integer;
begin
  Result := -1;
  IF FMarkers <> nil then
    Result := FMarkers.IndexOf(AMarker);
end;

function TMarkerContainer.GetIndependentAxis: Integer;
begin
  //Result := FIndependentAxis;
  Result := TPlotSeries(OwnerSeries).XAxis;
end;


constructor TMarkerContainer.Create(AOwnerSeries: TPlotSeriesBase);
begin
  FOwnerSeries := AOwnerSeries;
  FPeakList := TPeakList.Create(Self);
  FMarkerDrawer := TMarkerDrawer_Bitmap.Create(Self);
  FValueAxis := -1;
  FIndependentAxis := -1;
  FZValue:=0;
  FHasPeakMarkers:=false;
end;

destructor TMarkerContainer.Destroy;
begin
  FPeakList.Free;
  IF FMarkers <> nil THEN begin
    while FMarkers.Count > 0 do begin
      TMarker(FMarkers.Items[0]).Free;
      FMarkers.Delete(0);
    end;
    FMarkers.Free;
  end;
  FMarkerDrawer.Free;
  inherited Destroy;
end;

procedure TMarkerContainer.DrawMarkers(ABackBitmap: TBitmap; IsGlobalImage: Boolean);
var
  vLoop: Integer;
begin
  //writeln('draw markers, peakcount ', IntToStr(MarkerPeakCount));
  IF ABackBitmap = nil then exit;

  for vLoop := 0 to MarkerCount-1 do begin
    FMarkerDrawer.DrawMarker(TMarker(FMarkers.Items[vLoop]), ABackBitmap, IsGlobalImage);
  end;
end;

procedure TMarkerContainer.ReNewData(AXYLine: TXYLine; AZValue: Extended; ANeedsSorting: Boolean = false);
begin
  // IF MarkerCount < 1 then exit; // cannot skip... if markers are added in a stopped (static) line, we would need the data
  FZValue:=AZValue;
  // copy data
  setlength(FXYLine, length(AXYLine));
  if length(AXYLine) > 0 then
    Move(AXYLine[0], FXYLine[0], length(AXYLine)*SizeOf(TXYValue));

  IF GetPeakMarkerCount > 0 THEN
    PeakList.ReNewData(AXYLine, AZValue, ANeedsSorting);
end;


function TMarkerContainer.AddMarker: Integer;
begin
  IF FMarkers = nil THEN FMarkers := TFPList.Create;
  Result := FMarkers.Add(TMarker.Create(Self));

  // check if we now need to build the peaklist
  if (not FHasPeakMarkers) then begin
    if GetPeakMarkerCount > 0 then begin
      PeakList.ReNewData(FXYLine, FZValue, false);
      FHasPeakMarkers:=true;
    end;
  end;
end;

function TMarkerContainer.RemoveMarker(AMarker: TMarker): Integer;
begin
  Result := -1;
  IF (FMarkers <> nil) and (AMarker <> nil) THEN
    Result := FMarkers.Remove(TMarker(AMarker));
  if (GetPeakMarkerCount = 0) then FHasPeakMarkers := false;
end;


function TMarkerContainer.GetValueFromXIndex(out AXYValue: TXYValue; out AZValue: Extended;
  AXIndex: Integer): Integer;
begin
  Result := -1;
  IF (AXIndex < 0) OR (AXIndex > length(FXYLine)-1) THEN begin
    AXYValue.X:=0;
    AXYValue.Y:=0;
    AZValue:=0;
    exit;
  end;
  AXYValue := FXYLine[AXIndex];
  AZValue:=FZValue;
  Result := 0;
end;

function TMarkerContainer.GetValueFromXValue(out AXYValue: TXYValue; out AZValue: Extended;
  AXValue: Extended): Integer;
var
  vSign, vLastSign: TValueSign;
  vLastDistance, vActualDistance, vActualValue: Extended;
  vResultindex: Integer;
  vLoop: Integer;
begin
  Result := -1;
  IF length(FXYLine) < 1 then exit;
  // attention: no interpolation at the moment, snap to nearest X indexed value
  IF ( (AXValue < OwnerSeries.OwnerPlot.Axis[IndependentAxis].ViewRange.min)
    OR (AXValue > OwnerSeries.OwnerPlot.Axis[IndependentAxis].ViewRange.max) ) THEN exit;

       IF length(FXYLine) < 2 then begin
         AXYValue := FXYLine[0];
         AZValue:=FZValue;
         Result := 0;
         exit;
       end;

       vResultindex := -1;
       vActualValue:=FXYLine[0].X;
       vActualDistance:=vActualValue-AXValue;
       vSign := Sign(vActualDistance);

       FOR vLoop := 1 to length(FXYLine)-1 DO BEGIN
         vLastSign := vSign;
         vLastDistance := vActualDistance;
         vActualValue := FXYLine[vLoop].X;
         vActualDistance := vActualValue-AXValue;
         vSign := Sign(vActualDistance);
         if vSign = vLastSign then continue else
         begin
           if vLastDistance < vActualDistance then vResultindex := vLoop-1
             else vResultindex:=vLoop;
         end;
       END;

       if vResultindex < 0 then vResultindex := length(FXYLine)-1;
       AXYValue := FXYLine[vResultindex];
       AZValue:=FZValue;
       Result := 0;
end;

procedure TMarkerContainer.UpdateMarker(AIndex: Integer);
begin
  // currently ALL markers are updated...
  // we notify the series for update request only
  OwnerSeries.UpdateMarkers(0);
end;



end.

