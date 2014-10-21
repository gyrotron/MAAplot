unit uPlotSeries;
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

// 03.10.12 new: GetStoredLine[ALine] newest is index 0
// GetDisplayLine: actual DisplayLine (can be FAVLine for averaged or maxhold mode)
// remove above functions ! (16.10.14)

// 24.05.13 export/ import a MAAResult

// TODO: unify exporters ! (use same datacontainers ?)
// TODO: property RollingGrid(AColor, onlyalpha)
//       property NicePaint3D; // enables alpha fadeout
// TODO: framerect clipping (otherwise markers draw outside)
// TODO: correctly inherit ADDLIne !

//{$DEFINE DEBUG_WRITE}

interface

uses
  Classes, Graphics, Types, math, GraphMath, uPlotClass, uPlotAxis,
  sysutils, ExtCtrls, uPlotUtils,
  IntfGraphics, GraphType, FPimage, LCLType,
  uPlot_ResultWriter, dateutils, uPlotDataTypes, useriesmarkers,
  uPlotInterpolator; //uPlotMarkers; lazcanvas

type

  //TColorPoint = packed record
  //  Pt:    TPoint;
  //  Color: TColor;
  //end;

   TDrawTarget = packed record
     drwCanvas : TCanvas;
     drwBitMap : TBitmap;
     drwRect: TRect;
   end;

   TTraceMode = (smClearWrite, smAveragingN, smMaxHold);  // smMaxHoldN possible ??

  { TPlotSeries }

  TPlotSeries = class(uPlotClass.TPlotSeriesBase)
  private
    FLineEndPoint: TPoint;
    FInterpolate: Boolean;
    FInterpolator: TInterpolator;
    FMarkerContainer: TMarkerContainer;
    FSeriestype: TSeriestype;
    FAutoDraw: Boolean;
    FCaption: String;
    FColoredAxis: Integer;
    FTransParency: Word;
    FUnits: TStrings;
    FYAxis: Integer;
    function GetColoredAxis: Integer; virtual;
    //function GetDrawTarget: TDrawTarget; virtual;
    function GetInterpolate: Boolean;
    function GetInterpolator: TInterpolator;
    function GetMarkerContainer: TMarkerContainer;
    procedure SetColoredAxis(const AValue: Integer);
    procedure SetInterpolate(AValue: Boolean);
    procedure SetUnits(const AValue: TStrings);
    procedure SetYAxis(Value: Integer);
    function GetXAxis: Integer;
    procedure SetXAxis(const Value: Integer);
  protected
    FDataChanged: Boolean;
    procedure Redraw; override;
    function PlotImage: TImage;override;
    function GetAxesUsed: TList; override;
    function GetUnitString(AAxisIndex: Integer): ShortString; override;

  public
    constructor Create(AOwnerPlot: TPlot); override;
    destructor Destroy; override;

    function CheckRefresh(var ANeedsRefresh: Boolean; var ANeedsClear: Boolean): Integer;virtual;
    procedure DoPlotToImage; override;
    procedure UpdateMarkers(AContainerIndex: Integer);override;

    function GetLineEndpoint(out ALineEndPoint: TPoint): Integer; virtual;
    procedure DrawXY(X, Y: Extended);virtual;
    procedure Clear;override;
    procedure AddUnit(Aaxis: char; AUnit: String);
    function DrawLegendSample(X, Y: Integer; ACanvas: TCanvas; ADrawVisible: Boolean): Integer;  // in screen coords, returns width in pixels
    property XAxis: Integer read GetXAxis write SetXAxis;
    property YAxis: Integer read FYAxis write SetYAxis;
    property AutoDraw: Boolean read FAutoDraw write FAutoDraw;  // TODO: implement
    property ColoredAxis: Integer read GetColoredAxis write SetColoredAxis;
    property Caption: String read FCaption write FCaption;
    property Units: TStrings read FUnits write SetUnits;   // in X Y Z order
    //property DrawTarget: TDrawTarget read GetDrawTarget;   // used especially for Markers !
    property TransParency: Word read FTransParency write FTransParency;
    property MarkerContainer: TMarkerContainer read GetMarkerContainer; // write SetMarkerContainer; // 19.08.14
    property Interpolate: Boolean read GetInterpolate write SetInterpolate;
    property Interpolator: TInterpolator read GetInterpolator;
  end;

  { TXYPlotSeries }

  TXYPlotSeries = class(TPlotSeries)
  private
    FItems: TFPList;
    FLastItem: Integer;                                                         // deprectated
    FMaxStoredItems: Integer;
  protected
    procedure Redraw; override;
    function GetNumStoredItems: Integer;
    procedure SetMaxStoredItems(const AValue: Integer);
    function GetValueRange(AAxisIndex: Integer): TValueRange; override;
    function GetAutoScaleRange(AAxisIndex: Integer): TValueRange; override; // TODO

    function IsInViewRange(XValue, YValue: Extended): Boolean;
  public
    constructor Create(AOwnerPlot: TPlot); override;
    destructor Destroy; override;

    function CheckRefresh(var ANeedsRefresh: Boolean; var ANeedsClear: Boolean): Integer;override;

    procedure AddValue(X, Y: Extended);   // TODO: implement maxstoreditems
    function ExportSeriesData(AFileName: TFilename): Integer;virtual;  // 24.05.13
    function ImportSeriesData(AFileName: TFileName): Integer;virtual;  // 24.05.13

    //function GetItemPtr(AIndex: Integer): Pointer; virtual;                     // deprectated
    procedure Clear;override;

    property MaxStoredItems: Integer read FMaxStoredItems write SetMaxStoredItems;
    property NumStoredItems: Integer read GetNumStoredItems;
  end;


  { TXYZPlotSeries }

  TXYZPlotSeries = class(TXYPlotSeries)
  private
    FZAxis: Integer;
    procedure SetZAxis(const AValue: Integer);virtual;
  protected
    procedure DrawXY(X, Y, Z: Extended); reintroduce;
    procedure Redraw; override;
    function GetValueRange(AAxisIndex: Integer): TValueRange; override;
    function GetAxesUsed: TList; override;
    function GetUnitString(AAxisIndex: Integer): ShortString; override;

    function IsInViewRange(XValue, YValue, ZValue: Extended): Boolean;
  public
    constructor Create(AOwnerPlot: TPlot); override;
    destructor Destroy; override;

    procedure AddValue(X, Y, Z: Extended);  // TODO: implement maxstoreditems
    property ZAxis: Integer read FZAxis write SetZAxis;
    procedure Clear;override;
  end;

  { TXYWFPlotSeries }

  TXYWFPlotSeries = class(TXYZPlotSeries)
  private
    FElapsedTime: Extended; // rework waterfalls to get a complete XYZ MAAresult
                            // this includes the time and elapsed counter can be deleted then !
    FMarkNext: Boolean;
    FYAct: Extended;
    FActualBaseLine: Cardinal;
    FLastIndex: Integer;
    FLineWidth: Extended; //Integer;
    FTimePerLine: Extended;
    FLinesQueued: Cardinal;
    FYThreshold: Extended;
    function GetColoredAxis: Integer; override;
    procedure SetActualBaseLine(AValue: Cardinal);
    procedure SetLineWidth(const AValue: Extended);
    procedure SetTimePerLine(const AValue: Extended);
    procedure SetYThreshold(AValue: Extended);
    procedure SetZAxis(const AValue: Integer);override;
  protected
    procedure DrawLine(ALine: TXYLine);virtual;
    procedure Redraw; override;
    function GetValueRange(AAxisIndex: Integer): TValueRange; override;
    function GetAxesUsed: TList; override;
  public
    constructor Create(AOwnerPlot: TPlot); override;
    destructor Destroy; override;

    procedure DoPlotToImage;override;

    // TODO testing
    function ExportSeriesData(AFileName: TFilename): Integer;override;  // 30.07.13
    function ImportSeriesData(AFileName: TFileName): Integer;override;  // 30.07.13

    procedure Clear;override;
    procedure AddLine(ALine: TXYLine);
    procedure MarkNext;

    property LineWidth: Extended read FLineWidth write SetLineWidth; // was Integer like
    property TimePerLine: Extended read FTimePerLine write SetTimePerLine;
    property ActualBaseLine: Cardinal read FActualBaseLine write SetActualBaseLine;    // todo: not used ???
    property YThreshold: Extended read FYThreshold write SetYThreshold;
  end;


  { TXYWF3DPlotSeries }

  TXYWF3DPlotSeries = class(TXYWFPlotSeries)
  private
    FAlphaMask: TLazIntfImage; // what is this TLazIntfImageMask; ?
    FXAct: Extended;
    //function GetDrawTarget: TDrawTarget; override;
    procedure _UpdateFadeMask;
  protected
    procedure DrawLine(ALine: TXYLine);override;
    procedure Redraw; override;

  public
    constructor Create(AOwnerPlot: TPlot); override;
    destructor Destroy; override;

    procedure DoPlotToImage;override;
  end;


  { TXYSpectrumPlotSeries }
  // TODO: transparency and FPColor

  TXYSpectrumPlotSeries = class(TXYPlotSeries)
  private
    FAVLine: TXYLine;
    FAverageN: Integer;
    //FLastDrawnScreenPt: TColorPoint;
    FActualLine: Int64;     // TODO: delete as can be calculated on the fly (=NumStoredItems-1)
    //FLastIndex: Integer;
    FTraceMode: TTraceMode;
    FLinesQueued: Cardinal;
    function GetDisplayedLine: TXYLine;
    function GetStoredLine(AIndex: Integer): TXYLine;
    procedure SetActualLine(const AValue: Int64);
    procedure SetAverageN(const AValue: Integer);
    procedure SetTraceMode(const AValue: TTraceMode);
  protected
    function GetValueRange(AAxisIndex: Integer): TValueRange; override;
    procedure DrawLine(ALine: TXYLine);virtual;
    procedure Redraw; override;
    property ActualLine: Int64 read FActualLine write SetActualLine;
  public
    constructor Create(AOwnerPlot: TPlot); override;
    destructor Destroy; override;

    function CheckRefresh(var ANeedsRefresh: Boolean; var ANeedsClear: Boolean): Integer;override;

    //function GetItemPtr(AIndex: Integer): Pointer; override;  // TODO: should be give the Pointer ? it is FAVLine or, StoredLine[x]
    function GetLineEndpoint(out ALineEndPoint: TPoint): Integer; override;

    // TODO testing
    function ExportSeriesData(AFileName: TFilename): Integer;override;  // 24.05.13
    function ImportSeriesData(AFileName: TFileName): Integer;override;  // 24.05.13

    procedure Clear;override;
    procedure AddLine(ALine: TXYLine);      // 29.01.13 correct to copy data

    procedure DoPlotToImage;override;

    property TraceMode: TTraceMode read FTraceMode write SetTraceMode;
    property AverageN: Integer read FAverageN write SetAverageN;
    //
    property DisplayedLine: TXYLine read GetDisplayedLine;
    property StoredLined[AIndex:Integer]: TXYLine read GetStoredLine;     // 0 is oldest !
  end;



const
  c_LegendSampleWidth = 10;    // width in px of legend sample drawn
  c_FITMARGIN = 0.04;    // 4% margin for autoscaling
  c_TraceMode_Names: array[0..2] of string = ('ClearWrite','AverageN','MaxHold');

implementation

//uses uPlotStyles, uPlotRect, uPlotMarkers;
uses uPlotStyles, uPlotRect;

resourcestring
  S_NoFastSeries  = 'This is not a Fast series.'#13#10+
                        'Only fast series can plot to the plotrects DataImage.';

{ TPlotSeries }
// ***************************************************************************
{
General 2D plotseries
- draws X/Y points directly to the PlotImage
- cannot store drawn points (display is cleared on every Redraw, i.e. size change
- not usually used (use TXYPlotSeries instead)
}

procedure TPlotSeries.DrawXY(X, Y: Extended);
var
  vPt: TPoint;
  vError: Integer;
begin
  vError := uPlotUtils.XYToScreen(TPlotAxis(OwnerPlot.Axis[XAxis]), TPlotAxis(OwnerPlot.Axis[YAxis]),
                        X, Y, vPt );
  IF vError < 0 THEN exit;
  if Style is TSeriesStyleLines then begin
    vError := uPlotUtils.XYToScreen(TPlotAxis(OwnerPlot.Axis[XAxis]), TPlotAxis(OwnerPlot.Axis[YAxis]), X, TPlotAxis(OwnerPlot.Axis[YAxis]).ViewRange.min ,FLineEndPoint);
  end;

  IF ColoredAxis = XAxis THEN DrawPoint(vPt, PlotImage.Canvas, uPlotAxis.TPlotAxis(OwnerPlot.Axis[XAxis]).ValueColor[X])
  ELSE IF ColoredAxis = YAxis THEN DrawPoint(vPt, PlotImage.Canvas, uPlotAxis.TPlotAxis(OwnerPlot.Axis[YAxis]).ValueColor[Y])
  ELSE DrawPoint(vPt, PlotImage.Canvas);
end;

procedure TPlotSeries.Clear;
begin
  //
end;

procedure TPlotSeries.AddUnit(Aaxis: char; AUnit: String);
var
  vAxis: Char;
begin
  vAxis := lowerCase(Aaxis);
  while Units.Count < 3 do Units.Add('unit not used');
  case vAxis of
  'x': begin
            Units.Strings[0] := AUnit;
       end;
  'y': begin
            Units.Strings[1] := AUnit;
       end;
  'z': begin
            Units.Strings[2] := AUnit;
       end;
  end;
end;


function TPlotSeries.DrawLegendSample(X, Y: Integer; ACanvas:TCanvas; ADrawVisible: Boolean): Integer;
// Draws sample of series i.e. for legends; returns width of drawn sample in px
// call X,Y coordinates in screen coords
var
  vPt : TPoint;
begin
  vPt.X := X;
  vPt.Y := Y;
  IF ADrawVisible THEN DrawSamplePoint(vPt, ACanvas, TRUE);
  vPt.X := vPt.X + c_LegendSampleWidth;
  IF ADrawVisible THEN DrawSamplePoint(vPt, ACanvas, FALSE);
  Result := c_LegendSampleWidth;
end;

function TPlotSeries.GetXAxis: Integer;
begin Result := inherited OwnerAxis; end;

procedure TPlotSeries.Redraw;
begin
  (* do nothing since nothing is stored *)
end;

function TPlotSeries.GetAxesUsed: TList;
var
  vItem : PInteger;
begin
  try
    Result := TList.Create;
    new(vItem);
    try
      vItem^ := XAxis;
      Result.Add(vItem);
    except
       Dispose(vItem); raise;
    end;
    new(vItem);
    try
      vItem^ := YAxis;
      Result.Add(vItem);
    except
       Dispose(vItem); raise;
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TPlotSeries.GetUnitString(AAxisIndex: Integer): ShortString;
begin
  IF (AAxisIndex <> XAxis) AND (AAxisIndex <> YAxis) THEN BEGIN
    Result := 'Axis does not match series';
    exit;
  END;
  IF (AAxisIndex = XAxis) THEN Result := Units.Strings[0] ELSE
  IF (AAxisIndex = YAxis) THEN Result := Units.Strings[1];
end;

function TPlotSeries.CheckRefresh(var ANeedsRefresh: Boolean;
  var ANeedsClear: Boolean): Integer;
begin
  Result := 0;
  // used in all series which do not directly plot to PlotImage.Canvas
  // when PlotImage has Changed, ANeedsRefresh should be set,
  // so PlotRect.UpdateSeriesData copies the new Dataimage to the PlotImage
end;


procedure TPlotSeries.SetXAxis(const Value: Integer);
begin inherited OwnerAxis := Value; end;

function TPlotSeries.PlotImage: TImage;
begin
  Result:=inherited PlotImage;   
end;

constructor TPlotSeries.Create(AOwnerPlot: TPlot);
begin
  inherited Create(AOwnerPlot);
  FDataChanged:=false;
  FTransParency:=$FFFF;       // $FFFF is solid, $0 is transparent
  FSeriestype := stPLAIN;
  FColoredAxis:=-1;
  FCaption := 'A very interesting Series';
  FUnits := TStringList.Create;
  AddUnit('x', 'unit not set');
  AddUnit('y', 'unit not set');
  AddUnit('z', 'unit not set');
  FLineEndPoint.X:=0; FLineEndPoint.Y := 0;
end;

destructor TPlotSeries.Destroy;
begin
  FUnits.Free;
  IF FMarkerContainer <> nil THEN FMarkerContainer.Free;
  IF FInterpolator <> nil THEN FInterpolator.Free;
  inherited Destroy;
end;

procedure TPlotSeries.DoPlotToImage;
begin
  raise EPlot.CreateRes(@S_NoFastSeries);
end;

procedure TPlotSeries.UpdateMarkers(AContainerIndex: Integer);
begin
  // we notify the plotrect for the update request
  IF IsFastSeries THEN
    TPlotRect(TPlotAxis(OwnerPlot.Axis[XAxis]).OwnerPlotRect).UpdateSeriesData(Self, true, false)
  ELSE begin
    // we do a direct marker update here
    OwnerPlot.Repaint;
    MarkerContainer.DrawMarkers(PlotImage.Picture.Bitmap, true);
  end;
end;

function TPlotSeries.GetLineEndpoint(out ALineEndPoint: TPoint): Integer;
begin
  ALineEndPoint := FLineEndPoint;
  Result := 0;
end;

procedure TPlotSeries.SetYAxis(Value: Integer);
begin
  IF FYAxis = Value THEN exit;
  FYAxis := Value;
  //OwnerPlot.Repaint;
end;

procedure TPlotSeries.SetColoredAxis(const AValue: Integer);
begin
  if FColoredAxis=AValue then exit;
  FColoredAxis:=AValue;
end;

procedure TPlotSeries.SetInterpolate(AValue: Boolean);
begin
  IF FInterpolate = AValue THEN exit;
  FInterpolate := AValue;
end;

{
function TPlotSeries.GetDrawTarget: TDrawTarget;
begin
  Result.drwCanvas := PlotImage.Canvas;
  Result.drwRect := TPlotAxisBase(OwnerPlot.Axis[OwnerAxis]).OwnerPlotRect.ClientRect;
end; }

function TPlotSeries.GetColoredAxis: Integer;
begin
  Result := FColoredAxis;
end;

function TPlotSeries.GetInterpolate: Boolean;
begin
  Result := FInterpolate;
end;

function TPlotSeries.GetInterpolator: TInterpolator;
begin
  Result := FInterpolator;
end;

function TPlotSeries.GetMarkerContainer: TMarkerContainer;
begin
  //TODO: always Create to speed up?
  IF FMarkerContainer = nil THEN FMarkerContainer := TMarkerContainer.Create(self);
  Result := FMarkerContainer;
end;

procedure TPlotSeries.SetUnits(const AValue: TStrings);
begin
  FUnits := AValue;
end;



{ TXYPlotSeries }
// ***************************************************************************
{
General 2D plotseries
- draws X/Y points directly to the PlotImage
- stores drawn points (display is refreashed on Redraw)
}

procedure TXYPlotSeries.SetMaxStoredItems(const AValue: Integer);
begin
  if FMaxStoredItems=AValue then exit;
  if AValue < FMaxStoredItems then Clear;
  FMaxStoredItems:=AValue;
end;

function TXYPlotSeries.GetNumStoredItems: Integer;
begin
  IF FItems = nil THEN Result := 0 ELSE
  Result := FItems.Count;
end;

{
function TXYPlotSeries.GetItemPtr(AIndex: Integer): Pointer;
begin
  try
    Result := FItems.Items[AIndex];
  finally
  end;
end;
}

procedure TXYPlotSeries.Redraw;
var
  vXYLine: TXYLine;  // for marker update
  vLoop: Integer;
  vXYPixelLine: TXYPixelLine;
  vCount: Integer;
begin
  IF FItems = nil THEN exit;
  BEGIN  // sorting required for linestyle
    setlength(vXYLine, NumStoredItems);
    for vLoop := 0 to NumStoredItems-1 do begin
      vXYLine[vLoop] := TXYValue(FItems.Items[vLoop]^);
    end;
    IF MarkerContainer.MarkerCount > 0 then begin
      QuickSort_XYLine(vXYLine, 0, length(vXYLine)-1);     // we always sort the data (i.e. we have no property 'IsSorted')
      MarkerContainer.ReNewData(vXYLine, 0, false);
    end;

    IF (FInterpolator <> nil) and Interpolate THEN begin   // TODO: actually no line style with interpolation
      vCount := TInterpolator_PixelXY_PlotRect(Interpolator).Interpolate(vXYPixelLine, vXYLine);
      for vLoop:=0 to vCount-1 do begin
        if Style is TSeriesStyleLines then FLineEndPoint := vXYPixelLine[vLoop].Pt;
        IF ColoredAxis = XAxis THEN DrawPoint(vXYPixelLine[vLoop].Pt, PlotImage.Canvas, FPColorToTColor(vXYPixelLine[vLoop].FPColor))
        ELSE IF ColoredAxis = YAxis THEN DrawPoint(vXYPixelLine[vLoop].Pt, PlotImage.Canvas, FPColorToTColor(vXYPixelLine[vLoop].FPColor))
        ELSE DrawPoint(vXYPixelLine[vLoop].Pt, PlotImage.Canvas);
      end;
    end ELSE
      for vLoop:=0 to length(vXYLine)-1 do DrawXY(vXYLine[vLoop].X, vXYLine[vLoop].Y);
  END;
  setlength(vXYLine, 0);
  setlength(vXYPixelLine, 0);
end;

function TXYPlotSeries.GetValueRange(AAxisIndex: Integer): TValueRange;
var vLoop: integer; vItem: PXYValue; vXmin, vXmax, vYmin, vYmax: Extended;
begin
  Result.min := c_GLOBALMAX;
  Result.max := -c_GLOBALMAX;
  IF (AAxisIndex <> XAxis) AND (AAxisIndex <> YAxis) THEN exit;
  IF FItems = nil THEN exit;

  vXmin := c_GLOBALMAX;
  vXmax := -c_GLOBALMAX;
  vYmin := vXmin; vYmax := vXmax;
  FOR vLoop := 0 TO FItems.Count-1 do
  begin
    vItem := FItems.Items[vLoop];
    if IsNan(vItem^.X) or IsNan(vItem^.Y) or IsInfinite(vItem^.X) or IsInfinite(vItem^.Y) then continue;
    vXmin := math.Min(vXmin, vItem^.X);
    vXmax := math.Max(vXmax, vItem^.X);
    vYmin := math.Min(vYmin, vItem^.Y);
    vYmax := math.Max(vYmax, vItem^.Y);
  end;

  IF AAxisIndex = XAxis THEN BEGIN
    Result.min := vXmin;
    Result.max := vXmax;
  END;
  IF AAxisIndex = YAxis THEN BEGIN
    Result.min := vYmin;
    Result.max := vYmax;
  END;
end;

function TXYPlotSeries.GetAutoScaleRange(AAxisIndex: Integer): TValueRange;
var
  vValueRange : TValueRange;
  vDigits: Integer;
  vValueSpan: Extended;
begin
  vValueRange := GetValueRange(AAxisIndex);

  IF vValueRange.max < vValueRange.min THEN begin
    Result.min := -1;
    Result.max := 1;
    IF TPlotAxis(OwnerPlot.Axis[AAxisIndex]).LogScale THEN Result.min := 0.1;
    exit;
  end;

  vValueSpan := abs(vValueRange.max - vValueRange.min);
  IF vValueSpan < c_GLOBALMIN THEN vValueSpan := c_GLOBALMIN;
  IF vValueSpan > c_GLOBALMAX THEN vValueSpan := c_GLOBALMAX;        // 03.05.14
  vDigits :=  trunc( logn(TPlotAxis(OwnerPlot.Axis[AAxisIndex]).LogBase, vValueSpan )) - 1;

  CASE AutoScaleMode of
    asFit:
      begin
          Result.min := vValueRange.min;
          Result.max := vValueRange.max;
      end;
    asFit125:           begin
          Result.min := ValueTo125(vValueRange.min, FALSE, vDigits);
          Result.max := ValueTo125(vValueRange.max, TRUE, vDigits);
                        end;
    asFitNext:          begin
          Result.min := ValueToNext(vValueRange.min, FALSE, vDigits);
          Result.max := ValueToNext(vValueRange.max, TRUE, vDigits);
                        end;
    asFitNextMargined:  begin
          Result.min := ValueToNext(vValueRange.min, FALSE, vDigits);
          IF ( abs(Result.min - vValueRange.min) / vValueSpan < c_FITMARGIN) and (abs(Result.min) > c_GLOBALMIN) THEN
            Result.min := ValueToNext(Result.min - (vValueSpan * c_FITMARGIN), FALSE, vDigits);
          Result.max := ValueToNext(vValueRange.max, TRUE, vDigits);
          IF ( abs(Result.max - vValueRange.max) / vValueSpan < c_FITMARGIN) and (abs(Result.max) > c_GLOBALMIN)  THEN
            Result.max := ValueToNext(Result.max + (vValueSpan * c_FITMARGIN), TRUE, vDigits);
                        end;
  end;

  IF Result.max <= Result.min THEN Result.max := Result.min + c_GLOBALMIN;

  //TODO: take first non negative point --> get first pos value ?
  IF TPlotAxis(OwnerPlot.Axis[AAxisIndex]).LogScale THEN BEGIN
    IF Result. min <= 0 THEN begin
      IF ValueRange[AAxisIndex].min > 0 THEN Result.min := ValueRange[AAxisIndex].min
      ELSE Result.min := 1; // TODO: give first point > 0 as default
    end;
    IF Result.max <= Result.min THEN Result.max := Result.min + c_GLOBALMIN;
  END;
end;

function TXYPlotSeries.IsInViewRange(XValue, YValue: Extended): Boolean;
begin
  IF (XValue < TPlotAxisBase(OwnerPlot.Axis[XAxis]).ViewRange.min) OR
     (XValue > TPlotAxisBase(OwnerPlot.Axis[XAxis]).ViewRange.max) OR
     (YValue < TPlotAxisBase(OwnerPlot.Axis[YAxis]).ViewRange.min) OR
     (YValue > TPlotAxisBase(OwnerPlot.Axis[YAxis]).ViewRange.max) THEN Result := FALSE
     ELSE Result := TRUE;
end;

function TXYPlotSeries.CheckRefresh(var ANeedsRefresh: Boolean;
  var ANeedsClear: Boolean): Integer;
begin
  Result:=inherited CheckRefresh(ANeedsRefresh, ANeedsClear);
  if FDataChanged then begin
    ANeedsRefresh:=true;
    FDataChanged:=false;
  end;
end;

constructor TXYPlotSeries.Create(AOwnerPlot: TPlot);
begin
  inherited Create(AOwnerPlot);
  FSeriestype := stXY;
  FMaxStoredItems := 4096000;
  FInterpolator := TInterpolator_PixelXY_PlotRect.Create(Self);
  TInterpolator_PixelXY_PlotRect(FInterpolator).CalcAxes := [caX, caY];
  TInterpolator_PixelXY_PlotRect(FInterpolator).IpolAxisMode := iamXY; // imXonly;
  TInterpolator_PixelXY_PlotRect(FInterpolator).IpolMode:=imLinear;
  FInterpolate:=false;
end;

destructor TXYPlotSeries.Destroy;
begin
  Clear;
  IF FItems <> nil THEN
  begin
    FItems.Free;
    FItems := nil;
  end;
  inherited Destroy;
end;

procedure TXYPlotSeries.AddValue(X, Y: Extended);
var
  vItem: PXYValue;
begin
  New(vItem);
  vItem^.X := Extended(X);
  vItem^.Y := Extended(Y);
  IF FItems = nil THEN FItems := TFPList.Create;
  FItems.Add(vItem);

  FDataChanged:=true;
  Redraw;
end;

function TXYPlotSeries.ExportSeriesData(AFileName: TFilename): Integer;
var
  vExporter: TPlot_ResultWriterMAAResult;
  vMeasResult: TMeasResult;
  vLoop: Integer;
begin
  vExporter := TPlot_ResultWriterMAAResult.Create;
  try
    New(vMeasResult.lpMeasData);
    try
      SetLength(vMeasResult.lpMeasData^, FItems.Count, 2);
      for vLoop := 0 to FItems.Count-1 do begin
        vMeasResult.lpMeasData^[vLoop, 0] := TXYValue(FItems.Items[vLoop]^).X;
        vMeasResult.lpMeasData^[vLoop, 1] := TXYValue(FItems.Items[vLoop]^).Y;
      end;

      vMeasResult.dwResultIdent.diPoints := FItems.Count;
      vMeasResult.dwResultIdent.diDimensions := 2;

      vExporter.AddResultRecord(vMeasResult);
      vExporter.WriteToFile(AFileName);

    finally
      SetLength(vMeasResult.lpMeasData^, 0, 0);
      Dispose(vMeasResult.lpMeasData);
    end;
  finally
    vExporter.Free;
  end;
  Result := FItems.Count;
end;

function TXYPlotSeries.ImportSeriesData(AFileName: TFileName): Integer;
var
  vImporter: TPlot_ResultReaderMAAResult;
  vLoop, vLoopPoints, vResultCount: Integer;
  vMeasResult: TMeasResult;
begin
  vImporter := TPlot_ResultReaderMAAResult.Create;
  New(vMeasResult.lpMeasData);
  try
    vImporter.LoadFromFile(AFileName);
    vResultCount := vImporter.NumResults;
    for vLoop := 0 to vResultCount-1 do begin // normally 1 on XY series !
        vImporter.GetResultRecord(vLoop, vMeasResult); // TODO: multitrace !
        IF (vMeasResult.dwResultIdent.diPoints < 1) OR (vMeasResult.dwResultIdent.diDimensions < 2) then exit;
        for vLoopPoints := 0 to vMeasResult.dwResultIdent.diPoints-1 do begin
          Self.AddValue(vMeasResult.lpMeasData^[vLoopPoints, 0], vMeasResult.lpMeasData^[vLoopPoints, 1]);
        end;
    end;
  finally
    setlength(vMeasResult.lpMeasData^, 0, 0);
    Dispose(vMeasResult.lpMeasData);
    vImporter.Free;
  end;
  Result := vResultCount;
end;

procedure TXYPlotSeries.Clear;
begin
  IF FItems = nil THEN exit;
  while FItems.Count > 0 do
  begin
    Dispose(PXYValue(FItems.Items[0]));
    FItems.Delete(0);
  end;
end;


{ TXYZPlotSeries }
{
General 3D plotseries
- draws X/Y/Z points directly to the PlotImage
- stores drawn points (display is refreshed on Redraw)
}

procedure TXYZPlotSeries.DrawXY(X, Y, Z: Extended);
var
  vPt: TPoint;
  vError : Integer;
begin
  vError := uPlotUtils.XYZToScreen(uPlotClass.TPlotSeriesBase(Self).OwnerPlot.Axis[XAxis], uPlotClass.TPlotSeriesBase(Self).OwnerPlot.Axis[YAxis],
                         uPlotClass.TPlotSeriesBase(Self).OwnerPlot.Axis[ZAxis], X,Y,Z,vPt);
  if Style is TSeriesStyleLines then begin
    vError := uPlotUtils.XYZToScreen(uPlotClass.TPlotSeriesBase(Self).OwnerPlot.Axis[XAxis], uPlotClass.TPlotSeriesBase(Self).OwnerPlot.Axis[YAxis],
                       uPlotClass.TPlotSeriesBase(Self).OwnerPlot.Axis[ZAxis], X,0,Z,FLineEndPoint);
  end;

  IF vError < 0 THEN exit;

  IF ColoredAxis = XAxis THEN DrawPoint(vPt, PlotImage.Canvas, uPlotAxis.TPlotAxis(OwnerPlot.Axis[XAxis]).ValueColor[X])
  ELSE IF ColoredAxis = YAxis THEN DrawPoint(vPt, PlotImage.Canvas, uPlotAxis.TPlotAxis(OwnerPlot.Axis[YAxis]).ValueColor[Y])
  ELSE IF ColoredAxis = ZAxis THEN DrawPoint(vPt, PlotImage.Canvas, uPlotAxis.TPlotAxis(OwnerPlot.Axis[ZAxis]).ValueColor[Z])
  ELSE DrawPoint(vPt, PlotImage.Canvas);
end;


procedure TXYZPlotSeries.SetZAxis(const AValue: Integer);
begin
  if FZAxis=AValue then exit;
  FZAxis:=AValue;
end;


procedure TXYZPlotSeries.Redraw;
var vLoop: Integer; vItem: PXYZValue;
begin
  FLastItem:=-1;
  IF FItems = nil THEN exit;
  FOR vLoop := 0 TO FItems.Count-1 do
  begin
    vItem := FItems.Items[vLoop];
    DrawXY(vItem^.X, vItem^.Y, vItem^.Z);
    FLastItem:=vLoop;
  end;
end;

function TXYZPlotSeries.GetValueRange(AAxisIndex: Integer): TValueRange;
var vLoop: integer; vItem: PXYZValue; vXmin, vXmax, vYmin, vYmax, vZmin, vZmax: Extended;
begin
  Result.min := c_GLOBALMAX;
  Result.max := -c_GLOBALMAX;
  IF (AAxisIndex <> XAxis) AND (AAxisIndex <> YAxis) AND (AAxisIndex <> ZAxis) THEN exit;
  IF FItems = nil THEN exit;

  vXmin := c_GLOBALMAX;
  vXmax := -c_GLOBALMAX;
  vYmin := vXmin; vYmax := vXmax;
  vZmin := vXmin; vZmax := vXmax;
  FOR vLoop := 0 TO FItems.Count-1 do
  begin
    vItem := FItems.Items[vLoop];
    vXmin := math.Min(vXmin, vItem^.X);
    vXmax := math.Max(vXmax, vItem^.X);
    vYmin := math.Min(vYmin, vItem^.Y);
    vYmax := math.Max(vYmax, vItem^.Y);
    vZmin := math.Min(vZmin, vItem^.Z);
    vZmax := math.Max(vZmax, vItem^.Z);
  end;

  IF AAxisIndex = XAxis THEN BEGIN
    Result.min := vXmin;
    Result.max := vXmax;
  END;
  IF AAxisIndex = YAxis THEN BEGIN
    Result.min := vYmin;
    Result.max := vYmax;
  END;
  IF AAxisIndex = ZAxis THEN BEGIN
    Result.min := vZmin;
    Result.max := vZmax;
  END;
end;


function TXYZPlotSeries.GetAxesUsed: TList;
var
  vItem : PInteger;
begin
  Result := TList.Create;
  try
    new(vItem);
    try
      vItem^ := XAxis;
      IF (XAxis <> -1) THEN Result.Add(vItem) ELSE Dispose(vItem);
    except
       Dispose(vItem); raise;
    end;

    new(vItem);
    try
      vItem^ := YAxis;
      IF (YAxis <> -1) THEN Result.Add(vItem) ELSE Dispose(vItem);
    except
       Dispose(vItem); raise;
    end;

    new(vItem);
    try
      vItem^ := ZAxis;
      IF (ZAxis <> -1) THEN Result.Add(vItem) ELSE Dispose(vItem);
    except
       Dispose(vItem); raise;
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TXYZPlotSeries.GetUnitString(AAxisIndex: Integer): ShortString;
begin
  IF AAxisIndex = ZAxis THEN BEGIN
    Result := Units.Strings[2];
    exit;
  END;
  Result:=inherited GetUnitString(AAxisIndex);
end;

function TXYZPlotSeries.IsInViewRange(XValue, YValue, ZValue: Extended
  ): Boolean;
begin
  IF (XValue < TPlotAxisBase(OwnerPlot.Axis[XAxis]).ViewRange.min) OR
     (XValue > TPlotAxisBase(OwnerPlot.Axis[XAxis]).ViewRange.max) OR
     (YValue < TPlotAxisBase(OwnerPlot.Axis[YAxis]).ViewRange.min) OR
     (YValue > TPlotAxisBase(OwnerPlot.Axis[YAxis]).ViewRange.max) OR
     (ZValue < TPlotAxisBase(OwnerPlot.Axis[ZAxis]).ViewRange.min) OR
     (ZValue > TPlotAxisBase(OwnerPlot.Axis[ZAxis]).ViewRange.max) THEN Result := FALSE
     ELSE Result := TRUE;
end;

constructor TXYZPlotSeries.Create(AOwnerPlot: TPlot);
begin
  inherited Create(AOwnerPlot);
  FSeriestype := stXYZ;
  FInterpolator.Free; // no interpolation for XYZ at the  moment
  FInterpolator := nil;
end;

destructor TXYZPlotSeries.Destroy;
begin
  Clear;
  IF FItems <> nil THEN
  begin
    FItems.Free;
    FItems := nil;
  end;       // TODO: is this inherited from TXYPlotSeries ?
  inherited Destroy;
end;

procedure TXYZPlotSeries.AddValue(X, Y, Z: Extended);
var vItem: PXYZValue;
begin
  New(vItem);
  vItem^.X := X;
  vItem^.Y := Y;
  vItem^.Z := Z;
  IF FItems = nil THEN FItems := TFPList.Create;
  FItems.Add(vItem);
  FLastItem := FItems.Count-2;

  DrawXY(X, Y, Z);

  FDataChanged := true;
end;

procedure TXYZPlotSeries.Clear;
begin
  IF FItems = nil THEN exit;
  while FItems.Count > 0 do
  begin
    Dispose(PXYZValue(FItems.Items[0]));
    FItems.Delete(0);
  end;
end;

{ TXYWFPlotSeries }
// ***************************************************************************
{
2D waterfall plotseries
- draws X/Y/Z points to a memory (TLazIntf-)image.
- Y axis is colored, X and Z are drawn in cartesian coordinates
- complete datalines are added (TXYZLine) and the display is shifted for each new line
- MaxStoredItems lines are stored and respected during redraw (i.e. after sizing)
}

procedure TXYWFPlotSeries.SetLineWidth(const AValue: Extended);
begin
  if FLineWidth=AValue then exit;
  FLineWidth:=AValue;
  TPlotAxis(OwnerPlot.Axis[ZAxis]).ValuePerPixel := TimePerLine / FLineWidth;
  OwnerPlot.Repaint;
end;

procedure TXYWFPlotSeries.SetActualBaseLine(AValue: Cardinal);
begin
  if FActualBaseLine=AValue then exit;
  FActualBaseLine:=AValue;
end;

function TXYWFPlotSeries.GetColoredAxis: Integer;
begin
  //Result:=inherited GetColoredAxis;   // actually fixed axis use
  Result := FYAxis;
end;

procedure TXYWFPlotSeries.SetTimePerLine(const AValue: Extended);
begin
  if FTimePerLine=AValue then exit;
  FTimePerLine:=AValue;
  //  calc time per pixelline for scale
  IF ZAxis = -1 THEN exit;   // scale only when ZAxis is already assigned !
  TPlotAxis(OwnerPlot.Axis[ZAxis]).ValuePerPixel := TimePerLine / FLineWidth;
    //TPlotAxis(OwnerPlot.Axis[ZAxis]).AxisMode:=amPixelPerValue;
  OwnerPlot.Repaint;
end;

procedure TXYWFPlotSeries.SetYThreshold(AValue: Extended);
begin
  if FYThreshold=AValue then Exit;
  FYThreshold:=AValue;
end;

procedure TXYWFPlotSeries.SetZAxis(const AValue: Integer);
begin
  inherited SetZAxis(AValue);
  uPlotAxis.TPlotAxis(OwnerPlot.Axis[ZAxis]).ValuePerPixel := TimePerLine;
end;


procedure TXYWFPlotSeries.DrawLine(ALine: TXYLine);
var
  vLoop: Integer;
  vPt : TPoint;
  vMathPt: TFloatPoint;
  vError: Integer;
  //
  vByteCount: PtrUInt;
  vLastX: Integer; // remember last X to only draw hottest color !
  vLastY: Extended;
  vDoDraw: Boolean;
  vFPColor: TFPColor;
  vMainIntervalZ: Extended; // vMainIntervalX
  vTickZ: Boolean;
  vLoopLineWidth: Integer;
  vNewLines: Integer;
  vCount: Integer;
  vXYPixelLine: TXYPixelLine;
//const
//  c_TickAlphaFactor = 1.41;
// TODO: see code of 3DWF if can be unified
// TODO: 2D waterfall works only for rectangles, i.e. 90° axis

// fast series (WF2D) as follows:
// Plotrect has a DataImage (TLazIntFImage) with the correct size (created at plotrect.redraw)
// Series (WF2D) draws on this DataImage
// Series then calls PlotRect.UpdateSeriesData
// PlotRect draws clientrect on screen.
// TODO: 04.09.14 replot line regarding to linewidth also in 3D
// TODO: respect Y AND X shifting for non rectangualr axes, see code in WF3D
begin
  FLineWidth:=TPlotAxis(OwnerPlot.Axis[ZAxis]).PixelsPerValue * TimePerLine;    // check for new axis scaling here, no other notification possible ?
  //writeln('pixels per value: ', FloatToStrF(TPlotAxis(OwnerPlot.Axis[ZAxis]).PixelsPerValue, ffExponent, 5, 5));
  //writeln('FLineWidth: ', FloatToStrF(FLineWidth, ffExponent, 5, 5));

  // rolling ticks
  vTickZ:=false;
  vMainIntervalZ := MainTickInterval(TPlotAxis(OwnerPlot.Axis[ZAxis]).ViewRange, 1, false);
  // elapsed time
  FElapsedTime:=FElapsedTime+TimePerLine;
  IF FElapsedTime > vMainIntervalZ THEN begin
    FElapsedTime:=FElapsedTime-vMainIntervalZ;
    vTickZ:=true;
  end;
  // Y lineshift
  FYAct:=FYAct+LineWidth;
  vNewLines:=trunc(FYAct / 1);  // thinks in pixels, shift n pixels
  FYAct:=FYAct-vNewLines;

  vByteCount:=0;
  vLastX := c_INVALIDCOORDINATE;
  vLastY := -c_GLOBALMAX;

  FLastIndex:=-1;

  vByteCount := TPlotRect(TPlotAxis(OwnerPlot.Axis[XAxis]).OwnerPlotRect).DataImage.DataDescription.BytesPerLine;
  // clear line 0
  FillByte((TPlotRect(TPlotAxis(OwnerPlot.Axis[XAxis]).OwnerPlotRect).DataImage.GetDataLineStart(0)^), vByteCount, 0);

  // draw line
  IF (Interpolate and (FInterpolator <> nil)) THEN BEGIN
    vCount := TInterpolator_PixelXY(FInterpolator).Interpolate(vXYPixelLine, ALine);
    for vLoop := 0 to vCount-1 do begin
      if Style is TSeriesStyleLines then FLineEndPoint := vXYPixelLine[vLoop].Pt; // vPt;
      if FMarkNext then DrawPoint(vXYPixelLine[vLoop].Pt, TPlotRect(TPlotAxis(OwnerPlot.Axis[XAxis]).OwnerPlotRect).DataImage, TColorToFPColor(clRed), false, false)
      else DrawPoint(vXYPixelLine[vLoop].Pt, TPlotRect(TPlotAxis(OwnerPlot.Axis[XAxis]).OwnerPlotRect).DataImage, vXYPixelLine[vLoop].FPColor, false, false);
    end;
  END ELSE
  FOR vLoop := 0 to Length(ALine)-1 DO BEGIN
    // solution 1: valuetomathcoord
    vError := uPlotUtils.ValueToMathCoord(OwnerPlot.Axis[XAxis], ALine[vLoop].X, vMathPt);
    // solution 2: xyto screen ??
    IF vError < 0 THEN BEGIN
                         FLastIndex:=vLoop;
                         continue;
                       END;
    vPt.X := round(vMathPt.X);
    // when filling (see above), always use Y=0
    // when drawing points with diameter > 1 on Y > 0 we get wrong colors when shifting
    vPt.Y := 0;

    vDoDraw:=false;
    IF vLastX <> vPt.X THEN begin
        vLastX := vPt.X;
        vLastY := ALine[vLoop].Y;
        vDoDraw:=TRUE;
      END
      ELSE begin
        if ALine[vLoop].Y > vLastY then begin
          vLastY := ALine[vLoop].Y;
          vDoDraw :=TRUE;
        end;
    end;
    //only draw hottest point on a single x value
    IF vDoDraw THEN BEGIN
      if Style is TSeriesStyleLines then FLineEndPoint := vPt;
      vFPColor := uPlotAxis.TPlotAxis(OwnerPlot.Axis[YAxis]).ValueFPColor[ALine[vLoop].Y];
      IF ALine[vLoop].Y > FYThreshold THEN vFPColor.alpha := TransParency ELSE vFPColor.alpha := 0; //  $7FFF;
      IF FMarkNext THEN begin
         vFPColor.alpha := 255-TransParency;
      end;

      // seamless grid - not implemented
      //IF vTickZ THEN begin
      //  IF vFPColor.alpha > $7FFF then vFPColor.alpha := round(vFPColor.alpha / c_TickAlphaFactor) else vFPColor.alpha:= round(vFPColor.alpha * c_TickAlphaFactor);
      //  vFPColor := TColorToFPColor(clRed);
      //end;
      DrawPoint(vPt, TPlotRect(TPlotAxis(OwnerPlot.Axis[XAxis]).OwnerPlotRect).DataImage, vFPColor, false, true);  // false for blending, true for alphamergeonly
    END;
  END;
  IF FMarkNext THEN FMarkNext := false;


  for vLoopLineWidth := 0 to vNewLines-1 do begin  // no shift if no new line
    FOR vLoop := TPlotRect(TPlotAxis(OwnerPlot.Axis[XAxis]).OwnerPlotRect).DataImage.Height - 2 downto 0 do begin
      Move( TPlotRect(TPlotAxis(OwnerPlot.Axis[XAxis]).OwnerPlotRect).DataImage.GetDataLineStart(vLoop)^, TPlotRect(TPlotAxis(OwnerPlot.Axis[XAxis]).OwnerPlotRect).DataImage.GetDataLineStart(vLoop+1)^, vByteCount);
    END;
  end;

  FLastIndex:=vLoop;
  ActualBaseLine:= ActualBaseLine + 1;
end;

procedure TXYWFPlotSeries.Redraw;
var
  vLoop : Integer;
  vItem: PXYLine;
// TODO: see code of 3DWF if can be unified
begin
   FYAct:=0;

  IF ( (XAxis < 0) OR (ZAxis <0) ) THEN exit;
  // make shure that all axes are already painted on canvas !

  // fill in stored data
  IF NumStoredItems < 1 THEN exit;
  ActualBaseLine:=1;
  IF round(uPlotAxis.TPlotAxis(OwnerPlot.Axis[ZAxis]).DrawLength) < NumStoredItems
    THEN ActualBaseLine := NumStoredItems - round(uPlotAxis.TPlotAxis(OwnerPlot.Axis[ZAxis]).DrawLength);

  FOR vLoop := ActualBaseLine TO NumStoredItems-1 DO BEGIN
    vItem := FItems[vLoop-1];
    DrawLine(vItem^);
  END;

  FDataChanged:=true;
  TPlotRect(TPlotAxis(OwnerPlot.Axis[XAxis]).OwnerPlotRect).UpdateSeriesData(Self, false, true);
end;


function TXYWFPlotSeries.GetValueRange(AAxisIndex: Integer): TValueRange;
var
  vLoop, vItemLoop: Integer;
  vPItem: PXYLine;
  vItem: TXYLine;
  vXmin, vXmax: Extended;
  vYmin, vYmax: Extended;
begin
  Result.min := c_GLOBALMAX;
  Result.max := -c_GLOBALMAX;
  //Result:=inherited GetValueRange(AAxisIndex); // can we inherit this ?
  IF (AAxisIndex <> XAxis) AND (AAxisIndex <> YAxis) AND (AAxisIndex <> ZAxis) THEN exit;
  IF FItems = nil THEN exit;

  vXmin := c_GLOBALMAX;
  vXmax := -c_GLOBALMAX;
  vYmin := vXmin; vYmax := vXmax;

  FOR vItemLoop := 0 TO NumStoredItems-1 DO BEGIN
    vPItem := FItems[vItemLoop];
    vItem := vPItem^;

    FOR vLoop := 0 TO length(vItem)-1 do
    begin
      vXmin := math.Min(vXmin, vItem[vLoop].X);
      vXmax := math.Max(vXmax, vItem[vLoop].X);
      vYmin := math.Min(vYmin, vItem[vLoop].Y);
      vYmax := math.Max(vYmax, vItem[vLoop].Y);
    end;

    IF AAxisIndex = XAxis THEN BEGIN
      Result.min := vXmin;
      Result.max := vXmax;
      //writeln('valuerage X: ', FloatToStrF(Result.min, ffExponent, 5, 5), ' .. ',FloatToStrF(Result.max, ffExponent, 5, 5));
    END;
    IF AAxisIndex = YAxis THEN BEGIN
      Result.min := vYmin;
      Result.max := vYmax;
    END;
    IF AAxisIndex = ZAxis THEN BEGIN
      Result.min := 0;
      Result.max := TimePerLine * math.max(1, MaxStoredItems);  //math.Max(1, FItems.Count);          // 10.08.14 / FLineWidth
    END;
  END;
end;

function TXYWFPlotSeries.GetAxesUsed: TList;
begin
  Result:=inherited GetAxesUsed;
end;

function TXYWFPlotSeries.ExportSeriesData(AFileName: TFilename): Integer;
var
  vExporter: TPlot_ResultWriterMAAResult;
  vMeasResult: TMeasResult;
  vLoop, vLoopResults: Integer;
{ returns number of written results (one result = one resultrecord with n points, m dimensions }
begin
  vExporter := TPlot_ResultWriterMAAResult.Create;
  try
    for vLoopResults := 0 to NumStoredItems-1 do begin

      New(vMeasResult.lpMeasData);
      try
        vMeasResult.dwResultIdent.diPoints := length(TXYLine(FItems.Items[vLoopResults]^));
        vMeasResult.dwResultIdent.diDimensions := 2;

        SetLength(vMeasResult.lpMeasData^, vMeasResult.dwResultIdent.diPoints, vMeasResult.dwResultIdent.diDimensions);
        for vLoop := 0 to vMeasResult.dwResultIdent.diPoints-1 do begin
          vMeasResult.lpMeasData^[vLoop, 0] := TXYLine(FItems.Items[vLoopResults]^)[vLoop].X;
          vMeasResult.lpMeasData^[vLoop, 1] := TXYLine(FItems.Items[vLoopResults]^)[vLoop].Y;
        end;

        vExporter.AddResultRecord(vMeasResult);
      finally
        SetLength(vMeasResult.lpMeasData^, 0, 0);
        Dispose(vMeasResult.lpMeasData);
      end;
    end;
    vExporter.WriteToFile(AFileName);
  finally
    vExporter.Free;
  end;
  Result := NumStoredItems;
end;

function TXYWFPlotSeries.ImportSeriesData(AFileName: TFileName): Integer;
var
  vImporter: TPlot_ResultReaderMAAResult;
  vLoop, vLoopPoints, vResultCount, vLoopResults: Integer; // vPointCount
  vMeasResult: TMeasResult;
  vXYLine: TXYLine;
{ returns number of results read (one result = one resultrecord with n points, m dimensions }
begin
  vImporter := TPlot_ResultReaderMAAResult.Create;
  try
    vImporter.LoadFromFile(AFileName);
    vResultCount := vImporter.NumResults;
    for vLoopResults := 0 to vResultCount-1 do begin
      New(vMeasResult.lpMeasData);
      try
        for vLoop := 0 to vResultCount-1 do begin // normally 1 on XY series !
            vImporter.GetResultRecord(vLoop, vMeasResult); // TODO: multitrace !
            IF (vMeasResult.dwResultIdent.diPoints < 1) OR (vMeasResult.dwResultIdent.diDimensions < 2) then exit;
            setlength(vXYLine, vMeasResult.dwResultIdent.diPoints);
            for vLoopPoints := 0 to vMeasResult.dwResultIdent.diPoints-1 do begin
              vXYLine[vLoopPoints].X := vMeasResult.lpMeasData^[vLoopPoints, 0];
              vXYLine[vLoopPoints].Y := vMeasResult.lpMeasData^[vLoopPoints, 1];
            end;
            Self.AddLine(vXYLine);
        end;
      finally
        setlength(vXYLine, 0);
        setlength(vMeasResult.lpMeasData^, 0, 0);
        Dispose(vMeasResult.lpMeasData);
      end;
    end;
  finally
    vImporter.Free;
  end;
  Result := vResultCount;
end;


constructor TXYWFPlotSeries.Create(AOwnerPlot: TPlot);
begin
  FYAct:=0;
  FMarkNext := false;
  FElapsedTime:=0;
  inherited Create(AOwnerPlot);
  FSeriestype := stWF2D;
  FIsFastSeries := TRUE;

  FZAxis:=-1;
  TimePerLine := 1;
  FLineWidth := 1;
  ActualBaseLine:=1;

  FLinesQueued := 0;
  FMaxStoredItems := 128;
  FYThreshold := -c_GLOBALMAX;
  // interpolate ?
  FInterpolator := TInterpolator_PixelXY.Create(Self);
  TInterpolator_PixelXY(FInterpolator).CalcAxes := [caX];
  TInterpolator_PixelXY(FInterpolator).IpolAxisMode := iamXonly; // imXonly;
  TInterpolator_PixelXY(FInterpolator).ZeroYcoord := true; // imXonly;
  TInterpolator_PixelXY(FInterpolator).HiddenPixelMode := hpmMax;
  FInterpolate:=false; // TODO: default to false after testing;
end;

destructor TXYWFPlotSeries.Destroy;
begin
  Clear;
  IF FItems <> nil THEN
  begin
    FItems.Free;
    FItems := nil;
  end;       // TODO: is this inherited from TXYPlotSeries ?
  inherited Destroy;
end;

procedure TXYWFPlotSeries.DoPlotToImage;
begin
  IF NumStoredItems < 1 THEN exit;

   // 23.07.14 direct draw to plotimage after adding data
   // so do nothing here when called by plotrect updateseriesdata
   // for waterfalls data is already up to date

   // 15.08.14 HINT:
   // direct plotting to DataImage is only possible for single series plotrects
   // (otherwise additional data is plotted -> series clear and replot must be done at once
   //  for multi series plotrects)
end;

procedure TXYWFPlotSeries.Clear;
var vLine: PXYLine;
begin
  IF FItems = nil THEN exit;
  while FItems.Count > 0 do
  begin
    vLine := FItems.Items[0];
    SetLength(vLine^, 0);
    FreeMem(FItems.Items[0]);
    FItems.Delete(0);
  end;
  ActualBaseLine := 1;
  FLinesQueued := 0;
  inherited Clear;
end;

procedure TXYWFPlotSeries.AddLine(ALine: TXYLine);
var
  vItem: PXYLine;
  //vIndex: Integer;
begin
  IF length(ALine) < 1 then exit;
  New(vItem);
  try
    // copy the data
    Setlength(vItem^, length(ALine));
    Move(ALine[0], vItem^[0], length(ALine)*SizeOf(TXYValue));
    {$IFDEF DEBUG_WRITE}
    IF IsNan(vItem^[0].X) OR (vItem^[0].Y < -128) then begin
      writeln('########## received problematic line no. ', IntToStr(FItems.Count+1));
    end;
    {$ENDIF}

    IF FItems = nil THEN begin
      FItems := TFPList.Create;
    end;

    //vIndex := FItems.Add(vItem);
    FItems.Add(vItem);

    IF FItems.Count > MaxStoredItems THEN BEGIN
      vItem := FItems.Items[0];
      SetLength(vItem^, 0);
      Dispose(vItem);
      FItems.Delete(0);
      IF ActualBaseLine > 0 THEN
        ActualBaseLine := ActualBaseLine - 1;
    END;

    vItem := FItems[NumStoredItems-1];
    DrawLine(vItem^);

    // 19.08.14 testing renew MarkerContainer
    // TODO: MarkerContainer should fetch the actual Data if needed;
    // this works only with unified DataLine types or MAAResult type with handshake !
    MarkerContainer.ReNewData(ALine, 0, false);

  finally
  end;
  FDataChanged := true;
  // does nothing when timed refresh active:
  TPlotRect(uPlotClass.TPlotSeriesBase(Self).OwnerPlot.Axis[XAxis].OwnerPlotRect).UpdateSeriesData(Self, FALSE);
end;

procedure TXYWFPlotSeries.MarkNext;
begin
  FMarkNext := true;
end;

{ TXYWF3DPlotSeries }
// ***************************************************************************
{
3D waterfall plotseries
- draws X/Y/Z points to a memory (TLazIntf-)image.
- Y axis is colored, X, Y and Z are drawn in cartesian coordinates
- complete datalines are added (TXYZLine) and the display is shifted for each new line
  according to LineWidth and DrawAngle of Z axis
- MaxStoredItems lines are stored and respected during redraw (i.e. after sizing)
}

{
function TXYWF3DPlotSeries.GetDrawTarget: TDrawTarget;
begin
  Result := inherited GetDrawTarget;
end;

}
procedure TXYWF3DPlotSeries._UpdateFadeMask;
var
  vFadePoint: TPoint;
  vFadeAngle: Extended;
  xmin, dx, d, dmax: Extended;
  xmax: Integer;
  xact, yact: Integer;
  valpha: word;
  vActualColor: TFPColor;
  vYShift: TPoint;
const
  c_ALPHAMAX = $FFFF;
  c_SPEEDUP = 3; // dmax is maximum distance to last possible point drawn (mostly edge of screen)
begin
  // Mask for whole image
  // applied only before redraw (--> gradient mask)
  // when applied at every drawline shift, use constant mask (slow !)

  // attention: mask is cumulative on running images when used with ApplyMask!!
  // this is the gradient mask, below commented find the constant mask ---------------------------------------
  //writeln('update mask');
  vActualColor.alpha :=$0; // transparent
  vActualColor.red := $0;
  vActualColor.green := $0;
  vActualColor.blue := $0;
  FAlphaMask.FillPixels(vActualColor);
  vFadePoint :=  uPlotAxis.TPlotAxis(OwnerPlot.Axis[ZAxis]).VisualParams.ptB;
  vYShift.X :=  uPlotAxis.TPlotAxis(OwnerPlot.Axis[YAxis]).VisualParams.ptB.X - uPlotAxis.TPlotAxis(OwnerPlot.Axis[YAxis]).VisualParams.ptA.X;
  vYShift.Y :=  uPlotAxis.TPlotAxis(OwnerPlot.Axis[YAxis]).VisualParams.ptB.Y - uPlotAxis.TPlotAxis(OwnerPlot.Axis[YAxis]).VisualParams.ptA.Y;
  vFadePoint.X := vFadePoint.X + vYShift.X;
  vFadePoint.Y := vFadePoint.Y + vYShift.Y;
  vFadeAngle :=  uPlotAxis.TPlotAxis(OwnerPlot.Axis[XAxis]).DrawAngle / 360 * 2* Pi();

  xmax := FAlphaMask.Width;

  IF round(uPlotAxis.TPlotAxis(OwnerPlot.Axis[XAxis]).DrawAngle) MOD 90 = 0 THEN exit; // no tangens for 90°

  yact := 0;
  dx := (vFadePoint.Y - yact) / tan(vFadeAngle);
  xmin := max(0, vFadePoint.X + dx);

  dmax := abs((xmax-xmin) * sin(vFadeAngle)) / c_SPEEDUP;  // fadeout speed here
  // loop from Y=Y0 (top) to bottom until dx=0;

  while xmin < xmax do begin
    dx := (vFadePoint.Y - yact-1) / tan(vFadeAngle);
    //xmin := max(0,vFadePoint.X + dx);
    //for xact := round(xmin) to xmax-1 do begin
    //  d := abs(sin(vFadeAngle) * (xact-xmin));
    xmin := vFadePoint.X + dx;
    for xact := round(max(xmin, 0)) to xmax-1 do begin
      d := abs(sin(vFadeAngle) * (xact-xmin));
      // inverse, see applyalpha
      valpha := $FFFF - EnsureRange(c_ALPHAMAX - round(c_ALPHAMAX * ((d))/((dmax))),low(WORD),high(WORD));

      vActualColor.alpha := valpha;
      vActualColor.red := vAlpha;
      vActualColor.green := $0;
      vActualColor.blue := $0;
      FAlphaMask.Colors[xact, yact] := vActualColor;
    end;
    //writeln('line x/y ',IntToStr(xact), ' / ', IntToStr(yact));
    Inc(yact);
    if yact > FAlphaMask.Height-1 then break;
  end;


  // attention: mask is cumulative on running images when used with ApplyMask!!!
  // this is the constant mask, above find the gradient mask ---------------------------------------
  //writeln('update mask finished'); }
  {
  //writeln('update mask');
  vActualColor.alpha :=$0; // transparent
  vActualColor.red := $0;
  vActualColor.green := $0;
  vActualColor.blue := $0;
  FAlphaMask.FillPixels(vActualColor);
  vFadePoint :=  uPlotAxis.TPlotAxis(OwnerPlot.Axis[ZAxis]).VisualParams.ptB;
  vFadeAngle :=  uPlotAxis.TPlotAxis(OwnerPlot.Axis[XAxis]).DrawAngle / 360 * 2* Pi();

  xmax := FAlphaMask.Width;
  ymax := FAlphaMask.Height;

  IF round(uPlotAxis.TPlotAxis(OwnerPlot.Axis[XAxis]).DrawAngle) MOD 90 = 0 THEN exit; // no tangens for 90°
                                             // TODO: fill maks solid !

  valpha := $F800;  // 0,95 * FFFF == F332 // 0,97 --> F850

  vActualColor.alpha := valpha; // $7FFF;
  vActualColor.red :=  $0; //$FFFF;
  vActualColor.green := $0;
  vActualColor.blue := $0;

  for yact := 0 to ymax-1 do begin
    dx := (vFadePoint.Y - yact-1) / tan(vFadeAngle);
    xmin := max(0, vFadePoint.X + dx);
    for xact := round(xmin) to xmax-1 do begin
       FAlphaMask.Colors[xact, yact] := vActualColor;
    end;
  end;}
end;


procedure TXYWF3DPlotSeries.DrawLine(ALine: TXYLine);
// TODO: inherit this (i.e. with a variable x-shift set to 0 in 2D waterfall)
var
  vLoop, vLoopNewLine: Integer;
  vPt : TPoint;
  //vMathPt: TFloatPoint;
  vError: Integer;
  vByteCount: PtrUInt;
  vBytePerPixel: byte;
  vFPColor: TFPColor;
  vMainIntervalZ: Extended; // vMainIntervalX
  vMainTickZ: Boolean;
  vMainTickColor: TFPColor;
  vNewLines: Integer;
  vNewRows: Integer;
  vNewRowsPerNewLine, vLoopNewRow: Extended;
  vCount: Integer;
  vXYPixelLine: TXYPixelLine;
const
  cZFILL = false; //TRUE; // fill Z gaps (i.e. redraw the same line)
begin
  // TODO: redraw code is 2x redundant - avoid this

  // **************************
  FLineWidth:=TPlotAxis(OwnerPlot.Axis[ZAxis]).PixelsPerValue * TimePerLine;    // check for new axis scaling here, no other notification possible ?
  //writeln('3D FLinewidth / ppv / timeperline: ', FloatToStrF(FLineWidth, ffFixed, 4, 4), ' / ',
  //                                                FloatToStrF( TPlotAxis(OwnerPlot.Axis[ZAxis]).PixelsPerValue, ffFixed, 4, 4), ' / ',
  //                                                 FloatToStrF(TimePerLine, ffFixed, 4, 4) );
  // rolling ticks
  vMainTickZ:=false;
  vMainIntervalZ := MainTickInterval(TPlotAxis(OwnerPlot.Axis[ZAxis]).ViewRange, 1, false);
  vMainTickColor := FPColor($FFFF, $FFFF, $C000, $7FFF);
  // elapsed time
  FElapsedTime:=FElapsedTime+TimePerLine;
  IF FElapsedTime > vMainIntervalZ THEN begin
    FElapsedTime:=FElapsedTime-vMainIntervalZ;
    vMainTickZ:=true;
  end;
  // Y lineshift
  FYAct:= FYAct + LineWidth * sin(TPlotAxis(OwnerPlot.Axis[ZAxis]).DrawAngle/180*Pi());   // TODO: PreCalc for speed!
  vNewLines:= trunc(FYAct / 1);  // thinks in pixels, shift n pixels
  FYAct:= FYAct-vNewLines;

  FXAct:= FXAct + LineWidth * cos(TPlotAxis(OwnerPlot.Axis[ZAxis]).DrawAngle/180*Pi()); // TODO: PreCalc !
  vNewRows:= trunc(FXAct / 1);
  //FXAct:= FXAct-vNewRows; // take actually shifted number
  if vNewLines > 0 then
    vNewRowsPerNewLine := vNewRows/vNewLines
  else vNewRowsPerNewLine:=1;

  FLastIndex:=-1; // use of this?

  // Rolling grid: check for ticklineZ
  //vMainIntervalX := MainTickInterval(TPlotAxis(OwnerPlot.Axis[XAxis]).ViewRange, TPlotAxis(OwnerPlot.Axis[XAxis]).LogBase, TPlotAxis(OwnerPlot.Axis[XAxis]).LogScale);
  vMainIntervalZ := MainTickInterval(TPlotAxis(OwnerPlot.Axis[ZAxis]).ViewRange, 1, false);
  IF FElapsedTime > vMainIntervalZ THEN begin
    FElapsedTime:=FElapsedTime-vMainIntervalZ;
    vMainTickZ:=true;
  end;

  // shift the whole thing up (not down as in 2D)
  vLoopNewRow := 0;
  vByteCount := TPlotRect(TPlotAxis(OwnerPlot.Axis[XAxis]).OwnerPlotRect).DataImage.DataDescription.BytesPerLine; // remove v variable
  vBytePerPixel := TPlotRect(TPlotAxis(OwnerPlot.Axis[XAxis]).OwnerPlotRect).DataImage.DataDescription.BitsPerPixel DIV 8; // remove v variable
  for vLoopNewLine := 0 to vNewLines-1 do begin
    vLoopNewRow := vLoopNewRow + vNewRowsPerNewLine;
    vNewRows := trunc(vLoopNewRow);
    FOR vLoop := 1 to TPlotRect(TPlotAxis(OwnerPlot.Axis[XAxis]).OwnerPlotRect).DataImage.Height - 1 do begin
      // move one up and right in one turn (only 45° actually)
      Move((TPlotRect(TPlotAxis(OwnerPlot.Axis[XAxis]).OwnerPlotRect).DataImage.GetDataLineStart(vLoop))^, (TPlotRect(TPlotAxis(OwnerPlot.Axis[XAxis]).OwnerPlotRect).DataImage.GetDataLineStart(vLoop-1)+ vNewRows * vBytePerPixel)^, vByteCount);
      // fill first pixel, as this is rolled over from right to left side !
      FillByte( (TPlotRect(TPlotAxis(OwnerPlot.Axis[XAxis]).OwnerPlotRect).DataImage.GetDataLineStart(vLoop))^ , vNewRows * vBytePerPixel, 0);
    END;
    // fill last row empty
    FillByte((TPlotRect(TPlotAxis(OwnerPlot.Axis[XAxis]).OwnerPlotRect).DataImage.GetDataLineStart(TPlotRect(TPlotAxis(OwnerPlot.Axis[XAxis]).OwnerPlotRect).DataImage.Height - 1))^, vByteCount, 0);

    // if cZFILL, redraw latest dataline  -----------------------------------------------------
    if cZFILL then begin
    //     // draw new line here, --> separate line drawing from shifting. not now (25.08.14)
      // draw line with interpolator
      IF (Interpolate and (FInterpolator <> nil)) THEN BEGIN
        vCount := TInterpolator_PixelXY(FInterpolator).Interpolate(vXYPixelLine, ALine);
        for vLoop := 0 to vCount-1 do begin
          if Style is TSeriesStyleLines then FLineEndPoint := vXYPixelLine[vLoop].Pt;
          DrawPoint(vXYPixelLine[vLoop].Pt, TPlotRect(TPlotAxis(OwnerPlot.Axis[XAxis]).OwnerPlotRect).DataImage, vXYPixelLine[vLoop].FPColor, false, false);
        end;
      END ELSE
      // or.. draw line without interpolator
      FOR vLoop := 0 to Length(ALine)-1 DO BEGIN
        vError := XYZToDataImage(OwnerPlot.Axis[XAxis], OwnerPlot.Axis[YAxis], nil, ALine[vLoop].X, ALine[vLoop].Y, 0, vPt);
        if Style is TSeriesStyleLines then begin
          vError := XYZToDataImage(OwnerPlot.Axis[XAxis], OwnerPlot.Axis[YAxis], nil, ALine[vLoop].X, OwnerPlot.Axis[YAxis].ViewRange.min, 0, FLineEndPoint);
        end;
        IF vError < 0 THEN BEGIN
                             FLastIndex:=vLoop;
                             continue;
                           END;

        vFPColor := uPlotAxis.TPlotAxis(OwnerPlot.Axis[YAxis]).ValueFPColor[ALine[vLoop].Y];
        IF ALine[vLoop].Y > FYThreshold THEN vFPColor.alpha := TransParency ELSE vFPColor.alpha := 0; //  $7FFF;

        // Rolling grid: for time axis (and frequency ?) --> TODO: make this a property
        //IF vMainTickZ THEN begin
        //  //IF vFPColor.alpha > $7FFF then vFPColor.alpha := (vFPColor.alpha DIV 2) else vFPColor.alpha:=vFPColor.alpha * 2;
        //  vFPColor := vMainTickColor;
        //end;

        //... do the drawing
        DrawPoint(vPt, TPlotRect(TPlotAxis(OwnerPlot.Axis[XAxis]).OwnerPlotRect).DataImage, vFPColor, false, true);  // false for blending, true for alphamergeonly
        FLastIndex:=vLoop;
      END;

    end; // if cZFILL -----------------------------------------------------------------
    FXAct:=FXAct-vNewRows;
    vLoopNewRow:=vLoopNewRow-vNewRows;
  end; // TODO: repeat drawing line according to linewidth

  if not cZFILL then begin
    // draw line with interpolator
    IF (Interpolate and (FInterpolator <> nil)) THEN BEGIN
      vCount := TInterpolator_PixelXY(FInterpolator).Interpolate(vXYPixelLine, ALine);
      for vLoop := 0 to vCount-1 do begin
        if Style is TSeriesStyleLines then FLineEndPoint := vXYPixelLine[vLoop].Pt;
        DrawPoint(vXYPixelLine[vLoop].Pt, TPlotRect(TPlotAxis(OwnerPlot.Axis[XAxis]).OwnerPlotRect).DataImage, vXYPixelLine[vLoop].FPColor, false, false);
      end;
    END ELSE
    // or.. draw line without interpolator
    FOR vLoop := 0 to Length(ALine)-1 DO BEGIN
      vError := XYZToDataImage(OwnerPlot.Axis[XAxis], OwnerPlot.Axis[YAxis], nil, ALine[vLoop].X, ALine[vLoop].Y, 0, vPt);
      if Style is TSeriesStyleLines then begin
        vError := XYZToDataImage(OwnerPlot.Axis[XAxis], OwnerPlot.Axis[YAxis], nil, ALine[vLoop].X, OwnerPlot.Axis[YAxis].ViewRange.min, 0, FLineEndPoint);
      end;
      IF vError < 0 THEN BEGIN
                           FLastIndex:=vLoop;
                           continue;
                         END;

      vFPColor := uPlotAxis.TPlotAxis(OwnerPlot.Axis[YAxis]).ValueFPColor[ALine[vLoop].Y];
      IF ALine[vLoop].Y > FYThreshold THEN vFPColor.alpha := TransParency ELSE vFPColor.alpha := 0; //  $7FFF;

      // Rolling grid: for time axis (and frequency ?) --> TODO: make this a property
      //IF vMainTickZ THEN begin
      //  //IF vFPColor.alpha > $7FFF then vFPColor.alpha := (vFPColor.alpha DIV 2) else vFPColor.alpha:=vFPColor.alpha * 2;
      //  vFPColor := vMainTickColor;
      //end;

      //... do the drawing
      DrawPoint(vPt, TPlotRect(TPlotAxis(OwnerPlot.Axis[XAxis]).OwnerPlotRect).DataImage, vFPColor, false, true);  // false for blending, true for alphamergeonly
      FLastIndex:=vLoop;
    END;
  end;

   // ATTENTION: constant (shifted) mask is applied at every drawline !
   // --> try change to gradient mask again and apply every update only (in DoPlotToImage)
   // TODO: make blending a property  because its slow !

   // TODO: if NOT timedrefresh, call refresh here !?
   //if not OwnerPlot.TimedRefresh then
   // TPlotRect(TPlotAxis(OwnerPlot.Axis[XAxis]).OwnerPlotRect).UpdateSeriesData(Self, FALSE);

   ActualBaseLine:= ActualBaseLine + 1;
end;

procedure TXYWF3DPlotSeries.Redraw;
var
  vLoop: integer;
  vItem: PXYLine;
begin
  FElapsedTime:=0;
  // inherited Redraw;
  // we resize the bitmap area here, the rest comes automagically
  IF ( (XAxis < 0) OR (ZAxis <0) ) THEN exit;

  //  calc time per pixelline for scale
  IF ZAxis = -1 THEN exit;   // scale only when ZAxis is already assigned !

  // generate a alpha mask
  IF FAlphaMask <> nil then FreeAndNil(FAlphaMask);
  FAlphaMask := TLazIntfImage.CreateCompatible(TPlotRect(TPlotAxis(OwnerPlot.Axis[XAxis]).OwnerPlotRect).DataImage, TPlotRect(TPlotAxis(OwnerPlot.Axis[XAxis]).OwnerPlotRect).DataImage.Width, TPlotRect(TPlotAxis(OwnerPlot.Axis[XAxis]).OwnerPlotRect).DataImage.Height);
  _UpdateFadeMask;
  //FAlphaMask.DataDescription := TPlotRect(TPlotAxis(OwnerPlot.Axis[XAxis]).OwnerPlotRect).DataImage.DataDescription;

  // fill in stored data
  IF NumStoredItems < 1 THEN exit;
  ActualBaseLine:=1;
  IF round(uPlotAxis.TPlotAxis(OwnerPlot.Axis[ZAxis]).DrawLength) < NumStoredItems
    THEN ActualBaseLine := NumStoredItems - round(uPlotAxis.TPlotAxis(OwnerPlot.Axis[ZAxis]).DrawLength);
  FOR vLoop := ActualBaseLine TO NumStoredItems-1 DO BEGIN
    vItem := FItems[vLoop-1];
    //vLine := vItem^;
    DrawLine(vItem^);
  END;

  FDataChanged:=true;
  // update the plot
  TPlotRect(TPlotAxis(OwnerPlot.Axis[XAxis]).OwnerPlotRect).UpdateSeriesData(Self, false, true);
end;


constructor TXYWF3DPlotSeries.Create(AOwnerPlot: TPlot);
begin
  inherited Create(AOwnerPlot);
  FSeriestype := stWF3D;
  FIsFastSeries := TRUE;

  FXact := 0;
  // interpolate ?
  FInterpolator := TInterpolator_PixelXY.Create(Self);
  TInterpolator_PixelXY(FInterpolator).CalcAxes := [caX, caY];
  TInterpolator_PixelXY(FInterpolator).IpolAxisMode := iamXonly; // imXonly;
  TInterpolator_PixelXY(FInterpolator).IpolMode := imStep; // imXonly;
  //TInterpolator_PixelXY(FInterpolator).ZeroYcoord := true; // imXonly;
  FInterpolate:=false; // TODO: default to false after testing;
end;

destructor TXYWF3DPlotSeries.Destroy;
begin
  inherited Destroy;
end;

procedure TXYWF3DPlotSeries.DoPlotToImage;
begin
  IF FAlphaMask <> nil THEN
    TPlotRect(TPlotAxis(OwnerPlot.Axis[XAxis]).OwnerPlotRect).DataImage.ApplyAlpha(FAlphaMask, 0, 0);
end;

{ TXYSpectrumPlotSeries }
// ***************************************************************************
{
2D spectrum plotseries
- draws X/Y points to a memory (TLazIntf-)image (which is fast)
- X or Y are drawn in cartesian coordinates and could be colored on choice
- complete datalines are added (TXYLine) while adding a new line
  clears the former display (a property 'persistance' is planned)
- MaxStoredItems is 1 for normal operation while more lines can be stroed
  for AverageN operation
}

procedure TXYSpectrumPlotSeries.SetActualLine(const AValue: Int64);
begin
  if FActualLine = AValue then exit;
  FActualLine := AValue;
end;

function TXYSpectrumPlotSeries.GetValueRange(AAxisIndex: Integer): TValueRange;
var
  vLoop, vItemLoop: Integer;
  vPItem: PXYLine;
  vItem: TXYLine;
  vXmin, vYmin: Extended;
  vXmax, vYmax: Extended;
begin
  //Result:=inherited GetValueRange(AAxisIndex); // can we inherit ?
  Result.min := c_GLOBALMAX;
  Result.max := -c_GLOBALMAX;
  IF (AAxisIndex <> XAxis) AND (AAxisIndex <> YAxis) THEN exit;
  IF FItems = nil THEN exit;

  vXmin := c_GLOBALMAX;
  vXmax := -c_GLOBALMAX;
  vYmin := vXmin; vYmax := vXmax;

  FOR vItemLoop := 0 TO NumStoredItems-1 DO BEGIN
    vPItem := FItems[vItemLoop];
    vItem := vPItem^;

    FOR vLoop := 0 TO length(vItem)-1 do
    begin
      vXmin := math.Min(vXmin, vItem[vLoop].X);
      vXmax := math.Max(vXmax, vItem[vLoop].X);
      vYmin := math.Min(vYmin, vItem[vLoop].Y);
      vYmax := math.Max(vYmax, vItem[vLoop].Y);
    end;

    IF AAxisIndex = XAxis THEN BEGIN
      Result.min := vXmin;
      Result.max := vXmax;
    END;
    IF AAxisIndex = YAxis THEN BEGIN
      Result.min := vYmin;
      Result.max := vYmax;
    END;
  END;
end;


function TXYSpectrumPlotSeries.GetDisplayedLine: TXYLine;
begin
  case TraceMode of
  smClearWrite:  begin
                   Result := GetStoredLine(NumStoredItems-1);
                 end;
  smAveragingN,
  smMaxHold:     begin
                   Result := FAVLine;
                 end;
  end;
end;

function TXYSpectrumPlotSeries.GetStoredLine(AIndex: Integer): TXYLine;
var
  vItem:PXYLine;
begin
  Result := nil;
  IF AIndex < 0 then exit;
  IF MaxStoredItems < 1 THEN exit;

  begin
    vItem := FItems[AIndex];
    Result := (vItem^);
  end;
end;

procedure TXYSpectrumPlotSeries.SetAverageN(const AValue: Integer);
begin
  IF FAverageN = AValue THEN exit;
  IF AValue < 1 THEN FAverageN := 1
  ELSE FAverageN := AValue;
  FMaxStoredItems := FAverageN;
  Clear;
end;

procedure TXYSpectrumPlotSeries.SetTraceMode(const AValue: TTraceMode);
begin
  if FTraceMode = AValue then exit;
  Clear; // reset when mode is changed
  FTraceMode := AValue;
  case FTraceMode of
  smClearWrite:   MaxStoredItems := 1;
  smAveragingN:   begin
                    IF MaxStoredItems < FAverageN THEN FMaxStoredItems := FAverageN;
                  end;
  end;
end;

procedure TXYSpectrumPlotSeries.DrawLine(ALine: TXYLine);
var
  vLoop: Integer;
  vPt : TPoint;
  vError: Integer;
  vFPColor: TFPColor;
  vCount: Integer;
  vXYPixelLine: TXYPixelLine;
begin
   //ActualBaseLine:= ActualBaseLine + 1;

   IF (Interpolate and (FInterpolator <> nil)) THEN BEGIN
     vCount := TInterpolator_PixelXY(FInterpolator).Interpolate(vXYPixelLine, ALine);
     for vLoop := 0 to vCount-1 do begin
       if Style is TSeriesStyleLines then FLineEndPoint := vXYPixelLine[vLoop].Pt;      // no linestyle for interpolated lines at the moment
       DrawPoint(vXYPixelLine[vLoop].Pt, TPlotRect(TPlotAxis(OwnerPlot.Axis[XAxis]).OwnerPlotRect).DataImage, vXYPixelLine[vLoop].FPColor, false, false);
     end;
   END ELSE
   // draw new line
   FOR vLoop := 0 to Length(ALine)-1 DO BEGIN
     // new calculation 09.08.14
     vError := XYZToDataImage(OwnerPlot.Axis[XAxis], OwnerPlot.Axis[YAxis], nil, ALine[vLoop].X, ALine[vLoop].Y, 0, vPt);
     // for seriesstyle lines we need to calc the lineendpoint and make it availabe
     // cannot calc on the fly as interpolator is rasterized and values are no longer known (we better dont use ScreenToXY function)
     if Style is TSeriesStyleLines then begin
       vError := XYZToDataImage(OwnerPlot.Axis[XAxis], OwnerPlot.Axis[YAxis], nil, ALine[vLoop].X, OwnerPlot.Axis[YAxis].ViewRange.min, 0, FLineEndPoint);
     end;
     IF vError < 0 then continue;

     IF ColoredAxis > -1 THEN BEGIN
       IF ColoredAxis = XAxis THEN vFPColor := uPlotAxis.TPlotAxis(OwnerPlot.Axis[XAxis]).ValueFPColor[ALine[vLoop].X];
       IF ColoredAxis = YAxis THEN vFPColor := uPlotAxis.TPlotAxis(OwnerPlot.Axis[YAxis]).ValueFPColor[ALine[vLoop].Y];
     END ELSE vFPColor := TColorToFPColor(TPlotStyle(TPlotSeriesBase(Self).Style).Color);

     vFPColor.alpha := TransParency; //  $7FFF;
     DrawPoint(vPt, TPlotRect(TPlotAxis(OwnerPlot.Axis[XAxis]).OwnerPlotRect).DataImage, vFPColor, false, false);  // false for blending, true for alphamergeonly
   END;

   //IF not OwnerPlot.TimedRefresh then
   //  TPlotRect(TPlotAxis(OwnerPlot.Axis[XAxis]).OwnerPlotRect).UpdateSeriesData(Self, TRUE);  // TODO: enable this when no timed update ?
end;

procedure TXYSpectrumPlotSeries.Redraw;
begin
  // inherited Redraw;
  // we resize the bitmap area here, the rest comes automagically
  IF ( (XAxis < 0) OR (YAxis <0) ) THEN exit;

  FDataChanged:=true;
  TPlotRect(TPlotAxis(OwnerPlot.Axis[XAxis]).OwnerPlotRect).UpdateSeriesData(Self, false, TRUE); // force refresh on Redraw !
end;

function TXYSpectrumPlotSeries.GetLineEndpoint(out ALineEndPoint: TPoint
  ): Integer;
begin
  Result := inherited GetLineEndpoint(ALineEndPoint);
end;

function TXYSpectrumPlotSeries.ExportSeriesData(AFileName: TFilename): Integer;
var
  vExporter: TPlot_ResultWriterMAAResult;
  vMeasResult: TMeasResult;
  vLoop, vLoopResults: Integer;
begin
  vExporter := TPlot_ResultWriterMAAResult.Create;
  try
    for vLoopResults := 0 to NumStoredItems-1 do begin

      New(vMeasResult.lpMeasData);
      try
        vMeasResult.dwResultIdent.diPoints := length(TXYLine(FItems.Items[vLoopResults]^));
        vMeasResult.dwResultIdent.diDimensions := 2;

        SetLength(vMeasResult.lpMeasData^, vMeasResult.dwResultIdent.diPoints, vMeasResult.dwResultIdent.diDimensions);
        for vLoop := 0 to vMeasResult.dwResultIdent.diPoints-1 do begin
          vMeasResult.lpMeasData^[vLoop, 0] := TXYLine(FItems.Items[vLoopResults]^)[vLoop].X;
          vMeasResult.lpMeasData^[vLoop, 1] := TXYLine(FItems.Items[vLoopResults]^)[vLoop].Y;
        end;

        vExporter.AddResultRecord(vMeasResult);
      finally
        SetLength(vMeasResult.lpMeasData^, 0, 0);
        Dispose(vMeasResult.lpMeasData);
      end;
    end;
    vExporter.WriteToFile(AFileName);
  finally
    vExporter.Free;
  end;
  Result := vMeasResult.dwResultIdent.diPoints;
end;

function TXYSpectrumPlotSeries.ImportSeriesData(AFileName: TFileName): Integer;
var
  vImporter: TPlot_ResultReaderMAAResult;
  vLoop, vLoopPoints, vResultCount, vLoopResults: Integer; // vPointCount
  vMeasResult: TMeasResult;
  vXYLine: TXYLine;
begin
  vImporter := TPlot_ResultReaderMAAResult.Create;
  try
    vImporter.LoadFromFile(AFileName);
    vResultCount := vImporter.NumResults;
    for vLoopResults := 0 to vResultCount-1 do begin
      New(vMeasResult.lpMeasData);
      try
        for vLoop := 0 to vResultCount-1 do begin // normally 1 on XY series !
            vImporter.GetResultRecord(vLoop, vMeasResult); // TODO: multitrace !
            IF (vMeasResult.dwResultIdent.diPoints < 1) OR (vMeasResult.dwResultIdent.diDimensions < 2) then exit;
            setlength(vXYLine, vMeasResult.dwResultIdent.diPoints);
            for vLoopPoints := 0 to vMeasResult.dwResultIdent.diPoints-1 do begin
              vXYLine[vLoopPoints].X := vMeasResult.lpMeasData^[vLoopPoints, 0];
              vXYLine[vLoopPoints].Y := vMeasResult.lpMeasData^[vLoopPoints, 1];
            end;
            Self.AddLine(vXYLine);
        end;
      finally
        setlength(vXYLine, 0);
        setlength(vMeasResult.lpMeasData^, 0, 0);
        Dispose(vMeasResult.lpMeasData);
      end;
    end;
  finally
    vImporter.Free;
  end;
  Result := vMeasResult.dwResultIdent.diPoints;
end;

constructor TXYSpectrumPlotSeries.Create(AOwnerPlot: TPlot);
begin
  inherited Create(AOwnerPlot);
  FSeriestype := stSPECTRUM;
  FIsFastSeries := TRUE;
  FMaxStoredItems := 1; // nothing is displayed if not at least 1 item is stored
  FActualLine := -1;
  FLinesQueued := 0;

  setlength(FAVLine, 0);
  FAverageN := 1;
  XAxis := 0; YAxis := 1;
  FInterpolator := TInterpolator_PixelXY.Create(Self);
  TInterpolator_PixelXY(FInterpolator).CalcAxes := [caX, caY];
  TInterpolator_PixelXY(FInterpolator).IpolAxisMode := iamXY; // imXonly;
  TInterpolator_PixelXY(FInterpolator).IpolMode:=imLinear;
  FInterpolate:=false; // TODO: default to false after testing;
end;

destructor TXYSpectrumPlotSeries.Destroy;
begin
  Clear;
  IF FItems <> nil THEN
  begin
    FItems.Free;
    FItems := nil;
  end;       // TODO: is this inherited from TXYPlotSeries ?
  inherited Destroy;
end;

function TXYSpectrumPlotSeries.CheckRefresh(var ANeedsRefresh: Boolean;
  var ANeedsClear: Boolean): Integer;
begin
  IF FDataChanged THEN begin
    ANeedsRefresh := true;
    ANeedsClear := true;  // we have only one dataline, otherwise we plot cummulative lines
    FDataChanged := false;
  end;
  Result := 0
end;


procedure TXYSpectrumPlotSeries.Clear;
var vLine: PXYLine;
begin
  IF FItems = nil THEN exit;
  while FItems.Count > 0 do
  begin
    vLine := FItems.Items[0];
    SetLength(vLine^, 0);
    Dispose(vLine);
    FItems.Delete(0);
  end;
  ActualLine := -1;
  FLinesQueued := 0;
  setlength(FAVLine,0 );
  TPlotRect(uPlotClass.TPlotSeriesBase(Self).OwnerPlot.Axis[XAxis].OwnerPlotRect).UpdateSeriesData(Self, false, FALSE);
  //inherited Clear;
end;

procedure TXYSpectrumPlotSeries.AddLine(ALine: TXYLine);
var
  vItem: PXYLine;
  vLoop: Integer;
  vLineZero: TXYLine;
begin
  IF length(ALine) = 0 THEN exit;
  New(vItem);
  try
    Setlength(vItem^, length(ALine));

    Move(ALine[0], vItem^[0], length(ALine)*SizeOf(TXYValue));
    //
    IF FItems = nil THEN FItems := TFPList.Create;
    FItems.Add(vItem);
    inc(FActualLine, 1);

    case TraceMode of
    smClearWrite: ;  // do nothing as already stored see above
    smMaxHold:    begin
                      IF length(FAVLine) < 1 THEN BEGIN
                        setlength(FAVLine, length(ALine));
                        for vLoop := 0 to length(ALine)-1 do begin
                          FAVLine[vLoop].X := ALine[vLoop].X;
                          FAVLine[vLoop].Y := ALine[vLoop].Y;
                        end;
                      END;
                    for vLoop := 0 to length(ALine)-1 do begin
                      IF (ALine[vLoop].Y > FAVLine[vLoop].Y) THEN BEGIN
                        FAVLine[vLoop].Y := ALine[vLoop].Y;
                        FAVLine[vLoop].X := ALine[vLoop].X;
                      END;
                    end;
                  end;
    smAveragingN: begin
                      // first init
                      IF (length(FAVLine) < 1) OR (length(FAVLine) <> length(ALine)) THEN BEGIN
                        setlength(FAVLine, length(ALine));
                        for vLoop := 0 to length(ALine)-1 do begin
                          FAVLine[vLoop].X := ALine[vLoop].X;
                          FAVLine[vLoop].Y := ALine[vLoop].Y;
                        end;
                      END;
                      IF (NumStoredItems < AverageN) AND (NumStoredItems > 1) THEN BEGIN
                        for vLoop := 0 to length(FAVLine)-1 do begin
                          FAVLine[vLoop].X := ALine[vLoop].X;
                          FAVLine[vLoop].Y := ( (FAVLine[vLoop].Y / NumStoredItems) +
                                                (ALine[vLoop].Y / (NumStoredItems * (NumStoredItems-1))) ) *
                                                (NumStoredItems - 1);
                        end;
                      END ELSE IF (NumStoredItems >= AverageN) THEN BEGIN
                        vLineZero := TXYLine(FItems.Items[0]^);
                        for vLoop := 0 to length(FAVLine)-1 do begin
                          FAVLine[vLoop].X := ALine[vLoop].X;
                          FAVLine[vLoop].Y := FAVLine[vLoop].Y +
                                                (ALine[vLoop].Y / AverageN) -
                                                (vLineZero[vLoop].Y / AverageN);
                        end;
                      END;

                  end;

    end;
    // delete old items
    while FItems.Count > MaxStoredItems do BEGIN    // delete item (0) = oldest
      vItem := FItems.Items[0];
      SetLength(vItem^, 0);
      Dispose(vItem);
      FItems.Delete(0);
      dec(FActualLine, 1);
    END;

    // TODO: MarkerContainer should fetch the actual Data if needed;
    // this works only with unified DataLine types or MAAResult type with handshake !
    MarkerContainer.ReNewData(ALine, 0, false);

  except
    Dispose(vItem);
  end;
  FDataChanged := true;
  TPlotRect(uPlotClass.TPlotSeriesBase(Self).OwnerPlot.Axis[XAxis].OwnerPlotRect).UpdateSeriesData(Self, false, FALSE);
end;

procedure TXYSpectrumPlotSeries.DoPlotToImage;
var
  vItem: PXYLine;
begin
  IF NumStoredItems < 1 THEN exit;
  case TraceMode of
  smClearWrite:  begin
                     vItem := FItems[NumStoredItems-1];
                     DrawLine(vItem^);
                 end;
  smMaxHold:     begin
                   DrawLine(FAVLine);
                 end;
  smAveragingN:  begin
                   DrawLine(FAVLine);
                 end;
  end;
end;


end.
