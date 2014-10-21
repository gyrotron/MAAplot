unit uPlotInterpolator;
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
  Classes, SysUtils, FPimage, uPlotAxis, uPlotDataTypes, uPlotClass, uPlotUtils, math, Graphics, uPlotStyles;


type

  TFPColorPoint = record
    Pt:    TPoint;
    FPColor: TFPColor;
  end;

  PXYPixelLine = ^TXYPixelLine;
  TXYPixelLine = array of TFPColorPoint;
  TExtendedArray = array of Extended;


  TIpolMode = (imStep, imLinear);
  TIpolAxisMode = (iamXonly, iamXY);                 // TODO
  THiddenPixelMode = (hpmMax, hpmMin, hpmAverage);
  TCalcAxis = (caX, caY, caZ);
  TCalcAxes = set of TCalcAxis;

  THiddenCalcFunction = function(ALine: TExtendedArray; AAxis: TPlotAxisBase): Extended of object;
  TIpolFunction = function(AScreenPoint: TPoint; AColorValueStart: Extended; AColorValueEnd: Extended; IsLastPoint: Boolean; var AXYPixelLine: TXYPixelLine): Integer of object;

  { TInterpolator }

  TInterpolator = class
  private
  protected
    FOwnerSeries: TPlotSeriesBase;
  public
    constructor Create(AOwnerSeries: TPlotSeriesBase); virtual;
    destructor Destroy; override;
  end;

  { TInterpolator_Pixel }

  { Notes:
    - the pixel interpolator calculates the screen pixels from XYValues
    - it avoids duplicate pixel drawing if we have more than 1 point per screen pixel
    - it fills the gaps between Screen points
  }

  { TInterpolator_PixelXY }
  // calculation based on DataImage for fast series

  TInterpolator_PixelXY = class(TInterpolator)       // coloredaxis and axes from ownerseries // axes in CoordAxes are respected, rest is not calcualted
  private                                            // Z axis disregarded, as PixelXY only takes TXYLines
    FIpolFunction: TIpolFunction;                    // either use _Ipol or _IpolStep
    FIpolMode: TIpolMode;
    FIpolAxisMode: TIpolAxisMode;                    // interpolate only X coordinates or XY coordinates
    FHiddenPixelMode: THiddenPixelMode;              // max, min or mean of pixels plottet to the same screen coordinate
    FCalcAxes: TCalcAxes;                            // simplify this
    FHiddenCalcFunc: THiddenCalcFunction;
    FHiddenLine: TExtendedArray;
    FLastHiddenPoint: TPoint;
    //FHideStartIndex: Integer;
    FXaxis, FYaxis, FZaxis, FColoredAxis: TPlotAxisBase;
    FColoredDimension: Integer;
    FZeroYcoord: Boolean;
    function GetCalcAxes: TCalcAxes;
    function GetHiddenPixelMode: THiddenPixelMode;
    function GetIpolAxisMode: TIpolAxisMode;
    function GetIpolMode: TIpolMode;
    procedure SetCalcAxes(AValue: TCalcAxes);
    procedure SetHiddenPixelMode(AValue: THiddenPixelMode);
    procedure SetIpolAxisMode(AValue: TIpolAxisMode);
    procedure SetIpolMode(AValue: TIpolMode);
    procedure SetZeroYcoord(AValue: Boolean);
    procedure _Hide(AScreenPoint: TPoint; AColorValue: Extended);
    procedure _UnHide(out AScreenPoint: TPoint; out AColorValue: Extended);
    function _Ipol(AScreenPoint: TPoint; AColorValueStart: Extended; AColorValueEnd: Extended; IsLastPoint: Boolean; var AXYPixelLine: TXYPixelLine): Integer;
    function _IpolStep(AScreenPoint: TPoint; AColorValueStart: Extended; AColorValueEnd: Extended; IsLastPoint: Boolean; var AXYPixelLine: TXYPixelLine): Integer;
    function _ColorValue2FPColor(AColorValue: Extended): TFPColor;
      // basic work done in AddGetPoints, see below
    function _AddGetPoints(AScreenPoint: TPoint; AColorValue: Extended; IsLastPoint: Boolean; out AXYPixelLine: TXYPixelLine): Integer;
    function _ResetGetPoints(out AXYPixelLine: TXYPixelLine): Integer;
    function _ValuesToScreenPoint(AXaxis, AYaxis, AZaxis: TPlotAxisBase; AXValue, AYValue, AZValue: Extended; out AScrPt: TPoint): Integer; virtual;
  protected
    function HiddenCalcMin(ALine: TExtendedArray; AAxis: TPlotAxisBase): Extended;
    function HiddenCalcMax(ALine: TExtendedArray; AAxis: TPlotAxisBase): Extended;
    function HiddenCalcMean(ALine: TExtendedArray; AAxis: TPlotAxisBase): Extended;
  public
    constructor Create(AOwnerSeries: TPlotSeriesBase); override;
    destructor Destroy; override;

    function Interpolate(out AXYPixelLine: TXYPixelLine; Aline: TXYLine): Integer;   // main routine called from outside

    property CalcAxes: TCalcAxes read GetCalcAxes write SetCalcAxes;            // needed ? or get from series ?  --> try to remove this
    property HiddenPixelMode: THiddenPixelMode read GetHiddenPixelMode write SetHiddenPixelMode;
    property IpolAxisMode: TIpolAxisMode read GetIpolAxisMode write SetIpolAxisMode;
    property IpolMode: TIpolMode read GetIpolMode write SetIpolMode;
    property ZeroYcoord: Boolean read FZeroYcoord write SetZeroYcoord;          // only used for colored spectrograms (2DWF waterfall)
  end;


  { TInterpolator_PixelXY_PlotRect }
  // calculation based on PlotImage for slow standard 2D and 3D series

  TInterpolator_PixelXY_PlotRect = class(TInterpolator_PixelXY)
  private
    function _ValuesToScreenPoint(AXaxis, AYaxis, AZaxis: TPlotAxisBase; AXValue, AYValue, AZValue: Extended; out AScrPt: TPoint): Integer; override;
  protected
  public
  end;

implementation

uses
  uPlotSeries;

{ TInterpolator_PixelXY_PlotRect }

function TInterpolator_PixelXY_PlotRect._ValuesToScreenPoint(AXaxis, AYaxis,
  AZaxis: TPlotAxisBase; AXValue, AYValue, AZValue: Extended; out AScrPt: TPoint
  ): Integer;
begin
  Result := XYZToScreen(AXaxis, AYaxis, AZaxis, AXValue, AYValue, AZValue, AScrPt);
end;


{ TInterpolator_Pixel }

function TInterpolator_PixelXY.GetIpolAxisMode: TIpolAxisMode;
begin
  Result := FIpolAxisMode;
end;

function TInterpolator_PixelXY.GetIpolMode: TIpolMode;
begin
  Result := FIpolMode;
end;

procedure TInterpolator_PixelXY.SetCalcAxes(AValue: TCalcAxes);
begin
  IF FCalcAxes = AValue THEN exit;
  FCalcAxes := AValue;
end;

function TInterpolator_PixelXY.GetHiddenPixelMode: THiddenPixelMode;
begin
  Result := FHiddenPixelMode;
end;

function TInterpolator_PixelXY.GetCalcAxes: TCalcAxes;
begin
  Result := FCalcAxes;
end;

procedure TInterpolator_PixelXY.SetHiddenPixelMode(AValue: THiddenPixelMode);
begin
  IF FHiddenPixelMode = AValue THEN exit;
  FHiddenPixelMode := AValue;
  case FHiddenPixelMode of
    hpmMin: FHiddenCalcFunc := @HiddenCalcMin;
    hpmMax: FHiddenCalcFunc := @HiddenCalcMax;
    hpmAverage: FHiddenCalcFunc := @HiddenCalcMean;
  end;
end;


procedure TInterpolator_PixelXY.SetIpolAxisMode(AValue: TIpolAxisMode);
begin
  IF FIpolAxisMode = AValue THEN exit;
  FIpolAxisMode := AValue;
end;

procedure TInterpolator_PixelXY.SetIpolMode(AValue: TIpolMode);
begin
  IF FIpolMode = AValue THEN exit;
  FIpolMode := AValue;
  case FIpolMode of
    imStep: FIpolFunction := @_IpolStep;
    imLinear: FIpolFunction := @_Ipol;
  end;
end;

procedure TInterpolator_PixelXY.SetZeroYcoord(AValue: Boolean);
begin
  if FZeroYcoord=AValue then Exit;
  FZeroYcoord:=AValue;
end;

procedure TInterpolator_PixelXY._Hide(AScreenPoint: TPoint; AColorValue: Extended);
begin
  FLastHiddenPoint := AScreenPoint;
  if FColoredAxis <> nil then begin
    setlength(FHiddenLine, length(FHiddenLine)+1);
    FHiddenLine[length(FHiddenLine)-1] :=  TPlotAxis(FColoredAxis).ReCalcValue(AColorValue);
  end else begin
    setlength(FHiddenLine, 1);
    FHiddenLine[0] := 0;
  end;
end;

procedure TInterpolator_PixelXY._UnHide(out AScreenPoint: TPoint; out AColorValue: Extended);
begin
  if (length(FHiddenLine) < 1) then begin
    AScreenPoint.X := c_INVALIDCOORDINATE;
    AScreenPoint.Y := c_INVALIDCOORDINATE;
    AColorValue := 0;
  end else
  begin
    AScreenPoint := FLastHiddenPoint;
    if (FColoredAxis <> nil) then AColorValue := FHiddenCalcFunc(FHiddenLine, FColoredAxis)
    else AColorValue:=0;
  end;
  setlength(FHiddenLine, 0);
end;

function TInterpolator_PixelXY._Ipol(AScreenPoint: TPoint;
  AColorValueStart: Extended; AColorValueEnd: Extended; IsLastPoint: Boolean; var AXYPixelLine: TXYPixelLine
  ): Integer;
var
  dx,dy: Integer;
  vXleading: Boolean;
  vLoop: Integer;
  vX1, vX2: Integer;
  vY1, vY2: Integer;
  dCol: Extended;
  //vPoint: TPoint;
  vSpan: Integer;
  vDirection: Integer; // interpolates from lowX to highX (direction +1) or inverse (direction -1)
  vIndex: Integer;
begin
  if length(AXYPixelLine) <> 1 then begin
    Result := 0;
    exit;
  end;
  vDirection:=1;
  // AXYPixelLine has 1 element which is the start point for Ipol;
  // Parameters AScreenPoint and AColorValue are the endpoints;
  // TODO: correct log color ipol !??
  dx := (AScreenPoint.X - AXYPixelLine[0].Pt.X);
  dy := (AScreenPoint.Y - AXYPixelLine[0].Pt.Y);
  dCol := AColorValueEnd - AColorValueStart;
  IF (dx = 0) and (dy = 0 ) THEN begin
    Result := 1;
    exit;
  end;
  IF ( abs(dx) > abs(dy) ) THEN vXleading := TRUE ELSE vXleading := FALSE;

  IF (IpolAxisMode = iamXonly) THEN begin
    vXleading:=TRUE;
    if dx = 0 then begin
      Result := 1;
      exit;
    end;
  end;


  IF vXleading THEN BEGIN
    IF AXYPixelLine[0].Pt.X > AScreenPoint.X THEN begin
      vX1 := AScreenPoint.X;
      vY1 := AScreenPoint.Y;
      vX2 := AXYPixelLine[0].Pt.X;
      vDirection:=-1;
      AColorValueStart:=AColorValueEnd;
      dCol := -dCol;
    end else begin
      vX1 := AXYPixelLine[0].Pt.X;
      vY1 := AXYPixelLine[0].Pt.Y;
      vX2 := AScreenPoint.X;
    end;

    vSpan := vX2-vX1;
    setlength(AXYPixelLine, length(AXYPixelLine) + vSpan + 1);
    for vLoop := vX1 to vX2 do begin
      vY2 := trunc(dy/dx * (vLoop-vX1)) + vY1;  //    trunc(dy/dx * (vLoop-vX1) + 1) + vY1; why +1 ???????????????? also in Lazarus bug ?

      AXYPixelLine[vLoop-vX1+1].Pt.X := vLoop;
      AXYPixelLine[vLoop-vX1+1].Pt.Y := vY2;
      AXYPixelLine[vLoop-vX1+1].FPColor := _ColorValue2FPColor( ((vLoop-vX1)/vSpan * dCol) + AColorValueStart );
    end;
  END ELSE BEGIN
    IF AXYPixelLine[0].Pt.Y > AScreenPoint.Y THEN begin
      vY1 := AScreenPoint.Y;
      vX1 := AScreenPoint.X;
      vY2 := AXYPixelLine[0].Pt.Y;
      vDirection:=-1;
      AColorValueStart:=AColorValueEnd;
      dCol := -dCol;
    end else begin
      vY1 := AXYPixelLine[0].Pt.Y;
      vX1 := AXYPixelLine[0].Pt.X;
      vY2 := AScreenPoint.Y;
    end;

    vSpan := vY2-vY1;
    setlength(AXYPixelLine, length(AXYPixelLine) + vSpan + 1);
    for vLoop := vY1 to vY2 do begin
      vX2 := trunc(dx/dy * (vLoop-vY1)) + vX1;
      if (vDirection > 0) then vIndex := vLoop-vY1+1 else vIndex := vY2 - vLoop + 1;  // fill forward

      AXYPixelLine[vIndex].Pt.X := vX2;
      AXYPixelLine[vIndex].Pt.Y := vLoop;
      AXYPixelLine[vIndex].FPColor := _ColorValue2FPColor( ((vLoop-vY1)/vSpan * dCol) + AColorValueStart );
    end;
  END;

  if (not IsLastPoint) then setlength(AXYPixelLine, length(AXYPixelLine)-1); // delete last point
  Result := length(AXYPixelLine);

end;

function TInterpolator_PixelXY._IpolStep(AScreenPoint: TPoint;
  AColorValueStart: Extended; AColorValueEnd: Extended; IsLastPoint: Boolean;
  var AXYPixelLine: TXYPixelLine): Integer;
var
  dx,dy: Integer;
  vXleading: Boolean;
  vLoop: Integer;
  vX1, vX2, vX, vmid: Integer;
  vY1, vY2, vY: Integer;
  dCol, vColTemp: Extended;
  //vPoint: TPoint;
  vSpan: Integer;
  vDirection: Integer; // interpolates from lowX to highX (direction +1) or inverse (direction -1)
  vIndex, vIndexbase: Integer;

begin
  if length(AXYPixelLine) <> 1 then begin
    Result := 0;
    exit;
  end;
  vDirection:=1;
  // AXYPixelLine has 1 element which is the start point for Ipol;
  // Parameters AScreenPoint and AColorValue are the endpoints;
  // TODO: correct log color ipol !??
  dx := (AScreenPoint.X - AXYPixelLine[0].Pt.X);
  dy := (AScreenPoint.Y - AXYPixelLine[0].Pt.Y);
  dCol := AColorValueEnd - AColorValueStart;
  IF (dx = 0) and (dy = 0 ) THEN begin
    Result := 1;
    exit;
  end;
  IF ( abs(dx) > abs(dy) ) THEN vXleading := TRUE ELSE vXleading := FALSE;

  IF (IpolAxisMode = iamXonly) THEN begin
    vXleading:=TRUE;
    if dx = 0 then begin
      Result := 1;
      exit;
    end;
  end;

  // debug
  if (dx > 3) then vXleading:=true;
  //if (dy > 0) then vXleading:=false;

  IF vXleading THEN BEGIN
    IF AXYPixelLine[0].Pt.X > AScreenPoint.X THEN begin
      vX1 := AScreenPoint.X;
      vY1 := AScreenPoint.Y;
      vX2 := AXYPixelLine[0].Pt.X;
      vDirection:=-1;
      vColTemp:=AColorValueStart;
      AColorValueStart:=AColorValueEnd;
      AColorValueEnd:=vColTemp;
      dCol := -dCol;
     vY2 := AXYPixelLine[0].Pt.Y;
    end else begin
      vX1 := AXYPixelLine[0].Pt.X;
      vY1 := AXYPixelLine[0].Pt.Y;
      vX2 := AScreenPoint.X;
     vY2 := AScreenPoint.Y;
    end;

    vSpan := vX2-vX1;
    vmid := vX1 + ((vX2-vX1) DIV 2);
    setlength(AXYPixelLine, length(AXYPixelLine) + vSpan + 1);
    for vLoop := vX1 to vX2 do begin
      if (vLoop <= vmid) then vY := vY1 else vY := vY2;

      AXYPixelLine[vLoop-vX1+1].Pt.X := vLoop;
      AXYPixelLine[vLoop-vX1+1].Pt.Y := vY; //vY2;
      AXYPixelLine[vLoop-vX1+1].FPColor := _ColorValue2FPColor( ((vLoop-vX1)/vSpan * dCol) + AColorValueStart );
    end;

    // add vertical line (X indices not in order !)
    vSpan := abs(dy)-1;
    vIndexbase:=length(AXYPixelLine);
    setlength(AXYPixelLine, length(AXYPixelLine) + vSpan );

    for vLoop := min(vY1, vY2)+1 to max(vY1,vY2)-1 do begin
      AXYPixelLine[vIndexbase + vLoop-min(vY1, vY2)-1].Pt.X := vmid;
      AXYPixelLine[vIndexbase + vLoop-min(vY1, vY2)-1].Pt.Y := vLoop; //vY2;
      AXYPixelLine[vIndexbase + vLoop-min(vY1, vY2)-1].FPColor := _ColorValue2FPColor( (0.5 * dCol) + AColorValueStart )
    end;

  END ELSE BEGIN
    IF AXYPixelLine[0].Pt.Y > AScreenPoint.Y THEN begin
      vY1 := AScreenPoint.Y;
      vX1 := AScreenPoint.X;
      vY2 := AXYPixelLine[0].Pt.Y;
      vDirection:=-1;
      vColTemp:=AColorValueStart;
      AColorValueStart:=AColorValueEnd;
      AColorValueEnd:=vColTemp;
      dCol := -dCol;
     vX2 := AXYPixelLine[0].Pt.X;
    end else begin
      vY1 := AXYPixelLine[0].Pt.Y;
      vX1 := AXYPixelLine[0].Pt.X;
      vY2 := AScreenPoint.Y;
     vX2 := AScreenPoint.X;
    end;

    vSpan := vY2-vY1;
    vmid := vY1 + ((vY2-vY1) DIV 2);
    setlength(AXYPixelLine, length(AXYPixelLine) + vSpan + 1);
    for vLoop := vY1 to vY2 do begin
      if (vLoop <= vmid) then vX := vX1 else vX := vX2;
      if (vDirection > 0) then vIndex := vLoop-vY1+1 else vIndex := vY2 - vLoop + 1;  // fill forward

      AXYPixelLine[vIndex].Pt.X := vX; //vX2;
      AXYPixelLine[vIndex].Pt.Y := vLoop;
      AXYPixelLine[vIndex].FPColor := _ColorValue2FPColor( ((vLoop-vY1)/vSpan * dCol) + AColorValueStart );
    end;

    // add horizontal line (X indices not in order !)
    vSpan := abs(dx)-1;
    vIndexbase:=length(AXYPixelLine);
    setlength(AXYPixelLine, length(AXYPixelLine) + vSpan );
    for vLoop := min(vX1, vX2)+1 to max(vX1,vX2)-1 do begin
      AXYPixelLine[vIndexbase + vLoop-min(vX1, vX2)-1].Pt.X := vLoop;
      AXYPixelLine[vIndexbase + vLoop-min(vX1, vX2)-1].Pt.Y := vmid; //vY2;
      AXYPixelLine[vIndexbase + vLoop-min(vX1, vX2)-1].FPColor := _ColorValue2FPColor( (0.5 * dCol) + AColorValueStart );
    end;

  END;

  if (not IsLastPoint) then setlength(AXYPixelLine, length(AXYPixelLine)-1); // delete last point
  Result := length(AXYPixelLine);

end;

function TInterpolator_PixelXY._ColorValue2FPColor(AColorValue: Extended
  ): TFPColor;
begin
  if (FColoredAxis <> nil) then
    Result := TPlotAxis(FColoredAxis).ValueFPColor[AColorValue]
  else Result := TColorToFPColor(TPlotStyle(FOwnerSeries.Style).Color);
  Result.alpha := TPlotSeries(FOwnerSeries).TransParency;
  if (FOwnerSeries is TXYWFPlotSeries) and (AColorValue < TXYWFPlotSeries(FOwnerSeries).YThreshold) then       // TODO: all series
    Result.alpha := alphaTransparent;
end;

function TInterpolator_PixelXY._AddGetPoints(AScreenPoint: TPoint;
  AColorValue: Extended; IsLastPoint: Boolean; out AXYPixelLine: TXYPixelLine
  ): Integer;
var
  dx, dy: Integer;
  vColorValue: Extended;
begin
{ Basic function as follows:
  a) when data points have higher densitiy than pixels (i.e. when these are plotted on top of each other)
  - When a next point for plotting has same coordinates than the recent one, it will be "hidden" --> _Hide
  - When the last point is reached or the coordinate of the next point is now different
    we call _Unhide
  - _UnHide will use @FHiddenCalcFunc to return one point which is
    maximum, minimum or mean of the formerly hidden points
  b) when distance bewteen points is > 1 pixel
  - @FIpolFunction is called to interpolate the pixels

Therefore this function returns
- 0 points when the actual point needs to be hidden
- 1 point if the coordinate changed by 1 pixel and the latest hidden points are to be drawn now
- n points when interpolation occured
}
  Result := 0;
  setlength(AXYPixelLine, 0);
  dx := AScreenPoint.X - FLastHiddenPoint.X;
  dy := AScreenPoint.Y - FLastHiddenPoint.Y;
  if length(FHiddenLine) = 0 then begin                 // first point
    _Hide(AScreenPoint, AColorValue);
    if IsLastPoint then begin
      setlength(AXYPixelLine, 1);
      _UnHide(AXYPixelLine[0].Pt, vColorValue);
      AXYPixelLine[0].FPColor := _ColorValue2FPColor(vColorValue);
    end;
  end else                                              // add hidden
  if (dx = 0) and (dy = 0) THEN begin
    _Hide(AScreenPoint, AColorValue);
    if IsLastPoint then begin
      setlength(AXYPixelLine, 1);
      _UnHide(AXYPixelLine[0].Pt, vColorValue);
      AXYPixelLine[0].FPColor := _ColorValue2FPColor(vColorValue);
    end;
  end else begin                                             // unhide, no ipol, distance <=1
    setlength(AXYPixelLine, 1);
    _UnHide(AXYPixelLine[0].Pt, vColorValue);
    AXYPixelLine[0].FPColor := _ColorValue2FPColor(vColorValue);
    if (abs(dx) <= 1) and (abs(dy) <= 1) THEN begin
    end else begin                                          // do ipol
      Result := FIpolFunction(AScreenPoint, vColorValue, AColorValue, IsLastPoint, AXYPixelLine);
    end;
    if (not IsLastPoint) then _Hide(AScreenPoint, AColorValue);
  end;
  Result := length(AXYPixelLine);
end;

function TInterpolator_PixelXY._ResetGetPoints(out AXYPixelLine: TXYPixelLine
  ): Integer;
var
  vColorValue: Extended;
begin
  if length(FHiddenLine) = 0 then begin
    setlength(AXYPixelLine, 0);
  end else begin
    setlength(AXYPixelLine, 1);
    _UnHide(AXYPixelLine[0].Pt, vColorValue);
    AXYPixelLine[0].FPColor := _ColorValue2FPColor(vColorValue);
  end;
  Result := length(AXYPixelLine);
end;

function TInterpolator_PixelXY._ValuesToScreenPoint(AXaxis, AYaxis,
  AZaxis: TPlotAxisBase; AXValue, AYValue, AZValue: Extended; out AScrPt: TPoint
  ): Integer;
begin
 Result := XYZToDataImage(AXaxis, AYaxis, AZaxis, AXValue, AYValue, AZValue, AScrPt);
end;

function TInterpolator_PixelXY.HiddenCalcMin(ALine: TExtendedArray; AAxis: TPlotAxisBase): Extended;
begin
  if (length(ALine) < 1) or (AAxis = nil) THEN BEGIN
    Result := 0;
    exit;
  end;
  Result := TPlotAxis(AAxis).ReCalcValueInverse( minvalue(ALine) );
end;

function TInterpolator_PixelXY.HiddenCalcMax(ALine: TExtendedArray; AAxis: TPlotAxisBase): Extended;
begin
  if (length(ALine) < 1) or (AAxis = nil) THEN BEGIN
    Result := 0;
    exit;
  end;
  Result := TPlotAxis(AAxis).ReCalcValueInverse( maxvalue(ALine) );
end;

function TInterpolator_PixelXY.HiddenCalcMean(ALine: TExtendedArray; AAxis: TPlotAxisBase): Extended;
begin
  if (length(ALine) < 1) or (AAxis = nil) THEN BEGIN
    Result := 0;
    exit;
  end;
  Result := TPlotAxis(AAxis).ReCalcValueInverse( mean(ALine) );
end;


constructor TInterpolator_PixelXY.Create(AOwnerSeries: TPlotSeriesBase);
begin
  inherited Create(AOwnerSeries);
  FIpolAxisMode := iamXY;
  FHiddenPixelMode := hpmMax;
  FHiddenCalcFunc := @HiddenCalcMax;
  //FHideStartIndex:=-1;
  setlength(FHiddenLine, 0);
  FLastHiddenPoint.X := c_INVALIDCOORDINATE;
  FLastHiddenPoint.Y := c_INVALIDCOORDINATE;
  FZeroYcoord:=false;
  FIpolMode:=imLinear;
  FIpolFunction:=@_Ipol;
  //FIpolMode:=imStep;
  //FIpolFunction:=@_IpolStep;
end;

destructor TInterpolator_PixelXY.Destroy;
begin
  setlength(FHiddenLine, 0);
  inherited Destroy;
end;

function TInterpolator_PixelXY.Interpolate(out AXYPixelLine: TXYPixelLine;
  Aline: TXYLine): Integer;
// operates on dataimage
var
  vScrPt: TPoint;
  vLoop: Integer;
  vError: Integer;
  vColorValue: extended;
  vXYPixelline: TXYPixelLine;
  vLoopAdd, vCount, vStart: Integer;
begin
  setlength(FHiddenLine, 0);     // start condition
  setlength(AXYPixelLine, 0);
  // check for coloredaxis
  if TPlotSeries(FOwnerSeries).ColoredAxis >= 0 then begin
    if (TPlotSeries(FOwnerSeries).XAxis = TPlotSeries(FOwnerSeries).ColoredAxis) then begin
      FColoredDimension := 0;
      FColoredAxis := FOwnerSeries.OwnerPlot.Axis[TPlotSeries(FOwnerSeries).XAxis];
    end else
    if (TPlotSeries(FOwnerSeries).YAxis = TPlotSeries(FOwnerSeries).ColoredAxis) then begin
      FColoredDimension := 1;
      FColoredAxis := FOwnerSeries.OwnerPlot.Axis[TPlotSeries(FOwnerSeries).YAxis];
    end
  end else begin
      FColoredDimension := -1;
      FColoredAxis := nil;
  end;


  if (caX in FCalcAxes) then FXaxis := FOwnerSeries.OwnerPlot.Axis[TPlotSeries(FOwnerSeries).XAxis]
    else FXaxis := nil;
  if (caY in FCalcAxes) then FYaxis := FOwnerSeries.OwnerPlot.Axis[TPlotSeries(FOwnerSeries).YAxis]
    else FYaxis := nil;
  if (FOwnerSeries is TXYZPlotSeries) then FZaxis := FOwnerSeries.OwnerPlot.Axis[TXYZPlotSeries(FOwnerSeries).ZAxis]
    else FZaxis := nil;

  //vEndPoint:=false;

  for vLoop := 0 to length(Aline)- 1 do begin
    vError := _ValuesToScreenPoint(FXaxis, FYaxis, FZaxis, Aline[vLoop].X, Aline[vLoop].Y, 0, vScrPt);
    if ZeroYcoord then vScrPt.Y:=0;
    case FColoredDimension of
      0: vColorValue := Aline[vLoop].X;
      1: vColorValue := Aline[vLoop].Y;
      otherwise vColorValue := 0;
    end;
    if vError >= 0 then vCount := _AddGetPoints(vScrPt, vColorValue, (vLoop = length(ALine)-1), vXYPixelline)
    else vCount := _ResetGetPoints(vXYPixelline); // do not draw any points outside viewrange
    if vCount > 0 then begin
      vStart := length(AXYPixelLine) ;
      setlength(AXYPixelLine, length(AXYPixelLine) + length(vXYPixelline));
      for vLoopAdd := 0 to vCount-1 do begin
        AXYPixelLine[vLoopAdd+vStart] := vXYPixelline[vLoopAdd];
      end;
    end;
  end;
  Result := length(AXYPixelLine);
end;

{ TInterpolator }

constructor TInterpolator.Create(AOwnerSeries: TPlotSeriesBase);
begin
  FOwnerSeries := AOwnerSeries;
end;

destructor TInterpolator.Destroy;
begin
  inherited Destroy;
end;

end.

