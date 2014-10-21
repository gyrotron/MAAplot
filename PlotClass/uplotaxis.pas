unit uPlotAxis;
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

//{$DEFINE DEBUG_SCALE_WRITE}

interface

uses
  Classes, Graphics, Types, GraphMath, GraphUtil, math, sysutils, uPlotStyles, uPlotClass,
  Dialogs, FPimage, IntfGraphics;

type

  TNumberFormat = (nfPlain, nfFloat, nfEngineering, nfEngineeringPrefix); // was in  uPlot...brought to axis now
  //TPointStyleShapeName : array[shapeDot..shapeMarkerTriangle] of String = (
  //  'Dot', 'Circle', 'Square solid', 'Square', 'Cross', 'Plus', 'MarkerTriangle')

  TAutoLengthMode = (lmRelToSmallSide, lmRelToLargeSide, lmRelToHeight, lmRelToWidth, lmRelToBorder); //, lmAutoFill);
  // check draw is done with 100%, then Drawlength is set from PLotrect for auto-axes (mainly used for 3D plots)
  // all further magic done in plotrect if plotrect property AxisAutoPlaceFill is set there
  // if not set or if no solution can be calculated, we default to axis settings defined here
  TTickAngle = (taPositive, taNegative);

  TAxisMode = (amPixelPerValue, amValuePerPixel);

  TOriginMode = (omAbs, omRel);

  TTickInfo = record                   // is recalculated on ReDraw and can then be used by tick drawing etc.
    mainlow      : Extended;            // is also availavle via VisualParams
    mainhigh     : Extended;
    maininterval : Extended;
    maincount    : Integer; // for convenience
    SubNumbers: Set of byte;
  end;

  PAxisVisualParams = ^TAxisVisualParams;

  { TAxisVisualParams }

  TAxisVisualParams = packed record
    ptA, ptB: TPoint;                    // relative to what ??
    PixelsPerValue: Extended;
    DrawAngle: Extended;
    DrawLength: Extended;
    DrawCanvas: TCanvas;
    TickLength, SubTickLength: Integer;
    Style: TAxisStyle;
    TickStyle, SubTickStyle: TAxisStyle;
    TickAngle: TTickAngle;
    Ticks, SubTicks, Marks, SubMarks: Boolean;
    Visible: Boolean; // TODO implement here, delete as individual parameter but keep property ;-)
    TickInfo: TTickInfo;
  end;

  PCloneAxisParams = ^TCloneAxisParams;
  TCloneAxisParams = packed record
    ShiftAxis: TPlotAxisBase;
    ShiftLengthRel: Integer;
    VisualParams: TAxisVisualParams;     // TODO: only Style and Tickparams should be stored here
                                         // separate AxisVisualParams in Style params and calculated params !
  end;

  { TPlotAxis }

  // Attention: all calculations refer to mathematical grid
  // i.e. Bottom-Left = 0-0 (or viewrange Xmin-Ymin)
  TPlotAxis = class(uPlotClass.TPlotAxisBase)
  private
    F3DGridAxis: Integer; // 02.08.14
    FInner3DTicks: Boolean;
    FTickInfo: TTickInfo; // 02.08.14
    FAutoLength: Boolean;
    FAxisLabel: String;
    FAxisMode: TAxisMode;
    FDrawAngle: Extended;
    FDrawLength: Extended;
    FAutoLengthMode: TAutoLengthMode;
    FDrawOrigin: TPoint;
    FInnerSubTicks: Boolean;
    FInnerTicks: Boolean;
    FLogBase: Extended;
    FLogScale: Boolean;
    FNumberFormat: TNumberFormat;
    FOriginMode: TOriginMode;
    FRotateLabelText: Boolean;
    FRotateUnitText: Boolean;
    FSubGrain: Integer;
    FSubTickLength: integer;
    FSubTickMarks: boolean;
    FSubTicks: boolean;
    FSubTickStyle: TAxisStyle;
    FTickAngle: TTickAngle;
    FTickLength: integer;
    FTickMarks: boolean;
    FTicks: boolean;
    FInnerGridAxes: TList;
    FCloneAxes: TList;
    FTickStyle: TAxisStyle;
    FValuePerPixel: Extended;
    FAutoPlacedLength: Extended;
    FAutoPlacedOriginRel: TPoint;
    function GetAutoPlacedLength: Extended;
    function GetAutoPlacedOriginRel: TPoint;
    function GetAxisMode: TAxisMode;
    function GetDrawAngle: Extended;
    //function GetDrawEndPoint: TPoint;
    //function GetDrawEndPointRel: TPoint;
    function GetDrawLength: Extended;
    function GetDrawOrigin: TPoint;
    function GetDrawOriginRel: TPoint;
    function GetSeriesUnits: TStrings;
    function GetValueColor(AValue : Extended): TColor;
    function GetValueFPColor(AValue : Extended): TFPColor;
    function GetValuePerPixel: Extended;
    function GetVisualParams: TAxisVisualParams;

    procedure SetAutoLength(const AValue: Boolean);
    procedure SetAutoLengthMode(const AValue: TAutoLengthMode);
    procedure SetAutoPlacedLength(AValue: Extended);
    procedure SetAutoPlacedOriginRel(AValue: TPoint);
    procedure SetAxisLabel(const AValue: String);
    procedure SetAxisMode(AValue: TAxisMode);
    procedure SetDrawAngle(AValue: Extended);
    procedure SetDrawLength(const AValue: Extended);
    procedure SetDrawOriginRel(const AValue: TPoint);
    procedure SetInner3DTicks(AValue: Boolean);
    procedure SetInnerSubTicks(const AValue: Boolean);
    procedure SetInnerTicks(const AValue: Boolean);
    procedure SetLogBase(const AValue: Extended);
    procedure SetLogScale(const AValue: Boolean);
    procedure SetOriginMode(const AValue: TOriginMode);
    procedure SetSubGrain(const AValue: Integer);
    procedure SetSubTickLength(const AValue: integer);
    procedure SetSubTickMarks(const AValue: boolean);
    procedure SetSubTicks(const AValue: boolean);
    procedure SetSubTickStyle(const AValue: TAxisStyle);
    procedure SetTickAngle(const AValue: TTickAngle);
    procedure SetTickLength(const AValue: integer);
    procedure SetTickMarks(const AValue: boolean);
    procedure SetTicks(const AValue: boolean);
    procedure SetTickStyle(const AValue: TAxisStyle);
    procedure SetValuePerPixel(const AValue: Extended);

  protected
    function GetViewRange: TValueRange;override;
    procedure SetViewRange(AValue: TValueRange); override;
    function GetPixelsPerValue: Extended; override;
    //procedure Redraw; override;
    function ReDraw(ADrawVisible:Boolean): TRect; override; // delivers used rect now
    // from 26.09.14 delivers distance for each side, additional to DataRect (i.e. shrink datarect by this amount);
    // respects also cloneaxes.
    //function DrawTicks: Integer; // includes drawing of marks
    procedure DrawInnerTicks; // only axes in FInnerGridAxes
    function DrawLabel(ACenterPt: TPoint; AVisualParams: TAxisVisualParams; ADrawvisible: Boolean): TRect;  // new code, old see before 14.10.14
    function DrawUnits(ACenterPt: TPoint; AVisualParams: TAxisVisualParams; ADrawvisible: Boolean): TRect;  // new code, old see before 14.10.14
  public
    constructor Create(AOwnerPlot: TPlot); override;
    destructor Destroy; override;
    function CheckSize(out ANetAxisRect: TRect): TRect; override; // delivers used rect now

    function ReCalcValue(AValue: Extended): Extended;
    function ReCalcValueInverse(AValue: Extended): Extended;
    procedure AddInnerGridAxis(AAxisIndex: Integer);
    procedure RemoveInnerGridAxis(AAxisIndex: Integer);
    procedure Set3DGridAxis(AAxisIndex: Integer);     // 02.08.14
    function Get3DGridAxis: Integer;     // 08.08.14
    procedure Remove3DGridAxis;  // 02.08.14
    procedure AddCloneAxis(AParams: TCloneAxisParams);
    //procedure RemoveIndicatorAxis(AIndex: Integer); // TODO
    procedure PanPixel(dX, dY: Integer);
    procedure ZoomPixel(X, Y: Integer; AFactor: Extended);

    //
    function DrawIndicatorAxis(AParams: TAxisVisualParams; ADrawVisible: Boolean): Integer;
    //function DrawCloneAxis(AParams: TCloneAxisParams; ADrawVisible: Boolean): Integer;
    function DrawCloneAxis(AParams: TCloneAxisParams; ADrawVisible: Boolean; out AUsedRect: TRect): Integer;

    //procedure DrawInnerTicks(AInnerTickAxis: Integer; ADataImage: TLazIntfImage; AFPColor: TFPColor ;ATicks, ASubTicks: Boolean); // used for 3D rolling grid only
    //
    // 02.10.12 introduce OriginMode omRel, omAbs
    // DrawOriginRel is relative to 0/0 of PlotRect otherwise cconstant given in pixels (new omAbs)
    // new mode omRel is in % of axislength
    property OriginMode : TOriginMode read FOriginMode write SetOriginMode;
    property DrawOriginRel : TPoint read GetDrawOriginRel write SetDrawOriginRel;  // % of DataImage !? //  relative to plotrect, used only in TPlotAxis
    property DrawOrigin : TPoint read GetDrawOrigin; // points relative to DataImage !? //relative to global image, used (only) in uPoltUtils to bring point to screen
    property DrawAngle : Extended read GetDrawAngle write SetDrawAngle;             // in degrees
    property AutoMode : TAutoLengthMode read FAutoLengthMode write SetAutoLengthMode;
    property DrawLength : Extended read GetDrawLength write SetDrawLength;    // percent of PlotRect see AutoMode in pixels
    property AutoLength : Boolean read FAutoLength write SetAutoLength;
    property PixelsPerValue : Extended read GetPixelsPerValue;
    //
    property LogScale : Boolean read FLogScale write SetLogScale;
    property LogBase : Extended read FLogBase write SetLogBase;
    property Ticks : boolean read FTicks write SetTicks;
    property SubTicks : boolean read FSubTicks write SetSubTicks;
    property TickLength : integer read FTickLength write SetTickLength;
    property SubTickLength : integer read FSubTickLength write SetSubTickLength;
    property InnerTicks: Boolean read FInnerTicks write SetInnerTicks;
    property InnerSubTicks: Boolean read FInnerSubTicks write SetInnerSubTicks;
    property Inner3DTicks: Boolean read FInner3DTicks write SetInner3DTicks;
    property Marks : boolean read FTickMarks write SetTickMarks;
    property SubMarks : boolean read FSubTickMarks write SetSubTickMarks;
    property TickAngle : TTickAngle read FTickAngle write SetTickAngle;
    property SubTickGrain : Integer read FSubGrain write SetSubGrain;
    property AxisLabel: String read FAxisLabel write SetAxisLabel;
    property ValueColor[AValue : Extended] : TColor read GetValueColor;
    property ValueFPColor[AValue : Extended] : TFPColor read GetValueFPColor;
    property TickStyle: TAxisStyle read FTickStyle write SetTickStyle;
    property SubTickStyle: TAxisStyle read FSubTickStyle write SetSubTickStyle;
    property ValuePerPixel: Extended read GetValuePerPixel  write SetValuePerPixel;  // needed for waterfalls see AxisMode
    property AxisMode: TAxisMode read GetAxisMode write SetAxisMode;
    property SeriesUnits: TStrings read GetSeriesUnits;
    //
    property VisualParams: TAxisVisualParams read GetVisualParams;
    //property DrawEndPoint: TPoint read GetDrawEndPoint;
    //property DrawEndPointRel: TPoint read GetDrawEndPointRel;
    //
    property RotateLabelText: Boolean read FRotateLabelText write FRotateLabelText;
    property RotateUnitText: Boolean read FRotateUnitText write FRotateUnitText;
    //
    property NumberFormat: TNumberFormat read FNumberFormat write FNumberFormat;
    property AutoPlacedLength: Extended read GetAutoPlacedLength write SetAutoPlacedLength;           // set by PlotRect in axisatoplacefill mode
    property AutoPlacedOriginRel: TPoint read GetAutoPlacedOriginRel write SetAutoPLacedOriginRel;    // set by PlotRect in axisatoplacefill mode
  end;

const
  TNumberFormatName : array[nfPlain..nfEngineeringPrefix] of String = (
   'Plain Number', 'float (1.10e4)', 'engineering (11.0e3)', 'engineering prefix (11k)');

implementation

uses uPlotUtils, uPlotRect;

//resourcestring
//  S_RelModeMaxExceeded  = 'Relative mode used but high shift value detected.'#13#10+
//                        'Please give relative distance in 0..100%';

{ TPlotAxis }

procedure TPlotAxis.SetDrawOriginRel(const AValue: TPoint);
begin
  FDrawOrigin:=AValue;
end;

procedure TPlotAxis.SetInner3DTicks(AValue: Boolean);
begin
  if FInner3DTicks=AValue then Exit;
  FInner3DTicks:=AValue;
end;


procedure TPlotAxis.SetInnerSubTicks(const AValue: Boolean);
begin
  if FInnerSubTicks=AValue then exit;
  FInnerSubTicks:=AValue;
end;


procedure TPlotAxis.SetInnerTicks(const AValue: Boolean);
begin
  if FInnerTicks=AValue then exit;
  FInnerTicks:=AValue;
end;

procedure TPlotAxis.SetLogBase(const AValue: Extended);
begin
  if FLogBase=AValue then exit;
  FLogBase:=AValue;
end;

procedure TPlotAxis.SetLogScale(const AValue: Boolean);
var vViewRange: TValueRange;
begin
  if FLogScale=AValue then exit;
  FLogScale:=AValue;
  // catch possible errors
  IF ViewRange.min <= 0 THEN BEGIN
    vViewRange.min := c_GLOBALMIN;
    vViewRange.max := ViewRange.max;
    ViewRange := vViewRange;
  END;
end;

procedure TPlotAxis.SetOriginMode(const AValue: TOriginMode);
begin
  if FOriginMode=AValue then exit;
  FOriginMode:=AValue;
end;

procedure TPlotAxis.SetSubGrain(const AValue: Integer);
begin
  if FSubGrain=AValue then exit;
  FSubGrain:=AValue;
end;

procedure TPlotAxis.SetSubTickLength(const AValue: integer);
begin
  if FSubTickLength=AValue then exit;
  FSubTickLength:=AValue;
end;

procedure TPlotAxis.SetSubTickMarks(const AValue: boolean);
begin
  if FSubTickMarks=AValue then exit;
  FSubTickMarks:=AValue;
end;       


procedure TPlotAxis.SetSubTicks(const AValue: boolean);
begin
  if FSubTicks=AValue then exit;
  FSubTicks:=AValue;
end;


procedure TPlotAxis.SetSubTickStyle(const AValue: TAxisStyle);
begin
  if FSubTickStyle=AValue then exit;
  FSubTickStyle:=AValue;
end;

procedure TPlotAxis.SetTickAngle(const AValue: TTickAngle);
begin
  if FTickAngle=AValue then exit;
  FTickAngle:=AValue;
end;
                                        
procedure TPlotAxis.SetTickLength(const AValue: integer);
begin
  if FTickLength=AValue then exit;
  FTickLength:=AValue;
end;

procedure TPlotAxis.SetTickMarks(const AValue: boolean);
begin
  if FTickMarks=AValue then exit;
  FTickMarks:=AValue;
end;


procedure TPlotAxis.SetTicks(const AValue: boolean);
begin
  if FTicks=AValue then exit;
  FTicks:=AValue;
end;


procedure TPlotAxis.SetTickStyle(const AValue: TAxisStyle);
begin
  if FTickStyle=AValue then exit;
  FTickStyle:=AValue;
end;

procedure TPlotAxis.SetValuePerPixel(const AValue: Extended);
begin
  FAxisMode := amValuePerPixel;
  FValuePerPixel := AValue;
end;

function TPlotAxis.GetViewRange: TValueRange;
begin
  IF AxisMode = amPixelPerValue THEN Result:=inherited GetViewRange
  ELSE BEGIN
    Result.min := 0;
    Result.max := math.max(DrawLength * ValuePerPixel, c_GLOBALMIN);
  END;
end;

function TPlotAxis.ReCalcValue(AValue: Extended): Extended;
begin
  //rercalc only when log scale
  IF LogScale THEN
  Result := logn(LogBase, AValue)
  ELSE
  Result := AValue;
end;

function TPlotAxis.ReCalcValueInverse(AValue: Extended): Extended;
begin
// inverse recalc only when logscale
  IF (LogScale) THEN
  Result := power(LogBase, AValue) ELSE
  Result := AValue;
end;

procedure TPlotAxis.SetViewRange(AValue: TValueRange);
begin
  IF LogScale THEN BEGIN
    IF ((AValue.min <= 0) ) THEN AValue.min := c_GLOBALMIN;
    IF ((AValue.max <= AValue.min) ) THEN AValue.max := AValue.max + c_GLOBALMIN;
  END ELSE BEGIN
    IF AValue.min < -c_GLOBALMAX THEN AValue.min := -c_GLOBALMAX;
    IF AValue.max > c_GLOBALMAX THEN AValue.max := c_GLOBALMAX;
  END;
  AxisMode:=amPixelPerValue;
  inherited SetViewRange(AValue);
  // AxisMode := amPixelPerValue;  28.10. do not set back, called by setvalueperpixel
end;


constructor TPlotAxis.Create(AOwnerPlot: TPlot);
begin
  inherited Create(AOwnerPlot);
  FInnerGridAxes := TList.Create;
  FCloneAxes := TList.Create;
  FDrawAngle := 0;
  FDrawLength := 100;
  FAutoLength := TRUE;
  FAutoLengthMode := lmRelToSmallSide;
  FDrawOrigin.X := 0 ; FDrawOrigin.Y := 0;

  FAutoPlacedLength := 1;
  AutoPLacedOriginRel := Point(0,0);
  //
  Ticks := TRUE;
  SubTicks := TRUE;
  TickLength := 10;
  SubTickLength := 5;
  Marks := TRUE;
  SubMarks := TRUE;
  //
  FTickStyle := TAxisStyle.Create;
  TickStyle.Font.Size := 8;
  TickStyle.PenInnerGrid.Style := psDash;
  FSubTickStyle := TAxisStyle.Create;
  SubTickStyle.Font.Size := 6;
  SubTickStyle.PenInnerGrid.Style := psDot;
  //
  TickAngle := taPositive;
  SubTickGrain := 10;
  //
  FInnerTicks:=TRUE;
  FInnerSubTicks:=TRUE;
  //
  FInner3DTicks:=false;
  //
  FAxisMode := amPixelPerValue;
  //
  FLogScale:=FALSE;
  FLogBase:=10;
  //
  FAxisLabel:='label';
  FRotateUnitText := TRUE;
  FRotateLabelText := TRUE;
  //
  FNumberFormat := nfEngineering;
  //
  FOriginMode := omAbs; // in the most used mode (origin is bottom left of PlotRect) please use omAbs

  F3DGridAxis:=-1;
  with FTickInfo do begin
      mainlow :=-c_GLOBALMIN;
      mainhigh := c_GLOBALMAX;
      maininterval := 1;
  end;

  FValuePerPixel:=1;
end;

destructor TPlotAxis.Destroy;
begin
  inherited Destroy;
  FTickStyle.Free;
  FSubTickStyle.Free;
  while FInnerGridAxes.Count > 0  do begin
    Freemem(FInnerGridAxes.Items[0]);
    FInnerGridAxes.Delete(0);
  end;
  FInnerGridAxes.Free;
  while FCloneAxes.Count > 0  do begin
    Freemem(FCloneAxes.Items[0]);
    FCloneAxes.Delete(0);
  end;
  FCloneAxes.Free;
end;

function TPlotAxis.CheckSize(out ANetAxisRect: TRect): TRect;
begin
  Result:=inherited CheckSize(ANetAxisRect);
end;

procedure TPlotAxis.AddInnerGridAxis(AAxisIndex: Integer);
var
  vItem : PInteger;
begin
  try
    new(vItem);
    try
      vItem^ := AAxisIndex;
      FInnerGridAxes.Add(vItem);
    except
       Dispose(vItem); raise;
    end;
  except
    raise;
  end;
end;

procedure TPlotAxis.RemoveInnerGridAxis(AAxisIndex: Integer);
var
  vItem: PInteger;
  vLoop: Integer;
begin
  FOR vLoop := FInnerGridAxes.Count - 1 downto 0 DO BEGIN
    vItem := FInnerGridAxes.Items[vLoop];
    IF vItem^ = AAxisIndex THEN BEGIN
      FInnerGridAxes.Remove(FInnerGridAxes.Items[vLoop]);
      dispose(vItem);
    END;
  END;
end;

procedure TPlotAxis.Set3DGridAxis(AAxisIndex: Integer);
begin
  if F3DGridAxis = AAxisIndex then exit;
  if (OwnerPlot.Axis[AAxisIndex] = Self) then exit;

  F3DGridAxis := AAxisIndex;
end;

function TPlotAxis.Get3DGridAxis: Integer;
begin
  Result := F3DGridAxis;
end;

procedure TPlotAxis.Remove3DGridAxis;
begin
  F3DGridAxis:=-1;
end;

procedure TPlotAxis.AddCloneAxis(AParams: TCloneAxisParams);
var
  vItem : PCloneAxisParams;
begin
  try
    new(vItem);
    try
      vItem^ := AParams;
      FCloneAxes.Add(vItem);
    except
       Dispose(vItem); raise;
    end;
  except
    raise;
  end;
end;

procedure TPlotAxis.PanPixel(dX, dY: Integer);
var
  vShift: Extended;
  vFactor: Extended;
  vViewRange: TValueRange;
begin
  // writeln('do panning...X/Y ', IntToStr(dX), '/', IntToStr(dY));
  // attention: PixelPerValue and ValuePerPixel are questionalble in LogScale mode
  // we check if logscale
  // shift is additive (in fraction of viewrange) for non logscale
  // shift is multiplicative  (in fraction of viewrange) for logscale
  // divide by drawangle !
  vShift := dX * cos(DrawAngle * Pi() / 180) + dY * sin(DrawAngle * Pi() / 180);
  vFactor := vShift / DrawLength;
  if LogScale then vFactor := ReCalcValueInverse( vFactor * ReCalcValue(ViewRange.max / ViewRange.min) );
  //writeln('factor: ', FloatToStrF(vFactor, ffExponent, 4, 4));
  vViewRange := ViewRange;
  //writeln('old VR: ', FloatToStrF(vViewRange.min, ffExponent, 4, 4), ' / ', FloatToStrF(vViewRange.min, ffExponent, 4, 4));
  CASE LogScale OF
    TRUE:  begin
             vViewRange.min := (vFactor) * vViewRange.min;
             vViewRange.max := (vFactor) * vViewRange.max;
           end;
    FALSE: begin
             vViewRange.min := vViewRange.min + vFactor * (vViewRange.max - vViewRange.min);
             vViewRange.max := vViewRange.max + vFactor * (vViewRange.max - vViewRange.min);
           end;
  END;
  //writeln('new VR: ', FloatToStrF(vViewRange.min, ffExponent, 4, 4), ' / ', FloatToStrF(vViewRange.min, ffExponent, 4, 4));
  ViewRange := vViewRange;
end;

procedure TPlotAxis.ZoomPixel(X, Y: Integer; AFactor: Extended);
var
  vValue: Extended;
  vMathPt: TFloatPoint;
  vError: Integer;
  vViewRange: TValueRange;
  vMindB, vMaxdB: Extended;
begin
  vMathPt.X := X - DrawOrigin.X;
  vMathPt.Y := -(Y - DrawOrigin.Y);
  vError := MathCoordToValue(Self, vMathPt, vValue);

  IF (vError < 0) then exit;

  vViewRange := ViewRange;

  CASE LogScale OF
    FALSE: begin
             vViewRange.min := vValue -  AFactor * abs(vValue - vViewRange.min);
             vViewRange.max := vValue +  AFactor * abs(vViewRange.max - vValue);
           end;
    TRUE:  begin
             vMindB := ReCalcValue(ViewRange.min);
             vMaxdB := ReCalcValue(ViewRange.max);
             vViewRange.min := vValue / ReCalcValueInverse(AFactor * (ReCalcValue(vValue) - vMindB));
             vViewRange.max := vValue * ReCalcValueInverse(AFactor * (vMaxdB - ReCalcValue(vValue)));
           end;
  END;
  ViewRange := vViewRange;

end;



function TPlotAxis.DrawIndicatorAxis(AParams: TAxisVisualParams; ADrawVisible: Boolean): Integer;
// Returns width used in normal angle
// TODO: modularize ticks, subticks,
var
  vMinMax : TValueRange;
  vValueRange, vMainInterval: Extended;
  vMainCount, vLoop, vLoopGrain : Integer;
  vMainLow, vMainHigh, vMain, vSub : Extended;
  vR : Extended;
  vPtA, vPtB : TPoint;
  vNormalAngle : Extended;
  vtextsize, vtexthight, vtextwidth, vxshift, vyshift: integer;
  vText : String;
  vDrawCanvas : TCanvas;
  vAngle : Extended;
  vDrawAngle: Extended;
  vDrawOrigin: TPoint;
  vdx, vdy: Integer;
  vEndPoint: TPoint;
  vmaxwidth, vmaxheight: Integer;
  vSubNumbers: Set of byte;
  vMainTotalSize: integer;
  vOnlyTwoMainMarks: Boolean;
  vNumSubMarks: integer;
  vSubReduceFactor: Extended;
  vDigits: Integer;
const
  cTEXTSPREAD = 1.1;
begin
//variable init
Result := 0;
vMainLow := 0; vMainHigh := 0; vMain := 0; vSub := 0;
vMainInterval := 0; vValueRange := 0;
vDrawAngle := 0;
vR := 0; vAngle := 0;
vPtA.X := 0; vPtA.Y := 0;
vPtB.X := 0; vPtB.Y := 0;
vOnlyTwoMainMarks := FALSE;

IF LogScale THEN vSubNumbers := [2,3,4,5,6,7,8,9] ELSE vSubNumbers := [1,2,3,4,5,6,7,8,9];
//vMainNumbers := [0,1,2,3,4,5,6,7,8,9];
vDrawOrigin := AParams.ptA;

  vmaxwidth:=0; vmaxheight:=0;
  vdx:=AParams.ptB.X - AParams.ptA.X ; vdy:= -AParams.ptB.Y + AParams.ptA.Y;
  IF vdx = 0 THEN vDrawAngle := (Sign(vdy) * 90) ELSE vDrawAngle := arctan2(vdy,vdx)*180/Pi;

  IF NOT AParams.Visible THEN ADrawVisible:=false;
  // TODO: do we need to draw an invisible axis ?
  vDrawCanvas := AParams.DrawCanvas;

  // draw lines
  vEndPoint := AParams.ptB;
  IF ADrawVisible THEN uPlotStyles.TAxisStyle(AParams.Style).DrawLine(vDrawOrigin, vEndPoint, vDrawCanvas);

  case  AParams.TickAngle of
  taPositive :  begin
                  vNormalAngle := (vDrawAngle + 90);
                end;
  taNegative : begin
                  vNormalAngle := (vDrawAngle -90);
                end;
  end;
  // bug in fpc ?  LineEndPoint accepts only 0..360 degrees (*16)
  while vNormalAngle > 360 do vNormalAngle := vNormalAngle - 360;
  while vNormalAngle < 0 do vNormalAngle := vNormalAngle + 360;

  // ---- caclulate the main interval(s) --------------------------------------
  vMinMax := ViewRange;

  vValueRange := (vMinMax.max) - (vMinMax.min);
  IF vValueRange < c_GLOBALMIN THEN vValueRange := c_GLOBALMIN;
  IF vValueRange > c_GLOBALMAX THEN vValueRange := c_GLOBALMAX;

  vMainHigh := MainTickRange(ViewRange, LogBase, LogScale).max;
  vMainLow := MainTickRange(ViewRange, LogBase, LogScale).min;
  vMainInterval := MainTickInterval(ViewRange, LogBase, LogScale);
  if vMainInterval = 0 then vMainInterval:=c_GLOBALMIN;

  // 09.10.14; number of fractional digits according to maininterval
  vDigits := DigitsNeeded(vMainInterval);

  IF AParams.Ticks THEN BEGIN
      IF LogScale THEN begin
        vMainCount := round(logn(LogBase, vMainHigh) - logn(LogBase,vMainLow)) + 1
      end
      ELSE begin
        vMainCount := round((vMainHigh - vMainlow)/vMainInterval); //+1;
      end;

  // adjust number of textmarks... ***************************
  // main....
     vText := FormatNumber(vMainHigh, NumberFormat, vDigits);
     IF abs(sin(DrawAngle)) < (0.5*sqrt(2)) THEN begin
       vtextsize := AParams.TickStyle.TextWidth[vText, vDrawCanvas];
       vtextsize := round(vtextsize / abs(cos(DrawAngle)) * cTEXTSPREAD);
     end
     ELSE begin vtextsize := AParams.TickStyle.TextHeigth[vText, vDrawCanvas];
                vtextsize := round(vtextsize / abs(sin(DrawAngle)) * cTEXTSPREAD);
     end;
     IF ((vMainCount * vtextsize) > DrawLength) and ADrawVisible THEN BEGIN  // 13.10.14 respect drawvisible, always all elements when invisible draw
       vMainTotalSize := 2* vtextsize;
       vOnlyTwoMainMarks:= TRUE;
       vSubNumbers := [];
       END
     ELSE vMainTotalSize := (vMainCount * vtextsize);

    // sub....
     IF (not vOnlyTwoMainMarks) THEN BEGIN        // TODO: delete vOnly... when subreduce works 100%
        vtextsize := 0;
        for vLoop := -1 to vMainCount do begin
          IF LogScale THEN vMain:= vMainlow * power(LogBase, (vLoop*vMainInterval))
          ELSE vMain:= vMainlow + (vLoop*vMainInterval);
          for vLoopGrain:= 1 to SubTickGrain-1 do begin              // -1 avoids drawing on main tick
            IF LogScale THEN vSub := vMain  * (0 + (LogBase / SubTickGrain * vLoopGrain))
            ELSE vSub := vMain  + (vMainInterval / SubTickGrain)*vLoopGrain;
              IF (vSub < vMinMax.min) OR (vSub > vMinMax.max) THEN continue;
              IF LogScale AND (vSub <= 0) THEN continue;
              vText := '__' + FormatNumber(vSub, NumberFormat, vDigits);
              IF abs(sin(DrawAngle)) < (0.5*sqrt(2)) THEN begin
                vtextsize := AParams.SubTickStyle.TextWidth[vText, vDrawCanvas];
                vtextsize := round(vtextsize / abs(cos(DrawAngle)) * cTEXTSPREAD);
              end
              ELSE begin vtextsize := AParams.SubTickStyle.TextHeigth[vText, vDrawCanvas];
                         vtextsize := round(vtextsize / abs(sin(DrawAngle)) * cTEXTSPREAD);
              end;
          end;
        end;


        IF LogScale THEN vNumSubMarks := round( (ReCalcValue(vMinMax.max) - ReCalcValue(vMinMax.min)) / (vMainInterval / SubTickGrain) )
        ELSE vNumSubMarks := round( vValueRange / (vMainInterval / SubTickGrain) );      // 09.09.14 try recalc here
        IF DrawLength = 0 THEN vSubReduceFactor := c_GLOBALMAX ELSE
          vSubReduceFactor := ((vNumSubMarks * vtextsize) - vMainTotalSize ) / DrawLength;

        IF (vSubReduceFactor > 1) and ADrawVisible THEN BEGIN
          CASE LogScale OF
          TRUE:  BEGIN
                   vSubNumbers := [];
                   IF vSubReduceFactor < 2 THEN vSubNumbers := [2,5] ELSE
                   IF vSubReduceFactor < 5 THEN vSubNumbers := [2];
                 END;
          FALSE: BEGIN
                   vSubNumbers := [];
                   IF vSubReduceFactor < 2 THEN vSubNumbers := [2,4,6,8] ELSE
                   IF vSubReduceFactor < 5 THEN vSubNumbers := [5];
                 END;
          END;
        END;
     END;
    // end adjust marks ****************************************


  // Draw mainticks ------------------------------------------------------------
    for vLoop := 0 to vMainCount-1 do begin
      IF LogScale THEN vMain:= vMainlow * power(LogBase, (vLoop*vMainInterval))
      ELSE vMain:= vMainlow + (vLoop*vMainInterval);
      IF (vMain < vMinMax.min) OR (vMain > vMinMax.max) THEN continue;
      vR:= round((ReCalcValue(vMain) - ReCalcValue(vMinMax.Min)) * AParams.PixelsPerValue);
      vPtA := LineEndPoint(vDrawOrigin, vDrawAngle*16, vR);
      vPtB := LineEndPoint(vPtA, vNormalAngle*16, AParams.TickLength);
      // cannot use uPlotUtils here
      //vDrawCanvas.Pen := TickStyle.Pen;
      IF ADrawVisible THEN uPlotStyles.TAxisStyle(AParams.TickStyle).DrawLine(vPtA, vPtB, vDrawCanvas);
      // Text to show for mainticks: (Marks)
      IF AParams.Marks THEN BEGIN
        vText := FormatNumber(vMain, NumberFormat, vDigits); //vText := '1e' + FormatNumber(vMain, nfFloat) ;
        vtextwidth := AParams.TickStyle.TextWidth[vText, vDrawCanvas];
        vtexthight := AParams.TickStyle.TextHeigth[vText, vDrawCanvas];
        // store max
        vmaxwidth := Max(vmaxwidth, vtextwidth);
        vmaxheight:= Max(vmaxheight, vtexthight);
        //
        IF AParams.TickAngle = taNegative THEN BEGIN
          vxshift := - round( (0.5 - 0.5*sin(vDrawAngle*pi/180)) * vtextwidth);
          vyshift := round( 0.5*(-sin((vDrawAngle)*pi/360)) * vtexthight);
        END;
        IF AParams.TickAngle = taPositive THEN BEGIN
          vAngle := vDrawAngle;
          vxshift := - round( (0.5 + 0.5*sin((0+vAngle)*pi/180)) * vtextwidth);
          while (vAngle+180) > 360 do vAngle := vAngle - 360;
          while (vAngle+180) < 0 do vAngle := vAngle + 360;
          vyshift := round( 0.5*(-sin((180+vAngle)*pi/360)) * vtexthight);
        END;
        IF (not vOnlyTwoMainMarks) OR ((vMain) < (vMinMax.min + vMainInterval))  OR (vLoop = vMainCount-1) THEN     // -1 correct?
        IF ADrawVisible THEN uPlotStyles.TPlotStyle(Self.TickStyle).DrawTextEx(vPtB.X + vxshift, vPtB.Y + vyshift,
          0, vText, vDrawCanvas);
      END; // Marks
    end;
  END;   // end drawing mainticks ----------------------------------------------


  // draw subticks  ------------------------------------------------------------
  IF AParams.SubTicks THEN BEGIN
    for vLoop := -1 to vMainCount do begin
      IF LogScale THEN vMain:= vMainlow * power(LogBase, (vLoop*vMainInterval))
      ELSE vMain:= vMainlow + (vLoop*vMainInterval);

      for vLoopGrain:= 1 to SubTickGrain-1 do begin              // -1 avoids drawing on main tick
      if vLoopGrain in vSubNumbers then begin                                    // comment if always all subticks to draw
        IF LogScale THEN vSub := vMain  * (0 + (LogBase / SubTickGrain * vLoopGrain))
        ELSE vSub := vMain  + (vMainInterval / SubTickGrain)*vLoopGrain;
          IF (vSub < vMinMax.min) OR (vSub > vMinMax.max) THEN continue;
          IF LogScale AND (vSub <= 0) THEN continue;
          vR:= round(( ReCalcValue(vSub) - ReCalcValue(vMinMax.Min)) * AParams.PixelsPerValue);
          vPtA := LineEndPoint(vDrawOrigin, vDrawAngle*16, vR);
          vPtB := LineEndPoint(vPtA, vNormalAngle*16, AParams.SubTickLength);
          //vDrawCanvas.Pen := SubTickStyle.Pen;
          IF ADrawVisible THEN uPlotStyles.TAxisStyle(AParams.SubTickStyle).DrawLine(vPtA, vPtB, vDrawCanvas);

          vText := FormatNumber((vSub), NumberFormat, vDigits);
          vtextwidth := AParams.SubTickStyle.TextWidth[vText, vDrawCanvas];
          vtexthight := AParams.SubTickStyle.TextHeigth[vText, vDrawCanvas];
          // store max
          vmaxwidth := Max(vmaxwidth, vtextwidth);
          vmaxheight:= Max(vmaxheight, vtexthight);
          //
          IF AParams.TickAngle = taNegative THEN BEGIN
            vxshift := - round( (0.5 - 0.5*sin(vDrawAngle*pi/180)) * vtextwidth);
            vyshift := round( 0.5*(-sin((vDrawAngle)*pi/360)) * vtexthight);
          END;
          IF AParams.TickAngle = taPositive THEN BEGIN
            vAngle := vDrawAngle;
            vxshift := - round( (0.5 + 0.5*sin((0+vAngle)*pi/180)) * vtextwidth);
            while (vAngle+180) > 360 do vAngle := vAngle - 360;
            while (vAngle+180) < 0 do vAngle := vAngle + 360;
            vyshift := round( 0.5*(-sin((180+vAngle)*pi/360)) * vtexthight);
          END;
          //IF vLoopGrain in vSubNumbers THEN                                    // uncomment if only text shall be omitted
          IF ADrawVisible THEN uPlotStyles.TPlotStyle(Self.SubTickStyle).DrawTextEx(vPtB.X + vxshift, vPtB.Y + vyshift,
            0, vText, vDrawCanvas);
        end;
      end;

    end;
  END; // end drawing subticks -------------------------------------------------

// return the distance in normal direction which was used for text, ticks etc.
  IF (not AParams.Ticks) and (not AParams.SubTicks) THEN Result := 0
  ELSE IF (not AParams.Marks) and (not AParams.SubMarks) THEN Result := round( Max(AParams.TickLength, AParams.SubTickLength)* abs(cos(vNormalAngle*Pi/180))  )
  ELSE Result := round( Max(AParams.TickLength, AParams.SubTickLength) +
             vmaxwidth * abs(cos(vNormalAngle*Pi/180)) + vmaxheight * abs(sin(vNormalAngle*Pi/180)) );

  IF not AParams.Visible then Result := 0;

  with FTickInfo do begin
    mainlow := vMainLow;
    mainhigh := vMainHigh;
    maininterval := vMainInterval;
    maincount:= vMainCount;
    SubNumbers:=vSubNumbers;
  end;
end;


function TPlotAxis.DrawCloneAxis(AParams: TCloneAxisParams;
  ADrawVisible: Boolean; out AUsedRect: TRect): Integer;
var
  vAxisParams: TAxisVisualParams;
  vValue: Extended;
  vPtA: TPoint;
  vPtB: TFloatPoint;
  vNormalAngle: Extended;
  vPtA2, vPtB2: TPoint;
  vLabelUsedRect, vUnitsUsedRect: TRect;
  vLabelCenterPt, vUnitsCenterPt: TPoint;
begin
  //IF not Visible then exit;
  //IF (not Visible) then ADrawVisible:=false;
  Result := 0; // TODO: what do we do with the result ?
  vAxisParams := AParams.VisualParams;
  vAxisParams.DrawCanvas := VisualParams.DrawCanvas;
  vAxisParams.PixelsPerValue := VisualParams.PixelsPerValue;
  {vValue := AParams.ShiftLengthRel * uPlotClass.TPlotAxisBase(AParams.ShiftAxis).ViewRange.max / 100;
  ValueToMathCoord(AParams.ShiftAxis, vValue, vShift);    // TODO: error checking   }

  vValue := AParams.ShiftLengthRel * AParams.ShiftAxis.ViewRange.max / 100;
  ValueToScreen((AParams.ShiftAxis), vValue, vPtA);
  vValue := ViewRange.max;
  ValueToMathCoord(Self, vValue, vPtB);

  vAxisParams.ptA := vPtA;
  vAxisParams.ptB.X := round(vPtB.X) + vPtA.X;
  vAxisParams.ptB.Y := - round(vPtB.Y) + vPtA.Y;
  Result := DrawIndicatorAxis(vAxisParams, ADrawVisible);

  // deliver usedrect --------------------------------------
  // axis line only...
  AUsedRect.Left := math.min(vAxisParams.ptA.X, vAxisParams.ptB.X);
  AUsedRect.Right := math.max(vAxisParams.ptA.X, vAxisParams.ptB.X);
  AUsedRect.Top := math.min(vAxisParams.ptA.Y, vAxisParams.ptB.Y);
  AUsedRect.Bottom := math.max(vAxisParams.ptA.Y, vAxisParams.ptB.Y);

  with FNetAxisRect do begin
    Left := min(Left, AUsedRect.Left);
    Right := max(Right, AUsedRect.Right);
    Top := min(Top, AUsedRect.Top);
    Bottom := max(Bottom, AUsedRect.Bottom);
  end;

  // add to datarect estimateion
  // vSpacing includes axis ticks and tick-text
  // see other routines --> modularize !
  case  vAxisParams.TickAngle of
  taPositive :  begin
                  vNormalAngle := (DrawAngle + 90);
                end;
  taNegative : begin
                  vNormalAngle := (DrawAngle -90);
                end;
  end;
  vPtA2 := LineEndPoint(vAxisParams.ptA, vNormalAngle*16, Result );
  vPtB2 := LineEndPoint(vAxisParams.ptB, vNormalAngle*16, Result );

  AUsedRect.Left :=  math.MinIntValue([AUsedRect.Left, vPtA2.X, vPtB2.X]);
  AUsedRect.Right :=  math.MaxIntValue([AUsedRect.Right, vPtA2.X, vPtB2.X]);
  AUsedRect.Top :=  math.MinIntValue([AUsedRect.Top, vPtA2.Y, vPtB2.Y]);
  AUsedRect.Bottom :=  math.MaxIntValue([AUsedRect.Bottom, vPtA2.Y, vPtB2.Y]);
  // above yields total rect used by axis including ticks and tickmarks

  // Draw / Check label --------------------------------------------------------
  // calc center point
  // center is between A2 and B2 (only canvas math !)
  vLabelCenterPt.X := vPtA2.X + ( (vPtB2.X - vPtA2.X) DIV 2 );
  vLabelCenterPt.Y := vPtA2.Y + ( (vPtB2.Y - vPtA2.Y) DIV 2 );

  vLabelUsedRect := DrawLabel(vLabelCenterPt, AParams.VisualParams, ADrawVisible);

  AUsedRect.Left :=  math.MinIntValue([AUsedRect.Left, vLabelUsedRect.Left]);
  AUsedRect.Right :=  math.MaxIntValue([AUsedRect.Right, vLabelUsedRect.Right]);
  AUsedRect.Top :=  math.MinIntValue([AUsedRect.Top, vLabelUsedRect.Top]);
  AUsedRect.Bottom :=  math.MaxIntValue([AUsedRect.Bottom, vLabelUsedRect.Bottom]);

  // Draw / Check units --------------------------------------------------------
  // calc center point
  // center is between A2 and B2 (only canvas math !)
  vUnitsCenterPt := vPtB2;

  vUnitsUsedRect := DrawUnits(vUnitsCenterPt, AParams.VisualParams, ADrawVisible);

  AUsedRect.Left :=  math.MinIntValue([AUsedRect.Left, vUnitsUsedRect.Left]);
  AUsedRect.Right :=  math.MaxIntValue([AUsedRect.Right, vUnitsUsedRect.Right]);
  AUsedRect.Top :=  math.MinIntValue([AUsedRect.Top, vUnitsUsedRect.Top]);
  AUsedRect.Bottom :=  math.MaxIntValue([AUsedRect.Bottom, vUnitsUsedRect.Bottom]);
end;

procedure TPlotAxis.SetDrawAngle(AValue: Extended);
begin
  if FDrawAngle=AValue then exit;
  while AValue > 360 do AValue := AValue - 360;
  while AValue < 0 do AValue := AValue + 360;
  FDrawAngle:=AValue;
end;

procedure TPlotAxis.SetDrawLength(const AValue: Extended);
begin
  //AutoLength := FALSE;
  FDrawLength := AValue;
end;

function TPlotAxis.GetDrawLength: Extended;
var
  vSideLength: Integer;
  vX, vY, l1, l2: Integer;
  vAngleRad: Extended;
  vWidth, vHeight: Integer; // 21.05.13 replace OwnerPlotRect by ClientRect ? try it
begin
  // 09.08.14
  // DrawLength calculated from ClientRect !
  IF (NOT AutoLength) THEN BEGIN
    Result := FDrawLength;
    exit;
    END;

  IF TPlotRect(OwnerPlotRect).AxisAutoPlaceFill THEN begin
    //writeln('use autoplacelength = ', FloatToStrF(AutoPlacedLength, ffFixed, 4, 4));
    vSideLength := round(AutoPlacedLength  * sqrt( sqr(sin(DrawAngle /180 * pi)) + sqr(cos(DrawAngle /180 * pi)) ));
    Result := vSideLength;
  end else begin

    vWidth := min(OwnerPlotRect.ClientRect.Right - OwnerPlotRect.ClientRect.Left - DrawOriginRel.X, MaxInt);
    vHeight := min(OwnerPlotRect.ClientRect.Bottom - OwnerPlotRect.ClientRect.Top - DrawOriginRel.Y, maxint);

    case AutoMode of
    lmRelToSmallSide : begin
                             vSideLength := math.Min(vWidth,
                                          vHeight);
                           end;
    lmRelToLargeSide : begin
                             vSideLength := math.Max(vWidth,
                                          vHeight);
                           end;
    lmRelToHeight :        begin
                             vSideLength := vHeight;
                           end;
    lmRelToWidth :         begin
                             vSideLength := vWidth;
                           end;
    lmRelToBorder:         begin
                             vAngleRad := (DrawAngle /180 * pi);
                             IF ((round(DrawAngle) MOD 90) = 0) THEN BEGIN
                               IF ((round(DrawAngle) MOD 180) = 0) THEN
                                                 vSideLength := vWidth
                               ELSE vSideLength := vHeight;
                             END ELSE BEGIN
                               IF sin(vAngleRad) >= 0 THEN
                                 vY := vHeight- TPlotAxis(Self).DrawOriginRel.Y ELSE
                                 vY := TPlotAxis(Self).DrawOriginRel.Y;
                               IF cos(vAngleRad) < 0 THEN
                                 vX := TPlotAxis(Self).DrawOriginRel.X ELSE
                                 vX := vWidth - TPlotAxis(Self).DrawOriginRel.X;

                             l1 := round(vX / cos(vAngleRad));
                             l2 := round(vY / sin(vAngleRad));
                             vSideLength := (math.min(abs(l1), abs(l2)));
                             end;
                           end;
    end;
    Result := vSideLength * FDrawLength/100;
  end;
  //writeln('Axis returned DrawLength: ', FloatToStrF(Result, ffFixed, 4,4));
end;

function TPlotAxis.GetDrawAngle: Extended;
begin
  case uPlotClass.TPlotAxisBase(Self).Orientation of
    aoHorizontal : Result := 0;
    aoVertical   : Result := 90;
    aoVariable   : Result := FDrawAngle;
  end;
end;

function TPlotAxis.GetAxisMode: TAxisMode;
begin
  Result := FAxisMode;
end;

function TPlotAxis.GetAutoPlacedLength: Extended;
begin
  Result := FAutoPlacedLength;
end;

function TPlotAxis.GetAutoPlacedOriginRel: TPoint;
begin
  Result := FAutoPlacedOriginRel;
end;

{
function TPlotAxis.GetDrawEndPoint: TPoint;
begin
  ValueToScreen(Self, ViewRange.max,Result);
end;

function TPlotAxis.GetDrawEndPointRel: TPoint;
begin
  ValueToScreen(Self, ViewRange.max,Result);
  Result.X := Result.X - DrawOrigin.X;
  Result.Y := Result.Y - DrawOrigin.Y;
end;
}

function TPlotAxis.GetDrawOrigin: TPoint;
// Note: DrawOrigin is in math coordinates relative to PlotRect BottomLeft
begin
  Result.X := OwnerPlotRect.DataRect.Left + DrawOriginRel.X;
  Result.Y := OwnerPlotRect.DataRect.Bottom - DrawOriginRel.Y;
  //writeln('GetDrawOrigin: ', IntToStr(Result.X), ' / ', IntToStr(Result.Y));
end;

function TPlotAxis.GetDrawOriginRel: TPoint;
var
  vDrawOrigin: TPoint;
begin
  IF TPlotRect(OwnerPlotRect).AxisAutoPlaceFill THEN
    vDrawOrigin := AutoPlacedOriginRel
  ELSE vDrawOrigin := FDrawOrigin;

  case OriginMode of
    omAbs:        begin
                    Result.X := vDrawOrigin.X;
                    Result.Y := vDrawOrigin.Y;
                  end;
    omRel:        begin
                    Result.X := trunc((OwnerPlotRect.DataRect.Right - OwnerPlotRect.DataRect.Left) * (vDrawOrigin.X / 100) );
                    Result.Y := trunc((OwnerPlotRect.DataRect.Bottom - OwnerPlotRect.DataRect.Top) * (vDrawOrigin.Y / 100) ) ;
                  end;
  end;
  //writeln('GetDrawOriginRel: ', IntToStr(Result.X), ' / ', IntToStr(Result.Y));
end;

function TPlotAxis.GetSeriesUnits: TStrings;
var
  vSeriesLoop, vAxisLoop: integer;
  vAxisIndex : Integer;
  vUnitStr: String;
  vSeriesList: TFPList;
begin
  try
    vSeriesList := OwnerPlotRect.SeriesContainedIdx;
    try
    Result := TStringList.Create;
    // all series
      for vSeriesLoop := 0 to vSeriesList.Count-1 do begin
        // all axes
        with uPlotClass.TPlotAxisBase(Self).OwnerPlot.Series[PInteger(vSeriesList.Items[vSeriesLoop])^].AxesUsed do try
          for vAxisLoop:=0 to Count-1 do begin  // all axes used by these series
            vAxisIndex := Integer(Items[vAxisLoop]^);
            IF uPlotClass.TPlotAxisBase(Self).OwnerPlot.Axis[vAxisIndex] = Self THEN BEGIN
            // ***************************** get x, y, z units
              vUnitStr := uPlotClass.TPlotSeriesBase(uPlotClass.TBasePlotRect(OwnerPlotRect).OwnerPlot.Series[PInteger(vSeriesList.Items[vSeriesLoop])^]).UnitString[vAxisIndex];
            // add only if not already existing
            IF Result.IndexOf(vUnitStr) < 0 THEN
              Result.Add(vUnitStr);
            END;
          end;
        finally
            // free AxesUsed
            while Count > 0 do
            begin
             FreeMem(Items[0]);
             Delete(0);
            end;
             Free;
        end;
      end; // for all series
    except
      // free stringlist on exception
      Result.Free;
    end;
  finally
     if vSeriesList <> nil then begin
       while vSeriesList.Count > 0 do begin
         Dispose(Pinteger(vSeriesList.Items[0]));
         vSeriesList.Delete(0);
       end;
       vSeriesList.Free;
     end;
  end;
end;


function TPlotAxis.GetValueColor(AValue : Extended): TColor;
var
  vValue1 : Extended;
  vValue2 : Integer;
  vValueRange, vStretchFactor, vMin: Extended; // , vMax
  vHue, vSat, vLight : byte;
begin
  vValueRange := ReCalcValue(ViewRange.max) - ReCalcValue(ViewRange.min);
  vMin := ReCalcValue(ViewRange.min);

  if IsNan(AValue) or IsInfinite(AValue) then begin
    Result := clNone;
    exit;
  end;

  vValue1 := ReCalcValue(AValue);
  begin  // color calculations
    if (vValueRange > 0) then vStretchFactor := 255 / vValueRange
    else vStretchFactor := c_GLOBALMAX;
    vValue2 := round( (vValue1-vMin) * vStretchFactor) ;
      //IF (not FlipWFY) THEN vValue2:=255-vValue2;
      vValue2:=255-vValue2;
      IF vvalue2 > 255 THEN vvalue2 := 255;
      IF vvalue2 < 0 THEN vvalue2 := 0;
      vHue := byte((vValue2 * 240) DIV 360);      // blue (240deg) to red (0deg)
      vsat := 255;
      // vlight :=  127;
      vlight := (127 - (vvalue2 DIV 2)) MOD 255;  // reduce light for low (more blue) values
      Result:=HLSToColor(vhue,vlight,vsat);
   end;
//////////////////
end;

function TPlotAxis.GetValueFPColor(AValue : Extended): TFPColor;
begin
  Result := TColorToFPColor(GetValueColor(AValue));
end;

function TPlotAxis.GetValuePerPixel: Extended;
begin
  IF AxisMode = amValuePerPixel THEN BEGIN
    Result := FValuePerPixel;
    exit
  END ELSE BEGIN
    IF PixelsPerValue <> 0 THEN Result := 1 / PixelsPerValue
    ELSE Result := 1; //c_GLOBALMAX;
  END;
end;

function TPlotAxis.GetVisualParams: TAxisVisualParams;
var
  vError: Integer;
begin
  Result.ptA := DrawOrigin;

  vError := ValueToScreen(Self, ViewRange.max, Result.ptB);  // this works somehow ?
  //// replace to be compatible with autoscaling in amValuePerPixel also:
  //if AxisMode = amValuePerPixel then vViewRange.max := uPlotClass.TPlotAxisBase(Self).ViewRange.max
  //else vViewRange.max := math.max(DrawLength * ValuePerPixel, c_GLOBALMIN);

  // why does this not work ???
  //vResult := uPlotUtils.ValueToMathCoord(Self, ViewRange.max, vPtBrel);
  //vResult := uPlotUtils.ValueToMathCoord(Self, ViewRange.max-ViewRange.min, vPtBrel);
  //Result.ptB.X := Result.ptA.X + round(vPtBrel.X);
  //Result.ptB.Y := Result.ptA.Y - round(vPtBrel.Y);

  IF vError < 0 THEN Result.ptB := Result.ptA;
  Result.PixelsPerValue := Self.PixelsPerValue;
  Result.DrawAngle:=DrawAngle;
  Result.DrawLength:=DrawLength;
  Result.DrawCanvas := PlotImage.Canvas;
  Result.TickLength := Self.TickLength;
  Result.SubTickLength := Self.SubTickLength;
  Result.Style := TAxisStyle(Self.Style);
  Result.TickStyle := Self.TickStyle;
  Result.SubTickStyle := Self.SubTickStyle;
  Result.TickAngle := Self.TickAngle;
  Result.Ticks := Self.Ticks;
  Result.SubTicks := Self.SubTicks;
  Result.Marks := Self.Marks;
  Result.SubMarks:= Self.SubMarks;
  Result.Visible:=Visible;
  Result.TickInfo := FTickInfo;

  {$IFDEF DEBUG_SCALE_WRITE}
  writeln('viewrange max: ', FloatToStrF(ViewRange.max, ffExponent, 5, 5) );
  writeln('ptA: ', IntToStr(Result.ptA.X), '/', IntToStr(Result.ptA.Y) );
  writeln('ptB: ', IntToStr(Result.ptB.Y), '/', IntToStr(Result.ptB.Y) );
  writeln('PpVal ', FloatToStrF(Result.PixelsPerValue, ffExponent, 5, 5) );
  {$ENDIF}
end;

function TPlotAxis.GetPixelsPerValue: Extended;
var vRange : Extended;
begin
  IF AxisMode = amPixelPerValue THEN BEGIN

  IF (not LogScale) THEN vRange := ReCalcValue(ViewRange.max) - ReCalcValue(ViewRange.min)
      ELSE vRange := logn(LogBase,ViewRange.max) - logn(LogBase,ViewRange.min);
    // check how this could be done better, NO IDEA
    IF vRange = 0 THEN vRange := c_GLOBALMIN;
    Result := DrawLength / vRange;
  END
  ELSE BEGIN
    IF FValuePerPixel <> 0 THEN Result := 1 / FValuePerPixel
    ELSE Result := c_GLOBALMAX;
  END;
end;

procedure TPlotAxis.SetAutoLength(const AValue: Boolean);
begin
  if FAutoLength=AValue then exit;
  FAutoLength:=AValue;
end;

procedure TPlotAxis.SetAutoLengthMode(const AValue: TAutoLengthMode);
begin
  if FAutoLengthMode=AValue then exit;
  FAutoLengthMode:=AValue;
end;

procedure TPlotAxis.SetAutoPlacedLength(AValue: Extended);
begin
  IF FAutoPlacedLength = AValue THEN exit;
  FAutoPlacedLength := AValue;
end;

procedure TPlotAxis.SetAutoPlacedOriginRel(AValue: TPoint);
begin
  FAutoPlacedOriginRel := AValue;
end;

procedure TPlotAxis.SetAxisLabel(const AValue: String);
begin
  if FAxisLabel=AValue then exit;
  FAxisLabel:=AValue;
end;

procedure TPlotAxis.SetAxisMode(AValue: TAxisMode);
begin
  IF FAxisMode = AValue THEN exit;
  FAxisMode := AValue;
end;


function TPlotAxis.ReDraw(ADrawVisible:Boolean): TRect;
var
  vSpacing: Integer;
  vLoop: Integer;
  vUsedRect, vLabelUsedRect, vUnitsUsedRect, vCloneUsedRect: TRect;
  vNormalAngle: Extended;
  vPtA2, vPtB2: TPoint;
  vUnitsCenterPt, vLabelCenterPt: TPoint;
begin
  vUsedRect := OwnerPlotRect.ClientRect;

  begin
    VisualParams.DrawCanvas.Clipping:=false;
    VisualParams.DrawCanvas.ClipRect := OwnerPlotRect.ClientRect;
    // check main axis #############################################
    vSpacing := DrawIndicatorAxis(VisualParams, ADrawVisible);

    // label draw after 15.10.14
    case  VisualParams.TickAngle of
    taPositive :  begin
                    vNormalAngle := (DrawAngle + 90);
                  end;
    taNegative : begin
                    vNormalAngle := (DrawAngle -90);
                  end;
    end;
    vPtA2 := LineEndPoint(VisualParams.ptA, vNormalAngle*16, vSpacing );
    vPtB2 := LineEndPoint(VisualParams.ptB, vNormalAngle*16, vSpacing );

    vLabelCenterPt.X := vPtA2.X + ( (vPtB2.X - vPtA2.X) DIV 2 );
    vLabelCenterPt.Y := vPtA2.Y + ( (vPtB2.Y - vPtA2.Y) DIV 2 );

    vLabelUsedRect := DrawLabel(vLabelCenterPt, VisualParams, ADrawVisible and VisualParams.Visible);

    // new code for unit draw after 15.10.14
    vUnitsCenterPt := vPtB2;
    vUnitsUsedRect := DrawUnits(vUnitsCenterPt, VisualParams, ADrawVisible and VisualParams.Visible);

    // axis line net size...
    vUsedRect.Left := math.min(VisualParams.ptA.X, VisualParams.ptB.X);
    vUsedRect.Right := math.max(VisualParams.ptA.X, VisualParams.ptB.X);
    vUsedRect.Top := math.min(VisualParams.ptA.Y, VisualParams.ptB.Y);
    vUsedRect.Bottom := math.max(VisualParams.ptA.Y, VisualParams.ptB.Y);

    // save new estimate for Data area
    FNetAxisRect := vUsedRect;

    // vSpacing includes axis ticks and tick-text
    // see other routines --> modularize !
    case  VisualParams.TickAngle of
    taPositive :  begin
                    vNormalAngle := (DrawAngle + 90);
                  end;
    taNegative : begin
                    vNormalAngle := (DrawAngle -90);
                  end;
    end;
    vPtA2 := LineEndPoint(VisualParams.ptA, vNormalAngle*16, vSpacing );
    vPtB2 := LineEndPoint(VisualParams.ptB, vNormalAngle*16, vSpacing );

    vUsedRect.Left :=  math.MinIntValue([vUsedRect.Left, vPtA2.X, vPtB2.X]);
    vUsedRect.Right :=  math.MaxIntValue([vUsedRect.Right, vPtA2.X, vPtB2.X]);
    vUsedRect.Top :=  math.MinIntValue([vUsedRect.Top, vPtA2.Y, vPtB2.Y]);
    vUsedRect.Bottom :=  math.MaxIntValue([vUsedRect.Bottom, vPtA2.Y, vPtB2.Y]);
    // above yields total rect used by axis including ticks and tickmarks
    // ########### axis end #############################################

    // add all together
    vUsedRect.Left :=  math.MinIntValue([vUsedRect.Left, vLabelUsedRect.Left, vUnitsUsedRect.Left]);
    vUsedRect.Right :=  math.MaxIntValue([vUsedRect.Right, vLabelUsedRect.Right, vUnitsUsedRect.Right]);
    vUsedRect.Top :=  math.MinIntValue([vUsedRect.Top, vLabelUsedRect.Top,vUnitsUsedRect.Top]);
    vUsedRect.Bottom :=  math.MaxIntValue([vUsedRect.Bottom, vLabelUsedRect.Bottom, vUnitsUsedRect.Bottom]);

  //
  end;


  //  check clone axis  #############################################
  FOR vLoop := 0 to FCloneAxes.Count-1 DO BEGIN
    if not TCloneAxisParams(FCloneAxes[vLoop]^).VisualParams.Visible then continue;
    vSpacing := DrawCloneAxis(TCloneAxisParams(FCloneAxes[vLoop]^), ADrawVisible, vCloneUsedRect);

  // ########### clone axis end #############################################

  // add clone axis together
    vUsedRect.Left :=  math.MinIntValue([vUsedRect.Left, vCloneUsedRect.Left]);
    vUsedRect.Right :=  math.MaxIntValue([vUsedRect.Right, vCloneUsedRect.Right]);
    vUsedRect.Top :=  math.MinIntValue([vUsedRect.Top, vCloneUsedRect.Top]);
    vUsedRect.Bottom :=  math.MaxIntValue([vUsedRect.Bottom, vCloneUsedRect.Bottom]);
  END;
    //writeln('axis redraw total usedrect:');
  //with vUsedRect do begin
  //  write('T: ', IntToStr(Top), '  ');
  //  write('B: ', IntToStr(Bottom), '  ');
  //  write('L: ', IntToStr(Left), '  ');
  //  writeln('R: ', IntToStr(Right), '  ');
  //end;

  IF ADrawVisible THEN DrawInnerTicks;      // we draw inner ticks even if axis is invisible ??
  Result := vUsedRect;
 end;


procedure TPlotAxis.DrawInnerTicks;
// TODO: modularize code together with DrawIndicatorAxis (esp. math routines !)
// 02.08.14
// first draw subticks, not to overwrite mainticks in 3D mode
// inner ticks only drawn when number is in subnumbers
var
  vMinMax : TValueRange;
  vMainInterval : Extended; // vValueRange, vSubInterval
  vMainCount, vLoop, vAxisLoop, vLoopGrain : Integer;
  vMainLow, vMainHigh, vMain, vSub : Extended;
  vResult : Integer;
  vPtA, vPtB : TPoint;
  vPtBrel: TFloatPoint;
  vDrawCanvas : TCanvas;
  Index : Integer;
  vR: Extended;
  vParams, v3DParams: TAxisVisualParams;
  vLoop3Dmain, vLoop3Dgrain: Integer;
  vPt3DabsA, vPt3DabsB: TPoint;
  vPt3Drel: TFloatPoint;
  vMain3D, vSub3D: Extended;
begin
  vParams := VisualParams;
  if (F3DGridAxis > -1) then v3DParams := TPlotAxis(OwnerPlot.Axis[F3DGridAxis]).VisualParams;
  // var inits
  vMainLow := 0; vMainHigh := 0; vMain := 0; vSub := 0;
  vMainInterval := 0; //vValueRange := 0;
  vPtA.X := 0; vPtA.Y := 0;
  vPtB.X := 0; vPtB.Y := 0;

  IF not (InnerTicks OR InnerSubTicks) THEN exit;
  vDrawCanvas := PlotImage.Canvas;

  vMinMax := ViewRange;

  vMainHigh := FTickInfo.mainhigh; // MainTickRange(ViewRange, LogBase, LogScale).max;
  vMainLow := FTickInfo.mainlow; // MainTickRange(ViewRange, LogBase, LogScale).min;
  vMainInterval := FTickInfo.maininterval; //  MainTickInterval(ViewRange, LogBase, LogScale);
  if vMainInterval = 0 then vMainInterval:=c_GLOBALMIN;             // needed ?


  IF LogScale THEN vMainCount := round(logn(LogBase, vMainHigh) - logn(LogBase,vMainLow)) + 1
  ELSE vMainCount := round((vMainHigh - vMainlow)/vMainInterval); // +1; ?

  // draw subticks inner  -----------------------------------------------------
  IF InnerSubTicks THEN BEGIN
    for vLoop := -1 to vMainCount do begin
      IF LogScale THEN vMain:= vMainlow * power(LogBase, (vLoop*vMainInterval))
      ELSE vMain:= vMainlow + (vLoop*vMainInterval);
      for vLoopGrain:= 1 to SubTickGrain-1 do begin              // -1 avoids drawing on main tick
        IF not ((vLoopGrain) in vParams.TickInfo.SubNumbers) then continue;
        IF (LogScale AND (vLoopGrain = 1)) THEN continue;
        IF LogScale THEN vSub := vMain  * (0 + (LogBase / SubTickGrain * vLoopGrain))
          ELSE vSub := vMain  + (vMainInterval / SubTickGrain)*vLoopGrain;
        IF (vSub < vMinMax.min) OR (vSub > vMinMax.max) THEN continue;

        uPlotUtils.ValueToScreen(Self, (vSub), vPtA);

        for vAxisLoop := 0 to FInnerGridAxes.Count-1 do begin
          Index := longint(FInnerGridAxes.Items[vAxisLoop]^);
          vResult := uPlotUtils.ValueToMathCoord(OwnerPlot.Axis[Index], OwnerPlot.Axis[Index].ViewRange.max, vPtBrel);
          IF vResult < 0 then continue;
          vPtB.X := vPtA.X + round(vPtBrel.X);
          vPtB.Y := vPtA.Y - round(vPtBrel.Y);
          uPlotStyles.TAxisStyle(SubTickStyle).DrawInnerGridLine(vPtA, vPtB, vDrawCanvas);

          // new code for 3D subticks:
          if (F3DGridAxis > -1) and (F3DGridAxis <> vAxisLoop) and TPlotAxis(OwnerPlot.Axis[F3DGridAxis]).InnerSubTicks and Inner3DTicks then begin
            for vLoop3Dmain := 0 to v3DParams.TickInfo.maincount do begin
              // calculate shift for ptsA and B, omit double drawing on 0/0 !
              IF TPlotAxis(OwnerPlot.Axis[F3DGridAxis]).LogScale THEN vMain3D:= v3DParams.TickInfo.mainlow * power(TPlotAxis(OwnerPlot.Axis[F3DGridAxis]).LogBase, (vLoop3Dmain*v3DParams.TickInfo.maininterval))
              ELSE vMain3D:= v3DParams.TickInfo.mainlow + (vLoop3Dmain*v3DParams.TickInfo.maininterval);

              for vLoop3Dgrain:= 0 to TPlotAxis(OwnerPlot.Axis[F3DGridAxis]).SubTickGrain do begin              // -1 avoids drawing on main tick, first 0 drwas pseudo main
                //IF not ((vLoop3Dgrain) in v3DParams.TickInfo.SubNumbers) then continue;
                IF (TPlotAxis(OwnerPlot.Axis[F3DGridAxis]).LogScale AND (vLoop3Dgrain = 1)) THEN continue;
                IF TPlotAxis(OwnerPlot.Axis[F3DGridAxis]).LogScale THEN vSub3D := vMain3D  * (0 + (TPlotAxis(OwnerPlot.Axis[F3DGridAxis]).LogBase / TPlotAxis(OwnerPlot.Axis[F3DGridAxis]).SubTickGrain * vLoop3Dgrain))
                  ELSE vSub3D := vMain3D  + (v3DParams.TickInfo.maininterval / TPlotAxis(OwnerPlot.Axis[F3DGridAxis]).SubTickGrain)*vLoop3Dgrain;
                // we need to draw a subtick where the main coordinate is as this is the subsubtick
                IF (not ((vLoop3Dgrain) in v3DParams.TickInfo.SubNumbers + [0] )) then continue;
                // IF (vSub > ReCalcValue(vMinMax.max)) OR (vSub < ReCalcValue(vMinMax.min)) then continue;
                IF (vSub3D < vMinMax.min) OR (vSub3D > vMinMax.max) THEN continue;

                vResult := uPlotUtils.ValueToMathCoord(OwnerPlot.Axis[F3DGridAxis], vSub3D, vPt3Drel);
                if vResult < 0 then continue;
                IF (round(vPt3Drel.X) = 0) AND (round(vPt3Drel.Y)=0) THEN continue;
                vPt3DabsA.X := vPtA.X + round(vPt3Drel.X);                                                             // overloads for point add operator ??
                vPt3DabsB.X := vPtB.X + round(vPt3Drel.X);
                vPt3DabsA.Y := vPtA.Y - round(vPt3Drel.Y);                                                             // overloads for point add operator ??
                vPt3DabsB.Y := vPtB.Y - round(vPt3Drel.Y);
                uPlotStyles.TAxisStyle(SubTickStyle).DrawInnerGridLine(vPt3DabsA, vPt3DabsB, vDrawCanvas);
              end;
            end;
          end; // ### code for 3D
        end;

      end;
    end;
  END; // end subticks inner ---------------------------------------------------

  // code for 3D grids
  IF InnerTicks THEN BEGIN  // ----------------------------------------------
    for vLoop := 0 to vMainCount-1 do begin
      IF LogScale THEN vMain:= vMainlow * power(LogBase, (vLoop*vMainInterval))
      ELSE vMain:= vMainlow + (vLoop*vMainInterval);
      IF (vMain < vMinMax.min) OR (vMain > vMinMax.max) THEN continue;
      // TODO: check this functions in utils - what is still needed, what can be unified
      // problem is change from global image to drawimage... ValueToMathCoord thinks diffrent than ValueToScreen

      vR:= round((ReCalcValue(vMain) - ReCalcValue(vMinMax.Min)) * vParams.PixelsPerValue);
      vPtA := LineEndPoint(vParams.ptA, DrawAngle*16, vR);

        for vAxisLoop := 0 to FInnerGridAxes.Count-1 do begin
          Index := longint(FInnerGridAxes.Items[vAxisLoop]^);
          vResult := uPlotUtils.ValueToMathCoord(OwnerPlot.Axis[Index], OwnerPlot.Axis[Index].ViewRange.max, vPtBrel);
          vPtB.X := vPtA.X + round(vPtBrel.X);
          vPtB.Y := vPtA.Y - round(vPtBrel.Y);

          uPlotStyles.TAxisStyle(TickStyle).DrawInnerGridLine(vPtA, vPtB, vDrawCanvas);

          // new code for 3D mainticks:
          if (F3DGridAxis > -1) and (F3DGridAxis <> vAxisLoop) and TPlotAxis(OwnerPlot.Axis[F3DGridAxis]).InnerTicks and Inner3DTicks then begin
            for vLoop3Dmain := -1 to v3DParams.TickInfo.maincount do begin
              // calculate shift for ptsA and B, omit double drawing on 0/0 !
              IF TPlotAxis(OwnerPlot.Axis[F3DGridAxis]).LogScale THEN vMain3D:= v3DParams.TickInfo.mainlow * power(TPlotAxis(OwnerPlot.Axis[F3DGridAxis]).LogBase, (vLoop3Dmain*v3DParams.TickInfo.maininterval))
              ELSE vMain3D:= v3DParams.TickInfo.mainlow + (vLoop3Dmain*v3DParams.TickInfo.maininterval);
              vResult := uPlotUtils.ValueToMathCoord(OwnerPlot.Axis[F3DGridAxis], vMain3D, vPt3Drel);
              IF (vMain3D < vMinMax.min) OR (vMain3D > vMinMax.max) THEN continue;
              //IF (round(vPt3Drel.X) = 0) AND (round(vPt3Drel.Y)=0) THEN continue;
              if (round(vPt3Drel.X) <> 0) OR (round(vPt3Drel.Y)<>0) THEN begin
              vPt3DabsA.X := vPtA.X + round(vPt3Drel.X);                                                             // overloads for point add operator ??
              vPt3DabsB.X := vPtB.X + round(vPt3Drel.X);
              vPt3DabsA.Y := vPtA.Y - round(vPt3Drel.Y);                                                             // overloads for point add operator ??
              vPt3DabsB.Y := vPtB.Y - round(vPt3Drel.Y);
              uPlotStyles.TAxisStyle(TickStyle).DrawInnerGridLine(vPt3DabsA, vPt3DabsB, vDrawCanvas);
              end;

                if TPlotAxis(OwnerPlot.Axis[F3DGridAxis]).InnerSubTicks then
                // new code for 3D subticks between 3D mainticks
                for vLoop3Dgrain:= 1 to TPlotAxis(OwnerPlot.Axis[F3DGridAxis]).SubTickGrain-1 do begin              // -1 avoids drawing on main tick
                  IF not ((vLoop3Dgrain) in v3DParams.TickInfo.SubNumbers) then continue;
                  IF (TPlotAxis(OwnerPlot.Axis[F3DGridAxis]).LogScale AND (vLoop3Dgrain = 1)) THEN continue;
                  IF TPlotAxis(OwnerPlot.Axis[F3DGridAxis]).LogScale THEN vSub3D := vMain3D  * (0 + (TPlotAxis(OwnerPlot.Axis[F3DGridAxis]).LogBase / TPlotAxis(OwnerPlot.Axis[F3DGridAxis]).SubTickGrain * vLoop3Dgrain))
                    ELSE vSub3D := vMain3D  + (v3DParams.TickInfo.maininterval / TPlotAxis(OwnerPlot.Axis[F3DGridAxis]).SubTickGrain)*vLoop3Dgrain;
                  IF (vSub3D < vMinMax.min) OR (vSub3D > vMinMax.max) THEN continue;
                 // IF (vSub > ReCalcValue(vMinMax.max)) OR (vSub < ReCalcValue(vMinMax.min)) then continue;
                 // TODO: // IF (vSub3D < vMinMax.min) OR (vSub3D > vMinMax.max) THEN continue;
                  vResult := uPlotUtils.ValueToMathCoord(OwnerPlot.Axis[F3DGridAxis], vSub3D, vPt3Drel);
                  IF (round(vPt3Drel.X) = 0) AND (round(vPt3Drel.Y)=0) THEN continue;
                  vPt3DabsA.X := vPtA.X + round(vPt3Drel.X);                                                             // overloads for point add operator ??
                  vPt3DabsB.X := vPtB.X + round(vPt3Drel.X);
                  vPt3DabsA.Y := vPtA.Y - round(vPt3Drel.Y);                                                             // overloads for point add operator ??
                  vPt3DabsB.Y := vPtB.Y - round(vPt3Drel.Y);
                  uPlotStyles.TAxisStyle(SubTickStyle).DrawInnerGridLine(vPt3DabsA, vPt3DabsB, vDrawCanvas);
                end;

            end;
          end; // ### code for 3D
        end;
    end;
  END; // end mainticks inner
end;


function TPlotAxis.DrawLabel(ACenterPt: TPoint;
  AVisualParams: TAxisVisualParams; ADrawvisible: Boolean): TRect;
var
  vPtTextOut: TPoint;
  vNormalAngle, vTextAngle: Extended;
  vDrawCanvas: TCanvas;
  vRotate: Boolean;
  vxshift, vyshift: Integer;
  vTextwidth, vTextHeight: integer;
  vUsedRect: TRect;
  vPtB1, vPtB2, vPtB3, vPtB4: TPoint; // used to calculate used rect
const
  cEpsilon = 1e-2;
begin
  vUsedRect.Top:=0; vUsedRect.Bottom:=0;
  vUsedRect.Left:=0; vUsedRect.Right:=0;
  vRotate := RotateLabelText; // FALSE ?
  Result := vUsedRect;
  IF AxisLabel = '' THEN exit;
  IF not AVisualParams.Visible then begin
    // need to return the net axis usedrect
    Result := Rect(min(AVisualParams.ptA.X, AVisualParams.ptB.X),
                   min(AVisualParams.ptA.Y, AVisualParams.ptB.Y),
                   max(AVisualParams.ptA.X, AVisualParams.ptB.X),
                   max(AVisualParams.ptA.Y, AVisualParams.ptB.Y));
    exit;
  end;

  vDrawCanvas := PlotImage.Canvas;
  vxshift:=0; vyshift:=0;
  case AVisualParams.TickAngle of
  taPositive :  begin
                  vNormalAngle := (DrawAngle + 90);
                end;
  taNegative : begin
                  vNormalAngle := (DrawAngle - 90);
                end;
  end;
  // TODO: bug in fpc ?  LineEndPoint accepts only 0..360 degrees (*16)
  while vNormalAngle > 360 do vNormalAngle := vNormalAngle - 360;
  while vNormalAngle < 0 do vNormalAngle := vNormalAngle + 360;
  //
    vtextwidth := uPlotStyles.TPlotStyle(Style).TextWidth[AxisLabel, vDrawCanvas];
    vtextheight := uPlotStyles.TPlotStyle(Style).TextHeigth[AxisLabel, vDrawCanvas];
    vPtTextOut := ACenterPt;

  //// shift width
  IF vRotate THEN begin
    if cos(DrawAngle*Pi()/180) >= -cEpsilon then begin
        if DrawAngle > 180 then vPtTextOut := LineEndPoint(vPtTextOut, (DrawAngle - 180) * 16, vTextwidth / 2)
        else vPtTextOut := LineEndPoint(vPtTextOut, (DrawAngle + 180) * 16, vTextwidth / 2)
      end
      else vPtTextOut := LineEndPoint(vPtTextOut, (DrawAngle) * 16, vTextwidth / 2);
  end;

  // turned in direction of axis
  IF vRotate THEN BEGIN
    vxshift := 0;//shifting is done by lazarus
    vyshift := 0;
    // 15.10.14
    if cos(DrawAngle*Pi()/180) < -cEpsilon then begin
      if (DrawAngle > 180) then vTextAngle := DrawAngle - 180 else vTextAngle := DrawAngle + 180;
    end else vTextAngle := DrawAngle;
  END ELSE BEGIN
  // always horizontal
            IF cos(vNormalAngle*Pi/180) < 0 THEN vxshift := - vtextwidth;
            IF sin(vNormalAngle*Pi/180) > 0 THEN vyshift := - vtextheight;
            vTextAngle := 0;
  END;

  IF ADrawVisible THEN uPlotStyles.TPlotStyle(Style).DrawTextEx(vPtTextOut.X + vxshift, vPtTextOut.Y + vyshift,
  vTextAngle, AxisLabel, vDrawCanvas);

  vPtB1.X := vPtTextOut.X + vxshift;
  vPtB1.Y := vPtTextOut.Y + vyshift;
  vPtB2 := LineEndPoint(vPtB1, vTextAngle*16, vTextwidth);
  vPtB3 := LineEndPoint(vPtB1, (vTextAngle-90)*16, vTextHeight);
  vPtB4 := LineEndPoint(vPtB3, vTextAngle*16, vTextwidth);

  vUsedRect.Left :=  math.MinIntValue([vPtB1.X, vPtB2.X, vPtB3.X, vPtB4.X]);
  vUsedRect.Right :=  math.MaxIntValue([vPtB1.X, vPtB2.X, vPtB3.X, vPtB4.X]);
  vUsedRect.Top :=  math.MinIntValue([vPtB1.Y, vPtB2.Y, vPtB3.Y, vPtB4.Y]);
  vUsedRect.Bottom :=  math.MaxIntValue([vPtB1.Y, vPtB2.Y, vPtB3.Y, vPtB4.Y]);


  Result := vUsedRect;
end;


function TPlotAxis.DrawUnits(ACenterPt: TPoint;
  AVisualParams: TAxisVisualParams; ADrawvisible: Boolean): TRect;
// This (new) code always inlines units and never rotates
var
  vPtTextOut: TPoint;
  vNormalAngle, vTextAngle: Extended;        // draw see also drawlabel, same code...
  vDrawCanvas: TCanvas;
  vxshift, vyshift, vtextwidth, vtextheight: Integer;
  vRotate: Boolean; // vInLine
  vLoop: Integer;
  vUnitStr: String;
  vUsedRect: TRect;
  vPtB1, vPtB2, vPtB3, vPtB4: TPoint; // used to calculate used rect
const
  cEpsilon = 1e-2;
begin
  //IF not Visible then exit;
  //IF (not Visible) then ADrawvisible:=false;
  //vInLine:=TRUE;
  vRotate := RotateUnitText; // FALSE ?

  vUsedRect.Top:=0; vUsedRect.Bottom:=0;
  vUsedRect.Left:=0; vUsedRect.Right:=0;
  IF not AVisualParams.Visible then begin
    // need to return the net axis usedrect
    Result := Rect(min(AVisualParams.ptA.X, AVisualParams.ptB.X),
                   min(AVisualParams.ptA.Y, AVisualParams.ptB.Y),
                   max(AVisualParams.ptA.X, AVisualParams.ptB.X),
                   max(AVisualParams.ptA.Y, AVisualParams.ptB.Y));
    exit;
  end;
  //IF not Visible THEN exit;
  vUnitStr:='';
  vDrawCanvas := PlotImage.Canvas;
  //vDrawCanvas.Pixels[ACenterPt.X, ACenterPt.Y] := clRed; // debug only
  vxshift:=0; vyshift:=0;
  case TickAngle of
  taPositive :  begin
                  vNormalAngle := (DrawAngle + 90);
                end;
  taNegative : begin
                  vNormalAngle := (DrawAngle -90);
                end;
  end;
  // TODO: bug in fpc ?  LineEndPoint accepts only 0..360 degrees (*16)
  while vNormalAngle > 360 do vNormalAngle := vNormalAngle - 360;
  while vNormalAngle < 0 do vNormalAngle := vNormalAngle + 360;

  with SeriesUnits do try
    for vLoop:=0 to Count-1 do begin
        vUnitStr:=vUnitStr + Strings[vLoop];
        IF vLoop < (Count-1) THEN vUnitStr := vUnitStr + ', ';
      end;
    BEGIN
      vtextwidth := uPlotStyles.TPlotStyle(Style).TextWidth[vUnitStr, vDrawCanvas];
      vtextheight := uPlotStyles.TPlotStyle(Style).TextHeigth[vUnitStr, vDrawCanvas];

      vPtTextOut := ACenterPt;
      //// shift width
      IF vRotate THEN begin
        if cos(DrawAngle*Pi()/180) >= -cEpsilon then begin
            if DrawAngle > 180 then vPtTextOut := LineEndPoint(vPtTextOut, (DrawAngle - 180) * 16, vTextwidth / 2)
            else vPtTextOut := LineEndPoint(vPtTextOut, (DrawAngle + 180) * 16, vTextwidth / 2)
          end
          else vPtTextOut := LineEndPoint(vPtTextOut, (DrawAngle) * 16, vTextwidth / 2);
      end;

      // turned in direction of axis
      IF vRotate THEN BEGIN
        vxshift := 0;//shifting is done by lazarus
        vyshift := 0;
        // 15.10.14
        if cos(DrawAngle*Pi()/180) < -cEpsilon then begin
          if (DrawAngle > 180) then vTextAngle := DrawAngle - 180 else vTextAngle := DrawAngle + 180;
        end else vTextAngle := DrawAngle;
      END ELSE BEGIN
      // always horizontal
                IF cos(vNormalAngle*Pi/180) < 0 THEN vxshift := - vtextwidth;
                IF sin(vNormalAngle*Pi/180) > 0 THEN vyshift := - vtextheight;
                vTextAngle := 0;
      END;

      IF ADrawVisible THEN uPlotStyles.TPlotStyle(Style).DrawTextEx(vPtTextOut.X + vxshift, vPtTextOut.Y + vyshift,
        vTextAngle, vUnitStr, vDrawCanvas);
      END;

  finally
     Free;
  end;

  // calculate used rectangle  vUsedRect.Left :=  math.MinIntValue([vPtB.X, vPtB2.X, vPtB3.X, vPtB4.X]);
  vPtB1.X := vPtTextOut.X + vxshift;
  vPtB1.Y := vPtTextOut.Y + vyshift;
  vPtB2 := LineEndPoint(vPtB1, vTextAngle*16, vTextwidth);
  vPtB3 := LineEndPoint(vPtB1, (vTextAngle-90)*16, vTextHeight);
  vPtB4 := LineEndPoint(vPtB3, vTextAngle*16, vTextwidth);

  vUsedRect.Left :=  math.MinIntValue([vPtB1.X, vPtB2.X, vPtB3.X, vPtB4.X]);
  vUsedRect.Right :=  math.MaxIntValue([vPtB1.X, vPtB2.X, vPtB3.X, vPtB4.X]);
  vUsedRect.Top :=  math.MinIntValue([vPtB1.Y, vPtB2.Y, vPtB3.Y, vPtB4.Y]);
  vUsedRect.Bottom :=  math.MaxIntValue([vPtB1.Y, vPtB2.Y, vPtB3.Y, vPtB4.Y]);

  Result := vUsedRect;
end;


end.

