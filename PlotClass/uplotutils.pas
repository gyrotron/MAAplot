unit uPlotUtils;
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
//{$mode delphi}{$H+}

interface

uses
  Classes, Types, sysutils, math, GraphMath, Dialogs, uPlotClass, uPlotAxis, FPimage, Graphics,
  uPlotDataTypes, LazUTF8; //GraphUtil

type
  TFPColorRange = record
    min: TFPColor;
    max: TFPColor;
  end;

  TColorRange = record
    min: TColor;
    max: TColor;
  end;



// TODO: implement better return codes
function ValueToMathCoord(AAxis: uPlotClass.TPlotAxisBase; AValue: Extended; out MathPt: TFloatPoint): integer;
                                 // pixel coordinates in math axis orientation relative to DrawOrigin 0/0
function MathCoordToValue(AAxis: uPlotClass.TPlotAxisBase; MathPt: TFloatPoint; out AValue: Extended): integer;  // used for PixelZoom in axis
                                 // pixel coordinates in math axis orientation relative to DrawOrigin 0/0
//function ValueToDataImage(AAxis: TPlotAxis; AValue: Extended; out ScrPt: TPoint): Integer;               // will not work, as we need to respect all axes !
                                 // pixel coordinates directly usable in DataImage
function XYZToDataImage(XAxis, YAxis, ZAxis: TPlotAxisBase; X, Y, Z: Extended; out ScrPt: TPoint): Integer;
                                 // pixel coordinates directly usable in DataImage
function ValueToScreen(AAxis: uPlotClass.TPlotAxisBase; AValue: Extended; out ScrPt: TPoint): integer;
                                 // pixel coordinates directly usable in Plot.PlotImage
function XYToScreen(XAxis, YAxis: uPlotClass.TPlotAxisBase; X, Y: Extended; out ScrPt: TPoint): integer;
function ScreenToXY(XAxis, YAxis: uPlotClass.TPlotAxisBase; out X, Y: Extended; ScrPt: TPoint): integer;
                                 // pixel coordinates directly usable in Plot.PlotImage
function XYZToScreen(XAxis, YAxis, ZAxis: uPlotClass.TPlotAxisBase; X, Y, Z: Extended; out ScrPt: TPoint): integer;
                                 // pixel coordinates directly usable in Plot.PlotImage
function ShiftPoint(AShiftSize: TSize ; var ScrPt: TPoint): integer;
function PlotImageCoordsToFrameRectCoords(APlotRect: TBasePlotRect; var ScrPt: TPoint): Integer; // used when calculated relative to PlotIMage (i.e. XYToScreen) but plotted into FrameRect (i.e. FBackBMP)
function PlotImageCoordsToDataRectCoords(APlotRect: TBasePlotRect; var ScrPt: TPoint): Integer; // used when calculated relative to PlotIMage (i.e. XYToScreen) but plotted into FrameRect (i.e. FBackBMP)
// for autoscaling
function ValueTo125(AValue: Extended; AUpwards: Boolean; APower: Integer): extended; // TODO: rework
function ValueToNext(AValue: Extended; AUpwards: Boolean; APower: Integer): extended; // TODO: rework
// tests 10.09.10
//function ScrCoordToPlotRect(X, Y: Integer; var PlotRectIndex: Integer): Integer;
function IsInsideRect(X, Y: Integer; ARect: TRect): Boolean; overload;
function IsInsideRect(APoint: TPoint; ARect: TRect): Boolean; overload;
function FormatNumber(ANumber: Extended; ANumberFormat: TNumberFormat; AFractionalDigits: Integer = 1): String;
// 15.09.14
function DigitsNeeded(ARange: TValueRange): integer;
// 09.10.14
function DigitsNeeded(AMainInterval: Extended): integer;
// new utils 10.10.12 for simplifying Axis drawindicator and innergrid
function MainTickInterval(ARange: TValueRange; ALogBase: Extended; AIsLogScale: Boolean): Extended;
function MainTickRange(ARange: TValueRange; ALogBase: Extended; AIsLogScale: Boolean): TValueRange;
//
function SimpleRoundUpDown(const AValue: Extended; AUpwards: Boolean; const Digits: TRoundToRange = -2): Extended;

procedure QuickSort_XYLine(var AXYLine: TXYLine; IdxStart, IdxStop: Integer; SortY: Boolean = false);


const
  c_SUCCESS = 0;
  c_NOAXIS = -1;
  c_VALUE_OUTSIDE_VIEWRANGE = -2;
  c_FPE = -4;
  c_IPOL_EDGEPOINT = -8;
  c_ERRUNKNOWN = -100;

implementation

//uses uPlotAxis;

resourcestring
  S_WrongAxisType   = 'Wrong Axis Type';
  S_UnknownError    = 'Unknown Error';


function ValueToMathCoord(AAxis: uPlotClass.TPlotAxisBase; AValue: Extended; out MathPt: TFloatPoint): integer;
// Returns math coordinate in pixels relaitve to axis origin set to 0-0
var
  vZero, vOrigin : TFloatPoint;
  vAngle, vR: Extended;
begin
  Result := c_SUCCESS;

  {disabled 03.09.14 for correct interpolator function
  IF (AValue < AAxis.ViewRange.min) OR (AValue > AAxis.ViewRange.max)
     THEN BEGIN
            Result := c_VALUE_OUTSIDE_VIEWRANGE;
            MathPt.X := c_INVALIDCOORDINATE;
            MathPt.Y := c_INVALIDCOORDINATE;
            exit;
          END;
  }


  IF AAxis = nil THEN begin
    Result := c_NOAXIS;
    MathPt := FloatPoint(0,0);
    exit;
  end;

  IF (AValue < AAxis.ViewRange.min) OR (AValue > AAxis.ViewRange.max)
     THEN BEGIN
            Result := c_VALUE_OUTSIDE_VIEWRANGE;
          END;
  // we calculate however... XYZtoDataImage shall respect this outside problem


  vZero.X:=0; vZero.Y:=0;
  IF AAxis is uPlotAxis.TPlotAxis
    THEN BEGIN
           vOrigin := vZero;
           vangle := uPlotAxis.TPlotAxis(AAxis).DrawAngle;
         END ELSE BEGIN
           vOrigin := vZero;
           IF uPlotClass.TPlotAxisBase(AAxis).Orientation = aoHorizontal
             THEN vAngle := 0 ELSE vAngle := 90;
         END;

  // convert axis points

  IF uPlotAxis.TPlotAxis(AAxis).LogScale AND (AValue <= 0) THEN BEGIN
    Result := c_FPE;
 //writeln('problem: ',FloatToStrF((AValue),ffFixed,4,3));
    exit;
  END;

  IF uPlotAxis.TPlotAxis(AAxis).LogScale THEN
     vR := (logn(uPlotAxis.TPlotAxis(AAxis).LogBase, AValue) - logn(uPlotAxis.TPlotAxis(AAxis).LogBase, AAxis.ViewRange.min))
            * AAxis.PixelsPerValue ELSE
     vR := (AValue - AAxis.ViewRange.min)* AAxis.PixelsPerValue;

  MathPt := LineEndPoint(vOrigin, vAngle*16, vR);
  MathPt.Y := - MathPt.Y;
end;

function MathCoordToValue(AAxis: uPlotClass.TPlotAxisBase; MathPt: TFloatPoint;
  out AValue: Extended): integer;
// Returns value relaitve to axis origin set to 0-0 (please respect shift upwards and give shifted point !)
var
  vAngle, vPixels, vR: Extended;
  vValue: Extended;
  vMindB, vMaxdB: Extended;
begin
  Result := c_SUCCESS;

  IF AAxis = nil THEN begin
    Result := c_NOAXIS;
    AValue := 0;
    exit;
  end;

  // find vR, projected to axis
  vR := sqrt( sqr(MathPt.X) + sqr(MathPt.Y) );
  if MathPt.X <> 0 then
    vAngle := arctan(MathPt.Y / MathPt.X)
  else if (MathPt.Y < 0) then vAngle := 1.5*Pi() else vAngle := Pi()/2;

  vAngle := vAngle - TPlotAxis(AAxis).DrawAngle * Pi() / 180;
  vPixels := vR * cos(vAngle) / TPlotAxis(AAxis).DrawLength;  // fraction of axis (when shifted to 0/0)

  IF uPlotAxis.TPlotAxis(AAxis).LogScale THEN BEGIN
    vMindB := TPlotAxis(AAxis).ReCalcValue(TPlotAxis(AAxis).ViewRange.min);
    vMaxdB := TPlotAxis(AAxis).ReCalcValue(TPlotAxis(AAxis).ViewRange.max);
    vValue := TPlotAxis(AAxis).ViewRange.min *  TPlotAxis(AAxis).ReCalcValueInverse(vPixels * (vMaxdB - vMindB));
  END ELSE BEGIN
    vValue := TPlotAxis(AAxis).ViewRange.min + vPixels * (TPlotAxis(AAxis).ViewRange.max - TPlotAxis(AAxis).ViewRange.min);
  END;

  IF (vValue < AAxis.ViewRange.min) OR (vValue > AAxis.ViewRange.max)
     THEN Result := c_VALUE_OUTSIDE_VIEWRANGE;

  // we calculate however... XYZtoDataImage shall respect this outside problem
  AValue:=vValue;
end;

function XYZToDataImage(XAxis, YAxis, ZAxis: TPlotAxisBase; X, Y, Z: Extended;
  out ScrPt: TPoint): Integer;
var
  vError: Integer;
  vMathPt: TFloatPoint;
  vScrPt: TPoint;
  vOrigin, vNextOrigin: TPoint;
begin
  Result := 0;
  vScrPt.X:=0; vScrPt.Y:=0;
  // XAxis
  IF XAxis <> nil THEN BEGIN
    vOrigin := TPLotAxis(XAxis).DrawOriginRel;
    vError := ValueToMathCoord(XAxis, X, vMathPt);
    if vError >= 0 then begin
      vScrPt.X := vOrigin.X + round(vMathPt.X);
      vScrPt.Y := -vOrigin.Y - round(vMathPt.Y);
    end else begin
      Result := min(Result, vError);
    end;
  END;
  // YAxis
  IF YAxis <> nil THEN BEGIN
    vNextOrigin := TPLotAxis(YAxis).DrawOriginRel;
    vScrPt.Y := vScrPt.Y - (vNextOrigin.Y-vOrigin.Y);  // shift to Y-origin
    vError := ValueToMathCoord(YAxis, Y, vMathPt);
    if vError >= 0 then begin
      vScrPt.X := vScrPt.X + round(vMathPt.X);
      vScrPt.Y := vScrPt.Y - round(vMathPt.Y);
    end else begin
        Result := min(Result, vError);
    end;
  END;
  // ZAxis
  IF ZAxis <> nil THEN BEGIN
    vNextOrigin := TPLotAxis(ZAxis).DrawOriginRel;
    vScrPt.Y := vScrPt.Y - (vNextOrigin.Y-vOrigin.Y);  // shift to Z-origin
    vScrPt.X := vScrPt.X + (vNextOrigin.X-vOrigin.X);  // shift to Z-origin
    vError := ValueToMathCoord(ZAxis, Z, vMathPt);
    if vError >= 0 then begin
      vScrPt.X := vScrPt.X + round(vMathPt.X);
      vScrPt.Y := vScrPt.Y - round(vMathPt.Y);
    end else begin
        Result := min(Result, vError);
    end;
  END;

  vScrPt.Y := vScrPt.Y +  (XAxis.OwnerPlotRect.DataRect.Bottom - XAxis.OwnerPlotRect.DataRect.Top);
  ScrPt := vScrPt;

  // results: 0 = OK
  //         -1 = c_outside_viewrange, screenpoint is interpolated to edge of viewport; break interpol line here...
  //         -2 = c_FPE some float exception, do not use point
end;

function ValueToScreen(AAxis: uPlotClass.TPlotAxisBase; AValue: Extended; out ScrPt: TPoint): integer;
// Returns Screen Point (directly useable for outer total image)
// TODO: use ValueToMathCoord to simplify this function ?
var
  vOrigin: TPoint;  //  vOriginRel, vPt
  vAngle, vR: Extended;
begin
  Result := c_SUCCESS;
  try
  IF not ((AAxis is uPlotAxis.TPlotAxis) OR (AAxis is uPlotAxis.TPlotAxis))
     THEN raise EPlot.CreateRes(@S_WrongAxisType);

  IF (AValue > c_GLOBALMAX) OR (AValue < -c_GLOBALMAX) THEN exit;

//  IF uPlotAxis.TPlotAxis(AAxis).LogScale;

  IF AAxis is uPlotAxis.TPlotAxis
    THEN BEGIN
           //vOriginRel := uPlotAxis.TPlotAxis(AAxis).DrawOriginRel;
           vOrigin := uPlotAxis.TPlotAxis(AAxis).DrawOrigin;
           vangle := uPlotAxis.TPlotAxis(AAxis).DrawAngle;
         END ELSE BEGIN
           //vOriginRel.X := 0;
           //vOriginRel.Y := 0;
           vOrigin := uPlotClass.TPlotAxisBase(AAxis).OwnerPlotRect.BottomLeft;
           IF uPlotClass.TPlotAxisBase(AAxis).Orientation = aoHorizontal
             THEN vAngle := 0 ELSE vAngle := 90;
         END;

  // convert X-axis and Y-axis points

  IF uPlotAxis.TPlotAxis(AAxis).LogScale THEN
     vR := (logn(uPlotAxis.TPlotAxis(AAxis).LogBase, AValue) - logn(uPlotAxis.TPlotAxis(AAxis).LogBase, AAxis.ViewRange.min))
            * AAxis.PixelsPerValue ELSE
     vR := (AValue - AAxis.ViewRange.min)* AAxis.PixelsPerValue;

  //ScrPt := LineEndPoint(vOriginRel, vAngle*16, vR);
  ScrPt := LineEndPoint(vOrigin, vAngle*16, vR);

  except
      on E:EPlot do begin
         MessageDlg('Error', E.Message, mtInformation, [mbOK], 0);
         Result := c_ERRUNKNOWN;
         end;
  end;
end;

function XYToScreen(XAxis, YAxis: uPlotClass.TPlotAxisBase; X,
  Y: Extended; out ScrPt: TPoint): integer;
var
  vMathPtX, vMathPtY: TFloatPoint;
  vPt: TPoint;
  vXOrigin, vYOrigin: TPoint;
  vError: Integer;
begin
  Result := c_SUCCESS;
  try
  IF not ((XAxis is uPlotAxis.TPlotAxis) AND (YAxis is uPlotAxis.TPlotAxis))
     THEN raise EPlot.CreateRes(@S_WrongAxisType);

  IF (X < XAxis.ViewRange.min) OR (X > XAxis.ViewRange.max)
     OR (Y < YAxis.ViewRange.min) OR (Y > YAxis.ViewRange.max)
     THEN BEGIN
            Result := c_VALUE_OUTSIDE_VIEWRANGE;
            ScrPt.X := c_INVALIDCOORDINATE;
            ScrPt.Y := c_INVALIDCOORDINATE;
            exit;
          END;

  // Here is the coordinate conversion float to Canvas

  IF XAxis is uPlotAxis.TPlotAxis
    THEN BEGIN
           vXOrigin := uPlotAxis.TPlotAxis(XAxis).DrawOrigin;
         END ELSE BEGIN
           vXOrigin := uPlotClass.TPlotAxisBase(XAxis).OwnerPlotRect.BottomLeft;
         END;

  IF YAxis is uPlotAxis.TPlotAxis
    THEN BEGIN
           vYOrigin := uPlotAxis.TPlotAxis(YAxis).DrawOrigin;
         END ELSE BEGIN
           vYOrigin := uPlotClass.TPlotAxisBase(YAxis).OwnerPlotRect.BottomLeft;
         END;

  // convert X-axis and Y-axis points

  vError := uPlotUtils.ValueToMathCoord(XAxis, X, vMathPtX);
  vError := uPlotUtils.ValueToMathCoord(YAxis, Y, vMathPtY);

  IF vError < 0 THEN BEGIN
    IF vError = c_FPE THEN Result := c_FPE ELSE Result := c_ERRUNKNOWN;
    ScrPt.X := c_INVALIDCOORDINATE;
    ScrPt.Y := c_INVALIDCOORDINATE;
    raise EPlot.CreateRes(@S_UnknownError);
    //exit;
  END;

  vPt.X := round(vMathPtX.X) + round(vMathPtY.X) + vXOrigin.X;
  vPt.Y := -round(vMathPtX.Y) - round(vMathPtY.Y) + vYOrigin.Y;

  ScrPt := vPt;

  except
      on E:EPlot do begin
         MessageDlg('Error XYtoScreen', E.Message, mtInformation, [mbOK], 0);
         Result := c_ERRUNKNOWN;
         end;
  end;
end;


function ScreenToXY(XAxis, YAxis: uPlotClass.TPlotAxisBase; out X, Y: Extended;
  ScrPt: TPoint): integer;
var
  vPixelPerCoord: Extended;
  vPixels: Integer;
  vValue: Extended;
begin
  Result := 0;
  // Xaxis
  IF cos(TPlotAxis(XAxis).DrawAngle * pi/180) = 0 THEN vPixelPerCoord := c_GLOBALMAX
    ELSE vPixelPerCoord := (1 / cos(TPlotAxis(XAxis).DrawAngle * pi/180)) * XAxis.PixelsPerValue;
  vPixels := (ScrPt.X - TPlotAxis(XAxis).DrawOrigin.X); // TODO: check for angles with cos < 0
  IF vPixels = 0 THEN BEGIN
    Result := c_FPE;
    exit;
  END;

  IF not uPlotAxis.TPlotAxis(XAxis).LogScale THEN
    vValue := TPlotAxis(XAxis).ViewRange.min + (1 / (vPixelPerCoord / vPixels))
  ELSE vValue := TPlotAxis(XAxis).ViewRange.min * power(uPlotAxis.TPlotAxis(XAxis).LogBase,  1 /(vPixelPerCoord / vPixels));
  X := vValue;

  // Yaxis
  IF sin(TPlotAxis(YAxis).DrawAngle * pi/180) = 0 THEN vPixelPerCoord := -c_GLOBALMAX
    ELSE vPixelPerCoord := (1 / sin(TPlotAxis(YAxis).DrawAngle * pi/180)) * YAxis.PixelsPerValue;
  vPixels := (TPlotAxis(YAxis).DrawOrigin.Y - ScrPt.Y) ; // TODO: check for angles with cos < 0
  IF vPixels = 0 THEN BEGIN
    Result := c_FPE;
    exit;
  END;

  IF not uPlotAxis.TPlotAxis(YAxis).LogScale THEN vValue := TPlotAxis(YAxis).ViewRange.min + (1 / (vPixelPerCoord / vPixels))
  ELSE vValue := TPlotAxis(YAxis).ViewRange.min * power(uPlotAxis.TPlotAxis(YAxis).LogBase,  1 /(vPixelPerCoord / vPixels));
  Y := vValue;
end;

function XYZToScreen(XAxis, YAxis, ZAxis: uPlotClass.TPlotAxisBase; X, Y,
  Z: Extended; out ScrPt: TPoint): integer;
var
  vMathPtX, vMathPtY, vMathPtZ: TFloatPoint;
  vPt: TPoint;
  vXOrigin, vYOrigin: TPoint; //vZOrigin: TPoint;
  vError : Integer;
  vWrongAxisError: Boolean;
  vOutSide: Boolean;
begin
  vOutSide:=false;

  vWrongAxisError :=false;

  vWrongAxisError := vWrongAxisError and not ((XAxis is TPlotAxis) or (XAxis = nil));
  vWrongAxisError := vWrongAxisError and not ((YAxis is TPlotAxis) or (YAxis = nil));
  vWrongAxisError := vWrongAxisError and not ((ZAxis is TPlotAxis) or (ZAxis = nil));
  IF vWrongAxisError
    THEN raise EPlot.CreateRes(@S_WrongAxisType);


  vOutSide := (X < XAxis.ViewRange.min) OR (X > XAxis.ViewRange.max)
              OR (Y < YAxis.ViewRange.min) OR (Y > YAxis.ViewRange.max);
  IF (ZAxis <> nil) then vOutSide := vOutSide OR (Z < ZAxis.ViewRange.min) OR (Z > ZAxis.ViewRange.max) ;
  IF vOutSide THEN
  BEGIN
    Result := c_VALUE_OUTSIDE_VIEWRANGE;
    ScrPt.X := c_INVALIDCOORDINATE;
    ScrPt.Y := c_INVALIDCOORDINATE;
    exit;
  END;

  // Here is the coordinate conversion float to Canvas

  IF XAxis is uPlotAxis.TPlotAxis
    THEN BEGIN
           vXOrigin := uPlotAxis.TPlotAxis(XAxis).DrawOrigin;
         END ELSE
  IF XAxis is TPlotAxisBase THEN BEGIN
           vXOrigin := uPlotClass.TPlotAxisBase(XAxis).OwnerPlotRect.BottomLeft;
         END ELSE begin
           vXOrigin := Point(0,0);
         end;

  IF YAxis is uPlotAxis.TPlotAxis
    THEN BEGIN
           vYOrigin := uPlotAxis.TPlotAxis(YAxis).DrawOrigin;
         END ELSE
  IF YAxis is TPlotAxisBase THEN BEGIN
           vYOrigin := uPlotClass.TPlotAxisBase(YAxis).OwnerPlotRect.BottomLeft;
         END ELSE begin
           vYOrigin := Point(0,0);
         end;

  { // Z axis origin not needed because it is not respected
  IF ZAxis is uPlotAxis.TPlotAxis
    THEN BEGIN
           vZOrigin := uPlotAxis.TPlotAxis(ZAxis).DrawOrigin;
         END ELSE
  IF ZAxis is TPlotAxisBase THEN BEGIN
           vZOrigin := uPlotClass.TPlotAxisBase(ZAxis).OwnerPlotRect.BottomLeft;
         END ELSE begin
                    vZOrigin := Point(0,0);
                  end;
  }

  // convert X-axis and Y-axis points
  try

      vError := uPlotUtils.ValueToMathCoord(XAxis, X, vMathPtX);
      vError := min(vError, uPlotUtils.ValueToMathCoord(YAxis, Y, vMathPtY));
      vError := min(vError, uPlotUtils.ValueToMathCoord(ZAxis, Z, vMathPtZ));

    IF vError = c_NOAXIS THEN vError:=c_SUCCESS;        //c_NOAXIS is without problem

    IF (vError < 0) THEN BEGIN
      Result := vError;
      ScrPt.X := c_INVALIDCOORDINATE;
      ScrPt.Y := c_INVALIDCOORDINATE;
      exit;
    END;

    vPt.X := round(vMathPtX.X) + round(vMathPtY.X) + round(vMathPtZ.X) + vXOrigin.X;
    vPt.Y := -round(vMathPtX.Y) - round(vMathPtY.Y) - round(vMathPtZ.Y) + vYOrigin.Y;

    ScrPt := vPt;
    Result := vError;

  except
      on E:EPlot do begin
         MessageDlg('Error', E.Message, mtInformation, [mbOK], 0);
         Result := c_ERRUNKNOWN;
         end;
  end;
end;

function ShiftPoint(AShiftSize: TSize; var ScrPt: TPoint): integer;
begin
  Result := c_SUCCESS;
  ScrPt.X := ScrPt.X - AShiftSize.cx;
  ScrPt.Y := ScrPt.Y + AShiftSize.cy;
end;

function PlotImageCoordsToFrameRectCoords(APlotRect: TBasePlotRect;
  var ScrPt: TPoint): Integer;
begin
  ScrPt.X := ScrPt.X - APlotRect.FrameRect.Left;
  ScrPt.Y := ScrPt.Y - APlotRect.FrameRect.Top;
  Result := 0;
end;

function PlotImageCoordsToDataRectCoords(APlotRect: TBasePlotRect;
  var ScrPt: TPoint): Integer;
begin
  ScrPt.X := ScrPt.X - APlotRect.DataRect.Left;
  ScrPt.Y := ScrPt.Y - APlotRect.DataRect.Top;
  Result := 0;
end;

function ValueTo125(AValue: Extended; AUpwards: Boolean; APower: Integer): extended;
var
  vResultValue: Extended;
  vDigit: Integer;
  vDigitStr: String;
  vSign: TValueSign;
  vpower: Integer;
begin
  vDigit := 1;
  IF (APower < -36) OR (APower > 36) THEN begin
    Result := AValue;
    exit;
  end;

  vResultValue:=AValue;

  IF abs(vResultValue) > c_GLOBALMIN THEN BEGIN
    vSign := Sign(vResultValue);
    vpower := floor(log10(vResultValue));
    vDigitStr := FloatToStrF(abs(vResultValue), ffExponent,2,2);
    IF AUpwards THEN
      CASE vDigitStr[1] OF
        '0':    vDigit := 1;
        '1':    vDigit := 2;
        '2'..'4': vDigit := 5;
        '5'..'9': vDigit := 10;
      END
    ELSE
      CASE vDigitStr[1] OF
        '1':    vDigit := 0;
        '2':    vDigit := 1;
        '3'..'5': vDigit := 2;
        '6'..'9': vDigit := 5;
      END;
    vResultValue := vSign * vDigit * intpower(10, vpower);
  END ELSE vResultValue := 0;

  Result := vResultValue;

end;

function ValueToNext(AValue: Extended; AUpwards: Boolean; APower: Integer): extended;
begin
  IF abs(AValue) < c_GLOBALMIN THEN begin
    Result := 0;
    exit;
  end;
  IF (APower < -36) OR (APower > 36) THEN begin
    Result := AValue;
    exit;
  end;

  Result := SimpleRoundUpDown(AValue, AUpwards, APower);
end;

function IsInsideRect(X, Y: Integer; ARect: TRect): Boolean;
begin
  Result := FALSE;
  IF (X >= ARect.Left) AND (X <= ARect.Right)
      AND (Y >= ARect.Top) AND (Y <= ARect.Bottom) THEN Result := TRUE;
end;

function IsInsideRect(APoint: TPoint; ARect: TRect): Boolean; overload;
begin
  Result := FALSE;
  IF (APoint.X >= ARect.Left) AND (APoint.X <= ARect.Right)
      AND (APoint.Y >= ARect.Top) AND (APoint.Y <= ARect.Bottom) THEN Result := TRUE;
end;

function FormatNumber(ANumber: Extended; ANumberFormat: TNumberFormat; AFractionalDigits: Integer = 1): String;
var
  //vFloatSettings: TFormatSettings;
  vPower : Integer;
  vValue: Extended;
  vEngStep: Integer;
  fractionaldigits: Integer;
const
  //precision = 2;    // only for built-in function, after comma digits for exp notation
  //digits = 1;       // only for built-in functions, after comma digits for ffFixed
  //format = ffFixed; // only for built-in functions
  //fractionaldigits = 1;
  floatcharacter = 'e';
  cEngString = '_afpnµm kMGTPE_'; // 8 is plain
begin
  // 15.09.14 estimate needed number of fractionaldigits
  //fractionaldigits := min(c_MAININTERVAL_MAXDIGITS, AFractionalDigits);
  fractionaldigits := EnsureRange(AFractionalDigits, 0, c_MAININTERVAL_MAXDIGITS);


  //vFloatSettings.DecimalSeparator := ',';
  case ANumberFormat of
  nfPlain:             begin
                         //Result := IntToStr(round(ANumber));
                         Result := FloatToStrF(ANumber, ffFixed,0,2);  // precision (0) used for exp formates
                       end;
  nfFloat:             begin
                        //if fractionaldigits < 0 then fractionaldigits := 0;
                        IF ANumber = 0 THEN begin
                            Result := '0';
                            exit;
                           end ELSE
                           vPower := trunc(logn(10, abs(ANumber) ));
                        // when power <0 switches to next power at 1 not at 10
                        // power >0 -->numbers 1..9      // power <0 --> numbers 10e-3..9e-2
                        // solution "IF...=10" below:
                        IF (abs(ANumber) < 1)  THEN vPower := vPower -1;
                         vValue := ANumber / power(10,vPower);

                         fractionaldigits := fractionaldigits + vPower;
                         fractionaldigits := EnsureRange(fractionaldigits, 0, c_MAININTERVAL_MAXDIGITS);
                         //if fractionaldigits < 0 then fractionaldigits := 0;
                         vValue := SimpleRoundTo(vValue, -fractionaldigits);

                        IF round(abs(vValue)) = 10 THEN begin     // int()
                          vValue:= vValue / 10;
                          vPower:= vPower + 1;
                        end;

                         Result := FloatToStr(vValue) + floatcharacter + IntToStr(vPower);
                       end;
  nfEngineering:        begin
                        //writeln('digits: ', IntToStr(fractionaldigits));
                        IF ANumber = 0 THEN begin
                            Result := '0';
                            exit;
                           end ELSE
                           vPower := trunc(logn(10, abs(ANumber) ));
                        IF abs(ANumber) < 1 THEN vPower := vPower -3;
                         vEngStep := vPower DIV 3;
                         //writeln('value before div: ', FloatToStrF(ANumber, ffFixed, 15, 5));
                         vValue := ANumber / power(10,(vEngStep * 3));
                         //writeln('value after div : ', FloatToStrF(vValue, ffFixed, 15, 5));

                         fractionaldigits:=fractionaldigits + (abs(vEngStep) * 3);
                         fractionaldigits := EnsureRange(fractionaldigits, 0, c_MAININTERVAL_MAXDIGITS);
                         vValue:=RoundTo(vValue, -fractionaldigits);

                        IF round(abs(vValue)) = 1000 THEN begin     // int()
                          vValue:= vValue / 1000;
                          vEngStep:= vEngStep + 1;
                        end;

                         Result := FloatToStr(vValue) + floatcharacter + IntToStr(vEngStep * 3);
                         //Result := FloatToStrF(vValue, ffFixed, 15, fractionaldigits) + floatcharacter + IntToStr(vEngStep * 3);
                       end;
  nfEngineeringPrefix: begin
                         IF ANumber = 0 THEN begin
                            Result := '0';
                            exit;
                           end ELSE
                           vPower := trunc(logn(10, abs(ANumber) ));
                         IF abs(ANumber) < 1 THEN vPower := vPower -3;
                         vEngStep := vPower DIV 3;
                         //vValue := SimpleRoundTo(vValue, -fractionaldigits);
                         vValue := ANumber / power(10,(vEngStep * 3));

                         fractionaldigits := fractionaldigits + (abs(vEngStep) * 3);
                         fractionaldigits := EnsureRange(fractionaldigits, 0, c_MAININTERVAL_MAXDIGITS);
                         vValue:=RoundTo(vValue, -fractionaldigits);

                         IF round(abs(vValue)) = 1000 THEN begin     // int()
                           vValue:= vValue / 1000;
                           vEngStep:= vEngStep + 1;
                         end;

                         IF (vEngStep < -6) OR (vEngStep > 6) THEN
                           //Result := FloatToStr(vValue) + 'e' + IntToStr(vPower)
                           Result := FloatToStrF(vValue, ffFixed, 10, fractionaldigits) + floatcharacter + IntToStr(vEngStep * 3)
                         ELSE
                           Result := FloatToStr(vValue) + UTF8Copy(cEngString, 8+vEngStep, 1); //  cEngString[8+vEngStep];
                           //Result := FloatToStrF(vValue, ffFixed, 15, fractionaldigits) + cEngString[8+vEngStep];
                       end;

  end;
end;

function DigitsNeeded(ARange: TValueRange): integer;
var
  vFractionalDigits: Integer;
begin
  Result := 1;
  IF (ARange.max - ARange.min) <= 0 then exit;
   vFractionalDigits := trunc(log10( (ARange.max - ARange.min) ));
   //IF vFractionalDigits > 0 then Result := 0 else Result := abs(vFractionalDigits) + 0; // +1/ 2 ??
   Result := -(vFractionalDigits) + 1; // +1/ 2 ??
end;

function DigitsNeeded(AMainInterval: Extended): integer;
begin
  // if interval >=1 then we need 0 fractional digits
  // if < 1 we need digits according to fraction
  Result := trunc(abs(EnsureRange( log10(AMainInterval), -c_MAININTERVAL_MAXDIGITS , 0)) + 1); // -n ..0; 0 digits for values > 1
end;

function MainTickInterval(ARange: TValueRange; ALogBase: Extended;
  AIsLogScale: Boolean): Extended;
var
  vPower : Extended;
  vRange : Extended;
begin
  Result := c_GLOBALMIN;
  vRange := (ARange.max) - (ARange.min);
  //IF vRange < c_GLOBALMIN THEN vRange := c_GLOBALMIN; // too much digits ! leads to 45 digits
  IF vRange < intpower(10, -c_MAININTERVAL_MAXDIGITS) THEN vRange := intpower(10, -c_MAININTERVAL_MAXDIGITS) ;
  IF AIsLogScale OR (ALogBase=1) THEN begin
    Result := 1;
    exit end
  ELSE begin
    vPower:= trunc(logn(ALogBase,vRange) );
    IF vRange < 1 THEN vPower:= vPower -1;  // always round down
    Result := ( power(ALogBase,vPower) ); // round for paranoia reason ? should be integer like
  end;
end;

function MainTickRange(ARange: TValueRange; ALogBase: Extended;
  AIsLogScale: Boolean): TValueRange;
begin
  IF AIsLogScale THEN BEGIN
    IF ARange.min <= 0 THEN ARange.min := c_GLOBALMIN;
    Result.min := power(ALogBase, trunc(logn(ALogBase,ARange.min)));
    IF ARange.max <= ARange.min THEN ARange.max := ARange.min + c_GLOBALMIN;
    Result.max := power(ALogBase, trunc(logn(ALogBase,ARange.max))+1);
  END ELSE BEGIN
    IF abs(ARange.min) < c_GLOBALMIN THEN Result.min := 0 else begin
      // new code for rounding:
      Result.min := ARange.min / MainTickInterval(ARange, ALogBase, AIsLogScale); //      power(vMinMax.min, -vPower);
      IF Result.min < 0 THEN Result.min := Result.min-1;
      Result.min := trunc(Result.min);
      Result.min := Result.min * MainTickInterval(ARange, ALogBase, AIsLogScale);
    end;
    // high end point
    IF abs(ARange.max) < c_GLOBALMIN THEN Result.max := 0 else begin       // oder 0 ? Result.max := c_GLOBALMIN
      // new code for rounding:
      Result.max := ARange.max / MainTickInterval(ARange, ALogBase, AIsLogScale);  //power(vMinMax.max, -vPower);
      IF Result.max < 0 THEN Result.max := Result.max-1;
      Result.max := trunc(Result.max+1);
      Result.max := Result.max * MainTickInterval(ARange, ALogBase, AIsLogScale);
    end;
  END;
end;


function SimpleRoundUpDown(const AValue: Extended; AUpwards: Boolean; const Digits: TRoundToRange = -2): Extended;
var
  RV : Extended;
  vUpDown : integer;
begin
  IF (Digits < -36) OR (Digits > 36) THEN begin
    Result := AValue;
    exit;
  end;
  if AUpwards and (AValue > 0) THEN vUpDown := 1 * Sign(AValue) ELSE vUpDown := 0; //?????????
  //vUpDown:=0;
  RV := IntPower(10, -Digits);

  if AValue < 0 then
    Result := Trunc((AValue*RV) - 1 + vUpDown)/RV // rounds betrag down
  else
    Result := Trunc((AValue*RV) + 0 + vUpDown)/RV;// rounds betrag down
end;

procedure QuickSort_XYLine(var AXYLine: TXYLine; IdxStart, IdxStop: Integer;
  SortY: Boolean);
var
  vL, vR: Integer;
  Pivot: Extended;
  vTempXYValue: TXYValue;
begin
  if IdxStart < IdxStop then
  begin
       vL := IdxStart;
       vR := IdxStop-1;
       IF SortY THEN Pivot := AXYLine[IdxStop].Y ELSE
         Pivot := AXYLine[IdxStop].X;

       repeat
             IF SortY THEN BEGIN
               while (AXYLine[vL].Y <= Pivot) and (vL < IdxStop) do
               begin
                     vL := vL + 1;
               end;

               while (AXYLine[vR].Y >= Pivot) and (vR > IdxStart) do
               begin
                     vR := vR -1;
               end;
             END ELSE BEGIN
               while (AXYLine[vL].X <= Pivot) and (vL < IdxStop) do
                begin
                      vL := vL + 1;
                end;

                while (AXYLine[vR].X >= Pivot) and (vR > IdxStart) do
                begin
                      vR := vR -1;
                end;
             END;

             if vL < vR then
             begin
                  vTempXYValue := AXYLine[vR];
                  AXYLine[vR] := AXYLine[vL];
                  AXYLine[vL] := vTempXYValue;
             end;
       until NOT (vL < vR);

       IF SortY AND (AXYLine[vL].Y > Pivot) THEN begin
         vTempXYValue := AXYLine[vL];
         AXYLine[vL] := AXYLine[IdxStop];
         AXYLine[IdxStop] := vTempXYValue;
       end ELSE
       IF (NOT SortY) AND (AXYLine[vL].X > Pivot) THEN begin
         vTempXYValue := AXYLine[vL];
         AXYLine[vL] := AXYLine[IdxStop];
         AXYLine[IdxStop] := vTempXYValue;
       end;

       QuickSort_XYLine(AXYLine, IdxStart, vL-1, SortY);
       QuickSort_XYLine(AXYLine, vL+1 , IdxStop, SortY);
  end;
end;


end.


