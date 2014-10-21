unit uMarkerDrawer;
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
//{$DEFINE GTK2}

interface

uses
  Classes, SysUtils, useriesmarkers, Graphics, uPlotSeries, uPlotUtils, uPlotDataTypes, IntfGraphics, FPimage,
  math, GraphType, GraphUtil, GraphMath; //uPlotOverrides, uIntfImageDrawingUtils

type

  // TODO: Marker doesNOT need OwnerContainer - rome this !?

  { TMarkerDrawer }

  { TMarkerDrawer_Bitmap }

  TMarkerDrawer_Bitmap = class(TMarkerDrawer)
  private
    FMarkerImage: TLazIntfImage;
    FMarkerBmp: TBitmap;
    FReadOutImage: TLazIntfImage;
    FReadoutBmp: TBitmap;
    function DrawTriangle(ADiameter: Integer; AAngle: Extended; AFPColor: TFPColor; AFilled: Boolean; AFlip: Boolean; out OriginShift: TPoint): TRect;
    //function DrawLine(AStartPt, AEndPt: TPoint; ADiameter: Integer; AAngle: Extended; AFPColor: TFPColor; AFilled: Boolean; out OriginShift: TPoint): TRect;  // Aangle not needed
    function DrawLine(AStartPt, AEndPt: TPoint; ADiameter: Integer; AFPColor: TFPColor; AFilled: Boolean; out OriginShift: TPoint): TRect;  // Aangle not needed
    //function DrawCircle(ADiameter: Integer; AAngle: Extended; AFPColor: TFPColor; AFilled: Boolean; out OriginShift: TPoint): TRect;
    procedure DrawReadout(AMarker: TMarker; AFPColor: TFPColor; ABackGndFPColor: TFPColor);
  protected
  public
    constructor Create(AOwnerContainer: TMarkerContainer);override;
    destructor Destroy; override;

    procedure DrawMarker(AMarker: TMarker; ABitMap: TBitmap; IsGlobalImage: Boolean);override;
  end;



implementation

uses uPlotAxis;

{ TMarkerDrawer }

function TMarkerDrawer_Bitmap.DrawTriangle(ADiameter: Integer; AAngle: Extended; AFPColor: TFPColor; AFilled: Boolean; AFlip: Boolean; out OriginShift: TPoint): TRect;
var
  vPointArray: array of TPoint;
  vFillArray: array of TPoint;
  vFillPt: TPoint;
  //Pt: TPoint;
  vCanvas: TCanvas;
  vCol, vRow: Integer;
  vFPColor: TFPColor;
  vColor: TColor;
  vLoopFill: Integer;
  vHue, vLight, vSat: byte;
  vNewLight: byte;
  vBackgroundColor: TColor;
  vDestRect: TRect;
  vDistance: Integer;
begin
  // TODO:
  // --> use directly the image
  vColor:=FPColorToTColor(AFPColor);
  vBackgroundColor := clCream;
  if vColor = vBackgroundColor then vBackgroundColor := clWhite;

  IF (not odd(ADiameter)) then ADiameter:=ADiameter+1;

  IF FMarkerBmp <> nil THEN FMarkerBmp.Free;
  FMarkerBmp := TBitmap.Create;
  FMarkerBmp.PixelFormat:=pf32bit;
  FMarkerBmp.SetSize(2*ADiameter, 2*ADiameter); //FMarkerBmp.SetSize(ADiameter, ADiameter);
  vCanvas := FMarkerBmp.Canvas;
  vCanvas.Brush.Color:=vBackgroundColor;
  vCanvas.FillRect(0,0,2*ADiameter-1, 2*ADiameter-1);  //vCanvas.FillRect(0,0,ADiameter-1, ADiameter-1);

  begin
      // set pen properties
      vCanvas.Pen.Color := vColor;
      //Canvas.Pen.Width := (ADiameter DIV 8) + 1;
      vCanvas.Pen.Width := 1;
      vCanvas.Pen.Mode := pmCopy;
      vCanvas.Pen.EndCap:=pecRound;
      vCanvas.Pen.JoinStyle:=pjsRound;
      vCanvas.Brush.Style := bsClear;

    setlength(vFillArray, 0);
    setlength(vPointArray, 3);                                           //
    vPointArray[0].X := ADiameter;                                       //        1 - 2
    vPointArray[0].Y := ADiameter;                                       //         \ /
    //                                                                   //          0

    IF AFlip then AAngle:=AAngle-180;

    IF AAngle = 0 THEN begin
      vPointArray[1].X := vPointArray[0].X  - (ADiameter DIV 2);  // cos 30°
      vPointArray[1].Y := vPointArray[0].Y - ADiameter +1; // sin 60° is 0,86 not 1 so we are 14% high
      vPointArray[2].X := vPointArray[0].X + (ADiameter DIV 2) -1;  // cos 30°
      vPointArray[2].Y := vPointArray[0].Y - ADiameter +1;

      //vCanvas.Polygon(vPointArray);  // other overloaded funcs available
      IF (not AFilled) or AFilled  THEN begin                                   // always draw border ?
        vCanvas.Line(vPointArray[0].X, vPointArray[0].Y, vPointArray[1].X, vPointArray[1].Y);
        vCanvas.Line(vPointArray[0].X, vPointArray[0].Y, vPointArray[2].X, vPointArray[2].Y);
        vCanvas.Line(vPointArray[1].X, vPointArray[1].Y, vPointArray[2].X, vPointArray[2].Y);
      end;
      IF AFilled then begin
        ColorToHLS(vColor, vHue, vLight, vSat);
        for vLoopFill:= 0 to ADiameter-1 do begin
          vNewLight:= min( vLight + round((255-vLight) *2 / ADiameter * abs(abs(vLoopFill-(ADiameter DIV 2)) - (ADiameter DIV 2)) ), 255);
          vCanvas.Pen.Color := HLStoColor(vHue, vNewLight, vSat);
          vCanvas.Line(vPointArray[0].X, vPointArray[0].Y, vPointArray[1].X + vLoopFill , vPointArray[1].Y );
        end;
      end;
    end
    ELSE begin
      vPointArray[1] := LineEndPoint(vPointArray[0], (AAngle+120)*16, ADiameter);
      vPointArray[2] := LineEndPoint(vPointArray[0], (AAngle+60)*16, ADiameter);

      //vCanvas.Polygon(vPointArray);  // other overloaded funcs available
      //IF (not AFilled) THEN begin
      IF (not AFilled) or AFilled THEN begin
        vCanvas.Line(vPointArray[0].X, vPointArray[0].Y, vPointArray[1].X, vPointArray[1].Y);
        vCanvas.Line(vPointArray[0].X, vPointArray[0].Y, vPointArray[2].X, vPointArray[2].Y);
        vCanvas.Line(vPointArray[1].X, vPointArray[1].Y, vPointArray[2].X, vPointArray[2].Y);
      end;
      IF AFilled then begin
        // color
        ColorToHLS(vColor, vHue, vLight, vSat);
        // endpoints
        vDistance := trunc( Distance(vPointArray[1], vPointArray[2]) + 1);
        for vLoopFill := 1 to vDistance-1 do begin
          vFillPt := LineEndPoint(vPointArray[1], AAngle*16, vLoopFill);
          if ( (length(vFillArray) > 0) and (vFillPt <> vFillArray[length(vFillArray)-1]) ) OR (length(vFillArray)=0) then begin
            setlength(vFillArray, length(vFillArray) +1);
            vFillArray[length(vFillArray)-1] := vFillPt;
          end;
        end;
        // fill
        for vLoopFill:= 0 to length(vFillArray)-1 do begin
          vNewLight:= min( vLight + round((255-vLight) *2 / ADiameter * abs(abs(vLoopFill-(length(vFillArray) DIV 2)) - (length(vFillArray) DIV 2)) ), 255);
          vCanvas.Pen.Color := HLStoColor(vHue, vNewLight, vSat);
          vCanvas.Line(vPointArray[0].X, vPointArray[0].Y, vFillArray[vLoopFill].X , vFillArray[vLoopFill].Y );
        end;
      end;
    end;
  end;

  // make transparent
  IF FMarkerImage <> nil then FMarkerImage.Free;
  FMarkerImage := FMarkerBmp.CreateIntfImage;
  for vRow:=0 to FMarkerImage.Height-1 do
    for vCol:=0 to FMarkerImage.Width-1 do begin
      vFPColor := FMarkerImage.Colors[vCol, vRow];
      if FPColorToTColor(vFPColor) = vBackgroundColor then
        vFPColor.alpha := alphaTransparent
      else vFPColor.alpha := AFPColor.alpha; // $7FFF;
      FMarkerImage.Colors[vCol, vRow] := vFPColor;
    end;
  FMarkerBmp.LoadFromIntfImage(FMarkerImage);

  // positions
  vDestRect.Left := MinIntValue([ vPointArray[0].X, vPointArray[1].X, vPointArray[2].X ]);
  vDestRect.Right := MaxIntValue([ vPointArray[0].X, vPointArray[1].X, vPointArray[2].X ]) +1; // +1? see line style
  vDestRect.Top := MinIntValue([ vPointArray[0].Y, vPointArray[1].Y, vPointArray[2].Y ]) ;
  vDestRect.Bottom := MaxIntValue([ vPointArray[0].Y, vPointArray[1].Y, vPointArray[2].Y ]) +1;
  Result := vDestRect;
  OriginShift.X := vPointArray[0].X - vDestRect.Left;
  OriginShift.Y := vPointArray[0].Y - vDestRect.Top;
  //FMarkerBmp.SaveToFile('test.bmp');

  setlength(vFillArray, 0);
  setlength(vPointArray, 0);
end;

//function TMarkerDrawer_Bitmap.DrawLine(AStartPt, AEndPt: TPoint;
//  ADiameter: Integer; AAngle: Extended; AFPColor: TFPColor;
//  AFilled: Boolean; out OriginShift: TPoint): TRect;
function TMarkerDrawer_Bitmap.DrawLine(AStartPt, AEndPt: TPoint;
  ADiameter: Integer; AFPColor: TFPColor;
  AFilled: Boolean; out OriginShift: TPoint): TRect;
var
  vWidth, vHeight: Integer;
  vNormalAngle: Extended;
  vNextStartPoint: TPoint;
  vDrawStartPt: TPoint;
  vPointArrayStart: array of TPoint;
  vEndPointShift: TPoint;
  //vFillArray: array of TPoint;
  //vFillPt: TPoint;
  //Pt: TPoint;
  vCanvas: TCanvas;
  vCol, vRow: Integer;
  vFPColor: TFPColor;
  vColor: TColor;
  vLoopFill: Integer;
  vHue, vLight, vSat: byte;
  vNewLight: byte;
  vBackgroundColor: TColor;
  vDestRect: TRect;
  //vDistance: Integer;
begin
  vColor:=FPColorToTColor(AFPColor);

  vDrawStartPt := Point(ADiameter DIV 2, ADiameter DIV 2);

  vEndPointShift.X := AEndPt.X - AStartPt.X;
  vEndPointShift.Y := AEndPt.Y - AStartPt.Y;
  vWidth := abs(vEndPointShift.X) + ADiameter; // 1;
  vHeight := abs(vEndPointShift.Y) + ADiameter; // 1;

  if (vEndPointShift.X < 0) then vDrawStartPt.X := vDrawStartPt.X - vEndPointShift.X;
  if (vEndPointShift.Y < 0) then vDrawStartPt.Y := vDrawStartPt.Y - vEndPointShift.Y;

  vBackgroundColor := clCream;
  if vColor = vBackgroundColor then vBackgroundColor := clWhite;

  IF (not odd(ADiameter)) then ADiameter:=ADiameter+1;

  IF FMarkerBmp <> nil THEN FMarkerBmp.Free;
  FMarkerBmp := TBitmap.Create;
  FMarkerBmp.PixelFormat:=pf32bit;
  FMarkerBmp.SetSize(vWidth, vHeight); //FMarkerBmp.SetSize(ADiameter, ADiameter);
  vCanvas := FMarkerBmp.Canvas;
  vCanvas.Brush.Color:=vBackgroundColor;
  vCanvas.FillRect(0,0,vWidth-1, vHeight-1);  //vCanvas.FillRect(0,0,ADiameter-1, ADiameter-1);

  // check angle and additional points
  IF ADiameter = 1 THEN BEGIN
    setlength(vPointArrayStart, 1);
    //setlength(vPointArrayEnd, 1);
    vPointArrayStart[0] := AStartPt;
    vPointArrayStart[0].X := vPointArrayStart[0].X + (ADiameter DIV 2);
    vPointArrayStart[0].Y := vPointArrayStart[0].Y + (ADiameter DIV 2);
    //vPointArrayEnd[0] := AEndPt;
  END ELSE
  IF ADiameter > 1 THEN BEGIN
    if vWidth = ADiameter then vNormalAngle:=0 else
      vNormalAngle := (arctan( (vHeight-ADiameter) / (vWidth-ADiameter) ) + Pi()/2) * 180 / Pi();

    for vLoopFill := -(ADiameter DIV 2) to (ADiameter DIV 2) do begin
      //vNextStartPoint := LineEndPoint(Point(ADiameter DIV 2, ADiameter DIV 2), vNormalAngle*16, vLoopFill);
      vNextStartPoint := LineEndPoint(vDrawStartPt, vNormalAngle*16, vLoopFill);
      if length(vPointArrayStart) = 0 then begin
        SetLength(vPointArrayStart,1);
        vPointArrayStart[0] := vNextStartPoint;
      end else
      if (vNextStartPoint <> vPointArrayStart[length(vPointArrayStart)-1]) then begin
        SetLength(vPointArrayStart,length(vPointArrayStart)+1);
        vPointArrayStart[length(vPointArrayStart)-1] := vNextStartPoint;
      end;
    end;
  END;

  ColorToHLS(vColor, vHue, vLight, vSat);

  // draw the lines
  for vLoopFill:= 0 to length(vPointArrayStart)-1 do begin
    vNewLight:= min( vLight + round((255-vLight) *2 / ADiameter * abs(abs(vLoopFill-(length(vPointArrayStart) DIV 2)) - (length(vPointArrayStart) DIV 2)) ), 255);
    vCanvas.Pen.Color := HLStoColor(vHue, vNewLight, vSat);
    vCanvas.Line(vPointArrayStart[vLoopFill].X, vPointArrayStart[vLoopFill].Y,
                  vPointArrayStart[vLoopFill].X + vEndPointShift.X, vPointArrayStart[vLoopFill].Y + vEndPointShift.Y);
  end;


    // make transparent
  IF FMarkerImage <> nil then FMarkerImage.Free;
  FMarkerImage := FMarkerBmp.CreateIntfImage;
  for vRow:=0 to FMarkerImage.Height-1 do
    for vCol:=0 to FMarkerImage.Width-1 do begin
      vFPColor := FMarkerImage.Colors[vCol, vRow];
      if FPColorToTColor(vFPColor) = vBackgroundColor then
        vFPColor.alpha := alphaTransparent
      else vFPColor.alpha := AFPColor.alpha; // $7FFF;
      FMarkerImage.Colors[vCol, vRow] := vFPColor;
    end;
  FMarkerBmp.LoadFromIntfImage(FMarkerImage);

  // positions
  vDestRect.Left := MinIntValue([ vPointArrayStart[0].X, vPointArrayStart[Length(vPointArrayStart)-1].X,
               vPointArrayStart[0].X + vEndPointShift.X, vPointArrayStart[Length(vPointArrayStart)-1].X + vEndPointShift.X]);
  vDestRect.Right := MaxIntValue([ vPointArrayStart[0].X, vPointArrayStart[Length(vPointArrayStart)-1].X,
               vPointArrayStart[0].X + vEndPointShift.X, vPointArrayStart[Length(vPointArrayStart)-1].X + vEndPointShift.X]) +1;
  vDestRect.Top := MinIntValue([ vPointArrayStart[0].Y, vPointArrayStart[Length(vPointArrayStart)-1].Y,
               vPointArrayStart[0].Y + vEndPointShift.Y, vPointArrayStart[Length(vPointArrayStart)-1].Y + vEndPointShift.Y]) ;
  vDestRect.Bottom := MaxIntValue([ vPointArrayStart[0].Y, vPointArrayStart[Length(vPointArrayStart)-1].Y,
               vPointArrayStart[0].Y + vEndPointShift.Y, vPointArrayStart[Length(vPointArrayStart)-1].Y + vEndPointShift.Y]) +1;
  // attention: +1 because of CopyRect width calculation: Right - left = width
  // this is wrong because (right-left) = width +1
  // windows widgetset has this bug (feature ?) so Delphi VCL has it and Lazarus also
  // --> solution: we add +1 for right and top

  Result := vDestRect;
  OriginShift.X := vDrawStartPt.X - vDestRect.Left - AStartPt.X ;
  OriginShift.Y := vDrawStartPt.Y - vDestRect.Top - AStartPt.Y ;
end;


procedure TMarkerDrawer_Bitmap.DrawReadout(AMarker: TMarker; AFPColor: TFPColor; ABackGndFPColor: TFPColor);
var
  vColor: TColor;
  vFPColor: TFPColor;
  vRow, vCol: Integer;
  vMaxWidth, vWidth: Integer;
  vHeight: Integer;
  vLineY: Integer;
  vText: string;
  vXYValue: TXYValue;
  vZValue: Extended;
  vError: Integer;
  vBackgroundColor: TColor;
  {$IFDEF GTK2}
    vTempCol: Word;
    vTempTextCol: TFPColor;
  {$ENDIF}
const
  cBORDER = 2;
  cLINESPREAD = 1; //1.1;
  cWIDTH = 200;
  cHEIGHT = 140;
begin
  vColor:=FPColorToTColor(AFPColor);
  {$IFDEF GTK2}
    vTempTextCol := AFPColor;
    vTempCol := vTempTextCol.blue;
    vTempTextCol.blue := vTempTextCol.red;
    vTempTextCol.red  := vTempCol;
    vColor:=FPColorToTColor(vTempTextCol);
  {$ENDIF}

  if ABackGndFPColor = AFPColor then begin
    if ABackGndFPColor.red < $FFFF then ABackGndFPColor.red := (ABackGndFPColor.red + 1) else
         ABackGndFPColor.red := (ABackGndFPColor.red - 1);
  end;
  vBackgroundColor := FPColorToTColor(ABackGndFPColor);

  IF FReadoutBmp <> nil THEN FReadoutBmp.Free;

  FReadoutBmp := TBitmap.Create;
  FReadoutBmp.PixelFormat:=pf32bit;
  FReadoutBmp.SetSize(cWIDTH, cHEIGHT);    // dynamic ? known before ?
  FReadoutBmp.Canvas.Brush.Color := vBackgroundColor;
  FReadoutBmp.Canvas.FillRect(0,0,cWIDTH-1, cHEIGHT-1);

  //FReadoutBmp.Canvas.AntialiasingMode:=amOff;
  FReadoutBmp.Canvas.Font.Quality:=fqNonAntialiased;
  FReadoutBmp.Canvas.Font.Color := vColor; // vColor;
  //FReadoutBmp.Canvas.Font.Name:='Sans Serif';
  //FReadoutBmp.Canvas.Font.Name:='Monospace';
  FReadoutBmp.Canvas.Font.Name:='Arial';
  FReadoutBmp.Canvas.Font.Size:=8;
  //FReadoutBmp.Canvas.Font.Style:=[fsBold];
  FReadoutBmp.Canvas.Brush.Style:=bsClear;

  // line 1  ..........................
  vText := '';
  vLineY := cBORDER;
  vMaxWidth:=0;
  if AMarker.VisualParams.ShowIdent then vText := vText + 'MKR';
  if AMarker.VisualParams.ShowIndex then vText := vText + '<' + IntToStr(AMarker.OwnerContainer.MarkerIndex[AMarker]) + '>';
  if AMarker.VisualParams.ShowMode then begin
    vText := vText + cMARKER_MODE_NAMES[AMarker.MarkerMode]
  end;
  if vText <> '' then begin
    FReadoutBmp.Canvas.TextOut(cBORDER, vLineY , vText);
    vWidth:=FReadoutBmp.Canvas.TextWidth(vText);
    vHeight:=FReadoutBmp.Canvas.TextHeight(vText);
    vMaxWidth:=max(vMaxWidth, vWidth);
    vLineY := vLineY + round(vHeight * cLINESPREAD);
  end;

  // line 2,X  ..........................
  vText:='';
  // TODO: error handling
  if AMarker.VisualParams.XReadout OR AMarker.VisualParams.YReadout OR AMarker.VisualParams.ZReadout then
       vError := AMarker.GetValue(vXYValue, vZValue);
    if AMarker.VisualParams.XReadout then begin
      vText := vText + 'X: ';
    if mroValues in AMarker.VisualParams.ReadoutItems then begin
      vText := vText + FormatNumber(vXYValue.X, TPlotAxis(AMarker.OwnerContainer.OwnerSeries.OwnerPlot.Axis[TXYPlotSeries(AMarker.OwnerContainer.OwnerSeries).XAxis]).NumberFormat);
    end;
    if mroUnits in AMarker.VisualParams.ReadoutItems then begin
      vText := vText + AMarker.OwnerContainer.OwnerSeries.UnitString[TXYPlotSeries(AMarker.OwnerContainer.OwnerSeries).XAxis];
    end;
    if vText <> '' then begin
      FReadoutBmp.Canvas.TextOut(cBORDER, vLineY , vText);
      vWidth:=FReadoutBmp.Canvas.TextWidth(vText);
      vHeight:=FReadoutBmp.Canvas.TextHeight(vText);
      vMaxWidth:=max(vMaxWidth, vWidth);
      vLineY := vLineY + round(vHeight * cLINESPREAD);
    end;
  end;

  // line 3,Y  ..........................
  vText:='';
  if AMarker.VisualParams.YReadout then begin
    vText := vText + 'Y: ';
    if mroValues in AMarker.VisualParams.ReadoutItems then begin
      vText := vText + FormatNumber(vXYValue.Y, TPlotAxis(AMarker.OwnerContainer.OwnerSeries.OwnerPlot.Axis[TXYPlotSeries(AMarker.OwnerContainer.OwnerSeries).YAxis]).NumberFormat);
    end;
    if mroUnits in AMarker.VisualParams.ReadoutItems then begin
      vText := vText + AMarker.OwnerContainer.OwnerSeries.UnitString[TXYPlotSeries(AMarker.OwnerContainer.OwnerSeries).YAxis];
    end;
    if vText <> '' then begin
      FReadoutBmp.Canvas.TextOut(cBORDER, vLineY , vText);
      vWidth:=FReadoutBmp.Canvas.TextWidth(vText);
      vHeight:=FReadoutBmp.Canvas.TextHeight(vText);
      vMaxWidth:=max(vMaxWidth, vWidth);
      vLineY := vLineY + round(vHeight * cLINESPREAD);
    end;
  end;

  // line 4,Z  ..........................
  vText:='';
  if AMarker.VisualParams.ZReadout and (AMarker.OwnerContainer.OwnerSeries is TXYZPlotSeries) then begin
    vText := vText + 'Z: ';
    if mroValues in AMarker.VisualParams.ReadoutItems then begin
      vText := vText + FormatNumber(vZValue, TPlotAxis(AMarker.OwnerContainer.OwnerSeries.OwnerPlot.Axis[TXYZPlotSeries(AMarker.OwnerContainer.OwnerSeries).ZAxis]).NumberFormat);
    end;
    if mroUnits in AMarker.VisualParams.ReadoutItems then begin
      vText := vText + AMarker.OwnerContainer.OwnerSeries.UnitString[TXYZPlotSeries(AMarker.OwnerContainer.OwnerSeries).ZAxis];
    end;
    if vText <> '' then begin
      FReadoutBmp.Canvas.TextOut(cBORDER, vLineY , vText);
      vWidth:=FReadoutBmp.Canvas.TextWidth(vText);
      vHeight:=FReadoutBmp.Canvas.TextHeight(vText);
      vMaxWidth:=max(vMaxWidth, vWidth);
      vLineY := vLineY + round(vHeight * cLINESPREAD);
    end;
  end;

  // set BMP size
  vHeight := vLineY + cBORDER;
  vWidth := vMaxWidth + 2 * cBORDER;
  FReadoutBmp.SetSize(vWidth, vHeight);

  // convert to transparent      // TODO: how can we use this with aliased fonts ?
  IF FReadOutImage <> nil then FReadOutImage.Free;
  FReadOutImage := FReadoutBmp.CreateIntfImage;
  for vRow := 0 to FReadOutImage.Height-1 do
    for vCol := 0 to FReadOutImage.Width-1 do begin
      vFPColor := FReadOutImage.Colors[vCol, vRow];
      if FPColorToTColor(vFPColor) = vBackgroundColor then begin
        vFPColor.alpha := ABackGndFPColor.alpha;
        FReadOutImage.Colors[vCol, vRow] := vFPColor;
      end;
    end;
  FReadoutBmp.LoadFromIntfImage(FReadOutImage);
end;

constructor TMarkerDrawer_Bitmap.Create(AOwnerContainer: TMarkerContainer);
begin
  Inherited Create(AOwnerContainer);
end;

destructor TMarkerDrawer_Bitmap.Destroy;
begin
  IF FMarkerBmp <> nil THEN FMarkerBmp.Free;
  IF FReadoutBmp <> nil THEN FReadoutBmp.Free;
  IF FMarkerImage <> nil THEN FreeAndNil(FMarkerImage);
  IF FReadOutImage <> nil THEN FreeAndNil(FReadOutImage);
  inherited Destroy;
end;

procedure TMarkerDrawer_Bitmap.DrawMarker(AMarker: TMarker; ABitMap: TBitmap; IsGlobalImage: Boolean);
var
  vTargetPt, vLineEndPt: TPoint;
  vXYValue: TXYValue;
  vXYLineEndValue: TXYValue;
  vZValue: Extended;
  vZLineEndValue: Extended;
  vMarkerRect: TRect;
  vDestRect: TRect;
  vOriginShift: TPoint;
  vVisualParams: TMarkerVisualParams;
  vFPColor, vBackGndFPColor: TFPColor;
  vError: Integer;
  vAngle: Extended;
begin
  //writeln('drawmarker');
  vVisualParams := AMarker.VisualParams;
  // marker
  vError := AMarker.GetValue(vXYValue, vZValue);
  if vError <> c_SUCCESS then exit;
  vAngle := vVisualParams.StyleParams.DrawAngle;

  vError := XYToScreen(AMarker.OwnerContainer.OwnerSeries.OwnerPlot.Axis[TPlotSeries(AMarker.OwnerContainer.OwnerSeries).XAxis], AMarker.OwnerContainer.OwnerSeries.OwnerPlot.Axis[TPlotSeries(AMarker.OwnerContainer.OwnerSeries).YAxis], vXYValue.X, vXYValue.Y, vTargetPt);
  if vError <> c_SUCCESS then exit;

  if (not IsGlobalImage) then
    PlotImageCoordsToFrameRectCoords(AMarker.OwnerContainer.OwnerSeries.OwnerPlot.Axis[AMarker.OwnerContainer.OwnerSeries.OwnerAxis].OwnerPlotRect, vTargetPt);

  IF msTriangle in vVisualParams.StyleParams.MarkShapes THEN begin              // TODO: flip for MinPeak marker
    vFPColor := TColorToFPColor(vVisualParams.StyleParams.Color);
    vFPColor.alpha := vVisualParams.StyleParams.Alpha;
    vMarkerRect := DrawTriangle(vVisualParams.StyleParams.Diameter, vAngle, vFPColor, true, AMarker.GetFlip, vOriginShift);


    with vDestRect do begin
      Left:= vTargetPt.X - vOriginShift.X;
      Right:= vTargetPt.X + (vMarkerRect.Right - vMarkerRect.Left) - vOriginShift.X;
      Top:= vTargetPt.Y - vOriginShift.Y;
      Bottom:= vTargetPt.Y + (vMarkerRect.Bottom - vMarkerRect.Top) - vOriginShift.Y;
    end;
    ABitMap.Canvas.CopyRect(vDestRect, FMarkerBmp.Canvas, vMarkerRect);
  end;

  // TODO: circle

  IF msLine in vVisualParams.StyleParams.MarkShapes THEN begin              // TODO: flip for MinPeak marker
    vFPColor := TColorToFPColor(vVisualParams.StyleParams.Color);  // outside
    vFPColor.alpha := vVisualParams.StyleParams.Alpha;             // outside
    vError := AMarker.GetLineEndValue(vXYLineEndValue, vZLineEndValue);

    XYToScreen(AMarker.OwnerContainer.OwnerSeries.OwnerPlot.Axis[TPlotSeries(AMarker.OwnerContainer.OwnerSeries).XAxis], AMarker.OwnerContainer.OwnerSeries.OwnerPlot.Axis[TPlotSeries(AMarker.OwnerContainer.OwnerSeries).YAxis], vXYLineEndValue.X, vXYLineEndValue.Y, vLineEndPt);
    if (not IsGlobalImage) then
      PlotImageCoordsToFrameRectCoords(AMarker.OwnerContainer.OwnerSeries.OwnerPlot.Axis[AMarker.OwnerContainer.OwnerSeries.OwnerAxis].OwnerPlotRect, vLineEndPt);
    vMarkerRect := DrawLine(vTargetPt, vLineEndPt,  vVisualParams.StyleParams.LineWidth, vFPColor, true, vOriginShift);


    with vDestRect do begin
      Left:= - vOriginShift.X;
      Right:= Left + (vMarkerRect.Right - vMarkerRect.Left);
      Top:=  - vOriginShift.Y;
      Bottom:= Top + (vMarkerRect.Bottom - vMarkerRect.Top);
    end;
    ABitMap.Canvas.CopyRect(vDestRect, FMarkerBmp.Canvas, vMarkerRect);
  end;

  // readout
  vBackGndFPColor := TColorToFPColor(vVisualParams.StyleParams.BackGndColor);  // outside
  vBackGndFPColor.alpha := vVisualParams.StyleParams.BackGndAlpha;             // outside
  DrawReadout(AMarker, vFPColor, vBackGndFPColor);                               // TODO: readout color and alpha in GUI

  // readout
  vDestRect.Left := vTargetPt.X + 4;
  vDestRect.Right:= vDestRect.Left + FReadoutBmp.Width;
  vDestRect.Top:= vTargetPt.Y;
  vDestRect.Bottom:= vDestRect.Top + FReadoutBmp.Height;

  ABitMap.Canvas.CopyRect(vDestRect, FReadoutBmp.Canvas, bounds(0,0,FReadoutBmp.Width,FReadoutBmp.Height));
end;

end.

