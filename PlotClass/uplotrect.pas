unit uPlotRect;
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

// TODO: transparancy property for legends and coloscale  ?
// Border properties rel and abs ?
{
New RECT handling 09.08.14
FrameRect is the whole PlotRect (used for Borders and Placing into PlotImage)
  holds background
ClientRect is a mathematical construct: Framerect shrinked by Axis text etc. (used for axis length calculations; i.e. is maximum DataRect);
  only for calculations
DataRect is the max extent of the axes placed according to Clientrect (used for Plot Clipping and Fast Series LazIntfImage)
  is size of IntFImage

// TODO: add a property to CloneAxis (Shiftmode: smLength, smValue) to lock a clone axis to a specific axis value of the shiftaxis
}

{$mode objfpc}{$H+}

//{$DEFINE DEBUG_SCALE_WRITE}
//{$DEFINE DEBUG_2_WRITE}

// //{$DEFINE GTK2} not needed ?

interface

uses
  Classes, Graphics, Types, uPlotClass, uPlotStyles, uPlotSeries, uPlotAxis, sysutils, math,
  LCLType, IntfGraphics, FPimage, GraphType, uPlotOverrides, dateutils, uPlotUtils,
  matrix; // lazcanvas;GraphMath

type
  TBorder = TRect;

  TRectPlacement = (plRight, plBottom, plCanvas);

  TLegendRect = class;
  TColorScaleRect = class;

  TPointArray = array of TPoint;

  { TPlotRect }

  TPlotRect = class(uPlotClass.TBasePlotRect)
  private
    FAxisAutoPlaceFill: Boolean;
    FScreenInvalid: Boolean;
    FDataImage: TLazIntfImage_debug; // series plot data here on this empty (transparent) image
    // TODO: can we write a Class Helper for our ApplyAlpha function instead of this crude inheritance ?
    //FDataImage: TLazIntfImage; // series plot data here on this empty (transparent) image
    FBackImage: TLazIntfImage;
    //FBackIMageCanvas: TLazCanvas;  textout not available with Lazarus 1.2.4, so we wait and see!  - was just for testing
    FBackBMP: TBitmap; // Background Bitmap to avoid repeated inneraxis draw etc.
    FDataImageBuffer: TBitmap;
    FFillPolyPoints: array of TPoint; // used to fill the inner part of the plotrect enclosed by the axes
    FColorScalePlacement: TRectPlacement;
    FLegendPlacement: TRectPlacement;
    FLegendRect: TLegendRect;
    FColorScaleRect: TColorScaleRect;
    FBorderAbs: TBorder;
    FBorderRel: TBorder;
    FShowColorScale: Boolean;
    FShowLegend: Boolean;
    FShowFrame: Boolean;
    FShowTitle: Boolean;
    FStyle: TPlotStyle;
    FTitleStr: String;
    FTitleTagStr: String;
    FAxisConstraints: array[0..2] of Double;
    function GetAxisAutoPlaceFillConstraints(AIndex: Integer): Double;
    procedure SetAxisAutoPlaceFill(AValue: Boolean);
    procedure SetAxisAutoPlaceFillConstraints(AIndex: Integer; AValue: Double);
    function _FocusColor(BackgroundColor: TColor): TColor;
    procedure SetBorderAbs(const AValue: TBorder);
    procedure SetBorderRel(const AValue: TBorder);
    procedure SetColorScalePlacement(const AValue: TRectPlacement);
    procedure SetColorScaleRect(const AValue: TColorScaleRect);
    procedure SetLegendPlacement(const AValue: TRectPlacement);
    procedure SetLegendRect(const AValue: TLegendRect);
    procedure SetStyle(const AValue: TPlotStyle);

    procedure _ShrinkColorScale(var ARect: TRect);
    procedure _ShrinkLegend(var ARect: TRect);
    procedure _ShrinkTitle(var ARect: TRect);
    procedure PreCalcClientRect;
    function _BuildConstraints3(a,b,c, anglea, angleb, anglec: Double; out s,t,u: Double): Integer;
    function _AxisAutoPlace3(a,b,c, anglea, angleb, anglec: Double; out La, Lb, Lc: Double; out NewOrigin: TPoint): Integer; // delivers 0 if solved
    function _AxisAutoPlace2(a,b,anglea, angleb: Double; out La, Lb: Double; out NewOrigin: TPoint): Integer; // delivers 0 if solved

    function DrawTitle(AVisible: Boolean): TRect;
    function _GetHasFastSeries: Boolean;
    procedure _DrawZoomRectPlotImage;
    procedure _DrawZoomRectDataImage;
    procedure _FillDataArea;
  protected
    function GetClientRect: TRect; override;
    function GetFrameRect: TRect; override;
    function GetDataRect: TRect;
    procedure Redraw; override;
  public
    constructor Create(APlot: TPlot); override;
    destructor Destroy; override;

    procedure StoreBackGround;

    procedure UpdateSeriesData(ASeries: TPlotSeries; AMarkerUpdateOnly: Boolean; AForceRefresh: Boolean = FALSE); // used for fast series
    procedure UpdateMarkers;
    procedure UpdateZoomRect;
    procedure Pan(dX, dY: Integer);
    procedure Zoom(X, Y: Integer; AFactorX: Extended; AFactorY: Extended);
    procedure Zoom(ARect: TRect);
    property DataImage: TLazIntfImage_debug read FDataImage;
    //property DataImage: TLazIntfImage read FDataImage;

    property BorderAbs: TBorder read FBorderAbs write SetBorderAbs; // pixels
    property BorderRel: TBorder read FBorderRel write SetBorderRel; // percent
    property ShowFrame: Boolean read FShowFrame write FShowFrame;
    property ShowTitle: Boolean read FShowTitle write FShowTitle;   // position is automagic
    property Title: String read FTitleStr write FTitleStr;
    property TitleTag: String read FTitleTagStr write FTitleTagStr;
    property Style: TPlotStyle read FStyle write SetStyle;
    property LegendRect: TLegendRect read FLegendRect write SetLegendRect;
    property ColorScaleRect: TColorScaleRect read FColorScaleRect write SetColorScaleRect;
    property ShowLegend: Boolean read FShowLegend write FShowLegend;
    property ShowColorScale: Boolean read FShowColorScale write FShowColorScale;
    //
    property LegendPlacement: TRectPlacement read FLegendPlacement write SetLegendPlacement;
    property ColorScalePlacement: TRectPlacement read FColorScalePlacement write SetColorScalePlacement;
    //
    property AxisAutoPlaceFill: Boolean read FAxisAutoPlaceFill write SetAxisAutoPlaceFill;
    property AxisAutoPlaceFillConstraints[AIndex: Integer]: Double read GetAxisAutoPlaceFillConstraints write SetAxisAutoPlaceFillConstraints;
  end;


  { TLegendRect }

  TLegendRect = class
  private
    FAutoShrink: Boolean;
    FFillBackground: Boolean;
    FDrawBMP: TBitmap;
    //FAnchorAbs: TPoint;        // 10-2014 all manual placement options deleted
    //FAnchorRel: TPoint;        // manual placement needs a rework if needed
    //FClientRect: TRect;
    //FAutoRect: Boolean;
    FOwnerPlotRect: TPlotRect;
    FRectSize: TSize;
    FShowFrame: Boolean;
    FStyle: TPlotStyle;

    procedure SetAutoShrink(const AValue: Boolean);
    procedure SetFillBackground(const AValue: Boolean);
    procedure SetOwnerPlotRect(const AValue: TPlotRect);
    procedure SetRectSize(const AValue: TSize);
    function DrawLegend_VER(AVisible, ATransparent: Boolean): TRect;
    function DrawLegend_HOR(AVisible, ATransparent: Boolean): TRect;

    procedure SetStyle(const AStyle: TPlotStyle);
    function _GetClientRect(APlacement: TRectPlacement): TRect;
  protected
    function GetClientRect: TRect; virtual;

  public
    constructor Create(AOwnerPlotRect: TPlotRect);virtual;
    destructor Destroy; override;

    function Redraw(AVisible, ATransparent: Boolean): TRect;virtual;
    property ShowFrame: Boolean read FShowFrame write FShowFrame;

    property RectSize: TSize read FRectSize write SetRectSize;                  // cx, cy in % of plotrect   // deprecated

    property Style: TPlotStyle read FStyle write SetStyle;
    property ClientRect: TRect read GetClientRect; // write SetClientRect;
    property OwnerPlotRect: TPlotRect read FOwnerPlotRect write SetOwnerPlotRect;
    property FillBackground: Boolean read FFillBackground write SetFillBackground;
    property AutoShrink: Boolean read FAutoShrink write SetAutoShrink;          // deprecated
  end;

  { TColorScaleRect }

  TColorScaleRect = class(TLegendRect)
  private
    FColorScaleOrientation: TAxisOrientation;
    FColorScaleWidth: Integer;
    FScaleAxisIndex: Integer;
    procedure SetColorScaleOrientation(const AValue: TAxisOrientation);
    procedure SetColorScaleWidth(const AValue: Integer);
    procedure SetScaleAxisIndex(const AValue: Integer);
  protected
    function GetClientRect: TRect; override;
  public
    constructor Create(AOwnerPlotRect: TPlotRect);override;

    function Redraw(AVisible, ATransparent: Boolean): TRect;override;
    property ScaleAxisIndex: Integer read FScaleAxisIndex write SetScaleAxisIndex;
    property ColorScaleWidth: Integer read FColorScaleWidth write SetColorScaleWidth;
    property ColorScaleOrientation: TAxisOrientation read FColorScaleOrientation write SetColorScaleOrientation;     // use orientation from LegendRect
  end;


const
  cMAXLEGENDCHARS = 18; // if textline is overfilled, start truncating legend text to cMAXLEGENDLETTERS per series
                          // if still insufficient, make new line (in horizontal mode)
  cMAXRECTSIZEPERCENT = 33; // maximum extent of legend / colorcale rect

implementation

{ TPlotRect }

function TPlotRect.GetClientRect: TRect;
begin
// this code is precalculated for speed and stored in FClientRect
  Result := FClientRect;
end;

procedure TPlotRect.Redraw;
var
  vImageDescription: TRawImageDescription;
  vFPColor: TFPColor;
begin
  inherited Redraw;
  // TEST: precalc values
  PreCalcClientRect;
  // components deliver usedrect
  // clientrect is framerect minus used space outside clientrect

  //-----------------------------------------
  // Frame draw and Fill Background
  IF ShowFrame THEN BEGIN
    PlotImage.Canvas.Pen.Assign(Style.Pen);
    PlotImage.Canvas.MoveTo(ClientRect.Left, ClientRect.Top);
    PlotImage.Canvas.LineTo(ClientRect.Right, ClientRect.Top);
    PlotImage.Canvas.LineTo(ClientRect.Right, ClientRect.Bottom);
    PlotImage.Canvas.LineTo(ClientRect.Left, ClientRect.Bottom);
    PlotImage.Canvas.LineTo(ClientRect.Left, ClientRect.Top);
  END;
  // 09.08. new rect handling, we fill only the frame
  PlotImage.Canvas.Brush.Color := Style.Brush.Color;

  // Extent of the plot (formerly Plotrect) is filled here
  _FillDataArea;

  // Draw Title, Colorscale and Legend
  DrawTitle(True);
  IF ShowColorScale THEN ColorScaleRect.Redraw(True, True);
  IF ShowLegend THEN LegendRect.Redraw(True, True);


  IF FDataImage <> nil then FreeAndNil(FDataImage);
  FDataImage := TLazIntfImage_debug.Create(max(DataRect.Right - DataRect.Left,1), max(DataRect.Bottom-DataRect.Top,1)); // LazIntfImage on GTK2 throws exception on diomension = 0
  {$IFNDEF GTK2}
    vImageDescription := GetDescriptionFromDevice(0, max(DataRect.Right - DataRect.Left,1), max(DataRect.Bottom-DataRect.Top,1));
  {$ENDIF}
  {$IFNDEF GTK2}
    // transparency does not work when GetDescritptionFromDevice is used, so use hardcoded working description
    vImageDescription.Init_BPP32_B8G8R8A8_BIO_TTB(max(DataRect.Right - DataRect.Left,1), max(DataRect.Bottom-DataRect.Top,1));
  {$ENDIF}
  FDataImage.DataDescription := vImageDescription;
  vFPColor.red:=$0; vFPColor.green:=$0; vFPColor.blue:=$0; vFPColor.alpha:=$0;
  FDataImage.FillPixels(vFPColor);
  //FDataImage.FillPixels(FPColor($7FFF,$1000, $1000, $1100));

  // create Background Buffer Bitmap
  // size = whole plotrect (DataImage shall be correctly placed into DrawBuffer)
  IF FDataImageBuffer <> nil THEN FreeAndNil(FDataImageBuffer);    // needed ? or can we simply resize ?
  FDataImageBuffer := TBitmap.Create;
  FDataImageBuffer.PixelFormat:=pf32bit;
  FDataImageBuffer.SetSize(max(DataRect.Right - DataRect.Left+1,1), max(DataRect.Bottom-DataRect.Top+1,1) );
  //FDataImageBuffer.Canvas.Line(0,0,1,1);  // TODO: this might be needed on Windoze to make it recognise a transparent Bitmap ?
  // no effect on GTK2
  //FDataImageBuffer.Canvas.Brush.Color:=clYellow;
  //FDataImageBuffer.Canvas.FillRect(rect(0,0,FDataImageBuffer.Canvas.Width-1, FDataImageBuffer.Height-1));
end;

procedure TPlotRect.UpdateSeriesData(ASeries: TPlotSeries; AMarkerUpdateOnly: Boolean; AForceRefresh: Boolean = FALSE);
var
  vLoop: Integer;
  vSeriesList: TFPList;
  vFPColor: TFPColor;
  vBackBmp: TBitmap;
  vNeedsClear, vNeedsRefresh: Boolean;
  //vPNG: TPortableNetworkGraphic;    // tested... works also with a png instead of a temp bitmap
begin
  // let series paint to FDataImage
  // overlay BackBMP and DataImage
  // copyrect to PlotCanvas
  // TODO: separate marker and series updating !
  vNeedsClear:=false; vNeedsRefresh:=false;
  // windows.................

  IF OwnerPlot.TimedRefresh AND (not AForceRefresh) AND (not AMarkerUpdateOnly) THEN begin
    FScreenInvalid:=TRUE;
    exit;
  end;

  vBackBmp := TBitmap.Create;
  //vBackBmp.PixelFormat:=pf32bit;
  vBackBmp.LoadFromIntfImage(FBackImage);

  vSeriesList := Self.SeriesContainedIdx;
  IF (not AMarkerUpdateOnly) THEN BEGIN
    // check if we need a update, result should be cumulative when multiple series are asked.
    for vLoop := 0 to vSeriesList.Count-1 do begin // seriescontained delivers integerIndex AS Pointer and backwards....
      IF OwnerPlot.Series[PInteger(vSeriesList.Items[vLoop])^].IsFastSeries then begin
        TPlotSeries(OwnerPlot.Series[PInteger(vSeriesList.Items[vLoop])^]).CheckRefresh(vNeedsRefresh, vNeedsClear);
      end
      ELSE raise Exception.Create('UpdateSeriesData shall not be called except from Fast Series');
    end;
    // if we need Clear, we clear the DataImage
    if vNeedsClear then begin
      vFPColor.red:=$0; vFPColor.green:=$0; vFPColor.blue:=$0; vFPColor.alpha:=$0;
      FDataImage.FillPixels(vFPColor);
    end;
    // if we need Refresh, we let all series contained plot to the dataimage
    if vNeedsRefresh then begin
      for vLoop := 0 to vSeriesList.Count-1 do begin
        //writeln('plot to image series _', IntToStr(vLoop));
        OwnerPlot.Series[PInteger(vSeriesList.Items[vLoop])^].DoPlotToImage;
      end;
    end;
  END;


  begin
    OwnerPlot.LockImage(true);
//  notes on speed ...
//  Copyrect (or Draw) to Bitmap takes 17msec(fullscreen 1380*960)
//  no matter if it is the PlotIMage.Bitmap or a memory one (vBackBMP)
//  Apply Alpha takes 75msec (on that size)
//  vBackBMP Creation takes 5msec

    FDataImageBuffer.LoadFromIntfImage(DataImage);
    IF Zooming THEN _DrawZoomRectDataImage;

    // windows makes the FBackBMP completely transparent (when transparent things are drawn on top of it
    // reason ?: FBackBMP does not know transparency, so windows adds it and makes it fully transparent

    // Draw and CopyRect work the same, no difference....
    //PlotImage.Canvas.CopyRect(Self.FrameRect, vBackBMP.Canvas, Bounds(0,0,vBackBMP.Width, vBackBMP.Height)); // test for 2D why is bacjgnd copy needed for transparency ??
    PlotImage.Canvas.Draw(FrameRect.Left, FrameRect.Top, vBackBmp);
    //PlotImage.Canvas.CopyRect(Self.DataRect, FDataImageBuffer.Canvas, Bounds(0,0,FDataImageBuffer.Width, FDataImageBuffer.Height));
    PlotImage.Canvas.Draw(Self.DataRect.Left, Self.DataRect.Top, FDataImageBuffer);

    // markerupdate
    IF ASeries is TXYSpectrumPlotSeries THEN begin
      //writeln('plot markers       _', IntToStr(vLoop));
      for vLoop := 0 to vSeriesList.Count-1 do begin
        //TPlotSeries(OwnerPlot.Series[PtrInt(vSeriesList.Items[vLoop])]).MarkerContainer.DrawMarkers(vBackBmp, true);
        TPlotSeries(OwnerPlot.Series[PInteger(vSeriesList.Items[vLoop])^]).MarkerContainer.DrawMarkers(PlotImage.Picture.Bitmap, true);
      end;
    END;

    OwnerPlot.LockImage(false);
  end;

  FScreenInvalid:=FALSE;
  vBackBmp.Free;
  if vSeriesList <> nil then begin
    while vSeriesList.Count > 0 do begin
      Dispose(Pinteger(vSeriesList.Items[0]));
      vSeriesList.Delete(0);
    end;
    vSeriesList.Free;
  end;

end;


procedure TPlotRect.UpdateMarkers;
begin

end;

procedure TPlotRect.UpdateZoomRect;
begin
  // if series in plotrect are fast series, call UpdateSeriesData
  // if series are standard, simply draw the focusrect

  if _GetHasFastSeries then UpdateSeriesData(nil, false, true)
  else _DrawZoomRectPlotImage;
end;

procedure TPlotRect.Pan(dX, dY: Integer);
var
  vSeriesList: TFPList;
  //vItemIndex: Integer;
  vSeriesLoop, vLoop: Integer;
  vXAxesArray: array of Integer;
  vYAxesArray: array of Integer;
  vXaxis, vYaxis: Integer;
  vPresent: Boolean;
begin
  //writeln('PR panning...X/Y ', IntToStr(dX), '/', IntToStr(dY));
  // we do only XY panning
  vSeriesList := SeriesContainedIdx;
  setlength(vXAxesArray, 0);
  setlength(vYAxesArray, 0);
  try
    for vSeriesLoop := 0 to vSeriesList.Count-1 do begin
      vXaxis := TPlotSeries(OwnerPlot.Series[PInteger(vSeriesList.Items[vSeriesLoop])^]).XAxis;
      vYaxis := TPlotSeries(OwnerPlot.Series[PInteger(vSeriesList.Items[vSeriesLoop])^]).YAxis;
      // no Zaxis handling so far
      // X
      vPresent := false;
      for vLoop := 0 to length(vXAxesArray)-1 do begin
        vPresent := vPresent OR (vXAxesArray[vLoop] = vXaxis);
      end;
      if (not vPresent) then begin
        setlength(vXAxesArray, length(vXAxesArray)+1);
        vXAxesArray[length(vXAxesArray)-1] := vXaxis;
      end;
      // Y
      vPresent := false;
      for vLoop := 0 to length(vYAxesArray)-1 do begin
        vPresent := vPresent OR (vYAxesArray[vLoop] = vYaxis);
      end;
      if (not vPresent) then begin
        setlength(vYAxesArray, length(vYAxesArray)+1);
        vYAxesArray[length(vYAxesArray)-1] := vYaxis;
      end;
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

  for vLoop:=0 to length(vXAxesArray)-1 do begin
    TPlotAxis(OwnerPlot.Axis[vXAxesArray[vLoop]]).PanPixel(dX, dY);
  end;
  for vLoop:=0 to length(vYAxesArray)-1 do begin
    TPlotAxis(OwnerPlot.Axis[vYAxesArray[vLoop]]).PanPixel(dX, dY);
  end;
  setlength(vXAxesArray, 0);
  setlength(vYAxesArray, 0);

  //if _GetHasFastSeries then UpdateSeriesData(nil, false, true)
  //else OwnerPlot.Repaint;
  OwnerPlot.Repaint;
end;

procedure TPlotRect.Zoom(X, Y: Integer; AFactorX: Extended; AFactorY: Extended);
var
  vSeriesList: TFPList;
  //vItemIndex: Integer;
  vSeriesLoop, vLoop: Integer;
  vXAxesArray: array of Integer;
  vYAxesArray: array of Integer;
  vXaxis, vYaxis: Integer;
  vPresent: Boolean;
begin
  // TODO: unify with Pan
  //writeln('PR panning...X/Y ', IntToStr(dX), '/', IntToStr(dY));
  // we do only XY panning
  vSeriesList := SeriesContainedIdx;
  setlength(vXAxesArray, 0);
  setlength(vYAxesArray, 0);
  try
    for vSeriesLoop := 0 to vSeriesList.Count-1 do begin
      vXaxis := TPlotSeries(OwnerPlot.Series[PInteger(vSeriesList.Items[vSeriesLoop])^]).XAxis;
      vYaxis := TPlotSeries(OwnerPlot.Series[PInteger(vSeriesList.Items[vSeriesLoop])^]).YAxis;
      // no Zaxis handling so far
      // X
      vPresent := false;
      for vLoop := 0 to length(vXAxesArray)-1 do begin
        vPresent := vPresent OR (vXAxesArray[vLoop] = vXaxis);
      end;
      if (not vPresent) then begin
        setlength(vXAxesArray, length(vXAxesArray)+1);
        vXAxesArray[length(vXAxesArray)-1] := vXaxis;
      end;
      // Y
      vPresent := false;
      for vLoop := 0 to length(vYAxesArray)-1 do begin
        vPresent := vPresent OR (vYAxesArray[vLoop] = vYaxis);
      end;
      if (not vPresent) then begin
        setlength(vYAxesArray, length(vYAxesArray)+1);
        vYAxesArray[length(vYAxesArray)-1] := vYaxis;
      end;
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

  for vLoop:=0 to length(vXAxesArray)-1 do begin
    TPlotAxis(OwnerPlot.Axis[vXAxesArray[vLoop]]).ZoomPixel(X, Y, AFactorX);
  end;
  for vLoop:=0 to length(vYAxesArray)-1 do begin
    TPlotAxis(OwnerPlot.Axis[vYAxesArray[vLoop]]).ZoomPixel(X, Y, AFactorY);
  end;
  setlength(vXAxesArray, 0);
  setlength(vYAxesArray, 0);

  //if _GetHasFastSeries then UpdateSeriesData(nil, false, true)
  //else OwnerPlot.Repaint;
  OwnerPlot.Repaint;
end;

procedure TPlotRect.Zoom(ARect: TRect);
var
  vDoneSet: TIntegerSet;
  vXaxis, vYaxis: Integer;
  vLoop: Integer;
  vScrPoint: TPoint;
  vXminmax, vYminmax, vViewRange: TValueRange;
  vSeriesList: TFPList;
begin
  vDoneSet:=[];
  //with SeriesContainedIdx do
  vSeriesList := SeriesContainedIdx;
  try
    //writeln('zoom now');

    for vLoop := 0 to vSeriesList.Count-1 do begin

      IF ( (OwnerPlot.Series[PInteger(vSeriesList.Items[vLoop])^] is TXYPlotSeries) ) THEN BEGIN
        vXaxis := TPlotSeries(OwnerPlot.Series[PInteger(vSeriesList.Items[vLoop])^]).XAxis;
        vYaxis := TPlotSeries(OwnerPlot.Series[PInteger(vSeriesList.Items[vLoop])^]).YAxis;
        //
        // first point
        vScrPoint.X:= ARect.Left;
        vScrPoint.Y:= ARect.Bottom;
        // min values
        ScreenToXY( OwnerPlot.Axis[vXaxis] ,
                    OwnerPlot.Axis[vYAxis] ,
                    vXminmax.min, vYminmax.min, vScrPoint); // FMDownPos

        // max values
        // second point
        vScrPoint.X:= ARect.Right;
        vScrPoint.Y:= ARect.Top;
        ScreenToXY( OwnerPlot.Axis[vXAxis] ,
                    OwnerPlot.Axis[vYAxis] ,
                    vXminmax.max, vYminmax.max, vScrPoint); //vMUpPos

        IF not (vXaxis in vDoneSet) THEN BEGIN
          //vDoneList.Add(vAxisPtr);
          vDoneSet := vDoneSet + [vXaxis];
          if vXminmax.min <= vXminmax.max then begin
            vViewRange.min := vXminmax.min; vViewRange.max := vXminmax.max;
          end else begin
            vViewRange.min := vXminmax.max; vViewRange.max := vXminmax.min;
          end;
          OwnerPlot.Axis[vXAxis].ViewRange := vViewRange;
        END;
        IF not (vYaxis in vDoneSet) THEN BEGIN
          vDoneSet := vDoneSet + [vYaxis];
          if vYminmax.min <= vYminmax.max then begin
            vViewRange.min := vYminmax.min; vViewRange.max := vYminmax.max;
          end else begin
            vViewRange.min := vYminmax.max; vViewRange.max := vYminmax.min;
          end;
          OwnerPlot.Axis[vYAxis].ViewRange := vViewRange;
        END;
      END;

    end;
    OwnerPlot.Repaint;

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

procedure TPlotRect.SetBorderAbs(const AValue: TBorder);
begin
  FBorderAbs:=AValue;
end;

function TPlotRect._FocusColor(BackgroundColor: TColor): TColor;
var
  vR, vG, vB: byte;
  vRGB: LongInt;
begin
  begin
    vRGB := ColorToRGB(BackgroundColor);
    vR:= vRGB AND $FF;
    vG := (vRGB SHR 8 ) AND $FF;
    vB := (vRGB SHR 16) AND $FF;

    Result := RGBToColor(
      IfThen(vR>$40, $00, $FF),
      IfThen(vG>$40, $00, $FF),
      IfThen(vB>$40, $00, $FF) );
  end;
end;

procedure TPlotRect.SetAxisAutoPlaceFill(AValue: Boolean);
begin
  if FAxisAutoPlaceFill=AValue then Exit;
  FAxisAutoPlaceFill:=AValue;
end;

function TPlotRect.GetAxisAutoPlaceFillConstraints(AIndex: Integer): Double;
begin
  IF (AIndex < 0) OR (AIndex > length(FAxisConstraints)-1) THEN Result := 0
  ELSE Result := FAxisConstraints[AIndex];
end;

procedure TPlotRect.SetAxisAutoPlaceFillConstraints(AIndex: Integer;
  AValue: Double);
begin
  IF (AIndex < 0) OR (AIndex > length(FAxisConstraints)-1) THEN exit;
  FAxisConstraints[AIndex] := AValue;
end;

procedure TPlotRect.SetBorderRel(const AValue: TBorder);
begin
  FBorderRel:=AValue;
end;

procedure TPlotRect.SetColorScalePlacement(const AValue: TRectPlacement);
begin
  if FColorScalePlacement=AValue then exit;
  FColorScalePlacement:=AValue;
end;

procedure TPlotRect.SetColorScaleRect(const AValue: TColorScaleRect);
begin
  if FColorScaleRect = AValue then exit;
  FColorScaleRect := AValue;
end;

procedure TPlotRect.SetLegendPlacement(const AValue: TRectPlacement);
begin
  if FLegendPlacement=AValue then exit;
  FLegendPlacement:=AValue;

end;

procedure TPlotRect.SetLegendRect(const AValue: TLegendRect);
begin
  if FLegendRect = AValue then exit;
  FLegendRect := AValue;
end;


procedure TPlotRect.SetStyle(const AValue: TPlotStyle);
begin
  if FStyle=AValue then exit;
  FStyle:=AValue;
end;

procedure TPlotRect._ShrinkColorScale(var ARect: TRect);
var
  vUsedRect: TRect;
begin
  {$IFDEF DEBUG_2_WRITE}
  writeln('old rect before ColorScale shrink:');
  with ARect do begin
    write('T: ', IntToStr(Top), '  ');
    write('B: ', IntToStr(Bottom), '  ');
    write('L: ', IntToStr(Left), '  ');
    writeln('R: ', IntToStr(Right), '  ');
  end;
  {$ENDIF}
  IF (ColorScalePlacement <> plCanvas) and ShowColorScale THEN BEGIN
    // check size at FrameRecttopleft
    // legend and colorscale deliver usedrect which means size, left = 0 , top = 0
    vUsedRect := ColorScaleRect.Redraw(FALSE, FALSE);
    {$IFDEF DEBUG_2_WRITE}
    writeln('ColorScale usedrect:');
    with vUsedRect do begin
      write('T: ', IntToStr(Top), '  ');
      write('B: ', IntToStr(Bottom), '  ');
      write('L: ', IntToStr(Left), '  ');
      writeln('R: ', IntToStr(Right), '  ');
    end;
    {$ENDIF}
    // asnpassen legendrect anchor und clientrect size
    CASE ColorScalePlacement of
    plBottom:            begin
                            ARect.Bottom := max(ARect.Top,  ARect.Bottom - (vUsedRect.Bottom-vUsedRect.Top));
                         end;
    plRight:             begin
                             ARect.Right := max(ARect.Left, ARect.Right - (vUsedRect.Right-vUsedRect.Left));
                         end;
    END;

  END;
  {$IFDEF DEBUG_2_WRITE}
  writeln('new rect after ColorScale shrink:');
  with ARect do begin
    write('T: ', IntToStr(Top), '  ');
    write('B: ', IntToStr(Bottom), '  ');
    write('L: ', IntToStr(Left), '  ');
    writeln('R: ', IntToStr(Right), '  ');
  end;
  {$ENDIF}
end;

procedure TPlotRect._ShrinkLegend(var ARect: TRect);
var
  vUsedRect: TRect;
begin
  {$IFDEF DEBUG_2_WRITE}
  writeln('old rect before legend shrink:');
  with ARect do begin
    write('T: ', IntToStr(Top), '  ');
    write('B: ', IntToStr(Bottom), '  ');
    write('L: ', IntToStr(Left), '  ');
    writeln('R: ', IntToStr(Right), '  ');
  end;
  {$ENDIF}
  IF (LegendPlacement <> plCanvas) and ShowLegend THEN BEGIN
    // legend and colorscale deliver usedrect which means size, left = 0 , top = 0
    vUsedRect := LegendRect.Redraw(FALSE, FALSE);
    // asnpassen legendrect anchor und clientrect size
    {$IFDEF DEBUG_2_WRITE}
    writeln('Legend usedrect:');
    with vUsedRect do begin
      write('T: ', IntToStr(Top), '  ');
      write('B: ', IntToStr(Bottom), '  ');
      write('L: ', IntToStr(Left), '  ');
      writeln('R: ', IntToStr(Right), '  ');
    end;
    {$ENDIF}
    CASE LegendPlacement of
    plBottom:            begin
                            ARect.Bottom := max(ARect.Top,  ARect.Bottom - (vUsedRect.Bottom-vUsedRect.Top));
                         end;
    plRight:             begin
                             ARect.Right := max(ARect.Left, ARect.Right - (vUsedRect.Right-vUsedRect.Left));
                         end;
    END;

  END;
  {$IFDEF DEBUG_2_WRITE}
  writeln('new rect after legend shrink:');
  with ARect do begin
    write('T: ', IntToStr(Top), '  ');
    write('B: ', IntToStr(Bottom), '  ');
    write('L: ', IntToStr(Left), '  ');
    writeln('R: ', IntToStr(Right), '  ');
  end;
  {$ENDIF}
end;

procedure TPlotRect._ShrinkTitle(var ARect: TRect);
var
  vUsedRect: TRect;
  vAdditional: TRect;
begin
  vUsedRect := DrawTitle(FALSE);  // usedrect with respect to clientrect
  {$IFDEF DEBUG_2_WRITE}
  writeln('old rect before title shrink:');
  with ARect do begin
    write('T: ', IntToStr(Top), '  ');
    write('B: ', IntToStr(Bottom), '  ');
    write('L: ', IntToStr(Left), '  ');
    writeln('R: ', IntToStr(Right), '  ');
  end;
  {$ENDIF}

  with vAdditional do begin
    Top := math.Max(0, ARect.Top - vUsedRect.Top);   // positive values means shrink clientrect
    Bottom := math.max(0, -(ARect.Bottom -vUsedRect.Bottom));  // neagtive values means do nothing
    Left := math.max(0, (ARect.Left -vUsedRect.Left));   // we do not enlarge as we start with max possible size
    Right := math.max(0, -(ARect.Right -vUsedRect.Right));
  end;

  with ARect do begin
    Left   := Left   + vAdditional.Left;
    Right  := max(Left, Right  - vAdditional.Right);
    Top    := Top    + vAdditional.Top;
    Bottom := max(Top, Bottom - vAdditional.Bottom);
  end;

  {$IFDEF DEBUG_2_WRITE}
  writeln('new rect after title shrink:');
  with ARect do begin
    write('T: ', IntToStr(Top), '  ');
    write('B: ', IntToStr(Bottom), '  ');
    write('L: ', IntToStr(Left), '  ');
    writeln('R: ', IntToStr(Right), '  ');
  end;
  {$ENDIF}
end;


procedure TPlotRect.PreCalcClientRect;
var
  vUsedRect: TRect;
  vNewClientRect: TRect; // vUsedFrame
  vAxisSizeAdd: TBorder;
  vLoop: integer;
  vNetAxisRect, vTotalAxisAdd: TRect;
  a,b,c: double; // length
  anglea, angleb, anglec: Extended;
  vLx, vLy, vLz: double;
  vAxisCount: Integer;
  vNewOriginRel: TPoint;
  vSolved: Integer;
  vAxisAutoPlaceFillActive: Boolean;
  vOriginMode: TOriginMode; // needed because DrawOrigin delivers NOT FDrawOrigin in omRel mode
  vFallBackLength: array[0..2] of Extended;
  vFallBackOriginRel: array[0..2] of TPoint;
begin
  // 10.10.14 the following new concept:
  { 1. check size additional to data area (i.e. labels, units etc.)
    2. shrink the DataArea accordingly (ClientRect - additional size)
    3. fill the axes into that
    ----------
    Axis placement is done by solving a 3 dimensional system (for 3 axes)
    or a 2 dimensional system (for 2 axes or when no solution is found for 3 axes)
    ..
    If no solution is possible, default to placement of individual axes
  }

  vAxisCount:=0;
  //writeln('Calc rects ..........................................................:');
  vAxisSizeAdd.Top:=0; vAxisSizeAdd.Bottom:=0;
  vAxisSizeAdd.Left:=0; vAxisSizeAdd.Right:=0;
  vTotalAxisAdd := rect(0,0,0,0);
  // check size of title    ------------------------
  IF not AutoClientRect THEN exit;
  FClientRect := FrameRect;
  FDataRect := FrameRect;

  vNewClientRect := FrameRect;
  _ShrinkTitle(vNewClientRect);
  FClientRect := vNewClientRect;

  _ShrinkLegend(vNewClientRect);
  FClientRect := vNewClientRect;

  _ShrinkColorScale(vNewClientRect);
  FClientRect := vNewClientRect;

  FDataRect := FClientRect;

  for vLoop := 0 to OwnerPlot.AxisCount - 1 do begin
    IF OwnerPlot.Axis[vLoop].OwnerPlotRect = Self THEN BEGIN
     {$IFDEF DEBUG_SCALE_WRITE}
      writeln('---------------------------------------------');
      writeln('Axis ', IntToStr(vLoop), ' checksize');
     {$ENDIF}
      vUsedRect := OwnerPlot.Axis[vLoop].CheckSize(vNetAxisRect);  // TODO
     {$IFDEF DEBUG_2_WRITE}
       writeln('axis used:');
       with vUsedRect do begin
         write('T: ', IntToStr(Top), '  ');
         write('B: ', IntToStr(Bottom), '  ');
         write('L: ', IntToStr(Left), '  ');
         writeln('R: ', IntToStr(Right), '  ');
       end;
       writeln('net axis used:');
       with vNetAxisRect do begin
         write('T: ', IntToStr(Top), '  ');
         write('B: ', IntToStr(Bottom), '  ');
         write('L: ', IntToStr(Left), '  ');
         writeln('R: ', IntToStr(Right), '  ');
       end;
     {$ENDIF}

    // now we have the difference between used and net rect, so we shrink ClientRect
    with vAxisSizeAdd do begin
      Top := vNetAxisRect.Top - vUsedRect.Top;
      Bottom := vNetAxisRect.Bottom - vUsedRect.Bottom;
      Left := vNetAxisRect.Left - vUsedRect.Left;
      Right := vNetAxisRect.Right - vUsedRect.Right;
    end;

    {$IFDEF DEBUG_2_WRITE}
      writeln('axis add:');
      with vAxisSizeAdd do begin
        write('T: ', IntToStr(Top), '  ');
        write('B: ', IntToStr(Bottom), '  ');
        write('L: ', IntToStr(Left), '  ');
        writeln('R: ', IntToStr(Right), '  ');
      end;
    {$ENDIF}
    with vTotalAxisAdd do begin
      Top :=    max(Top, vAxisSizeAdd.Top);
      Bottom := min(Bottom, vAxisSizeAdd.Bottom);
      Left :=   max(Left, vAxisSizeAdd.Left);
      Right :=  min(Right, vAxisSizeAdd.Right);
    end;

    END;
  end;

  with FClientRect do begin
    Top := min(Bottom-1, Top + vTotalAxisAdd.Top);
    Bottom := max(Top+1, Bottom + vTotalAxisAdd.Bottom);
    Left := min(Right-1, Left + vTotalAxisAdd.Left);
    Right := max(Left+1, Right + vTotalAxisAdd.Right);
  end;
  {$IFDEF DEBUG_2_WRITE}
    writeln('axis add:');
    with vAxisSizeAdd do begin
      write('T: ', IntToStr(Top), '  ');
      write('B: ', IntToStr(Bottom), '  ');
      write('L: ', IntToStr(Left), '  ');
      writeln('R: ', IntToStr(Right), '  ');
    end;
    writeln('New client:');
    with FClientRect do begin
      write('T: ', IntToStr(Top), '  ');
      write('B: ', IntToStr(Bottom), '  ');
      write('L: ', IntToStr(Left), '  ');
      writeln('R: ', IntToStr(Right), '  ');
    end;
  {$ENDIF}
  FDataRect := FClientRect;

  // if we have <2 or >3 axes, we are finished now with the new clientrect
  // for 2 or 3 axes, we have autoplace when property is set
  // if autoplace (below) is not working, axis placement defaults to the axis settings

  IF not AxisAutoPlaceFill then exit;
  // ---------------------------------------------------------------------------

  for vLoop := 0 to OwnerPlot.AxisCount - 1 do begin
    IF (OwnerPlot.Axis[vLoop].OwnerPlotRect = Self)  THEN inc(vAxisCount);
  end;

  vSolved:=-1;
  // for default placement we need to do
  // switch OFF AxisAutoPlaceFill
  // - remember DrawLength
  // Switch ON AxisAutoPlaceFill
  vAxisAutoPlaceFillActive:=AxisAutoPlaceFill;

  if (vAxisCount > 3) or (vAxisCount < 2) then begin
    AxisAutoPlaceFill:=false;
    exit;
  end;

  if (vAxisCount = 3) or (vAxisCount = 2) then begin
    // get coefficients for length and angle
    vAxisCount:=0;
    if vAxisAutoPlaceFillActive = true then AxisAutoPlaceFill := false;
    for vLoop := 0 to OwnerPlot.AxisCount - 1 do begin
      IF OwnerPlot.Axis[vLoop].OwnerPlotRect = Self THEN BEGIN
        inc(vAxisCount);
        case vAxisCount of
          1: begin
               vFallBackLength[0] := TPLotAxis(OwnerPlot.Axis[vLoop]).DrawLength;
               vOriginMode := TPLotAxis(OwnerPlot.Axis[vLoop]).OriginMode;
               TPLotAxis(OwnerPlot.Axis[vLoop]).OriginMode := omAbs;
               vFallBackOriginRel[0] := TPLotAxis(OwnerPlot.Axis[vLoop]).DrawOriginRel; // unfortunately axis delivers FDrawOrigin only in omAbs mode
               TPLotAxis(OwnerPlot.Axis[vLoop]).OriginMode := vOriginMode;
               a := 1; //FAxisConstraints[0];
               anglea := TPLotAxis(OwnerPlot.Axis[vLoop]).DrawAngle / 180 * Pi();
             end;
          2: begin
               vFallBackLength[1] := TPLotAxis(OwnerPlot.Axis[vLoop]).DrawLength;
               vOriginMode := TPLotAxis(OwnerPlot.Axis[vLoop]).OriginMode;
               TPLotAxis(OwnerPlot.Axis[vLoop]).OriginMode := omAbs;
               vFallBackOriginRel[1] := TPLotAxis(OwnerPlot.Axis[vLoop]).DrawOriginRel;
               TPLotAxis(OwnerPlot.Axis[vLoop]).OriginMode := vOriginMode;
               b := 1; //FAxisConstraints[1];
               angleb := TPLotAxis(OwnerPlot.Axis[vLoop]).DrawAngle / 180 * Pi();
             end;
          3: begin
               vFallBackLength[2] := TPLotAxis(OwnerPlot.Axis[vLoop]).DrawLength;
               vOriginMode := TPLotAxis(OwnerPlot.Axis[vLoop]).OriginMode;
               TPLotAxis(OwnerPlot.Axis[vLoop]).OriginMode := omAbs;
               vFallBackOriginRel[2] := TPLotAxis(OwnerPlot.Axis[vLoop]).DrawOriginRel;
               TPLotAxis(OwnerPlot.Axis[vLoop]).OriginMode := vOriginMode;
               c := 1; //FAxisConstraints[2];
               anglec := TPLotAxis(OwnerPlot.Axis[vLoop]).DrawAngle / 180 * Pi();
             end;
        end;
      END;
    end;
    if vAxisAutoPlaceFillActive = true then AxisAutoPlaceFill := true;

    if vAxisCount = 3 then
      vSolved := _AxisAutoPlace3(a,b,c, anglea, angleb, anglec, vLx, vLy, vLz, vNewOriginRel)
    else if vAxisCount = 2 then
      vSolved := _AxisAutoPlace2(a,b,anglea, angleb, vLx, vLy, vNewOriginRel);

    // check if we have a solution, otherwise try 2 axis solve
    IF (vSolved < 0) and (vAxisCount=3) THEN BEGIN
      //writeln('solving 2 axes');
      // check which axis has negative solution....
      if (vLx < 0) then begin
        vLx := 0;
        vSolved := _AxisAutoPlace2(b,c,angleb, anglec, vLy, vLz, vNewOriginRel);
      end else
      if (vLy < 0) then begin
        vLy := 0;
        vSolved := _AxisAutoPlace2(a,c,anglea, anglec, vLx, vLz, vNewOriginRel);
      end else
      if (vLz < 0) then begin
        vLz := 0;
        vSolved := _AxisAutoPlace2(a,b,anglea, angleb, vLx, vLy, vNewOriginRel);
      end else vSolved := -1;
    END;
  end;  // end 3 axis solver

  IF (vSolved = 0) THEN BEGIN
    //writeln(' axis problem SOLVED ##########################################');
    //writeln('New origin #############:');
    //writeln('x: ', IntToStr(vNewOriginRel.X));
    //writeln('y: ', IntToStr(vNewOriginRel.Y));
    vAxisCount:=0;
    for vLoop := 0 to OwnerPlot.AxisCount - 1 do begin
      IF OwnerPlot.Axis[vLoop].OwnerPlotRect = Self THEN BEGIN
        inc(vAxisCount);
        case vAxisCount of
          1: begin
               TPLotAxis(OwnerPlot.Axis[vLoop]).AutoPlacedLength := max(0, (vLx)/1 );
               TPLotAxis(OwnerPlot.Axis[vLoop]).AutoPlacedOriginRel := vNewOriginRel;
             end;
          2: begin
               TPLotAxis(OwnerPlot.Axis[vLoop]).AutoPlacedLength := max(0, (vLy)/1 );
               TPLotAxis(OwnerPlot.Axis[vLoop]).AutoPlacedOriginRel := vNewOriginRel;
             end;
          3: begin
               TPLotAxis(OwnerPlot.Axis[vLoop]).AutoPlacedLength := max(0, (vLz)/1 );
               TPLotAxis(OwnerPlot.Axis[vLoop]).AutoPlacedOriginRel := vNewOriginRel;
             end;
        end;
      END;
    end;
  END ELSE BEGIN
    //writeln(' 2 axis problem UNSOLVED - dafaulting ##########################################');
    vAxisCount:=0;
    for vLoop := 0 to OwnerPlot.AxisCount - 1 do begin
      IF OwnerPlot.Axis[vLoop].OwnerPlotRect = Self THEN BEGIN
        inc(vAxisCount);
        case vAxisCount of
          1: begin
               TPLotAxis(OwnerPlot.Axis[vLoop]).AutoPlacedLength := vFallBackLength[0];
               TPLotAxis(OwnerPlot.Axis[vLoop]).AutoPlacedOriginRel := vFallBackOriginRel[0];
             end;
          2: begin
               TPLotAxis(OwnerPlot.Axis[vLoop]).AutoPlacedLength := vFallBackLength[1];
               TPLotAxis(OwnerPlot.Axis[vLoop]).AutoPlacedOriginRel := vFallBackOriginRel[1];
             end;
          3: begin
               TPLotAxis(OwnerPlot.Axis[vLoop]).AutoPlacedLength := vFallBackLength[2];
               TPLotAxis(OwnerPlot.Axis[vLoop]).AutoPlacedOriginRel := vFallBackOriginRel[2];
             end;
        end;
      END;
    end;
  END;
  //readln;
end;

function TPlotRect._BuildConstraints3(a,b,c, anglea, angleb, anglec: Double; out s, t, u: Double): Integer;
var
  vLoop, vZeroCount, vZeroIndex: Integer;
  va, vb, vc, vLa, vLb, vAnglea, vAngleb, vAnglec: Double;
  vLa2, vLb2, vLc: Double;
  vCa, vCb: Double;
  vCc: Double;
  vW, vH: Integer;
  vSolutionPossible: Boolean;
  vGamma: Double;
  vRemain: Double;
const
  cEpsilon = 1e-10;
begin
  { What it does..
    Used for 3 axes auto placement
    AxisAutoPlaceFillConstraints[0,1,2] need to have one element = 0
    The element = 0 has variable length
    The other 2 elements will be placed with length according to the relation of given numbers
    IF no elemnt is = 0 we default to the matrix row 1,-2,1 which means 2* y axis length 2*Ly = Lx + Lz
  }
  // standard solution:
    s := -1;
    t := 2;
    u := -1;
  Result := -1;
  vSolutionPossible:=true;

  vZeroCount := 0;
  vZeroIndex := -1;
  for vLoop := 0 to 2 do begin
    if AxisAutoPlaceFillConstraints[vLoop] < cEpsilon then begin
      inc(vZeroCount);
      vZeroIndex:=vLoop;
    end;
  end;

  if vZeroCount <> 1 then vSolutionPossible:=false;
  if (abs(cos(anglea)) < cEpsilon) and (abs(cos(angleb)) < cEpsilon) then vSolutionPossible:=false;
  if (abs(sin(anglea)) < cEpsilon) and (abs(sin(angleb)) < cEpsilon) then vSolutionPossible:=false;

  if not vSolutionPossible then  exit;      // default 1, -2, 1 placement



  BEGIN
    //writeln('solving 2 axes');
    // cannot use the 2 axis solver..
    if (vZeroIndex = 0) then begin
      va := b;  vAnglea := angleb;
      vb := c;  vAngleb := anglec;
      vc := a;  vAnglec := anglea;
      vCa := AxisAutoPlaceFillConstraints[1];
      vCb := AxisAutoPlaceFillConstraints[2];
    end else
    if (vZeroIndex = 1) then begin
      va := a;  vAnglea := anglea;
      vb := c;  vAngleb := anglec;
      vc := b;  vAnglec := angleb;
      vCa := AxisAutoPlaceFillConstraints[0];
      vCb := AxisAutoPlaceFillConstraints[2];
    end else
    if (vZeroIndex = 2) then begin
      va := a;  vAnglea := anglea;
      vb := b;  vAngleb := angleb;
      vc := c;  vAnglec := anglec;
      vCa := AxisAutoPlaceFillConstraints[0];
      vCb := AxisAutoPlaceFillConstraints[1];
    end;
  END;

  vGamma := (va*vCa) / (vb*vCb);

  vW := FClientRect.Right - FClientRect.Left;
  vH := FClientRect.Bottom - FClientRect.Top;

  vLb := vW / (abs(vGamma*va*vCa*cos(vAnglea)) + abs(vGamma*vb*vCb*cos(vAngleb))  ) ;
  vLa := vGamma * vLb;
  //writeln('vLa, vLb: ', FloatToStrF(vLa, ffFixed, 4,4), ' , ', FloatToStrF(vLb, ffFixed, 4,4) );

  vLb2 := vH / (abs(vGamma*va*vCa*sin(vAnglea)) + abs(vGamma*vb*vCb*sin(vAngleb))  ) ;
  vLa2 := vGamma * vLb2;
  //writeln('vLa2, vLb2: ', FloatToStrF(vLa2, ffFixed, 4,4), ' , ', FloatToStrF(vLb2, ffFixed, 4,4) );

  // take minimum and calc remaining space for vLc
  if (vLb < vLb2) then begin        // vertical space remains
    if (abs(sin(vAnglec)) < cEpsilon) then begin
      vSolutionPossible:=false;
    end else begin
      vRemain := vH - abs(sin(vAnglea) * vLa) - abs(sin(vAngleb) * vLb) ;
      vLc := vRemain / abs(sin(vAnglec));
    end;
  end else begin                    // hor space remains
    if (abs(cos(vAnglec)) < cEpsilon) then begin
      vSolutionPossible:=false;
    end else begin
      vRemain := vW - abs(cos(vAnglea) * vLa2) - abs(cos(vAngleb) * vLb2) ;
      vLc := vc * vRemain / abs(cos(vAnglec));
    end;
  end;

  if vLc < 0 then vSolutionPossible:=false;
  // we here have some relation between vLa, vLb and vLc

  //0 = a*vCa*La + b*vCb*Lb - c*vCc*Lc; //
  vCc := -( va*vCa*vLa + vb*vCb*vLb ) / (vc*vLc);

  IF not vSolutionPossible THEN BEGIN
    Result := -1;
  END ELSE
  BEGIN
    Result := 0;
    if (vZeroIndex = 0) then begin
      s := vCc;
      t := vCa;
      u := vCb;
    end else
    if (vZeroIndex = 1) then begin
      s := vCa;
      t := vCc;
      u := vCb;
    end else
    if (vZeroIndex = 2) then begin
      s := vCa;
      t := vCb;
      u := vCc;
    end;
  END;
end;  // end constraints solver


function TPlotRect._AxisAutoPlace3(a, b, c, anglea, angleb, anglec: Double; out
  La, Lb, Lc: Double; out NewOrigin: TPoint): Integer;
var
  vW, vH: Integer;
  vS: Double;
  x,y,z: Double; // width coeff
  p,q,r: double; // height coeff
  s,t,u: double; // constraints
  vLx, vLy, vLz: double;
  vCol1, vCol2, vCol3, vColRes: Tvector3_double;
  vMZ, vMN: Tmatrix3_double;
  vDetN: Double;
  dx, dy: double; // new origin coordinates in pixels
  vNewOriginRel: TPoint;
begin
  Result := 0;
  //FAxisPlacementSolved:=false;
  // build coefficients from length and angle for width and height constraints
  // constraints s,t,u
  _BuildConstraints3(a, b, c, anglea, angleb, anglec,s,t,u);
  x := abs(a * cos(anglea));  // add all axes length, x,y,z = width coeff
  y := abs(b * cos(angleb));
  z := abs(c * cos(anglec));
  p := abs(a * sin(anglea));  // add all axes hight, p,q,r = heigth coeff
  q := abs(b * sin(angleb));
  r := abs(c * sin(anglec));

  {$IFDEF DEBUG_2_WRITE}
    writeln('coefficients #########:');
      write('x: ', FloatToStrF(x, ffFixed, 4,4),  '  /  ');
      write('y: ', FloatToStrF(y, ffFixed, 4,4),  '  /  ');
      writeln('z: ', FloatToStrF(z, ffFixed, 4,4));
      write('p: ', FloatToStrF(p, ffFixed, 4,4), '  /  ');
      write('q: ', FloatToStrF(q, ffFixed, 4,4), '  /  ');
      writeln('r: ', FloatToStrF(r, ffFixed, 4,4));
  {$ENDIF}

  // build vectors
  vCol1.init(x,p,s);
  vCol2.init(y,q,t);
  vCol3.init(z,r,u);

  vW := FClientRect.Right - FClientRect.Left;
  vH := FClientRect.Bottom - FClientRect.Top;
  vS := 0; //(a+b+c) ;
  vColRes.init(vW, vH, vS);
  // solve system:
  vMN.set_column(0, vCol1);
  vMN.set_column(1, vCol2);
  vMN.set_column(2, vCol3);
  vDetN := vMN.determinant;
  //write('detN: ', FloatToStrF(vDetN, ffFixed, 4,4), '  ');
  if vDetN <> 0 then begin
      // X
    vMZ := vMN;
    vMZ.set_column(0, vColRes);
    vLx := vMZ.determinant / vDetN;
      // Y
    vMZ := vMN;
    vMZ.set_column(1, vColRes);
    vLy := vMZ.determinant / vDetN;
      // Z
    vMZ := vMN;
    vMZ.set_column(2, vColRes);
    vLz := vMZ.determinant / vDetN;
    {$IFDEF DEBUG_2_WRITE}
      writeln('length results #############:');
        write('vH: ', INtToStr(vH), '  ');
        write('vW: ', INtToStr(vW));
        write('X: ', FloatToStrF(vLx, ffFixed, 4,4), '  ');
        write('Y: ', FloatToStrF(vLy, ffFixed, 4,4), '  ');
        writeln('Z: ', FloatToStrF(vLz, ffFixed, 4,4), '  ');
    {$ENDIF}

    //// need to calc the new origin and set for all 3 axes (origin is in math coordinates)
    //writeln('dx, dy: ', FloatToStrF(dx, ffFixed, 5,5), ' / ', FloatToStrF(dy, ffFixed, 5,5));
    dx := - min(0, max(0, vLx) * (1/a * cos(anglea))) - min(0, max(0, vLy) * (1/b * cos(angleb))) - min(0, max(0, vLz) * (1/c * cos(anglec)));
    dy := - min(0, max(0, vLx) * (1/a * sin(anglea))) - min(0, max(0, vLy) * (1/b * sin(angleb))) - min(0, max(0, vLz) * (1/c * sin(anglec)));

    if (vW > 0) and (vH > 0) then begin
      vNewOriginRel.X := round(100*dx/vW);
      vNewOriginRel.Y := round(100*dy/vH);
    end;
    {$IFDEF DEBUG_2_WRITE}
      writeln('xy origin shift #############:');
        write('dx: ', FloatToStrF(dx, ffFixed, 4,4), '  ');
        writeln('dy: ', FloatToStrF(dy, ffFixed, 4,4), '  ');
    {$ENDIF}

  end;
  La := vLx;  Lb := vLy;  Lc := vLz;
  NewOrigin := vNewOriginRel;
  if (La<0) or (Lb<0) or (Lc<0) then Result := -1
  else Result := 0;
end;

function TPlotRect._AxisAutoPlace2(a, b, anglea, angleb: Double; out La,
  Lb: Double; out NewOrigin: TPoint): Integer;
var
  vW, vH: Integer;
  //vS: Double;
  x,y: Double; // witdth coeff
  p,q: double; // height coeff
  //s,t: double; // constraints
  vLx, vLy: double;
  vCol1, vCol2, vColRes: Tvector2_double;
  vMZ, vMN: Tmatrix2_double;
  vDetN: Double;
  dx, dy: double; // new origin coordinates in pixels
  vNewOriginRel: TPoint;
begin
  Result := 0;
  vLx := -1; vLy := -1;
  // build coefficients from length and angle for width and height constraints

  x := abs(1*a * cos(anglea));  // add all axes length, x,y,z = width coeff
  y := abs(1*b * cos(angleb));
  p := abs(1*a * sin(anglea));  // add all axes hight, p,q,r = heigth coeff
  q := abs(1*b * sin(angleb));

  {$IFDEF DEBUG_2_WRITE}
    writeln('coefficients #########:');
      write('x: ', FloatToStrF(x, ffFixed, 4,4),  '  /  ');
      write('y: ', FloatToStrF(y, ffFixed, 4,4),  '  /  ');
      //writeln('z: ', FloatToStrF(z, ffFixed, 4,4));
      write('p: ', FloatToStrF(p, ffFixed, 4,4), '  /  ');
      writeln('q: ', FloatToStrF(q, ffFixed, 4,4), '  /  ');
      //writeln('r: ', FloatToStrF(r, ffFixed, 4,4));
  {$ENDIF}

  // constraints
  //s := 1 / a;
  //t := -1 / b;
  //u := 1 / c;
  // build vectors
  vCol1.init(x,p);
  vCol2.init(y,q);
  //vCol3.init(z,r,u);

  vW := FClientRect.Right - FClientRect.Left;
  vH := FClientRect.Bottom - FClientRect.Top;  // only one needed from vW, vH, delivers same result (or at least should..)
  //vS := 0; //(a+b+c) ;                       // however we use both and do NOT use the constraints in S
  vColRes.init(vW, vH);
  // solve system:
  vMN.set_column(0, vCol1);
  vMN.set_column(1, vCol2);
  vDetN := vMN.determinant;
  //write('detN: ', FloatToStrF(vDetN, ffFixed, 4,4), '  ');
  if vDetN <> 0 then begin
      // X
    vMZ := vMN;
    vMZ.set_column(0, vColRes);
    vLx := vMZ.determinant / vDetN;
      // Y
    vMZ := vMN;
    vMZ.set_column(1, vColRes);
    vLy := vMZ.determinant / vDetN;
    {$IFDEF DEBUG_2_WRITE}
      writeln('length results #############:');
        write('vH: ', INtToStr(vH), '  ');
        write('vW: ', INtToStr(vW));
        write('X: ', FloatToStrF(vLx, ffFixed, 4,4), '  ');
        writeln('Y: ', FloatToStrF(vLy, ffFixed, 4,4), '  ');
        //writeln('Z: ', FloatToStrF(vLz, ffFixed, 4,4), '  ');
    {$ENDIF}

    //// need to calc the new origin and set for all 3 axes (origin is in math coordinates)
    dx := - minvalue( [double(0), double(vLx * cos(anglea)), double(vLy * cos(angleb))] );
    dy := max(0,  vH + minvalue( [double(0), double(-vLx * sin(anglea)), double(-vLy * sin(angleb))] ) );

    if (vW > 0) and (vH > 0) then begin
      vNewOriginRel.X := round(100*dx/vW);
      vNewOriginRel.Y := round(100*dy/vH);
    end;
    {$IFDEF DEBUG_2_WRITE}
      writeln('xy origin shift #############:');
        write('dx: ', FloatToStrF(dx, ffFixed, 4,4), '  ');
        writeln('dy: ', FloatToStrF(dy, ffFixed, 4,4), '  ');
    {$ENDIF}
    {$IFDEF DEBUG_2_WRITE}
      writeln('new origin returned #############:');
        write('x: ', IntToStr(vNewOriginRel.X));
        writeln('  y: ', IntToStr(vNewOriginRel.Y));
    {$ENDIF}

  end;
  La := vLx;  Lb := vLy;
  NewOrigin := vNewOriginRel;
  if (La<0) or (Lb<0) then Result := -1
  else Result := 0;
end;


function TPlotRect.DrawTitle(AVisible: Boolean): TRect;
// TODO: implement "transparent"
var
  vTextWidth: Integer;
  vTextHeight: Integer;
  vTextPt: TPoint;
  vWholeTitle: String;
begin
  Result.Top := 0; Result. Bottom := 0;
  Result.Left := 0; Result. Right := 0;
  IF ShowTitle THEN BEGIN
    vWholeTitle := Title + TitleTag;
    vTextWidth := uPlotStyles.TPlotStyle(Style).TextWidth[vWholeTitle, PlotImage.Canvas];
    vTextHeight := uPlotStyles.TPlotStyle(Style).TextHeigth[vWholeTitle, PlotImage.Canvas];
    vTextPt.X := ClientRect.Left +
                   ((ClientRect.Right - ClientRect.Left - vTextWidth) DIV 2);
    vTextPt.Y := ClientRect.Top - (vTextHeight);
    IF AVisible THEN Style.DrawText(vTextPt.X, vTextPt.Y,vWholeTitle, PlotImage.Canvas);
    // Return used rect with respect to clientrect
    Result.Top := vTextPt.Y;
    Result.Bottom := Result.Top + vTextHeight;
    Result.Left := vTextPt.X;
    Result.Right := Result.Left + vTextWidth;
  END;

end;


function TPlotRect._GetHasFastSeries: Boolean;
var
  vSeriesList: TFPList;
begin
  // if series in plotrect are fast series, call UpdateSeriesData
  // if series are standard, simply draw the focusrect
  Result := false;
  vSeriesList := Self.SeriesContainedIdx;
  try
    if vSeriesList.Count > 0 then Result := OwnerPlot.Series[PInteger(vSeriesList.Items[0])^].IsFastSeries;
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

procedure TPlotRect._DrawZoomRectPlotImage;
var
  vZoomInfo: TZoomRecord;
  //vTargetRect: TRect;
begin
  //draws on Plotimage directly
  vZoomInfo := OwnerPlot.HIDHandler.ZoomInfo;

  PlotImage.Picture.Bitmap.Canvas.Pen.Mode := pmXor;
  PlotImage.Picture.Bitmap.Canvas.Pen.Color := clGray; //Self.Style.Brush.Color;
  //PlotImage.Picture.Bitmap.Canvas.Pen.Color := _FocusColor(Self.Style.Brush.Color);
  PlotImage.Picture.Bitmap.Canvas.Frame(vZoomInfo.dwOldRect);
  PlotImage.Picture.Bitmap.Canvas.Frame(vZoomInfo.dwNewRect);
end;

procedure TPlotRect._DrawZoomRectDataImage;
var
  vZoomInfo: TZoomRecord;
begin
  // draws on DataImageBuffer during update - do not delete old zoomrect
  vZoomInfo := OwnerPlot.HIDHandler.Zoominfo;

  //PlotImage.Picture.Bitmap.Canvas.Pen.Mode := pmXor;
  FDataImageBuffer.Canvas.Pen.Color := _FocusColor(Self.Style.Brush.Color);
  //FDataImageBuffer.Canvas.Frame(vZoomInfo.dwOldRect);
  PlotImageCoordsToDataRectCoords(Self, vZoomInfo.dwNewRect.TopLeft);
  PlotImageCoordsToDataRectCoords(Self, vZoomInfo.dwNewRect.BottomRight);
  FDataImageBuffer.Canvas.Frame(vZoomInfo.dwNewRect);
end;

    // local, only used for _FillDataArea algorithm
    procedure _ShiftAddPoints(var APointArray: TPointArray; ShiftX, ShiftY: Integer);
    var
      vLoop: Integer;
      vPoints: Integer;
    begin
      vPoints:=length(APointArray);
      setlength(APointArray, 2*vPoints);
      for vLoop := 0 to vPoints-1 do begin
        APointArray[vLoop+vPoints].X := ShiftX + APointArray[vLoop].X;
        APointArray[vLoop+vPoints].Y := ShiftY + APointArray[vLoop].Y;
      end;
    end;


procedure TPlotRect._FillDataArea;
var
  vAxes: array of Integer;
  vPoly: array of TPoint;
  vTotalPoints: TPointArray;
  vAxisCount, vLoop: Integer;
  vdx, vdy: Integer;
  vIndex, vStartIndex: Integer;
  vAngle, vStartAngle, vIndexAngle: Double;
  vLastPt: TPoint;
  vLoopAngle: Integer;
begin
  // generate the polygon
  //1. find out which axes we have
  vAxisCount := 0;
  setlength(vAxes, 0);
  for vLoop := 0 to OwnerPlot.AxisCount - 1 do begin
    IF (OwnerPlot.Axis[vLoop].OwnerPlotRect = Self)  THEN begin
      inc(vAxisCount);
      setlength(vAxes, length(vAxes)+1);
      vAxes[length(vAxes)-1] := vLoop;
    end;
  end;

  if (vAxisCount > 3) or (vAxisCount < 2) then begin
    setlength(vAxes, 0);
    exit; // can fill only 2D or 3D plot areas
  end;

  //2.Package wrap (Jarvis-March algorithm)

  //2.1. find all possible points (8 for 3 axes, 4 for 2 axes)
  setlength(vTotalPoints, 1);
  vTotalPoints[0] := TPlotAxis(OwnerPlot.Axis[vAxes[0]]).DrawOrigin;
  for vLoop:=0 to vAxisCount-1 do begin
    vdx := TPlotAxis(OwnerPlot.Axis[vAxes[(vLoop) MOD vAxisCount]]).VisualParams.ptB.X - TPlotAxis(OwnerPlot.Axis[vAxes[(vLoop) MOD vAxisCount]]).VisualParams.ptA.X ;
    vdy := TPlotAxis(OwnerPlot.Axis[vAxes[(vLoop) MOD vAxisCount]]).VisualParams.ptB.Y - TPlotAxis(OwnerPlot.Axis[vAxes[(vLoop) MOD vAxisCount]]).VisualParams.ptA.Y ;
    _ShiftAddPoints(vTotalPoints, vdx, vdy);
  end;

  //2.2. find start condition (i.e. highest Y value with highest X if Y are equal)
  vLastPt := vTotalPoints[0];
  vIndex:=0;
  for vLoop:=1 to length(vTotalPoints)-1 do begin
    if vTotalPoints[vLoop].Y >= vLastPt.Y then begin
      if vTotalPoints[vLoop].Y > vLastPt.Y then vIndex:=vLoop else begin
        if vTotalPoints[vLoop].X > vLastPt.X then vIndex:=vLoop;
      end;
    end;
    vLastPt := vTotalPoints[vIndex];
  end;
  vStartIndex:=vIndex;

  {$IFDEF DEBUG_2_WRITE}
  for vLoop:=0 to length(vTotalPoints)-1 do begin
    writeln('vTotal[', IntToStr(vLoop),'] = ', IntToStr(vTotalPoints[vLoop].X), ' / ',IntToStr(vTotalPoints[vLoop].Y));
  end;
  {$ENDIF}


  //2.3. init asnd start the algorithm
  setlength(vPoly, 1);
  vPoly[0] := vTotalPoints[vStartIndex]; // our start point
  {$IFDEF DEBUG_2_WRITE} writeln('Poly[0] = ', IntToStr(vPoly[0].X), ' / ',IntToStr(vPoly[0].Y));{$ENDIF}

  vStartAngle:=0;
  vIndexAngle:=2*Pi();
  for vLoop := 0 to length(vTotalPoints)-1 do begin
    vLastPt := vPoly[length(vPoly)-1];
    for vLoopAngle := 0 to length(vTotalPoints)-1 do begin
      if (vLoopAngle = vStartIndex) and (vLoop = 0) then Continue;     // at the first run, we disregard the starting point itself
      if vLastPt.X = vTotalPoints[vLoopAngle].X then vAngle := (Sign(-vTotalPoints[vLoopAngle].Y + vLastPt.Y) * Pi()/2)
      else vAngle:=arctan2(-vTotalPoints[vLoopAngle].Y + vLastPt.Y, vTotalPoints[vLoopAngle].X - vLastPt.X );
      if vAngle < 0 then vAngle:=vAngle+2*Pi();
      if (vAngle > vStartAngle) and (vAngle < vIndexAngle) then begin
        vIndexAngle:=vAngle;
        vIndex:=vLoopAngle;
      end;
    end;
    vStartAngle:=vIndexAngle;     // init new start angle
    vIndexAngle:=2*Pi();          // init indexangle
    setlength(vPoly, length(vPoly)+1);
    vPoly[length(vPoly)-1] := vTotalPoints[vIndex];
    if vIndex = vStartIndex then break;   // we are done, poly is closed
    {$IFDEF DEBUG_2_WRITE}
    write('Poly[', IntToStr(length(vPoly)-1),'] = ', IntToStr(vPoly[length(vPoly)-1].X), ' / ',IntToStr(vPoly[length(vPoly)-1].Y));
    writeln('  angle= ', IntToStr(round(vStartAngle * 180 / Pi())));
    {$ENDIF}
  end;

  // fill Data area
  Self.PlotImage.Canvas.Pen.Style := psClear;
  //Self.PlotImage.Canvas.Pen.Mode := pmNop;
  Self.PlotImage.Canvas.Polygon(vPoly);
  setlength(vPoly, 0);
  setlength(vAxes, 0);
  setlength(vTotalPoints, 0);
end;

function TPlotRect.GetFrameRect: TRect;
var vFrameRect : TRect;
begin
  vFrameRect := PlotImage.ClientRect;
          vFrameRect.Left := PlotImage.ClientRect.Left + BorderAbs.Left
                              + Integer(round((PlotImage.ClientRect.Right - PlotImage.ClientRect.Left) * BorderRel.Left DIV 100));
          vFrameRect.Right := PlotImage.ClientRect.Right - BorderAbs.Right
                              - Integer(round((PlotImage.ClientRect.Right - PlotImage.ClientRect.Left) * BorderRel.Right DIV 100));
          vFrameRect.Top := PlotImage.ClientRect.Top + BorderAbs.Top
                              + Integer(round((PlotImage.ClientRect.Bottom - PlotImage.ClientRect.Top) * BorderRel.Top DIV 100));
          vFrameRect.Bottom := PlotImage.ClientRect.Bottom - BorderAbs.Bottom
                              - Integer(round((PlotImage.ClientRect.Bottom - PlotImage.ClientRect.Top) * BorderRel.Bottom DIV 100));
  IF AutoFrameRect THEN FFrameRect := vFrameRect;
  Result := FFrameRect;
end;

function TPlotRect.GetDataRect: TRect;
begin
  // TODO ? PreCalc?
  Result := FDataRect;
end;


constructor TPlotRect.Create(APlot: TPlot);
begin
  inherited Create(APlot);
  FAxisAutoPlaceFill:=false; //true;
  FAxisConstraints[0] := 1;
  FAxisConstraints[1] := 1; //0.5;
  FAxisConstraints[2] := 1;

  FScreenInvalid:=FALSE;
  FBorderAbs.Left := 0; FBorderAbs.Right := 0;
  FBorderAbs.Top := 0; FBorderAbs.Bottom := 0;
  FBorderRel.Left := 0; FBorderRel.Right := 0;
  FBorderRel.Top := 0; FBorderRel.Bottom := 0;
  //BorderRelMode := FALSE;
  FStyle := uPlotStyles.TPlotStyle.Create;
  FStyle.Pen.Style := psDash;
  FStyle.Pen.Width := 1;
  FStyle.Pen.Color := clBlack;
  FStyle.Font.Size :=8;
  FStyle.Font.Bold := TRUE;
  FStyle.Brush.Style := bsSolid;
  FStyle.Brush.Color := OwnerPlot.BackgroundColor;
  //
  //uPlotClass.TBasePlotRect(Self).PlotImage.Canvas.Brush := Style.Brush;
  PlotImage.Canvas.Brush.Color := Style.Brush.Color;
  PlotImage.Canvas.FillRect(uPlotClass.TBasePlotRect(Self).ClientRect);
  ShowFrame := FALSE;   // TODO: default this to FALSE after development
  ShowTitle := TRUE;   // TODO: default this to FALSE after development
  Title := 'Title of PlotRect';
  TitleTag := '';
  LegendRect := TLegendRect.Create(Self);
  ColorScaleRect := TColorScaleRect.Create(Self);

  FShowLegend:=false; //TRUE;
  FShowColorScale:=false; //TRUE;

  AutoClientRect:=TRUE;
  AutoFrameRect :=TRUE;
  FColorScalePlacement:= plRight;
  FLegendPlacement:= plBottom;
end;

destructor TPlotRect.Destroy;
begin
  FStyle.Destroy;
  FStyle := nil;
  LegendRect.Destroy;
  ColorScaleRect.Destroy;

  IF FBackImage <> nil THEN FBackImage.Free;
  IF FDataImage <> nil THEN FDataImage.Free; //  FreeAndNil(FDataImage);
  IF FBackBMP <> nil THEN FBackBMP.Free; //   FreeAndNil(FBackBMP);

  setlength(FFillPolyPoints, 0);

  inherited Destroy;
end;

procedure TPlotRect.StoreBackGround;
var
  vLoopRow, vLoopLine: Integer;
  vFPColor: TFPColor;
begin
  IF FBackBMP <> nil THEN FreeAndNil(FBackBMP);   // faster cleaning possible ?
  FBackBMP := TBitmap.Create;
  FBackBMP.PixelFormat:=pf32bit;
  FBackBMP.SetSize(Self.Width, Self.Heigth);
  FBackBMP.Canvas.CopyRect(Bounds(0,0, FBackBMP.Width, FBackBMP.Height), PlotImage.Picture.Bitmap.Canvas, Self.FrameRect);

  //_FillDataArea;

  IF FBackImage <> nil then FreeAndNil(FBackImage);
  FBackImage := FBackBMP.CreateIntfImage;
  for vLoopLine:=0 to FBackImage.Height-1 do
    for vLoopRow:=0 to FBackImage.Width-1 do begin
      vFPColor := FBackImage.Colors[vLoopRow, vLoopLine];
      vFPColor.alpha:= $FFFF;
      FBackImage.Colors[vLoopRow, vLoopLine]:=vFPColor;
    end;
end;


{ TLegendRect }
// **************************************************************************
// 06.10.2014 old code with variable placement is broken anyway so we simplify placement options:
// plBottom, plRight glues the ColorScale to the Framerect of Plotrect
// skip placement with respect to plot (only relative to plotrect frame now).
// rework placement of all components in next major rework


procedure TLegendRect.SetFillBackground(const AValue: Boolean);
begin
  if FFillBackground=AValue then exit;
  FFillBackground:=AValue;
end;

procedure TLegendRect.SetAutoShrink(const AValue: Boolean);
begin
  if FAutoShrink=AValue then exit;
  FAutoShrink:=AValue;
end;

procedure TLegendRect.SetOwnerPlotRect(const AValue: TPlotRect);
begin
  if FOwnerPlotRect=AValue then exit;
  FOwnerPlotRect:=AValue;
end;

procedure TLegendRect.SetRectSize(const AValue: TSize);
begin
  FRectSize := AValue;
end;

function TLegendRect.DrawLegend_VER(AVisible, ATransparent: Boolean): TRect;
var
  vLoop: Integer;
  vCaption: String;
  vActualCoord: TPoint;
  vActualRow, vActualCol: Integer;
  vTextHeight: Integer;
  vMaxTextLines, vYstep: integer;
  vSampleWidth : Integer;
  vDrawCanvas: TCanvas;
  vUsedWidth, vTotalWidth, vTotalHeight: Integer;
  vUsedRect, vTargetRect: TRect;
  //vRowWidths: array of Integer;
  //
  vMaxElementWidth: Integer;
  vAvailWidthLine: Integer;
  vColsUsed, vRowsUsed: Integer;
  vDummyText: string;
  vDoShrink: Boolean;
  vSeriesList : TFPList;
const
  c_spacing = 8;
  c_linespread = 1.2;
begin
  vActualCoord := Point(0,0);
  vDoShrink:=false;
  vDummyText:='';
  vColsUsed:=1;
  Result := Rect(0,0,0,0);
  // shrinking criteria:
  // 1. if all tags fit in one line, dont shrink
  // 2. if not, shrink all tags down to cMAXLEGENDCHARS;
  // fill up all available lines
  // if still does not fit, dont care
  //vSeriesList := TList.Create;
  vDrawCanvas := FDrawBMP.Canvas;
  vUsedWidth := 0;
  // size FLegendBMP
  FDrawBMP.SetSize(ClientRect.Right - ClientRect.Left, ClientRect.Bottom - ClientRect.Top);
  // fill background if needed
  IF FillBackground THEN BEGIN
    vDrawCanvas.Brush := Style.Brush;
    vDrawCanvas.FillRect(0,0,ClientRect.Right-ClientRect.Left, ClientRect.Bottom-ClientRect.Top);
  END;
  //
  vCaption:='XX';
  vTextHeight := (Style.TextHeigth[vCaption, vDrawCanvas]);
  // calc maximum lines to display
  vMaxTextLines := round((ClientRect.Bottom - ClientRect.Top) / vTextHeight / c_linespread) - 1;
  IF vMaxTextLines <= 0 THEN exit; //vMaxTextLines := 1;
  vYstep := trunc(vTextHeight * c_linespread + 1); // (ClientRect.Bottom - ClientRect.Top) DIV (vMaxTextLines + 1);
  //

    vSeriesList := OwnerPlotRect.SeriesContainedIdx;
    //with TBasePlotRect(OwnerPlotRect).SeriesContained do try
    try
    IF vSeriesList.Count <= 0 THEN exit;
    // calculate maximum width of all elements without shrinking
    vAvailWidthLine := ClientRect.Right - ClientRect.Left;
    vMaxElementWidth := c_spacing;
    for vLoop:=0 to vSeriesList.Count-1 do begin
       vSampleWidth :=
            TPlotSeries(TBasePlotRect(OwnerPlotRect).OwnerPlot.Series[PInteger(vSeriesList.Items[vLoop])^]).
            DrawLegendSample(vActualCoord.X,vActualCoord.Y, vDrawCanvas, AVisible);
       vCaption := TPlotSeries(TBasePlotRect(OwnerPlotRect).OwnerPlot.Series[PInteger(vSeriesList.Items[vLoop])^]).Caption;
       vUsedWidth := Style.TextWidth[vCaption, vDrawCanvas];

       vMaxElementWidth := max(vMaxElementWidth, 2*c_spacing + vSampleWidth + vUsedWidth);
    end;
    writeln('avail width',IntToStr(vAvailWidthLine));
    writeln('element w  ',IntToStr(vMaxElementWidth));
    // if it fits, we use 1 column, otherwise shrink text
    if (vSeriesList.Count) <= vMaxTextLines then vColsUsed := 1 else vColsUsed := ( (vSeriesList.Count-1) DIV (vMaxTextLines)  ) + 1;
    writeln('cols used: ', IntToStr(vColsUsed));
    IF vColsUsed = 0 then exit;

    if (vColsUsed * vMaxElementWidth) <= vAvailWidthLine then vDoShrink:=false
    else begin
      vDoShrink:=true;
      for vLoop:=0 to cMAXLEGENDCHARS-1 do vDummyText:=vDummyText+'X';
      vUsedWidth := Style.TextWidth[vDummyText, vDrawCanvas];
      vMaxElementWidth := min(vMaxElementWidth, 2*c_spacing + vSampleWidth + vUsedWidth);  // one element, maximum chars
    end;


    // here we have
    // - vColsUsed, used colums
    // - vYStep, distance to next line
    // - vDoShrink, wether we shrink to cMAXLEGENDCHARS;

    vActualCoord := Point(0,0);

    vRowsUsed := ( (vSeriesList.Count-1) DIV vColsUsed ) + 1;
    writeln('rows used: ', IntToStr(vRowsUsed));

      for vLoop :=  0 to (vSeriesList.Count-1) do
      begin
         vActualCol := (((vLoop) MOD vColsUsed) + 1);  // from 1...
         vActualRow := (((vLoop) DIV vColsUsed) + 1);  // from 1...
         vActualCoord.Y := 0 + (vActualRow * vYstep);

         //if vActualCol = 1 then vActualCoord.X := 0;
         vActualCoord.X := 2*c_spacing + (vActualCol-1) * vMaxElementWidth;

         vCaption := uPlotSeries.TPlotSeries(uPlotClass.TBasePlotRect(OwnerPlotRect).OwnerPlot.Series[PInteger(vSeriesList.Items[vLoop])^]).Caption;
         // shorten string if necessary
         if vDoShrink then vCaption := LeftStr(vCaption, cMAXLEGENDCHARS);

         // draw series sample
         vSampleWidth :=
            TPlotSeries(uPlotClass.TBasePlotRect(OwnerPlotRect).OwnerPlot.Series[PInteger(vSeriesList.Items[vLoop])^]).
            DrawLegendSample(vActualCoord.X,vActualCoord.Y, vDrawCanvas, AVisible);
         // draw caption
         vActualCoord.X := vActualCoord.X + vSampleWidth + c_spacing;
         Style.DrawText(vActualCoord.X, vActualCoord.Y - (vTextHeight DIV 2), vCaption,
                              vDrawCanvas);
         vUsedWidth := Style.TextWidth[vCaption, vDrawCanvas];
         // advance X coordinateh
         //vActualCoord.X := vActualCoord.X + vUsedWidth + c_spacing;
      end;

      // now copy FLegendBMP to Plot Canvas    // TODO: should be done in PlotRect later !
      vTotalWidth := vMaxElementWidth * vColsUsed + c_spacing;
      vTotalHeight := (vRowsUsed + 1) * vYstep;

      // used rect in local coordinates FDrawBMP
      vUsedRect.Top := 0;
      vUsedRect.Left := 0;
      vUsedRect.Right := min(vTotalWidth, vAvailWidthLine);
      vUsedRect.Bottom := vTotalHeight; // (ClientRect.Bottom - ClientRect.Top); // (vTotalHeight);
      // show frame if needed
      IF ShowFrame THEN BEGIN
        vDrawCanvas.Pen := Style.Pen;
        vDrawCanvas.Frame(vUsedRect);
      END;

      // Target in PLotimage Coordinates (respect shrinking !)

      with vTargetRect do begin
        Left := ClientRect.Right - min(vTotalWidth, vAvailWidthLine);
        Right:= ClientRect.Right;
        Top  := ClientRect.Top;
        Bottom:= ClientRect.Bottom;
      end;

      IF AVisible THEN begin
        uPlotClass.TBasePlotRect(OwnerPlotRect).PlotImage.Canvas.CopyRect(vTargetRect, vDrawCanvas, vUsedRect);
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
    Result := vUsedRect;
end;

function TLegendRect.DrawLegend_HOR(AVisible, ATransparent: Boolean): TRect;
var
  vLoop: Integer;
  vCaption: String;
  vActualCoord: TPoint;
  vActualRow, vActualCol: Integer;
  vTextHeight: Integer;
  vMaxTextLines, vYstep: integer;
  vSampleWidth : Integer;
  vDrawCanvas: TCanvas;
  vUsedWidth, vTotalWidth, vTotalHeight: Integer;
  vUsedRect, vTargetRect: TRect;
  //vRowWidths: array of Integer;
  //
  vMaxElementWidth: Integer;
  vAvailWidthLine: Integer;
  vColsUsed, vRowsUsed: Integer;
  vDummyText: string;
  vDoShrink: Boolean;
  vSeriesList: TFPList;
const
  c_spacing = 8;
  c_linespread = 1.2;
begin
  vActualCoord := Point(0,0);
  vDoShrink:=false;
  vDummyText:='';
  vColsUsed:=1;
  Result := Rect(0,0,0,0);
  // shrinking criteria:
  // 1. if all tags fit in one line, dont shrink
  // 2. if not, shrink all tags down to cMAXLEGENDCHARS;
  // fill up all available lines
  // if still does not fit, dont care
  //vSeriesList := TList.Create;
  vDrawCanvas := FDrawBMP.Canvas;
  vUsedWidth := 0;
  // size FLegendBMP
  FDrawBMP.SetSize(ClientRect.Right - ClientRect.Left, ClientRect.Bottom - ClientRect.Top);
  // fill background if needed
  IF FillBackground THEN BEGIN
    vDrawCanvas.Brush := Style.Brush;
    vDrawCanvas.FillRect(0,0,ClientRect.Right-ClientRect.Left, ClientRect.Bottom-ClientRect.Top);
  END;
  //
  vCaption:='XX';
  vTextHeight := (Style.TextHeigth[vCaption, vDrawCanvas]);
  // calc maximum lines to display
  vMaxTextLines := round((ClientRect.Bottom - ClientRect.Top) / vTextHeight / c_linespread);
  IF vMaxTextLines <= 0 THEN exit; //vMaxTextLines := 1;
  vYstep := trunc(vTextHeight * c_linespread + 1); // (ClientRect.Bottom - ClientRect.Top) DIV (vMaxTextLines + 1);
  //
    vSeriesList := OwnerPlotRect.SeriesContainedIdx;
    //with TBasePlotRect(OwnerPlotRect).SeriesContained do try
    try
    IF vSeriesList.Count <= 0 THEN exit;
    // calculate maximum width of all elements without shrinking
    vAvailWidthLine := ClientRect.Right - ClientRect.Left;
    vMaxElementWidth := c_spacing;
    for vLoop:=0 to vSeriesList.Count-1 do begin
       vSampleWidth :=
            TPlotSeries(uPlotClass.TBasePlotRect(OwnerPlotRect).OwnerPlot.Series[PInteger(vSeriesList.Items[vLoop])^]).
            DrawLegendSample(vActualCoord.X,vActualCoord.Y, vDrawCanvas, AVisible);
       vCaption := TPlotSeries(uPlotClass.TBasePlotRect(OwnerPlotRect).OwnerPlot.Series[PInteger(vSeriesList.Items[vLoop])^]).Caption;
       vUsedWidth := Style.TextWidth[vCaption, vDrawCanvas];

       vMaxElementWidth := max(vMaxElementWidth, 2*c_spacing + vSampleWidth + vUsedWidth);
    end;
    //writeln('avail width',IntToStr(vAvailWidthLine));
    //writeln('element w  ',IntToStr(vMaxElementWidth));
    // if it fits, we use 1 column, otherwise shrink text
    if (vSeriesList.Count * vMaxElementWidth)  <= vAvailWidthLine then vColsUsed := vSeriesList.Count
    else begin
      vDoShrink:=true;
      for vLoop:=0 to cMAXLEGENDCHARS-1 do vDummyText:=vDummyText+'X';
      vUsedWidth := Style.TextWidth[vDummyText, vDrawCanvas];
      vMaxElementWidth := min(vMaxElementWidth, 2*c_spacing + vSampleWidth + vUsedWidth);  // one element, maximum chars
      vColsUsed := vAvailWidthLine DIV vMaxElementWidth;
    end;
    IF vColsUsed = 0 then exit;

    //writeln('cols used: ', IntToStr(vColsUsed));

    // here we have
    // - vColsUsed, used colums
    // - vYStep, distance to next line
    // - vDoShrink, wether we shrink to cMAXLEGENDCHARS;

    vActualCoord := Point(0,0);

      for vLoop :=  0 to (vSeriesList.Count-1) do
      begin
         vActualRow := (((vLoop) DIV vColsUsed) + 1);  // from 1...
         vActualCoord.Y := 0 + (vActualRow * vYstep);
         vActualCol := (((vLoop) MOD vColsUsed) + 1);  // from 1...

         //if vActualCol = 1 then vActualCoord.X := 0;
         vActualCoord.X := 2*c_spacing + (vActualCol-1) * vMaxElementWidth;

         vCaption := uPlotSeries.TPlotSeries(uPlotClass.TBasePlotRect(OwnerPlotRect).OwnerPlot.Series[PInteger(vSeriesList.Items[vLoop])^]).Caption;
         // shorten string if necessary
         if vDoShrink then vCaption := LeftStr(vCaption, cMAXLEGENDCHARS);

         // draw series sample
         vSampleWidth :=
            TPlotSeries(uPlotClass.TBasePlotRect(OwnerPlotRect).OwnerPlot.Series[PInteger(vSeriesList.Items[vLoop])^]).
            DrawLegendSample(vActualCoord.X,vActualCoord.Y, vDrawCanvas, AVisible);
         // draw caption
         vActualCoord.X := vActualCoord.X + vSampleWidth + c_spacing;
         Style.DrawText(vActualCoord.X, vActualCoord.Y - (vTextHeight DIV 2), vCaption,
                              vDrawCanvas);
         vUsedWidth := Style.TextWidth[vCaption, vDrawCanvas];
         // advance X coordinateh
         //vActualCoord.X := vActualCoord.X + vUsedWidth + c_spacing;
      end;

      // now copy FLegendBMP to Plot Canvas    // TODO: should be done in PlotRect later !
      vTotalWidth := vAvailWidthLine;

      vRowsUsed := ( (vSeriesList.Count-1) DIV vColsUsed ) + 1;

      vTotalHeight := (vRowsUsed + 1) * vYstep;

      // used rect in local coordinates FDrawBMP
      vUsedRect.Top := 0;
      vUsedRect.Left := 0;
      vUsedRect.Right := (vTotalWidth);
      vUsedRect.Bottom := (vTotalHeight);
      // show frame if needed
      IF ShowFrame THEN BEGIN
        vDrawCanvas.Pen := Style.Pen;
        vDrawCanvas.Frame(vUsedRect);
      END;

      // Target in PLotimage Coordinates (respect shrinking !)

      with vTargetRect do begin
        Left := ClientRect.Left + vUsedRect.Left;
        Right:= Left + vTotalWidth;
        Top  := ClientRect.Bottom - vTotalHeight;
        Bottom:= ClientRect.Bottom;
      end;

      IF AVisible THEN begin
        uPlotClass.TBasePlotRect(OwnerPlotRect).PlotImage.Canvas.CopyRect(vTargetRect, vDrawCanvas, vUsedRect);
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
    Result := vUsedRect;
end;

procedure TLegendRect.SetStyle(const AStyle: TPlotStyle);
begin
  IF AStyle = FStyle THEN exit;
  IF FStyle <> nil THEN FreeAndNil(FStyle);
  FStyle := AStyle;
  IF (FStyle <> nil) AND (OwnerPlotRect.OwnerPlot <> nil) THEN OwnerPlotRect.OwnerPlot.Repaint;
end;

function TLegendRect._GetClientRect(APlacement: TRectPlacement): TRect;
var
  vMaxRect: TRect;
const
  cBORDERSPACING = 6;
begin
  // we have coordinates relative to the PlotRect 0-0 canvas coordinates.
  // AnchorAbs is absolute, AnchorRel is in percent of OwnerPlotRect
  //vPlotRect := uPlotClass.TBasePlotRect(OwnerPlotRect).ClientRect;
  { 06.10.14
  vPlotRect := OwnerPlotRect.ClientRect;
  //
  Result.Left := vPlotRect.Left + AnchorAbs.X + round((vPlotRect.Right - vPlotRect.Left)  * AnchorRel.X / 100);
  Result.Right := Result.Left + round((vPlotRect.Right - vPlotRect.Left)  * RectSize.cx / 100);
  Result.Top := vPlotRect.Top + AnchorAbs.Y + round((vPlotRect.Bottom - vPlotRect.Top)  * AnchorRel.Y / 100);
  Result.Bottom := Result.Top + round((vPlotRect.Bottom - vPlotRect.Top)  * RectSize.cy / 100);
  }
  // we start with maximum possible extent = FRameRect
  // cMAXSIZEPERCENT is maximum extent of legend;

  vMaxRect := OwnerPlotRect.FrameRect;
  with vMaxRect do begin
    Left  := Left + cBORDERSPACING;
    Right := Right - cBORDERSPACING;
    Top   := Top + cBORDERSPACING;
    Bottom:= Bottom - cBORDERSPACING;
  end;
  Result := vMaxRect;

  CASE APlacement OF
    plRight:  begin
                Result.Left := vMaxRect.Right - round((cMAXRECTSIZEPERCENT / 100) * (vMaxRect.Right - vMaxRect.Left));
                Result .Bottom := OwnerPlotRect.ClientRect.Bottom;
              end;
    plBottom: begin
                Result.Top := vMaxRect.Bottom - round((cMAXRECTSIZEPERCENT / 100) * (vMaxRect.Bottom - vMaxRect.Top));
                Result .Right := OwnerPlotRect.ClientRect.Right;
              end;
  END;
end;


function TLegendRect.GetClientRect: TRect;
begin
  Result := _GetClientRect(OwnerPlotRect.LegendPlacement);
end;


function TLegendRect.Redraw(AVisible, ATransparent: Boolean): TRect;
begin
  Result := Rect(0,0,0,0);
  IF (not OwnerPlotRect.ShowLegend) THEN exit;
  IF OwnerPlotRect.LegendPlacement = plBottom then
    Result := DrawLegend_HOR(AVisible, ATransparent)
  ELSE Result := DrawLegend_VER(AVisible, ATransparent); // TODO VERT
end;

constructor TLegendRect.Create(AOwnerPlotRect: TPlotRect);
begin
  OwnerPlotRect := AOwnerPlotRect;
  FStyle := uPlotStyles.TPlotStyle.Create;
  Style.Pen.Style := psSolid;
  Style.Pen.Width := 1;
  Style.Pen.Color := clBlack;
  Style.Font.Size:=8;
  Style.Font.Bold:= FALSE;
  Style.Brush.Color := clCream; //     clCream; clNone not working win32 qt
  Style.Brush.Style := bsSolid; // bsClear; // bsSolid;
  ShowFrame := TRUE;   // TODO: default this to FALSE after development
  FRectSize.cx := 25;
  FRectSize.cy := 25;
  FDrawBMP := TBitmap.Create;
  //FDrawBMP.PixelFormat := pf24bit;
  FAutoShrink := TRUE;
  FFillBackground := TRUE;
  //FPlacement := plBottom;
end;

destructor TLegendRect.Destroy;
begin
  FStyle.Destroy;
  FStyle := nil;
  FDrawBMP.Free;
  inherited Destroy;
end;

{ TColorScaleRect }
// ***************************************************************************
// Note: ColorScaleRect uses the axisstyle of the respective axis for axis drawing
// although it could have its own axisstyle defines.

// 06.10.2014 old code with variable placement is broken anyway so we simplify placement options:
// plBottom, plRight glues the ColorScale to the Framerect of Plotrect
// skip placement with respect to plot (only relative to plotrect frame now).
// rework placement of all components in next major rework

procedure TColorScaleRect.SetScaleAxisIndex(const AValue: Integer);
begin
  if FScaleAxisIndex=AValue then exit;
  IF uPlotRect.TPlotRect(uPlotClass.TBasePlotRect(OwnerPlotRect).OwnerPlot.Axis[AValue].OwnerPlotRect).ColorScaleRect = Self
  THEN FScaleAxisIndex:=AValue ELSE FScaleAxisIndex := -1;
end;

function TColorScaleRect.GetClientRect: TRect;
begin
  Result := _GetClientRect(OwnerPlotRect.ColorScalePlacement);
end;

procedure TColorScaleRect.SetColorScaleOrientation(
  const AValue: TAxisOrientation);
begin
  if FColorScaleOrientation=AValue then exit;
  FColorScaleOrientation:=AValue;
end;

procedure TColorScaleRect.SetColorScaleWidth(const AValue: Integer);
begin
  if FColorScaleWidth=AValue then exit;
  FColorScaleWidth:=AValue;
end;

function TColorScaleRect.Redraw(AVisible, ATransparent: Boolean): TRect;
var
  vPtA, vPtB: TPoint;
  vPixelsPerValue: Extended;
  vUsedRect, vTargetRect: TRect;
  vDrawCanvas: TCanvas;
  vLoop : integer;
  vX, vY, vTextX, vTextY: integer;
  vMin, vMax: Extended;
  //vUnitTextSize: Integer;
  vUnitStr: String;
  vSeriesLoop: Integer;
  vUnitStrings: TStrings;
  vUnitHeight, vUnitWidth: Extended;
  vUsedAxisWidth: Integer;
  //vViewRange : TValueRange;
  c2_spacing:integer = 10; // transversal
  vColorScaleVisualParams: TAxisVisualParams;
const
  c_spacing = 10;   // longitudinal
  //c2_spacing = 10; // transversal
  c_LineSpread = 1.2;
begin
  // AutoOrientation:

  IF OwnerPlotRect.ColorScalePlacement = plBottom THEN ColorScaleOrientation := aoHorizontal
  ELSE ColorScaleOrientation := aoVertical;
  // TODO delete property ColorScaleOrientation !


  c2_spacing := c2_spacing + ColorScaleWidth;
  try
  vUnitStrings := TStringList.Create;
 // this works only horizontal
  IF (not OwnerPlotRect.ShowColorScale) THEN exit;
  IF ScaleAxisIndex < 0 THEN exit;

  // we take the visual params from the axis
  vColorScaleVisualParams := uPlotAxis.TPlotAxis(uPlotClass.TBasePlotRect(OwnerPlotRect).OwnerPlot.Axis[ScaleAxisIndex]).VisualParams;
  // no matter if the axis is visible, coloscale visibility has its own property (ShowColorScale)
  vColorScaleVisualParams.Visible := TPlotRect(OwnerPlotRect).ShowColorScale;
  // if we do a automatic axis placement from the plotrect we need a stable width
  // (circular problem because of invibile draw first, then shrink Client, then visible draw)
  // to avoid the circualr size change when subticks are added or not, we always respect the size of subticks in invisible draw
  // (see axis DrawIndicatorAxis)

  vDrawCanvas := FDrawBMP.Canvas;
  // size FLegendBMP
  FDrawBMP.SetSize(Self.ClientRect.Right - Self.ClientRect.Left, Self.ClientRect.Bottom - Self.ClientRect.Top);
  // fill background if needed
  IF FillBackground THEN BEGIN
    vDrawCanvas.Brush := Style.Brush;
    vDrawCanvas.FillRect(0,0,Self.ClientRect.Right-Self.ClientRect.Left, Self.ClientRect.Bottom-Self.ClientRect.Top);
  END;
  //  seriescontained in plotrect --> series
  //  wenn series.axesused  x oder y oder z axis = ScaleAxisIndex
  //  dann vUnitStr := vUnitStr + ', ' + Series.Units[1,2,3]  // TODO: document this, was redone
  vUnitStr:='';
  vUnitHeight:=0;
  vUnitWidth:=0;
  //with TBasePlotRect(OwnerPlotRect).SeriesContained do try
  with OwnerPlotRect.SeriesContainedIdx do try
    IF Count <= 0 THEN exit;
    for vSeriesLoop := 0 to count-1 do begin
      vUnitStr := TPlotSeriesBase(uPlotClass.TBasePlotRect(OwnerPlotRect).OwnerPlot.Series[PInteger(Items[vSeriesLoop])^]).UnitString[ScaleAxisIndex];
      IF vUnitStrings.IndexOf(vUnitStr) < 0 THEN vUnitStrings.Add(vUnitStr);
    end;
  finally
    while Count > 0 do begin
      Dispose(Pinteger(Items[0]));
      Delete(0)
    end;
    Free;
  end;
  //
  IF ColorScaleOrientation = aoVariable THEN ColorScaleOrientation := aoHorizontal; //TODO: only right angles
  //
  vMin := TPlotAxis(uPlotClass.TBasePlotRect(OwnerPlotRect).OwnerPlot.Axis[ScaleAxisIndex]).ViewRange.Min;
  vMin := TPlotAxis(uPlotClass.TBasePlotRect(OwnerPlotRect).OwnerPlot.Axis[ScaleAxisIndex]).ReCalcValue(vMin);
  vMax := TPlotAxis(uPlotClass.TBasePlotRect(OwnerPlotRect).OwnerPlot.Axis[ScaleAxisIndex]).ViewRange.Max;
  vMax := TPlotAxis(uPlotClass.TBasePlotRect(OwnerPlotRect).OwnerPlot.Axis[ScaleAxisIndex]).ReCalcValue(vMax);
  CASE ColorScaleOrientation of
  aoHorizontal: begin
                  // here we can draw a vUnitStr as a whole line
                  vUnitHeight := uPlotStyles.TPlotStyle(uPlotClass.TBasePlotRect(OwnerPlotRect).OwnerPlot.Axis[ScaleAxisIndex].Style).TextHeigth['88', vDrawCanvas]
                   * c_LineSpread * 1;
                  for vLoop := 0 to vUnitStrings.Count-1 do begin
                  vUnitStr := vUnitStrings.Strings[vLoop];
                    IF vLoop < (vUnitStrings.Count-1) THEN vUnitStr := vUnitStr + ', ';
                  end;
                  vUnitWidth := uPlotStyles.TPlotStyle(uPlotClass.TBasePlotRect(OwnerPlotRect).OwnerPlot.Axis[ScaleAxisIndex].Style).TextWidth[vUnitStr, vDrawCanvas];
                  // grid coordinates
                  vPtA.X := c_spacing;
                  vPtB.X := Self.ClientRect.Right - Self.ClientRect.Left - (2*c_spacing) - Integer(round(vUnitWidth));
                  vPtA.Y := c2_spacing;
                  vPtB.Y := c2_spacing;
                  //
                  vPixelsPerValue := (vPtB.X - vPtA.X) /(vMax - vMin);
                  vTextX := vPtB.X + c_spacing;
                  vTextY := vPtB.Y - (round(vUnitHeight) DIV 2);
                  vColorScaleVisualParams.TickAngle := taNegative;
                end;
  aoVertical:   begin
                  // here we have n lines
                  vUnitHeight := uPlotStyles.TPlotStyle(uPlotClass.TBasePlotRect(OwnerPlotRect).OwnerPlot.Axis[ScaleAxisIndex].Style).TextHeigth['88', vDrawCanvas]
                   * c_LineSpread * (vUnitStrings.Count + 0);
                  for vLoop := 0 to vUnitStrings.Count-1 do begin
                    vUnitStr := vUnitStrings.Strings[vLoop];
                    vUnitWidth := Max(vUnitWidth, uPlotStyles.TPlotStyle(uPlotClass.TBasePlotRect(OwnerPlotRect).OwnerPlot.Axis[ScaleAxisIndex].Style).TextWidth[vUnitStr, vDrawCanvas]);
                  end;
                  // grid coordinates
                  vPtA.X := c2_spacing;
                  vPtB.X := c2_spacing; //(( ClientRect.Right - ClientRect.Left - (2*c_spacing) - vUnitWidth;
                  vPtA.Y := (Self.ClientRect.Bottom - Self.ClientRect.Top - c_spacing );
                  vPtB.Y := (2*c_spacing) + round(vUnitHeight);
                  //
                  vPixelsPerValue := (vPtA.Y - vPtB.Y) /(vMax - vMin);
                  //
                  vTextX := vPtB.X - (round(vUnitWidth) DIV 2);
                  vTextY := 0 + round(vUnitHeight);
                  vColorScaleVisualParams.TickAngle := taNegative;
                end;
  END;
  // prepare settings record // TODO: prepare this only at start or at modification !
  vColorScaleVisualParams.DrawCanvas := vDrawCanvas;
  vColorScaleVisualParams.ptA := vPtA;
  vColorScaleVisualParams.ptB := vPtB;
  vColorScaleVisualParams.PixelsPerValue := vPixelsPerValue;
  // take styles from original axis (altjough we could make new ones)
  vColorScaleVisualParams.Style := TAxisStyle(uPlotClass.TPlotAxisBase(uPlotClass.TBasePlotRect(OwnerPlotRect).OwnerPlot.Axis[ScaleAxisIndex]).Style);
  vColorScaleVisualParams.TickStyle := uPlotAxis.TPlotAxis(uPlotClass.TBasePlotRect(OwnerPlotRect).OwnerPlot.Axis[ScaleAxisIndex]).TickStyle;
  vColorScaleVisualParams.SubTickStyle := uPlotAxis.TPlotAxis(uPlotClass.TBasePlotRect(OwnerPlotRect).OwnerPlot.Axis[ScaleAxisIndex]).SubTickStyle;
  // draw routine from axis class
  vUsedAxisWidth := TPlotAxis(uPlotClass.TBasePlotRect(OwnerPlotRect).OwnerPlot.Axis[ScaleAxisIndex]).
    DrawIndicatorAxis(vColorScaleVisualParams, AVisible);
  // draw colored line  and ****************************
  // copy colored line n times (for width)
  CASE ColorScaleOrientation of
  aoHorizontal : begin
                   // draw
                    vY := vPtA.Y - 1;
                    for vLoop := 0 to ((vPtB.X - vPtA.X)-1) do begin
                      vX := vPtA.X + vLoop;
                      vDrawCanvas.Pixels[vX,vY] :=
                         TPlotAxis(uPlotClass.TBasePlotRect(OwnerPlotRect).OwnerPlot.Axis[ScaleAxisIndex]).
                           ValueColor[ uPlotAxis.TPlotAxis(uPlotClass.TBasePlotRect(OwnerPlotRect).OwnerPlot.Axis[ScaleAxisIndex]).ReCalcValueInverse(vMin + (vMax-vMin)*vLoop /(vPtB.X - vPtA.X)) ];
                    end;
                   // copy
                   vUsedRect.Left := vPtA.X;
                   vUsedRect.Top := vY;
                   vUsedRect.Right := vPtB.X;
                   vUsedRect.Bottom :=vY+1;
                   vTargetRect := vUsedRect;

                   for vLoop := 1 to ColorScaleWidth do begin
                   vTargetRect.Top := vUsedRect.Top-vLoop;
                   vTargetRect.Bottom := vUsedRect.Bottom -vLoop;
                     vDrawCanvas.CopyRect(vTargetRect, vDrawCanvas, vUsedRect);
                   end;
                   // calculate BMP rect used
                   vUsedRect.Top := 0;
                   vUsedRect.Left := 0;
                   IF AutoShrink THEN BEGIN
                     vUsedRect.Right := Self.ClientRect.Right - Self.ClientRect.Left;
                     vUsedRect.Bottom := vUsedRect.Top + c2_spacing + vUsedAxisWidth;
                   END ELSE BEGIN
                     vUsedRect.Right := Self.ClientRect.Right - Self.ClientRect.Left;
                     vUsedRect.Bottom := Self.ClientRect.Bottom - Self.ClientRect.Top;
                   END;
                 end;
  aoVertical :   begin
                   // draw
                    vX := vPtA.X - 1;
                    for vLoop := 0 to ((vPtA.Y - vPtB.Y)-1) do begin
                      vY := vPtA.Y - vLoop;
                      vDrawCanvas.Pixels[vX,vY] :=
                         uPlotAxis.TPlotAxis(uPlotClass.TBasePlotRect(OwnerPlotRect).OwnerPlot.Axis[ScaleAxisIndex]).
                           ValueColor[uPlotAxis.TPlotAxis(uPlotClass.TBasePlotRect(OwnerPlotRect).OwnerPlot.Axis[ScaleAxisIndex]).ReCalcValueInverse( vMin + (vMax-vMin)*vLoop /(vPtA.Y - vPtB.Y) )];
                    end;
                   // copy colored line
                   vUsedRect.Top := vPtB.Y;
                   vUsedRect.Left := vX-0;
                   vUsedRect.Bottom := vPtA.Y;
                   vUsedRect.Right :=vX+1;
                   vTargetRect := vUsedRect;

                   for vLoop := 1 to ColorScaleWidth do begin
                   vTargetRect.Left := vUsedRect.Left-vLoop;
                   vTargetRect.Right := vUsedRect.Right -vLoop;
                     vDrawCanvas.CopyRect(vTargetRect, vDrawCanvas, vUsedRect);
                   end;
                   // calculate BMP rect used
                   vUsedRect.Top := 0;
                   vUsedRect.Left := 0;
                   IF AutoShrink THEN BEGIN
                     vUsedRect.Right := vUsedRect.Left + c2_spacing + vUsedAxisWidth; //+ ColorScaleWidth;  // ColorScaleWidth added 29.07.14;
                     vUsedRect.Bottom := Self.ClientRect.Bottom - Self.ClientRect.Top;
                   END ELSE BEGIN
                     vUsedRect.Right := Self.ClientRect.Right - Self.ClientRect.Left;
                     vUsedRect.Bottom := Self.ClientRect.Bottom - Self.ClientRect.Top;
                   END;
                 end;
  end;
  // draw unit text  **********************
  vDrawCanvas.Font := Style.Font;
  //vTextX := vPtB.X + c_spacing;
  //vTextY := vPtB.Y - (uPlotStyles.TPlotStyle(uPlotClass.TBasePlotRect(OwnerPlotRect).OwnerPlot.Axis[ScaleAxisIndex].Style).TextHeigth[vUnitStr, vDrawCanvas] DIV 2);
  // for horizontal
  IF ColorScaleOrientation = aoHorizontal THEN
    vDrawCanvas.TextOut(vTextX,vTextY, vUnitStr);
  // for vertical
  IF ColorScaleOrientation = aoVertical THEN BEGIN
    for vLoop := 0 to vUnitStrings.Count-1 do begin
      vDrawCanvas.TextOut(vTextX,vTextY, vUnitStrings.Strings[vLoop]);
      vUnitStr := vUnitStrings.Strings[vLoop];
      vTextY := vTextY + round(vUnitHeight);
    end;
  END;

  // copy to plot canvas
      // show frame if needed
      IF ShowFrame THEN BEGIN
        vDrawCanvas.Pen := Style.Pen;
        vDrawCanvas.Frame(vUsedRect);
      END;
      vTargetRect := Self.ClientRect;
      vTargetRect.Left := vTargetRect.Right - (vUsedRect.Right - vUsedRect.Left);
      vTargetRect.Top := vTargetRect.Bottom - (vUsedRect.Bottom - vUsedRect.Top);
      IF AVisible THEN uPlotClass.TBasePlotRect(OwnerPlotRect).PlotImage.Canvas.CopyRect(vTargetRect, vDrawCanvas, vUsedRect);
  finally
    vUnitStrings.Free;
  end;
  Result := vUsedRect;
end;

constructor TColorScaleRect.Create(AOwnerPlotRect: TPlotRect);
begin
  inherited Create(AOwnerPlotRect);
  FRectSize.cx := 50; // % of rect
  FRectSize.cy := 100;

  //
  FScaleAxisIndex:=-1;
  ShowFrame:=TRUE;
  //
  FColorScaleWidth := 12;
  FColorScaleOrientation:=aoVertical;
  //FColorScaleOrientation:=aoHorizontal;
  //FPlacement := plRight;
  FAutoShrink := true;
end;

end.
