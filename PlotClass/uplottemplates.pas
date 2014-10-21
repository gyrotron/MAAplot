unit uPlotTemplates;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uPlotClass, uplotrect, uplotaxis, uPlotSeries, uPlotStyles,
  types, Graphics;


type

  ETemplate = class(Exception);

const
  cAxisName : array[0..2] of String = ('X-Axis','Y-Axis','Z-Axis' );


  //procedure TmplPlotRect(APlot: TPlot; ABorder: TBorder; ADimensions: Integer; ALegend: Boolean);
  // we do not generate the axes, only The Plotrect
  function TmplPlotRect(APlot: TPlot; ABorderAbs: TBorder; ABorderRel: TBorder; ALegend: Boolean; AColorScale: Boolean): integer;
  function TmplAxes(APlot: TPlot; APlotRect: TPlotRect; ADimensions: Integer): TList;
  function TmplSeries(APlot: TPlot; AAxisList: TList; ASeriestype: TSeriestype): integer;

implementation

resourcestring
  S_InvalidDimension  = 'Invalid Dimensions.'#13#10+
                        'Please use 2 or 3 dimensions only.';
  S_InvalidType       = 'Invalid Series type'#13#10+
                        'Please do not use BASE class.';
  S_DimensionMismatch  = 'Dimension Mismatch.'#13#10+
                         'Number of axes does not match Seriestype.';


function TmplPlotRect(APlot: TPlot; ABorderAbs: TBorder; ABorderRel: TBorder; ALegend: Boolean; AColorScale: Boolean): integer;
// TODO: add borderel and borderabs
begin
  Result := -1;
  // PlotRect
  with TPlotRect.Create(APlot) do
  begin
    BorderAbs := ABorderAbs;
    BorderRel := ABorderRel;
    ShowLegend:=ALegend;
    ShowColorScale := AColorScale;
    //Style.Brush.Color := clBtnFace;
    Style.Brush.Color := APlot.BackgroundColor;
    Title := 'Title';
    Result := PlotRectIndex;
    //writeln('Result (Index) ', IntToStr(Result));
  end;
end;

function TmplAxes(APlot: TPlot; APlotRect: TPlotRect; ADimensions: Integer): TList;
// delivers: TList containing the axis INDICES
var
  vLoop: integer;
  vViewRange: TValueRange;
  vPoint: TPoint;
  vItem: Pinteger;
  vAxisIndex: integer;
begin
  try
    Result := TList.Create; // to deliver axislist
    for vLoop := 0 to ADimensions-1 do begin     // generate N Axes and set default values
      TPlotAxis.Create(APlot);
      vAxisIndex := (APlot.AxisCount-1);
      with TPlotAxis(APlot.Axis[vAxisIndex]) do
      begin
        OwnerPlotRect := APlotRect;
        AxisLabel := cAxisName[vLoop]; //'
        // default style
        TPlotStyle(Style).Font.Size := 8;
        TPlotStyle(Style).Font.Color := clBlack;
        // axis orientation
        vPoint.X := 0; vPoint.Y := 0;
        DrawOriginRel := vPoint;
        Orientation := aoVariable;
        DrawAngle := round((vLoop * 90) MOD 135); // x=0; y=90; z=45 degrees
        DrawLength := 100;
        CASE vLoop of
          0: AutoMode := lmRelToWidth ;
          1: AutoMode := lmRelToHeight ;
          2: begin
               AutoMode := lmRelToBorder ;
               DrawLength := 50;
             end;
        end;
        IF vLoop = 0 THEN  TickAngle := taNegative;
        // scale and viewrange
        vViewRange.min := -1; vViewRange.max := 1;
        ViewRange := vViewRange;
        LogScale := FALSE;
        // axis number formatting
        NumberFormat := nfEngineeringPrefix;
        //vPoint.X := 50; vPoint.Y := 60;
        //DrawOriginRel := vPoint;
        // add axisindex to result list
        new(vItem);
        try
          vItem^ := vAxisIndex;
          Result.Add(vItem);
        except
           Dispose(vItem); raise;
        end; // add axisindex
      end;
    end;
  except
    FreeAndNil(Result);
  end;
end;


function TmplSeries(APlot: TPlot; AAxisList: TList; ASeriesType: TSeriestype
  ): integer;
var
  vAxesCount: integer;
begin
  Result := -1; // deliver series index of newly generated series
  vAxesCount := AAxisList.Count;
  IF (AAxisList = nil) or (vAxesCount = 0) THEN begin
    exit;
    raise ETemplate.CreateRes(@S_InvalidDimension);
  end;
  CASE ASeriesType of
    stBASE:        begin
                     raise ETemplate.CreateRes(@S_InvalidType);
                     exit;
                   end;
    stPLAIN:       begin
                     IF vAxesCount > 1 THEN begin
                       raise ETemplate.CreateRes(@S_DimensionMismatch);
                       exit;
                     end;
                     TPlotSeries.Create(APlot);
                     APlot.Series[APlot.SeriesCount-1].OwnerAxis :=
                       PInteger(AAxisList.Items[0])^;
                   end;
    stXY:          begin
                     IF vAxesCount <> 2 THEN begin
                       raise ETemplate.CreateRes(@S_DimensionMismatch);
                       exit;
                     end;
                     TXYPlotSeries.Create(APlot);
                     TXYPlotSeries(APlot.Series[APlot.SeriesCount-1]).XAxis :=
                       PInteger(AAxisList.Items[0])^;
                     TXYPlotSeries(APlot.Series[APlot.SeriesCount-1]).YAxis :=
                       PInteger(AAxisList.Items[1])^;
                   end;
    stXYZ:         begin
                     IF vAxesCount <> 3 THEN begin
                       raise ETemplate.CreateRes(@S_DimensionMismatch);
                       exit;
                     end;
                     TXYZPlotSeries.Create(APlot);
                     TXYZPlotSeries(APlot.Series[APlot.SeriesCount-1]).XAxis :=
                       PInteger(AAxisList.Items[0])^;
                     TXYZPlotSeries(APlot.Series[APlot.SeriesCount-1]).YAxis :=
                       PInteger(AAxisList.Items[1])^;
                     TXYZPlotSeries(APlot.Series[APlot.SeriesCount-1]).ZAxis :=
                       PInteger(AAxisList.Items[2])^;
                   end;
    stSPECTRUM:    begin
                     IF vAxesCount <> 2 THEN begin
                       raise ETemplate.CreateRes(@S_DimensionMismatch);
                       exit;
                     end;
                     TXYSpectrumPlotSeries.Create(APlot);
                     TXYSpectrumPlotSeries(APlot.Series[APlot.SeriesCount-1]).XAxis :=
                       PInteger(AAxisList.Items[0])^;
                     TXYSpectrumPlotSeries(APlot.Series[APlot.SeriesCount-1]).YAxis :=
                       PInteger(AAxisList.Items[1])^;
                   end;
    stWF2D:        begin
                     IF vAxesCount <> 3 THEN begin
                       raise ETemplate.CreateRes(@S_DimensionMismatch);
                       exit;
                     end;
                     TXYWFPlotSeries.Create(APlot);
                     TXYWFPlotSeries(APlot.Series[APlot.SeriesCount-1]).XAxis :=
                       PInteger(AAxisList.Items[0])^;
                     TXYWFPlotSeries(APlot.Series[APlot.SeriesCount-1]).YAxis :=
                       PInteger(AAxisList.Items[1])^;
                     TXYWFPlotSeries(APlot.Series[APlot.SeriesCount-1]).ZAxis :=
                       PInteger(AAxisList.Items[2])^;
                   end;
    stWF3D:        begin
                     IF vAxesCount <> 3 THEN begin
                       raise ETemplate.CreateRes(@S_DimensionMismatch);
                       exit;
                     end;
                     TXYWF3DPlotSeries.Create(APlot);
                     TXYWF3DPlotSeries(APlot.Series[APlot.SeriesCount-1]).XAxis :=
                       PInteger(AAxisList.Items[0])^;
                     TXYWF3DPlotSeries(APlot.Series[APlot.SeriesCount-1]).YAxis :=
                       PInteger(AAxisList.Items[1])^;
                     TXYWF3DPlotSeries(APlot.Series[APlot.SeriesCount-1]).ZAxis :=
                       PInteger(AAxisList.Items[2])^;
                   end;
  end;

  Result := APlot.SeriesCount-1;

end;

end.

