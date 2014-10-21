unit uPlotHIDHandler;
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
  Classes, SysUtils, Controls, uPlotClass, math, uPlotUtils, useriesmarkers, uPlotDataTypes, Forms; // ExtCtrls;
{
handels events for Left mouse (+Shift / + CNTRL)  and wheel (+shift) for
- zoom
- pan
- adhocmarker
// fixed implementation: wheel change during adhoc marker on changes active series
}

type

  { TPlotHIDHandler }

  TPlotHIDHandler = class(TPlotHIDHandlerBase)
  private
    FStatesUsed: set of THIDMouseState;
    FZoomState: THIDMouseState;
    FPanState: THIDMouseState;
    FAdHocMarkerState: THIDMouseState;
    FMouseActionActive: THIDAction;
    FAdHocMarkerIndex: Integer;
    FAdHocSeriesIndex: Integer;
    procedure _MarkerChangeActiveSeries(AIncrement: Integer);
  protected
    procedure DoMouseUp(Sender: TObject; Button: TMouseButton;  Shift: TShiftState; X, Y: Integer);
    procedure DoMouseDown(Sender: TObject; Button: TMouseButton;  Shift: TShiftState; X, Y: Integer);
    procedure DoMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure DoMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  public
    constructor Create(AOwnerPlot: TPlot);override;
    destructor Destroy; override;

    procedure GetHIDActionStatesAvail(AHIDAction: THIDAction; out AHIDMouseStates: THIDMouseStates);override;
    function SetHIDAction(AHIDAction: THIDAction; AHIDMouseState: THIDMouseState): Integer; override;
    function GetHIDAction(out AHIDAction: THIDAction; AShiftState: TShiftState; AMouseButton: TMouseButton): Integer;      // action for given state
    function GetHIDAction(out AHIDAction: THIDAction; AShiftState: TShiftState; AWheelDelta: Integer): Integer;      // action for given state
    procedure GetHIDActionState(AHIDAction: THIDAction; out AHIDMouseState: THIDMouseState); override;     // state for given action
  end;

const

  cMouseWheelZoomButtonPan = false; //TRUE;
  cHID_MOUSESTATE_Names: array[mcNone..mcShiftWheel] of String = ('disabled','Mouse left', 'shift + Mouse left', 'ctrl + Mouse left','Mouse wheel', 'shift + Mouse wheel');

implementation

uses
  uPlotSeries, uPlotRect;

{ TPlotHIDHandler }

procedure TPlotHIDHandler._MarkerChangeActiveSeries(AIncrement: Integer);
var
  vNewSeriesIndex: Integer;
  vXYZValue: TXYZValue;
begin
  IF FMouseActionActive <> haAdHocMarker THEN exit;
  IF FOwnerPlot.SeriesCount < 2 THEN exit;
  vNewSeriesIndex := EnsureRange(FAdHocSeriesIndex + AIncrement, 0, FOwnerPlot.SeriesCount-1);
  // exchange the markers
  TPLotSeries(OwnerPlot.Series[FAdHocSeriesIndex]).MarkerContainer.RemoveMarker(TPLotSeries(OwnerPlot.Series[FAdHocSeriesIndex]).MarkerContainer.Marker[FAdHocMarkerIndex]);
  FAdHocSeriesIndex := vNewSeriesIndex;
  FAdHocMarkerIndex := TPLotSeries(OwnerPlot.Series[FAdHocSeriesIndex]).MarkerContainer.AddMarker;
  TPLotSeries(OwnerPlot.Series[FAdHocSeriesIndex]).MarkerContainer.Marker[FAdHocMarkerIndex].MarkerMode := mmFixedXValue;
  TPLotSeries(OwnerPlot.Series[FAdHocSeriesIndex]).MarkerContainer.Marker[FAdHocMarkerIndex].MarkerType := mtValue;
  ScreenToXY(OwnerPlot.Axis[TPLotSeries(OwnerPlot.Series[FAdHocSeriesIndex]).XAxis],OwnerPlot.Axis[TPLotSeries(OwnerPlot.Series[0]).YAxis],
                                             vXYZValue.X, vXYZValue.Y, Point(ZoomInfo.dwNewRect.Left,ZoomInfo.dwNewRect.Bottom)  );
  vXYZValue.Z := 0;
  TPLotSeries(OwnerPlot.Series[FAdHocSeriesIndex]).MarkerContainer.Marker[FAdHocMarkerIndex].FixedValueXYZ := vXYZValue;
  TPLotSeries(OwnerPlot.Series[FAdHocSeriesIndex]).MarkerContainer.UpdateMarker(FAdHocMarkerIndex);

end;

procedure TPlotHIDHandler.DoMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  vRequestedAction: THIDAction;
begin
 // check if a HID action is associated, when yes,
 // -perform action (zoom)
 // -remove AdHocMarker (adhoc marker)
 // .. if no,
 // - remove adhocmarker (if present)
 // finally set activeMouseaction to haNone;
 // writeln('mouse Lup');
  IF FMouseActionActive = haNone THEN exit;
  FZoomInfo.dbZooming:=false;
  OwnerPlot.PlotRect[Zoominfo.PlotRectIndex].Zooming := false;
  GetHIDAction(vRequestedAction, Shift, Button);

  // ccheck pressed mouseaction

  CASE FMouseActionActive OF
    haZoom:        begin
                    if vRequestedAction = haZoom then OwnerPlot.Zoom(ZoomInfo);
                   end;
    haPan:         begin
                    // nothing to to, pan is immediate
                   end;
    haAdHocMarker: begin
                    TPLotSeries(OwnerPlot.Series[FAdHocSeriesIndex]).MarkerContainer.RemoveMarker(TPLotSeries(OwnerPlot.Series[FAdHocSeriesIndex]).MarkerContainer.Marker[FAdHocMarkerIndex]);
                    FAdHocMarkerIndex := -1;
                    TPLotSeries(OwnerPlot.Series[FAdHocSeriesIndex]).MarkerContainer.UpdateMarker(-1);
                   end;
  END;

  FMouseActionActive := haNone;
  //FOnMouseMoveProc := nil;
  OwnerPlot.PlotImage.OnMouseMove:=nil;
end;

procedure TPlotHIDHandler.DoMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  vIndex: Integer;
  vMouseAction: THIDAction;
  vXYZValue: TXYZValue;
begin
  // check if a HID action is associated, when yes,
  // -set mouse action active
  // -connect mousemove

  IF Button <> mbLeft THEN exit;        // remove when btnRight is also used
  //writeln('mouse Ldown');

  IF OwnerPlot.ScrCoordToPlotRect(X, Y, vIndex) = 0 THEN begin
    // TODO: remove dbZooming here and in plotrect
    // add FZoomInfo.dwHIDAction ?
    GetHIDAction(vMouseAction, Shift, Button);
    //writeln('mouseaction: ', IntToStr(ord(vMouseAction)));
    FMouseActionActive := vMouseAction;
    if FMouseActionActive = haNone then exit;

    FZoomInfo.PlotRectIndex := vIndex;
    //writeln('vIndex: ', IntToStr(vIndex), '  Zoominfo.PlotrectIndex = ', IntToStr(ZoomInfo.PlotRectIndex));
    OwnerPlot.PlotImage.OnMouseMove:=Self.OnMouseMove;
    //FOnMouseMoveProc := @DoMouseMove;
    FZoomInfo.dbZooming:=false;
    FZoomInfo.dwNewRect.Left:=X;
    FZoomInfo.dwNewRect.Top:=Y;
    FZoomInfo.dwNewRect.Right:=X;
    FZoomInfo.dwNewRect.Bottom:=Y;
    FZoomInfo.dwOldRect.Left:=X;
    FZoomInfo.dwOldRect.Top:=Y;
    FZoomInfo.dwOldRect.Right:=X;
    FZoomInfo.dwOldRect.Bottom:=Y;

    IF FMouseActionActive = haZoom then begin
      FZoomInfo.dbZooming:=TRUE;
      OwnerPlot.PlotRect[Zoominfo.PlotRectIndex].Zooming := true;
    end ELSE
    IF (FMouseActionActive = haAdHocMarker) then begin
      if (OwnerPlot.SeriesCount > 0) then begin
        FAdHocSeriesIndex := 0;
        FAdHocMarkerIndex := TPLotSeries(OwnerPlot.Series[0]).MarkerContainer.AddMarker;
        TPLotSeries(OwnerPlot.Series[0]).MarkerContainer.Marker[FAdHocMarkerIndex].MarkerMode := mmFixedXValue;
        TPLotSeries(OwnerPlot.Series[0]).MarkerContainer.Marker[FAdHocMarkerIndex].MarkerType := mtValue;
        ScreenToXY(OwnerPlot.Axis[TPLotSeries(OwnerPlot.Series[0]).XAxis],OwnerPlot.Axis[TPLotSeries(OwnerPlot.Series[0]).YAxis],
                                                   vXYZValue.X, vXYZValue.Y, Point(X,Y)  );
        vXYZValue.Z := 0;
        TPLotSeries(OwnerPlot.Series[0]).MarkerContainer.Marker[FAdHocMarkerIndex].FixedValueXYZ := vXYZValue;
        TPLotSeries(OwnerPlot.Series[0]).MarkerContainer.UpdateMarker(FAdHocMarkerIndex);
      end else begin
        FAdHocSeriesIndex := -1;
        FMouseActionActive := haNone;
      end;
    end;

    // TODO: if action = adhoc marker, create marker and remember its index, set for XFixedValue mode and to XValue == Xpos
  end;
end;

procedure TPlotHIDHandler.DoMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  vPos: TPoint;
  vXYZValue: TXYZValue;
begin
  {$IFDEF GTK2}
    Application.ProcessMessages;
  {$ENDIF}
  //NOTE: gtk2 does not stop firing mousemove events, once dragging started

  //writeln('mouse move');
  // exit if no action associated
  IF FMouseActionActive = haNone THEN exit;

  // assure coordinates
  vPos.X := EnsureRange(X,TPlotRect(OwnerPlot.PlotRect[FZoomInfo.PlotRectIndex]).DataRect.Left+1, TPlotRect(OwnerPlot.PlotRect[FZoomInfo.PlotRectIndex]).DataRect.Right );
  vPos.Y := EnsureRange(Y,TPlotRect(OwnerPlot.PlotRect[FZoomInfo.PlotRectIndex]).DataRect.Top, TPlotRect(OwnerPlot.PlotRect[FZoomInfo.PlotRectIndex]).DataRect.Bottom-1 );
  //writeln('X / Y: ', IntToStr(X), ' / ', IntToStr(Y));
  Mouse.CursorPos := OwnerPlot.ClientToScreen(vPos);

  CASE FMouseActionActive OF
    haZoom:        begin
                    FZoomInfo.dwNewRect.Right:=vPos.X;
                    FZoomInfo.dwNewRect.Bottom:=vPos.Y;
                    OwnerPlot.DrawZoomRect(FZoomInfo);
                    FZoomInfo.dwOldRect := FZoomInfo.dwNewRect;
                   end;
    haPan:         begin
                    FZoomInfo.dwNewRect.Right:=vPos.X;
                    FZoomInfo.dwNewRect.Bottom:=vPos.Y;
                    //writeln('rect left old/new ', IntToStr(FZoomInfo.dwOldRect.Left), '/', IntToStr(FZoomInfo.dwNewRect.Left));
                    // pan
                    OwnerPlot.Pan(ZoomInfo);
                    FZoomInfo.dwOldRect := FZoomInfo.dwNewRect;
                   end;
    haAdHocMarker: begin
                     // TODO: to implement
                     // TODO: change relevant series somehow (key or wheel ?)
                     // for now, use first series
                     FZoomInfo.dwNewRect.Right:=vPos.X;
                     FZoomInfo.dwNewRect.Bottom:=vPos.Y;
                    if FAdHocMarkerIndex >= 0 then begin
                      ScreenToXY(OwnerPlot.Axis[TPLotSeries(OwnerPlot.Series[FAdHocSeriesIndex]).XAxis],OwnerPlot.Axis[TPLotSeries(OwnerPlot.Series[FAdHocSeriesIndex]).YAxis],
                                                                 vXYZValue.X, vXYZValue.Y, Point(X,Y)  );   // needs vPos ?
                      vXYZValue.Z := 0;
                      //writeln('adhoc change to X= ', FloatToStrF(vXYZValue.X, ffExponent, 4, 4));
                      TPLotSeries(OwnerPlot.Series[FAdHocSeriesIndex]).MarkerContainer.Marker[FAdHocMarkerIndex].FixedValueXYZ := vXYZValue;
                      TPLotSeries(OwnerPlot.Series[FAdHocSeriesIndex]).MarkerContainer.UpdateMarker(FAdHocMarkerIndex);
                    end;
                   end;
  END;

end;

procedure TPlotHIDHandler.DoMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  //vPos: TPoint;
  //vINside: Boolean;
  vPlotRectIdx: Integer;
  //vCoords: Integer;
  vHIDAction: THIDAction;
  vState: THIDMouseState;
const
  cZFactor = 1.19;
begin
  // fixed series change implementation
  IF FMouseActionActive = haAdHocMarker THEN begin
    _MarkerChangeActiveSeries(1* sign(WheelDelta));
    exit;
  end;

  //writeln('mouse wheel');
  //  writeln('wheeldata: ', IntToStr(WheelDelta));
  //  writeln('mousepos : ', IntToStr(MousePos.X), ' / ', INtToStr(MousePos.Y));

  // check if a HID action is associated, when yes, do it (wheel actions immediate)
  GetHIDAction(vHIDAction, Shift, WheelDelta);
    // leave mouse position at same place !
    // zoom 10%
  //writeln('MO zooming...X/Y ', IntToStr(MousePos.X), '/', IntToStr(MousePos.Y));
  if OwnerPlot.ScrCoordToPlotRect(MousePos.X, MousePos.Y, vPlotRectIdx) <> 0 then exit;
  if (not IsInsideRect(MousePos, TPlotRect(OwnerPlot.PlotRect[vPlotRectIdx]).DataRect)) then exit;
  if vHIDAction <> haZoom then exit;      // only zoom implemented for wheel so far

  GetHIDActionState(vHIDAction, vState);

  // if HIDaction = ShiftWheel then we zoom X AND Y at the same time
  // otherwise if HIDAction = Wheel then we zoom only X (shiftWheel: Y)

  if vState = mcWheel then begin
    if [ssShift] <= Shift then
      OwnerPlot.Zoom(ZoomInfo, MousePos, 1, power(cZFactor, sign(WheelDelta)) )
    else OwnerPlot.Zoom(ZoomInfo, MousePos, power(cZFactor, sign(WheelDelta)), 1 );
  end else  OwnerPlot.Zoom(ZoomInfo, MousePos, power(cZFactor, sign(WheelDelta)), power(cZFactor, sign(WheelDelta)) );


end;

constructor TPlotHIDHandler.Create(AOwnerPlot: TPlot);
begin
  Inherited Create(AOwnerPlot);
  FOnMouseDownProc := @DoMouseDown;
  FOnMouseUpProc:= @DoMouseUp ;
  FOnMouseWheelProc := @DoMouseWheel;
  FOnMouseMoveProc := @DoMouseMove; // nil;  // zuweisung geht nicht
  FStatesUsed := [];
  FZoomState := mcNone;
  FPanState := mcNone;
  FPanState := mcNone;
  FAdHocMarkerIndex := -1;
  FAdHocSeriesIndex := 0;
  //SetHIDAction(haZoom, mcMLeft);
  SetHIDAction(haZoom, mcWheel);
  SetHIDAction(haPan, mcShiftMLeft);
  SetHIDAction(haAdHocMarker, mcCtrlMLeft);
end;

destructor TPlotHIDHandler.Destroy;
begin
  inherited Destroy;
end;

procedure TPlotHIDHandler.GetHIDActionStatesAvail(AHIDAction: THIDAction;
  out AHIDMouseStates: THIDMouseStates);
var
  vStates: THIDMouseStates;
  //vLoop: Integer;
  //vAvailSet: set of THIDMouseState;
  vOldState: THIDMouseState;
begin
  vStates := [mcNone, mcMLeft, mcShiftMLeft, mcCtrlMLeft, mcWheel, mcShiftWheel];
  GetHIDActionState(AHIDAction, vOldState);

 CASE AHIDAction OF
   haZoom:        begin
                    vStates := vStates - FStatesUsed;
                  end;
   haPan, haAdHocMarker:
                  begin
                    vStates := vStates - FStatesUsed - [mcShiftWheel, mcWheel];
                  end;
 END;
 Include(vStates, vOldState);
 AHIDMouseStates := vStates;
end;

function TPlotHIDHandler.SetHIDAction(AHIDAction: THIDAction; AHIDMouseState: THIDMouseState): Integer;
begin
 Result := -1;
 // check present
 if (AHIDMouseState <> mcNone) and (AHIDMouseState in FStatesUsed) then exit;

 CASE AHIDAction OF
   haZoom:        begin
                    Exclude(FStatesUsed, FZoomState);
                    FZoomState := AHIDMouseState;
                    Result := 0;
                  end;
   haPan:         begin
                    Exclude(FStatesUsed, FPanState);
                    FPanState := AHIDMouseState;
                    Result := 0;
                  end;
   haAdHocMarker: begin
                    Exclude(FStatesUsed, FAdHocMarkerState);
                    FAdHocMarkerState := AHIDMouseState;
                    Result := 0;
                  end;
 END;
 if (Result = 0) and (AHIDMouseState <> mcNone) then Include(FStatesUsed, AHIDMouseState);
end;

function TPlotHIDHandler.GetHIDAction(out AHIDAction: THIDAction;
  AShiftState: TShiftState; AMouseButton: TMouseButton): Integer;
var
  vPressedMouseState, vMouseState: THIDMouseState;
  vLoop: Integer;
begin
 Result := 0;
 //writeln('GetHIDaction') ;
  // convert to HIDmousestate
 vMouseState := mcNone;
 vPressedMouseState := mcNone;
 IF AMouseButton <> mbLeft then begin
   AHIDAction := haNone;
   exit;
 end;
 //IF AShiftState <= [ssCtrl] THEN vPressedMouseState := mcCtrlMLeft else
 //IF AShiftState <= [ssShift] THEN vPressedMouseState := mcShiftMLeft else
 //IF AShiftState <= [] THEN vPressedMouseState := mcMLeft;

 IF [ssCtrl] <= AShiftState THEN vPressedMouseState := mcCtrlMLeft else
 IF [ssShift] <= AShiftState THEN vPressedMouseState := mcShiftMLeft else
 IF [] <= AShiftState THEN vPressedMouseState := mcMLeft;

 //writeln('vPressedMouseState: ', IntToStr(ord(vPressedMouseState)));
 // check if there is an associated HID action
 for vLoop := ord(haNone) to ord(haAdHocMarker) do begin
   GetHIDActionState(THIDAction(vLoop), vMouseState);
   if vMouseState = vPressedMouseState then begin
     Result := vLoop;
     break;
   end;
 end;
 AHIDAction := THIDAction(Result);
 //writeln('AHIDAction: ', IntToStr(ord(AHIDAction)));
end;

function TPlotHIDHandler.GetHIDAction(out AHIDAction: THIDAction;
  AShiftState: TShiftState; AWheelDelta: Integer): Integer;
var
  vMouseState: THIDMouseState;
  vLoop: Integer;
begin
 // we want wheel actions with and without shift, so respect both

 Result := 0;
  // convert to HIDmousestate
 vMouseState := mcNone;
 //IF AShiftState = [] THEN vPressedMouseState := mcWheel;
 //IF AShiftState = [ssShift] THEN vPressedMouseState := mcShiftWheel;
 // check if there is an associated HID action
 for vLoop := ord(haNone) to ord(haAdHocMarker) do begin
   GetHIDActionState(THIDAction(vLoop), vMouseState);
   if (vMouseState = mcWheel) or (vMouseState = mcShiftWheel) then begin
     Result := vLoop;
     break;
   end;
 end;
 AHIDAction := THIDAction(Result);
end;


procedure TPlotHIDHandler.GetHIDActionState(AHIDAction: THIDAction; out
  AHIDMouseState: THIDMouseState);
begin
 CASE AHIDAction OF
   haNone:        AHIDMouseState := mcNone;
   haZoom:        AHIDMouseState := FZoomState;
   haPan:         AHIDMouseState := FPanState;
   haAdHocMarker: AHIDMouseState := FAdHocMarkerState;
 END;
end;

end.

