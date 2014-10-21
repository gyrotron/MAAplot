unit uPlotClass;

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

{$DEFINE GUIsupport}  // you won't be happy without the context menu....

// This is MAAplot, a Math and Audio Plot Program
// version 141020
// also known as Multiple Acronym Advanced Plot or anything else starting with M A A....

{
What it is :   Pascal classes for plotting data, especially 2D measurement data
               - Fast modes for 2D waterfall and 3D waterfall and spectrum display.
               - for use with Lazarus / fpc

Author         Stefan Junghans (C) 2010-2014
Credits        Stephan Schamberger (first concept and support)
//
Description:   Plots data series into axis grid.
               How to use:
               1. Establish Plot
               2. Establish a PlotRect (or more)
               3. Establish Axis (or more) and connect it to OwnerPlotRect
               4. Create a Series and give it axes (2 or 3)
TPLot:         Container for PlotRects, Axes and Series.
PlotRects:     Logical Rects within the Plot area (making plot-in-plot plossible if needed)
               Could be regarded as a container for axes and series belonging together.
               Used for drawing calculations.
Axis:          Logical Axis with arbitrary origin and vector (draws itself)
               Axes have also labels drawn next to it.
               Cloneaxes could be drawn at other coordinates for indication.
               Axes are given a PlotRect as owner.
Series:        Currently 2D and 3D Series are implemented which need 2 or 3 axes
               Series could have units of measure (Text) drawn next to the used axis.
               Fast plotting series for 2D and 3D waterfalls and 2D spectra.
               Autoscaling is done on request (AutoScaleSeries(x))
               - TPLotSeries:    XY Data, plotted to the plot image - no data is kept
               - TXYPlotSeries:  XY Data, plotted to the plot image - data is kept for redraw
               - TXYZPlotSeries: XYZ data, otherwise as XYSeries
               - TSpectrumPlotSeries: like XYSeries but plots inderectly via a TLazIntfImage (fast)
               - TXYWFPlotSeries: like XYZSeries but plots inderectly via a TLazIntfImage (fast)
                                  Y axis is colored (and not shown otherwise), Z axis is usually time
                                  Series rolls down one line with every new dataline added (waterfall)
               - TXYWFPlotSeries: like TXYWFSeries but Y axis is respected.
                                  Shifting is upwards AND to the right giving a 2.5D effect.
               Series get axes (2 or 3).

Legends:       Part of a PlotRect showing the series names
ColorScales:   Part of a PlotRect showing a colorscale (if values are coloured)

MarkerContainer:
               Container for Markers within a series.
Marker:        Element of a markercontainer, marks something like maxpeak

Constraints:
- Axes must be scaled from low to high value
- ColorScales and Legends shall be horizonal or vertical
//
License:       no public license, except otherwise stated with the bundle you received
               inquiries to  >> MAAplot@junghans-electronics.de <<
}

{ bad bugs:
- ?? whats that ?? (Linux)
- GamesOS: issue with FPU exceptionmask: FPU exceptions (like logn(-x) or x/0) lead to exceptions
           on linux we check for exceptions OR we evaluate the result for NaN or inf
           on windows this does not work.
           If you find some ramaining EDIV0 error or the like, please catch it manually (like if n <> 0 then result := x/n) }

{ minor bugs:
- some properties not needed, unused or deprecated - delete these }

{ compatibility
- Lazarus 0.9.26 .. 1.2.4 (latest testing with Lazarus 1.2.4)
- Linux 3.x.y (i.e. Ubuntu 12.04. and 14.04) with Qt4 (also GTK2, needs some tweaks in uPlotHIDHandler and Markerdrawer)
- Windows XP, 7
}

{ Future improvements:
- Separate data from series drawer so the drawer can be changed without pushing the data again
  (currently a series holds all the data to be able to redraw itself).
- Seaparete screen coordinates from internal coordinates to enable arbitrary transforms
- Interpolation also for 3D and more interpolation modes added to simple "linear".
- Value interpolation instead of PixelInterpolation as implemented
- Unified Datatype for plotting (i.e. MAAResult)
- Improved unit structure and more modularizations
- Better exception handling, especially for win32
}
{ Future additions:
- More series like Smith Charts, polar charts
}

{ This component consists of the following units:
- uPlotClass:         base classes (TPLot holds a PLotImage = TImage where the display is presented) *
- uPLotDataTypes:     basic datatypes
- uPlotRect:          plotrect, the container for axes *
- uPlotAxis:          axes (to be used by the series as X, Y, Z axis...) *
- uPlotSeries:        data series (stores the data for autonomous redrawing)
- uPlotStyles:        points, lines etc.; actual drawing is done here
- uPlotTemplates:     to ease your life creating plotrects, series and axes, otherwise not needed
- uSeriesMarkers:     data storage and data evaluation for markers (like peakmarkers)
- uMarkerDrawer:      class for drawing the markers
- uPlotUtils:         math helpers
- uPLotInterpolator:  connect points with lines (i.e. interpolate between points on a pixel base)
- uPLotOverrides:     LazIntfImage changes and additions
- uPlot_ResultWriter: Export data (exports the values, please use ResultReader to read again, stand alone reader available)
- uPlotForms:         Helper forms (used with context menu)
- uPlotHIDhandler:    Mouse (and keyboard) input handler for Zoom, Pan and AdHoc markers

Along with your copy you should have received
- a license
- a brief user documentation

Please submit feature requests, bug requests and comments to MAAplot@junghans-electronics.de
}

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  SysUtils, Classes, Types, Controls, ExtCtrls, Graphics, math, Menus,
  Dialogs, Forms, LCLType, IntfGraphics, FPimage, ExtDlgs, dateutils; //        GraphType, dateutils; // GraphType not needed

type
  EPlot = class(Exception);
  TPlot = class;

  TColorPoint = packed record
    Pt:    TPoint;
    Color: TColor;
  end;

  THIDMouseState = (mcNone, mcMLeft, mcShiftMLeft, mcCtrlMLeft, mcWheel, mcShiftWheel);
  THIDMouseStates = set of THIDMouseState;
  THIDAction = (haNone, haZoom, haPan, haAdHocMarker);

  { TPlotStyleBase }

  { TODO : test documentation
 }
  TPlotStyleBase = class
  protected
    {$HINTS OFF}
    procedure DrawPoint(Pt: TPoint; Canvas: TCanvas); virtual; overload;
    procedure DrawPoint(Pt: TPoint; Canvas: TCanvas; AColor: TColor); virtual; overload;
    procedure DrawPoint(Pt: TPoint; ADataImage: TLazIntfImage); virtual; overload;
    procedure DrawPoint(Pt: TPoint; ADataImage: TLazIntfImage; AColor: TColor); virtual; overload;
    procedure DrawPoint(Pt: TPoint; ADataImage: TLazIntfImage; AFPColor: TFPColor; AAlphaBlend: Boolean = true; AAlphaMergeOnly: Boolean = false); virtual;overload;
    procedure DrawSamplePoint(Pt: TPoint; Canvas: TCanvas; BeginNew: Boolean); virtual;overload;
    {$HINTS ON}
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;


  TAxisOrientation = (aoHorizontal, aoVertical, aoVariable);

  TAutoScaleMode = (asFirstManualThenFit, asFit, asFit125, asFitNext, asFitNextMargined);

  TValueRange = packed record
    min : Extended;
    max : Extended;
  end;

  TSeriesType = (stBASE, stPLAIN, stXY, stXYZ, stSPECTRUM, stWF2D, stWF3D);

  TZoomRecord = record
    dbZooming:     Boolean;
    PlotRectIndex: Integer;
    dwOldRect:     TRect;
    dwNewRect:     TRect;
  end;

  { TBasePlotRect }
  { This is used as a container for axes, legends, colorscales etc. mainly for position calculations}

  TBasePlotRect = class
  private
    FAutoFrameRect: Boolean;
    FAutoRect: Boolean;                             // deprecated
    FOwnerPlot: TPlot;
    FVisible: Boolean;
    FZooming: Boolean;

    function GetBottomLeft: TPoint;
    function GetDataRect: TRect;

    function GetHeigth: integer;
    function GetPlotRectIndex: integer;
    function GetSeriesContainedIdx: TFPList;
    function GetTopLeft: TPoint;
    function GetWidth: integer;
    function GetZooming: Boolean;
    procedure SetAutoFrameRect(const AValue: Boolean);
    procedure SetAutoRect(const AValue: Boolean);
    procedure SetClientRect(const AValue: TRect);
    procedure SetDataRect(AValue: TRect);
    procedure SetFrameRect(const AValue: TRect);
    procedure SetOwnerPlot(const APlot: TPlot);
    procedure SetVisible(const AValue: Boolean);
    procedure SetZooming(AValue: Boolean);
  protected
    FFrameRect: TRect;     // bounds of PlotRect
    FClientRect: TRect;    // axes length calculation (without text etc.)
    FDataRect: TRect;      // Data plot area = max. extent of axes

    function GetClientRect: TRect; virtual;
    function GetFrameRect: TRect; virtual;
    procedure Redraw; virtual;
  public
    constructor Create(OwnerPlot: TPlot); virtual;
    destructor Destroy; override;

    function PlotImage: TImage;
    // ClientRect is inner part of plotrect used for ALL  calculations
    property ClientRect: TRect read GetClientRect write SetClientRect;
    // FrameRect is toal size of plotrect including title, axes-marks, axis label, colorscale and serieslabel (from 12.10.12)
    property FrameRect: TRect read GetFrameRect write SetFrameRect;
    // DataRect is the Data plot area
    property DataRect: TRect read GetDataRect write SetDataRect;
    property AutoClientRect: Boolean read FAutoRect write SetAutoRect;
    property AutoFrameRect: Boolean read FAutoFrameRect write SetAutoFrameRect;
    property Width: integer read GetWidth;
    property Heigth: integer read GetHeigth;
    property TopLeft: TPoint read GetTopLeft;
    property BottomLeft: TPoint read GetBottomLeft;
    property OwnerPlot: TPlot read FOwnerPlot write SetOwnerPlot;
    property Visible: Boolean read FVisible write SetVisible;
    property SeriesContainedIdx: TFPList read GetSeriesContainedIdx;
    property PlotRectIndex: integer read GetPlotRectIndex;
    property Zooming: Boolean read GetZooming write SetZooming;
  end;

  (*
    TPlotAxis Class
    This class will handle the axis operations.
  *)

  { TPlotAxisBase }

  TPlotAxisBase = class
  private
    FOwnerPlot: TPlot;
    FAutoRect: Boolean;                                   // deprecated
    FOwnerPlotRect: TBasePlotRect;
    FVisible: Boolean;
    FStyle: TPlotStyleBase;
    FOrientation: TAxisOrientation;
    FViewRange: TValueRange;
    procedure SetOwnerPlot(APlot: TPlot);
    procedure SetOwnerPlotRect(const AValue: TBasePlotRect);
    procedure SetStyle(AStyle: TPlotStyleBase);
    procedure SetOrientation(Value: TAxisOrientation);
    procedure SetVisible(Value: Boolean);
  protected
    FNetAxisRect: TRect; // used to check for additional space required for labels and other text
    function GetViewRange: TValueRange; virtual;
    procedure SetViewRange(AValue: TValueRange); virtual;
    function GetPixelsPerValue: Extended; virtual;
    function Redraw(ADrawVisible:Boolean): TRect; virtual;
    function PlotImage: TImage;
  public
    constructor Create(OwnerPlot: TPlot); virtual;
    destructor Destroy; override;

    function CheckSize(out ANetAxisRect: TRect): TRect; virtual; // delivers used rect now while ADataRect excludes text
    property OwnerPlot: TPlot read FOwnerPlot write SetOwnerPlot;
    property OwnerPlotRect: TBasePlotRect read FOwnerPlotRect write SetOwnerPlotRect;
    property Style: TPlotStyleBase read FStyle write SetStyle;
    property Orientation: TAxisOrientation read FOrientation write SetOrientation;
    property ViewRange : TValueRange read GetViewRange write SetViewRange;
    property PixelsPerValue : Extended read GetPixelsPerValue;
    property Visible: Boolean read FVisible write SetVisible;
  end;

  (*
    TplotSeries Class
    This class will keep all data (not TPLotSeries) and calculate the current point position and draw
    it using the style and the assigned axis.
  *)

  { TPlotSeriesBase }

  TPlotSeriesBase = class
  private
    FAutoScaleMode: TAutoScaleMode;
    FSeriestype: TSeriestype;
    FOwnerPlot: TPlot;
    FVisible: Boolean;
    FStyle: TPlotStyleBase;
    FAxis: Integer;
    procedure SetAutoScaleMode(AValue: TAutoScaleMode);
    procedure SetOwnerPlot(APlot: TPlot);
    procedure SetStyle(AStyle: TPlotStyleBase);
    procedure SetVisible(Value: Boolean);
  protected
    FIsFastSeries: Boolean;
    FSeriesIndex: Integer;
    procedure Redraw; virtual;

    function PlotImage: TImage;virtual;
    function GetAxesUsed: TList; virtual;
    function GetValueRange(AAxisIndex: Integer): TValueRange; virtual;
    function GetAutoScaleRange(AAxisIndex: Integer): TValueRange; virtual;
    function GetUnitString(AAxisIndex: Integer): ShortString; virtual;
    procedure DrawPoint(Pt: TPoint; Canvas: TCanvas); overload;
    procedure DrawPoint(Pt: TPoint; Canvas: TCanvas; AColor: TColor); overload;
    procedure DrawPoint(Pt: TPoint; ADataImage: TLazIntfImage); overload;
    procedure DrawPoint(Pt: TPoint; ADataImage: TLazIntfImage; AColor: TColor); overload;
    //procedure DrawPoint(Pt: TPoint; ADataImage: TLazIntfImage; AFPColor: TFPColor); overload;
    procedure DrawPoint(Pt: TPoint; ADataImage: TLazIntfImage; AFPColor: TFPColor; AAlphaBlend: Boolean = false; AAlphaMergeOnly: Boolean = false); overload;
    procedure DrawSamplePoint(Pt: TPoint; Canvas: TCanvas; BeginNew: Boolean);

  public

    constructor Create(OwnerPlot: TPlot); virtual;
    destructor Destroy; override;


    procedure DoPlotToImage; virtual;abstract;
    procedure UpdateMarkers(AContainerIndex: Integer);virtual; abstract;

    procedure Clear;virtual;abstract;

    property AxesUsed: TList read GetAxesUsed;  // TODO: better give the index, not the axis
    property ValueRange[AAxisIndex: Integer]: TValueRange read GetValueRange;
    property AutoScaleRange[AAxisIndex: Integer]: TValueRange read GetAutoScaleRange;

    property OwnerPlot: TPlot read FOwnerPlot write SetOwnerPlot;
    property OwnerAxis: Integer read FAxis write FAxis;
    property Style: TPlotStyleBase read FStyle write SetStyle;
    property Visible: Boolean read FVisible write SetVisible;
    property UnitString[AAxisIndex: Integer]: ShortString read GetUnitString;   // TODO: move to uPlotSeries ?
    property SeriesIndex: Integer read FSeriesIndex;// write FSeriesIndex; // TODO: dynamically deliver !!!!!
    property IsFastSeries: Boolean read FIsFastSeries;
    property SeriesType: TSeriestype read FSeriestype;
    property AutoScaleMode: TAutoScaleMode read FAutoScaleMode write SetAutoScaleMode;
  end;


  { THelperFormsBase }
  {used mainly for the context PopUp menu}

  THelperFormsBase = class(TForm)
  private
    FOwnerPlot: TPlot;
    procedure SetOwnerPlot(const APlot: TPlot);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property OwnerPlot: TPlot read FOwnerPlot write SetOwnerPlot;
  end;

  { TPlotMenuHelpers }

  { MenuHelpers are called from the PopupMenu
    ( The PopUpMenu itself is handeled by TPlot )
    MenuHelper Mehods might do some action or evaluation  AND/OR
    they call some HelperForms (see above)
  }
  TPlotMenuHelpers = class
  private
    FFileDialog: TFileDialog;
    FColorDialog: TColorDialog;
    FHelperFormsExport: THelperFormsBase;
    FHelperFormsSeries: THelperFormsBase;
    FHelperFormsPlotRect: THelperFormsBase;
    FHelperFormsAxes: THelperFormsBase;
    FOwnerPlot: TPlot;
    procedure SetOwnerPlot(const AValue: TPlot);
  protected
  public
    constructor Create(AOwnerPlot: TPlot); virtual;
    destructor Destroy; override;

    property OwnerPlot: TPlot read FOwnerPlot;

    procedure EvalMenuPlotOpts(Sender: TObject); // 22.03.13
    procedure EvalMenuSeriesOpts(Sender:TObject);  // markers 22.08.14
    procedure EvalMenuPlotRectOpts(Sender:TObject);
    procedure EvalMenuAxesOpts(Sender:TObject);

    procedure DoMenuPRAutoScale(Sender: TObject);
    procedure DoMenuSeriesScale(ASeriesIndex: Integer);
    procedure DoMenuSeriesColorChoose(ASeriesIndex: Integer);
    procedure DoMenuSeriesStyleChoose(ASeriesIndex: Integer);
    procedure DoMenuSeriesMarkersChoose(ASeriesIndex: Integer);

    procedure DoMenuPlotRectStyleChoose(APlotRectIndex: Integer);

    procedure DoExportImportData(AImport: Boolean; ASeriesIndex: Integer);
  end;

  { TPlotHIDHandlerBase }
  {Catch mouse/keyboard input for Zoom, Pan or AdHoc marker}

  TPlotHIDHandlerBase = class
  private
    function GetOnMouseDownProc: TMouseEvent;
    function GetOnMouseMoveProc: TMouseMoveEvent;
    function GetOnMouseUpProc: TMouseEvent;
    function GetOnMouseWheelProc: TMouseWheelEvent;
    function GetOwnerPlot: TPlot;
    function GetZoomInfo: TZoomRecord;
  protected
    FZoomInfo: TZoomRecord;
    FOwnerPlot: TPLot;
    FOnMouseWheelProc: TMouseWheelEvent;
    FOnMouseUpProc: TMouseEvent;
    FOnMouseDownProc: TMouseEvent;
    FOnMouseMoveProc: TMouseMoveEvent;

  public
    constructor Create(AOwnerPlot: TPLot);virtual;
    destructor Destroy; override;

    procedure GetHIDActionStatesAvail(AHIDAction: THIDAction; out AHIDMouseStates: THIDMouseStates);virtual;abstract;
    function SetHIDAction(AHIDAction: THIDAction; AHIDMouseState: THIDMouseState): Integer; virtual;abstract;
    procedure GetHIDActionState(AHIDAction: THIDAction; out AHIDMouseState: THIDMouseState); virtual;abstract;

    property OwnerPlot: TPlot read GetOwnerPlot;

    property OnMouseDown: TMouseEvent read GetOnMouseDownProc; // GetOnMouseDownProc;
    property OnMouseUp: TMouseEvent read GetOnMouseUpProc;
    property OnMouseMove: TMouseMoveEvent read GetOnMouseMoveProc;
    property OnMouseWheel: TMouseWheelEvent read GetOnMouseWheelProc;

    property ZoomInfo: TZoomRecord read GetZoomInfo;

  end;

  (*
    TPlot class
    This is the main container class.
  *)
  TPlot = class(TWinControl)
  private
    FExportDialog: TSavePictureDialog;
    FImage: TImage;
    FAxis: TList;
    FSeries: TList;
    FPlotRects: TList;
    FMenuHelpers: TPlotMenuHelpers;
    FHIDHandler: TPlotHIDHandlerBase;
    procedure _Clear;
    function _GetImage: TImage;
    // axis properties
    function GetAxisCount: Integer;
    function GetAxis(Index: Integer): TPlotAxisBase;
    // series properties
    function GetSeriesCount: Integer;
    function GetSeries(Index: Integer): TPlotSeriesBase;
    // PlotRects properties
    function GeTBasePlotRectCount: Integer;
    function GeTBasePlotRect(Index: Integer): TBasePlotRect;
  private
    //FDebugTime1, FDebugTime2: TTime;
    FExportPrinterFriedlyColors: Boolean;
    FExportSize: TSize;
    FMPlotRectDown: Integer;
    //FMLeftButtonDown: Boolean; // currently unused - could be used for smart drawing of zoomrects
    FSmartSizing: Boolean;     // if set from outside, series are NOT redrawn for smart Repaint routine...
                               // do this when you have huge waterfalls - set SmartSize when resizing host, reset when finished (i.e. left mouse released)
                               // TODO: is not fully implemented, do not use (just an idea)
    FBackGroundColor: TColor;
    FOnResize: TNotifyEvent;
    FStyle: TPlotStyleBase;
    FTimedRefresh: Boolean;
    procedure DoOnResize(Sender: TObject); reintroduce;
    function GetZoominfo: TZoomRecord;

    procedure SetBackGroundColor(const AValue: TColor);
    procedure SetPopupMenu(const AValue: TPopupMenu);
    procedure SetStyle(const AStyle: TPlotStyleBase);

    procedure DoExportToFilePlot(Sender: TObject);

  protected
    function RegisterPlotObject(Obj: TObject) : Integer; virtual;
    procedure UnregisterPlotObject(Obj: TObject); virtual;
    procedure OnPaintImage(Sender: TObject); experimental;

    property MousePlotRectDown: Integer read FMPlotRectDown;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function ScrCoordToPlotRect(X, Y: Integer; out PlotRectIndex: Integer): Integer;

    // new HID handling 10.09.14
    procedure DrawZoomRect(AZoomInfo: TZoomRecord);
    procedure Zoom(AZoomInfo: TZoomRecord);
    procedure Zoom(AZoomInfo: TZoomRecord; ACenterPoint: TPoint; AFactorX: Extended; AFactorY: Extended);
    procedure Pan(AZoomInfo: TZoomRecord);

    // PopupMenu functions
    procedure AddPMContextItems(AMenu: TObject);
    procedure CheckPMContextItems(AMenu: TObject);
    procedure RemovePMContextItems(AMenu: TObject);

    procedure Repaint; override;
    procedure AutoScaleSeries(ASeriesIndex: Integer; AGrowOnly: Boolean = FALSE);
    procedure AutoScalePlotRect(APlotRectIndex: Integer);

    procedure LockImage(ADoLock: Boolean);
    procedure ExportToFile(AFileName: TFilename);
    procedure ClearAll;                          // attention: removes everything (series, axes, plotrects..)

    procedure ForceRefreshFastPlotRects;

    property PlotImage: TImage read _GetImage;

    property OnResize: TNotifyEvent read FOnResize write FOnResize;
    property BackgroundColor: TColor read FBackGroundColor write SetBackGroundColor;
    property Style: TPlotStyleBase read FStyle write SetStyle;
    property AxisCount: Integer read GetAxisCount;
    property Axis[Index: Integer]: TPlotAxisBase read GetAxis;
    property SeriesCount: Integer read GetSeriesCount;
    property Series[Index: Integer]: TPlotSeriesBase read GetSeries;
    property PlotRectCount: Integer read GeTBasePlotRectCount;
    property PlotRect[Index: Integer]: TBasePlotRect read GeTBasePlotRect;
    property ExportSize: TSize read FExportSize write FExportSize;
    property ExportPrinterFriedlyColors: Boolean read FExportPrinterFriedlyColors write FExportPrinterFriedlyColors;
    property TimedRefresh: Boolean read FTimedRefresh write FTimedRefresh;      // use this for refresh rates > 20..40/sec
    property SmartSizing: Boolean read FSmartSizing write FSmartSizing;         // currently unused
    property HIDHandler: TPlotHIDHandlerBase read FHIDHandler;                  // class for handling mouse/key actions for zoom, pan etc
  end;

const
  c_GLOBALMIN = 1e-30;  // used to avoid SIGFPE, maximum value the component will handle
  c_GLOBALMAX = 1e30;   // and for var init    , minimum value the component will handle
  c_INVALIDCOORDINATE = -16777216;
  c_MAININTERVAL_MAXDIGITS = 10;  // maximum fractional digits for formatting numbers

implementation

uses uPlotStyles, uPlotRect, uPlotUtils, uPlotSeries, uPlotForms, uPlotAxis, uPlotHIDHandler;

resourcestring
  S_InvalidPlotStyle  = 'This is a invalid PlotStyle.'#13#10+
                        'Please use one of uPlotStyles unit.';
  S_InvalidSeries     = 'This is a invalid PlotSeries.'#13#10+
                        'Please use one of uPlotSeries unit.';
  S_UnknownPlotObject = 'Unable to register unknown plot object.';
  //S_MaxSmallerMin     = 'Maximun cannot be smaller than minimum.';
  //S_MinHigherMax      = 'Minimum cannot be higher than maximum.';

{ TPlotHIDHandlerBase }

function TPlotHIDHandlerBase.GetOnMouseDownProc: TMouseEvent;
begin
  Result := FOnMouseDownProc;
end;

function TPlotHIDHandlerBase.GetOnMouseMoveProc: TMouseMoveEvent;
begin
  Result := FOnMouseMoveProc;
end;

function TPlotHIDHandlerBase.GetOnMouseUpProc: TMouseEvent;
begin
  Result := FOnMouseUpProc;
end;

function TPlotHIDHandlerBase.GetOnMouseWheelProc: TMouseWheelEvent;
begin
  Result := FOnMouseWheelProc;
end;

function TPlotHIDHandlerBase.GetOwnerPlot: TPlot;
begin
  Result := FOwnerPlot;
end;

function TPlotHIDHandlerBase.GetZoomInfo: TZoomRecord;
begin
  Result := FZoominfo;
end;

constructor TPlotHIDHandlerBase.Create(AOwnerPlot: TPLot);
begin
  FOwnerPlot := AOwnerPlot;
  FOnMouseDownProc := nil;
  FOnMouseUpProc := nil;
  FOnMouseMoveProc := nil;
  FOnMouseWheelProc := nil;
end;

destructor TPlotHIDHandlerBase.Destroy;
begin
  inherited Destroy;
  FOnMouseDownProc := nil;
  FOnMouseUpProc := nil;
  FOnMouseMoveProc := nil;
  FOnMouseWheelProc := nil;
end;



{ TPlotStyleBase }
// **************************************************************************

{$HINTS OFF}
procedure TPlotStyleBase.DrawPoint(Pt: TPoint; Canvas: TCanvas);
begin
  raise EPlot.CreateRes(@S_InvalidPlotStyle);
end;

procedure TPlotStyleBase.DrawPoint(Pt: TPoint; Canvas: TCanvas; AColor: TColor);
begin
  raise EPlot.CreateRes(@S_InvalidPlotStyle);
end;

procedure TPlotStyleBase.DrawPoint(Pt: TPoint; ADataImage: TLazIntfImage);
begin
  raise EPlot.CreateRes(@S_InvalidPlotStyle);
end;

procedure TPlotStyleBase.DrawPoint(Pt: TPoint; ADataImage: TLazIntfImage;
  AColor: TColor);
begin
  raise EPlot.CreateRes(@S_InvalidPlotStyle);
end;

procedure TPlotStyleBase.DrawPoint(Pt: TPoint; ADataImage: TLazIntfImage;
  AFPColor: TFPColor; AAlphaBlend: Boolean; AAlphaMergeOnly: Boolean = false);
begin
  raise EPlot.CreateRes(@S_InvalidPlotStyle);
end;

procedure TPlotStyleBase.DrawSamplePoint(Pt: TPoint; Canvas: TCanvas; BeginNew: Boolean);
begin
  raise EPlot.CreateRes(@S_InvalidPlotStyle);
end;

constructor TPlotStyleBase.Create;
begin
  inherited Create;
end;

destructor TPlotStyleBase.Destroy;
begin
  inherited Destroy;
end;

{$HINTS ON}

{ TPlotAxisBase }
// **************************************************************************

constructor TPlotAxisBase.Create(OwnerPlot: TPlot);
begin
  FOwnerPlot := OwnerPlot;
  inherited Create;
  FAutoRect := TRUE;
  FViewRange.min := 0;
  FViewRange.max := 100;
  FVisible := TRUE;
  // create default style object
  FStyle := uPlotStyles.TAxisStyle.Create;
  FOrientation := aoHorizontal;
  // register axis in the plot container object
  OwnerPlot.RegisterPlotObject(Self);
  IF OwnerPlot.PlotRect[0] <> nil THEN FOwnerPlotRect := OwnerPlot.PlotRect[0]
  ELSE FOwnerPlotRect := TBasePlotRect.Create(OwnerPlot);

  FNetAxisRect := Rect(0,0,0,0);
end;

destructor TPlotAxisBase.Destroy;
begin
  Visible := FALSE;
  OwnerPlot.UnRegisterPlotObject(Self);
  OwnerPlot := nil; Style := nil;
  inherited;
end;

function TPlotAxisBase.CheckSize(out ANetAxisRect: TRect): TRect;
begin
  ANetAxisRect := Rect(0,0,0,0);
  Result := Self.Redraw(FALSE);
  ANetAxisRect := FNetAxisRect;
end;


function TPlotAxisBase.PlotImage: TImage;
begin Result := OwnerPlot.PlotImage; end;

function TPlotAxisBase.Redraw(ADrawVisible: Boolean): TRect;
var vDrawRect: TRect;
   ptA, ptB: TPoint;
begin
  IF (not Visible) or (not ADrawVisible) THEN exit;

  {IF FAutoRect THEN
    vDrawRect := OwnerPlot.PlotImage.ClientRect
  ELSE vDrawRect := FClientRect;     }
  vDrawRect := OwnerPlotRect.ClientRect;

  case Orientation of
  aoHorizontal : begin
                   ptA.X := vDrawRect.Left;
                   ptA.Y := vDrawRect.Bottom;
                   ptB.X := vDrawRect.Right;
                   ptB.Y := ptA.Y;
                   uPlotStyles.TAxisStyle(Style).DrawLine(ptA, ptB, PlotImage.Canvas);
                 end;
  aoVertical :   begin
                   ptA.X := vDrawRect.Left;
                   ptA.Y := vDrawRect.Bottom;
                   ptB.X := ptA.X;
                   ptB.Y := vDrawRect.Top;
                   uPlotStyles.TAxisStyle(Style).DrawLine(ptA, ptB, PlotImage.Canvas);
                 end;
  end;
  Result := vDrawRect;
end;


procedure TPlotAxisBase.SetOrientation(Value: TAxisOrientation);
begin
  IF Value = FOrientation THEN exit;
  FOrientation := Value;
  OwnerPlot.Repaint;
end;


procedure TPlotAxisBase.SetViewRange(AValue: TValueRange);
begin
  FViewRange.min := AValue.min;
  FViewRange.max := AValue.max;
  IF FViewRange.min >= FViewRange.max THEN BEGIN
    FViewRange.max := FViewRange.min + 1;// c_GLOBALMIN; // c_GlobalMin does not work (too small here ?)
  END;
end;


procedure TPlotAxisBase.SetOwnerPlot(APlot: TPlot);
begin
  IF APlot = FOwnerPlot THEN exit;
  IF FOwnerPlot <> nil THEN
    FOwnerPlot.UnregisterPlotObject(Self); 
  FOwnerPlot := APlot;
  IF FOwnerPlot <> nil THEN
  begin
    FOwnerPlot.RegisterPlotObject(Self); 
    OwnerPlot.Repaint;
  end;
end;

function TPlotAxisBase.GetViewRange: TValueRange;
begin
  Result := FViewRange;
end;

function TPlotAxisBase.GetPixelsPerValue: Extended;
begin
  Result := 1;
end;


procedure TPlotAxisBase.SetOwnerPlotRect(const AValue: TBasePlotRect);
begin
  if FOwnerPlotRect=AValue then exit;
  FOwnerPlotRect:=AValue;
end;

procedure TPlotAxisBase.SetStyle(AStyle: TPlotStyleBase);
begin
  IF AStyle = FStyle THEN exit;
  IF FStyle <> nil THEN FreeAndNil(FStyle);
  FStyle := AStyle;
  IF (FStyle <> nil) AND (OwnerPlot <> nil) THEN OwnerPlot.Repaint;
end;

procedure TPlotAxisBase.SetVisible(Value: Boolean);
begin
  IF Value = FVisible THEN exit;
  FVisible := Value;
  IF FVisible THEN OwnerPlot.Repaint;
end;

{ TPlotSeriesBase }
// **************************************************************************

constructor TPlotSeriesBase.Create(OwnerPlot: TPlot);
begin
  FSeriestype := stBase;
  FIsFastSeries := FALSE;
  FOwnerPlot := OwnerPlot;
  inherited Create;
  FStyle := uPlotStyles.TSeriesStyle.Create;
  uPlotStyles.TSeriesStyle(FStyle).OwnerSeries := Self;
  FVisible := TRUE;
  // register series in the plot container object
  FSeriesIndex := OwnerPlot.RegisterPlotObject(Self);
  FAutoScaleMode := asFitNextMargined; // asFit, asFit125, asFirstManualThenFit, asFitNext;
end;

destructor TPlotSeriesBase.Destroy;
begin
  Visible := FALSE;
  //OwnerPlot.UnRegisterPlotObject(Self); is done with next line Ownerplot:=nil;
  OwnerPlot := nil; Style := nil;
  inherited;
end;

procedure TPlotSeriesBase.DrawPoint(Pt: TPoint; Canvas: TCanvas);
begin Style.DrawPoint(Pt, Canvas); end;

procedure TPlotSeriesBase.DrawPoint(Pt: TPoint; Canvas: TCanvas; AColor: TColor);
begin
  Style.DrawPoint(Pt, Canvas, AColor);
end;

procedure TPlotSeriesBase.DrawPoint(Pt: TPoint; ADataImage: TLazIntfImage);
begin
  Style.DrawPoint(Pt, ADataImage);
end;

procedure TPlotSeriesBase.DrawPoint(Pt: TPoint; ADataImage: TLazIntfImage;
  AColor: TColor);
begin
  Style.DrawPoint(Pt, ADataImage, AColor);
end;

procedure TPlotSeriesBase.DrawPoint(Pt: TPoint; ADataImage: TLazIntfImage;
  AFPColor: TFPColor; AAlphaBlend: Boolean = false; AAlphaMergeOnly: Boolean = false);
begin
  Style.DrawPoint(Pt, ADataImage, AFPColor, AAlphaBlend, AAlphaMergeOnly);
end;

procedure TPlotSeriesBase.DrawSamplePoint(Pt: TPoint; Canvas: TCanvas;
  BeginNew: Boolean);
begin
  Style.DrawSamplePoint(Pt, Canvas, BeginNew);
end;

function TPlotSeriesBase.PlotImage: TImage;
begin Result := OwnerPlot.PlotImage; end;

procedure TPlotSeriesBase.Redraw;
begin
  raise EPlot.CreateRes(@S_InvalidSeries); 
end;

procedure TPlotSeriesBase.SetOwnerPlot(APlot: TPlot);
begin
  IF APlot = FOwnerPlot THEN exit;
  IF FOwnerPlot <> nil THEN
    FOwnerPlot.UnregisterPlotObject(Self); 
  FOwnerPlot := APlot;
  IF FOwnerPlot <> nil THEN
  begin
    FOwnerPlot.RegisterPlotObject(Self);
    OwnerPlot.Repaint;
  end;
end;

procedure TPlotSeriesBase.SetAutoScaleMode(AValue: TAutoScaleMode);
begin
  if FAutoScaleMode=AValue then Exit;
  FAutoScaleMode:=AValue;
end;

function TPlotSeriesBase.GetAutoScaleRange(AAxisIndex: Integer): TValueRange;
begin
  Result.min := -1;
  Result.max := 1; // this is default, TPlotSeriesBase and TPlotSeries do not store values
end;


function TPlotSeriesBase.GetUnitString(AAxisIndex: Integer): ShortString;
begin
  Result:='not available in base class';
end;

function TPlotSeriesBase.GetValueRange(AAxisIndex: Integer): TValueRange;
var
  vResult: TValueRange;
begin
  vResult.min := 0; //c_GLOBALMAX;
  vResult.max := 0; //c_GLOBALMIN;
  Result := vResult;
end;

function TPlotSeriesBase.GetAxesUsed: TList;
var
  vItem : PInteger;
begin
  try
    Result := TList.Create;
    new(vItem);
    try
      vItem^ := OwnerAxis;
      Result.Add(vItem);
    except
       Dispose(vItem); raise;
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

procedure TPlotSeriesBase.SetStyle(AStyle: TPlotStyleBase);
begin
  IF AStyle = FStyle THEN exit;
  IF FStyle <> nil THEN FreeAndNil(FStyle);
  FStyle := AStyle;
  // set OwnerSeries of style if possible
  IF AStyle is uPlotStyles.TSeriesStyle
  THEN uPlotStyles.TSeriesStyle(FStyle).OwnerSeries := Self;
  // repaint if possible
  IF (FStyle <> nil) AND (OwnerPlot <> nil) THEN OwnerPlot.Repaint;
end;

procedure TPlotSeriesBase.SetVisible(Value: Boolean);
begin
  IF Value = FVisible THEN exit;
  FVisible := Value;
  IF FVisible THEN OwnerPlot.Repaint;
end;

{ TPlot }
// **************************************************************************
// **************************************************************************

constructor TPlot.Create(AOwner: TComponent);
begin
  inherited;
  FSmartSizing:=false;
  FTimedRefresh:=FALSE;
  FImage := TImage.Create(Self);
  FImage.Parent := Self;
  FImage.Align := alClient;
  FImage.Visible := TRUE;
  inherited OnResize := @Self.DoOnResize;

  FAxis := TList.Create;
  FSeries := TList.Create;
  FPlotRects := TList.Create;
  // we do NOT create default axes, plotrects or series, just the lists

  FBackgroundColor := clLtGray;

  FHIDHandler := TPlotHIDHandler.Create(Self);
  FImage.OnMouseDown := FHIDHandler.OnMouseDown;
  FImage.OnMouseUp := FHIDHandler.OnMouseUp;
  FImage.OnMouseWheel := FHIDHandler.OnMouseWheel;
  FImage.OnMouseMove := nil;                       //set by HIDHandler if needed

  FIMage.OnPaint:=@OnPaintImage;                   // use this when you do not want to draw to TIMage.Picture.Bitmap.Canvas
                                                   // I noticed no speed improvement when drawing during onpaint, so we still paint to the persistent bitmap
  FExportSize.cx := 1024;
  FExportSize.cy := 768;

  FExportPrinterFriedlyColors := TRUE;

  {$IFDEF GUIsupport}
  FMenuHelpers := TPlotMenuHelpers.Create(Self);
  PopupMenu := TPopupMenu.Create(Self);
  PopupMenu.AutoPopup := TRUE;
  AddPMContextItems(PopupMenu);
  PopupMenu.OnPopup := @CheckPMContextItems;
  {$ENDIF}
end;

destructor TPlot.Destroy;
begin
  _Clear;
  FHIDHandler.Free;
  {$IFDEF GUIsupport}
  FMenuHelpers.Free;
  RemovePMContextItems(PopupMenu);;
  {$ENDIF}

  FSeries.Free; FAxis.Free; FPlotRects.Free;
  //
  FImage.Free;
  inherited;
end;

procedure TPlot.DrawZoomRect(AZoomInfo: TZoomRecord);
begin
  TPlotRect(PlotRect[AZoominfo.PlotRectIndex]).UpdateZoomRect;
end;

procedure TPlot.Zoom(AZoomInfo: TZoomRecord);
var
  vRect: TRect;
begin
  vRect.Left:= Min(AZoomInfo.dwNewRect.Left, AZoomInfo.dwNewRect.Right);
  vRect.Right:= Max(AZoomInfo.dwNewRect.Left, AZoomInfo.dwNewRect.Right);
  vRect.Top:= Min(AZoomInfo.dwNewRect.Bottom, AZoomInfo.dwNewRect.Top);
  vRect.Bottom:= Max(AZoomInfo.dwNewRect.Bottom, AZoomInfo.dwNewRect.Top);
  // zoom PR
  TPlotRect(PlotRect[AZoomInfo.PlotRectIndex]).Zoom(vRect);
end;

procedure TPlot.Zoom(AZoomInfo: TZoomRecord; ACenterPoint: TPoint; AFactorX: Extended; AFactorY: Extended);
begin
  TPlotRect(PlotRect[AZoomInfo.PlotRectIndex]).Zoom(ACenterPoint.X, ACenterPoint.Y, AFactorX, AFactorY);
end;

procedure TPlot.Pan(AZoomInfo: TZoomRecord);
begin
  if (AZoomInfo.PlotRectIndex < PlotRectCount) and (AZoomInfo.PlotRectIndex >= 0) then
    TPlotRect(PlotRect[AZoomInfo.PlotRectIndex]).Pan(-AZoomInfo.dwNewRect.Right + AZoomInfo.dwOldRect.Right, AZoomInfo.dwNewRect.Bottom - AZoomInfo.dwOldRect.Bottom);
end;


procedure TPlot.AddPMContextItems(AMenu: TObject);
// used to populate the PM with the top level items
var
  vItem: TMenuItem;
begin
  IF not (AMenu is TPopupMenu) then exit;
  // auto scale rect
  vItem := TMenuItem.Create(AMenu as TPopupMenu);
  vItem.Caption := 'AutoScale PlotRect';
  vItem.OnClick := @FMenuHelpers.DoMenuPRAutoScale;
  TPopupMenu(AMenu).Items.Add(vItem);

  // Plot settings file export
  vItem := TMenuItem.Create(AMenu as TPopupMenu);
    vItem.Caption := 'Plot ...';
  TPopupMenu(AMenu).Items.Add(vItem);
  vItem := TMenuItem.Create(AMenu as TPopupMenu);
    vItem.Caption := 'Settings';
    vItem.OnClick := @FMenuHelpers.EvalMenuPlotOpts;
      TPopupMenu(AMenu).Items[1].Add(vItem);
  vItem := TMenuItem.Create(AMenu as TPopupMenu);
    vItem.Caption := 'Export to file';
    vItem.OnClick := @FMenuHelpers.EvalMenuPlotOpts;
    TPopupMenu(AMenu).Items[1].Add(vItem);
  vItem := TMenuItem.Create(AMenu as TPopupMenu);
    vItem.Caption := '-';
    TPopupMenu(AMenu).Items[1].Add(vItem);
  vItem := TMenuItem.Create(AMenu as TPopupMenu);
    vItem.Caption := 'Clear all data';
    vItem.OnClick := @FMenuHelpers.EvalMenuPlotOpts;
    TPopupMenu(AMenu).Items[1].Add(vItem);

  // series menu
  vItem := TMenuItem.Create(AMenu as TPopupMenu);
  vItem.Caption := 'Series';
  TPopupMenu(AMenu).Items.Add(vItem);
  // plotrect menu
  vItem := TMenuItem.Create(AMenu as TPopupMenu);
  vItem.Caption := 'PlotRect';
  TPopupMenu(AMenu).Items.Add(vItem);
  // axes menu
  vItem := TMenuItem.Create(AMenu as TPopupMenu);
  vItem.Caption := 'Axes';
  TPopupMenu(AMenu).Items.Add(vItem);

end;

procedure TPlot.CheckPMContextItems(AMenu: TObject);
//TODO: find out number of submenu instead of fixed numer "2"   or "3"
var
  vName: String;
  vLoop: Integer;
  vSubItem, vSubSubItem, vSub3Item: TMenuItem;
  vItem: TMenuItem;
  vItemNumber: Integer;
  vMCoord, vOrigin: TPoint;
begin
  IF not (AMenu is TPopupMenu) then exit;
  vMCoord := Mouse.CursorPos;
  vOrigin := FImage.ClientOrigin;
  ScrCoordToPlotRect(vMCoord.X - vOrigin.X,
    vMCoord.Y-vOrigin.Y, FMPlotRectDown);

  //
  // adjust visibility of items not needed outside plotrects
  vItem := TPopupMenu(AMenu).Items.Find('AutoScale PlotRect');
  IF vItem <> nil THEN
    vItem.Visible := (FMPlotRectDown >= 0);
  vItem := TPopupMenu(AMenu).Items.Find('Series');
  IF vItem <> nil THEN
    vItem.Visible := (FMPlotRectDown >= 0);

  IF FMPlotRectDown < 0 THEN exit;

  // SERIES ------------------------------------------
  // remove subitems SERIES
  vItem := TPopupMenu(AMenu).Items.Find('Series');
  vItemNumber := TPopupMenu(AMenu).Items.IndexOf(vItem);
  IF TPopupMenu(AMenu).ComponentCount > 0 THEN
  while (TPopupMenu(AMenu).Items[vItemNumber].Count > 0) do
    TPopupMenu(AMenu).Items[vItemNumber].Remove(TMenuItem(TPopupMenu(AMenu).Items[vItemNumber].Items[0] ));
  // add subitems manual scale
  with PlotRect[FMPlotRectDown].SeriesContainedIdx do try
  // attach ManualScale to series
     for vLoop := 0 to Count-1 do begin
      // attach one series as subitem
      BEGIN
        vName := TPlotSeries(Series[Pinteger(Items[vLoop])^]).Caption;
        vSubItem := TMenuItem.Create(vItem);
        vSubItem.Caption := IntToStr(Pinteger(Items[vLoop])^) + ' - ' + vName;
        //TPopupMenu(AMenu).Items[vItemNumber].Add(vSubItem);                 // for ADD
        TPopupMenu(AMenu).Items[vItemNumber].Insert(0,vSubItem);
        // attach subsubitems "ManualScale" ------------------------------------
        vSubSubItem := TMenuItem.Create(vSubItem);
        vSubSubItem.Caption := 'Manual scale';
        vSubSubItem.OnClick := @FMenuHelpers.EvalMenuSeriesOpts;
        //TPopupMenu(AMenu).Items[vItemNumber].Items[vLoop].Add(vSubSubItem);   // for ADD
        TPopupMenu(AMenu).Items[vItemNumber].Items[0].Add(vSubSubItem);   // for Insert
        // attach subsubitems "ColorChoose" ------------------------------------
        vSubSubItem := TMenuItem.Create(vSubItem);
        vSubSubItem.Caption := 'Color';
        vSubSubItem.OnClick := @FMenuHelpers.EvalMenuSeriesOpts;
        //TPopupMenu(AMenu).Items[vItemNumber].Items[vLoop].Add(vSubSubItem);   // for ADD
        TPopupMenu(AMenu).Items[vItemNumber].Items[0].Add(vSubSubItem);   // for Insert
        // attach subsubitems "StyleChoose" ------------------------------------
        vSubSubItem := TMenuItem.Create(vSubItem);
        vSubSubItem.Caption := 'Style';
        vSubSubItem.OnClick := @FMenuHelpers.EvalMenuSeriesOpts;
        //TPopupMenu(AMenu).Items[vItemNumber].Items[vLoop].Add(vSubSubItem);   // for ADD
        TPopupMenu(AMenu).Items[vItemNumber].Items[0].Add(vSubSubItem);   // for Insert
        // attach subsubitems "Markers" ------------------------------------   // 22.08.14
        vSubSubItem := TMenuItem.Create(vSubItem);
        vSubSubItem.Caption := 'Markers';
        vSubSubItem.OnClick := @FMenuHelpers.EvalMenuSeriesOpts;
        //TPopupMenu(AMenu).Items[vItemNumber].Items[vLoop].Add(vSubSubItem);   // for ADD
        TPopupMenu(AMenu).Items[vItemNumber].Items[0].Add(vSubSubItem);   // for Insert
        // attach subsubitems "Data"  ------------------------------------------
        vSubSubItem := TMenuItem.Create(vSubItem);
        vSubSubItem.Caption := 'Data';
        //vSubSubItem.OnClick := @FMenuHelpers.EvalMenuSeriesOpts;
        //TPopupMenu(AMenu).Items[vItemNumber].Items[vLoop].Add(vSubSubItem);   // for ADD
        TPopupMenu(AMenu).Items[vItemNumber].Items[0].Add(vSubSubItem);   // for Insert
          // attach sub3item "Data"
          vSub3Item := TMenuItem.Create(vSubItem);
          vSub3Item.Caption := 'Clear';
          vSub3Item.OnClick := @FMenuHelpers.EvalMenuSeriesOpts;
          //TPopupMenu(AMenu).Items[vItemNumber].Items[vLoop].Add(vSubSubItem);   // for ADD
          TPopupMenu(AMenu).Items[vItemNumber].Items[0].Items[TPopupMenu(AMenu).Items[vItemNumber].Items[0].Count-1].Add(vSub3Item);   // for Insert
          // attach sub3item "Data"
          vSub3Item := TMenuItem.Create(vSubItem);
          vSub3Item.Caption := 'Export';
          vSub3Item.OnClick := @FMenuHelpers.EvalMenuSeriesOpts;
          //TPopupMenu(AMenu).Items[vItemNumber].Items[vLoop].Add(vSubSubItem);   // for ADD
          TPopupMenu(AMenu).Items[vItemNumber].Items[0].Items[TPopupMenu(AMenu).Items[vItemNumber].Items[0].Count-1].Add(vSub3Item);   // for Insert
          // attach sub3item "Data"
          vSub3Item := TMenuItem.Create(vSubItem);
          vSub3Item.Caption := 'Import';
          vSub3Item.OnClick := @FMenuHelpers.EvalMenuSeriesOpts;
          //TPopupMenu(AMenu).Items[vItemNumber].Items[vLoop].Add(vSubSubItem);   // for ADD
          TPopupMenu(AMenu).Items[vItemNumber].Items[0].Items[TPopupMenu(AMenu).Items[vItemNumber].Items[0].Count-1].Add(vSub3Item);   // for Insert

        // test case: more than one series in plotrect with non-ongoing numbers
      END;
      //
    end;
  finally
    while Count > 0 do begin
      Dispose(Pinteger(Items[0]));
      Delete(0);
    end;
    Free;
  end;

  // PLOTRECT ------------------------------------------
  // remove subitems plotrect
  vItem := TPopupMenu(AMenu).Items.Find('PlotRect');
  vItemNumber := TPopupMenu(AMenu).Items.IndexOf(vItem);
  IF TPopupMenu(AMenu).ComponentCount > 0 THEN
  while (TPopupMenu(AMenu).Items[vItemNumber].Count > 0) do
    TPopupMenu(AMenu).Items[vItemNumber].Remove(TMenuItem(TPopupMenu(AMenu).Items[vItemNumber].Items[0] ));
  // add subitems
        vName := TPlotRect(PlotRect[FMPlotRectDown]).Title;
        vSubItem := TMenuItem.Create(vItem);
        vSubItem.Caption := IntToStr(FMPlotRectDown) + ' - ' + vName;
        TPopupMenu(AMenu).Items[vItemNumber].Add(vSubItem);
        // attach subsubitems "StyleChoose"
        vSubSubItem := TMenuItem.Create(vSubItem);
        vSubSubItem.Caption := 'Properties';
        vSubSubItem.OnClick := @FMenuHelpers.EvalMenuPlotRectOpts;
        TPopupMenu(AMenu).Items[vItemNumber].Items[0].Add(vSubSubItem);   // TODO: number by code

  // AXES ------------------------------------------
  // remove subitems axes
  vItem := TPopupMenu(AMenu).Items.Find('Axes');
  vItemNumber := TPopupMenu(AMenu).Items.IndexOf(vItem);
  IF TPopupMenu(AMenu).ComponentCount > 0 THEN
  while (TPopupMenu(AMenu).Items[vItemNumber].Count > 0) do
    TPopupMenu(AMenu).Items[vItemNumber].Remove(TMenuItem(TPopupMenu(AMenu).Items[vItemNumber].Items[0] ));

    for vLoop := 0 to AxisCount - 1 do begin
      IF Axis[vLoop].OwnerPlotRect.PlotRectIndex = FMPlotRectDown THEN BEGIN
      vSubItem := TMenuItem.Create(vItem);
      vSubItem.Caption := (IntToStr(vLoop) + ' - ' + TPlotAxis(Axis[vLoop]).AxisLabel );
      vSubItem.OnClick := @FMenuHelpers.EvalMenuAxesOpts;
      TPopupMenu(AMenu).Items[vItemNumber].Add(vSubItem);
      END;
    end;

end;

procedure TPlot.RemovePMContextItems(AMenu: TObject);
begin
  // remove everything
    while (TPopupMenu(AMenu).Items.Count > 0) do begin
      while (TPopupMenu(AMenu).Items[0].Count > 0) do begin
        TPopupMenu(AMenu).Items[0].Remove(TMenuItem(TPopupMenu(AMenu).Items[0].Items[0] ));
      end;
      TPopupMenu(AMenu).Items.Remove(TMenuItem(TPopupMenu(AMenu).Items[0] ));
    end;
end;

procedure TPlot.DoOnResize(Sender: TObject);
begin
  Repaint;
end;

function TPlot.GetZoominfo: TZoomRecord;
begin
  Result := FHIDHandler.ZoomInfo;
end;

procedure TPlot.SetBackGroundColor(const AValue: TColor);
begin
  if FBackGroundColor=AValue then exit;
  FBackGroundColor:=AValue;
end;

procedure TPlot.SetPopupMenu(const AValue: TPopupMenu);
begin
  IF AValue <> nil THEN
    FImage.PopupMenu := AValue;
end;

procedure TPlot.SetStyle(const AStyle: TPlotStyleBase);
begin
  IF AStyle = FStyle THEN exit;
  IF FStyle <> nil THEN FreeAndNil(FStyle);
  FStyle := AStyle;
  IF (FStyle <> nil) THEN Repaint;
end;


function TPlot.ScrCoordToPlotRect(X, Y: Integer; out PlotRectIndex: Integer
  ): Integer;
var
  vPlotX, vPlotY: Integer;
  vLoop: Integer;
  vIndex: Integer;
begin
  Result := -1; PlotRectIndex := -1; vIndex := -1;
  IF PlotRectCount < 1 THEN exit;
  // transform screen to Plot (FImage)
  vPlotX := X - PlotImage.ClientRect.Left;
  vPlotY := Y - PlotImage.ClientRect.Top;
  // check if it is in a PlotRect (from high to low order
  //    so we have highest plotrects respected first for zooming, i.e. they are topmost
  for vLoop := PlotRectCount-1 downto 0 do begin
    IF IsInsideRect(vPlotX, vPlotY, PlotRect[vLoop].ClientRect) THEN BEGIN
      vIndex := vLoop;
      Result := 0;
      break;
    end;
  end;
  PlotRectIndex := vIndex;
end;

procedure TPlot.DoExportToFilePlot(Sender: TObject);
var
  vFreeDialog: Boolean;
  vFileName: TFilename;
  vExtension: String;
begin
  vFreeDialog := FExportDialog = nil;
  if FExportDialog = nil then FExportDialog := TSavePictureDialog.Create(Self);
  try
   FExportDialog.Filter := 'Bitmap|*.bmp|PNG|*.png';
   if FExportDialog.Execute then begin
     vFileName := FExportDialog.FileName;
     vExtension := '.' + FExportDialog.GetFilterExt;
     end;
  finally
    if vFreeDialog then FreeAndNil(FExportDialog);
  end;

  IF UpperCase(vExtension) <> UpperCase(ExtractFileExt(vFileName)) THEN
    vFileName := vFileName + '.' + vExtension;
  ExportToFile(vFileName);
end;

function TPlot.GeTBasePlotRect(Index: Integer): TBasePlotRect;
begin
  IF (Index > PlotRectCount-1) then exit;
  Result := TBasePlotRect(FPlotRects.Items[Index]);
end;

function TPlot.GeTBasePlotRectCount: Integer;
begin Result := FPlotRects.Count; end;

function TPlot.GetAxis(Index: Integer): TPlotAxisBase;
begin
  IF (Index > AxisCount-1) then exit;
  Result := TPlotAxisBase(FAxis.Items[Index]);
end;

function TPlot.GetAxisCount: Integer;
begin Result := FAxis.Count; end;

function TPlot.GetSeries(Index: Integer): TPlotSeriesBase;
begin
  IF (Index > SeriesCount-1) then exit;
  Result := TPlotSeriesBase(FSeries.Items[Index]);
end;

function TPlot.GetSeriesCount: Integer;
begin Result := FSeries.Count; end;

function TPlot.RegisterPlotObject(Obj: TObject): Integer;
begin
  Result := -1;
  IF Obj is TPlotAxisBase THEN
  begin
    Result := FAxis.Add(Obj);
    exit;
  end;

  IF Obj is TPlotSeriesBase THEN
  begin
    Result := FSeries.Add(Obj);
    exit;
  end;

  IF Obj is TBasePlotRect THEN
  begin
    Result := FPlotRects.Add(Obj);
    exit;
  end;

  raise EPlot.CreateRes(@S_UnknownPlotObject);
end;

procedure TPlot.Repaint;
var vLoop: Integer;
begin
  inherited;
  IF not Visible THEN exit;

  DoubleBuffered := TRUE; //false; //TRUE;

   //Mouse
  // smart repainting:
  FImage.Color := clWhite;
  FImage.Picture.Clear;

  //FImage.Picture.Bitmap.PixelFormat:=pf32bit;
  //FImage.Transparent:=false;
  //FImage.Picture.Bitmap.Transparent:=true;

  FImage.Canvas.Brush.Color := BackgroundColor;
  FImage.Canvas.FillRect(FImage.ClientRect);
  FOR vLoop:= 0 TO PlotRectCount-1 do
    PlotRect[vLoop].Redraw;

  FOR vLoop:= 0 TO AxisCount-1 do
    Axis[vLoop].Redraw(TRUE);
  // postpone updates when mouse button is down
  //IF not FSmartSizing then begin

    FOR vLoop:= 0 TO PlotRectCount-1 do
      TPlotRect(PlotRect[vLoop]).StoreBackGround;
    // legend needs to be drawn in plotrect as soon as alpha channel works ?
    //FOR vLoop:= 0 TO PlotRectCount-1 do
    //  uPlotRect.TPlotRect(PlotRect[vLoop]).LegendRect.Redraw(True, False);
    FOR vLoop:= 0 TO SeriesCount-1 do
      Series[vLoop].Redraw;
  //end;
end;

procedure TPlot.AutoScaleSeries(ASeriesIndex: Integer; AGrowOnly: Boolean = FALSE);
var
  vViewRange: TValueRange;
  vAxis: Integer;
begin
  vAxis := TXYPlotSeries(Self.Series[ASeriesIndex]).XAxis;
  vViewRange := (Self.Series[ASeriesIndex]).AutoScaleRange[vAxis];
  if AGrowOnly then begin
    vViewRange.min := math.min(vViewRange.min, Self.Axis[vAxis].ViewRange.min);
    vViewRange.max := math.max(vViewRange.max, Self.Axis[vAxis].ViewRange.max);
  end;
  Self.Axis[vAxis].ViewRange := vViewRange;

  vAxis := TXYPlotSeries(Self.Series[ASeriesIndex]).YAxis;
  vViewRange := (Self.Series[ASeriesIndex]).AutoScaleRange[vAxis];
  if AGrowOnly then begin
    vViewRange.min := math.min(vViewRange.min, Self.Axis[vAxis].ViewRange.min);
    vViewRange.max := math.max(vViewRange.max, Self.Axis[vAxis].ViewRange.max);
  end;
  Self.Axis[vAxis].ViewRange := vViewRange;

  IF Self.Series[ASeriesIndex] is TXYZPlotSeries THEN begin
    vAxis := TXYZPlotSeries(Self.Series[ASeriesIndex]).ZAxis;
    vViewRange := Self.Series[ASeriesIndex].AutoScaleRange[vAxis];
    if AGrowOnly then begin
      vViewRange.min := math.min(vViewRange.min, Self.Axis[vAxis].ViewRange.min);
      vViewRange.max := math.max(vViewRange.max, Self.Axis[vAxis].ViewRange.max);
    end;
    Self.Axis[vAxis].ViewRange := vViewRange;
  end;

  Repaint;
end;

procedure TPlot.AutoScalePlotRect(APlotRectIndex: Integer);
var
  vItem: Pinteger;
  vSeriesList : TFPList;
  vLoop: Integer;
begin
  IF APlotRectIndex < 0 THEN exit;

  vSeriesList := PlotRect[APlotRectIndex].SeriesContainedIdx;
  try
    for vLoop := 0 to vSeriesList.Count-1  do begin
      vItem := Pinteger(vSeriesList.Items[vLoop]);
      AutoScaleSeries(vItem^, (vLoop <> 0));
    end;
    Repaint;
  finally
    while vSeriesList.Count > 0 do begin
      Dispose(Pinteger(vSeriesList.Items[0]));
      vSeriesList.Delete(0);
    end;
    vSeriesList.Free;
  end;
end;

procedure TPlot.LockImage(ADoLock: Boolean);
begin
  IF ADoLock THEN BEGIN
                   Self.PlotImage.Picture.Bitmap.BeginUpdate(TRUE);   // why Canvas only ? difference ?
                   exit;
                 END;
  Self.PlotImage.Picture.Bitmap.EndUpdate(TRUE);
end;

procedure TPlot.ExportToFile(AFileName: TFilename);
var
  vAlign: TAlign;
  vSize: TSize;
  vPNG: TPortableNetworkGraphic;
  vColor_BackGround: TColor;
  vColor_PlotRectTitles: array of TColor;
  vColor_PlotRects: array of TColor;
  vColor_Axes: array of TColor;
  vColor_AxesTicks: array of TColor;
  vColor_AxesSubticks: array of TColor;
  vColor_Series: array of TColor;
  vLoop: Integer;
const
  c_DefaultName = 'PVsim_plot_defaultname.png';
begin
  // --- change colorscheme -------------------------------------
  // --- TODO: implement colorschemes
  IF ExportPrinterFriedlyColors THEN
  BEGIN
    vColor_BackGround := Self.BackgroundColor;
    Self.BackgroundColor := clWhite;

    setlength(vColor_PlotRects, Self.PlotRectCount);
    setlength(vColor_PlotRectTitles, Self.PlotRectCount);
    for vLoop := 0 to PlotRectCount-1 do begin
      vColor_PlotRects[vLoop] := TPlotRect(PlotRect[vLoop]).Style.Color;
      vColor_PlotRectTitles[vLoop] := TPlotRect(PlotRect[vLoop]).Style.Font.Color;
      TPlotRect(PlotRect[vLoop]).Style.Color := clWhite;
      TPlotRect(PlotRect[vLoop]).Style.Font.Color := clBlack;
    end;

    setlength(vColor_Axes, Self.AxisCount);
    setlength(vColor_AxesTicks, Self.AxisCount);
    setlength(vColor_AxesSubticks, Self.AxisCount);
    for vLoop := 0 to AxisCount-1 do begin
      vColor_Axes[vLoop] := TAxisStyle(TPlotAxis(Axis[vLoop]).Style).Color;
      vColor_AxesTicks[vLoop] := TAxisStyle(TPlotAxis(Axis[vLoop]).TickStyle).Color;
      vColor_AxesSubticks[vLoop] := TAxisStyle(TPlotAxis(Axis[vLoop]).SubTickStyle).Color;
      TAxisStyle(TPlotAxis(Axis[vLoop]).Style).Color := clBlack;
      TAxisStyle(TPlotAxis(Axis[vLoop]).TickStyle).Color := clBlack;
      TAxisStyle(TPlotAxis(Axis[vLoop]).SubTickStyle).Color := clBlack;
    end;

    setlength(vColor_Series, Self.SeriesCount);
    for vLoop := 0 to SeriesCount-1 do begin
      vColor_Series[vLoop] := TSeriesStyle(TPlotSeries(Series[vLoop]).Style).Color;
      IF TSeriesStyle(TPlotSeries(Series[vLoop]).Style).Color = clWhite THEN
        TSeriesStyle(TPlotSeries(Series[vLoop]).Style).Color := clBlue;
    end;
  END;

  // ------------------------------------
  // Rescale
  vAlign := Self.Align;
  vSize.cx := Self. Width;
  vSize.cy := Self.Height;
  Self.Align := alNone;
  Self.Width := FExportSize.cx;
  Self.Height := FExportSize.cy;
  Repaint;
  // Export
  IF AFileName = '' THEN AFileName := c_DefaultName;
  // convert to png if wanted
  IF UpperCase(ExtractFileExt(AFileName)) = '.PNG' THEN BEGIN
    vPNG := TPortableNetworkGraphic.Create;
    vPNG.Assign(FImage.Picture.Bitmap);
    vPNG.SaveToFile(AFileName);
    vPNG.Free;
  END ELSE
    FImage.Picture.Bitmap.SaveToFile(AFileName);

  // --- revert colorscheme -------------------------------------
  IF ExportPrinterFriedlyColors THEN
  BEGIN
    Self.BackgroundColor := vColor_BackGround;

    for vLoop := 0 to PlotRectCount-1 do begin
      TPlotRect(PlotRect[vLoop]).Style.Color := vColor_PlotRects[vLoop];
      TPlotRect(PlotRect[vLoop]).Style.Font.Color := vColor_PlotRectTitles[vLoop];
    end;
    setlength(vColor_PlotRects, 0);
    setlength(vColor_PlotRectTitles, 0);

    for vLoop := 0 to AxisCount-1 do begin
      TAxisStyle(TPlotAxis(Axis[vLoop]).Style).Color := vColor_Axes[vLoop];
      TAxisStyle(TPlotAxis(Axis[vLoop]).TickStyle).Color := vColor_AxesTicks[vLoop];
      TAxisStyle(TPlotAxis(Axis[vLoop]).SubTickStyle).Color := vColor_AxesSubticks[vLoop];
    end;
    setlength(vColor_Axes, 0);
    setlength(vColor_AxesTicks, 0);
    setlength(vColor_AxesSubticks, 0);

    for vLoop := 0 to SeriesCount-1 do begin
      TSeriesStyle(TPlotSeries(Series[vLoop]).Style).Color := vColor_Series[vLoop];
    end;
    setlength(vColor_Series, 0);
  END;

  Self.Align := vAlign;
  Self. Width := vSize.cx;
  Self.Height := vSize.cy;
  Repaint;
end;

procedure TPlot.ClearAll;
begin
  _Clear;
  Visible := TRUE;
end;

procedure TPlot.ForceRefreshFastPlotRects;
// TODO: number of series is obsolete as ALL series in one fast plot are updated together (for multitrace)
var
  vLoop: Integer;
begin
  IF SeriesCount < 1 THEN exit;
  FOR vLoop:= 0 TO PlotRectCount-1 do begin
    TPlotRect(PlotRect[vLoop]).UpdateSeriesData(TPlotSeries(Series[0]), FALSE, TRUE); // TODO: series not evaluated ! do not clear waterfalls
  end;
end;

procedure TPlot.UnregisterPlotObject(Obj: TObject);
var
  vDeletedIndex, vLoop: Integer;
begin
  IF Obj is TPlotAxisBase THEN
  begin
    FAxis.Remove(Obj);
    exit;
  end;

  IF Obj is TPlotSeriesBase THEN
  begin
    vDeletedIndex := FSeries.IndexOf(Obj);
    FSeries.Remove(Obj);
    // adjust indices
    IF (FSeries.Count  = 0) THEN exit;
    for vLoop := vDeletedIndex to FSeries.Count - 1 do begin
      TPlotSeriesBase(FSeries.Items[vLoop]).FSeriesIndex := vLoop;
    end;
    exit;
  end;

  IF Obj is TBasePlotRect THEN
  begin
    FPlotRects.Remove(Obj);
    exit;
  end;

  raise EPlot.CreateRes(@S_UnknownPlotObject);
end;

procedure TPlot.OnPaintImage(Sender: TObject);
begin
  // This is dperecated;
  // was used to draw in the OnPaint event of the PlotIMage
  // however there is no speed improve, so we still plot to the Bitmap
  // for now used to check update rate....
  //FDebugTime2:=Now;
  //writeln('OnPaint time: ', MilliSecondsBetween(FDebugTime2, FDebugTime1));
  //FDebugTime1:=FDebugTime2;
end;

procedure TPlot._Clear;
begin
  Visible := FALSE;

  while SeriesCount > 0 do
    Series[SeriesCount-1].Free;

  while AxisCount > 0 do
    Axis[AxisCount-1].Free;

  while PlotRectCount > 0 do
    PlotRect[PlotRectCount-1].Free;
end;

function TPlot._GetImage: TImage;
begin Result := FImage; end;

{ TBasePlotRect }
// **************************************************************************

function TBasePlotRect.GetClientRect: TRect;
begin
  IF AutoClientRect THEN
    Result := PlotImage.ClientRect
  ELSE Result := FClientRect;
end;

procedure TBasePlotRect.Redraw;
begin
  // do nothing since real work is implemented in next class only
end;

function TBasePlotRect.GetPlotRectIndex: integer;
begin
  Result := OwnerPlot.FPlotRects.IndexOf(Self);
end;


function TBasePlotRect.GetSeriesContainedIdx: TFPList;
var
  vLoop1: Integer;
  vItem: PInteger;
begin
  Result := TFPList.Create;
  FOR vLoop1 := OwnerPlot.SeriesCount-1 downto 0 do
  begin
    if TPlotAxis(OwnerPlot.Axis[TPLotSeries(OwnerPlot.Series[vLoop1]).XAxis]).OwnerPlotRect = Self then begin
      New(vItem);
      try
        vItem^ := vLoop1;
        Result.Add(vItem);
      except
        Dispose(vItem);
      end;
    end;
  end;
end;

function TBasePlotRect.GetBottomLeft: TPoint;
begin
  Result.X := FrameRect.Left;
  Result.Y := FrameRect.Bottom;
end;

function TBasePlotRect.GetDataRect: TRect;
begin
  Result := FDataRect;
end;

function TBasePlotRect.GetFrameRect: TRect;
begin
  Result := FFrameRect;
end;

function TBasePlotRect.GetTopLeft: TPoint;
begin
  Result := FrameRect.TopLeft;
end;

function TBasePlotRect.GetWidth: integer;
begin
  Result := FrameRect.Right - FrameRect.Left;
end;

function TBasePlotRect.GetZooming: Boolean;
begin
  Result := FZooming;
end;

function TBasePlotRect.GetHeigth: integer;
begin
  Result := FrameRect.Bottom - FrameRect.Top;
end;

procedure TBasePlotRect.SetAutoFrameRect(const AValue: Boolean);
begin
  if FAutoFrameRect=AValue then exit;
  FAutoFrameRect:=AValue;
  OwnerPlot.Repaint;
end;

procedure TBasePlotRect.SetAutoRect(const AValue: Boolean);
begin
  IF AValue = FAutoRect THEN exit;
  FAutoRect := AValue;
  OwnerPlot.Repaint;
end;

procedure TBasePlotRect.SetClientRect(const AValue: TRect);
begin
  FClientRect := AValue;
  //PlotImage.Repaint;  // TODO: needed repaint ?
end;

procedure TBasePlotRect.SetDataRect(AValue: TRect);
begin
  FDataRect := AValue;
end;

procedure TBasePlotRect.SetFrameRect(const AValue: TRect);
begin
  FFrameRect := AValue;
  PlotImage.Repaint;
end;

procedure TBasePlotRect.SetOwnerPlot(const APlot: TPlot);
begin
  IF APlot = FOwnerPlot THEN exit;
  IF FOwnerPlot <> nil THEN
    FOwnerPlot.UnregisterPlotObject(Self);
  FOwnerPlot := APlot;
  IF FOwnerPlot <> nil THEN
  begin
    FOwnerPlot.RegisterPlotObject(Self);
    OwnerPlot.Repaint;
  end;
end;

procedure TBasePlotRect.SetVisible(const AValue: Boolean);
begin
  if FVisible=AValue then exit;
  FVisible:=AValue;
end;

procedure TBasePlotRect.SetZooming(AValue: Boolean);
begin
  IF FZooming = AValue THEN exit;
  FZooming := AValue;
end;

function TBasePlotRect.PlotImage: TImage;
begin Result := OwnerPlot.PlotImage; end;

constructor TBasePlotRect.Create(OwnerPlot: TPlot);
begin
  inherited Create;
  FOwnerPlot := OwnerPlot;
  OwnerPlot.RegisterPlotObject(Self);
  FAutoRect := TRUE;
  FAutoFrameRect := TRUE;
  FVisible := TRUE;
  FZooming:=false;

  FFrameRect := Rect(0,0,4,4);
  FClientRect := FFrameRect;
  FDataRect := FClientRect;
end;

destructor TBasePlotRect.Destroy;
begin
  OwnerPlot.UnRegisterPlotObject(Self);
  OwnerPlot := nil;
  inherited Destroy;
end;


{ TPlotMenuHelpers }
// **************************************************************************

procedure TPlotMenuHelpers.SetOwnerPlot(const AValue: TPlot);
begin
  if FOwnerPlot = AValue then exit;
  FOwnerPlot := AValue;
end;

constructor TPlotMenuHelpers.Create(AOwnerPlot: TPlot);
begin
  inherited Create;
  FOwnerPlot := AOwnerPlot;
  FHelperFormsSeries := THelperFormsSeries.Create(FOwnerPlot.Owner);
  FHelperFormsSeries.OwnerPlot := FOwnerPlot;
  FHelperFormsPlotRect := THelperFormsPlotRect.Create(FOwnerPlot.Owner);
  FHelperFormsPlotRect.OwnerPlot := FOwnerPlot;
  FHelperFormsAxes := THelperFormsAxes.Create(FOwnerPlot.Owner);
  FHelperFormsAxes.OwnerPlot := FOwnerPlot;
  FHelperFormsExport := THelperFormsPlot.Create(FOwnerPlot.Owner);
  FHelperFormsExport.OwnerPlot := FOwnerPlot;

  FColorDialog := TColorDialog.Create(AOwnerPlot);
end;

destructor TPlotMenuHelpers.Destroy;
begin
  IF FColorDialog <> nil THEN FColorDialog.Free;
  IF FFileDialog <> nil THEN FFileDialog.Free;
  FHelperFormsSeries.Free;
  FHelperFormsPlotRect.Free;
  FHelperFormsAxes.Free;
  FHelperFormsExport.Free;
  inherited Destroy;
end;

procedure TPlotMenuHelpers.EvalMenuPlotOpts(Sender: TObject);
var
  vLoop: Integer;
begin
  IF TMenuItem(Sender).Caption = 'Export to file' THEN
    OwnerPlot.DoExportToFilePlot(Sender) ELSE
  IF TMenuItem(Sender).Caption = 'Settings' THEN
    THelperFormsPlot(FHelperFormsExport).PlotOptionsShow;
  IF TMenuItem(Sender).Caption = 'Clear all data' THEN begin
    for vLoop := 0 to OwnerPlot.SeriesCount-1 do begin
      OwnerPlot.Series[vLoop].Clear;
      OwnerPlot.Repaint;
    end;
  end;
end;

procedure TPlotMenuHelpers.EvalMenuSeriesOpts(Sender: TObject);
var
  vSeriesString: AnsiString;
  vIndexString: AnsiString;
  vSeriesIndex: Integer;
begin
  // Data subitem clicked ?
  IF TMenuItem(Sender).Parent.Caption = 'Data' THEN
  BEGIN
    vSeriesString := AnsiString(TMenuItem(Sender).Parent.Parent.Caption);
    vIndexString := vSeriesString[1];
    IF (Length(vSeriesString)>1) THEN
      IF (vSeriesString[2]<>' ') THEN  vIndexString := vIndexString + vSeriesString[2];
    vSeriesIndex := StrToInt(vIndexString);
    // Clear Clicked ?
    IF TMenuItem(Sender).Caption = 'Clear' THEN
      OwnerPlot.Series[vSeriesIndex].Clear
    ELSE IF TMenuItem(Sender).Caption = 'Export' THEN
      begin
        DoExportImportData(FALSE, vSeriesIndex);
      end
    ELSE IF TMenuItem(Sender).Caption = 'Import' THEN
      begin
        DoExportImportData(TRUE, vSeriesIndex);
      end;
  END
  ELSE BEGIN
    // else find out series index clicked
    vSeriesString := AnsiString(TMenuItem(Sender).Parent.Caption);
    vIndexString := vSeriesString[1];
    IF (Length(vSeriesString)>1) THEN
      IF (vSeriesString[2]<>' ') THEN  vIndexString := vIndexString + vSeriesString[2];
    vSeriesIndex := StrToInt(vIndexString);
    // find out wether 'Manual scale' or 'Color' was chosen
    IF TMenuItem(Sender).Caption = 'Manual scale' THEN
      DoMenuSeriesScale(vSeriesIndex) ELSE
    IF TMenuItem(Sender).Caption = 'Color' THEN
      DoMenuSeriesColorChoose(vSeriesIndex);
    IF TMenuItem(Sender).Caption = 'Style' THEN
      DoMenuSeriesStyleChoose(vSeriesIndex);
    IF TMenuItem(Sender).Caption = 'Markers' THEN
      DoMenuSeriesMarkersChoose(vSeriesIndex);
  END;
end;

procedure TPlotMenuHelpers.EvalMenuPlotRectOpts(Sender: TObject);
begin
// for PlotRects the number is stored in MousePlotRectDown, so we need no parser here
  IF TMenuItem(Sender).Caption = 'Properties' THEN
    DoMenuPlotRectStyleChoose(OwnerPlot.MousePlotRectDown);
end;

procedure TPlotMenuHelpers.EvalMenuAxesOpts(Sender: TObject);
var
  vAxisString: AnsiString;
  vIndexString: AnsiString;
  vAxisIndex: Integer;
begin
  // again that Parser, please please modularize it
  // find out axis index clicked
  vAxisString := AnsiString(TMenuItem(Sender).Caption);
  vIndexString := vAxisString[1];
  IF (Length(vAxisString)>1) THEN
    IF (vAxisString[2]<>' ') THEN  vIndexString := vIndexString + vAxisString[2];
  vAxisIndex := StrToInt(vIndexString);
  THelperFormsAxes(FHelperFormsAxes).AxesStyleChoose(vAxisIndex) ;
end;

procedure TPlotMenuHelpers.DoMenuPRAutoScale(Sender: TObject);
begin
  OwnerPlot.AutoScalePlotRect(OwnerPlot.MousePlotRectDown);
end;

procedure TPlotMenuHelpers.DoMenuSeriesScale(ASeriesIndex: Integer);
begin
  THelperFormsSeries(FHelperFormsSeries).SeriesManualScale(ASeriesIndex);
end;

procedure TPlotMenuHelpers.DoMenuSeriesColorChoose(ASeriesIndex: Integer);
var
  vNewColor: TColor;
  vFreeDialog: Boolean;
begin
  vFreeDialog := FColorDialog = nil;
  if FColorDialog = nil then FColorDialog := TColorDialog.Create(OwnerPlot.Parent);
  try
    vNewColor := TPlotStyle(OwnerPlot.Series[ASeriesIndex].Style).Color;
    FColorDialog.Color := vNewColor;
    if FColorDialog.Execute then
      vNewColor := FColorDialog.Color;
  finally
    if vFreeDialog then FreeAndNil(FColorDialog);
  end;

  TPlotStyle(OwnerPlot.Series[ASeriesIndex].Style).Color := vNewColor;
  OwnerPlot.Repaint;
end;

procedure TPlotMenuHelpers.DoMenuSeriesStyleChoose(ASeriesIndex: Integer);
begin
  THelperFormsSeries(FHelperFormsSeries).SeriesStyleChoose(ASeriesIndex);
end;

procedure TPlotMenuHelpers.DoMenuSeriesMarkersChoose(ASeriesIndex: Integer);
begin
  THelperFormsSeries(FHelperFormsSeries).SeriesMarkersChoose(ASeriesIndex);
end;

procedure TPlotMenuHelpers.DoMenuPlotRectStyleChoose(APlotRectIndex: Integer);
begin
  THelperFormsPlotRect(FHelperFormsPlotRect).PlotRectStyleChoose(APlotRectIndex);
end;

procedure TPlotMenuHelpers.DoExportImportData(AImport: Boolean;
  ASeriesIndex: Integer);
var
  vFileName: TFilename;
  vFreeDialog: Boolean;
begin
  vFreeDialog := FFileDialog = nil;

  if FFileDialog = nil then begin
    IF AImport THEN FFileDialog := TOpenDialog.Create(OwnerPlot.Parent)
    ELSE FFileDialog := TSaveDialog.Create(OwnerPlot.Parent);
  end;

  try
    FFileDialog.Filter := 'MAAResult | *.mdt';
    if FFileDialog.Execute then
      vFileName := FFileDialog.FileName;
  finally
    if vFreeDialog then FreeAndNil(FFileDialog);
  end;
  IF vFileName = '' then exit;
  //
  IF AImport THEN begin
    TXYPlotSeries(OwnerPlot.Series[ASeriesIndex]).ImportSeriesData(vFileName);
    OwnerPlot.Repaint;
  end
  ELSE
    TXYPlotSeries(OwnerPlot.Series[ASeriesIndex]).ExportSeriesData(vFileName);
end;

{ THelperFormsBase }
// **************************************************************************

procedure THelperFormsBase.SetOwnerPlot(const APlot: TPlot);
begin
  FOwnerPlot := APlot;
end;

constructor THelperFormsBase.Create(AOwner: TComponent);
begin
  inherited CreateNew(AOwner);  // resourceless forms need CreateNew !
end;

destructor THelperFormsBase.Destroy;
begin
  inherited Destroy;
end;

end.
