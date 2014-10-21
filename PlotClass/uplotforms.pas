unit uPlotForms;
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
  Classes, SysUtils, Forms, uPlotClass, StdCtrls, Controls, ExtCtrls, Graphics,
  Dialogs, Spin, uPlotStyles, uPlotAxis, uPlotRect, types, useriesmarkers,
  CheckLst, uPlotDataTypes, uPlotInterpolator, uPlotHIDHandler;


// TODO: make more modular
// move buttons to base class
// evtl. destroy unneeded forms ?
// make forms nicer !

type

  THelperFormPlotOptions = class;
  THelperFormSeriesManualScale = class;
  THelperFormSeriesStyleChoose = class;
  THelperFormSeriesMarkersChoose = class;
  THelperFormPlotRectStyleChoose = class;
  THelperFormAxesStyleChoose = class;

  { THelperForms } //=====================================================-

  THelperForms = class(THelperFormsBase)
  private
    FBtnOK, FBtnClose: TButton;
  protected
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure OnButton(Sender: TObject);virtual;
    procedure Apply(Sender: TObject); virtual;
    procedure Done; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;


  { THelperFormsPlot }

  THelperFormsPlot = class(THelperForms)
  private
    FfrmPlotOptions: THelperFormPlotOptions;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure PlotOptionsShow;
  end;

  { THelperFormExportOptions }

  { THelperFormPlotOptions }

  THelperFormPlotOptions = class(THelperForms)
  private

    Fgb_MouseActions: TGroupBox;
    Fcb_Zoom: TComboBox;
    Flbl_Zoom: TLabel;
    Fcb_Pan: TComboBox;
    Flbl_Pan: TLabel;
    Fcb_AdHocMarker: TComboBox;
    Flbl_AdHocMarker: TLabel;

    // export size
    Fgb_exportsize: TGroupBox;
    Fed_sizeX, Fed_sizeY: TSpinEdit;
    Flbl_sizedelimiter: TLabel;
    // color scheme
    Fcb_PrinterFriendly: TCheckBox;
    // background
    Fbtn_color: TColorButton;
    procedure _ComboDropDown(Sender: TObject);
    procedure _ComboChange(Sender: TObject);
  protected
    procedure InitForm;
    procedure Apply(Sender: TObject);override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;



  { THelperFormsSeries } //=====================================================-

  THelperFormsSeries = class(THelperForms)
  private
    FfrmManualScale: THelperFormSeriesManualScale;
    FfrmStyleChoose: THelperFormSeriesStyleChoose;
    FfrmMarkersChoose: THelperFormSeriesMarkersChoose;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SeriesManualScale(ASeriesIndex: Integer);
    procedure SeriesStyleChoose(ASeriesIndex: Integer);
    procedure SeriesMarkersChoose(ASeriesIndex: Integer);
  end;


  { THelperFormManualScale } //-------------------------------------------------

  THelperFormSeriesManualScale = class(THelperFormsSeries)
  private
    FActiveSeriesManualScale: integer;
    FEdXmin, FEdXmax, FEdYmin, FEdYmax, FEdZmin, FEdZmax: TLabeledEdit;
    Fed_SeriesName: TEdit;
  protected
    procedure InitForm(ASeriesIndex: Integer);
    procedure Apply(Sender: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { THelperFormSeriesStyleChoose } //-------------------------------------------

  THelperFormSeriesStyleChoose = class(THelperFormsSeries)
  private
    FInitInProgress: Boolean;
    FActiveSeriesStyleChoose: integer;
    Flbl_style, Flbl_stylepoints, Flbl_stylellines: TLabel;
    Flbl_sizepoints, Flbl_sizelines: TLabel;
    Fcb_style, Fcb_stylepoints, Fcb_stylellines: TComboBox;
    Fed_sizepoints, Fed_sizelines: TSpinEdit;
    //------------------------
    Fcb_interpolate: TCheckBox;
    Fcb_Ipol_AxisMode: TComboBox;
    Fcb_Ipol_Mode: TComboBox;
    Fshp: TShape; //------------------------
    Flbl_seriesname, Flbl_units: TLabel;
    Fed_seriesname, FedXunit, FedYunit, FedZunit: TEdit;
    // components for colored axes
    Fcb_drawcolored: TCheckBox;
    Fcb_coloraxes: TComboBox;
    procedure EnableControlsLineStyle(AVisible: Boolean);
  protected
    procedure InitForm(ASeriesIndex:Integer);
    procedure Apply(Sender: TObject);override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { THelperFormSeriesStyleChoose } //-------------------------------------------

  { THelperFormSeriesMarkersChoose }

  THelperFormSeriesMarkersChoose = class(THelperFormsSeries)
  private
    FInitInProgress: Boolean;
    FActiveSeriesMarkersChoose: integer;
    Flbl_mkrIndex, Flbl_mkrType, Flbl_mkrMode,
    Flbl_diameter, Flbl_linewidth: TLabel;

    Fcb_mkrIndex: TComboBox;
    Fbtn_Addmarker: TButton;
    Fbtn_RemoveMarker: TButton;
    Fcb_mkrType, Fcb_mkrMode: TComboBox;
    Fcb_mkrStyle: TCheckListBox;
    Fed_diameter, Fed_linewidth: TSpinEdit;
    //Fshp: TShape; //------------------------
    Fbtn_color, Fbtn_backgndcolor: TColorButton;
    Fed_markeralpha, Fed_backgndalpha: TSpinEdit;
    Flbl_XIndex: TLabel;
    Fed_Xindex: TSpinEdit;
    Flbl_XYZValues: TLabel;
    Fed_XValue, Fed_YValue, Fed_ZValue: TEdit;
    procedure _PropertyChanged(Sender: TObject);
    procedure AddRemoveMarker(Sender: TObject);
    procedure ShowMarkerProperties(AMarkerIndex: Integer);
    procedure ApplyMarkerProperties(AMarkerIndex: Integer);
  protected
    procedure InitForm(ASeriesIndex:Integer);
    procedure Apply(Sender: TObject);override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;


  { THelperFormsPlotRect } //===================================================

  THelperFormsPlotRect = class(THelperForms)
  private
    FActivePlotRect: integer;
    FfrmPlotRectStyleChoose: THelperFormPlotRectStyleChoose;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure PlotRectStyleChoose(APlotRectIndex: Integer);
  end;

  { THelperFormPlotRectStyleChoose } //-----------------------------------------

  THelperFormPlotRectStyleChoose = class(THelperFormsPlotRect)
  private
    Fcb_legend, Fcb_colorscale: TCheckBox;
    Fcb_axes: TComboBox;
    Fbtn_color: TColorButton;
  protected
    procedure Apply(Sender: TObject);override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure InitForm(APlotRectIndex:Integer);
  end;


  { THelperFormsAxes } //===================================================

  THelperFormsAxes = class(THelperForms)
  private
    FActiveAxis: integer;
    FfrmAxesStyleChoose: THelperFormAxesStyleChoose;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure AxesStyleChoose(AAxis: Integer);
  end;

  { THelperFormPlotRectStyleChoose } //-----------------------------------------

  { THelperFormAxesStyleChoose }

  THelperFormAxesStyleChoose = class(THelperFormsAxes)
  private
    Fcb_logscale: TCheckBox;
    Fed_logbase: TSpinEdit;
    Fcb_innerticks, Fcb_innersubticks, Fcb_InnerTicks3D: TCheckBox;
    //Fshp: TShape; //------------------------
    Flbl_numformat: TLabel;
    Fed_axisname: TLabeledEdit;
    Fcb_numberformat: TComboBox;
    Fbtn_color: TColorButton;
  protected
    procedure Apply(Sender: TObject);override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure InitForm(AAxis:Integer);
  end;


implementation

uses
  uPlotSeries;



{THelperForms}

procedure THelperForms.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
begin
  CloseAction := caHide;
end;

procedure THelperForms.OnButton(Sender: TObject);
begin
  IF TButton(Sender) = FBtnOK THEN BEGIN
    Apply(Sender);
  END ELSE BEGIN
    Done;
  END;
end;

procedure THelperForms.Apply(Sender:TObject);
begin
  // do nothing in base class
end;

procedure THelperForms.Done;
begin
  OnClose := @FormClose;
  Self.Close;
end;

constructor THelperForms.Create(AOwner: TComponent);
const
  cBUTTON_GEN_SPACE = 20;
begin
  inherited Create(AOwner);
  // generic buttons
  FBtnOK := TButton.Create(Self);
  FBtnOK.Parent := Self;
  FBtnOK.Caption := 'Apply';
  FBtnOK.OnClick := @OnButton;
  FBtnOK.BorderSpacing.Around := cBUTTON_GEN_SPACE;
  FBtnOK.AnchorSide[akLeft].Control := Self;
  FBtnOK.AnchorSide[akLeft].Side := asrLeft;
  FBtnOK.AnchorSide[akBottom].Control := Self;
  FBtnOK.AnchorSide[akBottom].Side := asrBottom;
  FBtnOK.Anchors := [akBottom, akLeft];

  FBtnClose := TButton.Create(Self);
  FBtnClose.Parent := Self;
  FBtnClose.Caption := 'Close';
  FBtnClose.OnClick := @OnButton;
  FBtnClose.BorderSpacing.Around := cBUTTON_GEN_SPACE;
  FBtnClose.AnchorSide[akLeft].Control := FBtnOK;
  FBtnClose.AnchorSide[akLeft].Side := asrRight;
  FBtnClose.AnchorSide[akBottom].Control := Self;
  FBtnClose.AnchorSide[akBottom].Side := asrBottom;
  FBtnClose.Anchors := [akBottom, akLeft];

end;

destructor THelperForms.Destroy;
begin
  //FBtnClose.Free;
  //FBtnOK.Free;
  inherited Destroy;
end;

{ THelperFormsExport }
{===========================================================================}

constructor THelperFormsPlot.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor THelperFormsPlot.Destroy;
begin
  IF FfrmPlotOptions <> nil THEN FfrmPlotOptions.Free;
  inherited Destroy;
end;

procedure THelperFormsPlot.PlotOptionsShow;
begin
  IF FfrmPlotOptions = nil THEN FfrmPlotOptions := THelperFormPlotOptions.Create(OwnerPlot);
  //FfrmManualScale.Owner := Self;
  FfrmPlotOptions.InitForm;
  FfrmPlotOptions.Show;
end;

{ THelperFormExportOptions }

procedure THelperFormPlotOptions._ComboDropDown(Sender: TObject);
var
  vStates: THIDMouseStates;
  vState: THIDMouseState;
  vHIDAction: THIDAction;
  //vLoop: Integer;
  //vOldState: THIDMouseState;
begin
  if Sender = Fcb_Zoom then vHIDAction:=haZoom else
  if Sender = Fcb_Pan then vHIDAction:=haPan else
  if Sender = Fcb_AdHocMarker then vHIDAction:=haAdHocMarker;

  OwnerPlot.HIDHandler.GetHIDActionStatesAvail(vHIDAction, vStates);
  //vOldState := OwnerPlot.HIDHandler.GetHIDActionState(vHIDAction, vOldState);

  TComboBox(Sender).Clear;
  //for vLoop:=0 to length(vStates)-1 do begin
  for vState in vStates do begin
    TComboBox(Sender).Items.Add(cHID_MOUSESTATE_Names[vState]);
  end;

end;

procedure THelperFormPlotOptions._ComboChange(Sender: TObject);
var
  vState: THIDMouseState;
  vHIDAction: THIDAction;
  vLoop: Integer;
begin
  if Sender = Fcb_Zoom then vHIDAction:=haZoom else
  if Sender = Fcb_Pan then vHIDAction:=haPan else
  if Sender = Fcb_AdHocMarker then vHIDAction:=haAdHocMarker;

  for vLoop := 0 to ord(high(THIDMouseState)) do begin
    if TComboBox(Sender).Items[TComboBox(Sender).ItemIndex] = cHID_MOUSESTATE_Names[THIDMouseState(vLoop)] then begin
      vState := THIDMouseState(vLoop);
      break;
    end;
  end;
  OwnerPlot.HIDHandler.SetHIDAction(vHIDAction, vState);
end;

procedure THelperFormPlotOptions.InitForm;
var
  vState: THIDMouseState;
begin
  Fed_sizeX.Value := OwnerPlot.ExportSize.cx;
  Fed_sizeY.Value := OwnerPlot.ExportSize.cy;
  Fcb_PrinterFriendly.Checked := OwnerPlot.ExportPrinterFriedlyColors;
  //Frg_MouseZoom.ItemIndex := Integer(OwnerPlot.MouseWheelZoomButtonPan = true);
  //for vLoop:=0 to length(vStates)-1 do begin
  OwnerPlot.HIDHandler.GetHIDActionState(haZoom, vState);
  Fcb_Zoom.Text := (cHID_MOUSESTATE_Names[vState]);
  OwnerPlot.HIDHandler.GetHIDActionState(haPan, vState);
  Fcb_Pan.Text := (cHID_MOUSESTATE_Names[vState]);
  OwnerPlot.HIDHandler.GetHIDActionState(haAdHocMarker, vState);
  Fcb_AdHocMarker.Text := (cHID_MOUSESTATE_Names[vState]);

  Fbtn_color.ButtonColor := OwnerPlot.BackgroundColor;
end;

procedure THelperFormPlotOptions.Apply(Sender: TObject);
var
  vSize: TSize;
begin
  vSize.cx := Fed_sizeX.Value;
  vSize.cy := Fed_sizeY.Value;
  OwnerPlot.ExportSize := vSize;
  OwnerPlot.ExportPrinterFriedlyColors := Fcb_PrinterFriendly.Checked;
  //OwnerPlot.MouseWheelZoomButtonPan := Frg_MouseZoom.ItemIndex = 1;
  if Sender = Fbtn_color then begin
    OwnerPlot.BackgroundColor := Fbtn_color.ButtonColor;
    OwnerPlot.Repaint;
  end;
  inherited Apply(Sender);
end;

constructor THelperFormPlotOptions.Create(AOwner: TComponent);
const
  cBORDERSPACE = 12;
begin
  inherited Create(AOwner);
  OwnerPlot := TPlot(AOwner);
  Self.Height := 360;
  Self.Width := 320;
  Self.Caption := 'Plot options';

  // export size
  //Fed_sizeX, Fed_sizeY: TSpinEdit;
  //Flbl_sizedelimiter: TLabel;

  // Mouse options

  Fgb_MouseActions := TGroupBox.Create(Self);
  with Fgb_MouseActions do begin
    Parent := Self;
    Visible := TRUE;
    AnchorSide[akLeft].Side:=asrLeft;
    AnchorSide[akLeft].Control:=Self;
    AnchorSide[akRight].Side:=asrRight;
    AnchorSide[akRight].Control:=Self;
    AnchorSide[akTop].Side:=asrTop;
    AnchorSide[akTop].Control:=Self;
    Anchors:=[akLeft, akTop, akRight];
    BorderSpacing.Around := cBORDERSPACE;
    Caption := 'Mouse actions';
  end;

  Flbl_Zoom := TLabel.Create(Self);
  with Flbl_Zoom do begin
    Parent := Self;
    Visible := TRUE;
    AnchorSide[akLeft].Side:=asrLeft;
    AnchorSide[akLeft].Control:=Fgb_MouseActions;
    AnchorSide[akTop].Side:=asrTop;
    AnchorSide[akTop].Control:=Fgb_MouseActions;
    Anchors:=[akLeft, akTop];
    Text := 'Zoom plot';
    BorderSpacing.Left:=6;
    BorderSpacing.Top:=28;
  end;

  Fcb_Zoom := TComboBox.Create(Self);
  with Fcb_Zoom do begin
    Parent := Self;
    Visible := TRUE;
    AnchorSide[akLeft].Side:=asrRight;
    AnchorSide[akLeft].Control:=Flbl_Zoom;
    AnchorSide[akTop].Side:=asrCenter;
    AnchorSide[akTop].Control:=Flbl_Zoom;
    AnchorSide[akRight].Side:=asrRight;
    AnchorSide[akRight].Control:=Fgb_MouseActions;
    Anchors:=[akLeft, akTop, akRight];
    BorderSpacing.Left := 3 *cBORDERSPACE;
    BorderSpacing.Right := 6;
    OnChange:= @_ComboChange;
    OnDropDown:= @_ComboDropDown;
  end;


  Flbl_Pan := TLabel.Create(Self);
  with Flbl_Pan do begin
    Parent := Self;
    Visible := TRUE;
    AnchorSide[akLeft].Side:=asrLeft;
    AnchorSide[akLeft].Control:=Flbl_Zoom;
    AnchorSide[akTop].Side:=asrBottom;
    AnchorSide[akTop].Control:=Flbl_Zoom;
    Anchors:=[akLeft, akTop];
    Text := 'Pan plot';
    BorderSpacing.Top:=cBORDERSPACE;
  end;

  Fcb_Pan := TComboBox.Create(Self);
  with Fcb_Pan do begin
    Parent := Self;
    Visible := TRUE;
    AnchorSide[akLeft].Side:=asrLeft;
    AnchorSide[akLeft].Control:=Fcb_Zoom;
    AnchorSide[akTop].Side:=asrCenter;
    AnchorSide[akTop].Control:=Flbl_Pan;
    AnchorSide[akRight].Side:=asrRight;
    AnchorSide[akRight].Control:=Fcb_Zoom;
    Anchors:=[akLeft, akTop, akRight];
    OnChange:= @_ComboChange;
    OnDropDown:= @_ComboDropDown;
  end;

  Flbl_AdHocMarker := TLabel.Create(Self);
  with Flbl_AdHocMarker do begin
    Parent := Self;
    Visible := TRUE;
    AnchorSide[akLeft].Side:=asrLeft;
    AnchorSide[akLeft].Control:=Flbl_Pan;
    AnchorSide[akTop].Side:=asrBottom;
    AnchorSide[akTop].Control:=Flbl_Pan;
    Anchors:=[akLeft, akTop];
    Text := 'Ad hoc marker';
    BorderSpacing.Top:=cBORDERSPACE;
  end;

  Fcb_AdHocMarker := TComboBox.Create(Self);
  with Fcb_AdHocMarker do begin
    Parent := Self;
    Visible := TRUE;
    AnchorSide[akLeft].Side:=asrLeft;
    AnchorSide[akLeft].Control:=Fcb_Zoom;
    AnchorSide[akTop].Side:=asrCenter;
    AnchorSide[akTop].Control:=Flbl_AdHocMarker;
    AnchorSide[akRight].Side:=asrRight;
    AnchorSide[akRight].Control:=Fcb_Zoom;
    Anchors:=[akLeft, akTop, akRight];
    OnChange:= @_ComboChange;
    OnDropDown:= @_ComboDropDown;
  end;



  // Export options --------------

  Fgb_exportsize := TGroupBox.Create(Self); // colorbutton, fixed to form
    Fgb_exportsize.Parent := Self;
    Fgb_exportsize.Visible := TRUE;
    Fgb_exportsize.AnchorSide[akLeft].Side:=asrLeft;
    Fgb_exportsize.AnchorSide[akLeft].Control:=Self;
    Fgb_exportsize.AnchorSide[akRight].Side:=asrRight;
    Fgb_exportsize.AnchorSide[akRight].Control:=Self;
    Fgb_exportsize.AnchorSide[akTop].Side:=asrBottom;
    Fgb_exportsize.AnchorSide[akTop].Control:=Fgb_MouseActions;
    Fgb_exportsize.Anchors:=[akLeft, akTop, akRight];
    Fgb_exportsize.BorderSpacing.Around := cBORDERSPACE;
    Fgb_exportsize.Caption := 'Export Size [width] x [height] in px';
    //Fgb_exportsize.AutoSize:=true;

  Fed_sizeX := TSpinEdit.Create(Self);
    Fed_sizeX.Parent := Fgb_exportsize;
    Fed_sizeX.Visible := TRUE;
    Fed_sizeX.AnchorSide[akLeft].Side:=asrLeft;
    Fed_sizeX.AnchorSide[akLeft].Control:=Fgb_exportsize;
    Fed_sizeX.AnchorSide[akTop].Side:=asrTop;
    Fed_sizeX.AnchorSide[akTop].Control:=Fgb_exportsize;
    Fed_sizeX.Anchors:=[akLeft,akTop];
    Fed_sizeX.BorderSpacing.Around := cBORDERSPACE;
    Fed_sizeX.Caption := 'sizeX';
    Fed_sizeX.MinValue := 1;
    Fed_sizeX.MaxValue := 8192;
    Fed_sizeX.Value := 1024;

  Flbl_sizedelimiter := TLabel.Create(Self);   // label 'X'
    Flbl_sizedelimiter.Parent := Fgb_exportsize;
    Flbl_sizedelimiter.Visible := TRUE;
    Flbl_sizedelimiter.AnchorSide[akLeft].Side:=asrRight;
    Flbl_sizedelimiter.AnchorSide[akLeft].Control:=Fed_sizeX;
    Flbl_sizedelimiter.AnchorSide[akTop].Side:=asrTop;
    Flbl_sizedelimiter.AnchorSide[akTop].Control:=Fed_sizeX;
    Flbl_sizedelimiter.Anchors:=[akLeft,akTop];
    Flbl_sizedelimiter.BorderSpacing.Left := cBORDERSPACE;
    Flbl_sizedelimiter.BorderSpacing.Top := 0;
    Flbl_sizedelimiter.Caption := 'x';

  Fed_sizeY := TSpinEdit.Create(Self);
    Fed_sizeY.Parent := Fgb_exportsize;
    Fed_sizeY.Visible := TRUE;
    Fed_sizeY.AnchorSide[akLeft].Side:=asrRight;
    Fed_sizeY.AnchorSide[akLeft].Control:=Flbl_sizedelimiter;
    Fed_sizeY.AnchorSide[akTop].Side:=asrTop;
    Fed_sizeY.AnchorSide[akTop].Control:=Fed_sizeX;
    Fed_sizeY.Anchors:=[akLeft,akTop];
    Fed_sizeY.BorderSpacing.Left := cBORDERSPACE;
    Fed_sizeY.BorderSpacing.Top := 0;
    //Fed_sizeY.Caption := 'sizeX';
    Fed_sizeY.MinValue := 1;
    Fed_sizeY.MaxValue := 8192;
    Fed_sizeY.Value := 768;



  // Checkbox Colorscheme printerfriendly

  Fcb_PrinterFriendly := TCheckBox.Create(Self);
    Fcb_PrinterFriendly.Parent := Fgb_exportsize;
    Fcb_PrinterFriendly.Visible := TRUE;
    Fcb_PrinterFriendly.Checked := FALSE;
    Fcb_PrinterFriendly.AnchorSide[akLeft].Side:=asrLeft;
    Fcb_PrinterFriendly.AnchorSide[akLeft].Control:=Fed_sizeX;
    Fcb_PrinterFriendly.AnchorSide[akTop].Side:=asrBottom;
    Fcb_PrinterFriendly.AnchorSide[akTop].Control:=Fed_sizeX;
    Fcb_PrinterFriendly.Anchors:=[akLeft, akTop];
    //Fcb_PrinterFriendly.BorderSpacing.Around := cBORDERSPACE;
    Fcb_PrinterFriendly.BorderSpacing.Top := cBORDERSPACE;
    Fcb_PrinterFriendly.Caption := 'Use printer friendly colors';
    //Fcb_legend.OnClick := @Apply;


  // general style (color)
  Fbtn_color := TColorButton.Create(Self);
  with Fbtn_color do begin
    Parent := Self;
    BorderSpacing.Top := cBORDERSPACE;
    AnchorSide[akLeft].Side := asrLeft;
    AnchorSide[akLeft].Control := Fgb_exportsize;
    AnchorSide[akTop].Side := asrBottom;
    AnchorSide[akTop].Control := Fgb_exportsize;
    Anchors := [akLeft, akTop];
    //Top := 28;
    Width := 132;
    //BorderSpacing.Around := cBorderSpace;
    Caption := 'Background';
    OnColorChanged := @Apply;
  end;

  Fbtn_color.Top := 28;
  Fbtn_color.Width := 132;
  Fbtn_color.BorderSpacing.Around := cBorderSpace;
  Fbtn_color.Caption := 'Background';
  Fbtn_color.OnColorChanged := @Apply;
end;

destructor THelperFormPlotOptions.Destroy;
begin
  Self.Hide;
  inherited Destroy;
end;



{ THelperFormsSeries }
{===========================================================================}



constructor THelperFormsSeries.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor THelperFormsSeries.Destroy;
begin
  // TODO: destroy eventual existing forms
  IF FfrmManualScale <> nil THEN FfrmManualScale.Free;
  IF FfrmStyleChoose <> nil THEN FfrmStyleChoose.Free;
  IF FfrmMarkersChoose <> nil THEN FfrmMarkersChoose.Free;
  inherited Destroy;
end;

procedure THelperFormsSeries.SeriesManualScale(ASeriesIndex: Integer);
begin
  IF FfrmManualScale = nil THEN FfrmManualScale := THelperFormSeriesManualScale.Create(OwnerPlot);
  //FfrmManualScale.Owner := Self;
  FfrmManualScale.InitForm(ASeriesIndex);
  FfrmManualScale.Show;
end;

procedure THelperFormsSeries.SeriesStyleChoose(ASeriesIndex: Integer);
begin
  IF FfrmStyleChoose = nil THEN FfrmStyleChoose := THelperFormSeriesStyleChoose.Create(OwnerPlot);
  FfrmStyleChoose.InitForm(ASeriesIndex);
  FfrmStyleChoose.Show;
end;

procedure THelperFormsSeries.SeriesMarkersChoose(ASeriesIndex: Integer);
begin
  IF FfrmMarkersChoose = nil THEN FfrmMarkersChoose := THelperFormSeriesMarkersChoose.Create(OwnerPlot);
  FfrmMarkersChoose.InitForm(ASeriesIndex);
  FfrmMarkersChoose.Show;
end;




{ THelperFormSeriesManualScale }
{===========================================================================}

procedure THelperFormSeriesManualScale.Apply(Sender:TObject);
var
  vViewRangeX, vViewRangeY, vViewRangeZ: TValueRange;
begin
  vViewRangeX.min := StrToFloatDef(FEdXmin.Caption, 0);
  vViewRangeX.max := StrToFloatDef(FEdXmax.Caption, 0);
  TPlotAxisBase(OwnerPlot.Axis[TPlotSeries(OwnerPlot.Series[FActiveSeriesManualScale]).XAxis]).ViewRange := vViewRangeX;
  vViewRangeY.min := StrToFloatDef(FEdYmin.Caption, 0);
  vViewRangeY.max := StrToFloatDef(FEdYmax.Caption, 0);
  TPlotAxisBase(OwnerPlot.Axis[TPlotSeries(OwnerPlot.Series[FActiveSeriesManualScale]).YAxis]).ViewRange := vViewRangeY;
  IF FEdZmin.Enabled THEN BEGIN
    vViewRangeZ.min := StrToFloatDef(FEdZmin.Caption, 0);
    vViewRangeZ.max := StrToFloatDef(FEdZmax.Caption, 0);
    TPlotAxisBase(OwnerPlot.Axis[TXYZPlotSeries(OwnerPlot.Series[FActiveSeriesManualScale]).ZAxis]).ViewRange := vViewRangeZ;
  END;
  OwnerPlot.Repaint;
end;


constructor THelperFormSeriesManualScale.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OwnerPlot := TPlot(AOwner);
  Self.Height := 240;
  Self.Width := 220;
  Self.Caption := 'Series scale';

  Fed_SeriesName := TEdit.Create(Self);
  with Fed_SeriesName do begin
    Parent := Self;
    BorderSpacing.Around := 6;
    AnchorSide[akLeft].Side := asrLeft;
    AnchorSide[akLeft].Control := Self;
    AnchorSide[akRight].Side := asrRight;
    AnchorSide[akRight].Control := Self;
    AnchorSide[akTop].Side := asrTop;
    AnchorSide[akTop].Control := Self;
    Anchors:=[akLeft]+[akTop]+[akRight];
    Caption := 'series';
    Enabled:=false;
  end;

  // X controls
  FEdXmin := TLabeledEdit.Create(Self);
  FEdXmin.Parent := Self;
  FEdXmin.BorderSpacing.Left := 20;
  FEdXmin.BorderSpacing.Top := 0;
  FEdXmin.Top := 50;
  FEdXmin.AnchorSide[akLeft].Side := asrLeft;
  FEdXmin.AnchorSide[akLeft].Control := Self;
  //FEdXmin.AnchorSide[akTop].Side := asrTop;
  //FEdXmin.AnchorSide[akTop].Control := Self;
  FEdXmin.LabelPosition := lpAbove;
  FEdXmin.LabelSpacing := 3;
  FEdXmin.EditLabel.Caption := 'Xmin';

  FEdXmax := TLabeledEdit.Create(Self);
  FEdXmax.Parent := Self;
  FEdXmax.BorderSpacing.Left := 20;
  FEdXmax.BorderSpacing.Top := 0;
  FEdXmax.AnchorSide[akLeft].Side := asrRight;
  FEdXmax.AnchorSide[akLeft].Control := FEdXmin;
  FEdXmax.AnchorSide[akTop].Side := asrTop;
  FEdXmax.AnchorSide[akTop].Control := FEdXmin;
  FEdXmax.LabelPosition := lpAbove;
  FEdXmax.LabelSpacing := 3;
  FEdXmax.EditLabel.Caption := 'Xmax';

  // Y controls
  FEdYmin := TLabeledEdit.Create(Self);
  FEdYmin.Parent := Self;
  FEdYmin.BorderSpacing.Left := 20;
  FEdYmin.BorderSpacing.Top := 0;
  FEdYmin.Top := 100;
  FEdYmin.AnchorSide[akLeft].Side := asrLeft;
  FEdYmin.AnchorSide[akLeft].Control := Self;
  //FEdYmin.AnchorSide[akTop].Side := asrTop;
  //FEdYmin.AnchorSide[akTop].Control := FEdXmin;
  FEdYmin.LabelPosition := lpAbove;
  FEdYmin.LabelSpacing := 3;
  FEdYmin.EditLabel.Caption := 'Ymin';

  FEdYmax := TLabeledEdit.Create(Self);
  FEdYmax.Parent := Self;
  FEdYmax.BorderSpacing.Left := 20;
  FEdYmax.BorderSpacing.Top := 00;
  FEdYmax.AnchorSide[akLeft].Side := asrRight;
  FEdYmax.AnchorSide[akLeft].Control := FEdYmin;
  FEdYmax.AnchorSide[akTop].Side := asrTop;
  FEdYmax.AnchorSide[akTop].Control := FEdYmin;
  FEdYmax.LabelPosition := lpAbove;
  FEdYmax.LabelSpacing := 3;
  FEdYmax.EditLabel.Caption := 'Ymax';

  // Z controls
  FEdZmin := TLabeledEdit.Create(Self);
  FEdZmin.Parent := Self;
  FEdZmin.BorderSpacing.Left := 20;
  FEdZmin.BorderSpacing.Top := 00;
  FEdZmin.Top := 150;
  FEdZmin.AnchorSide[akLeft].Side := asrLeft;
  FEdZmin.AnchorSide[akLeft].Control := Self;
  //FEdZmin.AnchorSide[akTop].Side := asrTop;
  //FEdZmin.AnchorSide[akTop].Control := FEdYmin;
  FEdZmin.LabelPosition := lpAbove;
  FEdZmin.LabelSpacing := 3;
  FEdZmin.EditLabel.Caption := 'Zmin';

  FEdZmax := TLabeledEdit.Create(Self);
  FEdZmax.Parent := Self;
  FEdZmax.BorderSpacing.Left := 20;
  FEdZmax.BorderSpacing.Top := 00;
  FEdZmax.AnchorSide[akLeft].Side := asrRight;
  FEdZmax.AnchorSide[akLeft].Control := FEdZmin;
  FEdZmax.AnchorSide[akTop].Side := asrTop;
  FEdZmax.AnchorSide[akTop].Control := FEdZmin;
  FEdZmax.LabelPosition := lpAbove;
  FEdZmax.LabelSpacing := 3;
  FEdZmax.EditLabel.Caption := 'Zmax';

end;

destructor THelperFormSeriesManualScale.Destroy;
begin
  Self.Hide;
  inherited Destroy;
end;

procedure THelperFormSeriesManualScale.InitForm(ASeriesIndex: Integer);
var
  vViewRangeX, vViewRangeY, vViewRangeZ: TValueRange;
begin
      FActiveSeriesManualScale := ASeriesIndex;
      Fed_SeriesName.Caption := TPlotSeries(OwnerPlot.Series[FActiveSeriesManualScale]).Caption;
      // init Z axis controls
      FEdZmin.Caption := ''; FEdZmax.Caption := '';
      FEdZmin.Enabled := FALSE; FEdZmax.Enabled := FALSE;

      IF ( (OwnerPlot.Series[ASeriesIndex] is TXYPlotSeries)  and
        ( not (OwnerPlot.Series[ASeriesIndex].IsFastSeries)) ) THEN BEGIN
        vViewRangeX := TPlotAxisBase(OwnerPlot.Axis[TPlotSeries(OwnerPlot.Series[FActiveSeriesManualScale]).XAxis]).ViewRange;
        vViewRangeY := TPlotAxisBase(OwnerPlot.Axis[TPlotSeries(OwnerPlot.Series[FActiveSeriesManualScale]).YAxis]).ViewRange;
      END;
      IF ( (OwnerPlot.Series[ASeriesIndex] is TXYZPlotSeries)   and
        ( not (OwnerPlot.Series[ASeriesIndex].IsFastSeries)) ) THEN BEGIN
        vViewRangeZ := TPlotAxisBase(OwnerPlot.Axis[TXYZPlotSeries(OwnerPlot.Series[FActiveSeriesManualScale]).ZAxis]).ViewRange;
        FEdZmin.Caption := FloatToStr(vViewRangeZ.min);
        FEdZmax.Caption := FloatToStr(vViewRangeZ.max);
        FEdZmin.Enabled := TRUE; FEdZmax.Enabled := TRUE;
      END;
      IF (OwnerPlot.Series[ASeriesIndex] is TXYWFPlotSeries )  THEN BEGIN
        vViewRangeX := TPlotAxisBase(OwnerPlot.Axis[TXYWFPlotSeries(OwnerPlot.Series[FActiveSeriesManualScale]).XAxis]).ViewRange;
        vViewRangeY := TPlotAxisBase(OwnerPlot.Axis[TXYWFPlotSeries(OwnerPlot.Series[FActiveSeriesManualScale]).YAxis]).ViewRange;
        vViewRangeZ := TPlotAxisBase(OwnerPlot.Axis[TXYWFPlotSeries(OwnerPlot.Series[FActiveSeriesManualScale]).ZAxis]).ViewRange;
        FEdZmin.Caption := FloatToStr(vViewRangeZ.min);
        FEdZmax.Caption := FloatToStr(vViewRangeZ.max);
        FEdZmin.Enabled := TRUE; FEdZmax.Enabled := TRUE;
      END;
      IF ( OwnerPlot.Series[ASeriesIndex] is TXYSpectrumPlotSeries )  THEN BEGIN
        vViewRangeX := TPlotAxisBase(OwnerPlot.Axis[TPlotSeries(OwnerPlot.Series[FActiveSeriesManualScale]).XAxis]).ViewRange;
        vViewRangeY := TPlotAxisBase(OwnerPlot.Axis[TPlotSeries(OwnerPlot.Series[FActiveSeriesManualScale]).YAxis]).ViewRange;
      END;

  FEdXmin.Caption := FloatToStr(vViewRangeX.min);
  FEdXmax.Caption := FloatToStr(vViewRangeX.max);
  FEdYmin.Caption := FloatToStr(vViewRangeY.min);
  FEdYmax.Caption := FloatToStr(vViewRangeY.max);
  Self.Show;
end;




{ THelperFormSeriesStyleChoose }
{===========================================================================}

procedure THelperFormSeriesStyleChoose.EnableControlsLineStyle(AVisible: Boolean
  );
begin
  Fed_sizelines.Enabled := AVisible;
  Fed_sizelines.Visible := AVisible;
  Flbl_sizelines.Visible := AVisible;
  Fcb_stylellines.Visible := AVisible;
  Flbl_stylellines.Visible := AVisible;
end;

procedure THelperFormSeriesStyleChoose.Apply(Sender:TObject);
var
  vOldColor: TColor;
  vShape: TPointStyleShape;
  vAxisString, vIndexString: string;                                // TODO: modularize this parser, see uPlotClass helpers and below !
  vAxisIndex: integer;
begin
  IF FInitInProgress THEN exit;  // needed because events are fired during init form (series caption bug !)
  vAxisString := ''; vIndexString := ''; vAxisIndex := -1;

  // change style
  IF Sender = Fcb_style THEN
  begin
    vOldColor := TPlotStyle(OwnerPlot.Series[FActiveSeriesStyleChoose].Style).Color;

    IF Fcb_style.ItemIndex = 0 THEN BEGIN
      OwnerPlot.Series[FActiveSeriesStyleChoose].Style := TSeriesStylePoints.Create;
      EnableControlsLineStyle(FALSE);
      END ELSE
    IF Fcb_style.ItemIndex = 1 THEN BEGIN
      OwnerPlot.Series[FActiveSeriesStyleChoose].Style := TSeriesStyleLines.Create;
      EnableControlsLineStyle(TRUE);
      END;

    TPlotStyle(OwnerPlot.Series[FActiveSeriesStyleChoose].Style).Color := vOldColor;
  end;
  // set size
  //IF (Sender = Fed_sizepoints) OR (Sender = Fed_sizelines) OR (Sender = Fcb_stylepoints) OR (Sender = Fcb_stylellines) THEN
  begin
    IF OwnerPlot.Series[FActiveSeriesStyleChoose].Style.ClassNameIs('TSeriesStyleLines') THEN
      TSeriesStyleLines(OwnerPlot.Series[FActiveSeriesStyleChoose].Style).LineWidth := Fed_sizelines.Value;
    IF OwnerPlot.Series[FActiveSeriesStyleChoose].Style is TSeriesStylePoints THEN begin
      TSeriesStylePoints(OwnerPlot.Series[FActiveSeriesStyleChoose].Style).Diameter := Fed_sizepoints.Value;
      vShape := TPointStyleShape(Fcb_stylepoints.ItemIndex);
      TSeriesStylePoints(OwnerPlot.Series[FActiveSeriesStyleChoose].Style).Shape := vShape;
    end;
  end;

  // seriesname
  TPlotSeries(OwnerPlot.Series[FActiveSeriesStyleChoose]).Caption := Fed_seriesname.Caption;

  // unit names
  // there is not setter implemented for UnitString !!  therefore use Add method
  // FedXunit.Caption := TPlotSeries(OwnerPlot.Series[FActiveSeries]).UnitString[TPlotSeries(OwnerPlot.Series[FActiveSeries]).XAxis];
  TPlotSeries(OwnerPlot.Series[FActiveSeriesStyleChoose]).AddUnit('x',FedXunit.Caption);
  TPlotSeries(OwnerPlot.Series[FActiveSeriesStyleChoose]).AddUnit('Y',FedYunit.Caption);
  TPlotSeries(OwnerPlot.Series[FActiveSeriesStyleChoose]).AddUnit('Z',FedZunit.Caption);    // semms to be hardened for 3 units in 2D series

  // color an axis ?
  // ColorScale : attention: the axis to scale is a colorscale property
  // which series to draw colored referred to an axis is a series property !
    //
    // parse ComboBox content :                      // TODO: modularize this parser, see uPlotClass helpers and below !
    IF Fcb_coloraxes.ItemIndex > -1 THEN vAxisString := Fcb_coloraxes.Items[Fcb_coloraxes.ItemIndex];
    vIndexString := vAxisString[1];
    IF (Length(vAxisString)>1) THEN
      IF (vAxisString[2] in ['0'..'9']) THEN  vIndexString := vIndexString + vAxisString[2];
    vAxisIndex := StrToIntDef(vIndexString, -1);  //StrToIntDef(vIndexString,-1);
    // end parser
  IF Fcb_drawcolored. Checked THEN TPlotSeries(OwnerPlot.Series[FActiveSeriesStyleChoose]).ColoredAxis := vAxisIndex
    ELSE TPlotSeries(OwnerPlot.Series[FActiveSeriesStyleChoose]).ColoredAxis := -1; //-1 means none is colored

  // Interpolation
  TPlotSeries(OwnerPlot.Series[FActiveSeriesStyleChoose]).Interpolate := Fcb_interpolate.Checked;
  if TPlotSeries(OwnerPlot.Series[FActiveSeriesStyleChoose]).Interpolator <> nil then begin
    TInterpolator_PixelXY( TPlotSeries(OwnerPlot.Series[FActiveSeriesStyleChoose]).Interpolator).IpolMode := TIpolMode(Fcb_Ipol_Mode.ItemIndex);
    TInterpolator_PixelXY( TPlotSeries(OwnerPlot.Series[FActiveSeriesStyleChoose]).Interpolator).IpolAxisMode := TIpolAxisMode(Fcb_Ipol_AxisMode.ItemIndex);
  end;

  OwnerPlot.Repaint;
end;


constructor THelperFormSeriesStyleChoose.Create(AOwner: TComponent);
var
  vLoop: integer;
const
  cHspace = 12;
  cVspace = 8;
  cLeft = 16;
begin
  inherited Create(AOwner);
  FInitInProgress:=TRUE;
  OwnerPlot := TPlot(AOwner);
  Self.Height := 280;
  Self.Width := 340;
  Self.Caption := 'Series properties';

  // LineStyle / PointStyle
  // --- combos -------------------------------------

  Fcb_style := TComboBox.Create(Self); // combobox style, fixed to form
    Fcb_style.Parent := Self;
    Fcb_style.Visible := TRUE;
    Fcb_style.AnchorSide[akLeft].Side:=asrLeft;
    Fcb_style.AnchorSide[akLeft].Control:=Self;
    Fcb_style.Anchors:=Fcb_style.Anchors+[akLeft];
    Fcb_style.Top := 28;
    Fcb_style.BorderSpacing.Left := cLeft;
    Fcb_style.Items.Add('Points');
    Fcb_style.Items.Add('Lines');
    Fcb_style.ItemIndex := -1;
    Fcb_style.OnChange := @Apply;

  Fcb_stylepoints := TComboBox.Create(Self); // combobox pointstyle
    Fcb_stylepoints.Parent := Self;
    Fcb_stylepoints.Visible := TRUE;
    Fcb_stylepoints.AnchorParallel(akTop,0,Fcb_style);
    Fcb_stylepoints.AnchorToNeighbour(akLeft,cHspace,Fcb_style);
    for vLoop := ord(shapeDot) to ord(shapeMarkerTriangle) do begin
      Fcb_stylepoints.Items.Add(TPointStyleShapeName[TPointStyleShape(vLoop)]);
    end;
    //Fcb_stylepoints.Items.Add('Dots');
    //Fcb_stylepoints.Items.Add('Crosses');
    Fcb_stylepoints.ItemIndex := 0;
    Fcb_stylepoints.OnChange := @Apply;

  Fcb_stylellines := TComboBox.Create(Self); // combobox linestyle
    Fcb_stylellines.Parent := Self;
    Fcb_stylellines.Visible := TRUE;
    Fcb_stylellines.AnchorParallel(akTop,0,Fcb_stylepoints);
    Fcb_stylellines.AnchorToNeighbour(akLeft,cHspace,Fcb_stylepoints);
    Fcb_stylellines.Items.Add('Lines');
    Fcb_stylellines.ItemIndex := 0;
    Fcb_stylellines.OnChange := @Apply;

  Flbl_style := TLabel.Create(Self);   // label style
    Flbl_style.Parent := Self;
    Flbl_style.Visible := TRUE;
    Flbl_style.Caption := 'Style';
    Flbl_style.AnchorToNeighbour(akBottom,0,Fcb_style);
    Flbl_style.AnchorParallel(akLeft,0,Fcb_style);

  Flbl_stylepoints := TLabel.Create(Self);   // label pointstyle
    Flbl_stylepoints.Parent := Self;
    Flbl_stylepoints.Visible := TRUE;
    Flbl_stylepoints.Caption := 'Pointstyle';
    Flbl_stylepoints.AnchorToNeighbour(akBottom,0,Fcb_stylepoints);
    Flbl_stylepoints.AnchorParallel(akLeft,0,Fcb_stylepoints); ;
    //Flbl_stylepoints.BorderSpacing.Left := 20;

  Flbl_stylellines := TLabel.Create(Self);   // label linestyle
    Flbl_stylellines.Parent := Self;
    Flbl_stylellines.Visible := TRUE;
    Flbl_stylellines.Caption := 'Linestyle';
    Flbl_stylellines.AnchorToNeighbour(akBottom,0,Fcb_stylellines);
    Flbl_stylellines.AnchorParallel(akLeft,0,Fcb_stylellines);
    //Flbl_stylepoints.BorderSpacing.Left := 20;


  //--edits ----------------------------------
  Flbl_sizepoints := TLabel.Create(Self);    // label size
    Flbl_sizepoints.Parent := Self;
    Flbl_sizepoints.Visible := TRUE;
    Flbl_sizepoints.Caption := 'Size';
    Flbl_sizepoints.AnchorToNeighbour(akTop,cVspace,Fcb_stylepoints);
    Flbl_sizepoints.AnchorParallel(akLeft,0,Fcb_stylepoints);

  Fed_sizepoints := TSpinEdit.Create(Self);  // spinedit size
    Fed_sizepoints.Parent := Self;
    Fed_sizepoints.Visible := TRUE;
    Fed_sizepoints.AnchorToNeighbour(akTop,8,Flbl_sizepoints);
    Fed_sizepoints.AnchorParallel(akLeft,0,Flbl_sizepoints);
    Fed_sizepoints.OnChange := @Apply;
    Fed_sizepoints.Value := 1;
    Fed_sizepoints.MinValue := 1;
    Fed_sizepoints.MaxValue := 13;
    Fed_sizepoints.Increment := 2;

  Flbl_sizelines := TLabel.Create(Self);    // label size
    Flbl_sizelines.Parent := Self;
    Flbl_sizelines.Visible := TRUE;
    Flbl_sizelines.Caption := 'Size';
    Flbl_sizelines.AnchorToNeighbour(akTop,cVspace,Fcb_stylellines);
    Flbl_sizelines.AnchorParallel(akLeft,0,Fcb_stylellines);

  Fed_sizelines := TSpinEdit.Create(Self);  // spinedit size
    Fed_sizelines.Parent := Self;
    Fed_sizelines.Visible := TRUE;
    Fed_sizelines.AnchorToNeighbour(akTop,8,Flbl_sizelines);
    Fed_sizelines.AnchorParallel(akLeft,0,Flbl_sizelines);
    Fed_sizelines.OnChange := @Apply;
    Fed_sizelines.Value := 1;
    Fed_sizelines.MinValue := 1;
    Fed_sizelines.MaxValue := 9;
    Fed_sizelines.Increment := 1;

  // interpolate
  Fcb_interpolate := TCheckBox.Create(Self);
  with Fcb_interpolate do begin
    Parent := Self;
    Visible := TRUE;
    Checked := FALSE;
    AnchorSide[akLeft].Side:=asrLeft;
    AnchorSide[akLeft].Control:=Fcb_style;
    AnchorSide[akTop].Side:=asrBottom;
    AnchorSide[akTop].Control:=Fed_sizepoints;
    Anchors:=[akLeft]+[akTop];
    BorderSpacing.Top := cVspace;
    Caption := 'Interpolate';
  end;

  Fcb_Ipol_Mode := TComboBox.Create(Self);
  with Fcb_Ipol_Mode do begin
    Parent := Self;
    Visible := TRUE;
    AnchorSide[akLeft].Side:=asrRight;
    AnchorSide[akLeft].Control:=Fcb_interpolate;
    AnchorSide[akTop].Side:=asrCenter;
    AnchorSide[akTop].Control:=Fcb_interpolate;
    Anchors:=[akLeft]+[akTop];
    Items.Add('step');
    Items.Add('linear');
    ItemIndex:=1;
  end;

  Fcb_Ipol_AxisMode := TComboBox.Create(Self);
  with Fcb_Ipol_AxisMode do begin
    Parent := Self;
    Visible := TRUE;
    AnchorSide[akLeft].Side:=asrRight;
    AnchorSide[akLeft].Control:=Fcb_Ipol_Mode;
    AnchorSide[akTop].Side:=asrCenter;
    AnchorSide[akTop].Control:=Fcb_interpolate;
    Anchors:=[akLeft]+[akTop];
    Items.Add('X only');
    Items.Add('XY');
    ItemIndex:=1;
  end;


  // --name and units               // shape splitter
  Fshp := TShape.Create(Self);
  Fshp.Parent := Self;
  Fshp.Visible := TRUE;
  Fshp.Pen.Width := 4;
  Fshp.Pen.Color := clGray;
  Fshp.Height := 4;
  //Fshp.AnchorToNeighbour(akTop,cVspace,Fed_sizepoints);
  Fshp.AnchorSide[akLeft].Control := Self;
  Fshp.AnchorSide[akLeft].Side := asrLeft;
  Fshp.AnchorSide[akRight].Control := Self;
  Fshp.AnchorSide[akRight].Side := asrRight;
  Fshp.AnchorSideTop.Control := Fcb_interpolate;
  Fshp.AnchorSideTop.Side := asrBottom;
  Fshp.Anchors := [akTop, akLeft, akRight];
  Fshp.BorderSpacing.Around := 6;

  Flbl_seriesname := TLabel.Create(Self);   // label seriesname
    Flbl_seriesname.Parent := Self;
    Flbl_seriesname.Visible := TRUE;
    Flbl_seriesname.Caption := 'Name of Series';
    Flbl_seriesname.AnchorToNeighbour(akTop,cVspace,Fshp);
    Flbl_seriesname.AnchorParallel(akLeft,0,Fshp);

  Fed_seriesname := TEdit.Create(Self);    // edit seriesname
    Fed_seriesname.Parent := Self;
    Fed_seriesname.Visible := TRUE;
    Fed_seriesname.AnchorSide[akLeft].Control := Flbl_seriesname;
    Fed_seriesname.AnchorSide[akLeft].Side := asrRight;
    Fed_seriesname.AnchorSide[akRight].Control := Self;
    Fed_seriesname.AnchorSide[akRight].Side := asrRight;
    Fed_seriesname.AnchorSide[akTop].Control := Flbl_seriesname;
    Fed_seriesname.AnchorSide[akTop].Side := asrCenter;
    Fed_seriesname.Anchors := [akTop, akLeft, akRight];
    Fed_seriesname.BorderSpacing.Around := 6;

  Flbl_units := TLabel.Create(Self);   // label seriesname
    Flbl_units.Parent := Self;
    Flbl_units.Visible := TRUE;
    Flbl_units.Caption := 'Units (XYZ)';
    Flbl_units.AnchorToNeighbour(akTop,cVspace,Flbl_seriesname);
    Flbl_units.AnchorParallel(akLeft,0,Flbl_seriesname);

  FedXunit := TEdit.Create(Self);   // edit Xunit
    FedXunit.Parent := Self;
    FedXunit.Visible := TRUE;
    FedXunit.AnchorSide[akTop].Control := Flbl_units;
    FedXunit.AnchorSide[akTop].Side := asrCenter;
    FedXunit.AnchorSide[akLeft].Control := Flbl_units;
    FedXunit.AnchorSide[akLeft].Side := asrRight;
    FedXunit.Anchors := [akLeft, akTop];
    FedXunit.BorderSpacing.Around := 6;
  FedYunit := TEdit.Create(Self);   // edit Xunit
    FedYunit.Parent := Self;
    FedYunit.Visible := TRUE;
    FedYunit.AnchorSide[akTop].Control := Flbl_units;
    FedYunit.AnchorSide[akTop].Side := asrCenter;
    FedYunit.AnchorSide[akLeft].Control := FedXunit;
    FedYunit.AnchorSide[akLeft].Side := asrRight;
    FedYunit.Anchors := [akLeft, akTop];
    FedYunit.BorderSpacing.Around := 6;
  FedZunit := TEdit.Create(Self);   // edit Xunit
    FedZunit.Parent := Self;
    FedZunit.Visible := TRUE;
    FedZunit.AnchorSide[akTop].Control := Flbl_units;
    FedZunit.AnchorSide[akTop].Side := asrCenter;
    FedZunit.AnchorSide[akLeft].Control := FedYunit;
    FedZunit.AnchorSide[akLeft].Side := asrRight;
    FedZunit.Anchors := [akLeft, akTop];
    FedZunit.BorderSpacing.Around := 6;


  // colored axes
  Fcb_drawcolored := TCheckBox.Create(Self);
    Fcb_drawcolored.Parent := Self;
    Fcb_drawcolored.Visible := TRUE;
    Fcb_drawcolored.Checked := FALSE;
    Fcb_drawcolored.AnchorSide[akLeft].Side:=asrLeft;
    Fcb_drawcolored.AnchorSide[akLeft].Control:=Self;
    Fcb_drawcolored.AnchorSide[akTop].Side:=asrBottom;
    Fcb_drawcolored.AnchorSide[akTop].Control:=Flbl_units;
    Fcb_drawcolored.Anchors:=Fcb_drawcolored.Anchors+[akLeft]+[akTop];
    Fcb_drawcolored.BorderSpacing.Around := cVspace;
    Fcb_drawcolored.Caption := 'Draw color-scaled axis:';
    //Fcb_drawcolored.OnClick := @Apply;


  Fcb_coloraxes := TComboBox.Create(Self);
    Fcb_coloraxes.Parent := Self;
    Fcb_coloraxes.Visible := TRUE;
    Fcb_coloraxes.AnchorSide[akLeft].Side:=asrRight;
    Fcb_coloraxes.AnchorSide[akLeft].Control:=Fcb_drawcolored;
    Fcb_coloraxes.AnchorSide[akTop].Side:=asrCenter;
    Fcb_coloraxes.AnchorSide[akTop].Control:=Fcb_drawcolored;
    Fcb_coloraxes.Anchors:=Fcb_coloraxes.Anchors+[akLeft]+[akTop];
    Fcb_coloraxes.BorderSpacing.Around := cVspace;

    //Fcb_coloraxes.OnChange := @Apply;

end;

destructor THelperFormSeriesStyleChoose.Destroy;
begin
  Self.Close;
  inherited Destroy;
end;

procedure THelperFormSeriesStyleChoose.InitForm(ASeriesIndex:Integer);
var
  vShape: TPointStyleShape;
begin
  FInitInProgress:=TRUE;
  FActiveSeriesStyleChoose := ASeriesIndex;
  // init controls - lines/points
    IF OwnerPlot.Series[FActiveSeriesStyleChoose].Style is TSeriesStyleLines THEN begin
    Fcb_style.ItemIndex := 1;
    Fed_sizepoints.Value := TSeriesStylePoints(OwnerPlot.Series[FActiveSeriesStyleChoose].Style).Diameter;
    Fed_sizelines.Value := TSeriesStyleLines(OwnerPlot.Series[FActiveSeriesStyleChoose].Style).LineWidth;
    end ELSE
    IF OwnerPlot.Series[FActiveSeriesStyleChoose].Style.ClassNameIs('TSeriesStylePoints') THEN begin
    Fcb_style.ItemIndex := 0;
    Fed_sizepoints.Value := TSeriesStylePoints(OwnerPlot.Series[FActiveSeriesStyleChoose].Style).Diameter;
    // shapes
      vShape := TSeriesStylePoints(OwnerPlot.Series[FActiveSeriesStyleChoose].Style).Shape;
      Fcb_stylepoints.ItemIndex := ord(vShape);
    end;

  //
  // deactivate unused controls - lines/points
      IF OwnerPlot.Series[FActiveSeriesStyleChoose].Style.ClassNameIs('TSeriesStyleLines') THEN
        EnableControlsLineStyle(TRUE) ELSE
        EnableControlsLineStyle(FALSE);


  // seriesname
  Fed_seriesname.Caption := TPlotSeries(OwnerPlot.Series[FActiveSeriesStyleChoose]).Caption;

  // unitnames
  FedXunit.Caption := TPlotSeries(OwnerPlot.Series[FActiveSeriesStyleChoose]).UnitString[TPlotSeries(OwnerPlot.Series[FActiveSeriesStyleChoose]).XAxis];
  FedYunit.Caption := TPlotSeries(OwnerPlot.Series[FActiveSeriesStyleChoose]).UnitString[TPlotSeries(OwnerPlot.Series[FActiveSeriesStyleChoose]).YAxis];
  IF TPlotSeries(OwnerPlot.Series[FActiveSeriesStyleChoose]) is TXYZPlotSeries THEN
    FedZunit.Caption := TPlotSeries(OwnerPlot.Series[FActiveSeriesStyleChoose]).UnitString[TXYZPlotSeries(OwnerPlot.Series[FActiveSeriesStyleChoose]).ZAxis];     // TODO !

  // Combo Axes
    // empty
      with Fcb_coloraxes.Items do begin
        while Count > 0 do begin
          Fcb_coloraxes.Items.Delete(0);
        end;
      end;
  // refill
  // add items; axes used by the series for colorscale
  Fcb_coloraxes.Items.Add( IntToStr(TPlotSeries(OwnerPlot.Series[FActiveSeriesStyleChoose]).XAxis) + ' - X axis' );
  IF TPlotSeries(OwnerPlot.Series[FActiveSeriesStyleChoose]) is TXYPlotSeries THEN
    Fcb_coloraxes.Items.Add( IntToStr(TPlotSeries(OwnerPlot.Series[FActiveSeriesStyleChoose]).YAxis) + ' - Y axis' );
  IF TPlotSeries(OwnerPlot.Series[FActiveSeriesStyleChoose]) is TXYZPlotSeries THEN
    Fcb_coloraxes.Items.Add( IntToStr(TXYZPlotSeries(OwnerPlot.Series[FActiveSeriesStyleChoose]).ZAxis) + ' - Z axis' );
    // choose itemindex for correct value
  //IF TPlotSeries(OwnerPlot.Series[FActiveSeries]).ColoredAxis =
   //    TPlotSeries(OwnerPlot.Series[FActiveSeries]).XAxis  THEN Fcb_coloraxes.ItemIndex := 0;
  Fcb_coloraxes.ItemIndex := 0; // default to X-axis; every series has an X-axsi (Todo refactoring OwnerAxis to XAxis)
  IF TPlotSeries(OwnerPlot.Series[FActiveSeriesStyleChoose]).ColoredAxis =
       TPlotSeries(OwnerPlot.Series[FActiveSeriesStyleChoose]).YAxis  THEN Fcb_coloraxes.ItemIndex := 1;
  IF TPlotSeries(OwnerPlot.Series[FActiveSeriesStyleChoose]) is TXYZPlotSeries THEN begin
    IF TPlotSeries(OwnerPlot.Series[FActiveSeriesStyleChoose]).ColoredAxis =
         TXYZPlotSeries(OwnerPlot.Series[FActiveSeriesStyleChoose]).ZAxis  THEN Fcb_coloraxes.ItemIndex := 2;
  end;
  //

  // 22.03.13
  // do not autoapply until series-caption bug is evaluated:
  // series caption changes unwantedly when this menu is used with autoapply
  // maybe some init problem with Create or during InitForm ??
  //Fcb_style.OnChange := @Apply;
  //Fcb_stylepoints.OnChange := @Apply;
  //Fcb_stylellines.OnChange := @Apply;
  //Fed_sizepoints.OnChange := @Apply;
  //Fed_sizelines.OnChange := @Apply;
  //Fcb_drawcolored.OnClick := @Apply;
  //Fcb_coloraxes.OnChange := @Apply;

  // interpolation
  Fcb_interpolate.Checked := TPlotSeries(OwnerPlot.Series[FActiveSeriesStyleChoose]).Interpolate;
  if TPlotSeries(OwnerPlot.Series[FActiveSeriesStyleChoose]).Interpolator <> nil then begin
    Fcb_Ipol_Mode.ItemIndex := ord( TInterpolator_PixelXY( TPlotSeries(OwnerPlot.Series[FActiveSeriesStyleChoose]).Interpolator).IpolMode );
    Fcb_Ipol_AxisMode.ItemIndex := ord( TInterpolator_PixelXY( TPlotSeries(OwnerPlot.Series[FActiveSeriesStyleChoose]).Interpolator).IpolAxisMode );
  end;

  FInitInProgress:=FALSE;
  Self.Show;
  //MessageDlg('StyleChoose','TODO: Choose style here', mtInformation, [mbOK],0);
end;


{ THelperFormSeriesMarkersChoose }

procedure THelperFormSeriesMarkersChoose._PropertyChanged(Sender: TObject);
begin
  IF Sender = Fcb_mkrIndex THEN BEGIN
    ShowMarkerProperties(Fcb_mkrIndex.ItemIndex);
  END;
end;

procedure THelperFormSeriesMarkersChoose.AddRemoveMarker(Sender: TObject);
var
  vMarkerIndex: Integer;
  vMarkerContainer: TMarkerContainer;
begin
  vMarkerContainer := TPlotSeries(OwnerPlot.Series[FActiveSeriesMarkersChoose]).MarkerContainer;
  if Sender = Fbtn_Addmarker then begin
    vMarkerIndex := vMarkerContainer.AddMarker;
    Fcb_mkrIndex.Items.Add(IntToStr(vMarkerIndex));
    InitForm(FActiveSeriesMarkersChoose);
    ShowMarkerProperties(vMarkerIndex);
  end else
  if Sender = Fbtn_Removemarker then begin
    vMarkerIndex := vMarkerContainer.RemoveMarker(vMarkerContainer.Marker[Fcb_mkrIndex.ItemIndex]);
    Fcb_mkrIndex.Items.Delete(vMarkerIndex);
    InitForm(FActiveSeriesMarkersChoose);
    ShowMarkerProperties(vMarkerIndex-1);
  end;
end;

procedure THelperFormSeriesMarkersChoose.ShowMarkerProperties(
  AMarkerIndex: Integer);
var
  vLoop: Integer;
  vMarkShapes: set of TMarkerShape;
  vMarkerContainer: TMarkerContainer;
begin
  vMarkerContainer := TPlotSeries(OwnerPlot.Series[FActiveSeriesMarkersChoose]).MarkerContainer;

  if (AMarkerIndex < 0) or (AMarkerIndex > vMarkerContainer.MarkerCount-1) then begin
    Fcb_mkrIndex.ItemIndex := -1;
    Fcb_mkrType.ItemIndex := -1;
    Fcb_mkrMode.ItemIndex := -1;
    for vLoop:=0 to Fcb_mkrStyle.Items.Count-1 do begin
      Fcb_mkrStyle.Checked[vLoop] :=  false;
    end;
    Fed_diameter.Value := 11;
    Fed_linewidth.Value := 1;
    Fbtn_color.ButtonColor :=clRed;
    Fed_markeralpha.Value := 255;
    Fbtn_backgndcolor.ButtonColor :=clCream;
    Fed_backgndalpha.Value:= 255;

    Fed_Xindex.Value := 0;
    Fed_XValue.Text := '0';
    Fed_YValue.Text := '0';
    Fed_ZValue.Text := '0';

    exit;
  end;


  Fcb_mkrIndex.ItemIndex := AMarkerIndex;
  Fcb_mkrType.ItemIndex := ord(vMarkerContainer.Marker[AMarkerIndex].MarkerType);
  Fcb_mkrMode.ItemIndex := ord(vMarkerContainer.Marker[AMarkerIndex].MarkerMode);

  vMarkShapes := vMarkerContainer.Marker[AMarkerIndex].VisualParams.StyleParams.MarkShapes;
  for vLoop:=0 to Fcb_mkrStyle.Items.Count-1 do begin
    Fcb_mkrStyle.Checked[vLoop] :=  TMarkerShape(vLoop) in vMarkShapes;
  end;

  Fed_diameter.Value := vMarkerContainer.Marker[AMarkerIndex].VisualParams.StyleParams.Diameter;
  Fed_linewidth.Value := vMarkerContainer.Marker[AMarkerIndex].VisualParams.StyleParams.LineWidth;
  Fbtn_color.ButtonColor :=vMarkerContainer.Marker[AMarkerIndex].VisualParams.StyleParams.Color;
  Fed_markeralpha.Value := (vMarkerContainer.Marker[AMarkerIndex].VisualParams.StyleParams.Alpha DIV 255);
  Fbtn_backgndcolor.ButtonColor :=vMarkerContainer.Marker[AMarkerIndex].VisualParams.StyleParams.BackGndColor;
  Fed_backgndalpha.Value := (vMarkerContainer.Marker[AMarkerIndex].VisualParams.StyleParams.BackGndAlpha DIV 255);

  Fed_Xindex.Value := vMarkerContainer.Marker[AMarkerIndex].Index ;
  Fed_XValue.Text := FloatToStrF(vMarkerContainer.Marker[AMarkerIndex].FixedValueXYZ.X, ffFixed,4,4);
  Fed_YValue.Text := FloatToStrF(vMarkerContainer.Marker[AMarkerIndex].FixedValueXYZ.Y, ffFixed,4,4);
  Fed_ZValue.Text := FloatToStrF(vMarkerContainer.Marker[AMarkerIndex].FixedValueXYZ.Z, ffFixed,4,4);

end;

procedure THelperFormSeriesMarkersChoose.ApplyMarkerProperties(
  AMarkerIndex: Integer);
var
  vLoop: Integer;
  vMarkShapes: set of TMarkerShape;
  vMarkerContainer: TMarkerContainer;
  //vStyleParams: TMarkerStyleParams;
  vVisualParams: TMarkerVisualParams;
  vXYZValue: TXYZValue;
begin
  if Fcb_mkrIndex.ItemIndex <> AMarkerIndex then begin
    EPlot.Create('MarkerIndex shown and demanded for apply do not match !');
    exit;
  end;


  vMarkerContainer := TPlotSeries(OwnerPlot.Series[FActiveSeriesMarkersChoose]).MarkerContainer;

  vMarkerContainer.Marker[AMarkerIndex].MarkerType := TMarkerType(Fcb_mkrType.ItemIndex) ;
  vMarkerContainer.Marker[AMarkerIndex].MarkerMode := TMarkerMode(Fcb_mkrMode.ItemIndex) ;


  vMarkShapes:=[];
  for vLoop:=0 to Fcb_mkrStyle.Items.Count-1 do begin
    if Fcb_mkrStyle.Checked[vLoop] then  include(vMarkShapes, TMarkerShape(vLoop));
  end;

  vVisualParams := vMarkerContainer.Marker[AMarkerIndex].VisualParams;
  with vVisualParams.StyleParams do begin
    MarkShapes := vMarkShapes;
    Diameter := Fed_diameter.Value;
    LineWidth := Fed_linewidth.Value;
    Color := Fbtn_color.ButtonColor;
    Alpha := Fed_markeralpha.Value * 255;
    BackGndColor := Fbtn_backgndcolor.ButtonColor;
    BackGndAlpha := Fed_backgndalpha.Value * 255;
  end;
  vMarkerContainer.Marker[AMarkerIndex].VisualParams := vVisualParams;

  vMarkerContainer.Marker[AMarkerIndex].Index := Fed_Xindex.Value ;
  vXYZValue.X := StrToFloatDef(Fed_XValue.Text, 0);
  vXYZValue.Y := StrToFloatDef(Fed_YValue.Text, 0);
  vXYZValue.Z := StrToFloatDef(Fed_ZValue.Text, 0);
  vMarkerContainer.Marker[AMarkerIndex].FixedValueXYZ := vXYZValue;
end;

procedure THelperFormSeriesMarkersChoose.InitForm(ASeriesIndex: Integer);
var
  vLoop: Integer;
  vMarkerCount: Integer;
  vMarkerContainer: TMarkerContainer;
begin
  FInitInProgress:=TRUE;
  FActiveSeriesMarkersChoose := ASeriesIndex;

  vMarkerContainer := TPlotSeries(OwnerPlot.Series[FActiveSeriesMarkersChoose]).MarkerContainer;

  vMarkerCount := vMarkerContainer.MarkerCount;

  while (Fcb_mkrType.Items.Count > 0) do Fcb_mkrType.Items.Delete(0);
  for vLoop := ord(Low(TMarkerType)) to ord(high(TMarkerType)) do
    Fcb_mkrType.Items.Add(cMARKER_TYPE_NAMES[TMarkerType(vLoop)]);

  while (Fcb_mkrMode.Items.Count > 0) do Fcb_mkrMode.Items.Delete(0);
  for vLoop := ord(Low(TMarkerMode)) to ord(high(TMarkerMode)) do
    Fcb_mkrMode.Items.Add(cMARKER_MODE_NAMES[TMarkerMode(vLoop)]);

  while (Fcb_mkrStyle.Items.Count > 0) do Fcb_mkrStyle.Items.Delete(0);
  for vLoop := ord(Low(TMarkerShape)) to ord(high(TMarkerShape)) do
    Fcb_mkrStyle.Items.Add(cMARKER_SHAPE_NAMES[TMarkerShape(vLoop)]);

  while (Fcb_mkrIndex.Items.Count > 0) do Fcb_mkrIndex.Items.Delete(0);
  IF vMarkerCount > 0 THEN begin
    for vLoop:=0 to vMarkerContainer.MarkerCount-1 do
      Fcb_mkrIndex.Items.Add(IntToStr(vLoop));

     ShowMarkerProperties(0);
  end;


  FInitInProgress:=FALSE;
  Self.Show;
end;

procedure THelperFormSeriesMarkersChoose.Apply(Sender: TObject);
begin
  // TODO: application handling better !
  ApplyMarkerProperties(Fcb_mkrIndex.ItemIndex);

  inherited Apply(Sender);
end;

constructor THelperFormSeriesMarkersChoose.Create(AOwner: TComponent);
const
  cVSPACE = 6;
  cLEFT = 6;
begin
  inherited Create(AOwner);
  OwnerPlot := TPlot(AOwner);
  Self.Height := 280;
  Self.Width := 340;
  Self.Caption := 'Marker properties';

  Flbl_mkrIndex := TLabel.Create(Self);           // Checkbox Index
  with Flbl_mkrIndex do begin
    Parent := Self;
    BorderSpacing.Left:=cLEFT;
    BorderSpacing.Top:=cVSPACE;
    AnchorSide[akLeft].Side := asrLeft;
    AnchorSide[akLeft].Control := Self;
    AnchorSide[akTop].Side := asrTop;
    AnchorSide[akTop].Control := Self;
    Anchors := [akLeft, akTop];
    Caption := 'Marker: ';
  end;

  Fcb_mkrIndex := TComboBox.Create(Self);
  with Fcb_mkrIndex do begin
    Parent := Self;
    AnchorSide[akLeft].Side := asrRight;
    AnchorSide[akLeft].Control := Flbl_mkrIndex;
    //AnchorSide[akRight].Side := asrLeft;
    //AnchorSide[akRight].Control := Flbl_mkrStyle;
    AnchorSide[akTop].Side := asrCenter;
    AnchorSide[akTop].Control := Flbl_mkrIndex;
    Anchors := [akLeft, akTop];
    //OnChange:=@Apply;
    OnChange:=@_PropertyChanged;
  end;

  Fbtn_RemoveMarker := TButton.Create(self);
  with Fbtn_RemoveMarker do begin
    Parent := Self;
    BorderSpacing.Left:=cLEFT;
    AnchorSide[akLeft].Side := asrRight;
    AnchorSide[akLeft].Control := Fcb_mkrIndex;
    //AnchorSide[akRight].Side := asrLeft;
    //AnchorSide[akRight].Control := Flbl_mkrStyle;
    AnchorSide[akTop].Side := asrCenter;
    AnchorSide[akTop].Control := Flbl_mkrIndex;
    Anchors := [akLeft, akTop];
    Caption := 'Remove';
    OnClick := @AddRemoveMarker;
  end;

  Fbtn_Addmarker := TButton.Create(self);
  with Fbtn_Addmarker do begin
    Parent := Self;
    BorderSpacing.Left:=cLEFT;
    AnchorSide[akLeft].Side := asrRight;
    AnchorSide[akLeft].Control := Fbtn_RemoveMarker;
    //AnchorSide[akRight].Side := asrLeft;
    //AnchorSide[akRight].Control := Flbl_mkrStyle;
    AnchorSide[akTop].Side := asrCenter;
    AnchorSide[akTop].Control := Flbl_mkrIndex;
    Anchors := [akLeft, akTop];
    Caption := 'New marker';
    OnClick := @AddRemoveMarker;
  end;



  // labels line 1
  Flbl_mkrType := TLabel.Create(Self);
  with Flbl_mkrType do begin
    Parent := Self;
    BorderSpacing.Top := cVSPACE;
    AnchorSide[akLeft].Side := asrLeft;
    AnchorSide[akLeft].Control := Flbl_mkrIndex;
    AnchorSide[akTop].Side := asrBottom;
    AnchorSide[akTop].Control := Flbl_mkrIndex;
    Anchors := [akLeft, akTop];
    Caption := 'Type';
  end;

  Flbl_mkrMode := TLabel.Create(Self);
  with Flbl_mkrMode do begin
    Parent := Self;
    BorderSpacing.Left := 72;
    AnchorSide[akLeft].Side := asrRight;
    AnchorSide[akLeft].Control := Flbl_mkrType;
    AnchorSide[akTop].Side := asrCenter;
    AnchorSide[akTop].Control := Flbl_mkrType;
    Anchors := [akLeft, akTop];
    Caption := 'Mode';
  end;


  Fcb_mkrMode := TComboBox.Create(Self);
  with Fcb_mkrMode do begin
    Parent := Self;
    AnchorSide[akLeft].Side := asrLeft;
    AnchorSide[akLeft].Control := Flbl_mkrMode;
    //AnchorSide[akRight].Side := asrLeft;
    //AnchorSide[akRight].Control := Flbl_mkrStyle;
    AnchorSide[akTop].Side := asrBottom;
    AnchorSide[akTop].Control := Flbl_mkrMode;
    Anchors := [akLeft, akTop];
    AutoSize:=true;
    //OnChange:=@Apply;
  end;

  Fcb_mkrType := TComboBox.Create(Self);
  with Fcb_mkrType do begin
    Parent := Self;
    BorderSpacing.Right := 4;
    //AnchorSide[akLeft].Side := asrLeft;
    //AnchorSide[akLeft].Control := Flbl_mkrType;
    AnchorSide[akRight].Side := asrLeft;
    AnchorSide[akRight].Control := Fcb_mkrMode;
    AnchorSide[akTop].Side := asrBottom;
    AnchorSide[akTop].Control := Flbl_mkrType;
    Anchors := [akTop, akRight];
    //OnChange:=@Apply;
    AutoSize:=true;
    //Width:=72;
  end;

  // line 3 index
  Flbl_XIndex := TLabel.Create(Self);
  with Flbl_XIndex do begin
    Parent := Self;
    BorderSpacing.Top := cVSPACE;
    AnchorSide[akLeft].Side := asrLeft;
    AnchorSide[akLeft].Control := Flbl_mkrIndex;
    AnchorSide[akTop].Side := asrBottom;
    AnchorSide[akTop].Control := Fcb_mkrType;
    Anchors := [akLeft, akTop];
    Caption := 'Index';
  end;

  Fed_Xindex := TSpinEdit.Create(Self);
  with Fed_Xindex do begin
    Parent := Self;
    AnchorSide[akLeft].Side := asrRight;
    AnchorSide[akLeft].Control := Flbl_XIndex;
    AnchorSide[akTop].Side := asrCenter;
    AnchorSide[akTop].Control := Flbl_XIndex;
    Anchors := [akLeft, akTop];
    MinValue := 0;
    MaxValue := 15;   // TODO
    Value := 0;
    OnChange:=@Apply;
  end;

  // line 4 values
  Flbl_XYZValues := TLabel.Create(Self);
  with Flbl_XYZValues do begin
    Parent := Self;
    BorderSpacing.Top := cVSPACE;
    AnchorSide[akLeft].Side := asrLeft;
    AnchorSide[akLeft].Control := Flbl_mkrIndex;
    AnchorSide[akTop].Side := asrBottom;
    AnchorSide[akTop].Control := Flbl_XIndex;
    Anchors := [akLeft, akTop];
    Caption := 'Fixed X Y Z';
  end;

  Fed_XValue := TEdit.Create(Self);
  with Fed_XValue do begin
    Parent := Self;
    AnchorSide[akLeft].Side := asrRight;
    AnchorSide[akLeft].Control := Flbl_XYZValues;
    AnchorSide[akTop].Side := asrCenter;
    AnchorSide[akTop].Control := Flbl_XYZValues;
    Anchors := [akLeft, akTop];
    Text:='';
  end;

  Fed_YValue := TEdit.Create(Self);
  with Fed_YValue do begin
    Parent := Self;
    AnchorSide[akLeft].Side := asrRight;
    AnchorSide[akLeft].Control := Fed_XValue;
    AnchorSide[akTop].Side := asrCenter;
    AnchorSide[akTop].Control := Flbl_XYZValues;
    Anchors := [akLeft, akTop];
    Text:='';
  end;

  Fed_ZValue := TEdit.Create(Self);
  with Fed_ZValue do begin
    Parent := Self;
    AnchorSide[akLeft].Side := asrRight;
    AnchorSide[akLeft].Control := Fed_YValue;
    AnchorSide[akTop].Side := asrCenter;
    AnchorSide[akTop].Control := Flbl_XYZValues;
    Anchors := [akLeft, akTop];
    Text:='';
  end;

  // line 5 diamteer / linwidth
  Flbl_diameter := TLabel.Create(Self);
  with Flbl_diameter do begin
    Parent := Self;
    BorderSpacing.Top := 2* cVSPACE;
    AnchorSide[akLeft].Side := asrLeft;
    AnchorSide[akLeft].Control := Flbl_mkrIndex;
    AnchorSide[akTop].Side := asrBottom;
    AnchorSide[akTop].Control := Flbl_XYZValues;
    Anchors := [akLeft, akTop];
    Caption := 'Diameter';
  end;

  Fed_diameter := TSpinEdit.Create(Self);
  with Fed_diameter do begin
    Parent := Self;
    AnchorSide[akLeft].Side := asrRight;
    AnchorSide[akLeft].Control := Flbl_diameter;
    AnchorSide[akTop].Side := asrCenter;
    AnchorSide[akTop].Control := Flbl_diameter;
    Anchors := [akLeft, akTop];
    MinValue := 0;
    MaxValue := 21;
    Value := 11;
  end;

  Flbl_linewidth := TLabel.Create(Self);
  with Flbl_linewidth do begin
    Parent := Self;
    AnchorSide[akLeft].Side := asrRight;
    AnchorSide[akLeft].Control := Fed_diameter;
    AnchorSide[akTop].Side := asrCenter;
    AnchorSide[akTop].Control := Flbl_diameter;
    Anchors := [akLeft, akTop];
    Caption := 'Linewidth';
  end;

  Fed_linewidth := TSpinEdit.Create(Self);
  with Fed_linewidth do begin
    Parent := Self;
    AnchorSide[akLeft].Side := asrRight;
    AnchorSide[akLeft].Control := Flbl_linewidth;
    AnchorSide[akTop].Side := asrCenter;
    AnchorSide[akTop].Control := Flbl_diameter;
    Anchors := [akLeft, akTop];
    MinValue := 0;
    MaxValue := 15;
    Value := 1;
  end;

  // line 6 colors

  Fbtn_color := TColorButton.Create(Self);
  with Fbtn_color do begin
    Parent := Self;
    BorderSpacing.Top := cVSPACE;
    AnchorSide[akLeft].Side := asrLeft;
    AnchorSide[akLeft].Control := Flbl_mkrIndex;
    AnchorSide[akTop].Side := asrBottom;
    AnchorSide[akTop].Control := Flbl_diameter;
    Anchors := [akLeft, akTop];
    Caption:='Marker';
    Width:=96;
  end;

  Fbtn_backgndcolor := TColorButton.Create(Self);
  with Fbtn_backgndcolor do begin
    Parent := Self;
    BorderSpacing.Top := cVSPACE;
    AnchorSide[akLeft].Side := asrLeft;
    AnchorSide[akLeft].Control := Fbtn_color;
    AnchorSide[akTop].Side := asrBottom;
    AnchorSide[akTop].Control := Fbtn_color;
    Anchors := [akLeft, akTop];
    Caption:='Background';
    Width:=96;
  end;

  Fed_markeralpha := TSpinEdit.Create(Self);
  with Fed_markeralpha do begin
    Parent := Self;
    AnchorSide[akLeft].Side := asrRight;
    AnchorSide[akLeft].Control := Fbtn_color;
    AnchorSide[akTop].Side := asrCenter;
    AnchorSide[akTop].Control := Fbtn_color;
    Anchors := [akLeft, akTop];
    MinValue := 0;
    MaxValue := 255;
    Value := 255;
  end;

  Fed_backgndalpha := TSpinEdit.Create(Self);
  with Fed_backgndalpha do begin
    Parent := Self;
    AnchorSide[akLeft].Side := asrRight;
    AnchorSide[akLeft].Control := Fbtn_backgndcolor;
    AnchorSide[akTop].Side := asrCenter;
    AnchorSide[akTop].Control := Fbtn_backgndcolor;
    Anchors := [akLeft, akTop];
    MinValue := 0;
    MaxValue := 255;
    Value := 255;
  end;


  Fcb_mkrStyle := TCheckListBox.Create(Self);
  with Fcb_mkrStyle do begin
    Parent := Self;
    AnchorSide[akLeft].Side := asrRight;
    AnchorSide[akLeft].Control := Fbtn_color;
    //AnchorSide[akRight].Side := asrRight;
    //AnchorSide[akRight].Control := self;
    AnchorSide[akTop].Side := asrTop;
    AnchorSide[akTop].Control := Fbtn_color;
    AnchorSide[akLeft].Side := asrRight;
    AnchorSide[akLeft].Control := Fed_markeralpha;
    Anchors := [akLeft, akTop];
    //AutoSize:=true;
    //OnChange:=@Apply;
  end;

end;

destructor THelperFormSeriesMarkersChoose.Destroy;
begin
  inherited Destroy;
end;


{ THelperFormsPlotRect }
{===========================================================================}

constructor THelperFormsPlotRect.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor THelperFormsPlotRect.Destroy;
begin
  IF FfrmPlotRectStyleChoose <> nil THEN FfrmPlotRectStyleChoose.Free;
  inherited Destroy;
end;

procedure THelperFormsPlotRect.PlotRectStyleChoose(APlotRectIndex: Integer);
begin
  IF FfrmPlotRectStyleChoose = nil THEN FfrmPlotRectStyleChoose := THelperFormPlotRectStyleChoose.Create(OwnerPlot);
  FfrmPlotRectStyleChoose.InitForm(APlotRectIndex);
end;


{ THelperFormPlotRectStyleChoose }
{===========================================================================}


procedure THelperFormPlotRectStyleChoose.Apply(Sender: TObject);
var
  vAxisString, vIndexString: string;                                // TODO: modularize this parser, see uPlotClass helpers
  vAxisIndex: integer;
begin
  vAxisString := '0';
  // Background Color
  IF Sender = Fbtn_color THEN begin
     //writeln('color3 after apply', ColorToString(Fbtn_color.ButtonColor) );
     TPlotRect(OwnerPlot.PlotRect[FActivePlotRect]).Style.Brush.Color := Fbtn_color.ButtonColor;
     //writeln('color3 Brush.color', ColorToString(TPlotRect(OwnerPlot.PlotRect[FActivePlotRect]).Style.Brush.Color) );
  end;
  // Legend
    TPlotRect(OwnerPlot.PlotRect[FActivePlotRect]).ShowLegend := Fcb_legend.Checked;
  // ColorScale : attention: the axis to scale is a colorscale property
  // which series to draw colored referred to an axis is a series property !
    TPlotRect(OwnerPlot.PlotRect[FActivePlotRect]).ShowColorScale := Fcb_colorscale.Checked;
    //
    IF Fcb_axes.ItemIndex > -1 THEN vAxisString := Fcb_axes.Items[Fcb_axes.ItemIndex];
    vIndexString := vAxisString[1];
    IF (Length(vAxisString)>1) THEN
      IF (vAxisString[2] in ['0'..'9']) THEN  vIndexString := vIndexString + vAxisString[2];
    vAxisIndex := StrToIntDef(vIndexString, -1);  //StrToIntDef(vIndexString,-1);
    TPlotRect(OwnerPlot.PlotRect[FActivePlotRect]).ColorScaleRect.ScaleAxisIndex := vAxisIndex;

  OwnerPlot.Repaint;
end;


constructor THelperFormPlotRectStyleChoose.Create(AOwner: TComponent);
const
  cBorderSpace = 36;
begin
  inherited Create(AOwner);
  OwnerPlot := TPlot(AOwner);
  Self.Height := 260;
  Self.Width := 340;
  Self.Caption := 'PlotRect properties';

  // colorbutton
  Fbtn_color := TColorButton.Create(Self); // colorbutton, fixed to form
    Fbtn_color.Parent := Self;
    Fbtn_color.Visible := TRUE;
    Fbtn_color.AnchorSide[akLeft].Side:=asrLeft;
    Fbtn_color.AnchorSide[akLeft].Control:=Self;
    Fbtn_color.Anchors:=Fbtn_color.Anchors+[akLeft];
    Fbtn_color.Top := 28;
    Fbtn_color.Width := 132;
    Fbtn_color.BorderSpacing.Around := cBorderSpace;
    Fbtn_color.Caption := 'Background';
    Fbtn_color.OnColorChanged := @Apply;

  Fcb_legend := TCheckBox.Create(Self);
    Fcb_legend.Parent := Self;
    Fcb_legend.Visible := TRUE;
    Fcb_legend.Checked := FALSE;
    Fcb_legend.AnchorSide[akLeft].Side:=asrLeft;
    Fcb_legend.AnchorSide[akLeft].Control:=Self;
    Fcb_legend.AnchorSide[akTop].Side:=asrBottom;
    Fcb_legend.AnchorSide[akTop].Control:=Fbtn_color;
    Fcb_legend.Anchors:=Fcb_legend.Anchors+[akLeft]+[akTop];
    Fcb_legend.BorderSpacing.Around := cBorderSpace;
    Fcb_legend.Caption := 'Show a legend';
    Fcb_legend.OnClick := @Apply;

  Fcb_colorscale := TCheckBox.Create(Self);
    Fcb_colorscale.Parent := Self;
    Fcb_colorscale.Visible := TRUE;
    Fcb_colorscale.Checked := FALSE;
    Fcb_colorscale.AnchorSide[akLeft].Side:=asrLeft;
    Fcb_colorscale.AnchorSide[akLeft].Control:=Self;
    Fcb_colorscale.AnchorSide[akTop].Side:=asrBottom;
    Fcb_colorscale.AnchorSide[akTop].Control:=Fcb_legend;
    Fcb_colorscale.Anchors:=Fcb_colorscale.Anchors+[akLeft]+[akTop];
    Fcb_colorscale.BorderSpacing.Around := cBorderSpace;
    Fcb_colorscale.Caption := 'Show a ColorScale for axis:';
    Fcb_colorscale.OnClick := @Apply;

  Fcb_axes := TComboBox.Create(Self);
    Fcb_axes.Parent := Self;
    Fcb_axes.Visible := TRUE;
    Fcb_axes.AnchorSide[akLeft].Side:=asrRight;
    Fcb_axes.AnchorSide[akLeft].Control:=Fcb_colorscale;
    Fcb_axes.AnchorSide[akTop].Side:=asrCenter;
    Fcb_axes.AnchorSide[akTop].Control:=Fcb_colorscale;
    Fcb_axes.BorderSpacing.Around := cBorderSpace;
    Fcb_axes.OnChange := @Apply;

  // colorscale adjustments
  TPlotRect(OwnerPlot.PlotRect[FActivePlotRect]).ColorScaleRect.ColorScaleOrientation := aoVertical;
  TPlotRect(OwnerPlot.PlotRect[FActivePlotRect]).ColorScaleRect.ColorScaleWidth := 12;
  TPlotRect(OwnerPlot.PlotRect[FActivePlotRect]).ColorScaleRect.AutoShrink := TRUE;
end;

destructor THelperFormPlotRectStyleChoose.Destroy;
begin
  inherited Destroy;
end;

procedure THelperFormPlotRectStyleChoose.InitForm(APlotRectIndex: Integer);
var
  vLoop: integer;
  vIndex: integer;
  vCBstring: string;
begin
  vLoop := 0; vIndex := 0; vCBstring := '';
  FActivePlotRect := APlotRectIndex;
  Fbtn_color.ButtonColor := TPlotRect(OwnerPlot.PlotRect[FActivePlotRect]).Style.Brush.Color;

  // Combo Axes
    // empty
      with Fcb_axes.Items do begin
        while Count > 0 do begin
          Fcb_axes.Items.Delete(0);
        end;
      end;
    // refill
    with TPlotRect(OwnerPlot.PlotRect[FActivePlotRect]).SeriesContainedIdx do try
        for vLoop := Count-1 downto 0 do begin
           //vSeriesIndex := TPlotSeries(Series[PtrInt(Items[vLoop])]).SeriesIndex;
           // find axes indices
           // X
           vCBstring := '';
           vIndex := (TPlotSeries(OwnerPlot.Series[PInteger(Items[vLoop])^]).XAxis);
           vCBstring := IntToStr(vIndex) + ' - ' +
              TPlotAxis(OwnerPlot.Axis[TPlotSeries(OwnerPlot.Series[PInteger(Items[vLoop])^]).XAxis]).AxisLabel;
           Fcb_axes.Items.Add(vCBstring);
           IF TPlotSeries(OwnerPlot.Series[PInteger(Items[vLoop])^]).ColoredAxis = vIndex THEN Fcb_axes.ItemIndex := 0;
           // Y
           vCBstring := '';
           vIndex := (TPlotSeries(OwnerPlot.Series[PInteger(Items[vLoop])^]).YAxis);
           vCBstring := IntToStr(vIndex) + ' - ' +
              TPlotAxis(OwnerPlot.Axis[TPlotSeries(OwnerPlot.Series[PInteger(Items[vLoop])^]).YAxis]).AxisLabel;
           Fcb_axes.Items.Add(vCBstring);
           IF TPlotSeries(OwnerPlot.Series[PInteger(Items[vLoop])^]).ColoredAxis = vIndex THEN Fcb_axes.ItemIndex := 1;
           // Z
           IF TPlotSeries(OwnerPlot.Series[PInteger(Items[vLoop])^]) is TXYZPlotSeries THEN begin
             vCBstring := '';
             vIndex := (TXYZPlotSeries(OwnerPlot.Series[PInteger(Items[vLoop])^]).ZAxis);
             vCBstring := IntToStr(vIndex) + ' - ' +
                TPlotAxis(OwnerPlot.Axis[TXYZPlotSeries(OwnerPlot.Series[PInteger(Items[vLoop])^]).ZAxis]).AxisLabel;
             Fcb_axes.Items.Add(vCBstring);
           IF TPlotSeries(OwnerPlot.Series[PInteger(Items[vLoop])^]).ColoredAxis = vIndex THEN Fcb_axes.ItemIndex := 2;
           end;
        end;
      finally
        while Count > 0 do begin
          Dispose(Pinteger(Items[0]));
          Delete(0);
        end;
        Free;
      end;
  // Combo axes end

  // do we have legend and colorscale enabled ?
     Fcb_colorscale.Checked := TPlotRect(OwnerPlot.PlotRect[FActivePlotRect]).ShowColorScale;
     Fcb_legend.Checked := TPlotRect(OwnerPlot.PlotRect[FActivePlotRect]).ShowLegend;

  Self.Show;
end;


{ THelperFormsAxes }
{===========================================================================}

constructor THelperFormsAxes.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor THelperFormsAxes.Destroy;
begin
  IF FfrmAxesStyleChoose <> nil THEN FfrmAxesStyleChoose.Free;
  inherited Destroy;
end;

procedure THelperFormsAxes.AxesStyleChoose(AAxis: Integer);
begin
    IF FfrmAxesStyleChoose = nil THEN FfrmAxesStyleChoose := THelperFormAxesStyleChoose.Create(OwnerPlot);
    FfrmAxesStyleChoose.InitForm(AAxis);
end;

{ THelperFormAxesStyleChoose }
{===========================================================================}



procedure THelperFormAxesStyleChoose.Apply(Sender: TObject);
begin
  TPlotAxis(OwnerPlot.Axis[FActiveAxis]).AxisLabel := Fed_axisname.Caption;
  TPlotAxis(OwnerPlot.Axis[FActiveAxis]).NumberFormat :=  TNumberFormat(Fcb_numberformat.ItemIndex);
  TPlotAxis(OwnerPlot.Axis[FActiveAxis]).LogScale := Fcb_logscale.Checked ;
  TPlotAxis(OwnerPlot.Axis[FActiveAxis]).LogBase := Fed_logbase.Value;
  TAxisStyle(TPlotAxis(OwnerPlot.Axis[FActiveAxis]).Style).Color := Fbtn_color.ButtonColor;
  TAxisStyle(TPlotAxis(OwnerPlot.Axis[FActiveAxis]).TickStyle).Color := Fbtn_color.ButtonColor;
  TAxisStyle(TPlotAxis(OwnerPlot.Axis[FActiveAxis]).SubTickStyle).Color := Fbtn_color.ButtonColor;

  TPlotAxis(OwnerPlot.Axis[FActiveAxis]).InnerTicks := Fcb_innerticks.Checked;
  TPlotAxis(OwnerPlot.Axis[FActiveAxis]).InnerSubTicks := Fcb_innersubticks.Checked;
  TPlotAxis(OwnerPlot.Axis[FActiveAxis]).Inner3DTicks := Fcb_InnerTicks3D.Checked;

  OwnerPlot.Repaint;
end;


constructor THelperFormAxesStyleChoose.Create(AOwner: TComponent);
var
  vLoop: integer;
const
  cBorderSpace = 22;
  cLblSpace = 8;
begin
  inherited Create(AOwner);
  OwnerPlot := TPlot(AOwner);
  Self.Height := 340;
  Self.Width := 340;
  Self.Caption := 'Axis properties';

  //// colorbutton
  Fbtn_color := TColorButton.Create(Self); // colorbutton, fixed to form
    Fbtn_color.Parent := Self;
    Fbtn_color.Visible := TRUE;
    Fbtn_color.AnchorSide[akLeft].Side:=asrLeft;
    Fbtn_color.AnchorSide[akLeft].Control:=Self;
    Fbtn_color.Anchors:=Fbtn_color.Anchors+[akLeft];
    Fbtn_color.Top := 28;
    Fbtn_color.Width := 132;
    Fbtn_color.BorderSpacing.Around := cBorderSpace;
    Fbtn_color.Caption := 'Color';
    //Fbtn_color.OnColorChanged := @Apply;
  //
  Fed_axisname := TLabeledEdit.Create(Self);        // Label and edit for axisname
  //Fed_axisname := TEdit.Create(Self);        // Label and edit for axisname
    Fed_axisname.Parent := Self;
    Fed_axisname.Visible:=TRUE;
    Fed_axisname.Enabled:=TRUE;
    Fed_axisname.BorderSpacing.Left := 20;
    Fed_axisname.BorderSpacing.Top := 00;
    Fed_axisname.AnchorSide[akLeft].Side := asrRight;
    Fed_axisname.AnchorSide[akLeft].Control := Fbtn_color;
    Fed_axisname.AnchorSide[akRight].Side := asrRight;
    Fed_axisname.AnchorSide[akRight].Control := Self;
    Fed_axisname.AnchorSide[akTop].Side := asrCenter;
    Fed_axisname.AnchorSide[akTop].Control := Fbtn_color;
    Fed_axisname.Anchors:=Fed_axisname.Anchors+[akLeft]+[akTop]+[akRight];
    Fed_axisname.BorderSpacing.Right := cBorderSpace;
    Fed_axisname.Text := '';
    Fed_axisname.LabelPosition := lpAbove;
    Fed_axisname.LabelSpacing := 3;
    Fed_axisname.EditLabel.Caption := 'Axis Caption';
  //
  Fcb_innerticks := TCheckBox.Create(Self);  // checkbox InnerTicks
  with Fcb_innerticks do begin
    Parent := Self;
    Visible := TRUE;
    Checked := FALSE;
    AnchorSide[akLeft].Side:=asrLeft;
    AnchorSide[akLeft].Control:=Self;
    AnchorSide[akTop].Side:=asrBottom;
    AnchorSide[akTop].Control:=Fbtn_color;
    Anchors:=Fcb_innerticks.Anchors+[akLeft]+[akTop];
    BorderSpacing.Around := cBorderSpace;
    Caption := 'Show ticklines';
    //Fcb_innerticks.OnClick := @Apply;
  end;
  //
  Fcb_innersubticks := TCheckBox.Create(Self);  // checkbox InnerSubTicks
    Fcb_innersubticks.Parent := Self;
    Fcb_innersubticks.Visible := TRUE;
    Fcb_innersubticks.Checked := FALSE;
    Fcb_innersubticks.AnchorSide[akLeft].Side:=asrRight;
    Fcb_innersubticks.AnchorSide[akLeft].Control:=Fcb_innerticks;
    //Fcb_innersubticks.AnchorSide[akRight].Side:=asrRight;
    //Fcb_innersubticks.AnchorSide[akRight].Control:=Self;
    Fcb_innersubticks.AnchorSide[akTop].Side:=asrCenter;
    Fcb_innersubticks.AnchorSide[akTop].Control:=Fcb_innerticks;
    Fcb_innersubticks.Anchors:=Fcb_innersubticks.Anchors+[akLeft]+[akTop]; //+[akRight];
    Fcb_innersubticks.BorderSpacing.Around := cBorderSpace;
    Fcb_innersubticks.Caption := 'Show subticklines';
    //Fcb_innersubticks.OnClick := @Apply;
  Fcb_InnerTicks3D := TCheckBox.Create(Self);  // checkbox InnerTicks3D
  with Fcb_InnerTicks3D do begin
    Parent := Self;
    Visible := TRUE;
    Checked := FALSE;
    AnchorSide[akLeft].Side:=asrRight;
    AnchorSide[akLeft].Control:=Fcb_innersubticks;
    AnchorSide[akTop].Side:=asrCenter;
    AnchorSide[akTop].Control:=Fcb_innersubticks;
    Anchors:=Fcb_innerticks.Anchors+[akLeft]+[akTop];
    BorderSpacing.Around := cBorderSpace;
    Caption := '3D ticks';
    //Fcb_innerticks.OnClick := @Apply;
  end;
  //
  Fcb_logscale := TCheckBox.Create(Self);  // checkbox LogScale
    Fcb_logscale.Parent := Self;
    Fcb_logscale.Visible := TRUE;
    Fcb_logscale.Checked := FALSE;
    Fcb_logscale.AnchorSide[akLeft].Side:=asrLeft;
    Fcb_logscale.AnchorSide[akLeft].Control:=Self;
    Fcb_logscale.AnchorSide[akTop].Side:=asrBottom;
    Fcb_logscale.AnchorSide[akTop].Control:=Fcb_innerticks;
    Fcb_logscale.Anchors:=Fcb_logscale.Anchors+[akLeft]+[akTop];
    Fcb_logscale.BorderSpacing.Around := cBorderSpace;
    Fcb_logscale.Caption := 'LogScale with LogBase -->';
    //Fcb_logscale.OnClick := @Apply;
  //
  Fed_logbase := TSpinEdit.Create(Self);  // spinedit LogBase
    Fed_logbase.Parent := Self;
    Fed_logbase.Visible := TRUE;
    Fed_logbase.AnchorSide[akLeft].Side:=asrRight;
    Fed_logbase.AnchorSide[akLeft].Control:=Fcb_logscale;
    Fed_logbase.AnchorSide[akTop].Side:=asrCenter;
    Fed_logbase.AnchorSide[akTop].Control:=Fcb_logscale;
    Fed_logbase.Anchors:=Fed_logbase.Anchors+[akLeft]+[akTop];
    //Fed_logbase.OnChange := @Apply;
    Fed_logbase.Value := 10;
    Fed_logbase.MinValue := 2;
    Fed_logbase.MaxValue := 20;
    Fed_logbase.Increment := 1;
  //
  Flbl_numformat := TLabel.Create(Self);    // label size
    Flbl_numformat.Parent := Self;
    Flbl_numformat.Visible := TRUE;
    Flbl_numformat.Caption := 'Numberformat';
    Flbl_numformat.AnchorSide[akLeft].Side:=asrLeft;
    Flbl_numformat.AnchorSide[akLeft].Control:=Self;
    Flbl_numformat.AnchorSide[akTop].Side:=asrBottom;
    Flbl_numformat.AnchorSide[akTop].Control:=Fcb_logscale;
    Flbl_numformat.Anchors:=Flbl_numformat.Anchors+[akLeft]+[akTop];
    Flbl_numformat.BorderSpacing.Left := cBorderSpace;

  //
  Fcb_numberformat := TComboBox.Create(Self);  // combo NumberFormat
    Fcb_numberformat.Parent := Self;
    Fcb_numberformat.Visible := TRUE;
    Fcb_numberformat.AnchorSide[akLeft].Side:=asrLeft;
    Fcb_numberformat.AnchorSide[akLeft].Control:=Self;
    Fcb_numberformat.AnchorSide[akRight].Side:=asrRight;
    Fcb_numberformat.AnchorSide[akRight].Control:=Self;
    Fcb_numberformat.AnchorSide[akTop].Side:=asrBottom;
    Fcb_numberformat.AnchorSide[akTop].Control:=Flbl_numformat;
    Fcb_numberformat.Anchors:=Fcb_numberformat.Anchors+[akLeft]+[akTop]+[akRight];
    Fcb_numberformat.BorderSpacing.Left := cBorderSpace;
    Fcb_numberformat.BorderSpacing.Right := cBorderSpace;
    Fcb_numberformat.BorderSpacing.Top := cLblSpace;
    //Fcb_numberformat.OnChange := @Apply;
    for vLoop := ord(nfPlain) to ord(nfEngineeringPrefix) do begin
      Fcb_numberformat.Items.Add(TNumberFormatName[TNumberFormat(vLoop)]);
    end;


end;

destructor THelperFormAxesStyleChoose.Destroy;
begin
  inherited Destroy;
end;

procedure THelperFormAxesStyleChoose.InitForm(AAxis: Integer);
begin
  FActiveAxis := AAxis;

  Fcb_InnerTicks3D.Enabled := (TPlotAxis(OwnerPlot.Axis[FActiveAxis]).Get3DGridAxis <> -1);
  Fcb_InnerTicks3D.Checked := TPlotAxis(OwnerPlot.Axis[FActiveAxis]).Inner3DTicks;

  Fcb_numberformat.ItemIndex := ord(TPlotAxis(OwnerPlot.Axis[FActiveAxis]).NumberFormat);

  Fbtn_color.ButtonColor := TAxisStyle(TPlotAxis(OwnerPlot.Axis[FActiveAxis]).Style).Color;

  Fed_axisname.Text :=  TPlotAxis(OwnerPlot.Axis[FActiveAxis]).AxisLabel;


  Fcb_numberformat.ItemIndex := ord( TPlotAxis(OwnerPlot.Axis[FActiveAxis]).NumberFormat );
  Fcb_logscale.Checked :=  TPlotAxis(OwnerPlot.Axis[FActiveAxis]).LogScale;
  Fed_logbase.Value := round ( TPlotAxis(OwnerPlot.Axis[FActiveAxis]).LogBase );

  Fcb_innerticks.Checked := TPlotAxis(OwnerPlot.Axis[FActiveAxis]).InnerTicks;
  Fcb_innersubticks.Checked := TPlotAxis(OwnerPlot.Axis[FActiveAxis]).InnerSubTicks;

  Self.Show;
end;




end.

