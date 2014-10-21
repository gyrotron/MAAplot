unit uPlotStyles;
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

interface

uses
  Graphics, Types, uPlotClass, GraphMath,
  LCLType, IntfGraphics, FPimage, FPCanvas; // lazcanvas

type

  TPointStyleShape = (shapeDot, shapeCircle, shapeSquareSolid, shapeSquare, shapeCross, shapePlus, shapeMarkerTriangle);

  { TPlotStyles }


  TPlotStyle = class(uPlotClass.TPlotStyleBase)
  private
    procedure SetBrush(const AValue: TBrush);
    procedure SetFont(const AValue: TFont);
    procedure SetPen(const AValue: TPen);
    function GetTextHeight(AText: String; ACanvas: TCanvas): Integer;
    function GetTextWidth(AText: String; ACanvas: TCanvas): Integer;
    // for Dataimage:
    procedure ClipDrawPixel(X, Y: Integer; ADataImage: TLazIntfImage; AColor: TColor);
    procedure ClipDrawPixel(X, Y: Integer; ADataImage: TLazIntfImage; AFPColor: TFPColor;  AAlphaBlend: Boolean = false; AAlphaMergeOnly: Boolean = false);
    // Note: AlphaBlend does alphablending like in TLazINtfImage (alpha untouched, check this)
    //       AlphaMergeOnly additively merges alpha channels but draws full AFPColors !
    procedure ClipDrawLineold(A, B: TPoint; ALineWidth: Integer; ADataImage: TLazIntfImage; AColor: TColor);
    procedure ClipDrawLine(A, B: TPoint; ALineWidth: Integer; ADataImage: TLazIntfImage; AColor: TColor);
    procedure ClipDrawLine(A, B: TPoint; ALineWidth: Integer; ADataImage: TLazIntfImage; AFPColor: TFPColor; AAlphaBlend: Boolean = false; AAlphaMergeOnly: Boolean = false);
  protected
    FBrush: TBrush;
    FColor: TColor;
    FFont: TFont;
    FPen: TPen;

    procedure DrawCircleEx(Pt: TPoint; ADataImage: TLazIntfImage; AColor: TColor; Filled: Boolean; ADiameter: Integer);
    procedure DrawCircleEx(Pt: TPoint; ADataImage: TLazIntfImage; AFPColor: TFPColor; Filled: Boolean; ADiameter: Integer; AAlphaBlend: Boolean = false; AAlphaMergeOnly: Boolean = false);

    procedure SetColor(const AValue: TColor); virtual;
    procedure DrawPoint(Pt: TPoint; Canvas: TCanvas); override; overload;
    procedure DrawPoint(Pt: TPoint; Canvas: TCanvas; AColor: TColor); override; overload;
    procedure DrawPoint(Pt: TPoint; ADataImage: TLazIntfImage); override; overload;
    procedure DrawPoint(Pt: TPoint; ADataImage: TLazIntfImage; AColor: TColor); override; overload;
    procedure DrawPoint(Pt: TPoint; ADataImage: TLazIntfImage; AFPColor: TFPColor;  AAlphaBlend: Boolean = false; AAlphaMergeOnly: Boolean = false);  override;overload;

  public
    constructor Create; override;
    destructor Destroy; override;
    property Color: TColor read FColor write SetColor;
    property Pen: TPen read FPen; //write SetPen;  use assign instead !
    property Font: TFont read FFont; //write SetFont;
    property Brush: TBrush read FBrush; //write SetBrush;
    property TextHeigth[AText: String; ACanvas: TCanvas]: Integer read GetTextHeight;
    property TextWidth[AText: String; ACanvas: TCanvas]: Integer read GetTextWidth;
    procedure DrawText(X,Y: Integer; AText: String; ACanvas: TCanvas);
    procedure DrawTextEx(X, Y: Integer; Angle: Double; AText: String;
      ACanvas: TCanvas);
  end;

  { TSeriesStyle }

  TSeriesStyle = class(uPlotStyles.TPlotStyle)
  private
    FOwnerSeries: uPlotClass.TPlotSeriesBase;
    procedure SetOwnerSeries(ASeries: uPlotClass.TPlotSeriesBase);
  protected
    procedure DrawSamplePoint(Pt: TPoint; Canvas: TCanvas; BeginNew: Boolean); override;
  public
    property OwnerSeries: uPlotClass.TPlotSeriesBase read FOwnerSeries write SetOwnerSeries;
  end;

  { TSeriesStylePoints }

  TSeriesStylePoints = class(uPlotStyles.TSeriesStyle)
  private

    FDiameter: Integer;
    FShape: TPointStyleShape;
    procedure SetDiameter(const AValue: Integer);
    procedure DrawCircle(Pt: TPoint; Canvas: TCanvas; AColor: TColor; Filled: Boolean);
    // LazintFImage with TColor
    procedure DrawCircle(Pt: TPoint; ADataImage: TLazIntfImage; AColor: TColor; Filled: Boolean);
    // LazINtfImage with TFPColor
    procedure DrawCircle(Pt: TPoint; ADataImage: TLazIntfImage; AFPColor: TFPColor; Filled: Boolean; AAlphaBlend: Boolean = false; AAlphaMergeOnly: Boolean = false);

    //
    procedure DrawSquare(Pt: TPoint; Canvas: TCanvas; AColor: TColor; Filled: Boolean);    // TODO: overload for DataImage
    procedure DrawCross(Pt: TPoint; Canvas: TCanvas; AColor: TColor);                      // TODO: overload for DataImage
    procedure DrawPlus(Pt: TPoint; Canvas: TCanvas; AColor: TColor);                       // TODO: overload for DataImage
    procedure DrawMarkerTriangle(Pt: TPoint; Canvas: TCanvas; AColor: TColor; Filled: Boolean); // TODO: overload for DataImage

  protected
    FLastSamplePt: TPoint;

    procedure DrawPoint(Pt: TPoint; Canvas: TCanvas); override; overload;
    procedure DrawPoint(Pt: TPoint; Canvas: TCanvas; AColor: TColor); override; overload;
    // overloads for drawing on TLazIntfImage
    procedure DrawPoint(Pt: TPoint; ADataImage: TLazIntfImage); override; overload;
    procedure DrawPoint(Pt: TPoint; ADataImage: TLazIntfImage; AColor: TColor); override; overload;
    // overloads for drawing on TLazIntfImage with FPColor (01.08.14)
    procedure DrawPoint(Pt: TPoint; ADataImage: TLazIntfImage; AFPColor: TFPColor;  AAlphaBlend: Boolean = false; AAlphaMergeOnly: Boolean = false);  override;overload;
    //
    procedure DrawSamplePoint(Pt: TPoint; Canvas: TCanvas; BeginNew: Boolean); override;
  public
    constructor Create; override;
    property Diameter: Integer read FDiameter write SetDiameter;
    property Shape: TPointStyleShape read FShape write FShape;
  end;

  { TSeriesStyleLines }

  TSeriesStyleLines = class(uPlotStyles.TSeriesStylePoints)
  private
    FLineWidth: Integer;
    procedure SetLineWidth(const AValue: Integer);
  protected
    procedure DrawPoint(Pt: TPoint; Canvas: TCanvas); override; overload;
    procedure DrawPoint(Pt: TPoint; Canvas: TCanvas; AColor: TColor); override; overload;
    // overloads for drawing on TLazIntfImage
    procedure DrawPoint(Pt: TPoint; ADataImage: TLazIntfImage); override; overload;
    procedure DrawPoint(Pt: TPoint; ADataImage: TLazIntfImage; AColor: TColor); override; overload;
    // LazINtfImage with TFPColor
    procedure DrawPoint(Pt: TPoint; ADataImage: TLazIntfImage; AFPColor: TFPColor; AAlphaBlend: Boolean = false; AAlphaMergeOnly: Boolean = false);override;overload;
    procedure DrawSamplePoint(Pt: TPoint; Canvas: TCanvas; BeginNew: Boolean); override;
  public
    constructor Create; override;
    property LineWidth: Integer read FLineWidth write SetLineWidth;
  end;

  { TAxisStyle }

  TAxisStyle = class(uPlotStyles.TPlotStyle)
  private
    FPenInnerGrid: TPen;
    procedure SetPenInnerGrid(const AValue: TPen);
  protected
    procedure SetColor(const AValue: TColor); override;
    procedure DrawPoint(Pt: TPoint; Canvas: TCanvas); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure DrawInnerGridLine(ptA, ptB: TPoint; Canvas: TCanvas);
    procedure DrawLine(ptA, ptB: TPoint; Canvas: TCanvas);
    procedure DrawLine(ptA, ptB: TPoint; ADataImage: TLazIntfImage; AFPColor: TFPColor);  // not used by now 08.08.14!
    property PenInnerGrid: TPen read FPenInnerGrid write SetPenInnerGrid;
  end;

const
  TPointStyleShapeName : array[shapeDot..shapeMarkerTriangle] of String = (
    'Dot', 'Circle', 'Square solid', 'Square', 'Cross', 'Plus', 'MarkerTriangle');


implementation

uses
  uPlotSeries;

resourcestring
  S_PointStyleNotImplemented  = 'This PointStyle is not yet implemented.'#13#10+
                        'Please use Dot instead';

{ TPlotStyle }
// *********************************************************************

procedure TPlotStyle.SetBrush(const AValue: TBrush);
begin
  if FBrush=AValue then exit;
  FBrush:=AValue;
end;

procedure TPlotStyle.SetColor(const AValue: TColor);
begin
  if FColor=AValue then exit;
  FColor:=AValue;
  Pen.Color := Color;
  Brush.Color := Color;
  Font.Color := Color;
end;

procedure TPlotStyle.SetFont(const AValue: TFont);
begin
  if FFont=AValue then exit;
  FFont:=AValue;
end;

procedure TPlotStyle.SetPen(const AValue: TPen);
begin
  if FPen=AValue then exit;
  FPen:=AValue;
end;

function TPlotStyle.GetTextHeight(AText: String; ACanvas: TCanvas): Integer;
begin
  //ACanvas.Pen := Pen;
  //ACanvas.Font := Font;
  Result := ACanvas.TextHeight(AText);
end;

function TPlotStyle.GetTextWidth(AText: String; ACanvas: TCanvas): Integer;
begin
  //ACanvas.Pen := Pen;
  //ACanvas.Font := Font;
  Result := ACanvas.TextWidth(AText);
end;

procedure TPlotStyle.ClipDrawPixel(X, Y: Integer; ADataImage: TLazIntfImage;
  AColor: TColor);
begin
  IF (X >= 0) and (X <= ADataImage.Width-1) and (Y >= 0) and (Y <= ADataImage.Height-1) THEN
    ADataImage.TColors[X, Y] := AColor;
end;

procedure TPlotStyle.ClipDrawPixel(X, Y: Integer; ADataImage: TLazIntfImage;
  AFPColor: TFPColor;  AAlphaBlend: Boolean = false; AAlphaMergeOnly: Boolean = false);
var
  vAlpha, vInverseAlpha: Word;
  CurColor: TFPColor;
begin
  IF (X >= 0) and (X <= ADataImage.Width-1) and (Y >= 0) and (Y <= ADataImage.Height-1) THEN
    //ADataImage.TColors[X, Y] := AColor;
  BEGIN
    if (not AAlphaBlend) and (not AAlphaMergeOnly) then
      ADataImage.Colors[X, Y] := AFPColor
    else
    begin
      // TODO: this is from LazIntfImage... is this correct blending ?

      vAlpha := AFPColor.alpha;
      vInverseAlpha := $FFFF - vAlpha;

      if vAlpha = $FFFF then
      begin
        ADataImage.Colors[X, Y] := AFPColor;
      end
      else if vAlpha > $00 then
      begin
        if AAlphaBlend then begin          // TODO: why is Alpha untouched with AlphaBlend ??
          CurColor := ADataImage.Colors[X, Y];

          CurColor.Red := Round(
            CurColor.Red * vInverseAlpha / $FFFF +
            AFPColor.Red * vAlpha / $FFFF);

          CurColor.Green := Round(
            CurColor.Green * vInverseAlpha / $FFFF +
            AFPColor.Green * vAlpha / $FFFF);

          CurColor.Blue := Round(
            CurColor.Blue * vInverseAlpha / $FFFF +
            AFPColor.Blue * vAlpha / $FFFF);

          ADataImage.Colors[X, Y] := CurColor;
        end else begin                      // here is AAlphaMergeOnly ;
          CurColor := ADataImage.Colors[X, Y];

          CurColor.Red := AFPColor.Red;
          CurColor.Green := AFPColor.green;
          CurColor.Blue := AFPColor.blue;

          // new 02.08.ju: merge alpha channels  otherwise alpha stays as background(=0)
          CurColor.alpha := $FFFF - (round( ($FFFF - CurColor.alpha) / $FFFF + vInverseAlpha)) DIV 2;

          ADataImage.Colors[X, Y] := CurColor;
        end;
      end;
    end;
  END;

end;

procedure TPlotStyle.ClipDrawLineold(A, B: TPoint; ALineWidth: Integer; ADataImage: TLazIntfImage;
  AColor: TColor);
{var
  vX, vY: Integer;
  vLoop: Integer;
  vTempPoint: TPoint;
//begin

// -----------------------------------
  procedure DrawSolidLine (ADataImage: TLazIntfImage; x1,y1, x2,y2:integer; const color:TColor);
  //var PutPixelProc : TPutPixelProc;                    // see also PixTools
    procedure HorizontalLine (x1,x2,y:integer);
      var x, vLoopWidth : integer;
      begin
        for x := x1 to x2 do
          //PutPixelProc (Canv, x,y, color);
          //ClipDrawPixel(x,y, ADataImage, color);
          IF LineWidth = 1 THEN
            ClipDrawPixel(x,y, ADataImage, color) ELSE BEGIN
            for vLoopWidth := (-LineWidth DIV 2) to (LineWidth DIV 2) do
              ClipDrawPixel(x,y+vLoopWidth, ADataImage, color);
          END;
      end;
    procedure VerticalLine (x,y1,y2:integer);
      var y, vLoopWidth : integer;
      begin
        for y := y1 to y2 do
          //PutPixelProc (Canv, x,y, color);
          //ClipDrawPixel(x,y, ADataImage, color);
          IF LineWidth = 1 THEN
            ClipDrawPixel(x,y, ADataImage, color) ELSE BEGIN
            for vLoopWidth := (-LineWidth DIV 2) to (LineWidth DIV 2) do
              ClipDrawPixel(x+vLoopWidth,y, ADataImage, color);
          END;
      end;
    procedure SlopedLine;
      var npixels,xinc1,yinc1,xinc2,yinc2,dx,dy,d,dinc1,dinc2, vLoopWidth : integer;
      procedure initialize;
        begin // precalculations
        dx := abs(x2-x1);
        dy := abs(y2-y1);
        if dx > dy then  // determining independent variable
          begin  // x is independent
          npixels := dx + 1;
          d := (2 * dy) - dx;
          dinc1 := dy * 2;
          dinc2:= (dy - dx) * 2;
          xinc1 := 1;
          xinc2 := 1;
          yinc1 := 0;
          yinc2 := 1;
          end
        else
          begin  // y is independent
          npixels := dy + 1;
          d := (2 * dx) - dy;
          dinc1 := dx * 2;
          dinc2:= (dx - dy) * 2;
          xinc1 := 0;
          xinc2 := 1;
          yinc1 := 1;
          yinc2 := 1;
          end;
        // going into the correct direction
        if x1 > x2 then
          begin
          xinc1 := - xinc1;
          xinc2 := - xinc2;
          end;
        if y1 > y2 then
          begin
          yinc1 := - yinc1;
          yinc2 := - yinc2;
          end;
        end;
      var r,x,y : integer;
      begin
      initialize;
      x := x1;
      y := y1;
      for r := 1 to nPixels do
        begin
        //PutPixelProc (Canv, x,y, color);
        IF LineWidth = 1 THEN
          ClipDrawPixel(x,y, ADataImage, color) ELSE BEGIN
          for vLoopWidth := (-LineWidth DIV 2) to (LineWidth DIV 2) do begin
            IF dx > dy THEN  ClipDrawPixel(x,y+vLoopWidth, ADataImage, color)
            ELSE ClipDrawPixel(x+vLoopWidth,y, ADataImage, color) END;
        END;
        if d < 0 then
          begin
          d := d + dinc1;
          x := x + xinc1;
          y := y + yinc1;
          end
        else
          begin
          d := d + dinc2;
          x := x + xinc2;
          y := y + yinc2;
          end;
        end;
      end;
  begin
    //with canv.pen do
    //  case mode of
    //    pmMerge : PutPixelProc := @PutPixelAnd;
    //    pmMask : PutPixelProc := @PutPixelOr;
    //    pmXor : PutPixelProc := @PutPixelXor;
    //    else PutPixelProc := @PutPixelCopy;
    //  end;
    if x1 = x2 then  // vertical line
      if y1 < y2 then
        VerticalLine (x1, y1, y2)
      else
        VerticalLine (x1, y2, y1)
    else if y1 = y2 then
      if x1 < x2 then
        HorizontalLine (x1, x2, y1)
      else
        HorizontalLine (x2, x1, y1)
    else  // sloped line
      SlopedLine;
  end;
// -----------------------------------
        }
begin


 //DrawSolidLine(ADataImage, A.x, A.y, B.x,B.y, AColor);
 ClipDrawLine(A, B, ALineWidth, ADataImage, AColor);

end;

procedure TPlotStyle.ClipDrawLine(A, B: TPoint; ALineWidth: Integer; ADataImage: TLazIntfImage;
  AColor: TColor);
var
  dx,dy: Integer;
  vXleading: Boolean;
  vLoop: Integer;
  vX1, vX2: Integer;
  vY1, vY2: Integer;
  vPoint: TPoint;
begin
  dx := (B.X-A.X);
  dy := (B.Y-A.Y);
  IF (dx = 0) and (dy = 0 ) THEN begin
    ClipDrawPixel(A.X, A.Y, ADataImage,AColor);
    exit;
  end;
  IF abs(dx) > abs(dy) THEN vXleading := TRUE ELSE vXleading := FALSE;

  IF vXleading THEN BEGIN
    IF B.X > A.X THEN begin
      vX1 := A.X;
      vY1 := A.Y;
      vX2 := B.X;
    end else begin
      vX1 := B.X;
      vY1 := B.Y;
      vX2 := A.X;
    end;

    for vLoop := vX1 to vX2 do begin
      vY2 := trunc(dy/dx * (vLoop-vX1) + 1) + vY1;
      vPoint.X := vLoop; vPoint.Y := vY2;
      DrawCircleEx(vPoint, ADataImage, AColor, TRUE, ALineWidth);
    end;
  END ELSE BEGIN
    IF B.Y > A.Y THEN begin
      vY1 := A.Y;
      vX1 := A.X;
      vY2 := B.Y;
    end else begin
      vY1 := B.Y;
      vX1 := B.X;
      vY2 := A.Y;
    end;

    for vLoop := vY1 to vY2 do begin
      vX2 := trunc(dx/dy * (vLoop-vY1)) + vX1;
      vPoint.X := vX2; vPoint.Y := vLoop;
      DrawCircleEx(vPoint, ADataImage, AColor, TRUE, ALineWidth);
    end;
  END;
end;

procedure TPlotStyle.ClipDrawLine(A, B: TPoint; ALineWidth: Integer; ADataImage: TLazIntfImage;
  AFPColor: TFPColor; AAlphaBlend: Boolean; AAlphaMergeOnly: Boolean);
var
  dx,dy: Integer;
  vXleading: Boolean;
  vLoop: Integer;
  vX1, vX2: Integer;
  vY1, vY2: Integer;
  vPoint: TPoint;
begin
  dx := (B.X-A.X);
  dy := (B.Y-A.Y);
  IF (dx = 0) and (dy = 0 ) THEN begin
    ClipDrawPixel(A.X, A.Y, ADataImage,AFPColor, AAlphaBlend, AAlphaMergeOnly);
    exit;
  end;
  IF abs(dx) > abs(dy) THEN vXleading := TRUE ELSE vXleading := FALSE;

  IF vXleading THEN BEGIN
    IF B.X > A.X THEN begin
      vX1 := A.X;
      vY1 := A.Y;
      vX2 := B.X;
    end else begin
      vX1 := B.X;
      vY1 := B.Y;
      vX2 := A.X;
    end;

    for vLoop := vX1 to vX2 do begin
      vY2 := trunc(dy/dx * (vLoop-vX1) + 1) + vY1;
      vPoint.X := vLoop; vPoint.Y := vY2;
      DrawCircleEx(vPoint, ADataImage, AFPColor, TRUE, ALineWidth, AAlphaBlend, AAlphaMergeOnly);
    end;
  END ELSE BEGIN
    IF B.Y > A.Y THEN begin
      vY1 := A.Y;
      vX1 := A.X;
      vY2 := B.Y;
    end else begin
      vY1 := B.Y;
      vX1 := B.X;
      vY2 := A.Y;
    end;

    for vLoop := vY1 to vY2 do begin
      vX2 := trunc(dx/dy * (vLoop-vY1)) + vX1;
      vPoint.X := vX2; vPoint.Y := vLoop;
      DrawCircleEx(vPoint, ADataImage, AFPColor, TRUE, ALineWidth, AAlphaBlend, AAlphaMergeOnly);
    end;
  END;
end;

procedure TPlotStyle.DrawCircleEx(Pt: TPoint; ADataImage: TLazIntfImage;
  AColor: TColor; Filled: Boolean; ADiameter: Integer);
var
  vRadius, vRy: Integer;
  vLoop, vLoopy: Integer;
  vLastY: Integer;
begin
  CASE ADiameter of
  0:   exit;
  1,2:   ClipDrawPixel(Pt.X, Pt.Y, ADataImage, AColor);
  3,4:   begin
         ClipDrawPixel(Pt.X-1, Pt.Y, ADataImage, AColor);
         ClipDrawPixel(Pt.X+1, Pt.Y, ADataImage, AColor);
         ClipDrawPixel(Pt.X, Pt.Y-1, ADataImage, AColor);
         ClipDrawPixel(Pt.X, Pt.Y+1, ADataImage, AColor);
         if Filled then begin
           ClipDrawPixel(Pt.X, Pt.Y, ADataImage, AColor);
         end;
       end;
  5:   begin
         ClipDrawPixel(Pt.X-2, Pt.Y, ADataImage, AColor);
         ClipDrawPixel(Pt.X+2, Pt.Y, ADataImage, AColor);
         ClipDrawPixel(Pt.X-1, Pt.Y-1, ADataImage, AColor);
         ClipDrawPixel(Pt.X+1, Pt.Y-1, ADataImage, AColor);
         ClipDrawPixel(Pt.X, Pt.Y-2, ADataImage, AColor);
         ClipDrawPixel(Pt.X-1, Pt.Y+1, ADataImage, AColor);
         ClipDrawPixel(Pt.X+1, Pt.Y+1, ADataImage, AColor);
         ClipDrawPixel(Pt.X, Pt.Y+2, ADataImage, AColor);
         if Filled then begin
           ClipDrawPixel(Pt.X, Pt.Y, ADataImage, AColor);  ;
           ClipDrawPixel(Pt.X-1, Pt.Y, ADataImage, AColor);
           ClipDrawPixel(Pt.X+1, Pt.Y, ADataImage, AColor);
           ClipDrawPixel(Pt.X, Pt.Y-1, ADataImage, AColor);
           ClipDrawPixel(Pt.X, Pt.Y+1, ADataImage, AColor);
         end;
       end;
  END;
  IF ADiameter < 6 THEN exit ELSE BEGIN
    vRadius := ADiameter DIV 2;

    vLastY := vRadius;

    IF Filled THEN BEGIN
      for vLoop := -vRadius to vRadius do begin
        vRy := trunc( sqrt( sqr(vRadius) - sqr(vLoop)));
        for vLoopy := -vRy to vRy do ClipDrawPixel(Pt.x+ vLoop, Pt.y+vLoopy, ADataImage, AColor);
      end;
    END ELSE BEGIN
      for vLoop := 0 to vRadius do begin
        vRy := round( sqrt( sqr(vRadius) - sqr(vLoop)));
          for vLoopy := vLastY downto vRy do begin
            ClipDrawPixel(Pt.x+ vLoop, Pt.y-vLoopy, ADataImage, AColor);  // Q1
            ClipDrawPixel(Pt.x- vLoop, Pt.y-vLoopy, ADataImage, AColor);  // Q2
            ClipDrawPixel(Pt.x- vLoop, Pt.y+vLoopy, ADataImage, AColor);  // Q3
            ClipDrawPixel(Pt.x+ vLoop, Pt.y+vLoopy, ADataImage, AColor);  // Q4
          end;
        vLastY := vRy;
      end;
    END;
  END;
end;

procedure TPlotStyle.DrawCircleEx(Pt: TPoint; ADataImage: TLazIntfImage;
  AFPColor: TFPColor; Filled: Boolean; ADiameter: Integer;
  AAlphaBlend: Boolean; AAlphaMergeOnly: Boolean);
var
  vRadius, vRy: Integer;
  vLoop, vLoopy: Integer;                                                              // TODO FPColor
  vLastY: Integer;
begin
  CASE ADiameter of
  0:   exit;
  1,2:   ClipDrawPixel(Pt.X, Pt.Y, ADataImage, AFPColor, AAlphaBlend, AAlphaMergeOnly);
  3,4:   begin
         ClipDrawPixel(Pt.X-1, Pt.Y, ADataImage, AFPColor, AAlphaBlend, AAlphaMergeOnly);
         ClipDrawPixel(Pt.X+1, Pt.Y, ADataImage, AFPColor, AAlphaBlend, AAlphaMergeOnly);
         ClipDrawPixel(Pt.X, Pt.Y-1, ADataImage, AFPColor, AAlphaBlend, AAlphaMergeOnly);
         ClipDrawPixel(Pt.X, Pt.Y+1, ADataImage, AFPColor, AAlphaBlend, AAlphaMergeOnly);
         if Filled then begin
           ClipDrawPixel(Pt.X, Pt.Y, ADataImage, AFPColor, AAlphaBlend, AAlphaMergeOnly);
         end;
       end;
  5:   begin
         ClipDrawPixel(Pt.X-2, Pt.Y, ADataImage, AFPColor, AAlphaBlend, AAlphaMergeOnly);
         ClipDrawPixel(Pt.X+2, Pt.Y, ADataImage, AFPColor, AAlphaBlend, AAlphaMergeOnly);
         ClipDrawPixel(Pt.X-1, Pt.Y-1, ADataImage, AFPColor, AAlphaBlend, AAlphaMergeOnly);
         ClipDrawPixel(Pt.X+1, Pt.Y-1, ADataImage, AFPColor, AAlphaBlend, AAlphaMergeOnly);
         ClipDrawPixel(Pt.X, Pt.Y-2, ADataImage, AFPColor, AAlphaBlend, AAlphaMergeOnly);
         ClipDrawPixel(Pt.X-1, Pt.Y+1, ADataImage, AFPColor, AAlphaBlend, AAlphaMergeOnly);
         ClipDrawPixel(Pt.X+1, Pt.Y+1, ADataImage, AFPColor, AAlphaBlend, AAlphaMergeOnly);
         ClipDrawPixel(Pt.X, Pt.Y+2, ADataImage, AFPColor, AAlphaBlend, AAlphaMergeOnly);
         if Filled then begin
           ClipDrawPixel(Pt.X, Pt.Y, ADataImage, AFPColor, AAlphaBlend, AAlphaMergeOnly);
           ClipDrawPixel(Pt.X-1, Pt.Y, ADataImage, AFPColor, AAlphaBlend, AAlphaMergeOnly);
           ClipDrawPixel(Pt.X+1, Pt.Y, ADataImage, AFPColor, AAlphaBlend, AAlphaMergeOnly);
           ClipDrawPixel(Pt.X, Pt.Y-1, ADataImage, AFPColor, AAlphaBlend, AAlphaMergeOnly);
           ClipDrawPixel(Pt.X, Pt.Y+1, ADataImage, AFPColor, AAlphaBlend, AAlphaMergeOnly);
         end;
       end;
  END;
  IF ADiameter < 6 THEN exit ELSE BEGIN
    vRadius := ADiameter DIV 2;

    vLastY := vRadius;

    IF Filled THEN BEGIN
      for vLoop := -vRadius to vRadius do begin
        vRy := trunc( sqrt( sqr(vRadius) - sqr(vLoop)));
        for vLoopy := -vRy to vRy do ClipDrawPixel(Pt.x+ vLoop, Pt.y+vLoopy, ADataImage, AFPColor, AAlphaBlend, AAlphaMergeOnly);
      end;
    END ELSE BEGIN
      for vLoop := 0 to vRadius do begin
        vRy := round( sqrt( sqr(vRadius) - sqr(vLoop)));
          for vLoopy := vLastY downto vRy do begin
            ClipDrawPixel(Pt.x+ vLoop, Pt.y-vLoopy, ADataImage, AFPColor, AAlphaBlend, AAlphaMergeOnly);  // Q1
            ClipDrawPixel(Pt.x- vLoop, Pt.y-vLoopy, ADataImage, AFPColor, AAlphaBlend, AAlphaMergeOnly);  // Q2
            ClipDrawPixel(Pt.x- vLoop, Pt.y+vLoopy, ADataImage, AFPColor, AAlphaBlend, AAlphaMergeOnly);  // Q3
            ClipDrawPixel(Pt.x+ vLoop, Pt.y+vLoopy, ADataImage, AFPColor, AAlphaBlend, AAlphaMergeOnly);  // Q4
          end;
        vLastY := vRy;
      end;
    END;
  END;
end;


procedure TPlotStyle.DrawPoint(Pt: TPoint; Canvas: TCanvas);
begin
  //Canvas.Pixels[Pt.X, Pt.Y] := FColor;
  DrawPoint(Pt, Canvas, FColor);
end;

procedure TPlotStyle.DrawPoint(Pt: TPoint; Canvas: TCanvas; AColor: TColor);
begin
  Canvas.Pixels[Pt.X, Pt.Y] := AColor;
end;

procedure TPlotStyle.DrawPoint(Pt: TPoint; ADataImage: TLazIntfImage);
begin
  DrawPoint(Pt, ADataImage, FColor);
end;

procedure TPlotStyle.DrawPoint(Pt: TPoint; ADataImage: TLazIntfImage;
  AColor: TColor);
begin
  //ADataImage.TColors[Pt.X, Pt.Y] := AColor;
  ClipDrawPixel(Pt.x, Pt.y, ADataImage, AColor);
end;

procedure TPlotStyle.DrawPoint(Pt: TPoint; ADataImage: TLazIntfImage;
  AFPColor: TFPColor; AAlphaBlend: Boolean; AAlphaMergeOnly: Boolean = false);
begin
  ClipDrawPixel(Pt.x, Pt.y, ADataImage, AFPColor, AAlphaBlend, AAlphaMergeOnly);
end;


constructor TPlotStyle.Create;
begin
  inherited Create;
  FColor := clBlack;
  FPen := TPen.Create;
  FPen.Width := 1;
  FPen.Style := psSolid;
  FPen.Color := clBlack;
  FFont := TFont.Create;
  Font.Color:= clBlack;
  Font.Size := 10;
  FBrush:= TBrush.Create;
  FBrush.Style:=bsSolid;
  FBrush.Color:= clBlack;
end;

destructor TPlotStyle.Destroy;
begin
  FPen.Free;
  FFont.Free;
  FBrush.Free;
  inherited Destroy;
end;

procedure TPlotStyle.DrawText(X, Y: Integer; AText: String; ACanvas: TCanvas);
begin
   DrawTextEx(X, Y, 0, AText, ACanvas);
end;

procedure TPlotStyle.DrawTextEx(X, Y: Integer; Angle: Double; AText: String; ACanvas: TCanvas);
var vBrush: TBrush;
begin
  //ACanvas.Pen := Pen;
  vBrush := TBrush.Create;
  try
    vBrush.Assign(ACanvas.Brush);
    try
      ACanvas.Brush.Color := clNone;
      ACanvas.Brush.Style := bsClear;
      ACanvas.Font := Font;
      ACanvas.Font.Orientation := Round(Angle * 10);

      ACanvas.TextOut(x,y,AText);
    finally ACanvas.Brush.Assign(vBrush); end;
  finally vBrush.Free; end;
end;


{ TAxisStyle }
// *********************************************************************

procedure TAxisStyle.SetPenInnerGrid(const AValue: TPen);
begin
  if FPen=AValue then exit;
  FPen:=AValue;
end;

procedure TAxisStyle.SetColor(const AValue: TColor);
begin
  inherited SetColor(AValue);
  PenInnerGrid.Color := Color;
end;

constructor TAxisStyle.Create;
begin
  inherited Create;
  FPenInnerGrid := TPen.Create;
  PenInnerGrid.Width := 1;
  PenInnerGrid.Style := psDot;
  PenInnerGrid.Color := clBlack;
end;

destructor TAxisStyle.Destroy;
begin
  FPenInnerGrid.Free;
  inherited Destroy;
end;

procedure TAxisStyle.DrawInnerGridLine(ptA, ptB: TPoint; Canvas: TCanvas);
begin
  Canvas.Pen.Assign(Self.PenInnerGrid);
  Canvas.Line(ptA, ptB);
end;

procedure TAxisStyle.DrawPoint(Pt: TPoint; Canvas: TCanvas);
begin
  Canvas.Pen.Assign(TPlotStyle(Self).Pen);
  inherited DrawPoint(Pt, Canvas);
end;

procedure TAxisStyle.DrawLine(ptA, ptB: TPoint; Canvas: TCanvas);
begin
  Canvas.Pen.Assign(uPlotStyles.TPlotStyle(Self).Pen);
  Canvas.Line(ptA, ptB);
end;

procedure TAxisStyle.DrawLine(ptA, ptB: TPoint; ADataImage: TLazIntfImage;
  AFPColor: TFPColor);
begin
  ClipDrawLine(ptA, ptB, 1, ADataImage, AFPColor, false, false);
end;


{ TSeriesStylePoints }
// *********************************************************************

procedure TSeriesStylePoints.SetDiameter(const AValue: Integer);
begin
  if FDiameter=AValue then exit;
  IF odd(AValue) THEN FDiameter:=AValue ELSE FDiameter := AValue-1; // round down to odd number
end;

procedure TSeriesStylePoints.DrawCircle(Pt: TPoint; Canvas: TCanvas;
  AColor: TColor; Filled: Boolean);
var
  vRadius: Integer;
begin
  CASE Diameter of
  0:   exit;
  1:   Canvas.Pixels[Pt.X, Pt.Y] := AColor;
  3:   begin
         Canvas.Pixels[Pt.X-1, Pt.Y] := AColor;
         Canvas.Pixels[Pt.X+1, Pt.Y] := AColor;
         Canvas.Pixels[Pt.X, Pt.Y-1] := AColor;
         Canvas.Pixels[Pt.X, Pt.Y+1] := AColor;
         if Filled then begin
           Canvas.Pixels[Pt.X, Pt.Y] := AColor;
         end;
       end;
  5:   begin
         Canvas.Pixels[Pt.X-2, Pt.Y] := AColor;
         Canvas.Pixels[Pt.X+2, Pt.Y] := AColor;
         Canvas.Pixels[Pt.X-1, Pt.Y-1] := AColor;
         Canvas.Pixels[Pt.X+1, Pt.Y-1] := AColor;
         Canvas.Pixels[Pt.X, Pt.Y-2] := AColor;
         Canvas.Pixels[Pt.X-1, Pt.Y+1] := AColor;
         Canvas.Pixels[Pt.X+1, Pt.Y+1] := AColor;
         Canvas.Pixels[Pt.X, Pt.Y+2] := AColor;
         if Filled then begin
           Canvas.Pixels[Pt.X, Pt.Y] := AColor;
           Canvas.Pixels[Pt.X-1, Pt.Y] := AColor;
           Canvas.Pixels[Pt.X+1, Pt.Y] := AColor;
           Canvas.Pixels[Pt.X, Pt.Y-1] := AColor;
           Canvas.Pixels[Pt.X, Pt.Y+1] := AColor;
         end;
       end;
  END;
  IF Diameter < 6 THEN exit ELSE BEGIN
    vRadius := Diameter DIV 2;
    Canvas.Pen.Color := AColor;
    Canvas.Pen.Width := 1;
    Canvas.Pen.Mode := pmCopy;
    //Canvas.Pen.Assign(TPlotStyle(Self).Pen);
    // set brush for filled ellipse
    IF Filled THEN BEGIN
      Canvas.Brush := Brush;
      Canvas.Brush.Color:=AColor;
    END ELSE Canvas.Brush.Style := bsClear;
    //Canvas.Brush.Assign(TPlotStyle(Self).Brush);
    Canvas.Ellipse(Pt.X-vRadius, Pt.Y-vRadius, Pt.X+vRadius, Pt.Y+vRadius);  // we do not use EllipseC as radius is integer and we dont know if diameter is odd then
  END;
end;

procedure TSeriesStylePoints.DrawCircle(Pt: TPoint; ADataImage: TLazIntfImage;
  AColor: TColor; Filled: Boolean);            // TODO: implement
begin
  DrawCircleEx(Pt, ADataImage, AColor, Filled, Diameter);
end;


procedure TSeriesStylePoints.DrawCircle(Pt: TPoint; ADataImage: TLazIntfImage;
  AFPColor: TFPColor; Filled: Boolean;  AAlphaBlend: Boolean = false; AAlphaMergeOnly: Boolean = false);
begin
  DrawCircleEx(Pt, ADataImage, AFPColor, Filled, Diameter, AAlphaBlend, AAlphaMergeOnly);
end;


procedure TSeriesStylePoints.DrawSquare(Pt: TPoint; Canvas: TCanvas;
  AColor: TColor; Filled: Boolean);
var vRadius: Integer;
begin
    vRadius := Diameter DIV 2;
  //raise EPlot.CreateRes(@S_PointStyleNotImplemented);
  // set pen properties
    Canvas.Pen.Color := AColor;
    Canvas.Pen.Width := (Diameter DIV 8) + 1;
    Canvas.Pen.Mode := pmCopy;
  // set brush properties
    IF Filled THEN BEGIN
      Canvas.Brush := Brush;
      Canvas.Brush.Color:=AColor;
    END ELSE Canvas.Brush.Style := bsClear;
  Canvas.Rectangle(Pt.X - vRadius, Pt.Y - vRadius, Pt.X + vRadius, Pt.Y + vRadius);
end;

procedure TSeriesStylePoints.DrawCross(Pt: TPoint; Canvas: TCanvas;
  AColor: TColor);
var vRadius: Integer;
begin
  CASE Diameter of
  0:   exit;
  1:   Canvas.Pixels[Pt.X, Pt.Y] := AColor;
  3:   begin
         Canvas.Pixels[Pt.X, Pt.Y] := AColor;
         Canvas.Pixels[Pt.X-1, Pt.Y-1] := AColor;
         Canvas.Pixels[Pt.X+1, Pt.Y-1] := AColor;
         Canvas.Pixels[Pt.X-1, Pt.Y+1] := AColor;
         Canvas.Pixels[Pt.X+1, Pt.Y+1] := AColor;
       end;
  5:   begin
         Canvas.Pixels[Pt.X, Pt.Y] := AColor;
         Canvas.Pixels[Pt.X-1, Pt.Y-1] := AColor;
         Canvas.Pixels[Pt.X+1, Pt.Y-1] := AColor;
         Canvas.Pixels[Pt.X-1, Pt.Y+1] := AColor;
         Canvas.Pixels[Pt.X+1, Pt.Y+1] := AColor;
         Canvas.Pixels[Pt.X-2, Pt.Y-2] := AColor;
         Canvas.Pixels[Pt.X+2, Pt.Y-2] := AColor;
         Canvas.Pixels[Pt.X-2, Pt.Y+2] := AColor;
         Canvas.Pixels[Pt.X+2, Pt.Y+2] := AColor;
       end;
  END;
  IF Diameter < 6 THEN exit ELSE BEGIN
    vRadius := Diameter DIV 2;
    Canvas.Pen.Color := AColor;
    Canvas.Pen.Width := (Diameter DIV 8) + 1;
    Canvas.Pen.Mode := pmCopy;
    Canvas.Line(Pt.X - vRadius, Pt.Y - vRadius, Pt.X + vRadius, Pt.Y + vRadius);
    Canvas.Line(Pt.X - vRadius, Pt.Y + vRadius, Pt.X + vRadius, Pt.Y - vRadius);
  END;
end;

procedure TSeriesStylePoints.DrawPlus(Pt: TPoint; Canvas: TCanvas;
  AColor: TColor);
var vRadius: Integer;
begin
  CASE Diameter of
  0:   exit;
  1:   Canvas.Pixels[Pt.X, Pt.Y] := AColor;
  3:   begin
         Canvas.Pixels[Pt.X, Pt.Y] := AColor;
         Canvas.Pixels[Pt.X, Pt.Y-1] := AColor;
         Canvas.Pixels[Pt.X, Pt.Y+1] := AColor;
         Canvas.Pixels[Pt.X-1, Pt.Y] := AColor;
         Canvas.Pixels[Pt.X+1, Pt.Y] := AColor;
       end;
  5:   begin
         Canvas.Pixels[Pt.X, Pt.Y] := AColor;
         Canvas.Pixels[Pt.X, Pt.Y-1] := AColor;
         Canvas.Pixels[Pt.X, Pt.Y+1] := AColor;
         Canvas.Pixels[Pt.X-1, Pt.Y] := AColor;
         Canvas.Pixels[Pt.X+1, Pt.Y] := AColor;
         Canvas.Pixels[Pt.X-2, Pt.Y] := AColor;
         Canvas.Pixels[Pt.X+2, Pt.Y] := AColor;
         Canvas.Pixels[Pt.X, Pt.Y-2] := AColor;
         Canvas.Pixels[Pt.X, Pt.Y+2] := AColor;
       end;
  END;
  IF Diameter < 6 THEN exit ELSE BEGIN
    vRadius := Diameter DIV 2;
    Canvas.Pen.Color := AColor;
    Canvas.Pen.Width := (Diameter DIV 8) + 1;
    Canvas.Pen.Mode := pmCopy;
    Canvas.Line(Pt.X - vRadius, Pt.Y, Pt.X + vRadius, Pt.Y);
    Canvas.Line(Pt.X, Pt.Y + vRadius, Pt.X, Pt.Y - vRadius);
  END;
end;

procedure TSeriesStylePoints.DrawMarkerTriangle(Pt: TPoint; Canvas: TCanvas;
  AColor: TColor; Filled: Boolean);
var vPointArray: array of TPoint;
begin
  //raise EPlot.CreateRes(@S_PointStyleNotImplemented);
  // set pen properties
    Canvas.Pen.Color := AColor;
    Canvas.Pen.Width := (Diameter DIV 8) + 1;
    Canvas.Pen.Mode := pmCopy;
  // set brush properties
    IF Filled THEN BEGIN
      Canvas.Brush := Brush;
      Canvas.Brush.Color:=AColor;
    END ELSE Canvas.Brush.Style := bsClear;
  setlength(vPointArray, 3);
  vPointArray[0].X := Pt.X;
  vPointArray[0].Y := Pt.Y;
  vPointArray[1].X := Pt.X - (Diameter DIV 2);  // cos 30°
  vPointArray[1].Y := Pt.Y - Diameter; // sin 60° is 0,86 not 1 so we are 14% high
  vPointArray[2].X := Pt.X + (Diameter DIV 2);  // cos 30°
  vPointArray[2].Y := Pt.Y - Diameter;

  Canvas.Polygon(vPointArray);  // other overloaded funcs available
end;


constructor TSeriesStylePoints.Create;
begin
  inherited Create;
  FDiameter := 1;
  FShape := shapeDot;
end;

procedure TSeriesStylePoints.DrawPoint(Pt: TPoint; Canvas: TCanvas);
begin
  //inherited DrawPoint(Pt, Canvas);      // TODO: use colored routine instead and call with AColor = FColor
  DrawPoint(Pt, Canvas, Color);
end;

procedure TSeriesStylePoints.DrawPoint(Pt: TPoint; Canvas: TCanvas; AColor: TColor
  );
begin
// we have (Dot, Circle, Square, Cross, Plus, MarkerTriangle)
  IF Diameter = 0 THEN exit;
  CASE Shape OF
    shapeDot:             BEGIN
                            DrawCircle(Pt, Canvas, AColor, TRUE);
                          END;
    shapeCircle:          BEGIN
                            DrawCircle(Pt, Canvas, AColor, FALSE);
                          END;
    shapeSquare:          BEGIN
                            DrawSquare(Pt, Canvas, AColor, FALSE);
                          END;
    shapeSquareSolid:     BEGIN
                            DrawSquare(Pt, Canvas, AColor, TRUE);
                          END;
    shapeCross:           BEGIN
                            DrawCross(Pt, Canvas, AColor);
                          END;
    shapePlus:            BEGIN
                            DrawPlus(Pt, Canvas, AColor);
                          END;
    shapeMarkerTriangle:  BEGIN
                            DrawMarkerTriangle(Pt, Canvas, AColor, FALSE);
                          END;
  END;
end;

procedure TSeriesStylePoints.DrawPoint(Pt: TPoint; ADataImage: TLazIntfImage);
begin
  DrawPoint(Pt, ADataImage, FColor);
end;

procedure TSeriesStylePoints.DrawPoint(Pt: TPoint; ADataImage: TLazIntfImage;
  AColor: TColor);
begin
// we have (Dot, Circle, Square, Cross, Plus, MarkerTriangle)
  IF Diameter = 0 THEN exit;
  CASE Shape OF
    shapeDot:             BEGIN
                            DrawCircle(Pt, ADataImage, AColor, TRUE);
                          END;
    shapeCircle:          BEGIN
                            DrawCircle(Pt, ADataImage, AColor, FALSE);
                          END;
    //shapeSquare:          BEGIN
    //                        DrawSquare(Pt, ADataImage, AColor, FALSE);
    //                      END;
    //shapeSquareSolid:     BEGIN
    //                        DrawSquare(Pt, ADataImage, AColor, TRUE);
    //                      END;
    //shapeCross:           BEGIN
    //                        DrawCross(Pt, ADataImage, AColor);
    //                      END;
    //shapePlus:            BEGIN
    //                        DrawPlus(Pt, ADataImage, AColor);
    //                      END;
    //shapeMarkerTriangle:  BEGIN
    //                        DrawMarkerTriangle(Pt, ADataImage, AColor, FALSE);
    //                      END;
  ELSE
    DrawCircle(Pt, ADataImage, AColor, TRUE);
  END;

  // attention: only point(circle) and line is implemented for drawing on TLazINtfImage !
end;

procedure TSeriesStylePoints.DrawPoint(Pt: TPoint; ADataImage: TLazIntfImage;
  AFPColor: TFPColor;  AAlphaBlend: Boolean = false; AAlphaMergeOnly: Boolean = false);
begin
  // we have (Dot, Circle, Square, Cross, Plus, MarkerTriangle)
    IF Diameter = 0 THEN exit;
    CASE Shape OF
      shapeDot:             BEGIN
                              DrawCircle(Pt, ADataImage, AFPColor, TRUE, AAlphaBlend, AAlphaMergeOnly);
                            END;
      shapeCircle:          BEGIN
                              DrawCircle(Pt, ADataImage, AFPColor, FALSE, AAlphaBlend, AAlphaMergeOnly);
                            END;
      //shapeSquare:          BEGIN
      //                        DrawSquare(Pt, ADataImage, AFPColor, FALSE);
      //                      END;
      //shapeSquareSolid:     BEGIN
      //                        DrawSquare(Pt, ADataImage, AFPColor, TRUE);
      //                      END;
      //shapeCross:           BEGIN
      //                        DrawCross(Pt, ADataImage, AFPColor);
      //                      END;
      //shapePlus:            BEGIN
      //                        DrawPlus(Pt, ADataImage, AFPColor);
      //                      END;
      //shapeMarkerTriangle:  BEGIN
      //                        DrawMarkerTriangle(Pt, ADataImage, AFPColor, FALSE);
      //                      END;
    ELSE
      DrawCircle(Pt, ADataImage, AFPColor, TRUE, AAlphaBlend, AAlphaMergeOnly);
    END;

    // attention: only point(circle) and line is implemented for drawing on TLazINtfImage !
end;

procedure TSeriesStylePoints.DrawSamplePoint(Pt: TPoint; Canvas: TCanvas;
  BeginNew: Boolean);
begin
  DrawPoint(Pt, Canvas, Color);
end;


{ TSeriesStyle }

procedure TSeriesStyle.SetOwnerSeries(ASeries: uPlotClass.TPlotSeriesBase);
begin
  if FOwnerSeries=ASeries then exit;
  FOwnerSeries:=ASeries;
end;


procedure TSeriesStyle.DrawSamplePoint(Pt: TPoint; Canvas: TCanvas;
  BeginNew: Boolean);
begin
  DrawPoint(Pt, Canvas);
end;

{ TSeriesStyleLines }

procedure TSeriesStyleLines.SetLineWidth(const AValue: Integer);
begin
  if FLineWidth=AValue then exit;
  FLineWidth:=AValue;
  Pen.Width := FLineWidth;
  IF FLineWidth > 1 THEN Pen.Cosmetic := FALSE ELSE Pen.Cosmetic := TRUE;
end;


constructor TSeriesStyleLines.Create;
begin
  inherited Create;
  FLineWidth := 1;
  FDiameter := 0;
end;

procedure TSeriesStyleLines.DrawPoint(Pt: TPoint; Canvas: TCanvas);
begin
  //inherited DrawPoint(Pt, Canvas);         // TODO: check this ??
  DrawPoint(Pt, Canvas, FColor);
end;

procedure TSeriesStyleLines.DrawPoint(Pt: TPoint; Canvas: TCanvas;
  AColor: TColor);
var
  vPt2: TPoint;          // interpolated point
  vError: Integer;
begin

  inherited DrawPoint(Pt, Canvas, AColor); // this draws points also, but diameter can be set to 0

  //new version
  vError := TPlotSeries(OwnerSeries).GetLineEndpoint(vPt2);
  IF (vError < 0) OR (vPt2 = Pt) THEN exit;

  Canvas.Pen.Style := TPlotStyle(Self).Pen.Style;
  //Canvas.Pen.Cosmetic := Pen.Cosmetic;
  Canvas.Pen.EndCap := pecFlat;
  Canvas.Pen.Width := Pen.Width;
  Canvas.Pen.Color :=   AColor;
  Canvas.Line(vPt2, Pt)
end;

procedure TSeriesStyleLines.DrawPoint(Pt: TPoint; ADataImage: TLazIntfImage);
begin
  DrawPoint(Pt, ADataImage, FColor);
end;

procedure TSeriesStyleLines.DrawPoint(Pt: TPoint; ADataImage: TLazIntfImage;
  AColor: TColor);
begin
  DrawPoint(Pt, ADataImage, TColorToFPColor(AColor), false, false);
end;

procedure TSeriesStyleLines.DrawPoint(Pt: TPoint; ADataImage: TLazIntfImage;
  AFPColor: TFPColor; AAlphaBlend: Boolean; AAlphaMergeOnly: Boolean);
var
  vPt2: TPoint;
  vError: Integer;
begin
  inherited DrawPoint(Pt, ADataImage, AFPColor, AAlphaBlend, AAlphaMergeOnly);

    vError := TPlotSeries(OwnerSeries).GetLineEndpoint(vPt2);
    IF (vError < 0) OR (vPt2 = Pt) THEN exit;

    ClipDrawLine(Pt, vPt2, LineWidth, ADataImage, AFPColor, AAlphaBlend, AAlphaMergeOnly);
end;

procedure TSeriesStyleLines.DrawSamplePoint(Pt: TPoint; Canvas: TCanvas; BeginNew: Boolean);
begin
  inherited DrawPoint(Pt, Canvas, TPlotStyle(Self).Color); // this draws points also, but diameter can be set to 0
  Canvas.Pen.Assign(TPlotStyle(Self).Pen);
  Canvas.Pen.Color:=TPlotStyle(Self).Color;
  IF BeginNew THEN FLastSamplePt := Pt;
  Canvas.Line(FLastSamplePt, Pt);
  FLastSamplePt := Pt;
end;

end.
