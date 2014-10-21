unit uIntfImageDrawingUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IntfGraphics, FPimage;

procedure ClipDrawPixel(X, Y: Integer; ADataImage: TLazIntfImage;
  AFPColor: TFPColor;  AAlphaBlend: Boolean = false; AAlphaMergeOnly: Boolean = false);

procedure DrawCircleEx(Pt: TPoint; ADataImage: TLazIntfImage;
  AFPColor: TFPColor; Filled: Boolean; ADiameter: Integer;
  AAlphaBlend: Boolean; AAlphaMergeOnly: Boolean);

procedure ClipDrawLine(A, B: TPoint; ALineWidth: Integer; ADataImage: TLazIntfImage;
  AFPColor: TFPColor; AAlphaBlend: Boolean; AAlphaMergeOnly: Boolean);

implementation


procedure ClipDrawPixel(X, Y: Integer; ADataImage: TLazIntfImage;
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

          //multiplikativ
          //CurColor.alpha := $FFFF - (round( ($FFFF - CurColor.alpha) / $FFFF * vInverseAlpha));
          //additiv ?
          CurColor.alpha := $FFFF - (round( ($FFFF - CurColor.alpha) / $FFFF + vInverseAlpha)) DIV 2;

          //CurColor.alpha := Round(
          //  CurColor.alpha * vInverseAlpha / $FFFF +
          //  AFPColor.alpha * vAlpha / $FFFF);

          ADataImage.Colors[X, Y] := CurColor;
        end;
      end;
    end;
  END;

end;



procedure DrawCircleEx(Pt: TPoint; ADataImage: TLazIntfImage;
  AFPColor: TFPColor; Filled: Boolean; ADiameter: Integer;
  AAlphaBlend: Boolean; AAlphaMergeOnly: Boolean);
var
  vRadius, vRy: Integer;
  vLoop, vLoopy: Integer;
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




procedure ClipDrawLine(A, B: TPoint; ALineWidth: Integer; ADataImage: TLazIntfImage;
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
      vY2 := trunc(dy/dx * (vLoop-vX1)) + vY1;  //    trunc(dy/dx * (vLoop-vX1) + 1) + vY1; why +1 ???????????????? also in Lazarus bug ?
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

end.

