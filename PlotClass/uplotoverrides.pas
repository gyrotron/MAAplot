unit uPlotOverrides;
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
  Classes, SysUtils, FPimage, IntfGraphics, math;


type

  { TLazIntfImage_debug }

  TLazIntfImage_debug = class (TLazIntfImage)
  private
    //procedure _SetAlpha_BPP32_A8R8G8B8(x, y: integer; const Value: TFPColor);
  protected
    //FSetAlphaProc: TLazIntfImageSetPixelProc;
  public
    //constructor Create; override;
    //destructor Destroy; override;

    procedure AlphaBlend(ASource, ASourceAlpha: TLazIntfImage; const ADestX, ADestY: Integer);reintroduce;
    procedure ApplyAlpha(ASourceAlpha: TLazIntfImage; const ADestX, ADestY: Integer);
  end;



implementation

{ TLazIntfImage_debug }

//procedure TLazIntfImage_debug._SetAlpha_BPP32_A8R8G8B8(x, y: integer;
//  const Value: TFPColor);
//var
//  VBytes: TFPColorBytes absolute Value;
//begin
//  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
//  begin
//    VBytes.Ah := B0;
//    VBytes.Al := B0;
//    VBytes.Rh := B1;
//    VBytes.Rl := B1;
//    VBytes.Gh := B2;
//    VBytes.Gl := B2;
//    VBytes.Bh := B3;
//    VBytes.Bl := B3;
//  end;
//end;

//constructor TLazIntfImage_debug.Create;
//begin
//  inherited Create;
//end;
//
//destructor TLazIntfImage_debug.Destroy;
//begin
//  FSetAlphaProc:=nil;
//  inherited Destroy;
//end;

procedure TLazIntfImage_debug.AlphaBlend(ASource, ASourceAlpha: TLazIntfImage;
  const ADestX, ADestY: Integer);
var
  x, y, CurX, CurY: Integer;
  MaskValue, InvMaskValue: Word;
  CurColor: TFPColor;
  lDrawWidth, lDrawHeight: Integer;
begin
  //writeln('AlphaBlend_debug');
  // Take care not to draw outside the destination area
  lDrawWidth := Min(Self.Width - ADestX, ASource.Width);
  lDrawHeight := Min(Self.Height - ADestY, ASource.Height);
  for y := 0 to lDrawHeight - 1 do
  begin
    for x := 0 to lDrawWidth - 1 do

    begin
      CurX := ADestX + x;
      CurY := ADestY + y;

      // Never draw outside the destination
      if (CurX < 0) or (CurY < 0) then Continue;

      if ASourceAlpha <> nil then
        MaskValue := ASourceAlpha.Colors[x, y].alpha
      else
        MaskValue := ASource.Colors[x, y].alpha;

      InvMaskValue := $FFFF - MaskValue;

      if MaskValue = $FFFF then
      begin
        Self.Colors[CurX, CurY] := ASource.Colors[x, y];
      end
      else if MaskValue > $00 then
      begin
        CurColor := Self.Colors[CurX, CurY];

        CurColor.Red := Round(
          CurColor.Red * InvMaskValue / $FFFF +
          ASource.Colors[x, y].Red * MaskValue / $FFFF);

        CurColor.Green := Round(
          CurColor.Green * InvMaskValue / $FFFF +
          ASource.Colors[x, y].Green * MaskValue / $FFFF);

        CurColor.Blue := Round(
          CurColor.Blue * InvMaskValue / $FFFF +
          ASource.Colors[x, y].Blue * MaskValue / $FFFF);

        Self.Colors[CurX, CurY] := CurColor;
      end;
    end;

  end;
end;


procedure TLazIntfImage_debug.ApplyAlpha(ASourceAlpha: TLazIntfImage;
  const ADestX, ADestY: Integer);
var
  x, y, CurX, CurY: Integer;
  MaskValue: Word;
  CurColor: TFPColor;
  lDrawWidth, lDrawHeight: Integer;
  //alphaprec, alphashift: byte;
begin
  //writeln('ApplyAlpha_debug');
  // Take care not to draw outside the destination area
  if ASourceAlpha = nil then exit;
  lDrawWidth := Min(Self.Width - ADestX, ASourceAlpha.Width);
  lDrawHeight := Min(Self.Height - ADestY, ASourceAlpha.Height);

  //alphaprec := Self.DataDescription.AlphaPrec;
  //alphashift := Self.DataDescription.AlphaShift;

  for y := 0 to lDrawHeight - 1 do
  begin
    for x := 0 to lDrawWidth - 1 do
    begin
      CurX := ADestX + x;
      CurY := ADestY + y;

      // Never draw outside the destination
      if (CurX < 0) or (CurY < 0) then Continue;

      MaskValue := ASourceAlpha.Colors[x, y].alpha;
      CurColor := Self.Colors[CurX, CurY];

      if (MaskValue = $0) or (CurColor.alpha = $00) or (CurColor.alpha < $FFFF-MaskValue)  then continue;
      //else if CurColor.alpha > $00 then
      begin
        //CurColor := Self.Colors[CurX, CurY];
        // following line multiplies current alpha.... not good for fadeout in update mode
        //CurColor.alpha := round((CurColor.alpha) * (MaskValue / $FFFF));
        // for fixed fadeout, use only mask alpha
        // problem: function does not know if we are in the masked area or not
        // solution: make an inverse mask and subtract the alpha mask values here

        CurColor.alpha := $FFFF-MaskValue;

        Self.Colors[CurX, CurY] := CurColor;
      end;
    end;
  end;
end;

end.

