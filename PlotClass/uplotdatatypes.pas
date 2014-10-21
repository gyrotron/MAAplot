unit uPlotDataTypes;
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
  Classes, SysUtils;

type

  PXYValue = ^TXYValue;
  TXYValue = packed record
    X,
    Y : Extended
  end;

  PXYZValue = ^TXYZValue;
  TXYZValue = packed record
    X,
    Y,
    Z : Extended
  end;

  PXYLine = ^TXYLine;
  TXYLine = array of TXYValue;

// types from uMAAdatatypes, needed for ResultWriter
  PValue = ^TValue;
  TValue = Double;

  TMeasResultType = (mrtGenResult);   // add mrtBinary, mrt2D

  TMeasResultIdent = record
    dwResultType: TMeasResultType;
    diDimensions: Integer;    // identifies the number of rows in one correlating measurement
    diPoints: Integer;        // contains the number of points in one correlating data row
    diMeasurement: Integer;   // identifies the measurement, correlated or not to the previous or subsequent one
    dwTimeStamp: TTimeStamp;
  end;

  TMeasPoint = array of TValue;
  PMeasArray = ^TMeasArray;
  TMeasArray = array of TMeasPoint;
  PMeasResult = ^TMeasResult;
  TMeasResult = record                         // denotes as mvtGenResult
    dwResultIdent: TMeasResultIdent;
    lpMeasData: PMeasArray;
  end;


implementation

end.

