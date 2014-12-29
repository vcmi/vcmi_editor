{ This file is a part of Map editor for VCMI project

  Copyright (C) 2013,2014 Alexander Shishkin alexvins@users.sourceforge,net

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}

unit road_transitions;

{$I compilersetup.inc}
{$MODESWITCH ADVANCEDRECORDS}
{$MODESWITCH NESTEDPROCVARS}

interface

uses
  Classes, SysUtils, transitions;

type

  TSimpleRule = string;

  TSimpleRulesArray = array[0..8] of TSimpleRule;

  TSimplePattern = record
    Rules:TSimpleRulesArray;
    Mapping: TMapping;
    HasVFlip: boolean;
    HasHFlip: boolean;
  end;

const

  ROAD_RULES : array[0..9] of TSimplePattern =
    (
      //single tile. In fact there is no good pattern for such case.
      (
        Rules:
        (
          '-','-','-',
          '-','+','-',
          '-','-','-'
        );
        Mapping: (Lower:14; Upper:14);
        HasVFlip: false;
        HasHFlip: false;
      ),
      //Road straight with angle  //???
      (
        Rules:
        (
          '?','-','+',
          '-','+','+',
          '+','+','?'
        );
        Mapping: (Lower:2; Upper:5);
        HasVFlip: true;
        HasHFlip: true;
      ),
      //Road turn right
      (
        Rules:
        (
          '?','-','?',
          '-','+','+',
          '?','+','?'
        );
        Mapping: (Lower:0; Upper:1);
        HasVFlip: true;
        HasHFlip: true;
      ),
      //dead end right
      (
        Rules:
        (
          '?','-','?',
          '-','+','+',
          '?','-','?'
        );
        Mapping: (Lower:15; Upper:15);
        HasVFlip: false;
        HasHFlip: true;
      ),
       //dead end down
      (
        Rules:
        (
          '?','-','?',
          '-','+','-',
          '?','+','?'
        );
        Mapping: (Lower:14; Upper:14);
        HasVFlip: true;
        HasHFlip: false;
      ),
      //T-cross right
      (
        Rules:
        (
          '?','+','?',
          '-','+','+',
          '?','+','?'
        );
        Mapping: (Lower:6; Upper:7);
        HasVFlip: false;
        HasHFlip: true;
      ),
      //T-cross down
      (
        Rules:
        (
          '?','-','?',
          '+','+','+',
          '?','+','?'
        );
        Mapping: (Lower:8; Upper:9);
        HasVFlip: true;
        HasHFlip: false;
      ),
      //Horizontal
      (
        Rules:
        (
          '?','-','?',
          '+','+','+',
          '?','-','?'
        );
        Mapping: (Lower:12; Upper:13);
        HasVFlip: false;
        HasHFlip: false;
      ),
      //Vertical
      (
        Rules:
        (
          '?','+','?',
          '-','+','-',
          '?','+','?'
        );
        Mapping: (Lower:10; Upper:11);
        HasVFlip: false;
        HasHFlip: false;
      ),
      //X-cross
      (
        Rules:
        (
          '?','+','?',
          '+','+','+',
          '?','+','?'
        );
        Mapping: (Lower:16; Upper:16);
        HasVFlip: false;
        HasHFlip: false;
      )
    );

  function  GetFlippedPattern(const APattern: TSimplePattern; flip:Integer):TSimplePattern;

implementation

function RuleIsAny(const Rule: TSimpleRule): boolean;
begin
  Result := Rule = '?';
end;

function RuleIsRoad(const Rule: TSimpleRule): boolean;
begin
  Result := Rule = '+';
end;

function RuleIsNone(const Rule: TSimpleRule): boolean;
begin
  Result := Rule = '-';
end;

function GetFlippedPattern(const APattern: TSimplePattern; flip: Integer
  ): TSimplePattern;
  procedure SwapRules(var p: TSimplePattern; idx1, idx2: Integer);
  var
    tmp: TSimpleRule;
  begin
    tmp := p.Rules[idx1];
    p.Rules[idx1] := p.Rules[idx2];
    p.Rules[idx2] := tmp;
  end;

var
  i: Integer;
  y: Integer;
begin
  //todo: use cached patterns

  if flip = 0 then
  begin
    Exit(APattern);
  end
  else begin
    Assert(flip > 0);
    Assert(flip < 4);

    Result := APattern;

    if flip in [FLIP_PATTERN_HORIZONTAL, FLIP_PATTERN_BOTH] then
      for i := 0 to 3 - 1 do
      begin
        y := i*3;

        SwapRules(Result,Y+2,Y);
      end;


    if flip in [FLIP_PATTERN_VERTICAL, FLIP_PATTERN_BOTH] then
    begin
      for i := 0 to 3 - 1 do
      begin
        SwapRules(Result,i,i+6);
      end;
    end;

  end;
end;

end.

