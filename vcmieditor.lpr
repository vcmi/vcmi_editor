{ This file is a part of Map editor for VCMI project

  Copyright (C) 2013 Alexander Shishkin alexvins@users.sourceforge.net

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
program vcmieditor;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, csvdocument_package, lazopenglcontext, runtimetypeinfocontrols, main,
  lod, def, Map, editor_types, terrain, undo_base, undo_map, map_actions,
  zlib_stream, objects, def_animation, new_map, minimap, filesystem_base,
  filesystem, vcmi_json, editor_utils, map_format, map_format_h3m,
  stream_adapter, editor_str_consts, map_format_vcmi, vcmi_fpjsonrtti,
  map_options, editor_classes, editor_consts
  { you can add units after this };

{$R *.res}

begin

  Application.Title := 'VCMI Editor';
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TfMain, fMain);
  Application.Run;
end.

