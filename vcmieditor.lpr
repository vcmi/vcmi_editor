{ This file is a part of Map editor for VCMI project

  Copyright (C) 2013-2016 Alexander Shishkin alexvins@users.sourceforge.net

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
  sysutils,
  Interfaces, // this includes the LCL widgetset
  Forms, lazcontrols, main, lod, editor_graphics, Map, editor_types, terrain,
  undo_base, undo_map, map_actions, zlib_stream, map_objects, new_map, minimap,
  filesystem_base, filesystem, vcmi_json, editor_utils, map_format,
  map_format_h3m, stream_adapter, editor_str_consts,
  vcmi_fpjsonrtti, glext40, map_options, editor_classes, editor_consts,
  root_manager, progress_form, editor_gl, object_options, edit_object_options,
  base_options_frame, grail_frame, flaggable_object_frame, lists_manager,
  witchhut_frame, h3_txt, gui_helpers, shrine_frame, spellscroll_frame,
  root_form, dwelling_frame, map_format_zip, map_format_json,
  map_terrain_actions, map_road_river_actions, map_object_actions,
  road_transitions, transitions, base_info, creature_frame,
  object_link_frame, logical_id_condition, logical_event_condition,
  logical_expression, abandoned_frame, scholar_frame, creature_set_frame,
  resource_frame, pandoras_reward_frame, local_event_frame, hero_frame,
  editor_rtti, hero_artifacts_frame, message_frame, edit_hero_options,
  hero_options_frame, player_options_form, 
player_options_frame, position, hero_spells_frame, hero_skills_frame, 
town_frame, town_buildings_frame, zipper_ex, edit_triggered_events, 
locale_manager, player_selection_form, map_rect, town_spells_frame, logical_building_condition, quest_frame, 
reward_frame, field_editors;

{$R *.res}

begin

  {$IF DEFINED(DEBUG)}
  if FileExists('heap.trc') then
    DeleteFile('heap.trc');
  SetHeapTraceOutput('heap.trc');
  {$ENDIF DEBUG}

  Application.Title := 'VCMI Editor';
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TRootManager, RootManager);
  Application.CreateForm(TfMain, fMain);
  Application.Run;
end.

