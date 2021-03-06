%%
%% Copyright (C) 2013 by Moritz Tenorth
%%
%% This program is free software; you can redistribute it and/or modify
%% it under the terms of the GNU General Public License as published by
%% the Free Software Foundation; either version 3 of the License, or
%% (at your option) any later version.
%%
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%%
%% You should have received a copy of the GNU General Public License
%% along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%

/** <module> knowrob_mud/init.pl - An Implementation of a MUD server


@license LGPL
*/

% This is for when PrologMUD starts KnowRob (instead of the other way around)
:- current_predicate(register_ros_package/1) -> true; consult(rosprolog).

:- register_ros_package(milo).
:- register_ros_package(euler).

load_more_schema :- rdf_library:((rdf_attach_library(rdf(cpack)), rdf_load_library(cpack), rdf_attach_library(rdf(base)),rdf_attach_library(rdf(base/manifest)))).

:-initialization(load_more_schema).

:- register_ros_package(knowrob_common).
:- register_ros_package(knowrob_objects).
:- register_ros_package(knowrob_mongo).
:- register_ros_package(comp_temporal).
:- register_ros_package(comp_spatial).

end_of_file.

:- register_ros_package(knowrob_roslog_launch).
:- register_ros_package(knowrob_cram).
:- register_ros_package(knowrob_sim).
:- register_ros_package(knowrob_mongo).
:- register_ros_package(knowrob_vis).
:- register_ros_package(iai_semantic_maps).
:- register_ros_package(knowrob_srdl).

% :- register_ros_package(knowrob_motion_constraints).


:- register_ros_package(knowrob_mud).


:- use_module(library('knowrob_mud')).

%Extended ontology
:- owl_parser:owl_parse('package://knowrob_mud/owl/knowrob_mud.owl').
:- rdf_db:rdf_register_ns(knowmud, 'http://ias.cs.tum.edu/kb/knowrob_mud.owl#', [keep(true)]).

:- owl_parser:owl_parse('package://knowrob_mud/owl/mud_simulation_map.owl').
:- rdf_db:rdf_register_ns(mud_map, 'http://ias.cs.tum.edu/kb/mud_simulation_map.owl#', [keep(true)]).
