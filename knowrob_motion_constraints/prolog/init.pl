%%
%% Copyright (C) 2013 by Moritz Tenorth
%%
%% This module provides methods for representing and reasoning about
%% constraint-based motion specifications in KnowRob.
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

:- register_ros_package(knowrob_srdl).
:- register_ros_package(knowrob_actions).
:- register_ros_package(knowrob_objects).
:- register_ros_package(knowrob_mesh_reasoning).

:- register_ros_package(knowrob_motion_constraints).
:- use_module(library(knowrob_motion_constraints)).

% namespace for general motion constraint ontology
:- rdf_db:rdf_register_ns(constr, 'http://knowrob.org/kb/motion-constraints.owl#', [keep(true)]).

% pancake making task definition
:- owl_parser:owl_parse('package://knowrob_motion_constraints/owl/pancake-making-constr.owl').
:- rdf_db:rdf_register_ns(pancake_constr, 'http://knowrob.org/kb/pancake-making-constr.owl#', [keep(true)]).

% pouring task definition
:- owl_parser:owl_parse('package://knowrob_motion_constraints/owl/pouring.owl').
:- rdf_db:rdf_register_ns(motion, 'http://knowrob.org/kb/motion-def.owl#', [keep(true)]).

% object models
:- owl_parser:owl_parse('package://knowrob_motion_constraints/owl/spatula-features.owl').
:- rdf_db:rdf_register_ns(spatula, 'http://knowrob.org/kb/spatula-features.owl#', [keep(true)]).

:- owl_parser:owl_parse('package://knowrob_motion_constraints/owl/mondamin-pancake-mix.owl').
:- owl_parser:owl_parse('package://knowrob_motion_constraints/owl/pancake-maker.owl').

