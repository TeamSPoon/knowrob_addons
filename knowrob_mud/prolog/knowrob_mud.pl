
/** <module> knowrob_mud_

  Copyright (C) 2013 by Asil Kaan Bozcuoglu and Moritz Tenorth

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.

@author Asil Kaan Bozcuoglu, Moritz Tenorth
@license GPL
*/


:- module(knowrob_mud,
    [
        mud_act/2,
        mud_act/3,
        mud_act_contact/4,
        mud_act_contact/6,
        mud_act_contact_specific/3,
        mud_act_contact_specific/4,
        mud_lift/4,
        mud_lift_specific/3,
        mud_lift_liftonly/4,
        mud_flip_full/9,
        mud_flip_fliponly/9,
        mud_supported_during/4,
        mud_act_start/3,
        mud_act_end/3,
        mud_timeline_val/2,
        mud_subact/3,
        mud_subact_all/2,
        successful_mud_acts_for_goal/2,
        mud_flipping/9,
        mud_grasped/4,
        mud_add_count/1,
        mud_experiment_file/1,
        mud_load_experiments/2
    ]).
:- use_module(library('semweb/rdf_db')).
:- use_module(library('semweb/rdfs')).
:- use_module(library('owl')).
:- use_module(library('rdfs_computable')).
:- use_module(library('owl_parser')).
:- use_module(library('comp_temporal')).
:- use_module(library('knowrob_mongo')).

:- rdf_db:rdf_register_ns(knowrob, 'http://knowrob.org/kb/knowrob.owl#',  [keep(true)]).
:- rdf_db:rdf_register_ns(knowrob_mud, 'http://knowrob.org/kb/knowrob_mud.owl#', [keep(true)]).
:- rdf_db:rdf_register_ns(knowrob_cram, 'http://knowrob.org/kb/knowrob_cram.owl#', [keep(true)]).

% define predicates as rdf_meta predicates
% (i.e. rdf namespaces are automatically expanded)
:-  rdf_meta
    mud_act(r,r),
    mud_act(r,r,r),
    mud_act_contact(r,r,r,r),
    mud_act_contact(r,r,r,r,r,r),
    mud_act_contact_specific(r,r,r),
    mud_act_contact_specific(r,r,r,r),
    mud_lift(r,r,r,r),
    mud_lift_specific(r,r,r),
    mud_lift_liftonly(r,r,r,r),
    mud_flip_full(r,r,r,r,r,r,r,r,r),
    mud_flip_fliponly(r,r,r,r,r,r,r,r,r),
    mud_timeline_val(r,r),
    mud_supported_during(r,r,r,r),
    mud_subact(r,r),
    mud_subact_all(r,r),
    mud_act_start(r,r),
    mud_act_end(r,r),
    mud_flipping(r,r,r,r,r,r,r,r,r),
    mud_grasped(r,r,r,r),
    mud_experiment_file(r),
    mud_load_experiments(r,r),
    successful_mud_acts_for_goal(+,-).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% List of available experiments

exp_list_new(['pf_10.owl','pf_13.owl','pf_16.owl','pf_19.owl','pf_2.owl','pf_5.owl','pf_8.owl','mud_exp2.owl','mud_exp5.owl','pf_11.owl','pf_14.owl','pf_17.owl','pf_1.owl','pf_3.owl','pf_6.owl','pf_9.owl','mud_exp3.owl','mud_exp6.owl','pf_12.owl','pf_15.owl','pf_18.owl','pf_20.owl','pf_4.owl','pf_7.owl','mud_exp1.owl','mud_exp4.owl']).
exp_list(['mud_exp1.owl','mud_exp2.owl','mud_exp4.owl','mud_exp6.owl']).

mud_experiment_file(X):-
    exp_list(List),
    member(X,List).


% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%
% Basic mud_act handling
% 

%% load_experiment(+ExpOwlPath, +ExpFiles) is nondet.
%
%  Loads the owl logfiles of a list of experiments, located on ExpOwlPath using
%  repeated calls to load_experiment in knowrob_cram
%
%  @param ExpOwlPath path where the logfiles are located
%  @param ExpFiles list of logfile names to be loaded
% 
mud_load_experiments(ExpOwlPath, []).
mud_load_experiments(ExpOwlPath, [ExpFile|T]) :-
    atom_concat(ExpOwlPath, ExpFile, Path),
    (load_experiment(Path) -> mud_load_experiments(ExpOwlPath, T); mud_load_experiments(ExpOwlPath, T)).


%% mud_act(?Task) is nondet.
%
%  Check if class of Task is a subclass of SimulationEvent, or looks for Tasks that are subclass of SimulationEvent
%  Keeps track which experiment a certain event belongs to by searching for experiment
%  and only returning events that are a mud_subaction of that experiment
%
%  @param Task Identifier of given Task
% 
mud_act(Experiment, EventID) :-
    rdf_has(EventID, rdf:type, EventClass),
    rdf_reachable(EventClass, rdfs:subClassOf, knowrob_mud_:'SimulationEvent'),
    rdf_has(MetaData, knowrob:'experiment', literal(type(_, Experiment))),
    rdf_has(MetaData, knowrob:'subAction', EventID). %make sure that the event is an mud_subaction of (belongs to) a specific experiment

%% mud_act(?Event, ?EventClass) is nondet.
%
%  Finds EventIDs of EventClass that are subclass of SimulationEvent
%  For example: mud_act(E, knowrob_mud_:'TouchingSituation').
%
%  @param Event Identifier of given Event
%  @param EventClass Identifier of given EventClass
% 
mud_act(Experiment, EventID, EventClass) :-
    rdf_has(EventID, rdf:type, EventClass),
    rdf_reachable(EventClass, rdfs:subClassOf, knowrob_mud_:'SimulationEvent'),
    rdf_has(MetaData, knowrob:'experiment', literal(type(_, Experiment))),
    rdf_has(MetaData, knowrob:'subAction', EventID).

mud_class_individual(ObjectClass, ObjectIndivid) :-
    rdf_has(ObjectIndivid, rdf:type, ObjectClass),
    rdf_reachable(ObjectClass, rdfs:subClassOf, owl:'Thing'). %Otherwise it will return owl:namedIndividual as a Class of any objectinstance as well

%%  mud_act_contact(?Event, ?EventClass, ?ObjectClass) 
%
%   Find a certain event involving a certain object type(s) or certain object(s)
%
%   Example calls:
%   > mud_act_contact(Exp, E, knowrob_mud_:'Cup', O).
%   > mud_act_contact_specific(Exp, E, knowrob_mud_:'Cup_object_hkm6glYmRQ0BWF').
%   > mud_act_contact(Exp, E, knowrob_mud_:'Cup', knowrob_mud_:'KitchenTable', O1, O2).
%   > mud_act_contact_specific(Exp, E, knowrob_mud_:'Cup_object_hkm6glYmRQ0BWF', knowrob_mud_:'KitchenTable_object_50SJX00eStoIfD').
mud_act_contact(Experiment, Event, ObjectClass, ObjectInstance) :-
    mud_act(Experiment, Event, knowrob_mud_:'TouchingSituation'),
    mud_class_individual(ObjectClass, ObjectInstance),
    rdf_has(Event, knowrob_mud_:'inContact', ObjectInstance),
    mud_act_start(Experiment, Event, StartTime).
    %writeln(StartTime).
 %% Find a certain event involving a certain object
mud_act_contact_specific(Experiment, Event, ObjectInstance) :-
    mud_act(Experiment, Event, knowrob_mud_:'TouchingSituation'),
    rdf_has(Event, knowrob_mud_:'inContact', ObjectInstance).
%%  mud_act_contact(?Event, ?EventClass, +Object1Class, +Object2Class, -ObjectInstance1, -ObjectInstance2)
%   
%   Find a certain event involving certain object types
%
mud_act_contact(Experiment, Event, Object1Class, Object2Class, ObjectInstance1, ObjectInstance2) :-
    mud_act(Experiment, Event, knowrob_mud_:'TouchingSituation'),
    mud_class_individual(Object1Class, ObjectInstance1),
    mud_class_individual(Object2Class, ObjectInstance2),
    ObjectInstance1\=ObjectInstance2,
    rdf_has(Event, knowrob_mud_:'inContact', ObjectInstance1),
    rdf_has(Event, knowrob_mud_:'inContact', ObjectInstance2).
%%  mud_act_contact(?Event, ?EventClass, +ObjectInstance1, +ObjectInstance2)
%   
%   Find a certain event involving certain objects
%
mud_act_contact_specific(Experiment, Event, ObjectInstance1, ObjectInstance2) :-
    mud_act(Experiment, Event, knowrob_mud_:'TouchingSituation'),
    ObjectInstance1\=ObjectInstance2,
    rdf_has(Event, knowrob_mud_:'inContact', ObjectInstance1),
    rdf_has(Event, knowrob_mud_:'inContact', ObjectInstance2).

%% Function returns a range for each event in the list during which that event is true
% Not done yet!
mud_timeline_val(EventList, vals).

%%  Auxilary function for enabling changing color on consecutive calls
%
%   Step should be something like 0x404000

%   Example use:
%   nb_setval(counter,0x0080ff), mud_lift_liftonly(Exp, knowrob:'Cup', Start, End), 
%   mud_add_count(0x404000), nb_getval(counter,Val).
%   Val will be different after each backtrack. Can use this as color
%
%   WARNING: not sure what will happen when "maximum" is reached
%
mud_add_count(Step) :- 
    nb_getval(counter, C), CNew is C + Step, nb_setval(counter, CNew), writeln(CNew).

%% ignore this predicate, doesn't work right now but is also not so important, meant to change color of successive calls
mud_act_count(T, Class) :-
    findall(Task, mud_act(Task,Class), T),
    length(T, Val),
    write('Counter:'), writeln(Val).

%% mud_subact(?Event, ?SubEvent) is nondet.
%
%  Check if SubEvent is a child of Event
%  Can be used for exampel to  
%
%  @param Task Identifier of given Task
%  @param Submud_act Identifier of given Submud_act
% 
mud_subact(Experiment, Event, SubEvent) :-
    rdf_has(Event, knowrob:'subAction', SubEvent),
    mud_act(Experiment, Event),
    mud_act(Experiment, SubEvent).

%% submud_act_all(?Task, ?Submud_act) is nondet.
%
%  Check if Task is an ancestor of Submud_act in the mud_act tree
%
%  @param Task Identifier of given Task
%  @param Submud_act Identifier of given Submud_act
% 
mud_subact_all(Event, SubEvent) :-
    owl_has(Event, knowrob:subAction,  SubEvent).


% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%
% Simact specific actions handling
% 

mud_grasped(Experiment, EventID, ObjectClass, ObjectInstance) :-
    mud_act(Experiment, EventID, knowrob:'GraspingSomething'),
    rdf_has(EventID, knowrob:'objectActedOn', ObjectInstance), %for a lift to occur, the event in which the object participates must involve GraspingSomething (lift can only happen while the object is grasped)
    mud_class_individual(ObjectClass, ObjectInstance).

%% Find event interval during which a specific object type is lifted
%% 
%% Example call: 
%% > mud_lift(E, knowrob_mud_:'Cup').
mud_lift(Experiment, EventID, ObjectClass, ObjectInstance) :-
    mud_act(Experiment, EventID, knowrob:'GraspingSomething'),
    rdf_has(EventID, knowrob:'objectActedOn', ObjectInstance), %for a lift to occur, the event in which the object participates must involve GraspingSomething (lift can only happen while the object is grasped)
    mud_class_individual(ObjectClass, ObjectInstance),
    not(mud_supported_during(Experiment, EventID, _, ObjectInstance)). %check that this specific object is not in a contact relation with a supporting object for at least part of the interval (the interval will overlap at least with some contact intervals, because for example when you lift the mug, it will still be in contact with the table while the hand initiates contact). 

%% Find event interval during which a specific object is lifted
%% 
%% Example call: 
%% > mud_lift_specific(E, knowrob_mud_:'Cup_object_hkm6glYmRQ0BWF').
mud_lift_specific(Experiment, EventID, ObjectInstance) :-
    mud_act(Experiment, EventID, knowrob:'GraspingSomething'),
    rdf_has(EventID, knowrob:'objectActedOn', ObjectInstance),
    not(mud_supported_during(Experiment, EventID, _,ObjectInstance)).

%% Gives a new interval (start and endtimes) during which the specified object is lifted
%% Differs from mud_lift because mud_lift can only return existing event intervals, and some
%% overlap with supportedby intervals are inevitable, while mud_lift_liftonly "defines" a new
%% interval by looking for the difference between grasping intervals and all supportedby 
%% intervals.
%%
%% Example call:
%% > mud_lift_liftonly(Exp, knowrob_mud_:'Cup', Start, End).
mud_lift_liftonly(Experiment, ObjectClass, Start, End) :-
    mud_act(Experiment, EventID, knowrob:'GraspingSomething'),
    rdf_has(EventID, knowrob:'objectActedOn', ObjectInstance), %for a lift to occur, the event in which the object participates must involve GraspingSomething (lift can only happen while the object is grasped)
    mud_class_individual(ObjectClass, ObjectInstance),
    mud_act_start(Experiment, EventID, TempStart),
    mud_act_end(Experiment, EventID, TempEnd),
    findall(EventID2, (mud_supported(Experiment, EventID2, ObjectInstance), not(comp_temporallySubsumes(EventID2, EventID))), Candidates),
    %% writeln(Candidates),
    interval_setdifference(Experiment, TempStart, TempEnd, Candidates, Start, End).

%% Flipping: grasping start until object entirely on tool, start contact tool and target, start object on tool until object back on pancakemaker, start still grasping tool until tool put down.
mud_flipping(Experiment, ObjectO, ToolO, TargetO, GraspS, ToolCTargetS, ObjectLiftS, PutbackS, PutbackE) :-
    % start of GraspSpatula
    mud_grasped(Experiment, Event1ID, _,ToolO),
    mud_act_start(Experiment, Event1ID, GraspS),
    mud_act_end(Experiment, Event1ID, PutbackE), %this end is the very end of the flipping
    % Object and target should have a contact interval overlapping with the beginning of GraspSpatula
    mud_act_contact(Experiment, Event0ID, knowrob:'LiquidTangibleThing', _, ObjectO, TargetO),
    comp_overlapsI(Event0ID, Event1ID),
    ToolO\=ObjectO,
    ObjectO\=TargetO,
    ToolO\=TargetO,
    %% Tool is in contact with the Object but Object has not left Target yet
    mud_act_contact(Experiment, Event2ID, _, _, ToolO, ObjectO),
    mud_act_start(Experiment, Event2ID, ToolCTargetS),
    %% Tool is no longer in contact with the Target, but it is in contact with the Object. There is a small overlapping issue because the pancake leaves the pancakemaker a few miliseconds after the spatula does, so ObjectLiftS starts a bit before mud_act_end(Event0ID, End0) ends.
    mud_flip_fliponly(Experiment,_,_,_,ObjectLiftS, _, ObjectO, ToolO, TargetO),
    %% Object touches the target again
    mud_act_contact(Experiment, Event3ID, _, _, ObjectO, TargetO),
    comp_beforeI(Event2ID, Event3ID), %touches target after leaving tool
    mud_act_start(Experiment, Event3ID, PutbackS). %WARNING: in old version need to cut because jsonquery backend returns all solutions, try again?

%% Gives a new interval, which is the union of contactPancake-Spatula and contactSpatula-Liquid.
%% These two events should be overlapping in order to be a full flipping interval 
%% Finds a flip given the class of the object to be flipped and the tool with which this is done
%% Note that maybe the most important thing, whether or not the object was turned, cannot be deducted from the owl file
%% 
%% Example call: 
%% > mud_flip_full(knowrob_mud_:'LiquidTangibleThing', knowrob_mud_:'Spatula', knowrob_mud_:'PancakeMaker', Start, End, OObj, TObj, LObj).
mud_flip_full(Experiment, ObjectClass, ToolClass, LocationClass, Start, End, OObj, TObj, LObj) :-
    % get contactInterval spatula-pancakemaker
    mud_act_contact(Experiment, EventID, ToolClass, LocationClass, TObj, LObj),
    % get contactInterval spatula-liquid
    mud_act_contact(Experiment, EventID2, ObjectClass, ToolClass, OObj, TObj),
    % these two should overlap, with the spatula-pancakemaker coming first
    comp_overlapsI(EventID, EventID2),
    % select start and end as union
    mud_act_start(Experiment, EventID, Start),
    mud_act_end(Experiment, EventID2, End).

%% Gives a new interval, which is a subset of contactSpatula-Liquid. This is only the time during
%% which the liquid is in contact with the spatula and not in contact with the pancakemaker (Note: pancakemaker is not a object-supportingFurniture in the ontology so can't use supportedby here).
%% 
%% Example call:
%% > mud_flip_fliponly(knowrob_mud_:'LiquidTangibleThing', knowrob_mud_:'Spatula', knowrob_mud_:'PancakeMaker', Start, End, OObj, TObj, LObj).
mud_flip_fliponly(Experiment, ObjectClass, ToolClass, LocationClass, Start, End, OObj, TObj, LObj) :-
    % get contactInterval spatula-pancakemaker
    mud_act_contact(Experiment, EventID, ToolClass, LocationClass, TObj, LObj),
    % get contactInterval spatula-liquid
    mud_act_contact(Experiment, EventID2, ObjectClass, ToolClass, OObj, TObj),
    %don't get the owlNamedIndividual classes
    ObjectClass\=ToolClass,
    ToolClass\=LocationClass,
    LocationClass\=ObjectClass,
    % these two should overlap, with the spatula-pancakemaker coming first
    comp_overlapsI(EventID, EventID2),
    % select start and end as intersection
    mud_act_end(Experiment,EventID, Start),
    mud_act_end(Experiment,EventID2, End).

%%  mud_supported(?Experiment, ?EventID, ?ObjectInstance) is 
%   Body of the function for mud_supported_during
%
%   Returns the EventID of the events in which ObjectInstance was supported by supporting Object-SupportingFurniture
%   Assumes that supportedby is a TouchingSituation.
%
mud_supported(Experiment, EventID, ObjectInstance) :-
    mud_act(Experiment, EventID, knowrob_mud_:'TouchingSituation'),
    rdf_has(EventID, knowrob_mud_:'inContact', ObjectInstance),
    rdf_has(EventID, knowrob_mud_:'inContact', ObjectInstance2),
    ObjectInstance \= ObjectInstance2,
    mud_class_individual(Obj2Class, ObjectInstance2),
    rdf_reachable(Obj2Class, rdfs:subClassOf, knowrob:'Object-SupportingFurniture').

test(Arr) :-
    jpl_new('org.knowrob.vis.MarkerVisualization', [], Canvas),
    jpl_list_to_array(['1','2','3','4'], Arr),
    jpl_call(Canvas, 'showAverageTrajectory', [bla, Arr, Arr, 1, 1], _).


% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%
% Interval handling
% 

%% 
%   Given a start/endtime value, looks at given list of time intervals and subtracts
%   these if they overlap with the given start/endtime interval. Returns start/endtime values
%   that do not include any of those intervals in the list.
%
%Bottom case; unify temporary start and end with the result
interval_setdifference(Experiment, Start, End, [], Start, End). 
%If the head overlaps at the beginning
interval_setdifference(Experiment, Start, End, [EventID2|Tail], ResStart, ResEnd) :-
    mud_act_start(Experiment,EventID2, Start2),
    mud_act_end(Experiment,EventID2, End2),
    mud_timepoints_overlap(Start, End, Start2, End2),
    interval_setdifference(Experiment, End2, End, Tail, ResStart, ResEnd), !.
%If the head overlaps at the end
interval_setdifference(Experiment, Start, End, [EventID2|Tail], ResStart, ResEnd) :-
    mud_act_start(Experiment,EventID2, Start2),
    mud_act_end(Experiment,EventID2, End2),
    mud_timepoints_overlap_inv(Start, End, Start2, End2),
    interval_setdifference(Experiment, Start, Start2, Tail, ResStart, ResEnd), !.
%if there is no overlap, so we don't care about the current head
interval_setdifference(Experiment, Start, End, [_|Tail], ResStart, ResEnd) :-
    interval_setdifference(Experiment, Start, End, Tail, ResStart, ResEnd), !.

%% Similar to the comp_overlapsI predicate but works with separate timepoints 
%% True if I2 overlaps with I1 at the beginning
%% Called by: interval_setdifference
mud_timepoints_overlap(Start1, End1, Start2, End2) :-
    time_point_value(Start1, SVal1),
    time_point_value(End1, EVal1),
    time_point_value(Start2, SVal2),
    time_point_value(End2, EVal2),
    SVal2 < SVal1, %Start2 is before Start1
    EVal2 > SVal1, %End2 is after Start1
    EVal2 < EVal1. %End2 ends before End1
mud_timepoints_overlap_inv(Start1, End1, Start2, End2) :-
    mud_timepoints_overlap(Start2, End2, Start1, End1).

%%  mud_supported_during(?Experiment, +EventID, ?EventID2, ?ObjectInstance)
%
%   helpfunction for mud_lift (and its variants)
%   returns true if Object in Event1 was supported by another object in Event2, and
%   Event1 is entirely contained in Event2 (e.g. Object was supported by something during Event1)
%   For example, if the Cup was grasp and during the entire grasping interval, there was also
%   contact with the kitchen table.
%
%   NOTE: nothing in this function checks that ObjectInstance is involved in EventID, 
%   This depends on the function calling mud_supported_during
%
mud_supported_during(Experiment, EventID, EventID2, ObjectInstance) :-
    mud_supported(Experiment, EventID2, ObjectInstance),
    EventID\=EventID2,
    comp_temporallySubsumes(EventID2, EventID). 

% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%
% Temporal stuff: start, end, duration of a mud_act
%


%% mud_act_start(?Task, ?Start) is nondet.
%
%  Check if Start is the start time of Task
%
%  @param Task Identifier of given Task
%  @param Start Identifier of given Start
% 
mud_act_start(Experiment, Event, Start) :-
    rdf_has(Event, knowrob:'startTime', Start),
    mud_act(Experiment, Event).


%% mud_act_end(?Task, ?End) is nondet.
%
%  Check if End is the end time of Task
%
%  @param Task Identifier of given Task
%  @param End Identifier of given End
% 
mud_act_end(Experiment, Event, End) :-
    rdf_has(Event, knowrob:'endTime', End),
    mud_act(Experiment,Event).

%% Note: To get the duration, just call comp_duration(+Task, -Duration) from the comp_temporal 
%% package. Make sure it's registered though (register_ros_package)


% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%
% Goals, success, failure
%

%% mud_act_goal(?Task, ?Goal) is nondet.
%
%  Check if Goal is the goal of Task
%
%  @param Task Identifier of given Task
%  @param Goal Identifier of given Goal
% 
mud_act_mud_subaction(Subaction, Type) :-
    mud_act(_, Task),
    rdf_has(Task, knowrob:'mud_actContext', literal(type(_, Goal))).

%% successful_mud_acts_for_goal(+Goal, -Tasks) is nondet.
%
% Finds all Tasks that successsfully achieved Goal, i.e. without failure.
% 
% @param Goal  Identifier of the goal to be searched for
% @param Tasks List of mud_acts that successfully accomplished the Goal 
% 
successful_mud_acts_for_goal(Goal, Tasks) :-
     findall(T, (mud_act_goal(T, Goal)), Ts),
     findall(FT, ((mud_act_goal(FT, Goal), rdf_has(FT, knowrob:'caughtFailure', _F))), FTs),
     subtract(Ts, FTs, Tasks).

% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%
% Objects (and robot parts) and their locations
%

add_object_as_semantic_instance(Obj, Matrix, Time, ObjInstance) :-
    add_object_to_semantic_map(Obj, Matrix, Time, ObjInstance, 0.2, 0.2, 0.2).

add_robot_as_basic_semantic_instance(PoseList, Time, ObjInstance) :-
    add_object_to_semantic_map(Time, PoseList, Time, ObjInstance, 0.5, 0.2, 0.2).


add_object_to_semantic_map(Obj, PoseList, Time, ObjInstance, H, W, D) :-
    is_list(PoseList),
    create_pose(PoseList, Matrix),
    add_object_to_semantic_map(Obj, Matrix, Time, ObjInstance, H, W, D).

add_object_to_semantic_map(Obj, Matrix, Time, ObjInstance, H, W, D) :-
    atom(Matrix),
    rdf_split_url(_, ObjLocal, Obj),
    atom_concat('http://knowrob.org/kb/cram_log.owl#Object_', ObjLocal, ObjInstance),
    rdf_assert(ObjInstance, rdf:type, knowrob:'SpatialThing-Localized'),
    rdf_assert(ObjInstance,knowrob:'depthOfObject',literal(type(xsd:float, D))),
    rdf_assert(ObjInstance,knowrob:'widthOfObject',literal(type(xsd:float, W))),
    rdf_assert(ObjInstance,knowrob:'heightOfObject',literal(type(xsd:float, H))),
    rdf_assert(ObjInstance,knowrob:'describedInMap','http://knowrob.org/kb/ias_semantic_map.owl#SemanticEnvironmentMap_PM580j'), % TODO: give map as parameter

    rdf_instance_from_class(knowrob:'SemanticMapPerception', Perception),
    rdf_assert(Perception, knowrob:'startTime', Time),
    rdf_assert(Perception, knowrob:'eventOccursAt', Matrix),

    set_object_perception(ObjInstance, Perception).